# Design generation: versions x tasks x concepts
# ---------------------------------------------------------------------------
# Four methods, all respecting prohibitions and never repeating a concept in a task:
#   complete   per concept: fewest repeated levels in the task first, then the most
#              balanced / orthogonal choice among all feasible concepts
#   shortcut   builds a concept attribute by attribute from the least-used levels
#   balanced   like complete but overlap inside a task is allowed (balance only)
#   random     uniformly random feasible concepts
# These follow the published ideas behind Sawtooth's CBC design methods, not their
# proprietary code, so designs are comparable in quality but not identical.
# Level counts and pair counts are kept per questionnaire version (one respondent).

POOL_MAX <- 3000L      # candidate concepts scored per pick; bigger spaces are sampled

# Where each level / level pair lives in the flat count vectors.
design_layout <- function(cs) {
  A <- cs$A; K <- nrow(cs$pairs); nlev <- cs$nlev
  psize <- if (K) nlev[cs$pairs[, 1]] * nlev[cs$pairs[, 2]] else integer()
  pidx <- matrix(0L, A, A)
  for (k in seq_len(K)) { pidx[cs$pairs[k, 1], cs$pairs[k, 2]] <- k; pidx[cs$pairs[k, 2], cs$pairs[k, 1]] <- k }
  list(off = c(0L, cumsum(nlev))[seq_len(A)], L = sum(nlev), lw = rep(nlev, nlev),
       poff = if (K) c(0L, cumsum(psize))[seq_len(K)] else integer(),
       ncell = sum(psize), pw = rep(psize, psize), pidx = pidx)
}

cell_ids <- function(cs, lay, x) {
  if (!nrow(cs$pairs)) return(integer())
  a <- cs$pairs[, 1]; b <- cs$pairs[, 2]
  lay$poff + (x[a] - 1L) * cs$nlev[b] + x[b]
}

# Candidate concepts with their flat level / pair-cell ids precomputed.
make_pool <- function(cs, lay, X) {
  N <- nrow(X); K <- nrow(cs$pairs)
  PG <- if (K) vapply(seq_len(K), function(k)
    lay$poff[k] + (X[, cs$pairs[k, 1]] - 1L) * cs$nlev[cs$pairs[k, 2]] + X[, cs$pairs[k, 2]], numeric(N))
  else matrix(0, N, 0L)
  list(X = X, G = X + rep(lay$off, each = N), PG = matrix(PG, nrow = N))
}

# Returns a function giving the pool for the next version (cached when it is small).
pool_source <- function(cs, lay, method) {
  if (method == "shortcut") return(function() NULL)
  feasible <- enumerate_feasible(cs)
  if (!is.null(feasible) && nrow(feasible) <= POOL_MAX) {
    pool <- make_pool(cs, lay, feasible)
    return(function() pool)
  }
  function() {
    X <- if (!is.null(feasible)) feasible[sample.int(nrow(feasible), POOL_MAX), , drop = FALSE]
         else { m <- sample_feasible(cs, POOL_MAX); m[!duplicated(m), , drop = FALSE] }
    make_pool(cs, lay, X)
  }
}

# Running counts for one questionnaire version.
new_state <- function(lay) {
  st <- new.env(parent = emptyenv())
  st$c1 <- numeric(lay$L); st$c2 <- numeric(lay$ncell); st$n <- 0
  st
}
record <- function(st, cs, lay, x) {
  g <- lay$off + x; st$c1[g] <- st$c1[g] + 1
  cc <- cell_ids(cs, lay, x); st$c2[cc] <- st$c2[cc] + 1
  st$n <- st$n + 1
}

LEVEL_WEIGHT <- 100    # level balance outranks pair balance (as in Sawtooth's priorities)

# Lower is better: how over-used the candidates' levels and level pairs already are,
# relative to what an even spread would have used.
score_balance <- function(pool, st, lay, rows) {
  G <- pool$G[rows, , drop = FALSE]
  s <- LEVEL_WEIGHT * rowSums(matrix(st$c1[G] * lay$lw[G], nrow(G))) / ncol(G)
  if (ncol(pool$PG)) {
    P <- pool$PG[rows, , drop = FALSE]
    s <- s + rowSums(matrix(st$c2[P] * lay$pw[P], nrow(P))) / ncol(P)
  }
  s / max(1, st$n)
}

matches <- function(X, x) rowSums(X == rep(x, each = nrow(X)))   # equal levels per row

# Candidates that are not already in the task (and, optionally, bring a new level of attribute 1).
eligible <- function(pool, task_x, once, A) {
  ok <- rep(TRUE, nrow(pool$X))
  for (r in seq_len(nrow(task_x))) ok <- ok & matches(pool$X, task_x[r, ]) < A
  if (once) {
    fresh <- ok & !(pool$X[, 1] %in% task_x[, 1])
    if (any(fresh)) ok <- fresh
  }
  if (!any(ok)) ok[] <- TRUE
  ok
}

pick_ce <- function(pool, st, cs, lay, task_x, once, overlap = TRUE, noise = 1e-6) {
  rows <- which(eligible(pool, task_x, once, cs$A))
  if (overlap && nrow(task_x)) {
    ov <- Reduce(`+`, lapply(seq_len(nrow(task_x)), function(r) matches(pool$X[rows, , drop = FALSE], task_x[r, ])))
    rows <- rows[ov == min(ov)]
  }
  sc <- score_balance(pool, st, lay, rows) + stats::runif(length(rows), 0, noise)
  pool$X[rows[which.min(sc)], ]
}

pick_random <- function(pool, task_x, cs) {
  rows <- which(eligible(pool, task_x, FALSE, cs$A))
  pool$X[rows[sample.int(length(rows), 1L)], ]
}

allowed_levels <- function(cs, lay, x, a) {
  ok <- rep(TRUE, cs$nlev[a])
  for (b in which(x > 0L)) {
    f <- cs$forb[[lay$pidx[a, b]]]
    ok <- ok & !(if (a < b) f[, x[b]] else f[x[b], ])
  }
  which(ok)
}

# Build one concept attribute by attribute from the least-used levels.
pick_shortcut <- function(cs, lay, st, task_x, once) {
  for (attempt in seq_len(100L)) {
    x <- integer(cs$A)
    for (a in sample.int(cs$A)) {
      lv <- allowed_levels(cs, lay, x, a)
      if (a == 1L && once && length(setdiff(lv, task_x[, 1]))) lv <- setdiff(lv, task_x[, 1])
      if (!length(lv)) { x <- NULL; break }
      in_task <- tabulate(task_x[, a], cs$nlev[a])[lv]
      x[a] <- lv[order(in_task, st$c1[lay$off[a] + lv], stats::runif(length(lv)))[1L]]
    }
    if (!is.null(x) && !any(matches(task_x, x) == cs$A)) return(x)
  }
  m <- sample_feasible(cs, 1L)
  if (!nrow(m)) stop("No concept satisfies all prohibitions.", call. = FALSE)
  m[1L, ]
}

pick_concept <- function(cs, ds, lay, st, pool, task_x, once) {
  switch(ds$design$method,
    complete = pick_ce(pool, st, cs, lay, task_x, once),
    balanced = pick_ce(pool, st, cs, lay, task_x, FALSE, overlap = FALSE, noise = 0.15),
    random   = pick_random(pool, task_x, cs),
    shortcut = pick_shortcut(cs, lay, st, task_x, once))
}

make_task <- function(cs, ds, lay, st, pool, once) {
  task_x <- matrix(0L, 0L, cs$A)
  for (j in seq_len(ds$general$concepts)) {
    x <- pick_concept(cs, ds, lay, st, pool, task_x, once)
    record(st, cs, lay, x)
    task_x <- rbind(task_x, x)
  }
  unname(task_x)
}

# Concept order inside a task: fully randomised, or sorted by one attribute with one
# level order per version (constant across tasks).
sort_concepts <- function(tasks, cs, d) {
  if (d$sort == "none") return(lapply(tasks, function(m) m[sample.int(nrow(m)), , drop = FALSE]))
  a <- match(d$sort_attr, cs$attr_ids)
  rank <- if (d$sort == "random") sample.int(cs$nlev[a]) else seq_len(cs$nlev[a])
  lapply(tasks, function(m) m[order(rank[m[, a]], stats::runif(nrow(m))), , drop = FALSE])
}

# Display order of the attributes for one respondent (constant across their tasks).
attribute_order <- function(cs, d) {
  o <- seq_len(cs$A)
  if (d$position == "all") o <- sample.int(cs$A)
  if (d$position == "subset") {
    lo <- min(d$pos_first, cs$A); hi <- min(max(d$pos_last, lo), cs$A)
    idx <- lo:hi
    o[idx] <- o[idx][sample.int(length(idx))]
  }
  o
}

# Fixed tasks sit at their positions; random tasks fill the remaining slots in order.
merge_fixed <- function(rnd, fixed, fpos, total) {
  out <- vector("list", total)
  for (i in seq_along(fpos)) out[[fpos[i]]] <- matrix(fixed[i, , ], nrow = dim(fixed)[2])
  out[setdiff(seq_len(total), fpos)] <- rnd
  out
}

make_version <- function(cs, ds, lay, pool, fpos) {
  d <- ds$design; g <- ds$general
  once <- isTRUE(d$first_attr_once) && d$method %in% c("complete", "shortcut") && g$concepts <= cs$nlev[1]
  st <- new_state(lay)
  rnd <- lapply(seq_len(g$random_tasks), function(t) make_task(cs, ds, lay, st, pool, once))
  rnd <- sort_concepts(rnd, cs, d)
  list(tasks = merge_fixed(rnd, cs$fixed, fpos, tasks_total(ds)), order = attribute_order(cs, d))
}

assemble_design <- function(blocks, cs, ds, fpos) {
  V <- length(blocks); J <- ds$general$concepts; n_tasks <- tasks_total(ds)
  X <- do.call(rbind, lapply(blocks, function(b) do.call(rbind, b$tasks)))
  tab <- data.frame(Version = rep(seq_len(V), each = n_tasks * J),
                    Task = rep(rep(seq_len(n_tasks), each = J), V),
                    Concept = rep(seq_len(J), n_tasks * V),
                    Fixed = rep(rep(seq_len(n_tasks) %in% fpos, each = J), V))
  att <- as.data.frame(X); names(att) <- paste0("Att", seq_len(cs$A))
  list(table = cbind(tab, att),
       attr_order = do.call(rbind, lapply(blocks, `[[`, "order")),
       meta = list(method = ds$design$method, seed = ds$design$seed, versions = V, concepts = J,
                   random_tasks = ds$general$random_tasks, fixed_positions = fpos,
                   none = ds$none$mode, response = ds$general$response, nlev = cs$nlev,
                   created = Sys.time()))
}

assert_generatable <- function(s) {
  iss <- validate_study(s)
  err <- iss$message[iss$severity == "error"]
  if (length(err)) stop(paste(err, collapse = " "), call. = FALSE)
}

# Generate the whole design. `progress(i, n)` is called before each version.
generate_design <- function(s, progress = NULL) {
  assert_generatable(s)
  cs <- compile_study(s); ds <- s$settings; lay <- design_layout(cs)
  fpos <- fixed_positions(ds); next_pool <- pool_source(cs, lay, ds$design$method)
  V <- ds$design$versions
  blocks <- with_seed(ds$design$seed, lapply(seq_len(V), function(v) {
    if (!is.null(progress)) progress(v, V)
    make_version(cs, ds, lay, next_pool(), fpos)
  }))
  assemble_design(blocks, cs, ds, fpos)
}
