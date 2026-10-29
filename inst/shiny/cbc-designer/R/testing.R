# Testing a design
# ---------------------------------------------------------------------------
# Counting checks (level balance, pair association, overlap, position balance) plus
# the precision of the part-worth estimates. Precision is computed analytically:
# with "dummy" respondents who answer at random (all part-worths 0) the MNL
# information matrix is  sum over tasks of X'(diag(p) - pp')X  with p = 1/(alternatives),
# and the standard errors are the square roots of the diagonal of its inverse.
# That is what Sawtooth's Test Design estimates by simulating random answers and
# running a logit. Only random tasks are tested; fixed tasks are hand-picked.

SE_GOOD <- 0.05        # Sawtooth: largest main-effect standard error around 0.05,
SE_EXCELLENT <- 0.03   # preferably 0.03, for the expected sample size

rate_se <- function(se) {
  ifelse(is.na(se), "Not estimable",
         ifelse(se <= SE_EXCELLENT, "Excellent", ifelse(se <= SE_GOOD, "Good", "Weak")))
}

effects_codes <- function(nlev) lapply(nlev, function(L) rbind(diag(L - 1L), -1))

# Standard errors for every attribute level (and the None constant) with n respondents
# who cycle through the versions.
design_se <- function(tab, nlev, concepts, none, n) {
  A <- length(nlev); J <- concepts; M <- J + as.integer(none)
  E <- effects_codes(nlev); npar <- nlev - 1L; k <- sum(npar) + as.integer(none)
  X <- do.call(cbind, lapply(seq_len(A), function(a) E[[a]][tab[[paste0("Att", a)]], , drop = FALSE]))
  if (none) X <- cbind(X, 0)
  task <- (seq_len(nrow(tab)) - 1L) %/% J
  S <- rowsum(X, task)
  if (none) S[, k] <- 1
  ver_row <- tab$Version; ver_task <- ver_row[seq(1L, nrow(tab), by = J)]
  versions <- sort(unique(ver_row))
  info <- lapply(versions, function(v) {
    Xv <- X[ver_row == v, , drop = FALSE]; Sv <- S[ver_task == v, , drop = FALSE]
    XX <- crossprod(Xv)
    if (none) XX[k, k] <- XX[k, k] + nrow(Sv)       # the None alternative is e_k in every task
    XX / M - crossprod(Sv) / M^2
  })
  w <- tabulate((seq_len(n) - 1L) %% length(versions) + 1L, length(versions))
  I <- Reduce(`+`, Map(`*`, w, info))
  failed <- list(estimable = FALSE, se = rep(NA_real_, sum(nlev)), none_se = NA_real_, n = n)
  if (!all(is.finite(I)) || rcond(I) < 1e-10) return(failed)
  Sigma <- solve(I)
  idx <- split(seq_len(sum(npar)), rep(seq_len(A), npar))
  se <- unlist(lapply(seq_len(A), function(a) {
    i <- idx[[a]]
    c(sqrt(diag(Sigma)[i]), sqrt(sum(Sigma[i, i])))   # last level = minus the sum of the others
  }))
  list(estimable = TRUE, se = unname(se),
       none_se = if (none) sqrt(Sigma[k, k]) else NA_real_, n = n)
}

cramers_v <- function(m) {
  m <- m[rowSums(m) > 0, colSums(m) > 0, drop = FALSE]
  if (nrow(m) < 2L || ncol(m) < 2L) return(0)
  n <- sum(m); e <- outer(rowSums(m), colSums(m)) / n
  sqrt(sum((m - e)^2 / e) / (n * (min(dim(m)) - 1L)))
}

oneway_counts <- function(tab, cs) {
  do.call(rbind, lapply(seq_len(cs$A), function(a) {
    cnt <- tabulate(tab[[paste0("Att", a)]], cs$nlev[a])
    exp <- nrow(tab) / cs$nlev[a]
    data.frame(attr = a, attr_name = cs$attr_names[a], level = seq_len(cs$nlev[a]),
               label = cs$level_labels[[a]], count = cnt, expected = exp,
               pct = 100 * cnt / exp, stringsAsFactors = FALSE)
  }))
}

pair_tables <- function(tab, cs) {
  assoc <- matrix(NA_real_, cs$A, cs$A, dimnames = list(cs$attr_names, cs$attr_names))
  tables <- list()
  for (k in seq_len(nrow(cs$pairs))) {
    a <- cs$pairs[k, 1]; b <- cs$pairs[k, 2]
    m <- table(factor(tab[[paste0("Att", a)]], seq_len(cs$nlev[a])),
               factor(tab[[paste0("Att", b)]], seq_len(cs$nlev[b])))
    dimnames(m) <- list(cs$level_labels[[a]], cs$level_labels[[b]])
    tables[[paste(a, b, sep = "_")]] <- unclass(m)
    assoc[a, b] <- assoc[b, a] <- cramers_v(m)
  }
  list(assoc = assoc, tables = tables)
}

overlap_summary <- function(tab, cs, J) {
  task <- (seq_len(nrow(tab)) - 1L) %/% J
  data.frame(attr = seq_len(cs$A), attr_name = cs$attr_names,
             repeat_pct = vapply(seq_len(cs$A), function(a)
               100 * mean(tapply(tab[[paste0("Att", a)]], task, anyDuplicated) > 0), numeric(1)),
             unavoidable = J > cs$nlev, stringsAsFactors = FALSE)
}

position_summary <- function(tab, cs, sort_attr) {
  sorted <- cs$attr_ids == sort_attr
  data.frame(attr = seq_len(cs$A), attr_name = cs$attr_names,
             max_dev_pct = vapply(seq_len(cs$A), function(a) {
               m <- table(factor(tab[[paste0("Att", a)]], seq_len(cs$nlev[a])), tab$Concept)
               e <- rowSums(m) / ncol(m)
               100 * max(abs(m[e > 0, , drop = FALSE] - e[e > 0]) / e[e > 0])
             }, numeric(1)),
             sorted = sorted, stringsAsFactors = FALSE)
}

test_design <- function(design, study, n = 300L) {
  cs <- compile_study(study); ds <- study$settings
  tab <- design$table[!design$table$Fixed, , drop = FALSE]
  J <- max(design$table$Concept); V <- length(unique(design$table$Version))
  none <- ds$none$mode != "none"
  se <- design_se(tab, cs$nlev, J, none, n)
  se_tab <- data.frame(attr = rep(seq_len(cs$A), cs$nlev), attr_name = rep(cs$attr_names, cs$nlev),
                       level = unlist(lapply(cs$nlev, seq_len)), label = unlist(cs$level_labels),
                       se = se$se, rating = rate_se(se$se), stringsAsFactors = FALSE)
  random_tasks <- nrow(tab) / (V * J)
  value <- n * random_tasks * J / max(cs$nlev)
  list(
    summary = list(versions = V, random_tasks = random_tasks,
                   fixed_tasks = length(unique(design$table$Task[design$table$Fixed])),
                   concepts = J, rows = nrow(design$table), n = n,
                   method = design$meta$method, seed = design$meta$seed, none = none),
    oneway = oneway_counts(tab, cs), pairs = pair_tables(tab, cs),
    overlap = overlap_summary(tab, cs, J),
    position = position_summary(tab, cs, ds$design$sort_attr),
    se = se_tab, none_se = se$none_se, estimable = se$estimable,
    sanity = list(value = value, ok = value >= 500)
  )
}
