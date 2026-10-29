# The study model
# ---------------------------------------------------------------------------
# A study is a plain list so it serialises straight to JSON:
#   attributes    data.frame(id, name)                       in display order
#   levels        data.frame(attr, id, label)                in order within an attribute
#   prohibitions  data.frame(a, la, b, lb)                   attribute `a` precedes `b`
#   cond          data.frame(target, on, tlevel, key, text)  conditional relationships
#   fixed         data.frame(task, concept, attr, level)     always complete
#   settings      nested list, see default_settings()
# Everything refers to attributes and levels by stable ids ("a1", "l7"), never by
# position, so reordering or deleting can never silently re-point a constraint.
# tidy_study() is the single place that restores the invariants after any edit.

tbl <- list(
  attributes   = function() data.frame(id = character(), name = character(), stringsAsFactors = FALSE),
  levels       = function() data.frame(attr = character(), id = character(), label = character(), stringsAsFactors = FALSE),
  prohibitions = function() data.frame(a = character(), la = character(), b = character(), lb = character(), stringsAsFactors = FALSE),
  cond         = function() data.frame(target = character(), on = character(), tlevel = character(),
                                       key = character(), text = character(), stringsAsFactors = FALSE),
  fixed        = function() data.frame(task = integer(), concept = integer(), attr = character(),
                                       level = character(), stringsAsFactors = FALSE)
)

CBC_LIMITS <- list(attributes = 30L, levels = 250L)

# ---- settings -------------------------------------------------------------

default_settings <- function() list(
  general = list(random_tasks = 12L, fixed_tasks = 2L, concepts = 3L, columns = 4L,
                 response = "choice", total_points = 100L),
  none    = list(mode = "traditional", text = "NONE: I wouldn't choose any of these.",
                 valign = "middle",
                 dual_question = "If you had to choose, would you actually buy the product you picked?",
                 dual_yes = "Yes, I would buy it", dual_no = "No, I would not buy it"),
  format  = list(header = "If these were your only options, which would you choose?",
                 footer = "", concept_label = "Concept {n}", button = "Select",
                 show_attr_names = TRUE),
  design  = list(method = "complete", versions = 300L, seed = 1L, position = "none",
                 pos_first = 1L, pos_last = 3L, sort = "none", sort_attr = NA_character_,
                 first_attr_once = FALSE),
  fixed   = list(positions = integer())
)

# Coerce anything (UI inputs, JSON) into a complete, in-range settings list.
sanitize_settings <- function(x) {
  d <- default_settings(); x <- if (is.list(x)) x else list()
  g <- x$general; n <- x$none; f <- x$format; e <- x$design
  list(
    general = list(
      random_tasks = as_int(g$random_tasks, d$general$random_tasks, 0L, 60L),
      fixed_tasks  = as_int(g$fixed_tasks,  d$general$fixed_tasks,  0L, 20L),
      concepts     = as_int(g$concepts,     d$general$concepts,     2L, 12L),
      columns      = as_int(g$columns,      d$general$columns,      1L, 12L),
      response     = as_choice(g$response, c("choice", "sum"), d$general$response),
      total_points = as_int(g$total_points, d$general$total_points, 2L, 10000L)),
    none = list(
      mode  = as_choice(n$mode, c("none", "traditional", "dual"), d$none$mode),
      text  = as_chr(n$text, d$none$text),
      valign = as_choice(n$valign, c("top", "middle", "bottom"), d$none$valign),
      dual_question = as_chr(n$dual_question, d$none$dual_question),
      dual_yes = as_chr(n$dual_yes, d$none$dual_yes),
      dual_no  = as_chr(n$dual_no, d$none$dual_no)),
    format = list(
      header = as_chr(f$header, d$format$header),
      footer = as_chr(f$footer, d$format$footer),
      concept_label = as_chr(f$concept_label, d$format$concept_label),
      button = as_chr(f$button, d$format$button),
      show_attr_names = as_lgl(f$show_attr_names, d$format$show_attr_names)),
    design = list(
      method = as_choice(e$method, c("complete", "shortcut", "balanced", "random"), d$design$method),
      versions = as_int(e$versions, d$design$versions, 1L, 10000L),
      seed = as_int(e$seed, d$design$seed, 0L, 999999999L),
      position = as_choice(e$position, c("none", "all", "subset"), d$design$position),
      pos_first = as_int(e$pos_first, d$design$pos_first, 1L, 30L),
      pos_last = as_int(e$pos_last, d$design$pos_last, 1L, 30L),
      sort = as_choice(e$sort, c("none", "random", "natural"), d$design$sort),
      sort_attr = as_chr(e$sort_attr, NA_character_),
      first_attr_once = as_lgl(e$first_attr_once, d$design$first_attr_once)),
    fixed = list(positions = as.integer(unlist(x$fixed$positions)))
  )
}

tasks_total <- function(settings) settings$general$random_tasks + settings$general$fixed_tasks

# Where the fixed tasks sit in the questionnaire: the user's positions when they
# are valid, otherwise spread evenly.
fixed_positions <- function(settings) {
  nf <- settings$general$fixed_tasks; total <- tasks_total(settings)
  if (nf == 0L) return(integer())
  p <- settings$fixed$positions
  if (length(p) == nf && !anyNA(p) && !anyDuplicated(p) && all(p >= 1L & p <= total))
    return(sort(as.integer(p)))
  auto <- as.integer(round(seq_len(nf) * (total + 1) / (nf + 1)))
  if (anyDuplicated(auto)) auto <- seq.int(total - nf + 1L, total)
  auto
}

# ---- creating and editing -------------------------------------------------

next_id <- function(s, prefix) {
  id <- paste0(prefix, s$next_id)
  s$next_id <- s$next_id + 1L
  list(s = s, id = id)
}

new_study <- function(name = "Untitled study", example = TRUE) {
  s <- list(version = 1L, name = name, notes = "", next_id = 1L,
            attributes = tbl$attributes(), levels = tbl$levels(),
            prohibitions = tbl$prohibitions(), cond = tbl$cond(), fixed = tbl$fixed(),
            settings = default_settings())
  if (example) {
    s <- add_attribute(s, "Brand", c("Acme", "Bolt", "Crest", "Delta"))
    s <- add_attribute(s, "Screen size", c("5.4\"", "6.1\"", "6.7\""))
    s <- add_attribute(s, "Price", c("$199", "$299", "$399", "$499"))
  } else {
    s <- add_attribute(add_attribute(s))
  }
  s
}

append_level <- function(s, attr, label = NULL) {
  r <- next_id(s, "l"); s <- r$s
  label <- label %||% paste("Level", sum(s$levels$attr == attr) + 1L)
  s$levels <- rbind(s$levels, data.frame(attr = attr, id = r$id, label = label, stringsAsFactors = FALSE))
  s
}

add_attribute <- function(s, name = NULL, levels = c("Level 1", "Level 2")) {
  name <- name %||% paste("Attribute", nrow(s$attributes) + 1L)
  r <- next_id(s, "a"); s <- r$s
  s$attributes <- rbind(s$attributes, data.frame(id = r$id, name = name, stringsAsFactors = FALSE))
  for (lab in levels) s <- append_level(s, r$id, lab)
  tidy_study(s)
}

remove_attribute <- function(s, id) {
  s$attributes <- s$attributes[s$attributes$id != id, , drop = FALSE]
  tidy_study(s)
}

rename_attribute <- function(s, id, name) {
  s$attributes$name[s$attributes$id == id] <- name
  s
}

move_attribute <- function(s, id, dir = c("up", "down")) {
  ord <- move_in(s$attributes$id, id, match.arg(dir))
  s$attributes <- s$attributes[match(ord, s$attributes$id), , drop = FALSE]
  tidy_study(s)
}

add_level <- function(s, attr, label = NULL) {
  stopifnot(attr %in% s$attributes$id)
  tidy_study(append_level(s, attr, label))
}

remove_level <- function(s, id) {
  s$levels <- s$levels[s$levels$id != id, , drop = FALSE]
  tidy_study(s)
}

set_level_label <- function(s, id, label) {
  s$levels$label[s$levels$id == id] <- label
  s
}

move_level <- function(s, id, dir = c("up", "down")) {
  rows <- which(s$levels$attr == s$levels$attr[s$levels$id == id])
  ord <- move_in(s$levels$id[rows], id, match.arg(dir))
  s$levels[rows, ] <- s$levels[rows[match(ord, s$levels$id[rows])], ]
  tidy_study(s)
}

# Replace all levels of an attribute from a list of labels (e.g. pasted lines).
# Existing ids are reused by position so constraints on early levels survive.
set_levels_bulk <- function(s, attr, labels) {
  labels <- trimws(labels); labels <- labels[nzchar(labels)]
  old <- s$levels$id[s$levels$attr == attr]
  rest <- s$levels[s$levels$attr != attr, , drop = FALSE]
  new <- tbl$levels()
  for (i in seq_along(labels)) {
    id <- if (i <= length(old)) old[i] else { r <- next_id(s, "l"); s <- r$s; r$id }
    new <- rbind(new, data.frame(attr = attr, id = id, label = labels[i], stringsAsFactors = FALSE))
  }
  s$levels <- rbind(rest, new)
  tidy_study(s)
}

# ---- prohibitions ---------------------------------------------------------

toggle_prohibition <- function(s, a, la, b, lb) {
  if (identical(a, b)) stop("A prohibition must combine levels of two different attributes.", call. = FALSE)
  stopifnot(la %in% s$levels$id[s$levels$attr == a], lb %in% s$levels$id[s$levels$attr == b])
  row <- order_pairs(data.frame(a = a, la = la, b = b, lb = lb, stringsAsFactors = FALSE), s$attributes$id)
  p <- s$prohibitions
  hit <- p$a == row$a & p$la == row$la & p$b == row$b & p$lb == row$lb
  s$prohibitions <- if (any(hit)) p[!hit, , drop = FALSE] else rbind(p, row)
  tidy_study(s)
}

clear_prohibitions <- function(s) {
  s$prohibitions <- tbl$prohibitions()
  s
}

# ---- conditional relationships -------------------------------------------

# One row per (target level) x (combination of the levels it depends on).
cond_cells <- function(s, target, on) {
  tl <- s$levels[s$levels$attr == target, , drop = FALSE]
  combos <- expand.grid(lapply(on, function(a) s$levels$id[s$levels$attr == a]),
                        stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  keys <- do.call(paste, c(unname(as.list(combos)), sep = "|"))
  grid <- expand.grid(i = seq_len(nrow(tl)), k = seq_along(keys))
  data.frame(target = target, on = paste(on, collapse = "|"), tlevel = tl$id[grid$i],
             key = keys[grid$k], text = tl$label[grid$i], stringsAsFactors = FALSE)
}

set_conditional <- function(s, target, on) {
  on <- unique(on)
  if (target %in% on) stop("An attribute cannot depend on itself.", call. = FALSE)
  stopifnot(target %in% s$attributes$id, length(on) >= 1L, all(on %in% s$attributes$id))
  s$cond <- rbind(s$cond[s$cond$target != target, , drop = FALSE], cond_cells(s, target, on))
  tidy_study(s)
}

set_conditional_text <- function(s, target, tlevel, key, text) {
  row <- s$cond$target == target & s$cond$tlevel == tlevel & s$cond$key == key
  if (!any(row)) stop("No such conditional cell.", call. = FALSE)
  s$cond$text[row] <- text
  s
}

remove_conditional <- function(s, target) {
  s$cond <- s$cond[s$cond$target != target, , drop = FALSE]
  tidy_study(s)
}

# ---- invariants -----------------------------------------------------------

tidy_study <- function(s) {
  s$settings <- sanitize_settings(s$settings)
  s <- tidy_levels(s)
  s <- tidy_prohibitions(s)
  s <- tidy_conditional(s)
  sa <- s$settings$design$sort_attr
  if (!is.na(sa) && !sa %in% s$attributes$id) s$settings$design$sort_attr <- NA_character_
  tidy_fixed(s)
}

tidy_levels <- function(s) {
  lv <- s$levels[s$levels$attr %in% s$attributes$id, , drop = FALSE]
  lv <- lv[order(match(lv$attr, s$attributes$id), seq_len(nrow(lv))), , drop = FALSE]
  rownames(lv) <- NULL
  rownames(s$attributes) <- NULL
  s$levels <- lv
  s
}

# Store every prohibition with the earlier attribute first: the one place that
# decides pair order, used when adding a prohibition and after attributes move.
order_pairs <- function(p, ids) {
  swap <- match(p$a, ids) > match(p$b, ids)
  if (any(swap)) {
    first <- p[swap, c("a", "la")]
    p[swap, c("a", "la")] <- p[swap, c("b", "lb")]
    p[swap, c("b", "lb")] <- first
  }
  p
}

tidy_prohibitions <- function(s) {
  p <- s$prohibitions
  known <- function(attr, lvl) paste(attr, lvl) %in% paste(s$levels$attr, s$levels$id)
  p <- p[p$a != p$b & known(p$a, p$la) & known(p$b, p$lb), , drop = FALSE]
  p <- order_pairs(p, s$attributes$id)
  p <- p[!duplicated(p), , drop = FALSE]
  rownames(p) <- NULL
  s$prohibitions <- p
  s
}

tidy_conditional <- function(s) {
  if (nrow(s$cond) == 0L) return(s)
  rels <- unique(s$cond[c("target", "on")])
  rels <- rels[!duplicated(rels$target), , drop = FALSE]
  out <- list()
  for (i in seq_len(nrow(rels))) {
    target <- rels$target[i]; on <- strsplit(rels$on[i], "|", fixed = TRUE)[[1]]
    if (!target %in% s$attributes$id || !all(on %in% s$attributes$id) || target %in% on) next
    cells <- cond_cells(s, target, on)
    have <- s$cond[s$cond$target == target, , drop = FALSE]
    m <- match(paste(cells$tlevel, cells$key), paste(have$tlevel, have$key))
    cells$text[!is.na(m)] <- have$text[m[!is.na(m)]]
    out[[length(out) + 1L]] <- cells
  }
  s$cond <- if (length(out)) do.call(rbind, out) else tbl$cond()
  rownames(s$cond) <- NULL
  s
}

# Fixed tasks are hand-picked concepts shown to everybody. Keep them complete:
# any missing concept is drawn at random (reproducibly) from the feasible ones.
tidy_fixed <- function(s) {
  g <- s$settings$general; ids <- s$attributes$id; A <- length(ids)
  if (A == 0L || g$fixed_tasks == 0L) { s$fixed <- tbl$fixed(); return(s) }
  s0 <- s; s0$fixed <- tbl$fixed(); cs <- compile_study(s0)
  f <- s$fixed
  ok <- f$task >= 1L & f$task <= g$fixed_tasks & f$concept >= 1L & f$concept <= g$concepts &
    paste(f$attr, f$level) %in% paste(s$levels$attr, s$levels$id)
  f <- f[ok, , drop = FALSE]
  f <- f[!duplicated(f[c("task", "concept", "attr")]), , drop = FALSE]
  out <- list()
  for (t in seq_len(g$fixed_tasks)) for (j in seq_len(g$concepts)) {
    rows <- f[f$task == t & f$concept == j, , drop = FALSE]
    if (nrow(rows) != A) rows <- draw_fixed_concept(cs, s$settings$design$seed, t, j)
    rows <- rows[order(match(rows$attr, ids)), , drop = FALSE]
    out[[length(out) + 1L]] <- rows
  }
  s$fixed <- do.call(rbind, out)
  rownames(s$fixed) <- NULL
  s
}

draw_fixed_concept <- function(cs, seed, t, j) {
  x <- with_seed(seed + 7919L * t + 104729L * j, {
    m <- sample_feasible(cs, 1L)
    if (nrow(m)) m[1L, ] else vapply(cs$nlev, function(k) sample.int(k, 1L), integer(1))
  })
  data.frame(task = t, concept = j, attr = cs$attr_ids,
             level = mapply(function(a, l) cs$level_ids[[a]][l], seq_len(cs$A), x),
             stringsAsFactors = FALSE)
}

# ---- compiled form used by the algorithms ---------------------------------
# Index based (1..nlev), built from the id based study at the moment of use.

compile_study <- function(s) {
  ids <- s$attributes$id; A <- length(ids)
  lv <- lapply(ids, function(a) s$levels[s$levels$attr == a, , drop = FALSE])
  nlev <- vapply(lv, nrow, integer(1))
  level_ids <- lapply(lv, `[[`, "id")
  pairs <- if (A >= 2L) t(utils::combn(A, 2L)) else matrix(0L, 0L, 2L)
  forb <- lapply(seq_len(nrow(pairs)), function(k) matrix(FALSE, nlev[pairs[k, 1]], nlev[pairs[k, 2]]))
  p <- s$prohibitions
  for (r in seq_len(nrow(p))) {
    ai <- match(p$a[r], ids); bi <- match(p$b[r], ids)
    k <- which(pairs[, 1] == ai & pairs[, 2] == bi)
    forb[[k]][match(p$la[r], level_ids[[ai]]), match(p$lb[r], level_ids[[bi]])] <- TRUE
  }
  cs <- list(A = A, attr_ids = ids, attr_names = s$attributes$name, nlev = nlev,
             level_ids = level_ids, level_labels = lapply(lv, `[[`, "label"),
             pairs = pairs, forb = forb, total = prod(as.numeric(nlev)),
             forb_active = which(vapply(forb, any, logical(1))))
  cs$cond  <- compile_cond(s, cs)
  cs$fixed <- compile_fixed(s, cs)
  cs
}

compile_cond <- function(s, cs) {
  if (nrow(s$cond) == 0L) return(list())
  unname(lapply(split(s$cond, s$cond$target), function(d) {
    t <- match(d$target[1], cs$attr_ids)
    on <- match(strsplit(d$on[1], "|", fixed = TRUE)[[1]], cs$attr_ids)
    key <- vapply(strsplit(d$key, "|", fixed = TRUE), function(k)
      paste(mapply(function(id, a) match(id, cs$level_ids[[a]]), k, on), collapse = "|"), character(1))
    list(target = t, on = on,
         lookup = stats::setNames(d$text, paste(match(d$tlevel, cs$level_ids[[t]]), key, sep = "#")))
  }))
}

compile_fixed <- function(s, cs) {
  g <- s$settings$general
  if (g$fixed_tasks == 0L || cs$A == 0L || nrow(s$fixed) == 0L)
    return(array(0L, c(0L, g$concepts, cs$A)))
  arr <- array(0L, c(g$fixed_tasks, g$concepts, cs$A))
  ai <- match(s$fixed$attr, cs$attr_ids)
  arr[cbind(s$fixed$task, s$fixed$concept, ai)] <-
    mapply(function(a, l) match(l, cs$level_ids[[a]]), ai, s$fixed$level)
  arr
}

# What the respondent reads for one concept (vector of level indices):
# the level label, replaced by the conditional text where a relationship applies.
concept_text <- function(cs, x) {
  txt <- vapply(seq_len(cs$A), function(a) cs$level_labels[[a]][x[a]], character(1))
  for (rel in cs$cond) {
    v <- rel$lookup[paste0(x[rel$target], "#", paste(x[rel$on], collapse = "|"))]
    if (!is.na(v) && nzchar(v)) txt[rel$target] <- unname(v)
  }
  txt
}

# ---- feasibility (which concepts the prohibitions allow) ------------------

violates <- function(cs, X) {
  bad <- logical(nrow(X))
  for (k in cs$forb_active)
    bad <- bad | cs$forb[[k]][cbind(X[, cs$pairs[k, 1]], X[, cs$pairs[k, 2]])]
  bad
}

# All feasible concepts, or NULL when the full space is too large to list.
enumerate_feasible <- function(cs, limit = 2e5) {
  if (cs$A == 0L || cs$total > limit) return(NULL)
  X <- as.matrix(expand.grid(lapply(cs$nlev, seq_len), KEEP.OUT.ATTRS = FALSE))
  storage.mode(X) <- "integer"
  X[!violates(cs, X), , drop = FALSE]
}

# Up to n random feasible concepts by rejection sampling.
sample_feasible <- function(cs, n, max_batches = 50L) {
  out <- matrix(0L, 0L, cs$A)
  m <- max(4L * n, 64L)
  for (b in seq_len(max_batches)) {
    X <- vapply(seq_len(cs$A), function(a) sample.int(cs$nlev[a], m, replace = TRUE), integer(m))
    out <- rbind(out, X[!violates(cs, X), , drop = FALSE])
    if (nrow(out) >= n) break
  }
  out[seq_len(min(n, nrow(out))), , drop = FALSE]
}

# ---- validation: plain-language problems, tagged with the page to fix them on

issue <- function(severity, where, message)
  data.frame(severity = severity, where = where, message = message, stringsAsFactors = FALSE)

validate_study <- function(s) {
  cs <- compile_study(s)
  parts <- c(check_attributes(s, cs), check_settings(s, cs), check_prohibitions(s, cs),
             check_fixed(s, cs))
  if (!length(parts)) return(issue(character(), character(), character()))
  do.call(rbind, parts)
}

check_attributes <- function(s, cs) {
  out <- list(); add <- function(...) out[[length(out) + 1L]] <<- issue(...)
  if (cs$A == 0L) add("error", "attrs", "Add at least one attribute.")
  if (cs$A > CBC_LIMITS$attributes)
    add("error", "attrs", sprintf("A CBC study can have at most %d attributes.", CBC_LIMITS$attributes))
  if (any(!nzchar(trimws(s$attributes$name))))
    add("error", "attrs", "Every attribute needs a name.")
  if (anyDuplicated(s$attributes$name))
    add("warning", "attrs", "Two attributes share the same name.")
  for (a in seq_len(cs$A)) {
    nm <- s$attributes$name[a]; labs <- cs$level_labels[[a]]
    if (cs$nlev[a] < 2L) add("error", "attrs", sprintf("\"%s\" needs at least two levels. Add a level or remove the attribute.", nm))
    if (cs$nlev[a] > CBC_LIMITS$levels) add("error", "attrs", sprintf("\"%s\" has more than %d levels.", nm, CBC_LIMITS$levels))
    if (any(!nzchar(trimws(labs)))) add("error", "attrs", sprintf("\"%s\" has a level without a label.", nm))
    if (anyDuplicated(labs)) add("warning", "attrs", sprintf("\"%s\" has two levels with the same label.", nm))
  }
  out
}

check_settings <- function(s, cs) {
  out <- list(); add <- function(...) out[[length(out) + 1L]] <<- issue(...)
  g <- s$settings$general; d <- s$settings$design
  if (tasks_total(s$settings) < 1L) add("error", "cbc", "Add at least one choice task.")
  if (cs$A > 0L && cs$total < g$concepts)
    add("error", "attrs", sprintf("The levels allow only %.0f distinct concepts, but each task shows %d. Add levels or show fewer concepts.",
                                  cs$total, g$concepts))
  if (d$sort != "none" && is.na(d$sort_attr))
    add("error", "design", "Choose the attribute to sort concepts by, or turn concept sorting off.")
  if (cs$A > 0L && d$first_attr_once && g$concepts > cs$nlev[1])
    add("warning", "design", "The first attribute has fewer levels than there are concepts, so it must repeat within a task.")
  if (cs$A > 0L && all(cs$nlev >= 2L)) {
    k <- sum(cs$nlev - 1L) + as.integer(s$settings$none$mode != "none")
    need <- ceiling(3 * k / (g$concepts - 1L))
    if (tasks_total(s$settings) < need)
      add("warning", "cbc", sprintf("Rule of thumb: %d parameters call for about %d tasks per respondent (3 x parameters / (concepts - 1)); this study has %d.",
                                    k, need, tasks_total(s$settings)))
  }
  out
}

check_prohibitions <- function(s, cs) {
  out <- list(); add <- function(...) out[[length(out) + 1L]] <<- issue(...)
  if (length(cs$forb_active) == 0L || any(cs$nlev < 1L)) return(out)
  X <- enumerate_feasible(cs, limit = 5e4)
  if (is.null(X)) return(out)
  if (nrow(X) == 0L) {
    add("error", "prohibs", "No concept satisfies all prohibitions. Remove some of them.")
    return(out)
  }
  for (a in seq_len(cs$A)) for (l in setdiff(seq_len(cs$nlev[a]), unique(X[, a])))
    add("error", "prohibs", sprintf("\"%s\" (%s) can never be shown: the prohibitions rule out every concept containing it.",
                                    cs$level_labels[[a]][l], cs$attr_names[a]))
  if (nrow(X) < s$settings$general$concepts)
    add("error", "prohibs", sprintf("The prohibitions leave only %d allowed concepts, but each task shows %d.",
                                    nrow(X), s$settings$general$concepts))
  share <- 1 - nrow(X) / cs$total
  if (share > 0.3)
    add("warning", "prohibs", sprintf("Prohibitions remove %.0f%% of all possible concepts; heavy prohibitions weaken the design.", 100 * share))
  out
}

# Fixed tasks are the user's own choice, so a prohibited combination there is a
# warning (it may be intended), not an error.
check_fixed <- function(s, cs) {
  out <- list()
  n <- dim(cs$fixed)[1]
  if (length(cs$forb_active) == 0L || n == 0L) return(out)
  for (t in seq_len(n)) for (j in seq_len(dim(cs$fixed)[2])) {
    x <- cs$fixed[t, j, ]
    for (k in cs$forb_active) {
      a <- cs$pairs[k, 1]; b <- cs$pairs[k, 2]
      if (cs$forb[[k]][x[a], x[b]])
        out[[length(out) + 1L]] <- issue("warning", "cbc",
          sprintf("Fixed task %d, concept %d combines \"%s\" with \"%s\", which a prohibition rules out.",
                  t, j, cs$level_labels[[a]][x[a]], cs$level_labels[[b]][x[b]]))
    }
  }
  out
}

# ---- JSON -----------------------------------------------------------------

study_to_json <- function(s) {
  jsonlite::toJSON(s, auto_unbox = TRUE, pretty = TRUE, na = "null", null = "null")
}

as_tbl <- function(x, template) {
  if (!is.data.frame(x) || nrow(x) == 0L) return(template)
  cols <- lapply(names(template), function(nm) {
    v <- x[[nm]]
    if (is.null(v)) stop("This file is not a CBC study (missing column '", nm, "').", call. = FALSE)
    if (is.integer(template[[nm]])) as.integer(v) else as.character(v)
  })
  as.data.frame(stats::setNames(cols, names(template)), stringsAsFactors = FALSE)
}

parse_study <- function(x) {
  lst <- if (is.list(x)) x else tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
  if (!is.list(lst) || !all(c("attributes", "levels") %in% names(lst)))
    stop("This file is not a CBC study (no attributes and levels found).", call. = FALSE)
  s <- list(version = 1L, name = as_chr(lst$name, "Untitled study"), notes = as_chr(lst$notes, ""),
            next_id = 1L,
            attributes = as_tbl(lst$attributes, tbl$attributes()),
            levels = as_tbl(lst$levels, tbl$levels()),
            prohibitions = as_tbl(lst$prohibitions, tbl$prohibitions()),
            cond = as_tbl(lst$cond, tbl$cond()),
            fixed = as_tbl(lst$fixed, tbl$fixed()),
            settings = lst$settings)
  used <- suppressWarnings(as.integer(sub("^[al]", "", c(s$attributes$id, s$levels$id))))
  s$next_id <- max(as_int(lst$next_id, 1L), max(used, 0L, na.rm = TRUE) + 1L)
  tidy_study(s)
}
