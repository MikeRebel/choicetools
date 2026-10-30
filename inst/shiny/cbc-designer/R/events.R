# Events and state
# ---------------------------------------------------------------------------
# The browser only reports what happened ({type: "toggle_prohib", ...}). Everything that
# decides what it means lives here, as pure functions, so it is testable without a browser:
#   apply_study_event()  study + event -> new study   (NULL if the event is not a study edit)
#   apply_ui_event()     ui state + event -> new ui state
#   settings_to_inputs() / settings_from_inputs()   the CBC settings forms <-> settings list

# Events after which the editors must be rebuilt. Text edits are not: rebuilding while
# someone types would steal the cursor.
STRUCTURAL <- c("add_attr", "remove_attr", "move_attr", "add_level", "remove_level", "move_level",
                "paste_levels", "toggle_prohib", "remove_prohib", "clear_prohibs",
                "cond_create", "cond_remove")
is_structural <- function(type) type %in% STRUCTURAL

apply_study_event <- function(s, ev) {
  switch(ev$type,
    add_attr        = add_attribute(s),
    remove_attr     = remove_attribute(s, ev$id),
    move_attr       = move_attribute(s, ev$id, ev$dir),
    set_attr_name   = rename_attribute(s, ev$id, as_chr(ev$value)),
    add_level       = add_level(s, ev$attr),
    remove_level    = remove_level(s, ev$id),
    move_level      = move_level(s, ev$id, ev$dir),
    set_level_label = set_level_label(s, ev$id, as_chr(ev$value)),
    paste_levels    = set_levels_bulk(s, ev$attr, strsplit(as_chr(ev$value), "\r?\n")[[1]]),
    toggle_prohib   = toggle_prohibition(s, ev$a, ev$la, ev$b, ev$lb),
    remove_prohib   = {
      i <- as_int(ev$index, 0L)
      if (i >= 1L && i <= nrow(s$prohibitions)) s$prohibitions <- s$prohibitions[-i, , drop = FALSE]
      tidy_study(s)
    },
    clear_prohibs   = clear_prohibitions(s),
    cond_create     = set_conditional(s, ev$target, unlist(strsplit(paste(unlist(ev$on), collapse = ","), ","))),
    cond_remove     = remove_conditional(s, ev$target),
    set_cond_cell   = set_conditional_text(s, ev$target, ev$tlevel, ev$key, as_chr(ev$value)),
    set_fixed_cell  = set_fixed_cell(s, as_int(ev$task, 1L), as_int(ev$concept, 1L), ev$attr, ev$level),
    set_name        = { s$name <- as_chr(ev$value, "Untitled study"); s },
    set_notes       = { s$notes <- as_chr(ev$value); s },
    NULL)
}

# ---- UI state --------------------------------------------------------------

new_ui_state <- function(s) {
  fix_ui_state(list(page = "attrs", sel_attr = NA_character_, proh_a = NA_character_, proh_b = NA_character_,
                    cond_target = NA_character_, cond_on = character(),
                    preview = list(version = 1L, task = 1L, seed = 1L),
                    rep_a = NA_integer_, rep_b = NA_integer_, browse_version = 1L, test_n = 300L), s)
}

# Keep every selection pointing at something that still exists.
fix_ui_state <- function(u, s) {
  ids <- s$attributes$id; A <- length(ids)
  keep <- function(x, default) if (!is.na(x) && x %in% ids) x else default
  first <- if (A) ids[1] else NA_character_
  u$sel_attr <- keep(u$sel_attr, first)
  u$proh_a <- keep(u$proh_a, first)
  u$proh_b <- keep(u$proh_b, if (A >= 2L) ids[2] else NA_character_)
  if (!is.na(u$proh_a) && identical(u$proh_a, u$proh_b)) u$proh_b <- setdiff(ids, u$proh_a)[1]
  u$cond_target <- keep(u$cond_target, if (A) ids[A] else NA_character_)
  u$cond_on <- setdiff(intersect(u$cond_on, ids), u$cond_target)
  u$rep_a <- if (A >= 2L && !is.na(u$rep_a) && u$rep_a >= 1L && u$rep_a <= A) u$rep_a else 1L
  u$rep_b <- if (A >= 2L && !is.na(u$rep_b) && u$rep_b >= 1L && u$rep_b <= A && u$rep_b != u$rep_a) u$rep_b
             else setdiff(seq_len(max(A, 2L)), u$rep_a)[1]
  u
}

clamp <- function(x, lo, hi) max(lo, min(hi, x))

# `dims` = c(versions, tasks) of the current design, used to keep the respondent view inside it.
apply_ui_event <- function(u, s, ev, dims = NULL) {
  v <- u$preview
  switch(ev$type,
    goto = { u$page <- as_chr(ev$page, u$page) },
    select_attr = { u$sel_attr <- ev$id },
    set_proh = {
      if (ev$which == "a") u$proh_a <- ev$id else u$proh_b <- ev$id
      other <- if (ev$which == "a") "proh_b" else "proh_a"
      if (identical(u[[other]], ev$id)) u[[other]] <- NA_character_
    },
    cond_pick_target = { u$cond_target <- ev$id },
    cond_toggle_on = {
      u$cond_on <- if (ev$id %in% u$cond_on) setdiff(u$cond_on, ev$id) else c(u$cond_on, ev$id)
    },
    preview_go = {
      u$preview$version <- as.integer(clamp(as_int(ev$version, v$version), 1L, dims[1] %||% 1L))
      u$preview$task <- as.integer(clamp(as_int(ev$task, v$task), 1L, dims[2] %||% 1L))
    },
    preview_step = {
      if (!is.null(dims)) {
        t <- v$task + as_int(ev$by, 1L); ver <- v$version
        if (t > dims[2]) { ver <- ver + 1L; t <- 1L }
        if (t < 1L) { ver <- ver - 1L; t <- dims[2] }
        u$preview$version <- as.integer(clamp(ver, 1L, dims[1])); u$preview$task <- as.integer(t)
      }
    },
    preview_shuffle = { u$preview$seed <- v$seed + 1L },
    set_test_n = { u$test_n <- as.integer(clamp(as_int(ev$value, u$test_n), 2L, 100000L)) },
    rep_pair = { u$rep_a <- as_int(ev$a, u$rep_a); u$rep_b <- as_int(ev$b, u$rep_b) },
    browse_version = { u$browse_version <- as.integer(clamp(as_int(ev$value, 1L), 1L, dims[1] %||% 1L)) },
    NULL)
  fix_ui_state(u, s)
}

# ---- CBC settings forms <-> settings ----------------------------------------

INPUT_MAP <- list(
  n_random = c("general", "random_tasks"), n_fixed = c("general", "fixed_tasks"),
  n_concepts = c("general", "concepts"), n_columns = c("general", "columns"),
  resp_type = c("general", "response"), total_points = c("general", "total_points"),
  none_mode = c("none", "mode"), none_text = c("none", "text"), none_valign = c("none", "valign"),
  dual_question = c("none", "dual_question"), dual_yes = c("none", "dual_yes"), dual_no = c("none", "dual_no"),
  fmt_header = c("format", "header"), fmt_footer = c("format", "footer"),
  fmt_concept_label = c("format", "concept_label"), fmt_button = c("format", "button"),
  fmt_show_attr = c("format", "show_attr_names"),
  d_method = c("design", "method"), d_versions = c("design", "versions"), d_seed = c("design", "seed"),
  d_position = c("design", "position"), d_pos_first = c("design", "pos_first"), d_pos_last = c("design", "pos_last"),
  d_sort = c("design", "sort"), d_sort_attr = c("design", "sort_attr"), d_first_once = c("design", "first_attr_once")
)

settings_to_inputs <- function(settings) {
  out <- lapply(INPUT_MAP, function(path) {
    v <- settings[[path[1]]][[path[2]]]
    if (length(v) == 1L && is.na(v)) "" else v
  })
  out$fixed_positions <- paste(settings$fixed$positions, collapse = ", ")
  out
}

parse_positions <- function(text) {
  v <- suppressWarnings(as.integer(trimws(strsplit(as_chr(text), "[,; ]+")[[1]])))
  v[!is.na(v)]
}

settings_from_inputs <- function(inp, base) {
  merged <- base
  for (id in names(INPUT_MAP)) {
    v <- inp[[id]]
    if (is.null(v) || length(v) == 0L || (length(v) == 1L && is.na(v))) next
    path <- INPUT_MAP[[id]]
    merged[[path[1]]][[path[2]]] <- v
  }
  if (!is.null(inp$fixed_positions)) merged$fixed$positions <- parse_positions(inp$fixed_positions)
  sanitize_settings(merged, d = base)
}
