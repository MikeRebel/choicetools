# The respondent view: one choice task as the respondent would see it.
# ---------------------------------------------------------------------------

# A sample task from the feasible concepts (before any design exists).
sample_task <- function(s, seed) {
  cs <- compile_study(s)
  if (cs$A == 0L) return(list(X = matrix(0L, 0L, 0L), order = integer()))
  J <- s$settings$general$concepts
  X <- with_seed(seed, {
    m <- sample_feasible(cs, J)
    while (nrow(m) < J) m <- rbind(m, vapply(cs$nlev, function(k) sample.int(k, 1L), integer(1)))
    m
  })
  list(X = X, order = seq_len(cs$A))
}

# What the respondent view shows: the chosen version/task of a current design,
# otherwise a reproducible sample.
task_view <- function(s, design, preview) {
  cs <- compile_study(s); n_tasks <- tasks_total(s$settings)
  if (cs$A > 0L && design_is_current(design, s)) {
    versions <- sort(unique(design$table$Version))
    vi <- clamp(preview$version, 1L, length(versions)); t <- clamp(preview$task, 1L, n_tasks)
    rows <- design$table[design$table$Version == versions[vi] & design$table$Task == t, , drop = FALSE]
    return(list(X = unname(as.matrix(rows[paste0("Att", seq_len(cs$A))])), order = design$attr_order[vi, ],
                task_no = t, n_tasks = n_tasks, fixed = rows$Fixed[1], version = versions[vi], source = "design"))
  }
  c(sample_task(s, preview$seed),
    list(task_no = 1L, n_tasks = NULL, fixed = FALSE, version = NA_integer_, source = "sample"))
}

radio_mark <- function() tags$span(class = "radio", `aria-hidden` = "true")

task_control <- function(settings) {
  if (settings$general$response == "sum")
    tags$input(type = "number", class = "alloc", min = 0, max = settings$general$total_points,
               value = 0, readonly = NA, `aria-label` = "Points")
  else tagList(radio_mark(), tags$span(class = "pick-label", settings$format$button))
}

# One table of concept columns (and possibly the None column).
task_block <- function(cs, settings, texts, order, cols) {
  fmt <- settings$format; non <- settings$none
  head <- lapply(cols, function(cc)
    if (identical(cc, "none")) tags$th(class = "none-head")
    else tags$th(class = "concept", gsub("{n}", cc, fmt$concept_label, fixed = TRUE)))
  rows <- lapply(seq_along(order), function(i) {
    a <- order[i]
    tags$tr(
      if (fmt$show_attr_names) tags$th(class = "attr-name", cs$attr_names[a]),
      lapply(cols, function(cc) {
        if (!identical(cc, "none")) return(tags$td(class = "cell", texts[[cc]][a]))
        if (i == 1L) tags$td(class = "none", rowspan = length(order),
                             style = paste0("vertical-align:", non$valign), non$text)
      }))
  })
  pick <- tags$tr(class = "pick", if (fmt$show_attr_names) tags$th(),
                  lapply(cols, function(cc) tags$td(task_control(settings))))
  tags$table(class = "task-grid",
             tags$thead(tags$tr(if (fmt$show_attr_names) tags$th(), head)),
             tags$tbody(rows, pick))
}

ui_task <- function(cs, settings, X, order = seq_len(cs$A), task_no = 1L, n_tasks = NULL, fixed = FALSE) {
  if (cs$A == 0L) return(div(class = "task task-empty", tags$p("Add attributes to see the respondent view.")))
  fmt <- settings$format; non <- settings$none
  X <- matrix(as.integer(X), ncol = cs$A)
  texts <- lapply(seq_len(nrow(X)), function(j) concept_text(cs, X[j, ]))
  cols <- c(as.list(seq_len(nrow(X))), if (non$mode == "traditional") list("none"))
  chunks <- split(seq_along(cols), ceiling(seq_along(cols) / max(1L, settings$general$columns)))
  div(class = "task",
    div(class = "task-meta",
        tags$span(class = "task-no", if (is.null(n_tasks)) "Sample task" else sprintf("Task %d of %d", task_no, n_tasks)),
        if (isTRUE(fixed)) tags$span(class = "badge", "Fixed task")),
    tags$p(class = "task-prompt", fmt$header),
    lapply(unname(chunks), function(idx) task_block(cs, settings, texts, order, cols[idx])),
    if (settings$general$response == "sum")
      div(class = "sum-total", sprintf("Total points: 0 of %d", settings$general$total_points)),
    if (non$mode == "dual")
      div(class = "dual", tags$p(class = "dual-q", non$dual_question),
          div(class = "dual-opts", tags$span(radio_mark(), non$dual_yes), tags$span(radio_mark(), non$dual_no))),
    if (nzchar(fmt$footer)) tags$p(class = "task-foot", fmt$footer))
}

# The side panel: the task plus the controls to move through the design.
ui_aside <- function(s, design, ui) {
  cs <- compile_study(s); v <- task_view(s, design, ui$preview)
  current <- identical(v$source, "design")
  controls <- if (current) {
    n_versions <- length(unique(design$table$Version))
    div(class = "aside-controls",
      tags$label("Version", tags$input(type = "number", class = "num", min = 1, max = n_versions, value = v$version,
                                       `data-evt` = "preview_go", `data-field` = "version", `data-task` = v$task_no)),
      tags$label("Task", tags$input(type = "number", class = "num", min = 1, max = v$n_tasks, value = v$task_no,
                                    `data-evt` = "preview_go", `data-field` = "task", `data-version` = v$version)),
      icon_btn("up", "preview_step", "Previous task", `data-by` = -1),
      icon_btn("down", "preview_step", "Next task", `data-by` = 1))
  } else {
    div(class = "aside-controls",
        tags$span(class = "hint", if (is.null(design)) "No design yet, showing a sample task." else "The design is out of date, showing a sample task."),
        btn("Another sample", "preview_shuffle", class = "btn btn-quiet"))
  }
  tags$aside(class = "aside", `aria-label` = "Respondent view",
    div(class = "aside-head", tags$h2("Respondent view"), controls),
    div(class = "stage", ui_task(cs, s$settings, v$X, v$order, v$task_no %||% 1L, v$n_tasks, v$fixed)))
}
