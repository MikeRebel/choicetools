# The pages: pure functions from study / ui state / design to HTML.
# Controls never hold state of their own: they send events (data-evt) that events.R interprets.
# ---------------------------------------------------------------------------

cls <- function(...) { x <- c(...); paste(x[nzchar(x)], collapse = " ") }

icon <- function(name) {
  d <- switch(name, up = "M4 10l4-4 4 4", down = "M4 6l4 4 4-4", x = "M4 4l8 8M12 4l-8 8",
              plus = "M8 3v10M3 8h10", "")
  HTML(sprintf('<svg class="ic" viewBox="0 0 16 16" aria-hidden="true"><path d="%s"/></svg>', d))
}
btn <- function(label, evt = NULL, class = "btn", ...)
  tags$button(type = "button", class = class, `data-evt` = evt, ..., label)
icon_btn <- function(name, evt, title, ...)
  tags$button(type = "button", class = "icon-btn", title = title, `aria-label` = title,
              `data-evt` = evt, ..., icon(name))

page_head <- function(title, lead) div(class = "page-head", tags$h1(title), tags$p(class = "lead", lead))

issue_list <- function(issues, where) {
  d <- issues[issues$where %in% where, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  div(class = "issues", role = "status", lapply(seq_len(nrow(d)), function(i)
    div(class = paste("issue", d$severity[i]), tags$span(class = "issue-mark"), tags$span(d$message[i]))))
}

empty_state <- function(title, text, ...) div(class = "empty", tags$h3(title), tags$p(text), ...)

lbl <- function(s, level_id) s$levels$label[match(level_id, s$levels$id)]
aname <- function(s, attr_id) s$attributes$name[match(attr_id, s$attributes$id)]

# ---- the rail ---------------------------------------------------------------

ui_rail <- function(s, ui, issues, design_state) {
  dot <- function(pages) {
    d <- issues[issues$where %in% pages, , drop = FALSE]
    if (any(d$severity == "error")) tags$span(class = "dot err", title = "Needs attention")
    else if (nrow(d)) tags$span(class = "dot warn", title = "Has warnings")
  }
  item <- function(page, label, meta = NULL, pages = page, extra = NULL)
    tags$button(type = "button", class = cls("rail-item", if (identical(ui$page, page)) "active"),
                `data-evt` = "goto", `data-page` = page, `aria-current` = if (identical(ui$page, page)) "page",
                tags$span(class = "rail-label", label), dot(pages), extra,
                if (!is.null(meta)) tags$span(class = "rail-meta", meta))
  stale <- identical(design_state, "stale")
  tags$nav(class = "rail", `aria-label` = "Study",
    div(class = "rail-group", "Study"),
    item("notes", "Study notes"),
    item("attrs", "Attributes", nrow(s$attributes)),
    item("prohibs", "Prohibitions", nrow(s$prohibitions)),
    item("cond", "Conditional relationships", length(unique(s$cond$target))),
    item("cbc", "CBC settings", pages = c("cbc", "design")),
    div(class = "rail-group", "Design"),
    item("results", "Design and test", extra = if (stale) tags$span(class = "dot warn", title = "Out of date")))
}

# ---- study notes -------------------------------------------------------------

ui_page_notes <- function(s, issues) {
  cs <- compile_study(s); g <- s$settings$general
  feas <- if (cs$A) enumerate_feasible(cs) else NULL
  k <- if (cs$A) sum(pmax(cs$nlev - 1L, 0L)) else 0L
  stat <- function(value, label) div(class = "stat", tags$b(value), tags$span(label))
  div(class = "page",
    page_head("Study notes", "Notes stay with the study file. Checks lists everything that needs attention before a design is generated."),
    div(class = "field", tags$label(`for` = "study-notes", "Notes"),
        tags$textarea(id = "study-notes", class = "txt area", rows = 5, `data-evt` = "set_notes", s$notes)),
    div(class = "stats",
        stat(cs$A, "attributes"), stat(sum(cs$nlev), "levels"), stat(k, "part-worth parameters"),
        stat(tasks_total(s$settings), sprintf("tasks of %d concepts", g$concepts)),
        stat(format(if (is.null(feas)) cs$total else nrow(feas), big.mark = ","), "possible concepts")),
    tags$h2("Checks"),
    if (nrow(issues)) div(class = "issues", lapply(seq_len(nrow(issues)), function(i)
      div(class = paste("issue", issues$severity[i]), tags$span(class = "issue-mark"), tags$span(issues$message[i]),
          btn("Open", "goto", class = "btn btn-link", `data-page` = if (issues$where[i] == "design") "cbc" else issues$where[i]))))
    else div(class = "issue ok", tags$span(class = "issue-mark"), tags$span("No problems found.")))
}

# ---- attributes ---------------------------------------------------------------

attr_item <- function(s, i, sel) {
  id <- s$attributes$id[i]; n <- sum(s$levels$attr == id)
  div(class = cls("attr-item", if (identical(id, sel)) "active"), role = "listitem",
      `data-evt` = "select_attr", `data-id` = id,
      tags$span(class = "ai-name", s$attributes$name[i]), tags$span(class = "ai-count", n),
      div(class = "ai-actions",
          icon_btn("up", "move_attr", "Move attribute up", `data-id` = id, `data-dir` = "up"),
          icon_btn("down", "move_attr", "Move attribute down", `data-id` = id, `data-dir` = "down"),
          icon_btn("x", "remove_attr", "Remove attribute", `data-id` = id,
                   `data-confirm` = sprintf("Remove \"%s\" and every prohibition and relationship that uses it?", s$attributes$name[i]))))
}

attr_editor <- function(s, id) {
  lv <- s$levels[s$levels$attr == id, , drop = FALSE]
  div(class = "attr-editor",
    tags$label(class = "field-label", `for` = "attr-name", "Attribute name"),
    tags$input(id = "attr-name", type = "text", class = "txt title-input", value = aname(s, id),
               `data-evt` = "set_attr_name", `data-id` = id),
    tags$h2("Levels"),
    div(class = "lvl-list", lapply(seq_len(nrow(lv)), function(i)
      div(class = "lvl-row", tags$span(class = "idx", i),
          tags$input(type = "text", class = "txt", value = lv$label[i], `aria-label` = paste("Level", i),
                     `data-evt` = "set_level_label", `data-id` = lv$id[i]),
          icon_btn("up", "move_level", "Move level up", `data-id` = lv$id[i], `data-dir` = "up"),
          icon_btn("down", "move_level", "Move level down", `data-id` = lv$id[i], `data-dir` = "down"),
          icon_btn("x", "remove_level", "Remove level", `data-id` = lv$id[i])))),
    btn(tagList(icon("plus"), "Add level"), "add_level", class = "btn btn-quiet", `data-attr` = id),
    tags$details(class = "paste", tags$summary("Paste a list of levels"),
      tags$textarea(id = paste0("paste-", id), class = "txt area", rows = 5, placeholder = "One level per line"),
      btn("Replace levels", "paste_levels", class = "btn", `data-attr` = id, `data-source` = paste0("#paste-", id))))
}

ui_page_attrs <- function(s, ui, issues) {
  sel <- ui$sel_attr
  div(class = "page",
    page_head("Attributes", "Level order is the order of the part-worths and of the level numbers in an exported design."),
    issue_list(issues, "attrs"),
    div(class = "split",
      div(class = "attr-list", role = "list",
          lapply(seq_len(nrow(s$attributes)), function(i) attr_item(s, i, sel)),
          btn(tagList(icon("plus"), "Add attribute"), "add_attr", class = "btn btn-quiet")),
      if (!is.na(sel)) attr_editor(s, sel) else empty_state("No attributes", "Add an attribute to start.")))
}

# ---- prohibitions -------------------------------------------------------------

attr_select <- function(s, which, current, other) {
  tags$select(class = "sel", `data-evt` = "set_proh", `data-which` = which,
    lapply(s$attributes$id, function(id)
      tags$option(value = id, selected = if (identical(id, current)) "selected", aname(s, id))))
}

ui_page_prohibs <- function(s, ui, issues) {
  head <- page_head("Prohibitions", "A prohibited pair of levels is never shown together in a random task. Use as few as possible: every prohibition costs precision.")
  if (nrow(s$attributes) < 2L)
    return(div(class = "page", head, empty_state("Two attributes needed", "Add a second attribute to combine its levels with the first.")))
  a <- ui$proh_a; b <- ui$proh_b
  la <- s$levels[s$levels$attr == a, , drop = FALSE]; lb <- s$levels[s$levels$attr == b, , drop = FALSE]
  p <- s$prohibitions
  on <- function(x, y) any(p$la == x & p$lb == y & p$a == a & p$b == b | p$la == y & p$lb == x & p$a == b & p$b == a)
  n_on <- sum(outer(la$id, lb$id, Vectorize(on)))
  div(class = "page",
    head, issue_list(issues, "prohibs"),
    div(class = "pair-pick", attr_select(s, "a", a), tags$span(class = "times", "with"), attr_select(s, "b", b)),
    div(class = "grid-scroll",
      tags$table(class = "proh-grid",
        tags$thead(tags$tr(tags$th(), lapply(lb$label, function(x) tags$th(x)))),
        tags$tbody(lapply(seq_len(nrow(la)), function(i) tags$tr(
          tags$th(la$label[i]),
          lapply(seq_len(nrow(lb)), function(j) {
            hit <- on(la$id[i], lb$id[j])
            tags$td(tags$button(type = "button", class = if (hit) "cell on" else "cell", `data-evt` = "toggle_prohib",
                                `aria-pressed` = tolower(hit), title = paste(la$label[i], "with", lb$label[j]),
                                `data-a` = a, `data-la` = la$id[i], `data-b` = b, `data-lb` = lb$id[j],
                                if (hit) icon("x")))
          })))))),
    tags$p(class = "count", sprintf("%d of %d combinations of %s and %s are allowed.", nrow(la) * nrow(lb) - n_on,
                               nrow(la) * nrow(lb), aname(s, a), aname(s, b))),
    tags$h2(sprintf("All prohibitions (%d)", nrow(p))),
    if (nrow(p)) div(class = "chips", lapply(seq_len(nrow(p)), function(i)
      div(class = "chip", tags$span(sprintf("%s: %s with %s: %s", aname(s, p$a[i]), lbl(s, p$la[i]), aname(s, p$b[i]), lbl(s, p$lb[i]))),
          icon_btn("x", "remove_prohib", "Remove prohibition", `data-index` = i))),
      btn("Clear all", "clear_prohibs", class = "btn btn-link"))
    else tags$p(class = "hint", "None yet. Click a cell above to prohibit that pair."))
}

# ---- conditional relationships ------------------------------------------------

cond_table <- function(s, target) {
  d <- s$cond[s$cond$target == target, , drop = FALSE]
  tl <- s$levels[s$levels$attr == target, , drop = FALSE]
  keys <- unique(d$key)
  key_label <- function(k) paste(lbl(s, strsplit(k, "|", fixed = TRUE)[[1]]), collapse = " / ")
  div(class = "grid-scroll", tags$table(class = "cond-grid",
    tags$thead(tags$tr(tags$th(), lapply(keys, function(k) tags$th(key_label(k))))),
    tags$tbody(lapply(seq_len(nrow(tl)), function(i) tags$tr(
      tags$th(tl$label[i]),
      lapply(keys, function(k) {
        r <- d[d$tlevel == tl$id[i] & d$key == k, , drop = FALSE]
        tags$td(tags$input(type = "text", class = "txt", value = r$text, `aria-label` = paste(tl$label[i], key_label(k)),
                           `data-evt` = "set_cond_cell", `data-target` = target, `data-tlevel` = tl$id[i], `data-key` = k))
      }))))))
}

ui_page_cond <- function(s, ui, issues) {
  head <- page_head("Conditional relationships", "Show different text for one attribute depending on the levels of others, for example prices that depend on brand. Estimation still uses the attribute's own level numbers.")
  if (nrow(s$attributes) < 2L)
    return(div(class = "page", head, empty_state("Two attributes needed", "Add a second attribute to make one depend on another.")))
  targets <- unique(s$cond$target); t <- ui$cond_target; on <- ui$cond_on
  others <- setdiff(s$attributes$id, t)
  div(class = "page", head, issue_list(issues, "cond"),
    lapply(targets, function(tg) div(class = "cond-block",
      div(class = "cond-head", tags$h2(sprintf("%s depends on %s", aname(s, tg),
                                               paste(aname(s, strsplit(s$cond$on[s$cond$target == tg][1], "|", fixed = TRUE)[[1]]), collapse = " and "))),
          btn("Remove", "cond_remove", class = "btn btn-link", `data-target` = tg)),
      cond_table(s, tg))),
    tags$h2("New relationship"),
    div(class = "cond-new",
      tags$label(class = "field-label", "Attribute whose text changes",
        tags$select(class = "sel", `data-evt` = "cond_pick_target",
          lapply(s$attributes$id, function(id) tags$option(value = id, selected = if (identical(id, t)) "selected", aname(s, id))))),
      div(class = "field-label", "Depends on",
        div(class = "checks", lapply(others, function(id) tags$label(class = "check",
          tags$input(type = "checkbox", checked = if (id %in% on) NA, `data-evt` = "cond_toggle_on", `data-id` = id), aname(s, id))))),
      btn("Create relationship", "cond_create", class = "btn btn-primary", `data-target` = t,
          `data-on` = paste(on, collapse = ","), disabled = if (!length(on)) NA)))
}

# ---- fixed tasks --------------------------------------------------------------

ui_fixed_editor <- function(s) {
  cs <- compile_study(s); g <- s$settings$general; fpos <- fixed_positions(s$settings)
  if (g$fixed_tasks == 0L || cs$A == 0L)
    return(empty_state("No fixed tasks", "Set the number of fixed tasks on the General tab to add hand-picked tasks."))
  div(class = "fixed-tasks", lapply(seq_len(g$fixed_tasks), function(t) {
    rows <- lapply(seq_len(cs$A), function(a) tags$tr(tags$th(cs$attr_names[a]),
      lapply(seq_len(g$concepts), function(j) tags$td(tags$select(class = "sel",
        `aria-label` = sprintf("%s, concept %d", cs$attr_names[a], j),
        `data-evt` = "set_fixed_cell", `data-task` = t, `data-concept` = j, `data-attr` = cs$attr_ids[a],
        lapply(seq_len(cs$nlev[a]), function(l) tags$option(value = cs$level_ids[[a]][l],
               selected = if (cs$fixed[t, j, a] == l) "selected", cs$level_labels[[a]][l])))))))
    div(class = "fixed-task", tags$h3(sprintf("Fixed task %d, shown as task %d", t, fpos[t])),
        div(class = "grid-scroll", tags$table(class = "fixed-grid",
          tags$thead(tags$tr(tags$th(), lapply(seq_len(g$concepts), function(j) tags$th(paste("Concept", j))))),
          tags$tbody(rows))))
  }))
}

# ---- results: the test report and the design browser ------------------------

se_row <- function(label, se) {
  rating <- rate_se(se); w <- if (is.na(se)) 100 else min(100, 100 * se / 0.10)
  div(class = "se-row", tags$span(class = "se-label", label),
      div(class = "se-track", tags$span(class = paste("se-fill", gsub(" ", "-", tolower(rating))), style = sprintf("width:%.1f%%", w)),
          tags$span(class = "se-mark", style = "left:30%"), tags$span(class = "se-mark", style = "left:50%")),
      tags$span(class = "se-val", if (is.na(se)) "n/a" else sprintf("%.3f", se)),
      tags$span(class = paste("rate", gsub(" ", "-", tolower(rating))), rating))
}

bar_row <- function(label, count, pct) {
  off <- abs(pct - 100) > 10
  div(class = "bal-row", tags$span(class = "se-label", label),
      div(class = "bal-track", tags$span(class = cls("bal-fill", if (off) "off"), style = sprintf("width:%.1f%%", min(100, pct / 2))),
          tags$span(class = "bal-mid")),
      tags$span(class = "se-val", format(count, big.mark = ",")), tags$span(class = "pct", sprintf("%.0f%%", pct)))
}

section <- function(title, note, ...) div(class = "section", tags$h2(title), if (!is.null(note)) tags$p(class = "lead", note), ...)

ui_report <- function(s, d, r, ui) {
  cs <- compile_study(s); sm <- r$summary
  se_groups <- lapply(seq_len(cs$A), function(a) {
    x <- r$se[r$se$attr == a, ]
    div(class = "group", tags$h3(cs$attr_names[a]), lapply(seq_len(nrow(x)), function(i) se_row(x$label[i], x$se[i])))
  })
  bal_groups <- lapply(seq_len(cs$A), function(a) {
    x <- r$oneway[r$oneway$attr == a, ]
    div(class = "group", tags$h3(cs$attr_names[a]), lapply(seq_len(nrow(x)), function(i) bar_row(x$label[i], x$count[i], x$pct[i])))
  })
  ra <- ui$rep_a; rb <- ui$rep_b
  pair_key <- paste(min(ra, rb), max(ra, rb), sep = "_"); pm <- r$pairs$tables[[pair_key]]
  assoc <- r$pairs$assoc
  heat <- tags$table(class = "assoc", tags$thead(tags$tr(tags$th(), lapply(cs$attr_names, tags$th))),
    tags$tbody(lapply(seq_len(cs$A), function(i) tags$tr(tags$th(cs$attr_names[i]), lapply(seq_len(cs$A), function(j)
      if (i == j) tags$td(class = "diag") else tags$td(tags$button(type = "button",
        class = cls("heat", if ((i == ra && j == rb) || (i == rb && j == ra)) "sel"), `data-evt` = "rep_pair", `data-a` = i, `data-b` = j,
        style = sprintf("background:rgba(11,110,117,%.2f)", min(0.85, assoc[i, j] / 0.3)), sprintf("%.2f", assoc[i, j]))))))))
  exp_cell <- if (!is.null(pm)) sum(pm) / length(pm)
  div(class = "report",
    div(class = "chips summary",
        tags$span(class = "chip", sprintf("%d versions", sm$versions)),
        tags$span(class = "chip", sprintf("%d random + %d fixed tasks", sm$random_tasks, sm$fixed_tasks)),
        tags$span(class = "chip", sprintf("%d concepts per task", sm$concepts)),
        tags$span(class = "chip", paste("Method:", sm$method))),
    section("Standard errors",
      "Expected precision of the part-worths if respondents answered at random, the same assumption as Sawtooth's Test Design. Aim for the largest standard error to be about 0.05, preferably 0.03; the two ticks mark those values.",
      div(class = "n-input", tags$label("Respondents",
        tags$input(type = "number", class = "num", min = 2, value = sm$n, `data-evt` = "set_test_n"))),
      if (!r$estimable) div(class = "issues", div(class = "issue error", tags$span(class = "issue-mark"),
        tags$span("The design is not estimable: a level is never shown or two attributes are perfectly confounded. Loosen prohibitions or add tasks."))),
      se_groups,
      if (!is.na(r$none_se)) div(class = "group", tags$h3("None"), se_row("None constant", r$none_se))),
    section("Level balance", "How often each level appears in the random tasks. The mid line is an even share; bars outside 90 to 110 percent are amber.", bal_groups),
    section("Pairs of attributes",
      "Cramer's V between attributes, 0 when every pair of levels appears equally often. Prohibitions raise it. Click a cell to see the counts.",
      div(class = "pairs", div(class = "grid-scroll", heat),
        if (!is.null(pm)) div(class = "grid-scroll", tags$table(class = "counts",
          tags$caption(sprintf("%s by %s", cs$attr_names[min(ra, rb)], cs$attr_names[max(ra, rb)])),
          tags$thead(tags$tr(tags$th(), lapply(colnames(pm), tags$th))),
          tags$tbody(lapply(seq_len(nrow(pm)), function(i) tags$tr(tags$th(rownames(pm)[i]), lapply(seq_len(ncol(pm)), function(j)
            tags$td(class = cls(if (pm[i, j] == 0) "zero"), pm[i, j])))))))
        else NULL)),
    section("Overlap and position", "Overlap is a level repeated inside a task. Position balance checks that levels appear equally in every concept column.",
      tags$table(class = "plain", tags$thead(tags$tr(tags$th("Attribute"), tags$th("Tasks repeating a level"), tags$th("Largest position deviation"))),
        tags$tbody(lapply(seq_len(cs$A), function(a) tags$tr(tags$th(cs$attr_names[a]),
          tags$td(sprintf("%.0f%%%s", r$overlap$repeat_pct[a], if (r$overlap$unavoidable[a]) " (unavoidable)" else "")),
          tags$td(sprintf("%.0f%%%s", r$position$max_dev_pct[a], if (r$position$sorted[a]) " (sorted by this attribute)" else ""))))))),
    section("Sample size rule of thumb", NULL,
      tags$p(class = if (r$sanity$ok) "ok-text" else "warn-text",
        sprintf("Respondents x tasks x concepts / largest number of levels = %.0f. The Johnson and Orme guideline for main effects is at least 500, so this %s.",
                r$sanity$value, if (r$sanity$ok) "passes" else "falls short"))))
}

ui_browse <- function(s, d, ui) {
  cs <- compile_study(s); versions <- sort(unique(d$table$Version))
  vi <- clamp(ui$browse_version, 1L, length(versions)); v <- versions[vi]
  rows <- d$table[d$table$Version == v, , drop = FALSE]; ord <- d$attr_order[vi, ]
  section("Browse the design", "The tasks of one questionnaire version, with attributes in that respondent's display order.",
    div(class = "n-input", tags$label("Version", tags$input(type = "number", class = "num", min = 1, max = length(versions),
                                                              value = vi, `data-evt` = "browse_version"))),
    div(class = "grid-scroll", tags$table(class = "browse",
      tags$thead(tags$tr(tags$th("Task"), lapply(seq_len(max(rows$Concept)), function(j) tags$th(paste("Concept", j))))),
      tags$tbody(lapply(sort(unique(rows$Task)), function(t) {
        r <- rows[rows$Task == t, , drop = FALSE]
        tags$tr(tags$th(t, if (r$Fixed[1]) tags$span(class = "badge", "Fixed")),
          lapply(seq_len(nrow(r)), function(j) tags$td(lapply(ord, function(a)
            div(concept_text(cs, as.integer(unlist(r[j, paste0("Att", seq_len(cs$A))])))[a])))))
      })))))
}

ui_page_results <- function(s, d, r, ui) {
  head <- page_head("Design and test", "Generate a design, then check how balanced and how precise it is before you field it.")
  gen <- btn("Generate design", class = "btn btn-primary", `data-click` = "btn_generate")
  if (is.null(d)) return(div(class = "page", head, empty_state("No design yet", "Generate one from the current attributes, prohibitions and CBC settings.", gen)))
  if (!design_is_current(d, s) || is.null(r))
    return(div(class = "page", head, empty_state("The design is out of date", "The study changed after this design was generated. Generate it again to refresh the report.", gen)))
  div(class = "page", head, ui_report(s, d, r, ui), ui_browse(s, d, ui))
}
