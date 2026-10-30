# The static page: top bar, rail, page containers, action bar, and the CBC settings forms.
# Pages that depend on the study are filled in by the server (see server.R).
# ---------------------------------------------------------------------------

# How each settings control is updated when a study is opened.
INPUT_KIND <- c(
  n_random = "num", n_fixed = "num", n_concepts = "num", n_columns = "num", resp_type = "radio",
  total_points = "num", none_mode = "radio", none_text = "area", none_valign = "sel",
  dual_question = "area", dual_yes = "text", dual_no = "text",
  fmt_header = "area", fmt_footer = "area", fmt_concept_label = "text", fmt_button = "text", fmt_show_attr = "chk",
  d_method = "sel", d_versions = "num", d_seed = "num", d_position = "radio", d_pos_first = "num", d_pos_last = "num",
  d_sort = "radio", d_sort_attr = "sel", d_first_once = "chk", fixed_positions = "text")

# TRUE = the control is greyed out because the choices above it make it irrelevant.
disabled_inputs <- function(s) {
  d <- s$settings$design; n <- s$settings$none
  c(d_pos_first = d$position != "subset", d_pos_last = d$position != "subset",
    d_sort_attr = d$sort == "none",
    d_first_once = !(nrow(s$prohibitions) > 0L && d$method %in% c("complete", "shortcut")),
    total_points = s$settings$general$response != "sum",
    none_text = n$mode != "traditional", none_valign = n$mode != "traditional",
    dual_question = n$mode != "dual", dual_yes = n$mode != "dual", dual_no = n$mode != "dual")
}

# ---- form controls (plain HTML; Shiny binds them by id) -----------------------

fld <- function(label, control, note = NULL)
  tags$label(class = "fld", tags$span(class = "fld-label", label), control,
             if (!is.null(note)) tags$span(class = "fld-note", note))
num <- function(id, value, min = NULL, max = NULL, step = 1)
  tags$input(id = id, type = "number", class = "num", value = value, min = min, max = max, step = step)
txt <- function(id, value) tags$input(id = id, type = "text", class = "txt", value = value)
area <- function(id, value, rows = 2) tags$textarea(id = id, class = "txt area", rows = rows, value)
sel <- function(id, choices, selected)
  tags$select(id = id, class = "sel", lapply(seq_along(choices), function(i)
    tags$option(value = choices[[i]], selected = if (identical(choices[[i]], selected)) "selected", names(choices)[i])))
chk <- function(id, label, value, note = NULL)
  tags$label(class = "check", tags$input(id = id, type = "checkbox", checked = if (isTRUE(value)) NA),
             tags$span(label, if (!is.null(note)) tags$span(class = "fld-note", note)))
radio_group <- function(id, choices, selected, notes = list())
  div(id = id, class = "shiny-input-radiogroup opts", role = "radiogroup",
    lapply(seq_along(choices), function(i) tags$label(class = "opt",
      tags$input(type = "radio", name = id, value = choices[[i]], checked = if (identical(choices[[i]], selected)) NA),
      tags$span(names(choices)[i], if (!is.null(notes[[choices[[i]]]])) tags$span(class = "fld-note", notes[[choices[[i]]]])))))
grp <- function(title, ...) div(class = "grp", tags$h3(title), ...)

settings_panel <- function(id, ...) tags$section(class = "panel", `data-panel` = id, hidden = if (id != "general") NA, ...)

cbc_settings_ui <- function(d) {
  tabs <- c(general = "General", none = "None option", format = "Format", design = "Design",
            fixed = "Fixed tasks", io = "Import / export design")
  div(class = "page",
    page_head("CBC settings", "How many tasks each respondent sees, what a task looks like, and how the design is built."),
    uiOutput("cbc_issues"),
    div(class = "tabbed",
      div(class = "tablist", role = "tablist", lapply(names(tabs), function(id)
        tags$button(type = "button", role = "tab", class = cls("tab", if (id == "general") "on"), `data-tab` = id, tabs[[id]]))),
      div(class = "tabpanels",
        settings_panel("general", div(class = "cols",
          div(grp("Task types", fld("Number of random choice tasks", num("n_random", d$general$random_tasks, 0, 60)),
                  fld("Number of fixed choice tasks", num("n_fixed", d$general$fixed_tasks, 0, 20))),
              grp("Choice task", fld("Concepts per task, excluding None", num("n_concepts", d$general$concepts, 2, 12)),
                  fld("Number of columns", num("n_columns", d$general$columns, 1, 12), "Columns of concepts before the task wraps onto another block."))),
          grp("Response type", radio_group("resp_type", c("Discrete choice (single select button)" = "choice",
                                                          "Constant sum / allocation (numeric entry box)" = "sum"), d$general$response),
              fld("Points to allocate", num("total_points", d$general$total_points, 2, 10000))))),
        settings_panel("none", div(class = "cols",
          grp("None option", radio_group("none_mode", c("Do not include a None option" = "none", "Traditional None option" = "traditional",
                                                         "Dual-response None option" = "dual"), d$none$mode,
                 notes = list(dual = "Respondents pick a concept, then say whether they would actually buy it."))),
          div(grp("Traditional None option", fld("Text shown in the None column", area("none_text", d$none$text, 3)),
                  fld("Vertical alignment", sel("none_valign", c(Top = "top", Middle = "middle", Bottom = "bottom"), d$none$valign))),
              grp("Dual-response None option", fld("Follow-up question", area("dual_question", d$none$dual_question, 2)),
                  fld("Answer for buying", txt("dual_yes", d$none$dual_yes)), fld("Answer for not buying", txt("dual_no", d$none$dual_no)))))),
        settings_panel("format", div(class = "cols",
          grp("Text", fld("Header above the task", area("fmt_header", d$format$header, 2)),
              fld("Footer below the task", area("fmt_footer", d$format$footer, 2))),
          grp("Labels", fld("Concept label", txt("fmt_concept_label", d$format$concept_label), "{n} becomes the concept number."),
              fld("Select button text", txt("fmt_button", d$format$button)),
              chk("fmt_show_attr", "Show attribute names", d$format$show_attr_names)))),
        settings_panel("design", div(class = "cols",
          div(grp("Experimental design creation settings",
                  fld("Random task generation method", sel("d_method", c("Complete enumeration" = "complete", "Shortcut" = "shortcut",
                       "Balanced overlap" = "balanced", "Random" = "random"), d$design$method)),
                  fld("Number of questionnaire versions", num("d_versions", d$design$versions, 1, 10000)),
                  fld("Design seed", num("d_seed", d$design$seed, 0, 999999999),
                      "The seed makes a design repeatable. A new seed gives a different design.")),
              grp("Randomize attribute position within concepts",
                  radio_group("d_position", c("Do not randomize attribute order" = "none", "Randomize the order of all attributes" = "all",
                                              "Randomize the order of a subset of the attributes" = "subset"), d$design$position),
                  div(class = "inline", fld("First randomized attribute", num("d_pos_first", d$design$pos_first, 1, 30)),
                      fld("Last randomized attribute", num("d_pos_last", d$design$pos_last, 1, 30))))),
          div(grp("Concept sorting within tasks",
                  radio_group("d_sort", c("No concept sorting (full randomization)" = "none",
                                          "Sort concepts using a randomized level order for a specific attribute" = "random",
                                          "Sort concepts using the natural level order for a specific attribute" = "natural"), d$design$sort,
                    notes = list(random = "The level order changes from respondent to respondent but stays constant across that respondent's tasks.",
                                 natural = "The natural level order is used for every respondent.")),
                  fld("Attribute to sort concepts by", sel("d_sort_attr", c("Choose an attribute" = ""), ""))),
              grp("Additional settings", chk("d_first_once", "Show levels of the first attribute no more than once per task",
                    d$design$first_attr_once, "Only affects complete enumeration and shortcut when prohibitions are in use."))))),
        settings_panel("fixed", fld("Positions of the fixed tasks", txt("fixed_positions", ""),
                                    "Task numbers separated by commas, for example 5, 10. Leave empty to spread them evenly."),
                       uiOutput("fixed_editor")),
        settings_panel("io", div(class = "cols",
          grp("Export", uiOutput("export_links"),
              fld("Column headers", radio_group("exp_header", c("Att1, Att2, ..." = "att", "Attribute names" = "name"), "att")),
              chk("exp_fixed", "Add a Fixed column (1 for fixed tasks)", FALSE)),
          div(grp("Sawtooth .cho file",
                  tags$p(class = "hint", "A .cho file stores respondents' answers, so the design is written as one respondent per version."),
                  fld("Answers", radio_group("cho_choices", c("None (0 = missing answer)" = "missing", "Random answers, for testing" = "random",
                                                              "Random part-worth respondents, for testing" = "utility"), "missing")),
                  div(class = "inline", fld("First respondent number", num("cho_first_id", 1, 0)), fld("Seed for simulated answers", num("cho_seed", 1, 0, 999999999))),
                  fld("Delimiter", radio_group("cho_sep", c("Comma" = ",", "Space" = " "), ",")),
                  chk("cho_extra", "Write the version number as an extra variable", TRUE)),
              grp("Import", tags$p(class = "hint", "Replace the design with one made elsewhere. It must have Version, Task and Concept columns, then one column per attribute, and match the task counts above."),
                  div(class = "visually-hidden", fileInput("import_design", NULL, accept = ".csv")),
                  btn("Import design (.csv)", class = "btn", `data-file` = "import_design"))))))))
}

cbc_ui <- function() {
  d <- default_settings(); s0 <- new_study()
  tagList(
    tags$head(
      tags$meta(charset = "utf-8"), tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
      tags$title("CBC design studio"),
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
      tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=IBM+Plex+Sans:wght@400;500;600&display=swap"),
      tags$link(rel = "stylesheet", href = "app.css"), tags$script(src = "app.js")),
    div(class = "app", `data-page` = "attrs",
      tags$header(class = "topbar",
        div(class = "brand", HTML('<svg viewBox="0 0 24 24" aria-hidden="true"><rect x="3" y="4" width="18" height="16" rx="2.5"/><path d="M9 4v16M15 4v16M3 10h18"/></svg>'),
            tags$span("CBC design studio")),
        tags$input(type = "text", class = "study-name", value = s0$name, `aria-label` = "Study name", `data-evt` = "set_name"),
        div(class = "top-actions",
            btn("New", "new_study", class = "btn btn-quiet", `data-confirm` = "Start a new empty study? Unsaved changes will be lost."),
            btn("Open", class = "btn btn-quiet", `data-file` = "open_study"),
            downloadLink("dl_study", "Save study", class = "btn btn-quiet"),
            div(class = "visually-hidden", fileInput("open_study", NULL, accept = c(".json", "application/json"))))),
      div(class = "shell",
        uiOutput("rail", container = tags$div, class = "rail-slot"),
        tags$main(class = "main",
          div(class = "pages",
            div(id = "page-notes", class = "page-wrap", uiOutput("page_notes")),
            div(id = "page-attrs", class = "page-wrap", uiOutput("page_attrs")),
            div(id = "page-prohibs", class = "page-wrap", uiOutput("page_prohibs")),
            div(id = "page-cond", class = "page-wrap", uiOutput("page_cond")),
            div(id = "page-cbc", class = "page-wrap", cbc_settings_ui(d)),
            div(id = "page-results", class = "page-wrap", uiOutput("page_results"))),
          div(class = "actionbar", role = "toolbar",
            actionButton("btn_generate", "Generate design", class = "btn btn-primary"),
            actionButton("btn_test", "Test design", class = "btn"),
            actionButton("btn_preview", "Preview", class = "btn btn-quiet"),
            btn("Export", "goto", class = "btn btn-quiet", `data-page` = "cbc", `data-tab-go` = "io"),
            uiOutput("status", container = tags$div, class = "status-slot"))),
        uiOutput("aside", container = tags$div, class = "aside-slot"))))
}
