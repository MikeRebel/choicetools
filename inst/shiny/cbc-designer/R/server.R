# The server: reactive glue between the browser and the pure functions.
# ---------------------------------------------------------------------------

file_stem <- function(name) {
  x <- gsub("[^[:alnum:]_-]+", "-", trimws(name)); x <- gsub("^-+|-+$", "", x)
  if (nzchar(x)) x else "study"
}

cbc_server <- function(input, output, session) {
  s0 <- new_study()
  study <- reactiveVal(s0)
  ui <- reactiveVal(new_ui_state(s0))
  design <- reactiveVal(NULL)
  test_n <- reactiveVal(300L)
  rev <- reactiveVal(0L)                 # bumped when the editors must be rebuilt
  bump <- function() rev(rev() + 1L)
  notify <- function(msg, type = "message", duration = 6)
    tryCatch(showNotification(msg, type = type, duration = duration), error = function(e) NULL)

  issues <- reactive(validate_study(study()))
  design_state <- reactive({
    d <- design()
    if (is.null(d)) "none" else if (design_is_current(d, study())) "current" else "stale"
  })
  export_design <- reactive(if (design_state() == "current") design() else NULL)
  report <- reactive({
    d <- export_design()
    if (is.null(d)) NULL else test_design(d, study(), test_n())
  })
  dims <- reactive({
    d <- export_design()
    if (is.null(d)) NULL else c(length(unique(d$table$Version)), length(unique(d$table$Task)))
  })

  go <- function(page) {
    ui(modifyList(ui(), list(page = page))); bump()
    session$sendCustomMessage("page", page)
  }

  # ---- settings forms <-> study --------------------------------------------
  sync_inputs <- function(s) {
    vals <- settings_to_inputs(s$settings)
    for (id in names(vals)) {
      v <- vals[[id]]
      switch(INPUT_KIND[[id]],
        num = updateNumericInput(session, id, value = v), text = updateTextInput(session, id, value = v),
        area = updateTextAreaInput(session, id, value = v), chk = updateCheckboxInput(session, id, value = v),
        radio = updateRadioButtons(session, id, selected = v), sel = updateSelectInput(session, id, selected = v))
    }
    session$sendCustomMessage("studyName", s$name)
  }

  observe({
    ids <- c(names(INPUT_MAP), "fixed_positions")
    inp <- stats::setNames(lapply(ids, function(id) input[[id]]), ids)
    s <- isolate(study())
    new <- settings_from_inputs(inp, s$settings)
    if (!identical(new, s$settings)) { s$settings <- new; study(tidy_study(s)) }
  })

  attr_sig <- reactiveVal(NULL)
  observe({
    s <- study(); sig <- list(s$attributes$id, s$attributes$name)
    if (identical(sig, isolate(attr_sig()))) return()
    attr_sig(sig)
    choices <- c("Choose an attribute" = "", stats::setNames(s$attributes$id, s$attributes$name))
    sa <- s$settings$design$sort_attr
    updateSelectInput(session, "d_sort_attr", choices = choices, selected = if (is.na(sa)) "" else sa)
    A <- max(1L, nrow(s$attributes))
    updateNumericInput(session, "d_pos_first", max = A); updateNumericInput(session, "d_pos_last", max = A)
  })

  observe({
    d <- disabled_inputs(study())
    session$sendCustomMessage("disable", list(ids = names(d), disabled = as.list(unname(d))))
  })

  # ---- events from the browser -----------------------------------------------
  load_study <- function(s) {
    study(s); design(NULL); ui(new_ui_state(s)); test_n(300L); bump(); sync_inputs(s)
  }

  handle_event <- function(ev) {
    if (identical(ev$type, "new_study")) return(load_study(new_study(example = FALSE)))
    s <- apply_study_event(study(), ev)
    if (!is.null(s)) {
      study(s); ui(fix_ui_state(ui(), s))
      if (is_structural(ev$type)) bump()
      return(invisible())
    }
    old <- ui(); u <- apply_ui_event(old, study(), ev, dims = dims()); ui(u)
    if (!identical(u$test_n, old$test_n)) test_n(u$test_n)
    if (!startsWith(ev$type, "preview_")) bump()
  }

  observeEvent(input$ui_event, {
    tryCatch(handle_event(input$ui_event), error = function(e) notify(conditionMessage(e), "error", 10))
  })

  # ---- generate, test, preview ------------------------------------------------
  do_generate <- function() {
    iss <- issues(); err <- iss[iss$severity == "error", , drop = FALSE]
    if (nrow(err)) {
      notify(paste(err$message, collapse = " "), "error", 10)
      go(if (err$where[1] == "design") "cbc" else err$where[1])
      return(FALSE)
    }
    d <- tryCatch(withProgress(message = "Generating design", value = 0,
      generate_design(study(), progress = function(i, n)
        if (i %% 5L == 0L || i == n) setProgress(i / n, detail = sprintf("version %d of %d", i, n)))),
      error = function(e) { notify(conditionMessage(e), "error", 10); NULL })
    if (is.null(d)) return(FALSE)
    design(d)
    ui(modifyList(ui(), list(preview = list(version = 1L, task = 1L, seed = ui()$preview$seed), browse_version = 1L)))
    notify(sprintf("Design ready: %d versions of %d tasks.", d$meta$versions, length(unique(d$table$Task))))
    TRUE
  }

  observeEvent(input$btn_generate, { do_generate() })
  observeEvent(input$btn_test, {
    if (design_state() != "current" && !do_generate()) return()
    go("results")
  })
  observeEvent(input$btn_preview, {
    u <- ui()
    ui(if (design_state() == "current") modifyList(u, list(preview = modifyList(u$preview, list(version = 1L, task = 1L))))
       else apply_ui_event(u, study(), list(type = "preview_shuffle")))
    session$sendCustomMessage("reveal", ".aside")
  })

  # ---- opening, importing -----------------------------------------------------
  observeEvent(input$open_study, {
    tryCatch({
      txt <- paste(readLines(input$open_study$datapath, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
      load_study(parse_study(txt))
    }, error = function(e) notify(conditionMessage(e), "error", 10))
  })
  observeEvent(input$import_design, {
    tryCatch({
      d <- read_design_csv(input$import_design$datapath, study())
      design(d); ui(modifyList(ui(), list(preview = list(version = 1L, task = 1L, seed = ui()$preview$seed), browse_version = 1L)))
      notify(sprintf("Imported %d versions of %d tasks.", d$meta$versions, length(unique(d$table$Task))))
    }, error = function(e) notify(conditionMessage(e), "error", 10))
  })

  # ---- pages -----------------------------------------------------------------
  output$rail <- renderUI(ui_rail(study(), ui(), issues(), design_state()))
  output$page_notes <- renderUI({ rev(); ui_page_notes(isolate(study()), isolate(issues())) })
  output$page_attrs <- renderUI({ rev(); ui_page_attrs(isolate(study()), isolate(ui()), isolate(issues())) })
  output$page_prohibs <- renderUI({ rev(); ui_page_prohibs(isolate(study()), isolate(ui()), isolate(issues())) })
  output$page_cond <- renderUI({ rev(); ui_page_cond(isolate(study()), isolate(ui()), isolate(issues())) })
  output$page_results <- renderUI(ui_page_results(study(), design(), report(), ui()))
  output$fixed_editor <- renderUI(ui_fixed_editor(study()))
  output$cbc_issues <- renderUI(issue_list(issues(), c("cbc", "design")))
  output$aside <- renderUI(ui_aside(study(), design(), ui()))

  output$status <- renderUI({
    iss <- issues(); ne <- sum(iss$severity == "error"); nw <- sum(iss$severity == "warning"); d <- design()
    label <- switch(design_state(), none = "No design yet",
      current = sprintf("%d versions, %d tasks%s", length(unique(d$table$Version)), length(unique(d$table$Task)),
                        if (identical(d$meta$method, "imported")) ", imported" else ""),
      stale = "Design out of date")
    div(class = "status", tags$span(class = paste("state", design_state()), label),
        if (ne) tags$button(type = "button", class = "count err", `data-evt` = "goto", `data-page` = "notes", title = "Show the checks",
                            sprintf("%d %s", ne, if (ne == 1L) "error" else "errors")),
        if (nw) tags$button(type = "button", class = "count warn", `data-evt` = "goto", `data-page` = "notes", title = "Show the checks",
                            sprintf("%d %s", nw, if (nw == 1L) "warning" else "warnings")))
  })

  # ---- downloads --------------------------------------------------------------
  output$export_links <- renderUI({
    if (is.null(export_design()))
      return(tags$p(class = "hint", "Generate a design first. If the study changed after the last one, generate it again."))
    cho <- if (study()$settings$general$response == "sum") tags$span(class = "btn is-off", "Sawtooth .cho (not available for constant sum)")
           else downloadLink("dl_cho", "Sawtooth .cho", class = "btn")
    div(class = "dl-row", downloadLink("dl_design", "Design (.csv)", class = "btn btn-primary"),
        downloadLink("dl_labels", "Design with labels (.csv)", class = "btn"),
        downloadLink("dl_codebook", "Codebook (.csv)", class = "btn"),
        if (nrow(study()$cond)) downloadLink("dl_conditional", "Conditional display (.csv)", class = "btn"), cho)
  })

  write_csv <- function(df, file) utils::write.csv(df, file, row.names = FALSE, fileEncoding = "UTF-8")
  stem <- function() file_stem(study()$name)
  need_design <- function() { d <- export_design(); if (is.null(d)) stop("Generate the design first.") else d }

  output$dl_study <- downloadHandler(function() paste0(stem(), ".json"),
    function(file) writeLines(enc2utf8(as.character(study_to_json(study()))), file, useBytes = TRUE))
  output$dl_design <- downloadHandler(function() paste0(stem(), "-design.csv"), function(file)
    write_csv(design_frame(need_design(), study(), "index", input$exp_header %||% "att", isTRUE(input$exp_fixed)), file))
  output$dl_labels <- downloadHandler(function() paste0(stem(), "-design-labels.csv"), function(file)
    write_csv(design_frame(need_design(), study(), "labels", "name", isTRUE(input$exp_fixed)), file))
  output$dl_codebook <- downloadHandler(function() paste0(stem(), "-codebook.csv"), function(file)
    write_csv(codebook_frame(study()), file))
  output$dl_conditional <- downloadHandler(function() paste0(stem(), "-conditional-display.csv"), function(file)
    write_csv(conditional_frame(study()), file))
  output$dl_cho <- downloadHandler(function() paste0(stem(), ".cho"), function(file)
    writeLines(write_cho(need_design(), study(), choices = input$cho_choices %||% "missing",
                         seed = as_int(input$cho_seed, 1L, 0L), first_id = as_int(input$cho_first_id, 1L, 0L),
                         extra_version = isTRUE(input$cho_extra %||% TRUE), sep = input$cho_sep %||% ","), file))
}
