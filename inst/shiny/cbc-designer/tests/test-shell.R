# Unit 5d: the static shell (every settings field exists), disabled states, and the server.

cat("shell and server\n")

test("every settings field has exactly one control in the page, and a kind to update it with", {
  h <- html(cbc_ui())
  ids <- c(names(INPUT_MAP), "fixed_positions")
  for (id in ids) {
    expect_equal(count(sprintf('id="%s"', id), h), 1)
    expect_true(id %in% names(INPUT_KIND), paste("no kind for", id))
  }
  expect_true(all(names(INPUT_KIND) %in% ids))
})

test("the initial controls show the default settings", {
  h <- html(cbc_ui())
  for (frag in c('id="n_random" type="number" class="num" value="12"', 'id="d_versions" type="number" class="num" value="300"',
                 'value="complete" selected="selected"', 'value="traditional" checked'))
    expect_true(grepl(frag, h, fixed = TRUE), paste("missing", frag))
})

test("which controls are disabled follows the settings, as in SSI Web", {
  s <- new_study()
  d <- disabled_inputs(s)
  expect_true(d[["d_pos_first"]] && d[["d_pos_last"]] && d[["d_sort_attr"]] && d[["d_first_once"]])   # nothing selected, no prohibitions
  expect_true(d[["total_points"]] && d[["dual_question"]])
  expect_true(!d[["none_text"]])
  s$settings$design$position <- "subset"; s$settings$design$sort <- "natural"
  s$settings$general$response <- "sum"; s$settings$none$mode <- "dual"
  s <- toggle_prohibition(s, aid(s, "Brand"), lid(s, "Brand", "Acme"), aid(s, "Price"), lid(s, "Price", "$199"))
  d <- disabled_inputs(s)
  expect_true(!d[["d_pos_first"]] && !d[["d_sort_attr"]] && !d[["d_first_once"]] && !d[["total_points"]] && !d[["dual_question"]])
  expect_true(d[["none_text"]])
  s$settings$design$method <- "random"
  expect_true(disabled_inputs(s)[["d_first_once"]])           # only for complete enumeration and shortcut
})

# ---- the server, driven without a browser ------------------------------------

test("events from the browser edit the study through the server", {
  testServer(cbc_server, {
    session$setInputs(ui_event = list(type = "add_attr", nonce = 1))
    expect_equal(nrow(study()$attributes), 4)
    session$setInputs(ui_event = list(type = "set_attr_name", id = study()$attributes$id[1], value = "Make", nonce = 2))
    expect_equal(study()$attributes$name[1], "Make")
    session$setInputs(ui_event = list(type = "goto", page = "prohibs", nonce = 3))
    expect_equal(ui()$page, "prohibs")
  })
})

test("a setting typed into a form lands in the study and survives a no-op", {
  testServer(cbc_server, {
    session$setInputs(n_random = 8, d_method = "shortcut", none_mode = "dual")
    expect_equal(study()$settings$general$random_tasks, 8L)
    expect_equal(study()$settings$design$method, "shortcut")
    expect_equal(study()$settings$none$mode, "dual")
    expect_equal(nrow(study()$fixed), 2 * 3 * 3)
    session$setInputs(n_random = 8)
    expect_equal(study()$settings$general$random_tasks, 8L)
  })
})

test("generate builds a design, edits make it stale, and nothing is exported when stale", {
  testServer(cbc_server, {
    session$setInputs(d_versions = 6)
    session$setInputs(btn_generate = 1)
    expect_true(!is.null(design()))
    expect_equal(design_state(), "current")
    expect_equal(nrow(design()$table), 6 * 14 * 3)
    session$setInputs(ui_event = list(type = "add_level", attr = study()$attributes$id[1], nonce = 1))
    expect_equal(design_state(), "stale")
    expect_true(is.null(export_design()))
  })
})

test("generate refuses a study with errors and says why", {
  testServer(cbc_server, {
    for (l in study()$levels$id[study()$levels$attr == study()$attributes$id[2]][-1])
      session$setInputs(ui_event = list(type = "remove_level", id = l, nonce = runif(1)))
    session$setInputs(btn_generate = 1)
    expect_true(is.null(design()))
  })
})

test("the test report is computed for a current design and follows the respondent count", {
  testServer(cbc_server, {
    session$setInputs(d_versions = 5)
    session$setInputs(btn_generate = 1)
    expect_equal(report()$summary$n, 300)
    session$setInputs(ui_event = list(type = "set_test_n", value = 120, nonce = 1))
    expect_equal(report()$summary$n, 120)
  })
})

test("the downloads produce the files the importer and Sawtooth readers expect", {
  testServer(cbc_server, {
    session$setInputs(d_versions = 4)
    session$setInputs(btn_generate = 1)
    csv <- csv_text(design_frame(export_design(), study()))
    back <- read_design_csv(csv, study())
    expect_identical(back$table[, names(design()$table)], design()$table)
    cho <- parse_cho(write_cho(export_design(), study()))
    expect_equal(length(cho), 4)
  })
})

test("a saved study opens again exactly, and clears the old design", {
  testServer(cbc_server, {
    session$setInputs(d_versions = 4, btn_generate = 1)
    s0 <- study()
    json <- study_to_json(s0)
    f <- tempfile(fileext = ".json"); writeLines(json, f)
    session$setInputs(open_study = data.frame(name = "x.json", size = 1, type = "application/json", datapath = f))
    expect_equal(study()$attributes, s0$attributes)
    expect_equal(study()$settings, s0$settings)
    expect_true(is.null(design()))
  })
})

test("a CSV design can be imported when it fits the study, and is refused when it does not", {
  testServer(cbc_server, {
    session$setInputs(d_versions = 3, btn_generate = 1)
    f <- tempfile(fileext = ".csv"); writeLines(csv_text(design_frame(design(), study())), f)
    session$setInputs(btn_generate = 2)    # replace the design, then import the earlier CSV back
    session$setInputs(import_design = data.frame(name = "d.csv", size = 1, type = "text/csv", datapath = f))
    expect_equal(design()$meta$method, "imported")
    bad <- tempfile(fileext = ".csv"); writeLines("a,b\n1,2\n", bad)
    session$setInputs(import_design = data.frame(name = "bad.csv", size = 1, type = "text/csv", datapath = bad))
    expect_equal(design()$meta$method, "imported")        # unchanged by the failed import
  })
})
