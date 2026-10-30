# Unit 5a: UI events -> new study / new UI state, settings <-> inputs, "design is current".
# The browser only sends events; every decision is made by these pure functions.

cat("events and state\n")

ev <- function(type, ...) list(type = type, ...)

test("study events edit attributes and levels", {
  s <- new_study(); b <- aid(s, "Brand")
  s <- apply_study_event(s, ev("add_attr"));                    expect_equal(nrow(s$attributes), 4)
  s <- apply_study_event(s, ev("set_attr_name", id = b, value = "Make"))
  expect_equal(s$attributes$name[1], "Make")
  l <- lid(s, "Make", "Acme")
  s <- apply_study_event(s, ev("set_level_label", id = l, value = "ACME"))
  expect_equal(s$levels$label[s$levels$id == l], "ACME")
  s <- apply_study_event(s, ev("add_level", attr = b));          expect_equal(compile_study(s)$nlev[1], 5L)
  s <- apply_study_event(s, ev("move_level", id = l, dir = "down"))
  expect_equal(compile_study(s)$level_labels[[1]][2], "ACME")
  s <- apply_study_event(s, ev("remove_level", id = l));         expect_equal(compile_study(s)$nlev[1], 4L)
  s <- apply_study_event(s, ev("paste_levels", attr = b, value = "Red\r\nGreen\n\nBlue\n"))
  expect_equal(compile_study(s)$level_labels[[1]], c("Red", "Green", "Blue"))
  s <- apply_study_event(s, ev("move_attr", id = b, dir = "down"))
  expect_equal(s$attributes$name[2], "Make")
  s <- apply_study_event(s, ev("remove_attr", id = b));          expect_equal(nrow(s$attributes), 3)
})

test("study events edit prohibitions, conditional relationships and fixed tasks", {
  s <- new_study(); b <- aid(s, "Brand"); p <- aid(s, "Price")
  l1 <- lid(s, "Brand", "Acme"); l2 <- lid(s, "Price", "$199")
  s <- apply_study_event(s, ev("toggle_prohib", a = b, la = l1, b = p, lb = l2))
  expect_equal(nrow(s$prohibitions), 1)
  s <- apply_study_event(s, ev("remove_prohib", index = 1))
  expect_equal(nrow(s$prohibitions), 0)
  s <- apply_study_event(s, ev("toggle_prohib", a = b, la = l1, b = p, lb = l2))
  s <- apply_study_event(s, ev("clear_prohibs"));               expect_equal(nrow(s$prohibitions), 0)
  s <- apply_study_event(s, ev("cond_create", target = p, on = list(b)))
  expect_equal(nrow(s$cond), 16)
  s <- apply_study_event(s, ev("set_cond_cell", target = p, tlevel = l2, key = lid(s, "Brand", "Bolt"), value = "$179"))
  expect_true("$179" %in% s$cond$text)
  s <- apply_study_event(s, ev("cond_remove", target = p));     expect_equal(nrow(s$cond), 0)
  x <- lid(s, "Screen size", "6.7\"")
  s <- apply_study_event(s, ev("set_fixed_cell", task = 2, concept = 3, attr = aid(s, "Screen size"), level = x))
  expect_equal(compile_study(s)$fixed[2, 3, 2], 3L)
  s <- apply_study_event(s, ev("set_name", value = "Phones"));  expect_equal(s$name, "Phones")
})

test("unknown events change nothing; structural events are the ones that rebuild editors", {
  expect_true(is.null(apply_study_event(new_study(), ev("nonsense"))))
  expect_true(is_structural("toggle_prohib") && is_structural("add_level") && is_structural("cond_create"))
  expect_true(!is_structural("set_level_label") && !is_structural("set_attr_name") && !is_structural("set_cond_cell"))
})

test("UI state follows the events and repairs itself when things disappear", {
  s <- new_study(); u <- new_ui_state(s)
  expect_equal(u$page, "attrs"); expect_equal(u$sel_attr, aid(s, "Brand"))
  u <- apply_ui_event(u, s, ev("goto", page = "prohibs"));      expect_equal(u$page, "prohibs")
  expect_equal(c(u$proh_a, u$proh_b), c(aid(s, "Brand"), aid(s, "Screen size")))
  u <- apply_ui_event(u, s, ev("set_proh", which = "b", id = aid(s, "Brand")))
  expect_true(u$proh_a != u$proh_b)                              # the pair can never be the same attribute
  u <- apply_ui_event(u, s, ev("select_attr", id = aid(s, "Price")))
  s2 <- remove_attribute(s, aid(s, "Price"))
  u <- fix_ui_state(u, s2)
  expect_equal(u$sel_attr, aid(s2, "Brand"))
  u <- apply_ui_event(u, s2, ev("cond_pick_target", id = aid(s2, "Screen size")))
  u <- apply_ui_event(u, s2, ev("cond_toggle_on", id = aid(s2, "Brand")))
  expect_equal(u$cond_on, aid(s2, "Brand"))
  u <- apply_ui_event(u, s2, ev("cond_toggle_on", id = aid(s2, "Brand")))
  expect_equal(length(u$cond_on), 0)
})

test("the respondent view moves through tasks and versions without leaving the design", {
  u <- new_ui_state(new_study())
  u <- apply_ui_event(u, new_study(), ev("preview_go", version = 3, task = 14), dims = c(10, 14))
  expect_equal(c(u$preview$version, u$preview$task), c(3, 14))
  u <- apply_ui_event(u, new_study(), ev("preview_step", by = 1), dims = c(10, 14))
  expect_equal(c(u$preview$version, u$preview$task), c(4, 1))        # steps over into the next version
  u <- apply_ui_event(u, new_study(), ev("preview_go", version = 99, task = 0), dims = c(10, 14))
  expect_equal(c(u$preview$version, u$preview$task), c(10, 1))      # clamped
  u <- apply_ui_event(u, new_study(), ev("preview_shuffle"))
  expect_true(u$preview$seed != 1)
})

test("settings round-trip through the form inputs", {
  s <- new_study()
  s$settings$general$random_tasks <- 8L; s$settings$design$method <- "shortcut"
  s$settings$none$mode <- "dual"; s$settings$format$button <- "Pick"
  s$settings$fixed$positions <- c(3L, 7L)
  inp <- settings_to_inputs(s$settings)
  expect_equal(settings_from_inputs(inp, default_settings()), sanitize_settings(s$settings))
})

test("form inputs are parsed once: junk is clamped, blanks keep the current value", {
  base <- default_settings()
  got <- settings_from_inputs(list(n_random = "abc", n_concepts = 99, d_versions = -5, d_method = "zzz",
                                   fixed_positions = "5, 10, x", n_fixed = NULL), base)
  expect_equal(got$general$random_tasks, base$general$random_tasks)
  expect_equal(got$general$concepts, 12L)
  expect_equal(got$design$versions, 1L)
  expect_equal(got$design$method, base$design$method)
  expect_equal(got$fixed$positions, c(5L, 10L))
  expect_equal(got$general$fixed_tasks, base$general$fixed_tasks)
})

test("a design is current until something that changes it is edited", {
  s <- base_study(3L); d <- generate_design(s)
  expect_true(design_is_current(d, s))
  s1 <- set_level_label(s, lid(s, "Brand", "Acme"), "ACME")
  expect_true(design_is_current(d, s1))                          # labels never change the design
  s2 <- s; s2$settings$none$mode <- "none"; s2 <- tidy_study(s2)
  expect_true(design_is_current(d, s2))
  s3 <- s; s3$settings$design$seed <- 9L
  expect_true(!design_is_current(d, s3))
  s4 <- add_level(s, aid(s, "Brand"), "Echo")
  expect_true(!design_is_current(d, s4))
  s5 <- toggle_prohibition(s, aid(s, "Brand"), lid(s, "Brand", "Acme"), aid(s, "Price"), lid(s, "Price", "$499"))
  expect_true(!design_is_current(d, s5))
  imp <- read_design_csv(csv_text(design_frame(d, s)), s)
  expect_true(design_is_current(imp, s))
  expect_true(!design_is_current(imp, s4))
})

test("removing a prohibition by index removes that one, not the first", {
  s <- new_study(); b <- aid(s, "Brand"); p <- aid(s, "Price")
  s <- toggle_prohibition(s, b, lid(s, "Brand", "Acme"), p, lid(s, "Price", "$199"))
  s <- toggle_prohibition(s, b, lid(s, "Brand", "Bolt"), p, lid(s, "Price", "$299"))
  s <- apply_study_event(s, ev("remove_prohib", index = 2))
  expect_equal(nrow(s$prohibitions), 1)
  expect_equal(s$prohibitions$la, lid(s, "Brand", "Acme"))
  s <- apply_study_event(s, ev("remove_prohib", index = 7))       # out of range: ignored
  expect_equal(nrow(s$prohibitions), 1)
})

test("an unusable form value falls back to the current setting, not the factory default", {
  base <- default_settings(); base$design$method <- "shortcut"; base$design$versions <- 50L
  base$general$random_tasks <- 9L
  got <- settings_from_inputs(list(d_method = "zzz", d_versions = "many", n_random = ""), base)
  expect_equal(got$design$method, "shortcut")
  expect_equal(got$design$versions, 50L)
  expect_equal(got$general$random_tasks, 9L)
})

test("changing the number of tasks or a fixed task makes the design out of date", {
  s <- base_study(3L); d <- generate_design(s)
  s1 <- s; s1$settings$general$random_tasks <- 10L; s1 <- tidy_study(s1)
  expect_true(!design_is_current(d, s1))
  a <- aid(s, "Brand"); cur <- compile_study(s)$fixed[1, 1, 1]
  other <- s$levels$id[s$levels$attr == a][cur %% 4L + 1L]
  s2 <- set_fixed_cell(s, 1, 1, a, other)
  expect_true(!design_is_current(d, s2))
})

test("without fixed tasks, changing the number of random tasks still makes the design out of date", {
  s <- base_study(3L); s$settings$general$fixed_tasks <- 0L; s <- tidy_study(s)
  d <- generate_design(s)
  s1 <- s; s1$settings$general$random_tasks <- 10L; s1 <- tidy_study(s1)
  expect_true(!design_is_current(d, s1))
  s2 <- s; s2$settings$general$total_points <- 50L; s2 <- tidy_study(s2)   # layout only
  expect_true(design_is_current(d, s2))
})

test("the respondent count for the report is clamped, and the relationship's attributes arrive comma-joined", {
  u <- new_ui_state(new_study()); s <- new_study()
  u <- apply_ui_event(u, s, ev("set_test_n", value = 1));      expect_equal(u$test_n, 2L)
  u <- apply_ui_event(u, s, ev("set_test_n", value = "450"));   expect_equal(u$test_n, 450L)
  b <- aid(s, "Brand"); sc <- aid(s, "Screen size"); p <- aid(s, "Price")
  s2 <- apply_study_event(s, ev("cond_create", target = p, on = paste(b, sc, sep = ",")))
  expect_equal(nrow(s2$cond), 4 * (4 * 3))
})
