# Unit 5c: choosing what the respondent view shows (design vs sample) and the pages around it.
# How one task is drawn is tested in test-task-view.R; the pages are smoke-tested, including
# the empty study.

cat("pages and respondent view source\n")

test("the respondent view comes from the design when it is current, else from a sample", {
  s <- base_study(3L); d <- generate_design(s)
  v <- task_view(s, d, list(version = 2L, task = 5L, seed = 1L))
  expect_equal(v$source, "design")
  rows <- d$table[d$table$Version == 2 & d$table$Task == 5, c("Att1", "Att2", "Att3")]
  expect_equal(v$X, as.matrix(rows))
  expect_equal(v$order, d$attr_order[2, ])
  expect_equal(c(v$task_no, v$n_tasks, v$version), c(5, 14, 2))
  s2 <- add_level(s, aid(s, "Brand"), "Echo")
  v2 <- task_view(s2, d, list(version = 2L, task = 5L, seed = 1L))
  expect_equal(v2$source, "sample")
  expect_equal(nrow(v2$X), 3)
})

test("a fixed task is flagged in the respondent view", {
  s <- base_study(3L); d <- generate_design(s); fp <- fixed_positions(s$settings)
  expect_true(task_view(s, d, list(version = 1L, task = fp[1], seed = 1L))$fixed)
  expect_true(!task_view(s, d, list(version = 1L, task = 1L, seed = 1L))$fixed)
})

test("a sample task never breaks a prohibition, whatever the seed", {
  s <- prohib_study("complete", 3L)
  for (seed in 1:30) {
    X <- sample_task(s, seed)$X
    tab <- as.data.frame(X); names(tab) <- paste0("Att", 1:3)
    expect_true(!any(breaks_prohibition(s, tab)))
  }
})

test("an empty study shows a hint instead of a task", {
  s <- new_study(example = FALSE)
  for (a in s$attributes$id) s <- remove_attribute(s, a)
  v <- task_view(s, NULL, list(version = 1L, task = 1L, seed = 1L))
  expect_true(grepl("Add attributes", html(ui_task(compile_study(s), s$settings, v$X, v$order))))
})

test("every page builds for a normal study and for an empty one", {
  s <- new_study(); iss <- validate_study(s); u <- new_ui_state(s)
  e <- new_study(example = FALSE); for (a in e$attributes$id) e <- remove_attribute(e, a)
  eu <- new_ui_state(e); ei <- validate_study(e)
  for (case in list(list(s, u, iss), list(e, eu, ei))) {
    st <- case[[1]]; ui <- case[[2]]; is <- case[[3]]
    for (f in list(function() ui_rail(st, ui, is, NULL), function() ui_page_notes(st, is),
                   function() ui_page_attrs(st, ui, is), function() ui_page_prohibs(st, ui, is),
                   function() ui_page_cond(st, ui, is), function() ui_fixed_editor(st),
                   function() ui_aside(st, NULL, ui)))
      expect_true(nzchar(html(f())))
  }
})

test("the attribute editor has one input per level, wired to the event bus", {
  s <- new_study(); h <- html(ui_page_attrs(s, new_ui_state(s), validate_study(s)))
  expect_equal(count('data-evt="set_level_label"', h), 4)               # Brand is selected
  expect_equal(count('data-evt="select_attr"', h), 3)
  expect_true(grepl('value="Acme"', h, fixed = TRUE))
})

test("the prohibition matrix marks prohibited cells and counts what is left", {
  s <- new_study(); b <- aid(s, "Brand"); sc <- aid(s, "Screen size")
  s <- toggle_prohibition(s, b, lid(s, "Brand", "Acme"), sc, lid(s, "Screen size", "5.4\""))
  h <- html(ui_page_prohibs(s, new_ui_state(s), validate_study(s)))
  expect_equal(count('data-evt="toggle_prohib"', h), 12)                # 4 x 3 cells
  expect_equal(count('class="cell on"', h), 1)
  expect_true(grepl("11 of 12", h, fixed = TRUE))
})

test("the conditional editor shows one text box per level and combination", {
  s <- new_study(); s <- set_conditional(s, aid(s, "Price"), aid(s, "Brand"))
  h <- html(ui_page_cond(s, new_ui_state(s), validate_study(s)))
  expect_equal(count('data-evt="set_cond_cell"', h), 16)
})

test("the fixed task editor offers a choice per attribute of every concept", {
  expect_equal(count('data-evt="set_fixed_cell"', html(ui_fixed_editor(new_study()))), 2 * 3 * 3)
})

test("the rail shows six pages and flags the ones with errors", {
  s <- new_study(); sc <- aid(s, "Screen size")
  for (l in s$levels$id[s$levels$attr == sc][-1]) s <- remove_level(s, l)
  h <- html(ui_rail(s, new_ui_state(s), validate_study(s), NULL))
  expect_true(grepl('class="dot err"', h, fixed = TRUE))
  expect_equal(count('data-evt="goto"', h), 6)
})

test("the report and the design browser render", {
  s <- base_study(6L); d <- generate_design(s); r <- test_design(d, s, 300L); u <- new_ui_state(s)
  h <- html(ui_page_results(s, d, r, u))
  for (txt in c("Standard errors", "Level balance", "Brand", "Screen size")) expect_true(grepl(txt, h, fixed = TRUE))
  expect_true(grepl("Generate", html(ui_page_results(s, NULL, NULL, u)), fixed = TRUE))
  expect_true(nzchar(html(ui_browse(s, d, u))))
})

test("the None cell spans every attribute row", {
  s <- new_study()
  h <- html(ui_task(compile_study(s), s$settings, rbind(c(1, 2, 3), c(2, 3, 1), c(3, 1, 2))))
  expect_true(grepl('rowspan="3"', h, fixed = TRUE))
})

test("the prohibition matrix shows a prohibition whichever attribute is picked first", {
  s <- new_study(); b <- aid(s, "Brand"); sc <- aid(s, "Screen size")
  s <- toggle_prohibition(s, b, lid(s, "Brand", "Acme"), sc, lid(s, "Screen size", "5.4\""))
  u <- new_ui_state(s)
  u <- apply_ui_event(u, s, list(type = "set_proh", which = "a", id = sc))
  u <- apply_ui_event(u, s, list(type = "set_proh", which = "b", id = b))
  expect_equal(c(u$proh_a, u$proh_b), c(sc, b))
  h <- html(ui_page_prohibs(s, u, validate_study(s)))
  expect_equal(count('class="cell on"', h), 1)
  expect_true(grepl("11 of 12", h, fixed = TRUE))
})

test("a report for a design that no longer matches the study is not shown", {
  s <- base_study(4L); d <- generate_design(s); r <- test_design(d, s, 300L)
  s2 <- add_level(s, aid(s, "Brand"), "Echo")
  h <- html(ui_page_results(s2, d, r, new_ui_state(s2)))
  expect_true(grepl("out of date", h, fixed = TRUE) && !grepl("Standard errors", h, fixed = TRUE))
})

test("the rail shows how many attributes, prohibitions and relationships there are", {
  s <- new_study(); b <- aid(s, "Brand"); p <- aid(s, "Price")
  s <- toggle_prohibition(s, b, lid(s, "Brand", "Acme"), p, lid(s, "Price", "$199"))
  s <- set_conditional(s, p, b)
  h <- html(ui_rail(s, new_ui_state(s), validate_study(s), NULL))
  expect_equal(count('<span class="rail-meta">3</span>', h), 1)    # attributes
  expect_equal(count('<span class="rail-meta">1</span>', h), 2)    # prohibitions and relationships
})
