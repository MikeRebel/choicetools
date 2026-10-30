# Unit 5b: the respondent view of one choice task (what the preview and the browser show).

task_html <- function(s, X = rbind(c(1, 2, 3), c(2, 3, 1), c(3, 1, 2)), ...) {
  as.character(ui_task(compile_study(s), s$settings, X, ...))
}

cat("respondent view\n")

test("a task shows a column per concept, the attribute rows and the None column", {
  h <- task_html(new_study(), task_no = 3, n_tasks = 14)
  for (txt in c("Concept 1", "Concept 2", "Concept 3", "Task 3 of 14", "NONE:", "Acme", "Bolt", "Crest",
                "Brand", "Screen size", "Price", "If these were your only options"))
    expect_true(grepl(txt, h, fixed = TRUE), paste("missing:", txt))
  expect_equal(count("class=\"pick\"", h), 1)
})

test("attribute rows follow the display order chosen for this respondent", {
  h <- task_html(new_study(), order = c(3, 1, 2))
  p <- function(x) regexpr(x, h, fixed = TRUE)[1]
  expect_true(p(">Price<") < p(">Brand<") && p(">Brand<") < p(">Screen size<"))
})

test("conditional display text replaces the level label", {
  s <- new_study()
  s <- set_conditional(s, aid(s, "Price"), aid(s, "Brand"))
  s <- set_conditional_text(s, aid(s, "Price"), lid(s, "Price", "$199"), lid(s, "Brand", "Bolt"), "$179")
  h <- task_html(s, X = rbind(c(2, 1, 1), c(1, 1, 1), c(3, 2, 2)))
  expect_true(grepl("$179", h, fixed = TRUE))
  expect_equal(count("$199", h), 1)                      # only Acme's concept keeps the plain price
})

test("None option: absent, traditional text, or a dual-response follow-up question", {
  s <- new_study(); s$settings$none$mode <- "none"
  expect_true(!grepl("NONE:", task_html(s), fixed = TRUE))
  s$settings$none$mode <- "dual"
  h <- task_html(s)
  expect_true(!grepl("NONE:", h, fixed = TRUE) && grepl("would you actually buy", h, fixed = TRUE))
})

test("constant-sum studies show a numeric box per concept (and None) and the point total", {
  s <- new_study(); s$settings$general$response <- "sum"; s$settings$general$total_points <- 100L
  h <- task_html(s)
  expect_equal(count("type=\"number\"", h), 4)
  expect_true(grepl("100", h, fixed = TRUE))
})

test("more columns than fit wrap the concepts into stacked blocks", {
  s <- new_study(); s$settings$general$columns <- 2L
  expect_equal(count("<table class=\"task-grid", task_html(s)), 2)
  expect_equal(count("<table class=\"task-grid", task_html(new_study())), 1)
})

test("attribute names, concept label template, button text, footer and fixed badge are configurable", {
  s <- new_study(); s$settings$format$show_attr_names <- FALSE
  s$settings$format$concept_label <- "Option {n}"; s$settings$format$button <- "Pick"
  s$settings$format$footer <- "Thank you"
  h <- task_html(s, fixed = TRUE)
  expect_true(!grepl("attr-name", h, fixed = TRUE))
  for (txt in c("Option 1", "Pick", "Thank you", "Fixed task")) expect_true(grepl(txt, h, fixed = TRUE))
})

test("text from the study is escaped, never interpreted as HTML", {
  s <- new_study(); s <- set_level_label(s, lid(s, "Brand", "Acme"), "<b>Acme</b>")
  h <- task_html(s)
  expect_true(grepl("&lt;b&gt;Acme", h, fixed = TRUE) && !grepl("<b>Acme", h, fixed = TRUE))
})

test("a sample task uses feasible concepts and is reproducible from its seed", {
  s <- new_study(); b <- aid(s, "Brand")
  for (l in c("Bolt", "Crest", "Delta")) for (p in c("$199", "$299", "$399"))
    s <- toggle_prohibition(s, b, lid(s, "Brand", l), aid(s, "Price"), lid(s, "Price", p))
  t1 <- sample_task(s, seed = 4L); t2 <- sample_task(s, seed = 4L)
  expect_identical(t1, t2)
  expect_equal(dim(t1$X), c(3L, 3L))
  expect_true(!any(violates(compile_study(s), t1$X)))
})
