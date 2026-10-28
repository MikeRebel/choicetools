# Unit 1: the study model (attributes, levels, prohibitions, conditional
# relationships, fixed tasks) and its JSON round trip.

lid <- function(s, attr_name, label) {
  a <- s$attributes$id[s$attributes$name == attr_name]
  s$levels$id[s$levels$attr == a & s$levels$label == label]
}
aid <- function(s, attr_name) s$attributes$id[s$attributes$name == attr_name]

cat("study model\n")

test("a new study is a valid example with three attributes", {
  s <- new_study()
  expect_equal(nrow(s$attributes), 3)
  expect_equal(compile_study(s)$nlev, c(4L, 3L, 4L))
  iss <- validate_study(s)
  expect_equal(sum(iss$severity == "error"), 0)
})

test("adding an attribute gives it a unique id and two levels", {
  s <- add_attribute(new_study())
  expect_equal(nrow(s$attributes), 4)
  expect_true(!anyDuplicated(s$attributes$id) && !anyDuplicated(s$levels$id))
  expect_equal(compile_study(s)$nlev[4], 2L)
})

test("removing an attribute removes its levels, prohibitions and conditional rows", {
  s <- new_study()
  s <- toggle_prohibition(s, aid(s, "Brand"), lid(s, "Brand", "Acme"),
                          aid(s, "Price"), lid(s, "Price", "$199"))
  s <- set_conditional(s, target = aid(s, "Price"), on = aid(s, "Brand"))
  s <- remove_attribute(s, aid(s, "Brand"))
  expect_equal(nrow(s$attributes), 2)
  expect_equal(nrow(s$prohibitions), 0)
  expect_equal(nrow(s$cond), 0)
  expect_true(all(s$levels$attr %in% s$attributes$id))
})

test("removing a level prunes the prohibitions that mention it", {
  s <- new_study()
  b <- aid(s, "Brand"); p <- aid(s, "Price")
  s <- toggle_prohibition(s, b, lid(s, "Brand", "Acme"), p, lid(s, "Price", "$199"))
  s <- remove_level(s, lid(s, "Brand", "Acme"))
  expect_equal(nrow(s$prohibitions), 0)
  expect_equal(compile_study(s)$nlev[1], 3L)
})

test("toggling a prohibition adds then removes it, keeping attribute order", {
  s <- new_study()
  b <- aid(s, "Brand"); p <- aid(s, "Price")
  l1 <- lid(s, "Brand", "Acme"); l2 <- lid(s, "Price", "$199")
  s <- toggle_prohibition(s, p, l2, b, l1)          # given in reverse order
  expect_equal(nrow(s$prohibitions), 1)
  expect_equal(s$prohibitions$a, b)                 # stored Brand first
  s <- toggle_prohibition(s, b, l1, p, l2)
  expect_equal(nrow(s$prohibitions), 0)
})

test("toggling the same prohibition twice removes it whichever way round it is given", {
  s <- new_study()
  b <- aid(s, "Brand"); p <- aid(s, "Price")
  l1 <- lid(s, "Brand", "Acme"); l2 <- lid(s, "Price", "$199")
  s <- toggle_prohibition(s, p, l2, b, l1)
  s <- toggle_prohibition(s, p, l2, b, l1)
  expect_equal(nrow(s$prohibitions), 0)
})

test("a prohibition between two levels of the same attribute is rejected", {
  s <- new_study(); b <- aid(s, "Brand")
  expect_error(toggle_prohibition(s, b, lid(s, "Brand", "Acme"), b, lid(s, "Brand", "Bolt")),
               "different attributes")
})

test("moving an attribute changes compile order and keeps prohibitions valid", {
  s <- new_study()
  b <- aid(s, "Brand"); p <- aid(s, "Price")
  s <- toggle_prohibition(s, b, lid(s, "Brand", "Acme"), p, lid(s, "Price", "$199"))
  s <- move_attribute(s, p, "up"); s <- move_attribute(s, p, "up")
  expect_equal(s$attributes$name[1], "Price")
  cs <- compile_study(s)
  expect_equal(cs$attr_names, c("Price", "Brand", "Screen size"))
  expect_equal(sum(cs$forb[[1]]), 1L)               # one forbidden cell in pair (Price, Brand)
})

test("conditional relationship creates one cell per combination with default text", {
  s <- new_study()
  s <- set_conditional(s, target = aid(s, "Price"), on = aid(s, "Brand"))
  expect_equal(nrow(s$cond), 4 * 4)
  expect_true(all(s$cond$text[s$cond$tlevel == lid(s, "Price", "$199")] == "$199"))
  s <- set_conditional_text(s, aid(s, "Price"), lid(s, "Price", "$199"),
                            lid(s, "Brand", "Bolt"), "$179")
  row <- s$cond$tlevel == lid(s, "Price", "$199") & s$cond$key == lid(s, "Brand", "Bolt")
  expect_equal(s$cond$text[row], "$179")
})

test("a conditional relationship cannot depend on its own attribute", {
  s <- new_study()
  expect_error(set_conditional(s, aid(s, "Price"), aid(s, "Price")), "itself")
})

test("fixed tasks are always complete: tasks x concepts x attributes cells", {
  s <- new_study()
  expect_equal(nrow(s$fixed), 2 * 3 * 3)
  s$settings$general$concepts <- 4L
  s <- tidy_study(s)
  expect_equal(nrow(s$fixed), 2 * 4 * 3)
})

test("validation flags an attribute with a single level and a dead level", {
  s <- new_study()
  b <- aid(s, "Brand"); sc <- aid(s, "Screen size")
  for (l in s$levels$id[s$levels$attr == sc][-1]) s <- remove_level(s, l)
  expect_true(any(validate_study(s)$severity == "error"))
  s <- new_study()
  # prohibit Acme with every Screen size -> Acme can never be shown
  for (l in s$levels$id[s$levels$attr == sc])
    s <- toggle_prohibition(s, b, lid(s, "Brand", "Acme"), sc, l)
  iss <- validate_study(s)
  expect_true(any(grepl("never", iss$message)))
})

test("a study survives the JSON round trip unchanged", {
  s <- new_study()
  s <- set_conditional(s, aid(s, "Price"), aid(s, "Brand"))
  s <- toggle_prohibition(s, aid(s, "Brand"), lid(s, "Brand", "Acme"),
                          aid(s, "Screen size"), lid(s, "Screen size", "6.1\""))
  s2 <- parse_study(study_to_json(s))
  expect_equal(s2$attributes, s$attributes)
  expect_equal(s2$levels, s$levels)
  expect_equal(s2$prohibitions, s$prohibitions)
  expect_equal(s2$cond, s$cond)
  expect_equal(s2$fixed, s$fixed)
  expect_equal(s2$settings, s$settings)
})

test("garbage JSON is rejected with a clear message", {
  expect_error(parse_study("{\"hello\": 1}"), "not a CBC study")
  expect_error(parse_study("not json"), "not a CBC study")
})
