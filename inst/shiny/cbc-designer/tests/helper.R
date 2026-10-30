# Minimal test harness (no testthat dependency).
# Usage: test("what the user sees", { expect_equal(...) })
.results <- new.env()
.results$pass <- 0L
.results$fail <- character()

# Each test runs in its own child environment: a variable a test creates (say `txt`) must
# never replace a function of the same name from the app code.
test <- function(desc, code) {
  expr <- substitute(code)
  res <- tryCatch({ eval(expr, new.env(parent = parent.frame())); NULL }, error = function(e) conditionMessage(e))
  if (is.null(res)) {
    .results$pass <- .results$pass + 1L
    cat("  ok   ", desc, "\n", sep = "")
  } else {
    .results$fail <- c(.results$fail, desc)
    cat("  FAIL ", desc, "\n         ", res, "\n", sep = "")
  }
  invisible(is.null(res))
}

expect_true <- function(x, msg = "expected TRUE") {
  if (!isTRUE(x)) stop(msg, call. = FALSE)
}
expect_equal <- function(x, y, tol = 1e-8) {
  r <- all.equal(x, y, tolerance = tol, check.attributes = FALSE)
  if (!isTRUE(r)) stop("not equal: ", paste(r, collapse = "; "), call. = FALSE)
}
expect_identical <- function(x, y) {
  if (!identical(x, y)) stop("not identical", call. = FALSE)
}
expect_error <- function(expr, pattern = NULL) {
  msg <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (is.null(msg)) stop("expected an error, got none", call. = FALSE)
  if (!is.null(pattern) && !grepl(pattern, msg, ignore.case = TRUE))
    stop("error '", msg, "' does not match '", pattern, "'", call. = FALSE)
}

# ---- fixtures shared by the test files
lid <- function(s, attr_name, label) {
  a <- s$attributes$id[s$attributes$name == attr_name]
  s$levels$id[s$levels$attr == a & s$levels$label == label]
}
aid <- function(s, attr_name) s$attributes$id[s$attributes$name == attr_name]

base_study <- function(versions = 20L, method = "complete") {
  s <- new_study()
  s$settings$design$versions <- versions
  s$settings$design$method <- method
  tidy_study(s)
}
prohib_study <- function(method = "complete", versions = 20L) {
  s <- base_study(versions, method)
  b <- aid(s, "Brand"); p <- aid(s, "Price"); sc <- aid(s, "Screen size")
  s <- toggle_prohibition(s, b, lid(s, "Brand", "Acme"),  p, lid(s, "Price", "$499"))
  s <- toggle_prohibition(s, b, lid(s, "Brand", "Delta"), p, lid(s, "Price", "$199"))
  toggle_prohibition(s, b, lid(s, "Brand", "Delta"), sc, lid(s, "Screen size", "5.4\""))
}

html <- function(x) paste(as.character(tagList(x)), collapse = "")
count <- function(pattern, x) length(regmatches(x, gregexpr(pattern, x, fixed = TRUE))[[1]])

finish_tests <- function() {
  cat("\n", .results$pass, " passed, ", length(.results$fail), " failed\n", sep = "")
  if (length(.results$fail)) quit(status = 1L)
}
