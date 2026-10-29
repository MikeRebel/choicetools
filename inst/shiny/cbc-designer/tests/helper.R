# Minimal test harness (no testthat dependency).
# Usage: test("what the user sees", { expect_equal(...) })
.results <- new.env()
.results$pass <- 0L
.results$fail <- character()

test <- function(desc, code) {
  res <- tryCatch({ force(code); NULL }, error = function(e) conditionMessage(e))
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

finish_tests <- function() {
  cat("\n", .results$pass, " passed, ", length(.results$fail), " failed\n", sep = "")
  if (length(.results$fail)) quit(status = 1L)
}
