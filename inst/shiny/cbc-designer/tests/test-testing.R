# Unit 3: testing a design (counts, pair association, overlap, position, standard errors).

# a hand-made design: 4 tasks, 2 concepts, one attribute with levels 1 and 2 shown against each other
tiny_tab <- function() data.frame(Version = 1L, Task = rep(1:4, each = 2), Concept = rep(1:2, 4),
                                  Fixed = FALSE, Att1 = rep(1:2, 4))

cat("design testing\n")

test("standard errors match the hand calculation: 4 tasks of 2 vs 2 give SE 0.5", {
  r <- design_se(tiny_tab(), nlev = 2L, concepts = 2L, none = FALSE, n = 1L)
  expect_true(r$estimable)
  expect_equal(r$se, c(0.5, 0.5))
})

test("standard errors shrink with the square root of the sample size", {
  tab <- generate_design(base_study(20L))$table; tab <- tab[!tab$Fixed, ]
  a <- design_se(tab, c(4L, 3L, 4L), 3L, TRUE, n = 100L)$se
  b <- design_se(tab, c(4L, 3L, 4L), 3L, TRUE, n = 400L)$se
  expect_equal(b / a, rep(0.5, length(a)), tol = 1e-8)
})

test("allowing a None answer makes the design less precise", {
  tab <- generate_design(base_study(20L))$table; tab <- tab[!tab$Fixed, ]
  with_none <- design_se(tab, c(4L, 3L, 4L), 3L, TRUE, 300L)
  no_none   <- design_se(tab, c(4L, 3L, 4L), 3L, FALSE, 300L)
  expect_true(all(with_none$se > no_none$se))
  expect_true(is.finite(with_none$none_se))
})

test("a level that is never shown makes the design not estimable", {
  tab <- tiny_tab(); tab$Att1 <- 1L
  r <- design_se(tab, nlev = 2L, concepts = 2L, none = FALSE, n = 10L)
  expect_true(!r$estimable)
  expect_true(all(is.na(r$se)))
})

test("a balanced design has lower standard errors than a random one", {
  se_of <- function(m) {
    s <- base_study(30L, m); d <- generate_design(s)
    max(test_design(d, s, n = 300L)$se$se)
  }
  expect_true(se_of("complete") < se_of("random"))
})

test("the report counts only random tasks: level counts add up", {
  s <- base_study(10L); d <- generate_design(s); r <- test_design(d, s)
  n_random <- sum(!d$table$Fixed)
  for (a in 1:3) expect_equal(sum(r$oneway$count[r$oneway$attr == a]), n_random)
  expect_equal(r$summary$versions, 10)
  expect_equal(r$summary$random_tasks, 12)
})

test("level counts are close to even for complete enumeration", {
  s <- base_study(30L); r <- test_design(generate_design(s), s)
  expect_true(all(abs(r$oneway$pct - 100) < 5))
})

test("pair association is low without prohibitions and high with strong ones", {
  s <- base_study(30L); free <- test_design(generate_design(s), s)$pairs$assoc
  p <- prohib_study("complete", 30L)
  b <- aid(p, "Brand"); pr <- aid(p, "Price")
  for (l in c("Acme", "Bolt")) for (v in c("$199", "$299"))
    p <- toggle_prohibition(p, b, lid(p, "Brand", l), pr, lid(p, "Price", v))
  held <- test_design(generate_design(p), p)$pairs$assoc
  expect_true(max(free, na.rm = TRUE) < 0.1)
  expect_true(held[1, 3] > free[1, 3] + 0.1)
})

test("overlap is reported: none for complete enumeration, plenty for random", {
  s <- base_study(20L)
  ce <- test_design(generate_design(s), s)$overlap
  expect_equal(ce$repeat_pct, c(0, 0, 0))
  s2 <- base_study(20L, "random")
  expect_true(test_design(generate_design(s2), s2)$overlap$repeat_pct[1] > 20)
})

test("repeats are marked unavoidable when a task shows more concepts than the attribute has levels", {
  s <- base_study(5L); s$settings$general$concepts <- 4L; s <- tidy_study(s)
  r <- test_design(generate_design(s), s)$overlap
  expect_equal(r$unavoidable, c(FALSE, TRUE, FALSE))
})

test("position balance flags the attribute that concepts are sorted by", {
  s <- base_study(); s$settings$design$sort <- "natural"; s$settings$design$sort_attr <- aid(s, "Brand")
  r <- test_design(generate_design(s), s)$position
  expect_true(r$max_dev_pct[1] > 50 && r$sorted[1])
  expect_true(all(!r$sorted[2:3]) && all(r$max_dev_pct[2:3] < 50))
})

test("the Johnson-Orme sample size rule of thumb is computed from the study", {
  s <- base_study(5L); r <- test_design(generate_design(s), s, n = 300L)
  expect_equal(r$sanity$value, 300 * 12 * 3 / 4)
  expect_true(r$sanity$ok)
  expect_true(!test_design(generate_design(s), s, n = 20L)$sanity$ok)
})

test("standard errors are rated against Sawtooth's 0.05 (preferably 0.03) guideline", {
  expect_equal(rate_se(c(0.02, 0.03, 0.04, 0.05, 0.06, NA)),
               c("Excellent", "Excellent", "Good", "Good", "Weak", "Not estimable"))
})

test("standard errors agree with a numerical Hessian of the expected log-likelihood", {
  # Independent oracle: at all part-worths = 0 every alternative is equally likely, so the
  # expected log-likelihood is  sum_tasks sum_i (1/M) * log softmax_i(b). Its negative
  # Hessian is the information matrix; no formulas from design_se() are reused.
  s <- base_study(3L); d <- generate_design(s)
  tab <- d$table[!d$table$Fixed, ]; nlev <- c(4L, 3L, 4L); J <- 3L
  code <- function(a, l) { v <- numeric(nlev[a] - 1L); if (l < nlev[a]) v[l] <- 1 else v[] <- -1; v }
  k <- sum(nlev - 1L) + 1L                                  # + None constant
  task_X <- lapply(split(tab, paste(tab$Version, tab$Task)), function(t) {
    rows <- lapply(seq_len(nrow(t)), function(i)
      c(unlist(lapply(1:3, function(a) code(a, t[[paste0("Att", a)]][i]))), 0))
    rbind(do.call(rbind, rows), c(rep(0, k - 1L), 1))       # the None alternative
  })
  nll <- function(b) -sum(vapply(task_X, function(X) {
    u <- X %*% b; mean(u - log(sum(exp(u))))
  }, numeric(1)))
  Sigma <- solve(optimHess(rep(0, k), nll))
  ref <- c(sqrt(diag(Sigma)[1:3]), sqrt(sum(Sigma[1:3, 1:3])),        # Brand: 3 params + last level
           sqrt(diag(Sigma)[4:5]), sqrt(sum(Sigma[4:5, 4:5])),        # Screen size
           sqrt(diag(Sigma)[6:8]), sqrt(sum(Sigma[6:8, 6:8])))        # Price
  got <- design_se(tab, nlev, J, TRUE, n = 3L)
  expect_equal(got$se, ref, tol = 1e-4)
  expect_equal(got$none_se, sqrt(Sigma[k, k]), tol = 1e-4)
})

test("without concept sorting no attribute is flagged as sorted (and nothing is NA)", {
  s <- base_study(5L); r <- test_design(generate_design(s), s)
  expect_identical(r$position$sorted, c(FALSE, FALSE, FALSE))
})
