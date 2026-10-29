# Unit 2: generating a CBC design (versions x tasks x concepts).

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

# independent re-implementation of "does this concept break a prohibition?"
breaks_prohibition <- function(s, tab) {
  ids <- s$attributes$id
  bad <- rep(FALSE, nrow(tab))
  for (r in seq_len(nrow(s$prohibitions))) {
    p <- s$prohibitions[r, ]
    ai <- match(p$a, ids); bi <- match(p$b, ids)
    la <- match(p$la, s$levels$id[s$levels$attr == p$a])
    lb <- match(p$lb, s$levels$id[s$levels$attr == p$b])
    bad <- bad | (tab[[paste0("Att", ai)]] == la & tab[[paste0("Att", bi)]] == lb)
  }
  bad
}
random_rows <- function(d) d$table[!d$table$Fixed, ]
task_key <- function(tab) paste(tab$Version, tab$Task)
# per version and attribute: max - min of how often each level is shown
level_spread <- function(tab, nlev) {
  unlist(lapply(split(tab, tab$Version), function(v)
    vapply(seq_along(nlev), function(a) {
      cnt <- tabulate(v[[paste0("Att", a)]], nlev[a]); max(cnt) - min(cnt)
    }, numeric(1))))
}
overlap_tasks <- function(tab, a) {   # number of tasks that repeat a level of attribute a
  sum(tapply(tab[[paste0("Att", a)]], task_key(tab), function(x) anyDuplicated(x) > 0))
}

cat("design generation\n")

test("the design has one row per version x task x concept, with fixed tasks marked", {
  s <- base_study(); d <- generate_design(s)
  expect_equal(nrow(d$table), 20 * 14 * 3)
  expect_equal(names(d$table), c("Version", "Task", "Concept", "Fixed", "Att1", "Att2", "Att3"))
  expect_equal(range(d$table$Task), c(1, 14))
  fp <- fixed_positions(s$settings)
  expect_equal(sort(unique(d$table$Task[d$table$Fixed])), fp)
  expect_true(all(d$table$Att1 >= 1 & d$table$Att1 <= 4 & d$table$Att2 <= 3 & d$table$Att3 <= 4))
})

test("fixed tasks are identical for every version and equal to the study's fixed concepts", {
  s <- base_study(); d <- generate_design(s)
  f <- d$table[d$table$Fixed & d$table$Task == fixed_positions(s$settings)[1], ]
  one <- f[f$Version == 1, c("Concept", "Att1", "Att2", "Att3")]
  for (v in unique(f$Version)) expect_equal(f[f$Version == v, c("Concept", "Att1", "Att2", "Att3")], one)
  cs <- compile_study(s)
  expect_equal(as.matrix(one[, c("Att1", "Att2", "Att3")]), matrix(cs$fixed[1, , ], nrow = 3))
})

test("the same seed gives the same design, another seed a different one", {
  s <- base_study(); d1 <- generate_design(s); d2 <- generate_design(s)
  expect_identical(d1$table, d2$table)
  s$settings$design$seed <- 2L
  expect_true(!identical(d1$table, generate_design(s)$table))
})

test("asking for more versions leaves the earlier versions unchanged", {
  a <- generate_design(base_study(5L))$table
  b <- generate_design(base_study(10L))$table
  expect_identical(a, b[b$Version <= 5, ])
})

test("generating a design leaves the caller's random number stream alone", {
  set.seed(42); expected <- runif(1)
  set.seed(42); generate_design(base_study(3L)); expect_equal(runif(1), expected)
})

for (m in c("complete", "shortcut", "balanced", "random")) {
  local({
    method <- m
    test(sprintf("[%s] never shows a prohibited combination nor a repeated concept", method), {
      s <- prohib_study(method); d <- generate_design(s)
      expect_true(!any(breaks_prohibition(s, random_rows(d))))
      cols <- c("Att1", "Att2", "Att3")
      key <- paste(task_key(d$table[!d$table$Fixed, ]), do.call(paste, d$table[!d$table$Fixed, cols]))
      expect_true(!anyDuplicated(key))
    })
  })
}

test("complete enumeration balances levels within each questionnaire version", {
  d <- generate_design(base_study(30L))
  expect_true(max(level_spread(random_rows(d), c(4, 3, 4))) <= 1)
})

test("complete enumeration is better balanced than random task generation", {
  ce <- generate_design(base_study(30L, "complete"))
  rn <- generate_design(base_study(30L, "random"))
  expect_true(mean(level_spread(random_rows(ce), c(4, 3, 4))) <
                mean(level_spread(random_rows(rn), c(4, 3, 4))) - 1)
})

test("complete enumeration and shortcut avoid repeating a level inside a task when they can", {
  for (m in c("complete", "shortcut")) {
    tab <- random_rows(generate_design(base_study(20L, m)))
    for (a in 1:3) expect_equal(overlap_tasks(tab, a), 0)
  }
  tab <- random_rows(generate_design(base_study(20L, "random")))
  expect_true(overlap_tasks(tab, 1) > 0)
})

test("'first attribute once per task' keeps attribute 1 distinct within a task under prohibitions", {
  s <- prohib_study("complete"); s$settings$design$first_attr_once <- TRUE
  tab <- random_rows(generate_design(s))
  expect_equal(overlap_tasks(tab, 1), 0)
})

test("natural concept sorting orders each task by the chosen attribute", {
  s <- base_study(); s$settings$design$sort <- "natural"; s$settings$design$sort_attr <- aid(s, "Brand")
  tab <- random_rows(generate_design(s))
  ok <- tapply(tab$Att1, task_key(tab), function(x) !is.unsorted(x))
  expect_true(all(ok))
})

test("random concept sorting uses one consistent level order per version", {
  s <- base_study(); s$settings$design$sort <- "random"; s$settings$design$sort_attr <- aid(s, "Brand")
  tab <- random_rows(generate_design(s))
  for (v in split(tab, tab$Version)) {
    before <- matrix(FALSE, 4, 4)
    for (t in split(v, v$Task)) for (i in 1:2) for (j in (i + 1):3)
      if (t$Att1[i] != t$Att1[j]) before[t$Att1[i], t$Att1[j]] <- TRUE
    expect_true(!any(before & t(before)))
  }
})

test("attribute display order: none keeps it, all permutes it, a subset permutes only that range", {
  s <- base_study(30L)
  expect_true(all(apply(generate_design(s)$attr_order, 1, function(r) identical(r, 1:3))))
  s$settings$design$position <- "all"; o <- generate_design(s)$attr_order
  expect_true(all(apply(o, 1, function(r) identical(sort(r), 1:3))))
  expect_true(nrow(unique(o)) > 1)
  s$settings$design$position <- "subset"; s$settings$design$pos_first <- 2L; s$settings$design$pos_last <- 3L
  o <- generate_design(s)$attr_order
  expect_true(all(o[, 1] == 1L))
  expect_true(any(o[, 2] == 3L))
})

test("a big design space is sampled instead of enumerated and still respects prohibitions", {
  s <- new_study(example = FALSE)
  for (i in 1:7) s <- add_attribute(s, levels = paste0("L", 1:5))
  s$settings$design$versions <- 3L
  a1 <- s$attributes$id[1]; a3 <- s$attributes$id[3]
  s <- toggle_prohibition(s, a1, s$levels$id[s$levels$attr == a1][1], a3, s$levels$id[s$levels$attr == a3][1])
  d <- generate_design(s)
  expect_equal(nrow(d$table), 3 * 14 * 3)
  expect_true(!any(breaks_prohibition(s, random_rows(d))))
})

test("progress is reported once per version", {
  n <- 0L; generate_design(base_study(4L), progress = function(i, total) n <<- n + 1L)
  expect_equal(n, 4L)
})

test("a study with validation errors cannot be generated", {
  s <- base_study(); b <- aid(s, "Brand"); sc <- aid(s, "Screen size")
  for (l in s$levels$id[s$levels$attr == sc]) s <- toggle_prohibition(s, b, lid(s, "Brand", "Acme"), sc, l)
  expect_error(generate_design(s), "never be shown")
})

test("'once' drops candidates that repeat a level of attribute 1, unless nothing else is left", {
  task <- rbind(c(1L, 1L))
  pool <- list(X = rbind(c(1L, 1L), c(1L, 2L), c(2L, 3L)))
  expect_equal(which(eligible(pool, task, once = TRUE,  A = 2L)), 3L)
  expect_equal(which(eligible(pool, task, once = FALSE, A = 2L)), c(2L, 3L))   # (1,1) is a duplicate
  only_repeats <- list(X = rbind(c(1L, 2L), c(1L, 3L)))
  expect_equal(which(eligible(only_repeats, task, once = TRUE, A = 2L)), c(1L, 2L))
})
