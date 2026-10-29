# Unit 4: getting the design out (CSV with indices or labels, codebook, .cho) and a CSV back in.

# Independent reader of the Sawtooth .CHO layout (written from the documentation, not from write_cho).
parse_cho <- function(lines, sep = ",") {
  tok <- function(l) as.numeric(strsplit(trimws(l), if (sep == " ") "[[:space:]]+" else sep)[[1]])
  i <- 1L; out <- list()
  nxt <- function() { r <- tok(lines[i]); i <<- i + 1L; r }
  while (i <= length(lines)) {
    h <- nxt()
    resp <- list(id = h[1], n_extra = h[2], A = h[3], T = h[4], none = h[5], extra = NULL, tasks = list())
    if (h[2] > 0) resp$extra <- nxt()
    for (t in seq_len(h[4])) {
      th <- nxt()
      concepts <- do.call(rbind, lapply(seq_len(th[1]), function(j) nxt()))
      resp$tasks[[t]] <- list(n = th[1], depth = th[2], concepts = concepts, answer = nxt())
    }
    out[[length(out) + 1L]] <- resp
  }
  out
}

cat("export and import\n")

test("the index CSV keeps the choicetools row order: version, then task, then concept", {
  s <- base_study(4L); d <- generate_design(s); f <- design_frame(d, s)
  expect_equal(names(f), c("Version", "Task", "Concept", "Att1", "Att2", "Att3"))
  expect_equal(nrow(f), 4 * 14 * 3)
  i <- (3 - 1) * 14 * 3 + (5 - 1) * 3 + 2          # respondent 3, trial 5, card 2
  expect_equal(unlist(f[i, c("Version", "Task", "Concept")]), c(Version = 3, Task = 5, Concept = 2))
  expect_equal(as.matrix(f[, 4:6]), as.matrix(d$table[, c("Att1", "Att2", "Att3")]))
})

test("headers can be the attribute names, and a Fixed column can be added at the end", {
  s <- base_study(2L); d <- generate_design(s)
  f <- design_frame(d, s, header = "name", fixed_col = TRUE)
  expect_equal(names(f), c("Version", "Task", "Concept", "Brand", "Screen size", "Price", "Fixed"))
  expect_equal(f$Fixed, as.integer(d$table$Fixed))
})

test("the label CSV shows the conditional display text where a relationship applies", {
  s <- base_study(2L)
  s <- set_conditional(s, aid(s, "Price"), aid(s, "Brand"))
  s <- set_conditional_text(s, aid(s, "Price"), lid(s, "Price", "$199"), lid(s, "Brand", "Bolt"), "$179")
  d <- generate_design(s); f <- design_frame(d, s, values = "labels", header = "name")
  bolt_cheap <- d$table$Att1 == 2 & d$table$Att3 == 1
  expect_true(any(bolt_cheap))
  expect_true(all(f$Price[bolt_cheap] == "$179"))
  expect_true(all(f$Price[d$table$Att1 == 1 & d$table$Att3 == 1] == "$199"))
  expect_equal(f$Brand[1], c("Acme", "Bolt", "Crest", "Delta")[d$table$Att1[1]])
})

test("the codebook lists every attribute level with its index", {
  cb <- codebook_frame(new_study())
  expect_equal(nrow(cb), 4 + 3 + 4)
  expect_equal(names(cb), c("Attribute", "AttributeName", "Level", "LevelLabel"))
  expect_equal(cb$LevelLabel[cb$AttributeName == "Brand"], c("Acme", "Bolt", "Crest", "Delta"))
  expect_equal(cb$Level[cb$AttributeName == "Price"], 1:4)
})

test("a CSV written by the designer reads back into the same design", {
  s <- base_study(5L); d <- generate_design(s)
  back <- read_design_csv(csv_text(design_frame(d, s)), s)
  expect_identical(back$table[, names(d$table)], d$table)
  back2 <- read_design_csv(csv_text(design_frame(d, s, header = "name", fixed_col = TRUE)), s)
  expect_identical(back2$table[, names(d$table)], d$table)
  expect_equal(back$meta$method, "imported")
})

test("an imported CSV without a Fixed column marks the tasks at the fixed positions", {
  s <- base_study(3L); d <- generate_design(s)
  back <- read_design_csv(csv_text(design_frame(d, s)), s)
  expect_equal(sort(unique(back$table$Task[back$table$Fixed])), fixed_positions(s$settings))
})

test("a CSV that does not fit the study is refused with a message that says what to change", {
  s <- base_study(3L); d <- generate_design(s); txt <- csv_text(design_frame(d, s))
  f <- design_frame(d, s); f$Att3 <- NULL
  expect_error(read_design_csv(csv_text(f), s), "2 attribute columns")
  f <- design_frame(d, s); f$Att1[5] <- 9L
  expect_error(read_design_csv(csv_text(f), s), "outside 1..4")
  s4 <- s; s4$settings$general$concepts <- 4L; s4 <- tidy_study(s4)
  expect_error(read_design_csv(txt, s4), "3 concepts per task")
  s5 <- s; s5$settings$general$random_tasks <- 10L; s5 <- tidy_study(s5)
  expect_error(read_design_csv(txt, s5), "14 tasks per version")
  expect_error(read_design_csv("a,b\n1,2\n", s), "Version, Task, Concept")
  f <- design_frame(d, s); f$Att2 <- as.character(f$Att2); f$Att2[2] <- "x"
  expect_error(read_design_csv(csv_text(f), s), "whole numbers")
  f <- design_frame(d, s); f$Concept[1:3] <- 2:4
  expect_error(read_design_csv(csv_text(f), s), "Concept numbers")
})

test(".cho header, extra variable and tasks follow the Sawtooth layout (missing answers = 0)", {
  s <- base_study(3L); d <- generate_design(s)
  r <- parse_cho(write_cho(d, s))
  expect_equal(length(r), 3)
  r1 <- r[[1]]
  expect_equal(c(r1$id, r1$n_extra, r1$A, r1$T, r1$none), c(1, 1, 3, 14, 1))
  expect_equal(r1$extra, 1)
  expect_equal(r[[3]]$extra, 3)
  t5 <- r1$tasks[[5]]
  expect_equal(c(t5$n, t5$depth), c(3, 1))
  expect_equal(t5$concepts, as.matrix(d$table[d$table$Version == 1 & d$table$Task == 5, c("Att1", "Att2", "Att3")]))
  expect_equal(t5$answer, c(0, 0))
})

test(".cho options: first respondent number, no extra variable, space delimiter", {
  s <- base_study(2L); d <- generate_design(s)
  lines <- write_cho(d, s, first_id = 1000L, extra_version = FALSE, sep = " ")
  r <- parse_cho(lines, sep = " ")
  expect_equal(c(r[[1]]$id, r[[2]]$id), c(1000, 1001))
  expect_equal(r[[1]]$n_extra, 0)
  expect_true(is.null(r[[1]]$extra))
  expect_equal(lines[1], "1000 0 3 14 1")
})

test("simulated answers stay in range and the None answer is concept J+1", {
  s <- base_study(200L); d <- generate_design(s)
  ans <- unlist(lapply(parse_cho(write_cho(d, s, choices = "random", seed = 7L)),
                       function(x) vapply(x$tasks, function(t) t$answer[1], numeric(1))))
  expect_true(all(ans >= 1 & ans <= 4))
  expect_true(abs(mean(ans == 4) - 1 / 4) < 0.03)
  s$settings$none$mode <- "none"; s <- tidy_study(s)
  r <- parse_cho(write_cho(d, s, choices = "random", seed = 7L))
  expect_equal(r[[1]]$none, 0)
  expect_true(all(unlist(lapply(r, function(x) vapply(x$tasks, function(t) t$answer[1], numeric(1)))) <= 3))
})

test("dual-response None writes four values per task: stage-1 choice, time, buy/no buy, time", {
  s <- base_study(30L); s$settings$none$mode <- "dual"; s <- tidy_study(s); d <- generate_design(s)
  r <- parse_cho(write_cho(d, s, choices = "random", seed = 3L))
  expect_equal(r[[1]]$none, 2)
  a <- do.call(rbind, lapply(r, function(x) do.call(rbind, lapply(x$tasks, function(t) t$answer))))
  expect_equal(ncol(a), 4)
  expect_true(all(a[, 1] >= 1 & a[, 1] <= 3) && all(a[, 3] %in% 1:2))
  z <- parse_cho(write_cho(d, s))[[1]]$tasks[[1]]$answer
  expect_equal(z, c(0, 0, 0, 0))
})

test("simulated answers are reproducible and utility-based answers favour high-utility concepts", {
  s <- base_study(40L); d <- generate_design(s)
  expect_identical(write_cho(d, s, "random", seed = 5L), write_cho(d, s, "random", seed = 5L))
  expect_true(!identical(write_cho(d, s, "random", seed = 5L), write_cho(d, s, "random", seed = 6L)))
  sim <- simulate_choices(d, s, "utility", seed = 11L)
  E <- effects_codes(c(4L, 3L, 4L))
  gain <- unlist(lapply(seq_len(40), function(v) {
    tv <- d$table[d$table$Version == v & !d$table$Fixed, ]
    vapply(split(tv, tv$Task), function(t) {
      u <- vapply(seq_len(nrow(t)), function(i)
        sum(unlist(lapply(1:3, function(a) E[[a]][t[[paste0("Att", a)]][i], ])) * sim$beta[v, 1:8]), numeric(1))
      ch <- sim$choice[v, t$Task[1]]
      if (ch > 3) NA_real_ else u[ch] - mean(u)
    }, numeric(1))
  }))
  expect_true(mean(gain, na.rm = TRUE) > 0.3)
})

test("constant-sum studies cannot be written as .cho", {
  s <- base_study(2L); s$settings$general$response <- "sum"; s <- tidy_study(s)
  expect_error(write_cho(generate_design(s), s), "constant sum")
})

test("the conditional table lists the display text for every combination", {
  s <- base_study(2L)
  s <- set_conditional(s, aid(s, "Price"), aid(s, "Brand"))
  s <- set_conditional_text(s, aid(s, "Price"), lid(s, "Price", "$199"), lid(s, "Brand", "Bolt"), "$179")
  cf <- conditional_frame(s)
  expect_equal(nrow(cf), 16)
  expect_equal(names(cf), c("Attribute", "Level", "LevelLabel", "DependsOn", "WhenLevels", "DisplayText"))
  expect_equal(cf$DisplayText[cf$LevelLabel == "$199" & cf$WhenLevels == "Bolt"], "$179")
  expect_equal(nrow(conditional_frame(new_study())), 0)
})
