# Getting a design out (CSV, codebook, Sawtooth .cho) and a CSV design back in.
# ---------------------------------------------------------------------------
# CSV layout = the "tab" matrix choicetools works with: one row per concept, ordered
# respondent (version) -> trial (task) -> card (concept), then one column of level indices
# per attribute. A .cho file stores respondents' answers, so the design is written as one
# respondent per version; answers are 0 (the official "missing") unless simulated.

csv_text <- function(df) {
  paste0(paste(utils::capture.output(utils::write.csv(df, row.names = FALSE)), collapse = "\n"), "\n")
}

# What the respondent reads for every concept row, as a character matrix.
label_matrix <- function(cs, X) {
  key <- do.call(paste, c(as.data.frame(X), sep = "-"))
  first <- !duplicated(key)
  Xu <- X[first, , drop = FALSE]
  txt <- matrix(vapply(seq_len(nrow(Xu)), function(i) concept_text(cs, Xu[i, ]), character(cs$A)),
                nrow = cs$A)
  t(txt)[match(key, key[first]), , drop = FALSE]
}

design_frame <- function(design, study, values = c("index", "labels"), header = c("att", "name"),
                         fixed_col = FALSE) {
  values <- match.arg(values); header <- match.arg(header)
  cs <- compile_study(study); tab <- design$table
  att <- paste0("Att", seq_len(cs$A)); X <- as.matrix(tab[, att, drop = FALSE])
  body <- if (values == "index") as.data.frame(X) else as.data.frame(label_matrix(cs, X), stringsAsFactors = FALSE)
  names(body) <- if (header == "name") make.unique(cs$attr_names) else att
  out <- cbind(tab[c("Version", "Task", "Concept")], body)
  if (fixed_col) out$Fixed <- as.integer(tab$Fixed)
  rownames(out) <- NULL
  out
}

codebook_frame <- function(study) {
  cs <- compile_study(study)
  data.frame(Attribute = rep(seq_len(cs$A), cs$nlev), AttributeName = rep(cs$attr_names, cs$nlev),
             Level = unlist(lapply(cs$nlev, seq_len)), LevelLabel = unlist(cs$level_labels),
             stringsAsFactors = FALSE)
}

# Display text for conditional relationships, one row per level x combination.
conditional_frame <- function(study) {
  empty <- data.frame(Attribute = character(), Level = integer(), LevelLabel = character(),
                      DependsOn = character(), WhenLevels = character(), DisplayText = character(),
                      stringsAsFactors = FALSE)
  if (nrow(study$cond) == 0L) return(empty)
  a_name <- function(id) study$attributes$name[match(id, study$attributes$id)]
  l_label <- function(id) study$levels$label[match(id, study$levels$id)]
  do.call(rbind, lapply(seq_len(nrow(study$cond)), function(i) {
    r <- study$cond[i, ]
    on <- strsplit(r$on, "|", fixed = TRUE)[[1]]; key <- strsplit(r$key, "|", fixed = TRUE)[[1]]
    tl <- study$levels$id[study$levels$attr == r$target]
    data.frame(Attribute = a_name(r$target), Level = match(r$tlevel, tl), LevelLabel = l_label(r$tlevel),
               DependsOn = paste(a_name(on), collapse = " x "), WhenLevels = paste(l_label(key), collapse = " x "),
               DisplayText = r$text, stringsAsFactors = FALSE)
  }))
}

# ---- reading a design from CSV --------------------------------------------

read_design_csv <- function(x, study) {
  cs <- compile_study(study); ds <- study$settings
  df <- tryCatch(
    if (length(x) == 1L && !grepl("\n", x, fixed = TRUE) && file.exists(x))
      utils::read.csv(x, check.names = FALSE, stringsAsFactors = FALSE, fileEncoding = "UTF-8-BOM")
    else utils::read.csv(text = x, check.names = FALSE, stringsAsFactors = FALSE),
    error = function(e) stop("Could not read the file as CSV.", call. = FALSE))
  low <- tolower(trimws(names(df))); key <- match(c("version", "task", "concept"), low)
  if (anyNA(key))
    stop("The file needs the columns Version, Task, Concept followed by one column per attribute.", call. = FALSE)
  fixed_i <- match("fixed", low)
  att_i <- setdiff(seq_along(df), c(key, fixed_i))
  if (length(att_i) != cs$A)
    stop(sprintf("The file has %d attribute columns but the study has %d attributes.", length(att_i), cs$A), call. = FALSE)
  num <- lapply(c(key, att_i), function(j) suppressWarnings(as.numeric(df[[j]])))
  if (any(vapply(num, anyNA, logical(1))) || any(vapply(num, function(v) any(v != round(v)), logical(1))))
    stop("Version, Task, Concept and the attribute columns must hold whole numbers.", call. = FALSE)
  for (a in seq_len(cs$A)) {
    v <- num[[3L + a]]
    if (any(v < 1 | v > cs$nlev[a]))
      stop(sprintf("Attribute %d (%s) has levels outside 1..%d.", a, cs$attr_names[a], cs$nlev[a]), call. = FALSE)
  }
  tab <- data.frame(Version = as.integer(num[[1]]), Task = as.integer(num[[2]]), Concept = as.integer(num[[3]]))
  att <- as.data.frame(lapply(num[-(1:3)], as.integer)); names(att) <- paste0("Att", seq_len(cs$A))
  fixed <- if (is.na(fixed_i)) NULL else {
    f <- as.logical(df[[fixed_i]]); f[is.na(f)] <- FALSE; f
  }
  tab <- cbind(tab, att); ord <- order(tab$Version, tab$Task, tab$Concept)
  tab <- tab[ord, , drop = FALSE]; fixed <- fixed[ord]
  check_design_shape(tab, ds)
  tab <- cbind(tab[c("Version", "Task", "Concept")], Fixed = fixed %||% (tab$Task %in% fixed_positions(ds)),
               tab[paste0("Att", seq_len(cs$A))])
  rownames(tab) <- NULL
  V <- length(unique(tab$Version))
  list(table = tab, attr_order = matrix(seq_len(cs$A), V, cs$A, byrow = TRUE),
       meta = list(method = "imported", seed = NA_integer_, versions = V, concepts = ds$general$concepts,
                   random_tasks = ds$general$random_tasks,
                   fixed_positions = sort(unique(tab$Task[tab$Fixed])), none = ds$none$mode,
                   response = ds$general$response, nlev = cs$nlev, created = Sys.time()))
}

check_design_shape <- function(tab, ds) {
  J <- ds$general$concepts; n_tasks <- tasks_total(ds)
  per_task <- table(paste(tab$Version, tab$Task))
  if (any(per_task != J))
    stop(sprintf("The file has %s concepts per task, but CBC Settings say %d.",
                 paste(sort(unique(as.integer(per_task))), collapse = "/"), J), call. = FALSE)
  per_version <- tapply(tab$Task, tab$Version, function(t) length(unique(t)))
  if (any(per_version != n_tasks))
    stop(sprintf("The file has %s tasks per version, but CBC Settings say %d.",
                 paste(sort(unique(as.integer(per_version))), collapse = "/"), n_tasks), call. = FALSE)
  if (!identical(tab$Concept, rep(seq_len(J), length.out = nrow(tab))))
    stop("Concept numbers must run from 1 to the number of concepts inside every task.", call. = FALSE)
}

# ---- simulated respondents ------------------------------------------------

# One simulated respondent per version. "random": every alternative equally likely.
# "utility": random part-worths ~ N(0,1) plus Gumbel noise (a multinomial logit respondent).
# Traditional None is concept J+1; dual-response None is a second buy / no-buy question.
simulate_choices <- function(design, study, mode = c("random", "utility"), seed = 1L) {
  mode <- match.arg(mode); cs <- compile_study(study); none_mode <- study$settings$none$mode
  tab <- design$table; versions <- sort(unique(tab$Version))
  J <- max(tab$Concept); n_tasks <- length(unique(tab$Task)); V <- length(versions)
  M <- J + as.integer(none_mode == "traditional")
  E <- effects_codes(cs$nlev); k <- sum(cs$nlev - 1L)
  Xc <- do.call(cbind, lapply(seq_len(cs$A), function(a) E[[a]][tab[[paste0("Att", a)]], , drop = FALSE]))
  gumbel <- function(n) -log(-log(stats::runif(n)))
  with_seed(seed, {
    beta <- matrix(stats::rnorm(V * k), V, k); bnone <- stats::rnorm(V)
    choice <- matrix(0L, V, n_tasks); buy <- if (none_mode == "dual") matrix(0L, V, n_tasks) else NULL
    for (v in seq_len(V)) for (t in seq_len(n_tasks)) {
      u <- as.vector(Xc[((v - 1L) * n_tasks + (t - 1L)) * J + seq_len(J), , drop = FALSE] %*% beta[v, ])
      if (mode == "random") {
        choice[v, t] <- sample.int(M, 1L)
        if (!is.null(buy)) buy[v, t] <- sample.int(2L, 1L)
      } else {
        if (none_mode == "traditional") u <- c(u, bnone[v])
        choice[v, t] <- which.max(u + gumbel(length(u)))
        if (!is.null(buy)) buy[v, t] <- if (stats::runif(1) < stats::plogis(u[choice[v, t]] - bnone[v])) 1L else 2L
      }
    }
    list(choice = choice, buy = buy, beta = beta, none = bnone)
  })
}

# ---- Sawtooth .cho --------------------------------------------------------

write_cho <- function(design, study, choices = c("missing", "random", "utility"), seed = 1L,
                      first_id = 1L, extra_version = TRUE, sep = ",") {
  choices <- match.arg(choices); ds <- study$settings
  if (ds$general$response == "sum")
    stop("Constant sum studies cannot be written as .cho (the .chs format is not supported here).", call. = FALSE)
  cs <- compile_study(study); tab <- design$table
  versions <- sort(unique(tab$Version)); J <- max(tab$Concept); n_tasks <- length(unique(tab$Task))
  stopifnot(nrow(tab) == length(versions) * n_tasks * J)
  none_flag <- c(none = 0L, traditional = 1L, dual = 2L)[[ds$none$mode]]
  sim <- if (choices == "missing") NULL else simulate_choices(design, study, choices, seed)
  line <- function(v) paste(v, collapse = sep)
  concept_lines <- apply(as.matrix(tab[paste0("Att", seq_len(cs$A))]), 1L, line)
  answer <- function(v, t) {
    if (is.null(sim)) return(line(rep(0L, if (none_flag == 2L) 4L else 2L)))
    line(if (none_flag == 2L) c(sim$choice[v, t], 0L, sim$buy[v, t], 0L) else c(sim$choice[v, t], 0L))
  }
  unlist(lapply(seq_along(versions), function(v) c(
    line(c(first_id + v - 1L, as.integer(extra_version), cs$A, n_tasks, none_flag)),
    if (extra_version) line(versions[v]),
    unlist(lapply(seq_len(n_tasks), function(t) {
      rows <- ((v - 1L) * n_tasks + (t - 1L)) * J + seq_len(J)
      c(line(c(J, 1L)), concept_lines[rows], answer(v, t))
    })))))
}
