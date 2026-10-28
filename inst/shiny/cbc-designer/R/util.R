# Small helpers shared by every other file. No domain knowledge here.

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0L) b else a

# Run `expr` with a fixed RNG seed and put the caller's RNG state back afterwards,
# so generating a design never disturbs the rest of the R session.
with_seed <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  if (had) old <- get(".Random.seed", envir = globalenv())
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

# Parse values once, where they enter (UI inputs, JSON files).
as_int <- function(x, default, lo = -.Machine$integer.max, hi = .Machine$integer.max) {
  v <- suppressWarnings(as.numeric(x[1]))
  if (length(v) == 0L || is.na(v)) return(as.integer(default))
  as.integer(min(max(round(v), lo), hi))
}
as_chr <- function(x, default = "") {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) default else as.character(x[1])
}
as_lgl <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) default else isTRUE(as.logical(x[1]))
}
as_choice <- function(x, allowed, default) {
  v <- as_chr(x, default)
  if (v %in% allowed) v else default
}

# Move element `id` one step up/down inside the vector `ids`.
move_in <- function(ids, id, dir = c("up", "down")) {
  dir <- match.arg(dir)
  i <- match(id, ids)
  if (is.na(i)) return(ids)
  j <- if (dir == "up") i - 1L else i + 1L
  if (j < 1L || j > length(ids)) return(ids)
  ids[c(i, j)] <- ids[c(j, i)]
  ids
}
