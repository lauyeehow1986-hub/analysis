# core/exec.R - seeded isolated execution, RNG streams and the content-addressed result cache.
#
# A route compiles a spec into an R code string. That exact string (with the RNG preamble in front)
# is what runs in an isolated callr worker, and it becomes the exported analysis.R. The fits/ cache
# is only a speed-up: it is never authoritative and any doubt about a file is a miss.

#' The two lines that fix the RNG for a run. `seed` is rendered as an integer literal.
sr_rng_preamble <- function(seed) {
  .sr_check_seed(seed)
  paste(
    "RNGkind(\"L'Ecuyer-CMRG\", \"Inversion\", \"Rejection\")",
    sprintf("set.seed(%dL)", as.integer(seed)),
    sep = "\n"
  )
}

#' `n` independent L'Ecuyer-CMRG streams for `seed`. Stream i is nextRNGStream applied i times to
#' the state set by the preamble, so it depends only on (seed, i), never on a worker count. The
#' caller's RNG kind and .Random.seed (or its absence) are restored on exit.
sr_streams <- function(seed, n) {
  .sr_check_seed(seed)
  if (!is.numeric(n) || length(n) != 1L || !is.finite(n) || n != round(n) || n < 1 ||
      n > .Machine$integer.max) {
    stop("n must be a single whole number >= 1", call. = FALSE)
  }
  genv <- globalenv()
  # RNGkind() itself creates .Random.seed, so record its existence first
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = genv, inherits = FALSE) else NULL
  old_kind <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(old_kind[1L], old_kind[2L], old_kind[3L]))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  }, add = TRUE)
  RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
  set.seed(as.integer(seed))
  s <- get(".Random.seed", envir = genv, inherits = FALSE)
  out <- vector("list", as.integer(n))
  for (i in seq_along(out)) {
    s <- parallel::nextRNGStream(s)
    out[[i]] <- s
  }
  out
}

.sr_check_seed <- function(seed) {
  if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed) || seed != round(seed) ||
      abs(seed) > .Machine$integer.max) {
    stop("seed must be a single finite whole number with |seed| <= ", .Machine$integer.max,
         call. = FALSE)
  }
  invisible(TRUE)
}

.sr_check_exec_args <- function(code, inputs, seed, timeout) {
  if (!is.character(code) || length(code) != 1L || is.na(code)) {
    stop("code must be a single non-NA string", call. = FALSE)
  }
  .sr_check_seed(seed)
  nms <- names(inputs)
  if (!is.list(inputs) || (length(inputs) > 0L &&
      (is.null(nms) || anyNA(nms) || any(!nzchar(nms)) || anyDuplicated(nms) > 0L))) {
    stop("inputs must be a list whose elements all have unique, non-empty names", call. = FALSE)
  }
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout <= 0) {
    stop("timeout must be a single positive number of seconds", call. = FALSE)
  }
  invisible(TRUE)
}

#' Run `code` (seeded by the RNG preamble) in a fresh callr worker with `inputs` as variables.
#' Returns list(ok, value, error, elapsed, code_run). Execution failures (an error in the code, a
#' crashed worker, a timeout) give ok = FALSE and a message; they never throw. `code_run` is the
#' exact string evaluated and is the text of the exported analysis.R.
sr_exec <- function(code, inputs = list(), seed, timeout = 3600) {
  .sr_check_exec_args(code, inputs, seed, timeout)
  code_run <- paste(sr_rng_preamble(seed), code, sep = "\n")
  worker <- function(code, inputs) {
    list2env(inputs, environment())
    eval(parse(text = code))
  }
  t0 <- proc.time()[["elapsed"]]
  res <- tryCatch(
    list(ok = TRUE, value = callr::r(worker, args = list(code = code_run, inputs = inputs),
                                     timeout = timeout, user_profile = FALSE,
                                     libpath = .libPaths(), show = FALSE)),
    error = function(e) list(ok = FALSE, error = .sr_exec_error_text(e, timeout))
  )
  elapsed <- as.numeric(proc.time()[["elapsed"]] - t0)
  list(ok = res$ok, value = if (res$ok) res$value else NULL,
       error = if (res$ok) NA_character_ else res$error,
       elapsed = elapsed, code_run = code_run)
}

# One message for a failed worker run: the timeout, else the root cause. callr wraps the worker's
# own error in a generic "in callr subprocess" condition and keeps the original in $parent.
.sr_exec_error_text <- function(e, timeout) {
  if (inherits(e, "callr_timeout_error")) {
    return(sprintf("execution timeout after %s seconds", format(timeout)))
  }
  root <- e
  for (i in 1:10) {
    nxt <- if (is.list(root) || is.environment(root)) root$parent else NULL
    if (is.null(nxt)) break
    root <- nxt
  }
  m <- tryCatch(conditionMessage(root), error = function(err) "")
  m <- sub("^[! ]+", "", paste(m, collapse = " "))
  if (is.na(m) || !nzchar(m)) m <- "execution failed"
  m
}

# A stored result is a hit only if it is a successful sr_exec list for exactly this code.
.sr_cache_read <- function(path, code_run) {
  if (!file.exists(path)) return(NULL)
  stored <- tryCatch(suppressWarnings(readRDS(path)), error = function(e) NULL)
  need <- c("ok", "value", "error", "elapsed", "code_run")
  if (!is.list(stored) || !all(need %in% names(stored))) return(NULL)
  if (!isTRUE(stored$ok) || !is.character(stored$code_run) ||
      !identical(stored$code_run, code_run)) return(NULL)
  stored[need]
}

# Atomic write (temp file in the same folder, then rename). Failure is silent: the cache is
# only a speed-up.
.sr_cache_write <- function(dir, key_hash, result) {
  tryCatch({
    if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
    tmp <- tempfile(pattern = paste0(key_hash, "-"), tmpdir = dir, fileext = ".tmp")
    saveRDS(result, tmp)
    if (!file.rename(tmp, file.path(dir, paste0(key_hash, ".rds")))) unlink(tmp)
  }, error = function(e) NULL)
  invisible(NULL)
}

#' sr_exec behind the project's fits/ cache. A hit needs fits/<key_hash>.rds to be readable and
#' to hold the same code_run; otherwise the code runs and an ok result replaces the file (never in
#' a read-only project). Returns the sr_exec list plus `cached`. `...` goes to sr_exec.
sr_exec_cached <- function(project, key_hash, code, inputs = list(), seed, ...) {
  if (!is.character(key_hash) || length(key_hash) != 1L || is.na(key_hash) ||
      !grepl("^[0-9a-f]{64}$", key_hash)) {
    stop("key_hash must be a 64-character lowercase hex string", call. = FALSE)
  }
  dots <- list(...)
  .sr_check_exec_args(code, inputs, seed, if ("timeout" %in% names(dots)) dots$timeout else 3600)
  fits <- project$paths$fits
  code_run <- paste(sr_rng_preamble(seed), code, sep = "\n")
  hit <- .sr_cache_read(file.path(fits, paste0(key_hash, ".rds")), code_run)
  if (!is.null(hit)) return(c(hit, list(cached = TRUE)))
  res <- sr_exec(code, inputs, seed, ...)
  if (isTRUE(res$ok) && !isTRUE(project$readonly)) .sr_cache_write(fits, key_hash, res)
  c(res, list(cached = FALSE))
}
