# The shared worker-job layer: a mirai worker outside the Shiny session that sources the app, receives a
# snapshot of the session's settings, and (optionally) records itself in a job file so that Cancel can kill
# it and its process tree. Used by the Analyses panel (.sr_an_*), the Data panel and the Write panel.
#
# The job file holds "<pid> <create_time>" of the worker, written atomically (a temp file renamed into place),
# so that .sr_job_kill() never kills an unrelated process that reused the PID.
#
# Options. A worker is a fresh R process: it sees none of the session's options.  .sr_job_opts() snapshots the
# settings the panel edited (never the Jev key) and .sr_job_spawn() sets them in the worker after global.R is
# sourced, so global.R cannot overwrite them.

# TRUE when `path` is one usable file path.
.sr_job_path_ok <- function(path) {
  is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path)
}

# Write one line to `path` when it is a file path; never errors.
.sr_job_put <- function(text, path) {
  if (.sr_job_path_ok(path)) try(writeLines(text, path), silent = TRUE)
  invisible(NULL)
}

# Record this process (PID and start time) in `job_file` through "<job_file>.part" and a rename. A no-op for
# NULL. NULL on success, else the reason it failed (a character string).
.sr_job_record <- function(job_file) {
  if (!.sr_job_path_ok(job_file)) return(NULL)
  ct <- tryCatch(as.numeric(ps::ps_create_time(ps::ps_handle())), error = function(e) NA_real_)
  if (!is.finite(ct)) return("could not read the start time of the worker process")
  part <- paste0(job_file, ".part")
  ok <- tryCatch({
    writeLines(sprintf("%d %.6f", Sys.getpid(), ct), part)
    isTRUE(file.rename(part, job_file))
  }, error = function(e) FALSE)
  if (!ok) {
    try(unlink(part), silent = TRUE)
    return("could not write the job file")
  }
  NULL
}

# The job file as list(pid, time), or NULL while it is missing or not a job record.
.sr_job_read <- function(job_file) {
  if (!.sr_job_path_ok(job_file) || !file.exists(job_file)) return(NULL)
  l <- tryCatch(readLines(job_file, warn = FALSE), error = function(e) character())
  if (length(l) != 1L || !grepl("^[0-9]+ [0-9]+([.][0-9]*)?$", l)) return(NULL)
  f <- strsplit(l, " ", fixed = TRUE)[[1L]]
  pid <- suppressWarnings(as.numeric(f[1L]))
  tm <- suppressWarnings(as.numeric(f[2L]))
  if (!.sr_pid_valid(pid) || !is.finite(tm)) return(NULL)
  list(pid = as.integer(pid), time = tm)
}

#' Kill the worker recorded in `job_file` and all its descendants (the callr fit included). Waits up to
#' `wait` seconds for the job file. Refuses (FALSE) when the PID is not running or its start time differs
#' from the recorded one by more than 0.01 s (the PID was reused). Otherwise suspends the worker, snapshots
#' its descendants, kills them, then the worker, and waits up to `wait` seconds until none of them runs.
#' TRUE when it killed the worker; an error (naming any kill failures) when a process still runs.
.sr_job_kill <- function(job_file, wait = 10) {
  elapsed <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))
  t0 <- Sys.time()
  rec <- .sr_job_read(job_file)
  while (is.null(rec) && elapsed(t0) < wait) {
    Sys.sleep(0.1)
    rec <- .sr_job_read(job_file)
  }
  if (is.null(rec)) return(FALSE)
  h <- tryCatch(ps::ps_handle(rec$pid), error = function(e) NULL)
  if (is.null(h)) return(FALSE)
  ct <- tryCatch(as.numeric(ps::ps_create_time(h)), error = function(e) NA_real_)
  if (!is.finite(ct) || abs(ct - rec$time) > 0.01) return(FALSE)

  errs <- character()
  note <- function(e) {
    if (!inherits(e, "no_such_process")) errs <<- c(errs, conditionMessage(e))
    NULL
  }
  # suspended first: the worker can neither record the end of a fit whose child dies nor start a new child
  tryCatch(ps::ps_suspend(h), error = note)
  kids <- tryCatch(ps::ps_children(h, recursive = TRUE), error = function(e) {
    note(e)
    list()
  })
  for (x in kids) tryCatch(ps::ps_kill(x), error = note)
  tryCatch(ps::ps_kill(h), error = note)

  procs <- c(kids, list(h))
  pids <- c(vapply(kids, function(x) as.integer(ps::ps_pid(x)), 0L), rec$pid)
  alive <- function() pids[vapply(pids, .sr_pid_running, NA)]
  t1 <- Sys.time()
  while (length(alive()) && elapsed(t1) < wait) Sys.sleep(0.1)
  left <- alive()
  try(unlink(paste0(job_file, ".part")), silent = TRUE)
  if (length(left)) {
    # never leave a suspended worker behind: it would keep holding the project lock
    for (x in procs[pids %in% left]) tryCatch(ps::ps_resume(x), error = function(e) NULL)
    stop("could not stop the analysis worker (process ", paste(left, collapse = ", "), " still running",
         if (length(errs)) paste0("; ", paste(unique(errs), collapse = "; ")), ")", call. = FALSE)
  }
  TRUE
}

# A message for a worker value that is not a result list: a mirai error value (the worker died or was
# killed), an R error raised in the worker, or anything else.
.sr_job_res_message <- function(res) {
  if (inherits(res, "miraiError")) {
    return(paste0("worker error: ", trimws(paste(as.character(res), collapse = " "))))
  }
  if (inherits(res, "errorValue")) {
    return(sprintf("worker stopped unexpectedly (mirai error %d)", as.integer(res)[1L]))
  }
  "worker stopped unexpectedly (no result)"
}

#' The session's settings (the names of .sr_set_names(), in ui/settings_view.R) as a named list of `sr.<name>` options, for a worker. Only settings that are set
#' (non-NULL options); sr.jev_key is never included, and neither are the test switches (run_sync,
#' ingest_sync, jev_transport), which are not settings.
.sr_job_opts <- function() {
  nm <- paste0("sr.", setdiff(.sr_set_names(), "jev_key"))
  o <- stats::setNames(lapply(nm, getOption), nm)
  o[!vapply(o, is.null, NA)]
}

#' Start `fn` (a function name) with `args` in a mirai worker and return the mirai. The worker sources the
#' app at `app_dir`, then sets `opts`, then calls the function; options are set after sourcing so global.R
#' cannot overwrite them. Nothing is read from the caller's session.
.sr_job_spawn <- function(app_dir, fn, args = list(), opts = .sr_job_opts()) {
  force(opts)
  mirai::mirai({
    source(file.path(app_dir, "global.R"), local = FALSE, encoding = "UTF-8")
    if (length(opts)) options(opts)
    do.call(fn, args)
  }, .args = list(app_dir = app_dir, fn = fn, args = args, opts = opts))
}
