# The Analyses panel worker layer: one analysis runs in a mirai worker, outside the Shiny session.
#
# Protocol. For each analysis of the queue the panel releases the project lock and calls .sr_an_spawn().
# The worker sources the app, opens the project itself (taking the lock) and runs .sr_an_job(), which
# talks to the panel through three files, every path an argument:
#   - job file:      "<pid> <create_time>" of the worker, written first, so that Cancel can kill the worker
#                    and its process tree (.sr_an_kill) and never an unrelated process that reused the PID;
#   - cancel file:   created by the panel on Cancel; when it exists before the fit, the run is skipped;
#   - progress file: the current step, "fitting" then "diagnostics".
# The worker returns list(status, message) (its project handle is dropped):
#   "ok"       the fit is ok and its diagnostics ran;
#   "failed"   the fit was recorded with ok = FALSE (message: the run's error); the queue continues;
#   "refused"  sr_run or sr_diag_run (message prefixed "diagnostics: ") raised, or the worker failed outside
#              them, or the project was locked by another session; the queue stops, the text verbatim;
#   "skipped"  the cancel file existed before the fit.
# The worker then closes the project and removes the three files. The session never writes to a project it
# released: it re-opens the project after the worker ends (or after .sr_an_kill) and only then calls
# .sr_an_settle(), which records run.cancelled for a cancelled job.

# Write one line to `path` when it is a file path; never errors.
.sr_an_put <- function(text, path) {
  if (is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path)) {
    try(writeLines(text, path), silent = TRUE)
  }
  invisible(NULL)
}

#' Run analysis `id` of the open, writable project `p` as job `job` and then its diagnostics. Inside a
#' worker. Returns list(status, message, project); see the protocol above.
.sr_an_job <- function(p, id, job, progress_file = NULL, job_file = NULL, cancel_file = NULL) {
  ct <- tryCatch(as.numeric(ps::ps_create_time(ps::ps_handle())), error = function(e) NA_real_)
  .sr_an_put(sprintf("%d %.6f", Sys.getpid(), ct), job_file)
  if (is.character(cancel_file) && length(cancel_file) == 1L && !is.na(cancel_file) && file.exists(cancel_file)) {
    return(list(status = "skipped", message = NULL, project = p))
  }
  .sr_an_put("fitting", progress_file)
  q <- tryCatch(sr_run(p, id, job = job), error = function(e) e)
  if (inherits(q, "error")) return(list(status = "refused", message = conditionMessage(q), project = p))
  r <- q$state$runs[[id]]
  if (!isTRUE(r$ok)) {
    return(list(status = "failed", message = as.character(r$error %||% NA_character_), project = q))
  }
  .sr_an_put("diagnostics", progress_file)
  d <- tryCatch(sr_diag_run(q, id), error = function(e) e)
  if (inherits(d, "error")) {
    return(list(status = "refused", message = paste0("diagnostics: ", conditionMessage(d)), project = q))
  }
  list(status = "ok", message = NULL, project = d)
}

#' Start analysis `id` as job `job` in a mirai worker and return the mirai. The worker sources the app at
#' `app_dir`, opens the project folder `project_dir` (the caller has released its lock), runs .sr_an_job(),
#' closes the project and removes the three files. Nothing is read from the caller's session: every path
#' is an argument. The value is list(status, message), never a project.
.sr_an_spawn <- function(app_dir, project_dir, id, job, progress_file, job_file, cancel_file) {
  mirai::mirai({
    source(file.path(app_dir, "global.R"), local = FALSE, encoding = "UTF-8")
    p <- sr_project_open(project_dir)
    out <- if (isTRUE(p$readonly)) {
      list(status = "refused", message = "the project is locked by another session")
    } else {
      res <- tryCatch(
        .sr_an_job(p, id, job, progress_file, job_file, cancel_file),
        error = function(e) list(status = "refused", message = conditionMessage(e)))
      try(sr_duck_close(p), silent = TRUE)
      try(sr_project_close(p), silent = TRUE)
      res$project <- NULL
      res
    }
    # the session may be gone (it cannot clean up after a worker still running): the worker removes the
    # files it was given
    try(unlink(c(progress_file, job_file, cancel_file)), silent = TRUE)
    out
  }, .args = list(app_dir = app_dir, project_dir = project_dir, id = id, job = job,
                  progress_file = progress_file, job_file = job_file, cancel_file = cancel_file))
}

# The job file as list(pid, time), or NULL while it is missing, partly written or not a job record.
.sr_an_job_read <- function(job_file) {
  if (!is.character(job_file) || length(job_file) != 1L || is.na(job_file) || !file.exists(job_file)) {
    return(NULL)
  }
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
#' from the recorded one by more than 0.01 s (the PID was reused). Then waits up to `wait` seconds until
#' none of the killed processes runs. TRUE when it killed the worker; an error when they still run.
.sr_an_kill <- function(job_file, wait = 10) {
  elapsed <- function(t0) as.numeric(difftime(Sys.time(), t0, units = "secs"))
  t0 <- Sys.time()
  rec <- .sr_an_job_read(job_file)
  while (is.null(rec) && elapsed(t0) < wait) {
    Sys.sleep(0.1)
    rec <- .sr_an_job_read(job_file)
  }
  if (is.null(rec)) return(FALSE)
  h <- tryCatch(ps::ps_handle(rec$pid), error = function(e) NULL)
  if (is.null(h)) return(FALSE)
  ct <- tryCatch(as.numeric(ps::ps_create_time(h)), error = function(e) NA_real_)
  if (!is.finite(ct) || abs(ct - rec$time) > 0.01) return(FALSE)
  kids <- tryCatch(ps::ps_children(h, recursive = TRUE), error = function(e) list())
  procs <- c(kids, list(h))
  pids <- vapply(procs, function(x) as.integer(ps::ps_pid(x)), 0L)
  for (x in procs) {
    # an already exited process is fine; any other failure shows up in the wait below
    tryCatch(ps::ps_kill(x), error = function(e) NULL)
  }
  t1 <- Sys.time()
  alive <- function() any(vapply(pids, .sr_pid_running, NA))
  while (alive() && elapsed(t1) < wait) Sys.sleep(0.1)
  if (alive()) {
    left <- pids[vapply(pids, .sr_pid_running, NA)]
    stop("could not stop the analysis worker (process ", paste(left, collapse = ", "), " still running)",
         call. = FALSE)
  }
  TRUE
}

#' Turn a finished queue item into what the panel does next: list(project, notice, type, stop). `res` is
#' the worker's value (list(status, message)), or anything else when the worker died (a mirai error value
#' after a kill); `cancelling` is TRUE when the user pressed Cancel. Only the cancel path writes (it
#' records run.cancelled through sr_run_cancel); `p` must be the project the session re-opened.
.sr_an_settle <- function(p, id, job, res, cancelling) {
  out <- function(notice = NULL, type = "message", stop, project = p) {
    list(project = project, notice = notice, type = type, stop = stop)
  }
  status <- if (is.list(res) && is.character(res$status) && length(res$status) == 1L) res$status else NA_character_
  msg <- if (is.list(res) && is.character(res$message) && length(res$message)) {
    paste(res$message, collapse = " ")
  } else if (is.na(status)) {
    paste(as.character(res), collapse = " ")
  } else {
    ""
  }
  if (isTRUE(cancelling)) {
    q <- tryCatch(sr_run_cancel(p, id, job), error = function(e) e)
    if (!inherits(q, "error")) return(out(sprintf("Cancelled %s.", id), "message", TRUE, project = q))
    if (inherits(q, "sr_run_stale")) {
      # skipped, or the worker never recorded this job (it was killed before sr_run started)
      if (identical(status, "skipped") || !identical(p$state$runs[[id]]$job, job)) {
        return(out(sprintf("Cancelled before %s started.", id), "message", TRUE))
      }
      return(out(sprintf(paste0("%s finished before it could be cancelled; if its diagnostics were cut ",
                                "short, run it again to complete them."), id), "warning", TRUE))
    }
    return(out(sprintf("Cancel %s: %s", id, conditionMessage(q)), "error", TRUE))
  }
  if (identical(status, "ok")) return(out(stop = FALSE))
  if (identical(status, "failed")) return(out(sprintf("%s failed: %s", id, msg), "warning", FALSE))
  if (identical(status, "skipped")) return(out(stop = TRUE))
  # "refused", or a worker that died without a status
  out(sprintf("Run stopped at %s: %s", id, msg), "error", TRUE)
}
