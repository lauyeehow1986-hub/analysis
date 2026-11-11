# The Analyses panel worker layer: one analysis runs in a mirai worker, outside the Shiny session. The generic
# job machinery (job file, kill, options snapshot, spawn) lives in ui/job.R (.sr_job_*); the .sr_an_* functions
# here are thin wrappers over it plus the analysis-specific worker, job and settle logic.
#
# Protocol. For each analysis of the queue the panel releases the project lock and calls .sr_an_spawn().
# The worker sources the app, opens the project itself (taking the lock) and runs .sr_an_job(), which
# talks to the panel through three files, every path an argument:
#   - job file:      "<pid> <create_time>" of the worker, written first (atomically: a temp file renamed
#                    into place), so that Cancel can kill the worker and its process tree (.sr_an_kill)
#                    and never an unrelated process that reused the PID;
#   - cancel file:   created by the panel on Cancel; when it exists before the fit, the run is skipped;
#   - progress file: the current step, "fitting" then "diagnostics".
# The worker returns list(status, message) (its project handle is dropped):
#   "ok"       the fit is ok and its diagnostics ran;
#   "failed"   the fit was recorded with ok = FALSE (message: the run's error); the queue continues;
#   "refused"  sr_run or sr_diag_run (message prefixed "diagnostics: ") raised, or the worker failed outside
#              them (sourcing the app, opening the project, writing the job file), or the project was
#              locked by another session; the queue stops, the text verbatim;
#   "skipped"  the cancel file existed before the fit.
# The worker then closes the project and removes the three files, also after an early error.
#
# Cancel. .sr_an_kill() suspends the worker first (so it can neither record a run.completed for a fit whose
# callr child is being killed nor start another child), then kills its descendants and the worker, and
# waits for them to exit. It raises an error when a process survives the wait, after resuming the survivors
# (a suspended worker would keep holding the project lock). Waiting for the job file is
# pointless once the mirai has resolved: the panel checks mirai::unresolved(m) before calling it.
#
# The session never writes to a project it released: it re-opens the project after the worker ends (or
# after .sr_an_kill) and only then calls .sr_an_settle(), which records run.cancelled for a cancelled job.

.sr_an_path_ok <- function(path) .sr_job_path_ok(path)

.sr_an_put <- function(text, path) .sr_job_put(text, path)

#' Run analysis `id` of the open, writable project `p` as job `job` and then its diagnostics. Inside a
#' worker. Returns list(status, message, project); see the protocol above.
.sr_an_job <- function(p, id, job, progress_file = NULL, job_file = NULL, cancel_file = NULL) {
  why <- .sr_job_record(job_file)
  if (!is.null(why)) return(list(status = "refused", message = why, project = p))
  if (.sr_an_path_ok(cancel_file) && file.exists(cancel_file)) {
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

#' The worker body of one analysis job (the app is sourced and the options set by .sr_job_spawn): open the
#' project folder `project_dir` (the caller has released its lock), run .sr_an_job(), close the project and
#' remove the three files (also when opening fails). The value is list(status, message), never a project.
.sr_an_worker <- function(project_dir, id, job, progress_file, job_file, cancel_file) {
  out <- tryCatch({
    p <- sr_project_open(project_dir)
    if (isTRUE(p$readonly)) {
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
  }, error = function(e) list(status = "refused", message = conditionMessage(e)))
  # the session may be gone (it cannot clean up after a worker still running): the worker removes the
  # files it was given
  try(unlink(c(progress_file, job_file, cancel_file, if (length(job_file)) paste0(job_file, ".part"))),
      silent = TRUE)
  out
}

#' Start analysis `id` as job `job` in a mirai worker and return the mirai (see .sr_an_worker and
#' .sr_job_spawn). Nothing is read from the caller's session: every path is an argument. A failure to source
#' the app surfaces as a mirai error value, which .sr_an_settle() turns into a refusal.
.sr_an_spawn <- function(app_dir, project_dir, id, job, progress_file, job_file, cancel_file) {
  .sr_job_spawn(app_dir, ".sr_an_worker",
                list(project_dir = project_dir, id = id, job = job, progress_file = progress_file,
                     job_file = job_file, cancel_file = cancel_file))
}

# The job file as list(pid, time), or NULL while it is missing or not a job record.
.sr_an_job_read <- function(job_file) .sr_job_read(job_file)

#' Kill the worker recorded in `job_file` and its descendants; see .sr_job_kill.
.sr_an_kill <- function(job_file, wait = 10) .sr_job_kill(job_file, wait)

# A message for a worker value that is not list(status, message); see .sr_job_res_message.
.sr_an_res_message <- function(res) .sr_job_res_message(res)

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
    .sr_an_res_message(res)
  } else {
    ""
  }
  if (isTRUE(cancelling)) {
    # a refusal the worker reported before it was cancelled is not dropped
    with_msg <- function(notice) {
      if (identical(status, "refused") && nzchar(msg)) paste0(notice, " The worker reported: ", msg) else notice
    }
    q <- tryCatch(sr_run_cancel(p, id, job), error = function(e) e)
    if (!inherits(q, "error")) {
      return(out(with_msg(sprintf("Cancelled %s.", id)), "message", TRUE, project = q))
    }
    if (inherits(q, "sr_run_stale")) {
      # skipped, or the worker never recorded this job (it was stopped before sr_run started)
      if (identical(status, "skipped") || !identical(p$state$runs[[id]]$job, job)) {
        return(out(with_msg(sprintf("Cancelled before %s started.", id)), "message", TRUE))
      }
      return(out(with_msg(sprintf(paste0("%s finished before it could be cancelled; if its diagnostics ",
                                         "were cut short, run it again to complete them."), id)),
                 "warning", TRUE))
    }
    return(out(with_msg(sprintf("Cancel %s: %s", id, conditionMessage(q))), "error", TRUE))
  }
  if (identical(status, "ok")) return(out(stop = FALSE))
  if (identical(status, "failed")) return(out(sprintf("%s failed: %s", id, msg), "warning", FALSE))
  if (identical(status, "skipped")) return(out(stop = TRUE))
  # "refused", or a worker that died without a status
  out(sprintf("Run stopped at %s: %s", id, msg), "error", TRUE)
}
