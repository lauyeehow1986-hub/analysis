# ui/archive_job.R - the Archive panel's background-job functions and the cleanup after a cancelled export.
#
# The workers run in a mirai process outside the Shiny session. The session keeps the project lock, so a
# worker opens the project read-only; an export needs the lock and refuses.

#' Verify freeze `label` with its data files, read-only. Returns the sr_freeze_verify list.
.sr_ar_verify_worker <- function(project_dir, label, job_file) {
  .sr_job_record(job_file)
  p <- sr_project_open(project_dir)
  on.exit(if (!isTRUE(p$readonly)) sr_project_close(p), add = TRUE)
  sr_freeze_verify(p, label, data = TRUE)
}

#' Export freeze `label` to `path`. list(ok = TRUE, path) or list(ok = FALSE, error).
.sr_ar_export_worker <- function(project_dir, path, label, data, job_file) {
  .sr_job_record(job_file)
  p <- sr_project_open(project_dir)
  if (isTRUE(p$readonly)) return(list(ok = FALSE, error = "the project is locked by another session"))
  on.exit(sr_project_close(p), add = TRUE)
  tryCatch({
    sr_srproj_export(p, path, label, data)
    list(ok = TRUE, path = path)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
}

# The number of lines of events.jsonl (0 when there is none).
.sr_ar_n_events <- function(project_dir) {
  f <- sr_project_paths(project_dir)$events
  if (!file.exists(f)) return(0L)
  length(readLines(f, warn = FALSE))
}

# The type of the last event of events.jsonl, or NA.
.sr_ar_last_type <- function(project_dir) {
  f <- sr_project_paths(project_dir)$events
  if (!file.exists(f)) return(NA_character_)
  l <- readLines(f, warn = FALSE)
  l <- l[nzchar(l)]
  if (!length(l)) return(NA_character_)
  t <- tryCatch(jsonlite::parse_json(l[length(l)])$type, error = function(e) NULL)
  if (is.character(t) && length(t) == 1L) t else NA_character_
}

#' After a cancelled or dead export job: "finished" when it got as far as the srproj.exported event (the
#' archive is kept), else "removed" after deleting the archive, its .partial and the staging folder.
.sr_ar_export_cleanup <- function(project_dir, path, label, n_before) {
  if (.sr_ar_n_events(project_dir) > n_before && identical(.sr_ar_last_type(project_dir), "srproj.exported")) {
    return("finished")
  }
  unlink(path, recursive = TRUE)
  unlink(paste0(path, ".partial"), recursive = TRUE)
  unlink(file.path(project_dir, "out", paste0(".srproj-", label, ".partial")), recursive = TRUE)
  "removed"
}
