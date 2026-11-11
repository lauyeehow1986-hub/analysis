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

# The event lines of events.jsonl the way they are counted and indexed everywhere here: complete
# (LF-terminated) lines only, blank ones dropped. A torn final fragment is not a line: the next append
# truncates it away (sr_project_append), so counting it would hide the event that replaces it.
.sr_ar_event_lines <- function(project_dir) {
  f <- sr_project_paths(project_dir)$events
  if (!file.exists(f)) return(character())
  l <- sub("\r$", "", .sr_split_lf(.sr_read_bytes(f))$lines)
  l[nzchar(l)]
}

# The number of event lines of events.jsonl (0 when there is none).
.sr_ar_n_events <- function(project_dir) length(.sr_ar_event_lines(project_dir))

# The type of the last event of events.jsonl, or NA.
.sr_ar_last_type <- function(project_dir) {
  l <- .sr_ar_event_lines(project_dir)
  if (!length(l)) return(NA_character_)
  t <- tryCatch(jsonlite::parse_json(l[length(l)])$type, error = function(e) NULL)
  if (is.character(t) && length(t) == 1L) t else NA_character_
}

# TRUE when an srproj.exported event is among the event lines after the first `n_before`. With `file` and
# `sha`, only an event for that archive counts: its payload names the file (basename) and, when `sha` is
# not NULL, holds that archive_sha256.
.sr_ar_exported_after <- function(project_dir, n_before, file = NULL, sha = NULL) {
  l <- .sr_ar_event_lines(project_dir)
  if (length(l) <= n_before) return(FALSE)
  any(vapply(l[(n_before + 1L):length(l)], function(x) {
    e <- tryCatch(jsonlite::parse_json(x), error = function(e) NULL)
    if (!is.list(e) || !identical(e$type, "srproj.exported")) return(FALSE)
    pl <- e$payload
    if (!is.null(file) && !identical(pl$file, basename(file))) return(FALSE)
    is.null(sha) || identical(pl$archive_sha256, sha)
  }, NA))
}

#' After a cancelled or dead export job: "finished" when its srproj.exported event was written (the archive
#' is kept), else "removed" after deleting what the job made: the archive, its .partial and the staging
#' folder. A path in `keep` (normalized paths that existed before the job started) is never removed.
#' The event is this job's: written after the first `n_before` event lines (counted by .sr_ar_n_events) and
#' naming this archive, with the archive_sha256 of the file at `path` when it exists.
.sr_ar_export_cleanup <- function(project_dir, path, label, n_before, keep = character()) {
  norm <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  kept <- norm(as.character(keep))
  # find this job's event by type and file name first; hash the archive only when one exists, to confirm it
  finished <- .sr_ar_exported_after(project_dir, n_before, file = path)
  if (finished && file.exists(path) && !dir.exists(path) && !(norm(path) %in% kept)) {
    sha <- tryCatch(sr_sha256_file(path), error = function(e) NULL)   # unreadable: match the file name only
    finished <- .sr_ar_exported_after(project_dir, n_before, file = path, sha = sha)
  }
  if (finished) return("finished")
  rm1 <- function(x, recursive) {
    if (!(norm(x) %in% kept)) unlink(x, recursive = recursive)
  }
  rm1(path, FALSE)
  rm1(paste0(path, ".partial"), FALSE)
  rm1(file.path(project_dir, "out", paste0(".srproj-", label, ".partial")), TRUE)
  "removed"
}

# ---- import ---------------------------------------------------------------------------------------------

# Which of the three paths an import may create exist now: the target folder, <dir>.partial and
# <dir>.partial.src (trailing slashes are stripped as sr_srproj_import does). A named logical: dir, partial, src.
.sr_im_snapshot <- function(dir) {
  dir <- sub("[/\\]+$", "", dir)
  c(dir = file.exists(dir), partial = file.exists(paste0(dir, ".partial")),
    src = file.exists(paste0(dir, ".partial.src")))
}

#' After a cancelled or failed import job: remove each of the three paths that exists now and did not exist in
#' `before` (a .sr_im_snapshot taken before the job started). A target folder that existed (empty) before is
#' kept: only what the job put into it is removed, and it is put back empty if the job removed it. Returns the
#' removed paths invisibly.
.sr_im_cleanup <- function(dir, before) {
  dir <- sub("[/\\]+$", "", dir)
  paths <- c(dir = dir, partial = paste0(dir, ".partial"), src = paste0(dir, ".partial.src"))
  gone <- character()
  for (nm in names(paths)) {
    p <- paths[[nm]]
    was <- isTRUE(before[[nm]])
    if (!was && file.exists(p)) {
      unlink(p, recursive = TRUE)
      gone <- c(gone, p)
    }
  }
  if (isTRUE(before[["dir"]])) {
    if (dir.exists(dir)) {
      kids <- list.files(dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
      if (length(kids)) {
        unlink(kids, recursive = TRUE)
        gone <- c(gone, kids)
      }
    } else {
      dir.create(dir, showWarnings = FALSE)
    }
  }
  invisible(gone)
}

#' Import `archive` into the new folder `dir`. list(ok = TRUE, dir, import) or list(ok = FALSE, error). The
#' project is closed again (the session opens it).
#'
#' `cancel_file` (created by the session on Cancel) closes the window before the worker has recorded itself in
#' `job_file`, when Cancel finds nothing to kill: the worker records itself first and then checks the file, so
#' a Cancel that gave up waiting for the job file always finds the worker stopping here. When the file exists
#' before the import, nothing is imported; when it appears during the import, what the import created is
#' removed again (.sr_im_cleanup against a snapshot taken just before it, so a folder that existed before is
#' never removed). Either way the value is list(ok = FALSE, cancelled = TRUE, error).
.sr_im_worker <- function(archive, dir, job_file, cancel_file = NULL) {
  .sr_job_record(job_file)
  asked <- function() .sr_job_path_ok(cancel_file) && file.exists(cancel_file)
  if (asked()) return(list(ok = FALSE, cancelled = TRUE, error = "the import was cancelled before it started"))
  before <- .sr_im_snapshot(dir)
  tryCatch({
    p <- sr_srproj_import(archive, dir, verify_data = TRUE)
    imp <- p$import
    try(sr_duck_close(p), silent = TRUE)
    try(sr_project_close(p), silent = TRUE)
    if (asked()) {
      .sr_im_cleanup(dir, before)
      return(list(ok = FALSE, cancelled = TRUE, error = "the import was cancelled; what it created was removed"))
    }
    list(ok = TRUE, dir = dir, import = imp)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
}

# The worker's result for an import that finished on disk (its srproj.imported event is written) when the
# session never received it: list(ok = TRUE, dir, import) rebuilt from the last srproj.imported event and a
# fresh freeze verification. The project is closed again.
.sr_im_result_from_disk <- function(dir) {
  q <- sr_project_open(dir)
  on.exit({
    try(sr_duck_close(q), silent = TRUE)
    try(sr_project_close(q), silent = TRUE)
  }, add = TRUE)
  im <- q$state$srproj_imports
  if (!length(im)) stop("no srproj.imported event", call. = FALSE)
  pl <- im[[length(im)]]
  v <- sr_freeze_verify(q, pl$label, data = TRUE)
  list(ok = TRUE, dir = dir,
       import = list(label = pl$label, data_mode = pl$data_mode, archive_sha256 = pl$archive_sha256,
                     parts_missing = pl$parts_missing, sources_missing = pl$sources_missing, verify = v))
}

# The $import list of sr_srproj_import as a field/value frame.
.sr_im_summary <- function(imp) {
  lst <- function(x) {
    x <- as.character(unlist(x))
    if (length(x)) paste(x, collapse = ", ") else "none"
  }
  v <- imp$verify
  vtxt <- if (is.list(v) && isTRUE(v$ok)) {
    "ok"
  } else if (is.list(v) && length(v$problems)) {
    paste(as.character(unlist(v$problems)), collapse = "; ")
  } else {
    "not ok"
  }
  data.frame(
    field = c("label", "data mode", "archive sha256", "parts missing", "sources missing", "verify"),
    value = c(as.character(imp$label)[1L], as.character(imp$data_mode)[1L],
              substr(as.character(imp$archive_sha256)[1L], 1L, 16L), lst(imp$parts_missing),
              lst(imp$sources_missing), vtxt),
    stringsAsFactors = FALSE
  )
}
