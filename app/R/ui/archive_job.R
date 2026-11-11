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

# TRUE when an srproj.exported event is among the lines of events.jsonl after the first `n_before`.
.sr_ar_exported_after <- function(project_dir, n_before) {
  f <- sr_project_paths(project_dir)$events
  if (!file.exists(f)) return(FALSE)
  l <- readLines(f, warn = FALSE)
  l <- l[nzchar(l)]
  if (length(l) <= n_before) return(FALSE)
  any(vapply(l[(n_before + 1L):length(l)], function(x) {
    t <- tryCatch(jsonlite::parse_json(x)$type, error = function(e) NULL)
    is.character(t) && length(t) == 1L && identical(t, "srproj.exported")
  }, NA))
}

#' After a cancelled or dead export job: "finished" when its srproj.exported event was written (the archive
#' is kept), else "removed" after deleting what the job made: the archive, its .partial and the staging
#' folder. A path in `keep` (normalized paths that existed before the job started) is never removed.
.sr_ar_export_cleanup <- function(project_dir, path, label, n_before, keep = character()) {
  if (.sr_ar_n_events(project_dir) > n_before && .sr_ar_exported_after(project_dir, n_before)) {
    return("finished")
  }
  norm <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  kept <- norm(as.character(keep))
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
.sr_im_worker <- function(archive, dir, job_file) {
  .sr_job_record(job_file)
  tryCatch({
    p <- sr_srproj_import(archive, dir, verify_data = TRUE)
    imp <- p$import
    try(sr_duck_close(p), silent = TRUE)
    try(sr_project_close(p), silent = TRUE)
    list(ok = TRUE, dir = dir, import = imp)
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
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
