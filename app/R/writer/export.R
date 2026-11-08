# writer/export.R - the export: the manuscript and its evidence in one folder, written atomically.
#
#   sr_export(project, opts)   model -> claim guard -> out/exports/<id>/ -> a doc.exported event
#   sr_exports(project)        data.frame(kind, id, n_files, audit_tip, sap_hash), one row per export
#
# The folder out/exports/<id>/ holds: manuscript.docx, supplement.docx, figures/*.png, analysis.R,
# renv.lock, references.json (CSL-JSON), references.ris, placeholders.json, checklist_strobe.csv (only when
# the model has a STROBE checklist: a CONSORT project has none) and
# manifest.json (the relative path and SHA-256 of every other file, plus id, created, audit_tip, sap_hash,
# preset, suppress_k and lockfile_matches_runs). The files are written into out/exports/.<id>.partial and the folder is renamed once it is
# complete, so an export is whole or absent. An export folder is never overwritten.
#
# placeholders.json is the registry rows (id, kind, text, analysis, source, suppressed, and value only
# for a row that is not suppressed: a suppressed count never leaves the registry). It is an array; the
# small-cell threshold of the document is suppress_k in manifest.json, which a re-run check
# (sr_ph_verify) needs to build the registry.

SR_EXPORT_KINDS <- c("manuscript", "sap", "feasibility")

# ---- the reducer ----------------------------------------------------------------------------------

sr_reducer_register("doc.exported", function(state, event) {
  p <- event$payload
  one <- function(x, what) {
    if (!(is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x))) {
      stop("doc.exported needs ", what, " as a single non-empty string", call. = FALSE)
    }
    x
  }
  kind <- one(p$kind, "kind")
  if (!(kind %in% SR_EXPORT_KINDS)) {
    stop("doc.exported kind must be one of ", paste(SR_EXPORT_KINDS, collapse = ", "), call. = FALSE)
  }
  files <- lapply(p$files %||% list(), function(f) list(path = as.character(f$path), sha = as.character(f$sha)))
  rec <- list(kind = kind, id = one(p$id, "an id"), files = unname(files),
              audit_tip = one(p$audit_tip, "audit_tip"), sap_hash = one(p$sap_hash, "sap_hash"),
              preset = as.character(p$preset %||% NA_character_)[1L])
  state$exports <- c(state$exports %||% list(), list(rec))
  state
})

#' The exports of a project, in order.
#' @param project an open project.
#' @return data.frame(kind, id, n_files, audit_tip, sap_hash).
sr_exports <- function(project) {
  ex <- project$state$exports %||% list()
  chr <- function(f) vapply(ex, function(e) as.character(e[[f]] %||% NA_character_)[1L], "")
  data.frame(kind = chr("kind"), id = chr("id"),
             n_files = vapply(ex, function(e) length(e$files), 0L),
             audit_tip = chr("audit_tip"), sap_hash = chr("sap_hash"), stringsAsFactors = FALSE)
}

# ---- the pieces -----------------------------------------------------------------------------------

.sr_ex_sha <- function(path) .sr_sha256(.sr_read_bytes(path))

# Write text as UTF-8 with LF line endings (a text-mode connection would write CRLF on Windows).
.sr_ex_write_text <- function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  writeBin(charToRaw(enc2utf8(text)), path)
  invisible(path)
}

# The placeholder registry as JSON rows: a suppressed row carries no `value`.
.sr_ex_placeholders_json <- function(reg) {
  rows <- sr_ph_table(reg)
  items <- lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, , drop = FALSE]
    it <- list(id = r$id, kind = r$kind, text = r$text)
    if (!isTRUE(r$suppressed)) it$value <- r$value
    it$analysis <- r$analysis
    it$source <- r$source
    it$suppressed <- isTRUE(r$suppressed)
    it
  })
  if (!length(items)) return("[]")
  .sr_json(items)
}

# The STROBE checklist block of the model as CSV (LF, UTF-8, empty string for NA); nothing is written when the
# model has none (a CONSORT project).
.sr_ex_write_checklist <- function(path, model) {
  blk <- Filter(function(b) identical(b$type, "checklist") && identical(b$id, "strobe"), model$supplement)
  if (!length(blk)) return(invisible(NULL))
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  utils::write.csv(blk[[1L]]$df, con, row.names = FALSE, na = "", eol = "\n", fileEncoding = "UTF-8")
  invisible(path)
}

# Every file under `dir`, relative, sorted bytewise.
.sr_ex_files <- function(dir) {
  f <- list.files(dir, recursive = TRUE, all.files = TRUE, include.dirs = FALSE)
  sort(f, method = "radix")
}

# The files of the export in the staging folder `st`.
.sr_ex_write_all <- function(st, project, model, id, tip, created) {
  props <- list(audit_tip = tip, sap_hash = model$meta$sap_hash, export_id = id, created = created)
  fig_dir <- file.path(st, "figures")
  sr_docx_render(model, "main", file.path(st, "manuscript.docx"), fig_dir, props)
  sr_docx_render(model, "supplement", file.path(st, "supplement.docx"), fig_dir, props)

  ids <- names(model$meta$purposes)
  .sr_ex_write_text(file.path(st, "analysis.R"), sr_analysis_script(project, ids))
  matches <- sr_lockfile_matches_runs(project, ids, model$meta$libsnap)
  .sr_ex_write_text(file.path(st, "renv.lock"), paste0(.sr_json(sr_lockfile(model$meta$libsnap, matches)), "\n"))

  refs <- Filter(function(b) identical(b$type, "refs"), model$main)
  items <- sr_cite_csl(if (length(refs)) refs[[1L]]$keys else character())
  .sr_ex_write_text(file.path(st, "references.json"), paste0(sr_cite_csl_json(items), "\n"))
  ris <- if (length(items)) paste0(paste(sr_cite_ris(items), collapse = "\n"), "\n") else ""
  .sr_ex_write_text(file.path(st, "references.ris"), ris)

  .sr_ex_write_text(file.path(st, "placeholders.json"), paste0(.sr_ex_placeholders_json(model$reg), "\n"))
  .sr_ex_write_checklist(file.path(st, "checklist_strobe.csv"), model)

  rel <- .sr_ex_files(st)
  man <- list(id = id, created = created, audit_tip = tip, sap_hash = model$meta$sap_hash,
              preset = model$meta$preset, suppress_k = model$reg$k, lockfile_matches_runs = matches,
              files = lapply(rel, function(f) list(path = f, sha = .sr_ex_sha(file.path(st, f)))))
  .sr_ex_write_text(file.path(st, "manifest.json"), paste0(.sr_json(man), "\n"))
  invisible(TRUE)
}

# ---- the export -----------------------------------------------------------------------------------

#' Export the manuscript: the Word files, figures, the analysis script, the lockfile, references,
#' placeholders, the STROBE checklist and a manifest, in out/exports/<id>/.
#'
#' Refused: a read-only project, a model refusal (sr_doc_model), any block row of the claim guard (one line
#' per row). The id is the next number that has neither an event nor a folder on disk. The folder appears whole or not at all, and a doc.exported event
#' (id, files with SHA-256 relative to the project folder, audit tip, SAP hash, preset) is appended after it.
#' @param project an open, writable project with a locked and run SAP.
#' @param opts from sr_writer_opts().
#' @return the project with the doc.exported event applied.
sr_export <- function(project, opts = sr_writer_opts()) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  model <- sr_doc_model(project, opts)

  gd <- sr_claim_guard(model)
  bad <- gd[gd$severity %in% "block", , drop = FALSE]
  if (nrow(bad)) {
    stop("the claim guard blocks the export:\n",
         paste0(bad$rule, " in ", bad$block, ": ", bad$detail, collapse = "\n"), call. = FALSE)
  }

  paths <- project$paths
  ex <- project$state$exports %||% list()
  k <- sum(vapply(ex, function(e) identical(e$kind, "manuscript"), NA)) + 1L
  root <- file.path(paths$out, "exports")
  # an orphan folder (a crash between the rename and the event) is skipped, never reused or blocking
  while (dir.exists(file.path(root, sprintf("export-%03d", k))) || file.exists(file.path(root, sprintf("export-%03d", k)))) {
    k <- k + 1L
  }
  id <- sprintf("export-%03d", k)
  dir <- file.path(root, id)
  st <- file.path(root, paste0(".", id, ".partial"))
  if (dir.exists(dir) || file.exists(dir)) {
    stop("the export folder ", file.path("out", "exports", id), " already exists; it is never overwritten", call. = FALSE)
  }

  tip <- sr_audit_tip(paths$audit)
  created <- .sr_now()

  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  unlink(st, recursive = TRUE)
  dir.create(st, recursive = TRUE)
  done <- FALSE
  on.exit(if (!done) unlink(st, recursive = TRUE), add = TRUE)
  .sr_ex_write_all(st, project, model, id, tip, created)
  if (!isTRUE(file.rename(st, dir))) {
    stop("could not move the finished export into ", file.path("out", "exports", id), call. = FALSE)
  }
  done <- TRUE

  rel <- .sr_ex_files(dir)
  files <- lapply(rel, function(f) list(path = paste("out", "exports", id, f, sep = "/"),
                                        sha = .sr_ex_sha(file.path(dir, f))))
  payload <- list(kind = "manuscript", id = id, files = files, audit_tip = tip,
                  sap_hash = model$meta$sap_hash, preset = model$meta$preset)
  tryCatch(sr_project_append(project, "doc.exported", payload), error = function(e) {
    # the folder goes unless the event did get written (a crash between the event and its audit entry)
    evs <- tryCatch(sr_events_read(paths$events), error = function(e2) list())
    last <- if (length(evs)) evs[[length(evs)]] else NULL
    if (!(identical(last$type, "doc.exported") && identical(last$payload$id, id))) {
      unlink(dir, recursive = TRUE)
    }
    stop(e)   # the condition object itself: its class and call survive
  })
}
