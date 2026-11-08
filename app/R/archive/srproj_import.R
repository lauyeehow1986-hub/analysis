# archive/srproj_import.R - import a .srproj at any path, all or nothing.
#
#   sr_srproj_import(archive, dir, verify_data = TRUE)
#
# Order: scan the tar headers (.sr_tar_check) before anything is extracted, extract to <dir>.partial,
# check every member against the index (SHA-256, no extra, no missing), rename into place, open, and
# run sr_freeze_verify. Any failure removes the partial folder and, once renamed, the project folder too.

sr_reducer_register("srproj.imported", function(state, event) {
  im <- state$srproj_imports %||% list()
  im[[length(im) + 1L]] <- event$payload
  state$srproj_imports <- im
  state
})

#' @return the open, writable project at `dir`; `$import` holds label, data_mode, archive_sha256,
#'   parts_missing, sources_missing and the sr_freeze_verify result.
sr_srproj_import <- function(archive, dir, verify_data = TRUE) {
  if (!is.character(archive) || length(archive) != 1L || is.na(archive) || !file.exists(archive)) {
    stop("archive not found: ", paste(format(archive), collapse = ","), call. = FALSE)
  }
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    stop("the target folder must be a path", call. = FALSE)
  }
  if (file.exists(dir) && (!dir.exists(dir) || length(list.files(dir, all.files = TRUE, no.. = TRUE)))) {
    stop("the target folder is not empty: ", dir, call. = FALSE)
  }
  parent <- dirname(dir)
  if (!dir.exists(parent)) stop("the folder ", parent, " does not exist", call. = FALSE)
  partial <- paste0(dir, ".partial")
  if (file.exists(partial)) stop(basename(partial), " already exists; remove it first", call. = FALSE)

  ent <- .sr_tar_entries(archive)
  .sr_tar_check(ent)
  files <- ent$name[ent$type == "0"]
  if (!("srproj.json" %in% files)) stop("not a StatRouter archive (no srproj.json)", call. = FALSE)
  arch_sha <- sr_sha256_file(archive)

  made_dir <- FALSE
  done <- FALSE
  on.exit({
    if (dir.exists(partial)) unlink(partial, recursive = TRUE)
    if (!done && made_dir) {
      unlink(dir, recursive = TRUE)
    }
  }, add = TRUE)

  dir.create(partial)
  utils::untar(archive, exdir = partial, tar = "internal")
  idx <- .sr_read_json_file(file.path(partial, "srproj.json"))
  if (!identical(idx$format, SR_SRPROJ_FORMAT)) stop("not a StatRouter archive (unknown format)", call. = FALSE)
  ver <- suppressWarnings(as.integer(idx$format_version)[1L])
  if (is.na(ver) || ver > SR_SRPROJ_VERSION) {
    stop("archive written by a newer StatRouter (format ", format(idx$format_version), ")", call. = FALSE)
  }
  label <- idx$label
  if (!is.character(label) || length(label) != 1L || is.na(label) || !.sr_archive_safe_path(label) ||
      grepl("/", label, fixed = TRUE)) {
    stop("the archive index has an invalid freeze label", call. = FALSE)
  }
  ipaths <- vapply(idx$files, function(f) as.character(f$path)[1L], "")
  isha <- vapply(idx$files, function(f) as.character(f$sha256)[1L], "")
  body <- setdiff(files, "srproj.json")
  extra <- setdiff(body, ipaths)
  if (length(extra)) stop("member '", extra[1L], "' is not listed in the index", call. = FALSE)
  bad <- setdiff(ipaths, body)
  for (i in seq_along(ipaths)) {
    f <- file.path(partial, ipaths[i])
    if (ipaths[i] %in% bad) next
    if (!file.exists(f) || !identical(sr_sha256_file(f), isha[i])) bad <- c(bad, ipaths[i])
  }
  if (length(bad)) {
    stop("the archive does not match its index: ", paste(utils::head(unique(bad), 5L), collapse = ", "),
         call. = FALSE)
  }

  fdir <- file.path(partial, "out", "freezes", label)
  dir.create(fdir, recursive = TRUE, showWarnings = FALSE)
  if (!isTRUE(file.rename(file.path(partial, "srproj.json"), file.path(fdir, "srproj.json")))) {
    stop("could not place the archive index", call. = FALSE)
  }
  if (dir.exists(dir)) unlink(dir, recursive = TRUE)
  made_dir <- TRUE
  if (!isTRUE(file.rename(partial, dir))) stop("could not move the imported project into place", call. = FALSE)

  q <- sr_project_open(dir)
  v <- tryCatch(sr_freeze_verify(q, label, data = verify_data), error = function(e) {
    sr_project_close(q)
    stop(e)
  })
  if (!isTRUE(v$ok)) {
    sr_project_close(q)
    stop("freeze '", label, "' does not verify: ", paste(v$problems, collapse = "; "), call. = FALSE)
  }
  om <- idx$omitted
  parts <- as.character(unlist(om$parts))
  srcs <- as.character(unlist(lapply(om$sources, function(s) s$table)))
  payload <- list(label = label, data_mode = idx$data_mode, file = basename(archive), archive_sha256 = arch_sha,
                  parts_missing = parts, sources_missing = srcs)
  q <- tryCatch(sr_project_append(q, "srproj.imported", payload), error = function(e) {
    sr_project_close(q)
    stop(e)
  })
  q$import <- list(label = label, data_mode = idx$data_mode, archive_sha256 = arch_sha,
                   parts_missing = parts, sources_missing = srcs, verify = v)
  done <- TRUE
  q
}
