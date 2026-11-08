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
  dir <- sub("[/\\]+$", "", dir)
  if (!nzchar(dir)) stop("the target folder must be a path", call. = FALSE)
  if (file.exists(dir) && (!dir.exists(dir) || length(list.files(dir, all.files = TRUE, no.. = TRUE)))) {
    stop("the target folder is not empty: ", dir, call. = FALSE)
  }
  parent <- dirname(dir)
  if (!dir.exists(parent)) stop("the folder ", parent, " does not exist", call. = FALSE)
  partial <- paste0(dir, ".partial")
  if (file.exists(partial)) stop(basename(partial), " already exists; remove it first", call. = FALSE)

  # TOCTOU guard: the archive is hashed before the scan, after the scan and check, and after the
  # extraction; all three must match, so what was scanned is what was extracted.
  arch_sha <- sr_sha256_file(archive)
  ent <- .sr_tar_entries(archive)
  .sr_tar_check(ent)
  if (!identical(sr_sha256_file(archive), arch_sha)) {
    stop("the archive changed while it was being scanned", call. = FALSE)
  }
  files <- ent$name[ent$type == "0"]
  if (!("srproj.json" %in% files)) stop("not a StatRouter archive (no srproj.json)", call. = FALSE)
  dirs <- ent$name[ent$type == "5"]
  special <- c(files, dirs)[grepl("(^|/)[.]lock(/|$)|[.]partial/?$|[.]tmp/?$", c(files, dirs), ignore.case = TRUE)]
  if (length(special)) {
    stop("archive member '", special[1L], "' is a lock or temporary file", call. = FALSE)
  }
  # the index is written into out/freezes/<label>/ by the importer, so no member may sit there already
  occ <- files[grepl("^out/freezes/[^/]+/srproj[.]json$", files, ignore.case = TRUE)]
  if (length(occ)) stop("the archive already holds ", occ[1L], call. = FALSE)
  cap <- suppressWarnings(as.numeric(sr_opt("srproj_max_total", 68719476736)))
  if (length(cap) != 1L || is.na(cap) || cap < 0) {
    stop("sr.srproj_max_total must be a number of bytes, zero or more", call. = FALSE)
  }
  if (sum(as.numeric(ent$size[ent$type == "0"])) > cap) {
    stop("the archive total size exceeds the limit (sr.srproj_max_total)", call. = FALSE)
  }

  # made_dir: the project folder at `dir` is this call's own (renamed into place). was_empty: `dir` was a
  # pre-existing empty folder, which cleanup puts back (empty) rather than leaving nothing.
  made_dir <- FALSE
  was_empty <- dir.exists(dir)
  done <- FALSE
  on.exit({
    if (dir.exists(partial)) {
      unlink(partial, recursive = TRUE)
      if (dir.exists(partial)) warning("could not remove ", basename(partial), "; remove it by hand", call. = FALSE)
    }
    if (!done && made_dir) {
      unlink(dir, recursive = TRUE)
      if (was_empty) dir.create(dir)
    }
  }, add = TRUE)

  dir.create(partial)
  # only the regular files the scanner approved may be written (defence in depth: untar2 skips any other
  # name); folders come from the approved file paths and the scanned folder members
  utils::untar(archive, files = files, exdir = partial, tar = "internal")
  if (!identical(sr_sha256_file(archive), arch_sha)) {
    stop("the archive changed while it was being imported", call. = FALSE)
  }
  for (i in which(ent$type == "0")) {
    got <- file.size(file.path(partial, ent$name[i]))
    if (is.na(got) || got != ent$size[i]) {
      stop("archive member '", ent$name[i], "' differs in size from its header", call. = FALSE)
    }
  }
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
  ok1 <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!is.list(idx$files) || !all(vapply(idx$files, function(f) {
    is.list(f) && ok1(f$path) && ok1(f$sha256)
  }, NA))) {
    stop("the archive index is malformed (files)", call. = FALSE)
  }
  ipaths <- vapply(idx$files, function(f) f$path, "")
  isha <- vapply(idx$files, function(f) f$sha256, "")
  if (anyDuplicated(ipaths) || !all(.sr_archive_safe_path(ipaths))) {
    stop("the archive index is malformed (member names)", call. = FALSE)
  }
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
  # Windows cannot rename over an existing folder, so an empty `dir` is removed only now, right before the
  # rename, after re-checking it is empty (recursive = TRUE is how Windows removes even an empty folder);
  # if the rename then fails the empty folder is put back.
  if (was_empty) {
    if (length(list.files(dir, all.files = TRUE, no.. = TRUE))) {
      stop("the target folder is not empty: ", dir, call. = FALSE)
    }
    unlink(dir, recursive = TRUE)
    if (dir.exists(dir)) stop("could not replace the empty target folder ", dir, call. = FALSE)
  }
  if (!isTRUE(file.rename(partial, dir))) {
    if (was_empty && !dir.exists(dir)) dir.create(dir)
    stop("could not move the imported project into place", call. = FALSE)
  }
  made_dir <- TRUE

  q <- sr_project_open(dir)
  fail <- function(msg) {
    sr_project_close(q)
    stop(msg, call. = FALSE)
  }
  if (isTRUE(q$readonly)) fail("the imported project could not be opened for writing")
  if (!isTRUE(q$verify$ok)) {
    fail(paste0("project history does not verify: ", paste(q$verify$problems, collapse = "; ")))
  }
  fseq <- suppressWarnings(as.integer(q$state$freezes[[label]]$events_seq)[1L])
  nev <- length(.sr_events_load(q$paths$events)$events)
  if (is.na(fseq) || nev != fseq + 1L) {
    fail("the project history continues after the freeze point or lacks the freeze event")
  }
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
