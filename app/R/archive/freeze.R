# archive/freeze.R - the freeze: an immutable snapshot of a finished analysis, recorded as one event.
#
#   sr_freeze(project, label)             gate -> out/freezes/<label>/ (manifest.json, analysis.R, renv.lock)
#                                         -> a freeze.created event
#   sr_freezes(project)                   data.frame(label, created, manifest_sha, libsnap_hash, events_seq)
#   sr_freeze_manifest(project, label)    the parsed manifest.json
#   sr_freeze_verify(project, label, data = TRUE)   re-hash every file the manifest lists
#
# The manifest lists, with SHA-256, the store manifests and parts, the in-project sources, each ok run's
# extract and fit, every finished export and the two code files. It also pins the events.jsonl prefix (the
# lines before the freeze event) and the audit tip, so a later edit of any of them is detected. Events
# appended after the freeze do not change its verification. The folder appears whole or not at all
# (partial folder + rename), and the event is appended last.

SR_FREEZE_LABEL_RE <- "^[a-z][a-z0-9-]{0,39}$"
SR_FREEZE_KINDS <- c("store_manifest", "part", "source", "extract", "fit", "export", "code")
.SR_FREEZE_DATA_KINDS <- c("part", "source")

# ---- paths and small helpers ------------------------------------------------------------------------

.sr_freeze_dir <- function(project, label) file.path(project$paths$out, "freezes", label)

.sr_freeze_chr1 <- function(x) if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) x else NA_character_

#' SHA-256 of the bytes of the first `seq` LF-terminated lines of events.jsonl.
.sr_events_prefix_sha <- function(events_path, seq) {
  b <- .sr_read_bytes(events_path)
  lf <- which(b == as.raw(0x0A))
  seq <- as.integer(seq)
  if (length(seq) != 1L || is.na(seq) || seq < 0L || seq > length(lf)) {
    stop("events.jsonl has fewer than ", paste(format(seq), collapse = ","), " lines", call. = FALSE)
  }
  if (seq == 0L) return(.sr_sha256(raw(0)))
  .sr_sha256(b[seq_len(lf[seq])])
}

# ---- labels -----------------------------------------------------------------------------------------

#' The next freeze label: "submission-v1" when there is none, else "revision-r<k>".
sr_freeze_next_label <- function(project) {
  have <- names(project$state$freezes %||% list())
  if (!length(have)) return("submission-v1")
  rev <- have[grepl("^revision-r[0-9]+$", have)]
  n <- suppressWarnings(as.integer(sub("^revision-r", "", rev)))
  paste0("revision-r", 1L + if (length(n)) max(n) else 0L)
}

# ---- the reducer ------------------------------------------------------------------------------------

sr_reducer_register("freeze.created", function(state, event) {
  p <- event$payload
  l <- p$label
  if (!is.character(l) || length(l) != 1L || is.na(l) || !grepl(SR_FREEZE_LABEL_RE, l)) {
    stop("freeze.created needs a valid label", call. = FALSE)
  }
  fz <- state$freezes %||% list()
  fz[[l]] <- c(p, list(ts = event$ts))
  state$freezes <- fz
  state$lib_baseline <- list(hash = p$libsnap_hash, snap = p$libsnap, from = "freeze", label = l)
  state
})

#' The freezes of a project, in event order.
#' @return data.frame(label, created, manifest_sha, libsnap_hash, events_seq); zero rows when there is none.
sr_freezes <- function(project) {
  fz <- project$state$freezes %||% list()
  chr <- function(f) vapply(fz, function(e) .sr_freeze_chr1(e[[f]]), "", USE.NAMES = FALSE)
  data.frame(label = as.character(names(fz)), created = chr("ts"), manifest_sha = chr("manifest_sha"),
             libsnap_hash = chr("libsnap_hash"),
             events_seq = vapply(fz, function(e) as.integer(e$events_seq %||% NA_integer_)[1L], 0L, USE.NAMES = FALSE),
             stringsAsFactors = FALSE)
}

#' The parsed out/freezes/<label>/manifest.json; an unknown label is an error.
sr_freeze_manifest <- function(project, label) {
  if (!is.character(label) || length(label) != 1L || is.na(label) || !(label %in% names(project$state$freezes))) {
    stop("unknown freeze label '", paste(format(label), collapse = ","), "'", call. = FALSE)
  }
  f <- file.path(.sr_freeze_dir(project, label), "manifest.json")
  if (!file.exists(f)) stop("the manifest of freeze '", label, "' is missing", call. = FALSE)
  .sr_read_json_file(f)
}

# ---- the gate ---------------------------------------------------------------------------------------

.sr_freeze_gate <- function(project, label) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  if (!is.character(label) || length(label) != 1L || is.na(label) || !grepl(SR_FREEZE_LABEL_RE, label)) {
    stop("invalid freeze label (must match ", SR_FREEZE_LABEL_RE, "): ", paste(format(label), collapse = ","),
         call. = FALSE)
  }
  if (label %in% names(project$state$freezes) || file.exists(.sr_freeze_dir(project, label))) {
    stop("freeze label '", label, "' is already used", call. = FALSE)
  }
  if (sr_blind(project)) stop("the SAP is not locked; lock it before freezing", call. = FALSE)
  need <- sr_tables_needing_hash(project)
  if (length(need)) {
    stop("the full SHA-256 of the source of table(s) ", paste0("'", need, "'", collapse = ", "),
         " is missing; run sr_source_hash() first", call. = FALSE)
  }
  runs <- project$state$runs %||% list()
  an <- .sr_sap_analyses(project)
  prim <- names(an)[vapply(an, function(a) identical(a$label, "primary"), NA)]
  for (id in prim) {
    if (is.na(.sr_freeze_chr1(runs[[id]]$ok_key))) {
      stop("primary analysis '", id, "' has no successful run", call. = FALSE)
    }
  }
  for (id in names(runs)) {
    key <- .sr_freeze_chr1(runs[[id]]$ok_key)
    if (is.na(key)) next
    if (!file.exists(file.path(project$paths$fits, paste0(key, ".rds")))) {
      stop("the stored result of '", id, "' is missing; run it again", call. = FALSE)
    }
    sha <- .sr_freeze_chr1(runs[[id]]$ok_extract_sha)
    if (is.na(sha) || !file.exists(file.path(project$paths$dir, "data", "extracts", paste0(sha, ".rds")))) {
      stop("the extract of '", id, "' is missing; run it again", call. = FALSE)
    }
  }
  v <- sr_project_verify(project)
  if (!isTRUE(v$ok) || !is.null(v$unaudited)) {
    stop("the project does not verify: ",
         paste(c(v$problems, if (!is.null(v$unaudited)) "an event has no audit entry"), collapse = "; "),
         call. = FALSE)
  }
  invisible(TRUE)
}

# ---- the files list ---------------------------------------------------------------------------------

# The manifest's file list: store manifests, parts, in-project sources, extracts and fits, exports, code.
# `code` maps the manifest path of a code file to where it is on disk now.
.sr_freeze_files <- function(project, ids, code) {
  dir <- project$paths$dir
  out <- list()
  seen <- character()
  add <- function(rel, kind, sha = NULL, abs = file.path(dir, rel)) {
    if (rel %in% seen) return(invisible())
    seen <<- c(seen, rel)
    out[[length(out) + 1L]] <<- list(path = rel, sha256 = sha %||% sr_sha256_file(abs),
                                     bytes = as.numeric(file.size(abs)), kind = kind)
  }
  tabs <- names(project$state$tables %||% list())
  man <- list()
  for (t in tabs) {
    if (file.exists(.sr_store_manifest_path(project, t))) man[[t]] <- sr_store_manifest(project, t)
  }
  for (t in names(man)) add(paste("data", "store", t, "manifest.json", sep = "/"), "store_manifest")
  for (t in names(man)) {
    for (p in man[[t]]$parts) add(paste("data", "store", t, p$file, sep = "/"), "part", sha = p$sha256)
  }
  for (t in tabs) {
    e <- project$state$tables[[t]]
    if (.sr_is_derived(e) || isTRUE(e$source$external)) next
    rel <- .sr_freeze_chr1(e$source$path)
    if (!is.na(rel)) add(gsub("\\\\", "/", rel), "source", sha = .sr_freeze_chr1(e$source$sha256))
  }
  for (id in ids) {
    r <- project$state$runs[[id]]
    add(paste("data", "extracts", paste0(r$ok_extract_sha, ".rds"), sep = "/"), "extract")
    add(paste("fits", paste0(r$ok_key, ".rds"), sep = "/"), "fit")
  }
  root <- file.path(project$paths$out, "exports")
  for (d in sort(list.files(root, pattern = "^export-[0-9]+$"), method = "radix")) {
    if (!dir.exists(file.path(root, d))) next
    for (f in .sr_ex_files(file.path(root, d))) add(paste("out", "exports", d, f, sep = "/"), "export")
  }
  for (f in names(code)) add(f, "code", abs = code[[f]])
  out
}

# ---- the freeze -------------------------------------------------------------------------------------

#' Freeze the project: gate, write out/freezes/<label>/ (manifest.json, analysis.R, renv.lock) and append
#' `freeze.created`. Refused (nothing written) for a read-only project, a bad or used label, an unlocked
#' SAP, an unhashed source, a pre-specified analysis without an ok run, a missing stored result, or a
#' project that does not verify.
#' @param project an open, writable project.
#' @param label matching SR_FREEZE_LABEL_RE; the default is sr_freeze_next_label().
#' @return the project with `freeze.created` applied.
sr_freeze <- function(project, label = sr_freeze_next_label(project)) {
  .sr_freeze_gate(project, label)
  paths <- project$paths
  runs <- project$state$runs %||% list()
  ids <- Filter(function(id) !is.na(.sr_freeze_chr1(runs[[id]]$ok_key)), names(runs))

  root <- file.path(paths$out, "freezes")
  st <- file.path(root, paste0(".", label, ".partial"))
  dir <- .sr_freeze_dir(project, label)
  dir.create(root, recursive = TRUE, showWarnings = FALSE)
  unlink(st, recursive = TRUE)
  dir.create(st, recursive = TRUE)
  done <- FALSE
  on.exit(if (!done) unlink(st, recursive = TRUE), add = TRUE)

  snap <- sr_libsnap()
  .sr_ex_write_text(file.path(st, "analysis.R"), sr_analysis_script(project, ids))
  .sr_ex_write_text(file.path(st, "renv.lock"),
                    paste0(.sr_json(sr_lockfile(snap, sr_lockfile_matches_runs(project, ids, snap))), "\n"))
  rel <- function(f) paste("out", "freezes", label, f, sep = "/")
  code <- stats::setNames(list(file.path(st, "analysis.R"), file.path(st, "renv.lock")),
                          c(rel("analysis.R"), rel("renv.lock")))
  files <- .sr_freeze_files(project, ids, code)

  tables <- list()
  for (t in names(project$state$tables %||% list())) {
    e <- project$state$tables[[t]]
    m <- if (file.exists(.sr_store_manifest_path(project, t))) sr_store_manifest(project, t) else NULL
    re <- project$state$reimports[[t]]
    derived <- .sr_is_derived(e)
    external <- isTRUE(e$source$external)
    tables[[t]] <- list(
      store_hash = if (is.null(m)) NA_character_ else sr_store_hash(m),
      rows = if (is.null(m)) 0 else sum(vapply(m$parts, function(p) p$rows, 0)),
      base = as.character(re$table %||% t), version = as.integer(re$version %||% 0L), derived = derived,
      source = list(sha256 = if (derived) NA_character_ else .sr_freeze_chr1(e$source$sha256),
                    external = external,
                    path = if (derived || external) NA_character_ else gsub("\\\\", "/", .sr_freeze_chr1(e$source$path))))
  }
  runl <- list()
  for (id in ids) {
    r <- runs[[id]]
    runl[[id]] <- list(label = id, ok_key = r$ok_key, ok_extract_sha = r$ok_extract_sha,
                       ok_lib_hash = .sr_freeze_chr1(r$ok_lib_hash))
  }

  seq <- length(.sr_events_load(paths$events)$events)
  tip <- sr_audit_tip(paths$audit)
  manifest <- list(
    format = "statrouter-freeze", format_version = 1L, label = label, created = .sr_now(),
    project_id = project$meta$id, schema_version = project$meta$schema_version,
    events = list(seq = as.integer(seq), sha256 = .sr_events_prefix_sha(paths$events, seq)),
    audit_tip = tip, sap_hash = project$state$sap$lock$sap_hash,
    libsnap = snap, libsnap_hash = sr_libsnap_hash(snap),
    lock_libsnap_hash = .sr_freeze_chr1(project$state$sap$lock$libsnap_hash),
    tables = tables, runs = runl, files = unname(files))
  .sr_write_json_file(file.path(st, "manifest.json"), manifest)
  msha <- sr_sha256_file(file.path(st, "manifest.json"))
  if (!isTRUE(file.rename(st, dir))) {
    stop("could not move the finished freeze into ", file.path("out", "freezes", label), call. = FALSE)
  }
  done <- TRUE

  payload <- list(label = label, manifest_sha = msha, audit_tip = tip, events_seq = as.integer(seq),
                  libsnap_hash = manifest$libsnap_hash, libsnap = .sr_libsnap_json(snap), n_files = length(files))
  tryCatch(sr_project_append(project, "freeze.created", payload), error = function(e) {
    # the folder goes unless the event did get written (a crash between the event and its audit entry)
    evs <- tryCatch(sr_events_read(paths$events), error = function(e2) list())
    last <- if (length(evs)) evs[[length(evs)]] else NULL
    if (!(identical(last$type, "freeze.created") && identical(last$payload$label, label))) {
      unlink(dir, recursive = TRUE)
    }
    stop(e)
  })
}

# ---- verification -----------------------------------------------------------------------------------

#' Check a freeze against the project folder.
#'
#' Problems, in this order: an unknown freeze; a manifest whose SHA-256 differs from the event's; a
#' different events.jsonl prefix; an audit tip that is not the entry before the freeze event's audit
#' entry; a present file whose SHA-256 differs; a missing file of a non-data kind. A missing store part
#' or source (the data kinds) is listed in `absent` and is not a problem: the data may live elsewhere.
#' With `data = FALSE` the data kinds are skipped, neither hashed nor reported.
#' @return list(ok, problems, checked = files hashed, absent).
sr_freeze_verify <- function(project, label, data = TRUE) {
  if (!is.character(label) || length(label) != 1L || is.na(label)) {
    stop("unknown freeze label '", paste(format(label), collapse = ","), "'", call. = FALSE)
  }
  problems <- character()
  absent <- character()
  checked <- 0L
  res <- function() list(ok = !length(problems), problems = problems, checked = checked, absent = absent)
  fz <- project$state$freezes[[label]]
  if (is.null(fz)) {
    problems <- paste0("freeze '", label, "' is not in the project")
    return(res())
  }
  mf <- file.path(.sr_freeze_dir(project, label), "manifest.json")
  if (!file.exists(mf)) {
    problems <- paste0("the manifest of freeze '", label, "' is missing")
    return(res())
  }
  if (!identical(sr_sha256_file(mf), fz$manifest_sha)) {
    problems <- c(problems, "manifest.json differs from the SHA-256 recorded by the freeze event")
  }
  m <- tryCatch(.sr_read_json_file(mf), error = function(e) NULL)
  if (!is.list(m)) {
    problems <- c(problems, "manifest.json cannot be read")
    return(res())
  }
  seq <- suppressWarnings(as.integer(m$events$seq))
  pre <- tryCatch(.sr_events_prefix_sha(project$paths$events, seq), error = function(e) NA_character_)
  if (!identical(pre, m$events$sha256)) {
    problems <- c(problems, "events.jsonl no longer starts with the lines the freeze recorded")
  }
  entries <- tryCatch(lapply(.sr_audit_lines(project$paths$audit), .sr_audit_parse), error = function(e) list())
  fseq <- as.integer(fz$events_seq) + 1L
  at <- which(vapply(entries, function(e) {
    !is.null(e) && identical(e$action, "event:freeze.created") && identical(as.integer(e$detail$seq)[1L], fseq)
  }, NA))
  before <- NA_character_
  if (length(at) == 1L) before <- if (at == 1L) .sr_audit_genesis else entries[[at - 1L]]$hash
  if (!identical(before, m$audit_tip)) {
    problems <- c(problems, "the audit tip of the freeze does not match the audit log")
  }
  for (f in m$files) {
    abs <- file.path(project$paths$dir, f$path)
    isdata <- f$kind %in% .SR_FREEZE_DATA_KINDS
    if (isdata && !isTRUE(data)) next
    if (!file.exists(abs)) {
      if (isdata) absent <- c(absent, f$path) else problems <- c(problems, paste0(f$path, ": file is missing"))
      next
    }
    checked <- checked + 1L
    if (!identical(sr_sha256_file(abs), f$sha256)) {
      problems <- c(problems, paste0(f$path, ": SHA-256 differs from the manifest"))
    }
  }
  res()
}
