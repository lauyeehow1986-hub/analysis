# archive/srproj_export.R - export a freeze as a .srproj (a gzip tar) that opens on another machine.
#
#   sr_srproj_export(project, path, label = NULL, data = c("auto", "full", "lean"))
#
# The archive holds the project as it stood at the freeze: project.json, events.jsonl and audit.log cut
# at the freeze.created entry, the files of this and earlier freezes, and the files the freeze manifest
# lists (never the live project's later work). A lean archive leaves out store parts and in-project
# sources; everything else stays, so a result can be rebuilt after import. srproj.json is the index.
# Nothing is left behind on failure: the staging folder and the .partial archive go on every exit, and
# the srproj.exported event is appended last.

SR_SRPROJ_FORMAT <- "statrouter-srproj"
SR_SRPROJ_VERSION <- 1L
SR_SRPROJ_MAX_MEMBER <- 8589934592

sr_reducer_register("srproj.exported", function(state, event) {
  ex <- state$srproj_exports %||% list()
  ex[[length(ex) + 1L]] <- event$payload
  state$srproj_exports <- ex
  state
})

# ---- the cut ----------------------------------------------------------------------------------------

#' The bytes of events.jsonl and audit.log up to and including the freeze.created entry of `label`.
.sr_srproj_cut <- function(project, label) {
  fz <- project$state$freezes[[label]]
  fseq <- as.integer(fz$events_seq) + 1L
  eb <- .sr_read_bytes(project$paths$events)
  elf <- which(eb == as.raw(0x0A))
  if (length(fseq) != 1L || is.na(fseq) || fseq < 1L || fseq > length(elf)) {
    stop("events.jsonl has no freeze.created line for '", label, "'", call. = FALSE)
  }
  ev <- .sr_events_load(project$paths$events)$events[[fseq]]
  if (!identical(ev$type, "freeze.created") || !identical(ev$payload$label, label)) {
    stop("events.jsonl line ", fseq, " is not the freeze.created event of '", label, "'", call. = FALSE)
  }
  ab <- .sr_read_bytes(project$paths$audit)
  alf <- which(ab == as.raw(0x0A))
  entries <- lapply(.sr_audit_lines(project$paths$audit), .sr_audit_parse)
  at <- which(vapply(entries, function(e) {
    !is.null(e) && identical(e$action, "event:freeze.created") && identical(as.integer(e$detail$seq)[1L], fseq)
  }, NA))
  if (length(at) != 1L || at > length(alf)) {
    stop("audit.log has no single entry for the freeze.created event of '", label, "'", call. = FALSE)
  }
  list(events = eb[seq_len(elf[fseq])], audit = ab[seq_len(alf[at])])
}

# ---- data mode --------------------------------------------------------------------------------------

#' "full" or "lean". Under "auto": full when the data files total at most sr_opt("freeze_full_max_bytes",
#' 2 GiB) and none of them is absent from the project folder.
.sr_srproj_data_mode <- function(project, label, data = "auto") {
  if (!identical(data, "auto")) return(data)
  lim <- suppressWarnings(as.numeric(sr_opt("freeze_full_max_bytes", 2147483648)))
  if (length(lim) != 1L || !is.finite(lim) || lim < 0) {
    stop("sr.freeze_full_max_bytes must be a finite number of bytes, zero or more", call. = FALSE)
  }
  m <- sr_freeze_manifest(project, label)
  dat <- Filter(function(f) f$kind %in% .SR_FREEZE_DATA_KINDS, m$files)
  total <- sum(vapply(dat, function(f) as.numeric(f$bytes), 0))
  gone <- any(!file.exists(file.path(project$paths$dir, vapply(dat, `[[`, "", "path"))))
  if (total <= lim && !gone) "full" else "lean"
}

# ---- members ----------------------------------------------------------------------------------------

#' The archive members: data.frame(path, src, kind). `src` is NA for the generated files.
.sr_srproj_members <- function(project, label, data_mode) {
  dir <- project$paths$dir
  m <- sr_freeze_manifest(project, label)
  fzs <- sr_freezes(project)
  mine <- fzs$events_seq[fzs$label == label]
  paths <- character()
  src <- character()
  kind <- character()
  add <- function(p, s, k) {
    if (p %in% paths) return(invisible())
    paths <<- c(paths, p)
    src <<- c(src, s)
    kind <<- c(kind, k)
  }
  add("project.json", file.path(dir, "project.json"), "project")
  add("events.jsonl", NA_character_, "events")
  add("audit.log", NA_character_, "audit")
  for (l in fzs$label[!is.na(fzs$events_seq) & fzs$events_seq <= mine]) {
    fd <- .sr_freeze_dir(project, l)
    if (!dir.exists(fd)) next
    for (f in sort(list.files(fd, recursive = TRUE, all.files = TRUE, no.. = TRUE), method = "radix")) {
      if (identical(f, "srproj.json")) next
      add(paste("out", "freezes", l, f, sep = "/"), file.path(fd, f), "freeze")
    }
  }
  for (f in m$files) {
    if (identical(data_mode, "lean") && f$kind %in% .SR_FREEZE_DATA_KINDS) next
    add(f$path, file.path(dir, f$path), f$kind)
  }
  add("srproj.json", NA_character_, "index")
  data.frame(path = paths, src = src, kind = kind, stringsAsFactors = FALSE)
}

# What a lean or external archive leaves out, from the manifest.
.sr_srproj_omitted <- function(m, data_mode) {
  parts <- list()
  sources <- list()
  if (identical(data_mode, "lean")) {
    for (f in m$files) {
      if (identical(f$kind, "part")) {
        t <- strsplit(f$path, "/", fixed = TRUE)[[1L]][3L]
        if (!(t %in% unlist(parts))) parts[[length(parts) + 1L]] <- t
      }
    }
  }
  for (t in names(m$tables)) {
    s <- m$tables[[t]]$source
    if (isTRUE(s$external)) {
      sources[[length(sources) + 1L]] <- list(table = t, reason = "external")
    } else if (identical(data_mode, "lean") && is.character(s$path) && length(s$path) == 1L && !is.na(s$path) &&
               any(vapply(m$files, function(f) identical(f$kind, "source") && identical(f$path, s$path), NA))) {
      sources[[length(sources) + 1L]] <- list(table = t, reason = "lean")
    }
  }
  list(parts = parts, sources = sources)
}

# ---- the tar ----------------------------------------------------------------------------------------

# Copy a tar (gzip or plain) to `to`, blanking every owner and group name so the local account name never
# travels in the archive; header checksums are rewritten. Member data is copied in chunks of 1 MiB.
.sr_srproj_scrub_owner <- function(from, to) {
  inp <- gzfile(from, "rb")
  on.exit(close(inp), add = TRUE)
  outp <- gzfile(to, "wb")
  on.exit(close(outp), add = TRUE)
  repeat {
    h <- readBin(inp, "raw", 512L)
    if (length(h) == 0L) break
    if (length(h) < 512L) stop("truncated tar archive", call. = FALSE)
    if (!all(h == as.raw(0L))) {
      h[266:329] <- as.raw(0L)
      h[149:156] <- as.raw(32L)
      ck <- sprintf("%06o", sum(as.integer(h)))
      h[149:156] <- c(charToRaw(ck), as.raw(0L), as.raw(32L))
    }
    writeBin(h, outp)
    if (all(h == as.raw(0L))) next
    left <- ceiling(.sr_tar_size(h[125:136]) / 512) * 512
    while (left > 0) {
      k <- min(left, 1048576)
      b <- readBin(inp, "raw", k)
      if (length(b) < k) stop("truncated tar archive", call. = FALSE)
      writeBin(b, outp)
      left <- left - k
    }
  }
  invisible(to)
}

#' utils::tar run from `staging`, so the member names are relative, then owner names blanked.
#' A function of its own for mocking.
.sr_srproj_tar <- function(staging, files, tarfile) {
  raw_tar <- paste0(tarfile, ".raw")
  on.exit(unlink(raw_tar), add = TRUE)
  old <- setwd(staging)
  tryCatch(utils::tar(raw_tar, files = files, compression = "gzip", tar = "internal"),
           finally = setwd(old))
  .sr_srproj_scrub_owner(raw_tar, tarfile)
  invisible(tarfile)
}

# ---- the export -------------------------------------------------------------------------------------

#' Export a freeze as `path` (must end in .srproj, must not exist). `label = NULL` is the latest freeze.
#' @param data "full" keeps store parts and in-project sources, "lean" drops them, "auto" is full when
#'   they total at most the sr.freeze_full_max_bytes option (2 GiB by default).
#' @return the project with `srproj.exported` applied.
sr_srproj_export <- function(project, path, label = NULL, data = c("auto", "full", "lean")) {
  data <- match.arg(data)
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  fzs <- sr_freezes(project)
  if (!nrow(fzs)) stop("no freeze to export; call sr_freeze() first", call. = FALSE)
  if (is.null(label)) label <- fzs$label[nrow(fzs)]
  if (!is.character(label) || length(label) != 1L || is.na(label) || !(label %in% fzs$label)) {
    stop("unknown freeze '", paste(format(label), collapse = ","), "'", call. = FALSE)
  }
  if (!is.character(path) || length(path) != 1L || is.na(path) || !grepl("[.]srproj$", path, ignore.case = TRUE)) {
    stop("the archive name must end in .srproj", call. = FALSE)
  }
  parent <- dirname(path)
  if (!dir.exists(parent)) stop("the folder ", parent, " does not exist", call. = FALSE)
  target <- file.path(normalizePath(parent, winslash = "/", mustWork = TRUE), basename(path))
  part <- paste0(target, ".partial")
  if (file.exists(target) || file.exists(part)) {
    stop(basename(target), " already exists; it is never overwritten", call. = FALSE)
  }
  mode <- .sr_srproj_data_mode(project, label, data)
  v <- sr_freeze_verify(project, label, data = identical(mode, "full"))
  if (!isTRUE(v$ok)) {
    stop("freeze '", label, "' no longer verifies: ", v$problems[1L], call. = FALSE)
  }
  if (identical(mode, "full") && length(v$absent)) {
    stop("data file ", v$absent[1L], " is missing from the project; use data = \"lean\"", call. = FALSE)
  }
  mem <- .sr_srproj_members(project, label, mode)
  bad <- mem$path[!.sr_archive_safe_path(mem$path)]
  if (length(bad)) stop("unsafe archive member name '", bad[1L], "'", call. = FALSE)
  for (i in which(!is.na(mem$src))) {
    if (!file.exists(mem$src[i])) stop("the file ", mem$path[i], " is missing from the project", call. = FALSE)
    if (file.size(mem$src[i]) >= SR_SRPROJ_MAX_MEMBER) {
      stop("the file ", mem$path[i], " is larger than 8 GiB; use data = \"lean\"", call. = FALSE)
    }
  }
  cut <- .sr_srproj_cut(project, label)
  man <- sr_freeze_manifest(project, label)

  staging <- file.path(project$paths$out, paste0(".srproj-", label, ".partial"))
  unlink(staging, recursive = TRUE)
  on.exit({
    unlink(staging, recursive = TRUE)
    unlink(part)
  }, add = TRUE)
  dir.create(staging, recursive = TRUE)

  for (i in seq_len(nrow(mem))) {
    p <- mem$path[i]
    if (identical(p, "srproj.json")) next
    dst <- file.path(staging, p)
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    if (identical(p, "events.jsonl")) {
      writeBin(cut$events, dst)
    } else if (identical(p, "audit.log")) {
      writeBin(cut$audit, dst)
    } else if (!isTRUE(suppressWarnings(file.link(mem$src[i], dst)))) {
      if (!isTRUE(file.copy(mem$src[i], dst, copy.date = TRUE))) {
        stop("could not stage ", p, call. = FALSE)
      }
    }
  }
  body <- mem[mem$path != "srproj.json", , drop = FALSE]
  files <- lapply(seq_len(nrow(body)), function(i) {
    f <- file.path(staging, body$path[i])
    list(path = body$path[i], sha256 = sr_sha256_file(f), bytes = as.numeric(file.size(f)))
  })
  index <- list(format = SR_SRPROJ_FORMAT, format_version = SR_SRPROJ_VERSION, label = label,
                created = .sr_now(), freeze_manifest_sha = project$state$freezes[[label]]$manifest_sha,
                data_mode = mode, project_id = project$meta$id, files = files,
                omitted = .sr_srproj_omitted(man, mode))
  .sr_write_json_file(file.path(staging, "srproj.json"), index)

  .sr_srproj_tar(staging, mem$path, part)
  ent <- .sr_tar_entries(part)
  .sr_tar_check(ent)
  if (!identical(sort(ent$name[ent$type == "0"], method = "radix"), sort(mem$path, method = "radix"))) {
    stop("the written archive does not match its index", call. = FALSE)
  }
  sha <- sr_sha256_file(part)
  bytes <- as.numeric(file.size(part))
  if (!isTRUE(file.rename(part, target))) stop("could not move the finished archive into place", call. = FALSE)

  om <- index$omitted
  payload <- list(label = label, data_mode = mode, file = basename(target), archive_sha256 = sha,
                  bytes = bytes, n_files = nrow(mem), parts_omitted = om$parts, sources_omitted = om$sources)
  tryCatch(sr_project_append(project, "srproj.exported", payload), error = function(e) {
    evs <- tryCatch(sr_events_read(project$paths$events), error = function(e2) list())
    last <- if (length(evs)) evs[[length(evs)]] else NULL
    if (!(identical(last$type, "srproj.exported") && identical(last$payload$archive_sha256, sha))) {
      unlink(target)
    }
    stop(e)
  })
}
