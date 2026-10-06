# io/ingest.R - the ingest dispatcher: pre-flight, source copy, checkpoint and resume, the
# `data.imported` event.
#
# sr_ingest() streams a data file into the project's Parquet store through a format reader
# (registered with .sr_reader_register(); read_text.R registers csv, tsv and txt). Everything that is
# not format-specific lives here:
#   * the pre-flight (nothing is created under data/ if it refuses);
#   * the source copy into data/source/<table>.<ext> (SHA-256 computed on the way) or, in external
#     mode, a reference to the file in place;
#   * the checkpoint, data/ingest/<table>/checkpoint.json, rewritten atomically after every committed
#     part, and the resume rules (the source fingerprint and the parse settings must match and the
#     committed parts must still hash to what the checkpoint recorded);
#   * the manifest, the store hash and the `data.imported` event.
#
# Reader contract:
#   function(project, sniff, table, ck, options, progress, cancel) -> list(schema, resumable, type_widened)
# `sniff$path` is the file to read (the copy in non-external mode). `ck` is the checkpoint object (an
# environment). A reader writes each part to .sr_part_path(ck) and registers it with
# .sr_part_commit(ck, part_file, rows), which renames it into place, records its rows and SHA-256,
# rewrites the checkpoint and returns the next part index. A reader that is cancelled calls
# stop(.sr_ingest_cancelled()); the checkpoint stays on disk (done = FALSE) so a later call resumes.

.sr_io_block <- 8388608     # 8 MiB per read or write when streaming

# ---- readers --------------------------------------------------------------------------------------

.sr_readers <- new.env(parent = emptyenv())

#' Register (or replace) the reader for a sniffed format.
.sr_reader_register <- function(format, fn) {
  if (!is.character(format) || length(format) != 1L || is.na(format) || !nzchar(format)) {
    stop("format must be a single non-empty string", call. = FALSE)
  }
  if (!is.function(fn)) stop("fn must be a reader function", call. = FALSE)
  assign(format, fn, envir = .sr_readers)
  invisible(format)
}

.sr_reader_get <- function(format) {
  fmt <- if (is.character(format) && length(format) == 1L) format else NA_character_
  if (is.na(fmt) || !exists(fmt, envir = .sr_readers, inherits = FALSE)) {
    stop("no reader registered for format ", if (is.na(fmt)) "NA" else fmt, call. = FALSE)
  }
  get(fmt, envir = .sr_readers, inherits = FALSE)
}

# ---- table names ----------------------------------------------------------------------------------

#' A table name derived from a file name: lower case, every character outside a-z0-9 becomes "_",
#' "t_" in front when it would not start with a letter, and "_2", "_3", ... when it is already in
#' `existing`. The result always matches ^[a-z][a-z0-9_]{0,62}$.
sr_table_name <- function(path, existing = character()) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("path must be a single non-empty string", call. = FALSE)
  }
  b <- basename(gsub(intToUtf8(92L), "/", path, fixed = TRUE))
  if (.sr_ext(b) %in% .sr_sniff_compressions) b <- tools::file_path_sans_ext(b)
  base <- tools::file_path_sans_ext(b)
  x <- gsub("[^a-z0-9]", "_", tolower(base))
  if (!nzchar(x) || !grepl("^[a-z]", x)) x <- paste0("t_", x)
  x <- substr(x, 1L, 55L)
  cand <- x
  i <- 2L
  while (cand %in% existing) {
    cand <- paste0(x, "_", i)
    i <- i + 1L
  }
  cand
}

# ---- the checkpoint -------------------------------------------------------------------------------

.sr_ingest_cancelled <- function() {
  structure(class = c("sr_ingest_cancelled", "error", "condition"),
            list(message = "ingest cancelled; call sr_ingest again with the same arguments to resume",
                 call = NULL))
}

# The persisted fields of a checkpoint object (what checkpoint.json holds).
.sr_ck_list <- function(ck) {
  list(source_fingerprint = ck$source_fingerprint, sniff_hash = ck$sniff_hash,
       chunk_bytes = ck$chunk_bytes, columns = ck$columns, next_offset = ck$next_offset,
       parts = unname(ck$parts), done = isTRUE(ck$done),
       type_widened = as.list(as.character(ck$type_widened)), widen_log = unname(ck$widen_log),
       restarts = ck$restarts, prepared_bytes = ck$prepared_bytes, source_sha256 = ck$source_sha256)
}

# Rewrite checkpoint.json atomically.
.sr_ck_write <- function(ck) {
  .sr_write_json_file(ck$path, .sr_ck_list(ck))
  invisible(ck)
}

.sr_ck_new <- function(project, table, fingerprint, sniff_hash, chunk_bytes) {
  ck <- new.env(parent = emptyenv())
  data <- sr_project_paths(project$paths$dir)$data
  ck$table <- table
  ck$ingest_dir <- file.path(data, "ingest", table)
  ck$store_dir <- file.path(ck$ingest_dir, "store")     # staging: the parts are built here ...
  ck$final_dir <- sr_store_dir(project, table)          # ... and swapped into data/store/<table> when done
  ck$path <- file.path(ck$ingest_dir, "checkpoint.json")
  ck$source_fingerprint <- fingerprint
  ck$sniff_hash <- sniff_hash
  ck$chunk_bytes <- chunk_bytes
  ck$columns <- NULL
  ck$next_offset <- 0
  ck$parts <- list()
  ck$done <- FALSE
  ck$type_widened <- character()
  ck$widen_log <- list()
  ck$restarts <- 0L
  ck$prepared_bytes <- NULL
  ck$source_sha256 <- NA_character_
  ck
}

# Fill a checkpoint object from the parsed checkpoint.json `disk`.
.sr_ck_load <- function(ck, disk) {
  ck$chunk_bytes <- as.numeric(disk$chunk_bytes)
  ck$columns <- if (is.null(disk$columns)) NULL else lapply(disk$columns, function(cl) {
    list(name = cl$name, type = cl$type, fmt = if (is.null(cl$fmt)) NA_character_ else cl$fmt)
  })
  ck$next_offset <- as.numeric(disk$next_offset)
  ck$parts <- lapply(disk$parts, function(p) list(file = p$file, rows = as.numeric(p$rows), sha256 = p$sha256))
  ck$type_widened <- as.character(unlist(disk$type_widened))
  ck$widen_log <- disk$widen_log %||% list()
  ck$restarts <- as.integer(disk$restarts %||% 0L)
  ck$prepared_bytes <- if (is.null(disk$prepared_bytes)) NULL else as.numeric(disk$prepared_bytes)
  ck$source_sha256 <- if (is.null(disk$source_sha256)) NA_character_ else disk$source_sha256
  ck
}

# The path the next part is written to (renamed into place by .sr_part_commit).
.sr_part_path <- function(ck) {
  file.path(ck$store_dir, sprintf("part-%05d.parquet.tmp", length(ck$parts) + 1L))
}

.sr_rename_retry <- function(from, to, tries = 5L) {
  for (i in seq_len(tries)) {
    if (isTRUE(suppressWarnings(file.rename(from, to)))) return(invisible(TRUE))
    gc()                      # release a DuckDB result that still holds the file open
    Sys.sleep(0.2 * i)        # a virus scanner may briefly hold a freshly written file
  }
  stop("cannot move ", basename(from), " to ", basename(to), call. = FALSE)
}

# Swap the verified staged store into data/store/<table>. The staged manifest is written and read
# back, every part is re-hashed against it, then the old store (if any) is renamed to
# data/ingest/<table>/replaced, the staging directory renamed into place, and the old store deleted.
# Until the second rename the previous table is untouched; if it fails the old one is put back.
.sr_store_install <- function(ck, manifest) {
  m <- .sr_store_norm_manifest(manifest, ck$table)
  mpath <- file.path(ck$store_dir, "manifest.json")
  .sr_write_json_file(mpath, m)
  back <- .sr_store_norm_manifest(.sr_read_json_file(mpath), ck$table)
  if (!identical(sr_store_hash(back), sr_store_hash(m)) || !identical(back$rows, m$rows)) {
    stop("the staged manifest does not read back identically", call. = FALSE)
  }
  for (p in back$parts) {
    f <- file.path(ck$store_dir, p$file)
    if (!file.exists(f) || !identical(sr_sha256_file(f), p$sha256)) {
      stop("a staged part failed verification: ", p$file, call. = FALSE)
    }
  }
  old <- file.path(ck$ingest_dir, "replaced")
  if (dir.exists(old)) unlink(old, recursive = TRUE)
  parent <- dirname(ck$final_dir)
  if (!dir.exists(parent) && !isTRUE(dir.create(parent, recursive = TRUE, showWarnings = FALSE))) {
    stop("cannot create directory: ", parent, call. = FALSE)
  }
  had <- dir.exists(ck$final_dir)
  if (had) .sr_rename_retry(ck$final_dir, old)
  tryCatch(.sr_rename_retry(ck$store_dir, ck$final_dir), error = function(e) {
    if (had) file.rename(old, ck$final_dir)
    stop(e)
  })
  if (had) unlink(old, recursive = TRUE)
  invisible(m)
}

# A crash between the two renames of .sr_store_install leaves the previous table in
# data/ingest/<table>/replaced and nothing in data/store/<table>: put it back.
.sr_store_recover <- function(ck) {
  old <- file.path(ck$ingest_dir, "replaced")
  if (!dir.exists(ck$final_dir) && file.exists(file.path(old, "manifest.json"))) {
    parent <- dirname(ck$final_dir)
    if (!dir.exists(parent)) dir.create(parent, recursive = TRUE, showWarnings = FALSE)
    file.rename(old, ck$final_dir)
  }
  invisible(ck)
}

#' Commit a written part: rename `part_file` (from .sr_part_path) to part-NNNNN.parquet, record its
#' rows and SHA-256 and rewrite checkpoint.json atomically. Returns the next part index. A reader
#' sets ck$next_offset to the offset AFTER the part before committing, so the checkpoint on disk is
#' always consistent with the parts it lists.
.sr_part_commit <- function(ck, part_file, rows) {
  idx <- length(ck$parts) + 1L
  final <- file.path(ck$store_dir, sprintf("part-%05d.parquet", idx))
  if (!file.exists(part_file)) stop("part file was not written: ", basename(part_file), call. = FALSE)
  if (file.exists(final)) unlink(final)
  .sr_rename_retry(part_file, final)
  ck$parts[[idx]] <- list(file = basename(final), rows = as.numeric(rows), sha256 = sr_sha256_file(final))
  .sr_ck_write(ck)
  idx + 1L
}

# Delete the committed parts (a type-conflict restart starts the table again from the top).
.sr_ck_drop_parts <- function(ck) {
  old <- list.files(ck$store_dir, pattern = "^part-.*[.]parquet([.]tmp)?$", full.names = TRUE)
  if (length(old)) unlink(old)
  ck$parts <- list()
  ck$next_offset <- 0
  invisible(ck)
}

# ---- source handling ------------------------------------------------------------------------------

# Extension a copied source keeps: "csv", "csv.gz", "zip", ...
.sr_source_ext <- function(path) {
  ext <- .sr_ext(path)
  if (ext %in% .sr_sniff_compressions) {
    inner <- .sr_ext(tools::file_path_sans_ext(path))
    return(if (nzchar(inner)) paste0(inner, ".", ext) else ext)
  }
  if (nzchar(ext)) ext else "dat"
}

# Copy `from` to `to` (through to.tmp and a rename) in 8 MiB blocks, hashing as it goes. Returns the
# SHA-256 of the copy.
.sr_copy_hashed <- function(from, to) {
  tmp <- paste0(to, ".tmp")
  inc <- .sr_md_incremental()
  fed <- !is.null(inc)
  ok <- FALSE
  cin <- file(from, "rb")
  cout <- file(tmp, "wb")
  tryCatch({
    repeat {
      blk <- readBin(cin, "raw", n = .sr_io_block)
      if (!length(blk)) break
      writeBin(blk, cout)
      if (fed) fed <- isTRUE(tryCatch({ inc$feed(blk); TRUE }, error = function(e) FALSE))
    }
    flush(cout)
    ok <- TRUE
  }, finally = {
    close(cin)
    close(cout)
    if (!ok) unlink(tmp)
  })
  sha <- if (fed) tryCatch(inc$final(), error = function(e) NULL) else NULL
  if (file.exists(to)) unlink(to)
  .sr_rename_retry(tmp, to)
  if (is.null(sha)) sha <- sr_sha256_file(to)
  sha
}

# Hash of the sniff fields (and the parse options) that decide how the text is read. A checkpoint can
# be resumed only when this is unchanged. Deliberately not sr_content_hash(): that mixes in the
# library snapshot and the hardware, and a job must resume on another machine.
.sr_ingest_sniff_hash <- function(sniff, options) {
  get <- function(k) {
    v <- sniff[[k]]
    if (is.null(v) || length(v) != 1L) NA else v
  }
  fields <- c("format", "compression", "member", "encoding", "delim", "quote", "decimal", "header",
              "quoted_newlines")
  f <- stats::setNames(lapply(fields, get), fields)
  f$date_order <- as.list(sniff$date_order %||% character())
  # which table / object of a multi-table source is read (only when chosen, so a text-format hash is unchanged)
  if (!is.null(options$table)) f$table <- as.character(options$table)
  if (!is.null(options$object)) f$object <- as.character(options$object)
  f$parse_options <- list(sample_size = as.numeric(options$sample_size %||% 1e6),
                          nullstr = as.character(options$nullstr %||% c("", "NA")))
  .sr_sha256(sr_canonical_json(f))
}

# Is the checkpoint `disk` safe to resume? Returns NULL when yes, else the reason (a string).
.sr_ck_refusal <- function(disk, fp, sniff_hash, store_dir, ingest_dir) {
  if (!is.list(disk)) return("the checkpoint is unreadable")
  a <- disk$source_fingerprint
  if (!is.list(a) || !identical(as.numeric(a$size), as.numeric(fp$size)) || !identical(a$mtime, fp$mtime) ||
      !identical(a$sample_sha, fp$sample_sha)) {
    return("the source file changed since the checkpoint was written")
  }
  if (!identical(disk$sniff_hash, sniff_hash)) return("the parse settings changed since the checkpoint")
  # defence in depth: the checkpoint is a file on disk, and its types end up in SQL and its names in paths
  for (cl in disk$columns) {
    if (!is.list(cl) || !is.character(cl$name) || length(cl$name) != 1L || is.na(cl$name) ||
        !is.character(cl$type) || length(cl$type) != 1L || !(cl$type %in% .sr_csv_types) ||
        !(is.null(cl$fmt) || (is.character(cl$fmt) && length(cl$fmt) == 1L))) {
      return("the checkpoint names a column type that is not allowed")
    }
  }
  for (i in seq_along(disk$parts)) {
    p <- disk$parts[[i]]
    if (!is.list(p) || !identical(p$file, sprintf("part-%05d.parquet", i)) || !is.character(p$sha256)) {
      return("the checkpoint lists a part file name that is not expected")
    }
    f <- file.path(store_dir, p$file)
    if (!file.exists(f)) return(paste0("a committed part is missing: ", p$file))
    if (!identical(sr_sha256_file(f), p$sha256)) return(paste0("a committed part changed: ", p$file))
  }
  if (!is.null(disk$prepared_bytes)) {
    src <- file.path(ingest_dir, "source.csv")
    if (!file.exists(src) || !isTRUE(as.numeric(file.size(src)) == as.numeric(disk$prepared_bytes))) {
      return("the decompressed copy of the source is missing or incomplete")
    }
  }
  NULL
}

# Remove what a crash can leave behind next to a valid checkpoint: temp files, a half-written chunk
# and parts that were renamed into place but never recorded.
.sr_ck_tidy <- function(ck) {
  tmp <- list.files(c(ck$store_dir, ck$ingest_dir), pattern = "[.]tmp$", full.names = TRUE)
  chunk <- file.path(ck$ingest_dir, "chunk.csv")
  unlink(c(tmp, chunk[file.exists(chunk)]))
  parts <- list.files(ck$store_dir, pattern = "^part-.*[.]parquet$")
  keep <- vapply(ck$parts, function(p) p$file, character(1))
  orphan <- setdiff(parts, keep)
  if (length(orphan)) unlink(file.path(ck$store_dir, orphan))
  invisible(ck)
}

# Remove everything inside a directory (the directory stays).
.sr_clear_dir <- function(dir) {
  inside <- list.files(dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (length(inside)) unlink(inside, recursive = TRUE)
  invisible(dir)
}

# ---- sr_ingest ------------------------------------------------------------------------------------

#' Ingest a data file into the project's Parquet store and append `data.imported`.
#'
#' Order of events: (1) validate, sniff, find the reader; (2) pre-flight (a refusal stops here, before
#' anything is created under data/; a source that is decompressed or re-encoded first also needs room
#' for that uncompressed copy); (3) resume a valid checkpoint or start fresh; (4) copy the source to
#' data/ingest/<table> (non-external mode); (5) run the reader, which builds the parts in
#' data/ingest/<table>/store; (6) write and verify the manifest, swap the staged store into
#' data/store/<table> (and the source copy into data/source), `data.imported`. An existing table of
#' that name is replaced only at step 6: a re-ingest that fails or is cancelled leaves it, its source
#' copy and its state entry as they were.
#'  - `external = NULL` takes the pre-flight's `external_suggested`.
#'  - `options`: `drive_info` (replaces the drive probe of the pre-flight) and the reader's own
#'    (csv: `sample_size`, default 1e6, and `nullstr`, default c("", "NA")).
#'  - `progress(done_bytes, total_bytes)` and `cancel()` are called between chunks. When `cancel()`
#'    returns TRUE the ingest stops with an error of class `sr_ingest_cancelled`; its checkpoint
#'    stays, and calling sr_ingest again with the same arguments resumes.
#' Returns the project with the event applied.
sr_ingest <- function(project, path, table = sr_table_name(path), sniff = sr_sniff(path, member = member),
                      external = NULL, member = NULL, sheet = NULL, options = list(),
                      chunk_bytes = 256 * 1024^2, progress = NULL, cancel = function() FALSE) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  .sr_store_check_table(table)
  chk <- .sr_sniff_check_path(path)
  path <- chk$path
  if (!is.list(options)) stop("options must be a list", call. = FALSE)
  if (!is.numeric(chunk_bytes) || length(chunk_bytes) != 1L || !is.finite(chunk_bytes) || chunk_bytes < 1) {
    stop("chunk_bytes must be a positive number", call. = FALSE)
  }
  if (!is.null(progress) && !is.function(progress)) stop("progress must be a function or NULL", call. = FALSE)
  if (!is.function(cancel)) stop("cancel must be a function", call. = FALSE)
  if (!is.null(external) && (!is.logical(external) || length(external) != 1L || is.na(external))) {
    stop("external must be TRUE, FALSE or NULL", call. = FALSE)
  }
  if (is.na(sniff$format %||% NA_character_)) {
    stop("cannot ingest: the file's format could not be determined",
         if (length(sniff$warnings)) paste0(" (", sniff$warnings[1L], ")") else "", call. = FALSE)
  }
  reader <- .sr_reader_get(sniff$format)

  # pre-flight: nothing has been created yet
  drive_info <- options$drive_info %||% .sr_drive_info
  if (is.null(external)) {
    external <- isTRUE(sr_preflight(project, path, sniff, drive_info = drive_info,
                                    external = FALSE)$external_suggested)
  }
  pf <- sr_preflight(project, path, sniff, drive_info = drive_info, external = external)
  if (!isTRUE(pf$ok)) stop(pf$refuse_reason, call. = FALSE)
  # a source that is decompressed or re-encoded first is staged as a full uncompressed UTF-8 copy
  # on the data drive: that space is on top of what the pre-flight counted
  if (.sr_ingest_stages(sniff) && pf$free_bytes < pf$need_bytes + pf$uncompressed_bytes) {
    stop(sprintf(paste0("insufficient space: need %s GB (including %s GB to decompress or re-encode the ",
                        "source), have %s GB"),
                 .sr_pf_fmt_gb(pf$need_bytes + pf$uncompressed_bytes), .sr_pf_fmt_gb(pf$uncompressed_bytes),
                 .sr_pf_fmt_gb(pf$free_bytes)), call. = FALSE)
  }

  data <- sr_project_paths(project$paths$dir)$data
  fp <- sr_fingerprint(path)
  sniff_hash <- .sr_ingest_sniff_hash(sniff, options)
  ck <- .sr_ck_new(project, table, fp, sniff_hash, as.numeric(chunk_bytes))
  .sr_store_recover(ck)

  # resume a valid checkpoint, else start fresh. The table's existing store (data/store/<table>) is
  # not touched until the new one is complete: everything is built under data/ingest/<table>.
  resumed <- FALSE
  if (file.exists(ck$path)) {
    disk <- tryCatch(.sr_read_json_file(ck$path), error = function(e) NULL)
    if (is.list(disk) && isTRUE(disk$done)) {
      disk <- NULL                                  # finished earlier: ingesting again replaces it
    } else {
      why <- .sr_ck_refusal(disk, fp, sniff_hash, ck$store_dir, ck$ingest_dir)
      if (is.null(why)) {
        .sr_ck_load(ck, disk)
        resumed <- TRUE
      } else {
        warning("not resuming the earlier ingest of '", table, "': ", why, "; starting again", call. = FALSE)
      }
    }
  }
  if (!resumed) .sr_clear_dir(ck$ingest_dir)
  for (d in c(ck$ingest_dir, ck$store_dir)) {
    if (!dir.exists(d) && !isTRUE(dir.create(d, recursive = TRUE, showWarnings = FALSE))) {
      stop("cannot create directory: ", d, call. = FALSE)
    }
  }
  if (resumed) .sr_ck_tidy(ck)

  # the source: a copy (built next to the staged store, moved to data/source when the table is
  # installed) or the file in place
  work <- path
  rel <- NA_character_
  copy <- file.path(ck$ingest_dir, paste0("copy.", .sr_source_ext(path)))
  if (!external) {
    rel <- paste0("data/source/", table, ".", .sr_source_ext(path))
    if (!(resumed && file.exists(copy) && is.character(ck$source_sha256) && !is.na(ck$source_sha256) &&
          isTRUE(as.numeric(file.size(copy)) == fp$size))) {
      ck$source_sha256 <- .sr_copy_hashed(path, copy)
    }
    work <- normalizePath(copy, winslash = "/", mustWork = TRUE)
  } else {
    unlink(list.files(ck$ingest_dir, pattern = "^copy[.]", full.names = TRUE))
  }
  .sr_ck_write(ck)

  rsniff <- sniff
  rsniff$path <- work
  rsniff$size_bytes <- as.numeric(file.size(work))
  res <- reader(project, rsniff, table, ck, options, progress, cancel)

  # every part must have the same columns and types before anything is installed
  .sr_ingest_parts_consistent(project, ck)

  # verified manifest, swap into data/store, the source copy into data/source, checkpoint done, event
  parts <- lapply(ck$parts, function(p) list(file = p$file, rows = p$rows, sha256 = p$sha256))
  rows <- sum(vapply(parts, function(p) p$rows, numeric(1)))
  manifest <- .sr_store_install(ck, list(table = table, parts = parts, schema = res$schema, rows = rows))
  store_hash <- sr_store_hash(manifest)
  if (!external) .sr_source_install(data, table, copy, rel)
  ck$done <- TRUE
  .sr_ck_write(ck)
  unlink(file.path(ck$ingest_dir, c("source.csv", "chunk.csv")))

  do <- as.list(sniff$date_order %||% character())
  if (!length(do)) do <- structure(list(), names = character())
  payload <- list(
    table = table, format = sniff$format,
    source = list(path = if (external) path else rel, external = external, fingerprint = fp,
                  sha256 = if (external) NA_character_ else ck$source_sha256),
    store = list(dir = paste0("data/store/", table), rows = rows, ncol = length(res$schema),
                 schema = res$schema, parts = parts),
    store_hash = store_hash, resumable = isTRUE(res$resumable),
    type_widened = as.list(as.character(res$type_widened)),
    sniff = list(delim = sniff$delim, encoding = sniff$encoding, decimal = sniff$decimal, date_order = do,
                 nullstr = as.list(as.character(res$nullstr))))
  if (length(res$source_tz)) payload$source_tz <- res$source_tz
  sr_project_append(project, "data.imported", payload)
}

# The parts of a store must agree on every column name and type with the first part: a reader that typed
# its chunks independently would otherwise install a table that cannot be read as one. Refused before
# the store is installed (the error names the part and the column).
.sr_ingest_parts_consistent <- function(project, ck) {
  if (length(ck$parts) < 2L) return(invisible(TRUE))
  con <- sr_duck(project)
  desc <- function(file) {
    f <- normalizePath(file.path(ck$store_dir, file), winslash = "/", mustWork = TRUE)
    d <- sr_duck_query(con, paste("DESCRIBE SELECT * FROM read_parquet(", DBI::dbQuoteString(con, f), ")"))
    stats::setNames(as.character(d$column_type), as.character(d$column_name))
  }
  first <- desc(ck$parts[[1L]]$file)
  for (k in seq_along(ck$parts)[-1L]) {
    cur <- desc(ck$parts[[k]]$file)
    if (!identical(names(cur), names(first))) {
      stop("the parts of the table disagree on their columns: ", ck$parts[[k]]$file, " does not have the columns of ",
           ck$parts[[1L]]$file, call. = FALSE)
    }
    bad <- names(first)[cur != first]
    if (length(bad)) {
      stop("the parts of the table disagree on the type of column '", bad[1L], "': ", ck$parts[[k]]$file, " has ",
           cur[[bad[1L]]], ", ", ck$parts[[1L]]$file, " has ", first[[bad[1L]]], call. = FALSE)
    }
  }
  invisible(TRUE)
}

# Does the reader first expand the source to a plain UTF-8 file under data/ingest? (bz2 / xz / zip, or
# an encoding other than UTF-8; a gzip is read in place by DuckDB.) Only the delimited-text formats.
.sr_ingest_stages <- function(sniff) {
  if (sniff$format %in% c("ndjson", "json")) return(.sr_json_needs_prep(sniff))
  if (!(sniff$format %in% c("csv", "tsv", "txt"))) return(FALSE)
  comp <- sniff$compression
  enc <- sniff$encoding
  (is.character(comp) && length(comp) == 1L && comp %in% c("bz2", "xz", "zip")) ||
    (is.character(enc) && length(enc) == 1L && !is.na(enc) && !(enc %in% c("UTF-8", "utf-8")))
}

# Move the staged source copy to data/source/<table>.<ext>, replacing the previous copy (and any
# other data/source/<table>.* left by an earlier ingest with a different extension).
.sr_source_install <- function(data, table, copy, rel) {
  src_dir <- file.path(data, "source")
  if (!dir.exists(src_dir) && !isTRUE(dir.create(src_dir, recursive = TRUE, showWarnings = FALSE))) {
    stop("cannot create directory: ", src_dir, call. = FALSE)
  }
  unlink(list.files(src_dir, pattern = paste0("^", table, "[.]"), full.names = TRUE))
  .sr_rename_retry(copy, file.path(src_dir, basename(rel)))
  invisible(rel)
}

# `data.imported` sets the table's entry. Keys the payload does not carry survive (a later task adds
# `active` to an entry), and a key it does carry is replaced whole (the parts are an array: a
# recursive merge would keep the old ones).
sr_reducer_register("data.imported", function(state, event) {
  p <- event$payload
  tabs <- state$tables %||% list()
  old <- tabs[[p$table]] %||% list()
  for (k in names(p)) old[k] <- list(p[[k]])
  if (is.null(p$source_tz)) old$source_tz <- NULL         # not carried: a re-import must not keep the old one
  tabs[[p$table]] <- old
  state$tables <- tabs
  state
})

# The delimited-text reader (read_text.R, loaded before this file) serves csv, and tsv / txt should
# the sniffer ever report them.
.sr_reader_register("csv", .sr_read_csv)
.sr_reader_register("tsv", .sr_read_csv)
.sr_reader_register("txt", .sr_read_csv)

# The columnar, JSON and database readers (read_columnar.R, loaded before this file).
.sr_reader_register("parquet", .sr_read_parquet)
.sr_reader_register("feather", .sr_read_feather)
.sr_reader_register("rds", .sr_read_rds)
.sr_reader_register("rdata", .sr_read_rdata)
.sr_reader_register("fst", .sr_read_fst)
.sr_reader_register("ndjson", .sr_read_ndjson)
.sr_reader_register("json", .sr_read_json)
.sr_reader_register("sqlite", .sr_read_sqlite)
.sr_reader_register("duckdb", .sr_read_duckdb)
