# scale/stream_extract.R - the Parquet extract of the streaming engine (P1e.1, spec section 5).
#
# sr_extract_stream() writes the analysis columns plus `.sr_row` (the 1-based position under
# ORDER BY every column, the order .sr_extract_frame uses, so it is the position that
# spec$exclude_rows refers to) to data/extracts/<sha>.parquet, <sha> being the streaming SHA-256 of
# the file. The file is written under a temp name and renamed, so a file named by a SHA is complete.

#' Bytes the stream extract needs on disk: rows * bytes_per_row * 1.35 (0.35 for the file plus 1.0 for
#' the DuckDB sort spill) plus 1 GB of headroom.
.sr_extract_stream_need <- function(rows, bytes_per_row) {
  rows * bytes_per_row * 1.35 + 1024^3
}

# Run the COPY statement on the connection. Internal and unguarded (the guard allowlist has no COPY);
# a separate function so a test can make it fail.
.sr_extract_copy <- function(con, sql) {
  DBI::dbExecute(con, sql)
}

#' Materialise `cols` of `table` plus `.sr_row` as data/extracts/<sha>.parquet. Returns
#' list(rel, sha, n, cols, kind = "parquet", bytes); `rel` is relative to the project folder. An
#' existing file of that name is kept. Refuses, before writing, when the data drive lacks space.
sr_extract_stream <- function(project, table, cols, drive_info = .sr_drive_info) {
  .sr_store_check_table(table)
  if (!is.character(cols) || !length(cols) || anyNA(cols) || any(!nzchar(cols))) {
    stop("cols must be a character vector naming at least one column", call. = FALSE)
  }
  cols <- unique(cols)
  if (!(table %in% sr_store_tables(project))) {
    stop("table '", table, "' is not in the project store", call. = FALSE)
  }
  m <- sr_store_manifest(project, table)
  have <- vapply(m$schema, function(s) s$name, "")
  unknown <- setdiff(cols, have)
  if (length(unknown)) {
    stop("unknown column(s) for table '", table, "': ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  dir <- file.path(project$paths$dir, "data", "extracts")

  # space check, before anything is written
  cp <- .sr_prof_columns(project, table, m, sr_store_hash(m))$columns
  cp <- cp[match(cols, cp$column), , drop = FALSE]
  nchar <- ifelse(is.na(cp$mean_nchar), 16, cp$mean_nchar)
  per_row <- sum(vapply(seq_len(nrow(cp)), function(i) .sr_mem_bytes(cp$type[i], nchar[i]), numeric(1)))
  need <- .sr_extract_stream_need(m$rows, per_row)
  free <- suppressWarnings(as.numeric(drive_info(dir)$free_bytes))
  if (length(free) != 1L || is.na(free) || need > free) {
    stop(sprintf("not enough free space for the stream extract: about %.2f GB needed, %.2f GB free",
                 need / 1024^3, if (length(free) == 1L && !is.na(free)) free / 1024^3 else 0),
         call. = FALSE)
  }

  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  # a killed run leaves its .tmp-*.parquet behind; sweep the ones too old to belong to a live run
  old <- list.files(dir, pattern = "^[.]tmp-[0-9a-f]+[.]parquet$", full.names = TRUE, all.files = TRUE)
  if (length(old)) unlink(old[difftime(Sys.time(), file.mtime(old), units = "hours") > 24])
  sr_store_view(project, table)
  con <- sr_duck(project)
  q <- DBI::dbQuoteIdentifier(con, cols)
  tmp <- file.path(dir, paste0(".tmp-", paste(as.character(openssl::rand_bytes(8L)), collapse = ""), ".parquet"))
  on.exit(unlink(tmp), add = TRUE)   # after a successful rename there is nothing left to remove
  sql <- sprintf(paste0("COPY (SELECT %s, row_number() OVER (%s) AS \".sr_row\" FROM %s ORDER BY \".sr_row\") ",
                        "TO %s (FORMAT parquet, COMPRESSION zstd, ROW_GROUP_SIZE 122880)"),
                 paste(q, collapse = ", "), .sr_extract_order_sql(con, cols),
                 DBI::dbQuoteIdentifier(con, table),
                 DBI::dbQuoteString(con, normalizePath(tmp, winslash = "/", mustWork = FALSE)))
  .sr_extract_copy(con, sql)
  if (!file.exists(tmp)) stop("could not write the extract", call. = FALSE)
  sha <- .sr_sha256_con(file(tmp))
  rel <- paste0("data/extracts/", sha, ".parquet")
  path <- file.path(project$paths$dir, rel)
  if (file.exists(path)) {
    unlink(tmp)
  } else if (!isTRUE(file.rename(tmp, path))) {
    stop("could not write the extract", call. = FALSE)
  }
  list(rel = rel, sha = sha, n = as.numeric(m$rows), cols = cols, kind = "parquet",
       bytes = as.numeric(file.size(path)))
}
