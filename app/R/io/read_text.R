# io/read_text.R - the delimited-text (csv / tsv / txt) reader behind sr_ingest().
#
# The file is streamed through the locked-down DuckDB into Parquet parts. The design keeps three
# promises: no value is ever silently changed or turned into NULL, a killed job resumes from its last
# committed part, and the same input gives the same bytes.
#
#  * Every read is ALL-VARCHAR (explicit `columns`, `parallel = false`, the sniffed dialect). Typing is
#    done in SQL with strict casts, because DuckDB's own typed reader rounds "7.5" into a BIGINT column.
#    A numeric cast is guarded by a regular expression (no leading zeros, no stray characters); the
#    cast of anything else (dates, timestamps, booleans) fails loudly when the text is not valid.
#    Only the strings in `nullstr` (default "" and "NA") are NULL, and only when UNQUOTED: a quoted
#    "NA" or "" is a text value (allow_quoted_nulls = false).
#  * The column types are fixed ONCE, from DuckDB's DESCRIBE over `sample_size` rows, and stored in the
#    checkpoint. dmy / mdy columns are DATE parsed with strptime and the separator seen in the data;
#    ambiguous date columns stay VARCHAR. A column whose sample is mostly numbers but not entirely is
#    VARCHAR (listed in type_widened); integers with leading zeros are VARCHAR too (zip codes, ids).
#  * A value that does not fit the fixed type (a late "pending" in a numeric column) fails the COPY;
#    the failing chunk is re-read to find the offending columns, they are widened (integer -> double ->
#    VARCHAR; anything else -> VARCHAR; a whole number above 2^53 never goes to double, a double
#    would round it, so such a column goes to VARCHAR), the parts written so far are deleted and the ingest restarts.
#  * Parts are made by chunking on record boundaries (a chunk ends after the first LF outside quotes
#    once chunk_bytes have been read), so a job is resumable. A file that is gzip (single pass), has
#    one chunk or fewer, or is read through one COPY is not resumable (`resumable = FALSE`).
#  * bz2 / xz / zip, and any source that is not UTF-8, are first streamed to data/ingest/<table>/
#    source.csv (decompressed, re-encoded to UTF-8; binary, so CRLF inside quotes survives).

.sr_csv_int_re <- "[+-]?(0|[1-9][0-9]*)"
.sr_csv_dbl_re <- "[+-]?((0|[1-9][0-9]*)([.][0-9]*)?|[.][0-9]+)([eE][+-]?[0-9]+)?"
# The column types a checkpoint may name (anything else is a corrupt or tampered checkpoint).
.sr_csv_types <- c("BIGINT", "INTEGER", "DOUBLE", "DATE", "TIMESTAMP", "TIME", "BOOLEAN", "VARCHAR")
# A whole number a double cannot hold exactly: more than 2^53 (9007199254740992) in absolute value.
.sr_csv_unsafe_int_sql <- function(t) {
  d <- sprintf("regexp_replace(%s, '^[+-]', '')", t)
  sprintf("(regexp_full_match(%s, '[+-]?(0|[1-9][0-9]*)') AND (length(%s) > 16 OR (length(%s) = 16 AND %s > '9007199254740992')))",
          t, d, d, d)
}

# ---- settings -------------------------------------------------------------------------------------

.sr_csv_settings <- function(sniff, options) {
  one <- function(x, default) if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) x else default
  nullstr <- options$nullstr %||% c("", "NA")
  if (!is.character(nullstr) || anyNA(nullstr)) stop("options$nullstr must be a character vector", call. = FALSE)
  ss <- options$sample_size %||% 1e6
  if (!is.numeric(ss) || length(ss) != 1L || !is.finite(ss) || ss < 1) {
    stop("options$sample_size must be a positive number", call. = FALSE)
  }
  list(delim = one(sniff$delim, ","), quote = one(sniff$quote, "\""), decimal = one(sniff$decimal, "."),
       header = isTRUE(sniff$header), nullstr = nullstr, sample_size = floor(ss))
}

# The read_csv(...) table expression. `cols` NULL: auto-detect the types over st$sample_size rows (for
# DESCRIBE); otherwise `cols` is a character vector of column names and every column is VARCHAR.
.sr_csv_from <- function(con, file, st, header, comp, cols = NULL) {
  lit <- function(x) DBI::dbQuoteString(con, x)
  opts <- c(sprintf("delim = %s", lit(st$delim)), sprintf("quote = %s", lit(st$quote)),
            sprintf("header = %s", if (header) "true" else "false"), "parallel = false", "allow_quoted_nulls = false",
            sprintf("compression = %s", lit(comp)))
  if (length(st$nullstr)) opts <- c(opts, sprintf("nullstr = [%s]", paste(lit(st$nullstr), collapse = ", ")))
  if (is.null(cols)) {
    if (!identical(st$decimal, ".")) opts <- c(opts, sprintf("decimal_separator = %s", lit(st$decimal)))
    opts <- c(opts, sprintf("sample_size = %.0f", st$sample_size))
  } else {
    struct <- paste0(lit(cols), ": 'VARCHAR'", collapse = ", ")
    opts <- c(opts, sprintf("columns = {%s}", struct))
  }
  sprintf("read_csv(%s, %s)", lit(normalizePath(file, winslash = "/", mustWork = FALSE)),
          paste(opts, collapse = ", "))
}

# ---- typing ---------------------------------------------------------------------------------------

# DuckDB type name -> the type this reader stores.
.sr_csv_map_type <- function(t) {
  t <- toupper(t)
  if (t %in% .sr_csv_types) return(t)
  if (grepl("^U?(BIG|SMALL|TINY|HUGE)?INT(EGER)?$", t)) return("BIGINT")
  if (grepl("^DECIMAL|^FLOAT|^REAL", t)) return("DOUBLE")
  if (t == "TIMESTAMP_S" || t == "TIMESTAMP_MS" || t == "TIMESTAMP_NS") return("TIMESTAMP")
  "VARCHAR"
}

# Strict SQL for one column read as VARCHAR: NULL stays NULL; a value that is not valid text for the
# type raises an error (error() / CAST), it is never turned into NULL.
.sr_csv_cast_sql <- function(con, cl, decimal) {
  lit <- function(x) DBI::dbQuoteString(con, x)
  id <- DBI::dbQuoteIdentifier(con, cl$name)
  t <- paste0("trim(", id, ")")
  bad <- "error('type conflict')"
  guarded <- function(x, re, to) {
    sprintf("CASE WHEN %s IS NULL THEN NULL WHEN regexp_full_match(%s, %s) THEN CAST(%s AS %s) ELSE %s END",
            id, x, lit(re), x, to, bad)
  }
  switch(cl$type,
    VARCHAR = id,
    BIGINT = guarded(t, .sr_csv_int_re, "BIGINT"),
    INTEGER = guarded(t, .sr_csv_int_re, "INTEGER"),
    DOUBLE = {
      x <- if (identical(decimal, ",")) paste0("replace(", t, ", ',', '.')") else t
      sprintf("CASE WHEN %s IS NULL THEN NULL WHEN %s THEN %s WHEN regexp_full_match(%s, %s) THEN CAST(%s AS DOUBLE) ELSE %s END",
              id, .sr_csv_unsafe_int_sql(x), bad, x, lit(.sr_csv_dbl_re), x, bad)
    },
    DATE = if (is.na(cl$fmt)) {
      sprintf("CASE WHEN %s IS NULL THEN NULL ELSE CAST(%s AS DATE) END", id, t)
    } else {
      sprintf("CASE WHEN %s IS NULL THEN NULL ELSE CAST(strptime(%s, %s) AS DATE) END", id, t, lit(cl$fmt))
    },
    sprintf("CASE WHEN %s IS NULL THEN NULL ELSE CAST(%s AS %s) END", id, t, cl$type))
}

# SQL (TRUE for each value that does NOT convert) used only to diagnose a failed chunk. Never used for
# output, so try_strptime / TRY_CAST are fine here.
.sr_csv_bad_sql <- function(con, cl, decimal, as = cl$type) {
  lit <- function(x) DBI::dbQuoteString(con, x)
  id <- DBI::dbQuoteIdentifier(con, cl$name)
  t <- paste0("trim(", id, ")")
  num <- function(x, re, to) sprintf("(NOT regexp_full_match(%s, %s) OR TRY_CAST(%s AS %s) IS NULL)", x, lit(re), x, to)
  body <- switch(as,
    BIGINT = num(t, .sr_csv_int_re, "BIGINT"),
    INTEGER = num(t, .sr_csv_int_re, "INTEGER"),
    DOUBLE = {
      x <- if (identical(decimal, ",")) paste0("replace(", t, ", ',', '.')") else t
      sprintf("(%s OR %s)", num(x, .sr_csv_dbl_re, "DOUBLE"), .sr_csv_unsafe_int_sql(x))
    },
    DATE = if (is.na(cl$fmt)) sprintf("TRY_CAST(%s AS DATE) IS NULL", t) else sprintf("try_strptime(%s, %s) IS NULL", t, lit(cl$fmt)),
    sprintf("TRY_CAST(%s AS %s) IS NULL", t, as))
  sprintf("count(*) FILTER (WHERE %s IS NOT NULL AND %s)", id, body)
}

.sr_type_conflict <- function(widen) {
  structure(class = c("sr_type_conflict", "error", "condition"),
            list(message = "a value does not fit its column type", call = NULL, widen = widen))
}

# Which columns of the file (or chunk) `file` do not fit their fixed type, and what they widen to.
# Returns list(list(name, from, to), ...); empty when the cause is not a type conflict.
.sr_csv_diagnose <- function(con, file, st, header, comp, ck) {
  cols <- ck$columns
  nm <- vapply(cols, function(cl) cl$name, character(1))
  sel <- character()
  for (i in seq_along(cols)) {
    cl <- cols[[i]]
    if (identical(cl$type, "VARCHAR")) next
    sel <- c(sel, sprintf("%s AS %s", .sr_csv_bad_sql(con, cl, st$decimal), paste0("b", i)))
    if (cl$type %in% c("BIGINT", "INTEGER")) {
      sel <- c(sel, sprintf("%s AS %s", .sr_csv_bad_sql(con, cl, st$decimal, as = "DOUBLE"), paste0("c", i)))
    }
  }
  if (!length(sel)) return(list())
  r <- tryCatch(
    sr_duck_query(con, sprintf("SELECT %s FROM %s", paste(sel, collapse = ", "),
                               .sr_csv_from(con, file, st, header, comp, cols = nm))),
    error = function(e) NULL)
  if (is.null(r)) return(list())
  out <- list()
  for (i in seq_along(cols)) {
    b <- r[[paste0("b", i)]]
    if (is.null(b) || !isTRUE(b > 0)) next
    cl <- cols[[i]]
    to <- "VARCHAR"
    if (cl$type %in% c("BIGINT", "INTEGER") && isTRUE(r[[paste0("c", i)]] == 0)) to <- "DOUBLE"
    out[[length(out) + 1L]] <- list(name = cl$name, from = cl$type, to = to)
  }
  out
}

# Apply a diagnosis: widen the columns, delete the parts, record why (checkpoint widen_log) and start
# from the top. Stops with a clear error when the restarts run out (a bug, never data).
.sr_csv_widen <- function(ck, widen) {
  for (w in widen) {
    i <- which(vapply(ck$columns, function(cl) identical(cl$name, w$name), logical(1)))[1L]
    ck$columns[[i]]$type <- w$to
    ck$columns[[i]]$fmt <- NA_character_
    ck$type_widened <- union(ck$type_widened, w$name)
    ck$widen_log[[length(ck$widen_log) + 1L]] <- list(column = w$name, from = w$from, to = w$to,
                                                       at_offset = ck$next_offset)
  }
  ck$restarts <- ck$restarts + 1L
  if (ck$restarts > 2L * length(ck$columns)) {
    stop("too many type-conflict restarts (", ck$restarts, "): giving up", call. = FALSE)
  }
  .sr_ck_drop_parts(ck)
  .sr_ck_write(ck)
  invisible(ck)
}

# Fix the column types once. Fills ck$columns (list(name, type, fmt)) and ck$type_widened.
.sr_csv_infer <- function(con, file, st, comp, ck, date_order) {
  lit <- function(x) DBI::dbQuoteString(con, x)
  d <- sr_duck_query(con, paste("DESCRIBE SELECT * FROM", .sr_csv_from(con, file, st, st$header, comp)))
  nm <- as.character(d$column_name)
  ty <- vapply(as.character(d$column_type), .sr_csv_map_type, character(1), USE.NAMES = FALSE)
  if (!length(nm)) stop("the file has no columns", call. = FALSE)
  if (anyDuplicated(nm)) stop("duplicate column names in the file", call. = FALSE)
  dord <- if (length(date_order)) as.character(date_order)[match(nm, names(date_order))] else rep(NA_character_, length(nm))

  # one pass over the sample: the facts the type decisions need
  sel <- character()
  for (i in seq_along(nm)) {
    id <- DBI::dbQuoteIdentifier(con, nm[i])
    t <- paste0("trim(", id, ")")
    o <- dord[i]
    if (!is.na(o) && o %in% c("dmy", "mdy")) {
      sel <- c(sel, sprintf(paste0("min(regexp_extract(%s, '^[0-9]{1,2}([/.-])', 1)) FILTER (WHERE %s IS NOT NULL AND ",
                                   "regexp_matches(%s, '^[0-9]{1,2}[/.-]')) AS sep%d"), t, id, t, i))
    } else if (is.na(o) && identical(ty[i], "VARCHAR")) {
      sel <- c(sel, sprintf("count(%s) AS nn%d", id, i),
               sprintf("count(*) FILTER (WHERE TRY_CAST(%s AS DOUBLE) IS NOT NULL) AS nd%d", t, i),
               sprintf("count(*) FILTER (WHERE TRY_CAST(%s AS DATE) IS NOT NULL) AS ndt%d", t, i))
    } else if (ty[i] %in% c("BIGINT", "INTEGER")) {
      sel <- c(sel, sprintf("count(*) FILTER (WHERE regexp_full_match(%s, '[+-]?0[0-9]+')) AS lz%d", t, i))
    }
  }
  facts <- list()
  if (length(sel)) {
    facts <- as.list(sr_duck_query(con, sprintf(
      "SELECT %s FROM (SELECT * FROM %s LIMIT %.0f) s", paste(sel, collapse = ", "),
      .sr_csv_from(con, file, st, st$header, comp, cols = nm), st$sample_size)))
  }
  num <- function(key) {
    v <- facts[[key]]
    if (is.null(v) || is.na(v)) 0 else as.numeric(v)
  }

  cols <- vector("list", length(nm))
  widened <- character()
  for (i in seq_along(nm)) {
    cl <- list(name = nm[i], type = ty[i], fmt = NA_character_)
    o <- dord[i]
    if (!is.na(o) && o %in% c("dmy", "mdy")) {
      sep <- facts[[paste0("sep", i)]]
      if (is.null(sep) || is.na(sep) || !nzchar(sep)) {
        cl$type <- "VARCHAR"
        widened <- c(widened, nm[i])
      } else {
        cl$type <- "DATE"
        f <- if (o == "dmy") c("%d", "%m", "%Y") else c("%m", "%d", "%Y")
        cl$fmt <- paste(f, collapse = sep)
      }
    } else if (!is.na(o) && o == "ambiguous") {
      cl$type <- "VARCHAR"
    } else if (identical(ty[i], "VARCHAR")) {
      nn <- num(paste0("nn", i))
      if (nn > 0 && (num(paste0("nd", i)) >= 0.9 * nn || num(paste0("ndt", i)) >= 0.9 * nn)) {
        widened <- c(widened, nm[i])
      }
    } else if (ty[i] %in% c("BIGINT", "INTEGER") && num(paste0("lz", i)) > 0) {
      cl$type <- "VARCHAR"
      widened <- c(widened, nm[i])
    }
    cols[[i]] <- cl
  }
  ck$columns <- cols
  ck$type_widened <- unique(widened)
  .sr_ck_write(ck)
  invisible(ck)
}

# ---- preparing the text (decompress, re-encode) ----------------------------------------------------

# 1-based end index of the last LF code unit in `b` for a UTF-16 stream (0 when there is none). The
# buffer always starts on a code-unit boundary, so an LF unit starts at an odd 1-based index.
.sr_csv_last_lf16 <- function(b, enc) {
  n <- length(b)
  if (n < 2L) return(0L)
  lo <- b[-n]
  hi <- b[-1L]
  hit <- if (identical(enc, "UTF-16LE")) which(lo == as.raw(0x0A) & hi == as.raw(0x00))
         else which(lo == as.raw(0x00) & hi == as.raw(0x0A))
  hit <- hit[hit %% 2L == 1L]
  if (length(hit)) max(hit) + 1L else 0L
}

.sr_csv_convert <- function(b, enc) {
  out <- iconv(list(b), from = enc, to = "UTF-8", toRaw = TRUE)[[1L]]
  if (is.null(out)) stop("cannot convert the file from ", enc, " to UTF-8 (invalid bytes)", call. = FALSE)
  out
}

# Stream the (decompressed) source to `dest` as UTF-8. Binary throughout, so every byte of a UTF-8
# source, CRLF inside quoted fields included, is kept; a non-UTF-8 source is converted block by block
# at LF boundaries. A BOM of a UTF-16 source is dropped.
.sr_csv_prepare <- function(sniff, dest) {
  enc <- sniff$encoding
  if (is.null(enc) || is.na(enc) || enc %in% c("UTF-8", "utf-8")) enc <- "UTF-8"
  tmp <- paste0(dest, ".tmp")
  cin <- .sr_sniff_conn(sniff$path, sniff$compression, sniff$member, "rb")
  cout <- file(tmp, "wb")
  ok <- FALSE
  on.exit({
    try(close(cin), silent = TRUE)
    try(close(cout), silent = TRUE)
    if (!ok) unlink(tmp)
  }, add = TRUE)
  carry <- raw(0)
  first <- TRUE
  repeat {
    blk <- readBin(cin, "raw", n = .sr_io_block)
    eof <- !length(blk)
    buf <- if (length(carry)) c(carry, blk) else blk
    if (first && length(buf) >= 2L) {
      first <- FALSE
      if (enc == "UTF-16LE" && buf[1L] == as.raw(0xFF) && buf[2L] == as.raw(0xFE)) buf <- buf[-(1:2)]
      if (enc == "UTF-16BE" && buf[1L] == as.raw(0xFE) && buf[2L] == as.raw(0xFF)) buf <- buf[-(1:2)]
    }
    cut <- if (eof) length(buf) else if (enc == "UTF-8" || enc == "latin1") length(buf) else .sr_csv_last_lf16(buf, enc)
    if (cut > 0L) {
      piece <- buf[seq_len(cut)]
      writeBin(if (enc == "UTF-8") piece else .sr_csv_convert(piece, enc), cout)
    }
    carry <- if (cut < length(buf)) buf[(cut + 1L):length(buf)] else raw(0)
    if (eof) break
  }
  flush(cout)
  close(cout)
  close(cin)
  ok <- TRUE
  .sr_rename_retry(tmp, dest)
  invisible(dest)
}

# ---- chunking -------------------------------------------------------------------------------------

# Offset just after the first record of `file` (the header): the first LF outside quotes.
.sr_csv_header_end <- function(file, qb) {
  con <- file(file, "rb")
  on.exit(close(con), add = TRUE)
  pos <- 0
  par <- 0L
  repeat {
    blk <- readBin(con, "raw", n = .sr_io_block)
    n <- length(blk)
    if (!n) break
    pa <- if (is.null(qb)) rep(0L, n) else (par + cumsum(blk == qb)) %% 2L
    cand <- which(blk == as.raw(0x0A) & pa == 0L)
    if (length(cand)) return(pos + cand[1L])
    par <- pa[n]
    pos <- pos + n
  }
  pos
}

# Write the chunk that starts at byte `start` of `file` to `dest` (the header bytes first): it ends
# after the first LF outside quotes at or beyond `min_end`, or at the end of the file. Returns the
# offset just after the chunk. The quote parity is counted from `start`, a record boundary.
.sr_csv_write_chunk <- function(file, start, min_end, qb, header_bytes, dest) {
  cin <- file(file, "rb")
  cout <- file(dest, "wb")
  on.exit({ close(cin); close(cout) }, add = TRUE)
  if (length(header_bytes)) writeBin(header_bytes, cout)
  seek(cin, where = start, origin = "start", rw = "read")
  pos <- start
  par <- 0L
  repeat {
    blk <- readBin(cin, "raw", n = .sr_io_block)
    n <- length(blk)
    if (!n) break
    isq <- if (is.null(qb)) NULL else blk == qb
    if (pos + n < min_end) {                       # the whole block lies before the earliest cut
      writeBin(blk, cout)
      if (!is.null(isq)) par <- (par + sum(isq)) %% 2L
      pos <- pos + n
      next
    }
    pa <- if (is.null(isq)) rep(0L, n) else (par + cumsum(isq)) %% 2L
    cand <- which(blk == as.raw(0x0A) & pa == 0L & (pos + seq_len(n)) >= min_end)
    if (length(cand)) {
      k <- cand[1L]
      writeBin(blk[seq_len(k)], cout)
      return(pos + k)
    }
    writeBin(blk, cout)
    par <- pa[n]
    pos <- pos + n
  }
  pos
}

# ---- one COPY -------------------------------------------------------------------------------------

# COPY the typed rows of `file` to the Parquet file `to`; returns the row count written (read back
# from the Parquet footer). On an error the temp file is removed and the error returned, not thrown.
.sr_csv_copy <- function(con, file, st, header, comp, ck, to) {
  nm <- vapply(ck$columns, function(cl) cl$name, character(1))
  sel <- vapply(ck$columns, function(cl) {
    paste0(.sr_csv_cast_sql(con, cl, st$decimal), " AS ", DBI::dbQuoteIdentifier(con, cl$name))
  }, character(1))
  sql <- sprintf("COPY (SELECT %s FROM %s) TO %s (FORMAT parquet, COMPRESSION zstd)",
                 paste(sel, collapse = ", "), .sr_csv_from(con, file, st, header, comp, cols = nm),
                 DBI::dbQuoteString(con, normalizePath(to, winslash = "/", mustWork = FALSE)))
  res <- tryCatch({
    sr_duck_exec(con, sql)
    n <- sr_duck_query(con, sprintf("SELECT count(*) AS n FROM read_parquet(%s)",
                                    DBI::dbQuoteString(con, normalizePath(to, winslash = "/", mustWork = FALSE))))
    as.numeric(n$n[1L])
  }, error = function(e) e)
  if (inherits(res, "error")) unlink(to)
  res
}

# On a failed COPY: widen and restart when the cause is a type conflict, else rethrow the error.
.sr_csv_fail <- function(con, err, file, st, header, comp, ck) {
  w <- .sr_csv_diagnose(con, file, st, header, comp, ck)
  if (!length(w)) stop(err)
  stop(.sr_type_conflict(w))
}

.sr_csv_run_single <- function(con, work, st, comp, ck, size, progress, cancel) {
  if (length(ck$parts) && ck$next_offset >= size) return(invisible())
  if (cancel()) stop(.sr_ingest_cancelled())
  to <- .sr_part_path(ck)
  rows <- .sr_csv_copy(con, work, st, st$header, comp, ck, to)
  if (inherits(rows, "error")) .sr_csv_fail(con, rows, work, st, st$header, comp, ck)
  nm <- vapply(ck$columns, function(cl) cl$name, character(1))
  total <- sr_duck_query(con, paste("SELECT count(*) AS n FROM", .sr_csv_from(con, work, st, st$header, comp, cols = nm)))
  if (as.numeric(total$n[1L]) != rows) {
    unlink(to)
    stop("row count mismatch: wrote ", rows, " rows but the source has ", total$n[1L], call. = FALSE)
  }
  ck$next_offset <- size
  .sr_part_commit(ck, to, rows)
  if (!is.null(progress)) progress(size, size)
  invisible()
}

.sr_csv_run_chunks <- function(con, work, st, ck, size, qb, progress, cancel) {
  hdr_end <- if (st$header) .sr_csv_header_end(work, qb) else 0
  hdr <- if (hdr_end > 0) readBin(work, "raw", n = hdr_end) else raw(0)
  if (ck$next_offset < hdr_end) ck$next_offset <- hdr_end
  chunk <- file.path(ck$ingest_dir, "chunk.csv")
  while (ck$next_offset < size) {
    start <- ck$next_offset
    end <- .sr_csv_write_chunk(work, start, start + ck$chunk_bytes, qb, hdr, chunk)
    to <- .sr_part_path(ck)
    rows <- .sr_csv_copy(con, chunk, st, st$header, "none", ck, to)
    if (inherits(rows, "error")) {
      tryCatch(.sr_csv_fail(con, rows, chunk, st, st$header, "none", ck), finally = unlink(chunk))
    }
    unlink(chunk)
    ck$next_offset <- end
    if (rows > 0 || !length(ck$parts)) {
      .sr_part_commit(ck, to, rows)
    } else {
      unlink(to)                                   # trailing blank lines: nothing to keep
      .sr_ck_write(ck)
    }
    if (!is.null(progress)) progress(end, size)
    if (end < size && cancel()) stop(.sr_ingest_cancelled())
  }
  invisible()
}

# ---- the reader -----------------------------------------------------------------------------------

.sr_read_csv <- function(project, sniff, table, ck, options, progress, cancel) {
  st <- .sr_csv_settings(sniff, options)
  con <- sr_duck(project)
  comp <- if (is.character(sniff$compression) && !is.na(sniff$compression)) sniff$compression else "none"
  enc_ok <- is.null(sniff$encoding) || is.na(sniff$encoding) || identical(sniff$encoding, "UTF-8")
  prep <- comp %in% c("bz2", "xz", "zip") || !enc_ok
  rcomp <- if (comp == "gz" && !prep) "gzip" else "none"
  work <- sniff$path
  if (prep) {
    dest <- file.path(ck$ingest_dir, "source.csv")
    if (!(is.numeric(ck$prepared_bytes) && file.exists(dest))) {
      .sr_csv_prepare(sniff, dest)
      ck$prepared_bytes <- as.numeric(file.size(dest))
      .sr_ck_write(ck)
    }
    work <- normalizePath(dest, winslash = "/", mustWork = TRUE)
  }
  size <- as.numeric(file.size(work))
  if (is.null(ck$columns)) .sr_csv_infer(con, work, st, rcomp, ck, sniff$date_order)

  chunked <- !identical(rcomp, "gzip") && !isTRUE(sniff$quoted_newlines) && size > ck$chunk_bytes
  qb <- if (nchar(st$quote, "bytes") == 1L) charToRaw(st$quote) else NULL
  repeat {
    out <- tryCatch({
      if (chunked) .sr_csv_run_chunks(con, work, st, ck, size, qb, progress, cancel)
      else .sr_csv_run_single(con, work, st, rcomp, ck, size, progress, cancel)
      NULL
    }, sr_type_conflict = function(cnd) cnd)
    if (is.null(out)) break
    .sr_csv_widen(ck, out$widen)
  }

  first <- file.path(ck$store_dir, ck$parts[[1L]]$file)
  d <- sr_duck_query(con, paste("DESCRIBE SELECT * FROM read_parquet(",
                                DBI::dbQuoteString(con, normalizePath(first, winslash = "/", mustWork = TRUE)), ")"))
  schema <- lapply(seq_len(nrow(d)), function(i) list(name = as.character(d$column_name[i]),
                                                      type = as.character(d$column_type[i])))
  list(schema = schema, resumable = chunked, type_widened = ck$type_widened, nullstr = st$nullstr)
}
