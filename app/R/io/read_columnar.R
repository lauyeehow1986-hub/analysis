# io/read_columnar.R - readers for columnar, R, JSON and database sources: parquet, feather, rds,
# rdata, fst, ndjson, json, sqlite and duckdb. They sit behind the reader contract of ingest.R:
#   function(project, sniff, table, ck, options, progress, cancel) -> list(schema, resumable, type_widened)
#
# Every reader ends in COPY (SELECT ... ORDER BY <row order>) TO <part>.parquet on the project's locked
# DuckDB connection. The explicit ORDER BY matters: the connection runs with preserve_insertion_order
# off, so without it the row order (and with it the part's SHA-256) would depend on thread timing.
#   * parquet / duckdb: read straight through DuckDB (file_row_number / rowid give the order);
#   * feather: arrow::read_feather -> duckdb_register_arrow;
#   * rds / rdata / fst: the data frame is registered with duckdb_register (zero copy);
#   * sqlite: RSQLite, read-only; chunked by row offset (ORDER BY rowid), so it resumes;
#   * ndjson: chunked by bytes like CSV; types fixed from the first chunk, a later mismatch widens the
#     column through the Task 5 conflict path (restart, recorded in type_widened);
#   * json: one array of objects, parsed whole (the pre-flight refuses documents over 512 MB).
# A temporary DuckDB registration is always removed (on.exit); a user's DuckDB file is attached
# READ_ONLY by .sr_duck_attach() and always detached (on.exit). Nothing here changes the lockdown.

.sr_col_roword <- ".sr_row_order"     # the row-order column added to a frame / Arrow table
.sr_json_chunk_max <- 32 * 1024^2     # an NDJSON chunk is parsed into R lists: bound its memory

# ---- shared helpers -------------------------------------------------------------------------------

.sr_col_check_names <- function(nm, what = "the data") {
  if (!length(nm)) stop(what, " has no columns", call. = FALSE)
  if (anyNA(nm) || any(!nzchar(nm))) stop(what, " has a column with an empty name", call. = FALSE)
  if (anyDuplicated(nm)) stop(what, " has duplicate column names", call. = FALSE)
  if (any(nm == .sr_col_roword)) stop(what, " has a column named ", .sr_col_roword, ", which is reserved", call. = FALSE)
  invisible(nm)
}

# Pick one of `cands`: the one named by `wanted`, or the only one. Else an error that lists them.
.sr_col_choose <- function(cands, wanted, what, label, opt) {
  if (!length(cands)) stop("the ", label, " has no ", what, "s", call. = FALSE)
  if (!is.null(wanted)) {
    if (!is.character(wanted) || length(wanted) != 1L || is.na(wanted)) {
      stop("options$", opt, " must be a single string", call. = FALSE)
    }
    if (!(wanted %in% cands)) {
      stop(what, " '", wanted, "' is not in the ", label, " (", paste(cands, collapse = ", "), ")", call. = FALSE)
    }
    return(wanted)
  }
  if (length(cands) == 1L) return(cands)
  stop("the ", label, " has several ", what, "s: choose one with options$", opt, " (",
       paste(cands, collapse = ", "), ")", call. = FALSE)
}

.sr_col_nocomp <- function(sniff, what) {
  comp <- sniff$compression
  if (is.character(comp) && length(comp) == 1L && !is.na(comp) && comp != "none") {
    stop(what, " inside a ", comp, " container is not supported: extract it first", call. = FALSE)
  }
  invisible(TRUE)
}

# COPY the rows of `from` (a table expression) to the part file `to` as zstd Parquet; returns the row
# count read back from the footer. On an error the temp file is removed and the error rethrown.
.sr_col_copy <- function(con, from, to, select = "*", order = NULL) {
  dest <- DBI::dbQuoteString(con, normalizePath(to, winslash = "/", mustWork = FALSE))
  sql <- sprintf("COPY (SELECT %s FROM %s%s) TO %s (FORMAT parquet, COMPRESSION zstd)", select, from,
                 if (is.null(order)) "" else paste(" ORDER BY", order), dest)
  res <- tryCatch({
    sr_duck_exec(con, sql)
    n <- sr_duck_query(con, sprintf("SELECT count(*) AS n FROM read_parquet(%s)", dest))
    as.numeric(n$n[1L])
  }, error = function(e) e)
  if (inherits(res, "error")) {
    unlink(to)
    stop(res)
  }
  res
}

# Schema (name, type) of the first committed part.
.sr_col_schema <- function(con, ck) {
  first <- file.path(ck$store_dir, ck$parts[[1L]]$file)
  d <- sr_duck_query(con, paste("DESCRIBE SELECT * FROM read_parquet(",
                                DBI::dbQuoteString(con, normalizePath(first, winslash = "/", mustWork = TRUE)), ")"))
  lapply(seq_len(nrow(d)), function(i) list(name = as.character(d$column_name[i]),
                                            type = as.character(d$column_type[i])))
}

# Run `writer(con, to)` (it writes one part and returns its row count) unless a part already exists;
# commit it; return the reader result. For the readers that produce a single part.
.sr_col_single <- function(project, sniff, ck, progress, cancel, writer) {
  con <- sr_duck(project)
  if (!length(ck$parts)) {
    if (cancel()) stop(.sr_ingest_cancelled())
    to <- .sr_part_path(ck)
    rows <- writer(con, to)
    size <- as.numeric(sniff$size_bytes)
    ck$next_offset <- size
    .sr_part_commit(ck, to, rows)
    if (!is.null(progress)) progress(size, size)
  }
  list(schema = .sr_col_schema(con, ck), resumable = FALSE, type_widened = character())
}

# Register `obj` under a throwaway name, run `fn(quoted_name)`, always unregister.
.sr_col_with_reg <- function(con, obj, fn, arrow = FALSE) {
  nm <- paste0("sr_df_", paste(as.character(openssl::rand_bytes(6L)), collapse = ""))
  if (arrow) duckdb::duckdb_register_arrow(con, nm, obj) else duckdb::duckdb_register(con, nm, obj)
  on.exit(try(if (arrow) duckdb::duckdb_unregister_arrow(con, nm) else duckdb::duckdb_unregister(con, nm),
              silent = TRUE), add = TRUE)
  fn(DBI::dbQuoteIdentifier(con, nm))
}

# One cell of a list column as JSON text (NULL stays NA).
.sr_col_cell <- function(x) if (is.null(x)) NA_character_ else .sr_json(x)

# A data frame ready for DuckDB: plain data.frame, UTF-8 text, factors as text, list columns as JSON
# text, difftime / hms as text; plus a row-order column.
.sr_col_frame <- function(df, what = "the data") {
  if (!is.data.frame(df)) stop(what, " is not a data frame", call. = FALSE)
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  .sr_col_check_names(names(df), what)
  i64 <- character()
  for (i in seq_along(df)) {
    x <- df[[i]]
    if (inherits(x, "integer64")) {
      # duckdb_register() would read the 64-bit pattern as a double: pass the digits as text and CAST
      if (!requireNamespace("bit64", quietly = TRUE)) stop("bit64 package required for 64-bit integers", call. = FALSE)
      x <- as.character(x)
      i64 <- c(i64, names(df)[i])
    } else if (is.factor(x)) {
      x <- as.character(x)
    } else if (inherits(x, "difftime")) {
      x <- ifelse(is.na(x), NA_character_, format(x))
    } else if (is.list(x) && !inherits(x, "POSIXlt")) {
      x <- vapply(x, .sr_col_cell, character(1))
    }
    if (is.character(x)) {
      x <- enc2utf8(unclass(x))
      attributes(x) <- NULL
    }
    df[[i]] <- x
  }
  df[[.sr_col_roword]] <- seq_len(nrow(df))
  attr(df, "sr_int64") <- i64
  df
}

# Write a data frame as one part.
.sr_col_frame_part <- function(con, df, to, what = "the data") {
  df <- .sr_col_frame(df, what)
  .sr_col_with_reg(con, df, function(from) {
    ro <- DBI::dbQuoteIdentifier(con, .sr_col_roword)
    i64 <- attr(df, "sr_int64")
    sel <- if (length(i64)) {
      nm <- setdiff(names(df), .sr_col_roword)
      q <- DBI::dbQuoteIdentifier(con, nm)
      paste(ifelse(nm %in% i64, paste0("CAST(", q, " AS BIGINT) AS ", q), q), collapse = ", ")
    } else {
      paste("* EXCLUDE (", ro, ")")
    }
    .sr_col_copy(con, from, to, select = sel, order = ro)
  })
}

# ---- parquet --------------------------------------------------------------------------------------

.sr_read_parquet <- function(project, sniff, table, ck, options, progress, cancel) {
  .sr_col_nocomp(sniff, "Parquet")
  .sr_col_single(project, sniff, ck, progress, cancel, function(con, to) {
    f <- DBI::dbQuoteString(con, normalizePath(sniff$path, winslash = "/", mustWork = TRUE))
    d <- sr_duck_query(con, paste("DESCRIBE SELECT * FROM read_parquet(", f, ")"))
    .sr_col_check_names(as.character(d$column_name), "the Parquet file")
    .sr_col_copy(con, sprintf("read_parquet(%s, file_row_number = true)", f), to,
                 select = paste("* EXCLUDE (", DBI::dbQuoteIdentifier(con, "file_row_number"), ")"),
                 order = DBI::dbQuoteIdentifier(con, "file_row_number"))
  })
}

# ---- feather --------------------------------------------------------------------------------------

.sr_read_feather <- function(project, sniff, table, ck, options, progress, cancel) {
  if (!requireNamespace("arrow", quietly = TRUE)) stop("arrow package required for Feather/IPC", call. = FALSE)
  .sr_col_nocomp(sniff, "Feather")
  .sr_col_single(project, sniff, ck, progress, cancel, function(con, to) {
    tab <- arrow::read_feather(normalizePath(sniff$path, winslash = "/", mustWork = TRUE), as_data_frame = FALSE)
    .sr_col_check_names(names(tab), "the Feather file")
    tab[[.sr_col_roword]] <- seq_len(tab$num_rows)
    .sr_col_with_reg(con, tab, function(from) {
      ro <- DBI::dbQuoteIdentifier(con, .sr_col_roword)
      .sr_col_copy(con, from, to, select = paste("* EXCLUDE (", ro, ")"), order = ro)
    }, arrow = TRUE)
  })
}

# ---- R data: rds, rdata, fst ----------------------------------------------------------------------

.sr_col_nocomp_zip <- function(sniff) {
  if (identical(sniff$compression, "zip")) stop("an R data file inside a zip archive is not supported: extract it first", call. = FALSE)
  invisible(TRUE)
}

.sr_read_rds <- function(project, sniff, table, ck, options, progress, cancel) {
  .sr_col_nocomp_zip(sniff)
  .sr_col_single(project, sniff, ck, progress, cancel, function(con, to) {
    x <- readRDS(sniff$path)
    if (!is.data.frame(x)) {
      stop("the RDS file does not hold a data frame (it holds ", paste(class(x), collapse = "/"), ")", call. = FALSE)
    }
    .sr_col_frame_part(con, x, to, "the RDS data frame")
  })
}

.sr_read_rdata <- function(project, sniff, table, ck, options, progress, cancel) {
  .sr_col_nocomp_zip(sniff)
  .sr_col_single(project, sniff, ck, progress, cancel, function(con, to) {
    e <- new.env(parent = emptyenv())
    load(sniff$path, envir = e)
    nm <- ls(e, all.names = TRUE)
    cands <- nm[vapply(nm, function(k) is.data.frame(get(k, envir = e)), logical(1))]
    obj <- .sr_col_choose(cands, options$object, "data frame", "RData file", "object")
    .sr_col_frame_part(con, get(obj, envir = e), to, paste0("the data frame '", obj, "'"))
  })
}

.sr_read_fst <- function(project, sniff, table, ck, options, progress, cancel) {
  if (!requireNamespace("fst", quietly = TRUE)) stop("fst package required to read .fst files", call. = FALSE)
  .sr_col_nocomp(sniff, "fst")
  .sr_col_single(project, sniff, ck, progress, cancel, function(con, to) {
    .sr_col_frame_part(con, fst::read_fst(sniff$path), to, "the fst file")
  })
}

# ---- JSON values to typed columns -----------------------------------------------------------------

# Does the source need staging as plain UTF-8 text (any compression, or an encoding other than UTF-8)?
.sr_json_needs_prep <- function(sniff) {
  comp <- sniff$compression
  enc <- sniff$encoding
  (is.character(comp) && length(comp) == 1L && !is.na(comp) && comp != "none") ||
    (is.character(enc) && length(enc) == 1L && !is.na(enc) && !(enc %in% c("UTF-8", "utf-8")))
}

# The plain UTF-8 file to read: the source itself, or data/ingest/<table>/source.csv (the name the
# checkpoint validation knows) made by .sr_csv_prepare().
.sr_json_work <- function(sniff, ck) {
  if (!.sr_json_needs_prep(sniff)) return(sniff$path)
  dest <- file.path(ck$ingest_dir, "source.csv")
  if (!(is.numeric(ck$prepared_bytes) && file.exists(dest))) {
    .sr_csv_prepare(sniff, dest)
    ck$prepared_bytes <- as.numeric(file.size(dest))
    .sr_ck_write(ck)
  }
  normalizePath(dest, winslash = "/", mustWork = TRUE)
}

# Flatten one record (a named list): nested objects become "a.b" columns, arrays (and empty objects)
# become JSON text, null stays NULL.
.sr_json_flat <- function(rec) {
  keys <- character()
  vals <- list()
  walk <- function(x, prefix) {
    nm <- names(x)
    if (anyNA(nm) || any(!nzchar(nm))) stop("a JSON object has an empty key", call. = FALSE)
    for (i in seq_along(x)) {
      v <- x[[i]]
      k <- paste0(prefix, nm[i])
      if (is.list(v) && length(v) && !is.null(names(v))) {
        walk(v, paste0(k, "."))
      } else {
        keys[length(keys) + 1L] <<- k
        vals[length(vals) + 1L] <<- list(if (is.list(v)) .sr_json(v) else v)
      }
    }
  }
  walk(rec, "")
  if (anyDuplicated(keys)) stop("a JSON record has the key '", keys[anyDuplicated(keys)], "' twice", call. = FALSE)
  names(vals) <- keys
  vals
}

# Parse JSON text that holds an array of records into flattened records. `at` names where the text
# starts, for the error message.
.sr_json_records <- function(txt, at) {
  recs <- tryCatch(jsonlite::parse_json(txt, simplifyVector = FALSE, bigint_as_char = TRUE),
                   error = function(e) stop("invalid JSON ", at, ": ", conditionMessage(e), call. = FALSE))
  lapply(recs, function(r) {
    if (!is.list(r) || is.null(names(r))) stop("every JSON record must be an object ", at, call. = FALSE)
    .sr_json_flat(r)
  })
}

# What a column holds: which value classes, and whether a number is not a whole number a BIGINT keeps.
.sr_json_scan <- function(vals) {
  cls <- character()
  frac <- FALSE
  for (v in vals) {
    if (is.null(v)) next
    if (is.logical(v)) {
      cls <- union(cls, "l")
    } else if (is.numeric(v)) {
      cls <- union(cls, "n")
      if (!(is.finite(v) && v == trunc(v) && abs(v) <= 2^53)) frac <- TRUE
    } else {
      cls <- union(cls, "c")
    }
  }
  list(cls = cls, frac = frac)
}

# The type a column gets from its first values: BOOLEAN, BIGINT (whole numbers), DOUBLE, else VARCHAR.
# A column with no value yet is BIGINT; a later value that does not fit widens it.
.sr_json_kind <- function(vals) {
  s <- .sr_json_scan(vals)
  if (!length(s$cls)) return("BIGINT")
  if (identical(s$cls, "l")) return("BOOLEAN")
  if (identical(s$cls, "n")) return(if (s$frac) "DOUBLE" else "BIGINT")
  "VARCHAR"
}

# NULL when the values fit `type`, else the type the column widens to.
.sr_json_fits <- function(vals, type) {
  s <- .sr_json_scan(vals)
  if (!length(s$cls) || type == "VARCHAR") return(NULL)
  num <- identical(s$cls, "n")
  switch(type,
    BIGINT = if (num && !s$frac) NULL else if (num) "DOUBLE" else "VARCHAR",
    DOUBLE = if (num) NULL else "VARCHAR",
    BOOLEAN = if (identical(s$cls, "l")) NULL else "VARCHAR",
    "VARCHAR")
}

.sr_json_text <- function(v) {
  if (is.null(v)) return(NA_character_)
  if (is.character(v)) return(v)
  if (is.logical(v)) return(if (isTRUE(v)) "true" else "false")
  if (is.integer(v)) return(as.character(v))
  if (is.finite(v) && v == trunc(v) && abs(v) < 1e15) return(sprintf("%.0f", v))
  as.character(v)
}

# The values as an R vector for the column type (they were checked to fit).
.sr_json_conv <- function(vals, type) {
  switch(type,
    BIGINT = , DOUBLE = vapply(vals, function(v) if (is.null(v)) NA_real_ else as.numeric(v), numeric(1)),
    BOOLEAN = vapply(vals, function(v) if (is.null(v)) NA else isTRUE(v), logical(1)),
    vapply(vals, .sr_json_text, character(1)))
}

.sr_json_conflict <- function(widen, added) {
  structure(class = c("sr_type_conflict", "error", "condition"),
            list(message = "a value does not fit its column type", call = NULL, widen = widen, added = added))
}

# Apply a conflict: add the new columns, widen the others, delete the parts and start from the top.
.sr_json_apply <- function(ck, cnd) {
  for (a in cnd$added) {
    ck$columns[[length(ck$columns) + 1L]] <- list(name = a$name, type = a$type, fmt = NA_character_)
    ck$widen_log[[length(ck$widen_log) + 1L]] <- list(column = a$name, from = "(new column)", to = a$type,
                                                       at_offset = ck$next_offset)
  }
  if (length(cnd$widen)) {
    .sr_csv_widen(ck, cnd$widen)
  } else {
    ck$restarts <- ck$restarts + 1L
    if (ck$restarts > 2L * length(ck$columns)) {
      stop("too many type-conflict restarts (", ck$restarts, "): giving up", call. = FALSE)
    }
    .sr_ck_drop_parts(ck)
    .sr_ck_write(ck)
  }
  invisible(ck)
}

# Write the flattened records as one part with the checkpoint's column types. With no columns yet
# (the first records) they are inferred from `flat`. Throws a type conflict when a value does not fit
# or a new key appears after parts were committed. Returns the rows written, or NULL when there is
# nothing to write yet (no records and no columns).
.sr_json_part <- function(con, flat, ck, to) {
  keys <- unique(unlist(lapply(flat, names), use.names = FALSE))
  have <- vapply(ck$columns, function(cl) cl$name, "")
  if (length(keys)) .sr_col_check_names(keys, "the JSON records")
  get_vals <- function(k) lapply(flat, function(r) r[[k]])
  if (is.null(ck$columns)) {
    if (!length(flat)) return(NULL)
    if (!length(keys)) stop("the JSON records have no keys", call. = FALSE)
    vl <- stats::setNames(lapply(keys, get_vals), keys)
    ck$columns <- lapply(keys, function(k) list(name = k, type = .sr_json_kind(vl[[k]]), fmt = NA_character_))
    ck$type_widened <- character()
    .sr_ck_write(ck)
  } else {
    all_keys <- unique(c(have, keys))
    vl <- stats::setNames(lapply(all_keys, get_vals), all_keys)
    widen <- list()
    for (cl in ck$columns) {
      to_type <- .sr_json_fits(vl[[cl$name]], cl$type)
      if (!is.null(to_type)) widen[[length(widen) + 1L]] <- list(name = cl$name, from = cl$type, to = to_type)
    }
    added <- lapply(setdiff(keys, have), function(k) list(name = k, type = .sr_json_kind(vl[[k]])))
    if (length(widen) || length(added)) stop(.sr_json_conflict(widen, added))
  }
  cols <- ck$columns
  lst <- lapply(cols, function(cl) .sr_json_conv(vl[[cl$name]], cl$type))
  names(lst) <- vapply(cols, function(cl) cl$name, "")
  n <- length(flat)
  lst[[.sr_col_roword]] <- seq_len(n)
  df <- structure(lst, class = "data.frame", row.names = .set_row_names(n))
  sel <- vapply(cols, function(cl) {
    id <- DBI::dbQuoteIdentifier(con, cl$name)
    paste0(if (cl$type == "BIGINT") paste0("CAST(", id, " AS BIGINT)") else id, " AS ", id)
  }, character(1))
  .sr_col_with_reg(con, df, function(from) {
    .sr_col_copy(con, from, to, select = paste(sel, collapse = ", "),
                 order = DBI::dbQuoteIdentifier(con, .sr_col_roword))
  })
}

# ---- NDJSON ---------------------------------------------------------------------------------------

# The records of the NDJSON chunk file `chunk` (which starts at byte `start` of the source): LF or
# CRLF lines, blank lines skipped, exactly one JSON value per line.
.sr_json_chunk_records <- function(chunk, start) {
  txt <- rawToChar(readBin(chunk, "raw", n = file.size(chunk)))
  Encoding(txt) <- "UTF-8"
  if (!validUTF8(txt)) stop("the file is not valid UTF-8 near byte ", start, call. = FALSE)
  lines <- sub("\r$", "", strsplit(txt, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(trimws(lines))]
  if (!length(lines)) return(list())
  at <- paste0("in the lines starting at byte ", format(start, scientific = FALSE))
  flat <- .sr_json_records(paste0("[", paste(lines, collapse = ","), "]"), at)
  if (length(flat) != length(lines)) stop("each line must hold exactly one JSON value ", at, call. = FALSE)
  flat
}

.sr_json_run <- function(con, work, ck, size, chunk_bytes, progress, cancel) {
  bom <- if (size >= 3 && identical(readBin(work, "raw", n = 3L), as.raw(c(0xEF, 0xBB, 0xBF)))) 3 else 0
  if (ck$next_offset < bom) ck$next_offset <- bom
  chunk <- file.path(ck$ingest_dir, "chunk.csv")           # (the name the resume tidy-up removes)
  while (ck$next_offset < size) {
    start <- ck$next_offset
    end <- .sr_csv_write_chunk(work, start, start + chunk_bytes, NULL, raw(0), chunk)
    flat <- tryCatch(.sr_json_chunk_records(chunk, start), finally = unlink(chunk))
    to <- .sr_part_path(ck)
    rows <- .sr_json_part(con, flat, ck, to)
    ck$next_offset <- end
    if (is.null(rows)) {
      .sr_ck_write(ck)                                      # nothing but blank lines so far
    } else if (rows > 0 || !length(ck$parts)) {
      .sr_part_commit(ck, to, rows)
    } else {
      unlink(to)
      .sr_ck_write(ck)
    }
    if (!is.null(progress)) progress(end, size)
    if (end < size && cancel()) stop(.sr_ingest_cancelled())
  }
  invisible()
}

.sr_read_ndjson <- function(project, sniff, table, ck, options, progress, cancel) {
  con <- sr_duck(project)
  work <- .sr_json_work(sniff, ck)
  size <- as.numeric(file.size(work))
  chunk_bytes <- min(ck$chunk_bytes, .sr_json_chunk_max)
  repeat {
    out <- tryCatch({
      .sr_json_run(con, work, ck, size, chunk_bytes, progress, cancel)
      NULL
    }, sr_type_conflict = function(cnd) cnd)
    if (is.null(out)) break
    .sr_json_apply(ck, out)
  }
  if (!length(ck$parts)) stop("the file holds no JSON records", call. = FALSE)
  list(schema = .sr_col_schema(con, ck), resumable = size > chunk_bytes, type_widened = ck$type_widened)
}

# ---- JSON (one array of objects) ------------------------------------------------------------------

.sr_read_json <- function(project, sniff, table, ck, options, progress, cancel) {
  con <- sr_duck(project)
  work <- .sr_json_work(sniff, ck)
  size <- as.numeric(file.size(work))
  if (!length(ck$parts)) {
    if (cancel()) stop(.sr_ingest_cancelled())
    raw <- readBin(work, "raw", n = size)
    if (length(raw) >= 3L && identical(raw[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) raw <- raw[-(1:3)]
    txt <- rawToChar(raw)
    rm(raw)
    Encoding(txt) <- "UTF-8"
    if (!validUTF8(txt)) stop("the file is not valid UTF-8", call. = FALSE)
    top <- tryCatch(jsonlite::parse_json(txt, simplifyVector = FALSE, bigint_as_char = TRUE),
                    error = function(e) stop("invalid JSON: ", conditionMessage(e), call. = FALSE))
    rm(txt)
    if (!is.list(top) || !is.null(names(top)) ||
        !all(vapply(top, function(r) is.list(r) && !is.null(names(r)), logical(1)))) {
      stop("the JSON file must be an array of objects (for one object per line use NDJSON)", call. = FALSE)
    }
    if (!length(top)) stop("the JSON file holds no records", call. = FALSE)
    flat <- lapply(top, .sr_json_flat)
    rm(top)
    to <- .sr_part_path(ck)
    rows <- .sr_json_part(con, flat, ck, to)
    ck$next_offset <- size
    .sr_part_commit(ck, to, rows)
    if (!is.null(progress)) progress(size, size)
  }
  list(schema = .sr_col_schema(con, ck), resumable = FALSE, type_widened = character())
}

# ---- SQLite ---------------------------------------------------------------------------------------

# The SELECT expression for one column, chosen from the storage classes SQLite holds in it, so that
# RSQLite never has to coerce (it would turn a value it cannot coerce into NA, with a warning).
.sr_sqlite_expr <- function(db, name, declared, types) {
  q <- DBI::dbQuoteIdentifier(db, name)
  t <- setdiff(types, "null")
  datelike <- grepl("DATE|TIME", toupper(declared))
  if (length(t) <= 1L && !(identical(t, "text") && datelike) && !identical(t, "blob")) return(q)
  if (identical(t, "blob")) return(paste0("hex(", q, ")"))
  if (setequal(t, c("integer", "real"))) return(paste0("CAST(", q, " AS REAL)"))
  paste0("CASE typeof(", q, ") WHEN 'blob' THEN hex(", q, ") ELSE CAST(", q, " AS TEXT) END")
}

.sr_read_sqlite <- function(project, sniff, table, ck, options, progress, cancel) {
  if (!requireNamespace("RSQLite", quietly = TRUE)) stop("RSQLite package required for SQLite files", call. = FALSE)
  .sr_col_nocomp(sniff, "SQLite")
  con <- sr_duck(project)
  path <- normalizePath(sniff$path, winslash = "/", mustWork = TRUE)
  db <- DBI::dbConnect(RSQLite::SQLite(), path, flags = RSQLite::SQLITE_RO, extended_types = TRUE)
  on.exit(try(DBI::dbDisconnect(db), silent = TRUE), add = TRUE)
  ob <- DBI::dbGetQuery(db, "SELECT name FROM sqlite_master WHERE type IN ('table', 'view') ORDER BY name")$name
  ob <- ob[!startsWith(tolower(ob), "sqlite_")]
  tb <- .sr_col_choose(ob, options$table, "table", "SQLite file", "table")
  qt <- DBI::dbQuoteIdentifier(db, tb)
  info <- DBI::dbGetQuery(db, paste("PRAGMA table_info(", qt, ")"))
  .sr_col_check_names(info$name, "the SQLite table")
  chunk_rows <- options$chunk_rows %||% 500000
  if (!is.numeric(chunk_rows) || length(chunk_rows) != 1L || !is.finite(chunk_rows) || chunk_rows < 1) {
    stop("options$chunk_rows must be a positive number", call. = FALSE)
  }
  # a deterministic row order: rowid, else the primary key; a view or WITHOUT ROWID table with
  # neither is read in the order SQLite returns it
  has_rowid <- !inherits(try(DBI::dbGetQuery(db, paste("SELECT rowid FROM", qt, "LIMIT 0")), silent = TRUE), "try-error")
  pk <- info$name[info$pk > 0][order(info$pk[info$pk > 0])]
  ord <- if (has_rowid) " ORDER BY rowid" else if (length(pk)) paste(" ORDER BY", paste(DBI::dbQuoteIdentifier(db, pk), collapse = ", ")) else ""
  # storage classes per column, in one pass
  q <- DBI::dbQuoteIdentifier(db, info$name)
  scan <- DBI::dbGetQuery(db, sprintf("SELECT %s FROM %s", paste0("group_concat(DISTINCT typeof(", q, "))", collapse = ", "), qt))
  exprs <- vapply(seq_along(q), function(i) {
    types <- strsplit(as.character(scan[[i]]), ",", fixed = TRUE)[[1L]]
    types <- types[!is.na(types)]
    paste0(.sr_sqlite_expr(db, info$name[i], info$type[i], types), " AS ", q[i])
  }, character(1))
  total <- as.numeric(DBI::dbGetQuery(db, paste("SELECT count(*) AS n FROM", qt))$n[1L])
  off <- ck$next_offset
  repeat {
    if (length(ck$parts) && off >= total) break
    df <- withCallingHandlers(
      DBI::dbGetQuery(db, sprintf("SELECT %s FROM %s%s LIMIT %.0f OFFSET %.0f", paste(exprs, collapse = ", "), qt,
                                  ord, chunk_rows, off)),
      warning = function(w) stop("reading the SQLite table would lose data: ", conditionMessage(w), call. = FALSE))
    to <- .sr_part_path(ck)
    rows <- .sr_col_frame_part(con, df, to, "the SQLite table")
    off <- off + nrow(df)
    ck$next_offset <- off
    .sr_part_commit(ck, to, rows)
    if (!is.null(progress)) progress(off, total)                 # rows, not bytes
    if (off < total && cancel()) stop(.sr_ingest_cancelled())
    if (!nrow(df)) break
  }
  list(schema = .sr_col_schema(con, ck), resumable = total > chunk_rows, type_widened = character())
}

# ---- DuckDB ---------------------------------------------------------------------------------------

.sr_read_duckdb <- function(project, sniff, table, ck, options, progress, cancel) {
  .sr_col_nocomp(sniff, "DuckDB")
  .sr_col_single(project, sniff, ck, progress, cancel, function(con, to) {
    alias <- .sr_duck_attach(con, sniff$path)
    on.exit(.sr_duck_detach(con, alias), add = TRUE)
    lit <- function(x) DBI::dbQuoteString(con, x)
    t <- sr_duck_query(con, sprintf(paste("SELECT schema_name, table_name FROM duckdb_tables()",
                                          "WHERE database_name = %s AND NOT internal ORDER BY schema_name, table_name"),
                                    lit(alias)))
    shown <- ifelse(t$schema_name == "main", t$table_name, paste0(t$schema_name, ".", t$table_name))
    pick <- .sr_col_choose(shown, options$table, "table", "DuckDB file", "table")
    i <- match(pick, shown)
    from <- paste(DBI::dbQuoteIdentifier(con, alias), DBI::dbQuoteIdentifier(con, t$schema_name[i]),
                  DBI::dbQuoteIdentifier(con, t$table_name[i]), sep = ".")
    .sr_col_copy(con, from, to, order = "rowid")
  })
}
