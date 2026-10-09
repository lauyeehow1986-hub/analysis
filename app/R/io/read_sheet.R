# io/read_sheet.R - readers for Excel (xlsx, xls, xlsm) and for the statistical-package formats
# SPSS (sav, por), Stata (dta), SAS (sas7bdat) and SAS transport (xpt). They sit behind the reader
# contract of ingest.R:
#   function(project, sniff, table, ck, options, progress, cancel) -> list(schema, resumable, type_widened, ...)
# and reuse the data.frame -> part conversion of read_columnar.R (.sr_col_frame_part).
#
# Excel
#   * the sheet is read as text (readxl, col_names = FALSE, col_types = "text"), the first
#     `header_rows` rows are flattened into the column names, then each column is typed from its text;
#   * one part, resumable = FALSE; more than 1,048,576 rows is refused before any part is written.
# SPSS / Stata / SAS
#   * haven, chunked by row offset (skip / n_max) and resumable. The column types, and which columns
#     carry value labels, are fixed once from the n_max = 0 metadata read and every chunk is coerced to
#     them, so a chunk in which a labelled column is all NA still becomes text;
#   * value-labelled columns are stored as their labels (haven::as_factor(levels = "default"): a code
#     without a label keeps its value as text, so nothing turns into NA); other columns stay as read;
#   * SPSS user-missing values are read with user_na = TRUE, recorded, then set to NA (zap_missing);
#   * the variable labels, value labels and missing codes come back as res$dict_labels for the
#     data dictionary (sr_ingest threads them into `data.imported` and `dict.set`).

.sr_xl_max_rows <- 1048576            # the Excel row limit: refused above this, before anything is written
.sr_xl_epoch_1900 <- "1899-12-30"     # Excel's 1900 date system (with the 1900 leap-year quirk folded in)
.sr_xl_epoch_1904 <- "1904-01-01"

# ---- Excel ----------------------------------------------------------------------------------------

#' The sheet names of an Excel workbook (xlsx, xlsm or xls), for the UI's sheet picker.
sr_excel_sheets <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl package required for Excel files", call. = FALSE)
  chk <- .sr_sniff_check_path(path)
  readxl::excel_sheets(chk$path)
}

.sr_xl_whole <- function(x, what, min = 0) {
  if (!is.numeric(x) || length(x) != 1L || !is.finite(x) || x != floor(x) || x < min) {
    stop("options$", what, " must be a whole number >= ", min, call. = FALSE)
  }
  as.numeric(x)
}

# Validate the Excel options once; returns the cleaned list.
.sr_xl_options <- function(options, path) {
  sheet <- options$sheet
  if (!is.null(sheet)) {
    if (!((is.character(sheet) || is.numeric(sheet)) && length(sheet) == 1L && !is.na(sheet))) {
      stop("options$sheet must be a sheet name or number", call. = FALSE)
    }
    nm <- readxl::excel_sheets(path)
    if (is.character(sheet) && !(sheet %in% nm)) {
      stop("sheet '", sheet, "' is not in the workbook (", paste(nm, collapse = ", "), ")", call. = FALSE)
    }
    if (is.numeric(sheet) && (sheet < 1 || sheet > length(nm) || sheet != floor(sheet))) {
      stop("sheet ", sheet, " is not in the workbook (it has ", length(nm), " sheets)", call. = FALSE)
    }
  }
  range <- options$range
  if (!is.null(range) && !(is.character(range) && length(range) == 1L && !is.na(range) && nzchar(range))) {
    stop("options$range must be a single string such as \"B3:F40\"", call. = FALSE)
  }
  nullstr <- options$nullstr %||% c("", "NA")    # the same default as the CSV reader
  if (!is.character(nullstr) || anyNA(nullstr)) stop("options$nullstr must be a character vector", call. = FALSE)
  list(sheet = sheet, range = range, nullstr = nullstr,
       header_rows = .sr_xl_whole(options$header_rows %||% 1L, "header_rows", 1),
       skip = .sr_xl_whole(options$skip %||% 0L, "skip", 0),
       guess_max = .sr_xl_whole(options$guess_max %||% 1000, "guess_max", 1),
       max_rows = .sr_xl_whole(options$max_rows %||% .sr_xl_max_rows, "max_rows", 1))
}

# The date system of an xlsx workbook: "1904-01-01" when workbook.xml says date1904, else the 1900 system.
.sr_xl_epoch <- function(path) {
  wb <- tryCatch({
    z <- unz(path, "xl/workbook.xml")
    on.exit(close(z), add = TRUE)
    paste(readLines(z, warn = FALSE, encoding = "UTF-8"), collapse = "")
  }, error = function(e) "", warning = function(w) "")
  if (grepl("date1904=\"(1|true)\"", wb)) .sr_xl_epoch_1904 else .sr_xl_epoch_1900
}

# Flatten the header rows `h` (a character matrix, k x ncol, "" for an empty cell) into column names.
# An empty cell in a spanning header row (every header row but the last) takes the value to its left,
# which undoes a merged header; the last row names the column itself, so an empty cell there is left
# empty. The non-empty parts are pasted with "_", names are made unique with make.unique(sep = "_")
# case-insensitively (DuckDB identifiers are), so "ID" and "id" become "ID" and "id_1".
.sr_xl_names <- function(h) {
  k <- nrow(h)
  if (k > 1L && ncol(h) > 1L) {
    for (r in seq_len(k - 1L)) {
      for (j in 2:ncol(h)) if (!nzchar(h[r, j])) h[r, j] <- h[r, j - 1L]
    }
  }
  nm <- vapply(seq_len(ncol(h)), function(j) {
    parts <- h[, j]
    parts <- parts[nzchar(parts)]
    if (length(parts)) paste(parts, collapse = "_") else paste0("column_", j)
  }, character(1))
  .sr_xl_unique(nm)
}

# make.unique(sep = "_") that ignores case; a generated name never collides with an existing one.
.sr_xl_unique <- function(nm) {
  out <- nm
  seen <- character()
  for (i in seq_along(nm)) {
    cand <- nm[i]
    n <- 0L
    while (tolower(cand) %in% seen) {
      n <- n + 1L
      cand <- paste0(nm[i], "_", n)
    }
    seen <- c(seen, tolower(cand))
    out[i] <- cand
  }
  out
}

# Which columns hold dates or date-times? Reads the same window as the text read (the same `skip`, or
# the same `range`), because `skip` is only a lower bound: leading blank rows are dropped as well, so a
# read shifted by header_rows would look at the wrong rows. col_types = "list" keeps every cell's own
# type (a date cell is POSIXct); the first `header_rows` rows are the header, the next `guess_max` rows
# are checked. A column is a date when it has a cell and every non-empty cell is a date. A failure is a
# warning and no column is converted.
.sr_xl_guess_dates <- function(path, o, ncol) {
  none <- rep(FALSE, ncol)
  tryCatch({
    args <- list(path = path, sheet = o$sheet, col_names = FALSE, col_types = "list",
                 .name_repair = "minimal", progress = FALSE)
    if (is.null(o$range)) {
      args$skip <- o$skip
      args$n_max <- o$header_rows + o$guess_max
    } else {
      args$range <- o$range                  # n_max is ignored with a range: the rows are cut below
    }
    g <- suppressMessages(do.call(readxl::read_excel, args))
    rows <- seq_len(nrow(g))
    rows <- rows[rows > o$header_rows & rows <= o$header_rows + o$guess_max]
    if (!length(rows)) return(none)
    flags <- vapply(g, function(x) {
      cells <- x[rows]
      cells <- cells[!vapply(cells, function(v) length(v) == 0L || (length(v) == 1L && is.na(v)), logical(1))]   # blank cells
      length(cells) > 0L && all(vapply(cells, function(v) inherits(v, "POSIXct") || inherits(v, "Date"), logical(1)))
    }, logical(1))
    out <- none
    m <- min(ncol, length(flags))
    out[seq_len(m)] <- flags[seq_len(m)]
    out
  }, error = function(e) {
    warning("could not check the Excel sheet for date columns: ", conditionMessage(e),
            "; date serials stay as numbers", call. = FALSE)
    none
  })
}

# Text that must not be turned into a number: an identifier with a leading zero ("007", but not "0" or
# "0.5"), an integer with more than 15 digits (it does not survive a double), or a hexadecimal string.
.sr_xl_identifier_like <- function(x) {
  any(grepl("^[+-]?0[0-9]", x)) || any(grepl("^[+-]?[0-9]{16,}$", x)) || any(grepl("^[+-]?0[xX]", x))
}

# Type one text column. `is_date`: readxl guessed a date / date-time here, so the text is an Excel serial.
.sr_xl_type_col <- function(x, is_date, epoch) {
  nn <- x[!is.na(x)]
  if (!length(nn)) return(x)
  if (is_date) {
    s <- suppressWarnings(as.numeric(nn))
    if (!anyNA(s) && all(is.finite(s))) {
      v <- suppressWarnings(as.numeric(x))
      if (all(v == floor(v), na.rm = TRUE)) return(as.Date(v, origin = epoch))
      return(as.POSIXct(round(v * 86400), origin = epoch, tz = "UTC"))
    }
    return(x)               # a text cell among the serials: keep the column as text
  }
  if (.sr_xl_identifier_like(nn)) return(x)
  y <- utils::type.convert(x, as.is = TRUE, na.strings = character())   # null strings are NA already
  if (is.logical(y) && !all(nn %in% c("TRUE", "FALSE"))) return(x)    # "T" / "F" are text, not Booleans
  y
}

# The sheet as a typed data frame, or an error (the row cap, a sheet with no rows, too few header rows).
.sr_xl_frame <- function(path, o) {
  args <- list(path = path, sheet = o$sheet, col_names = FALSE, col_types = "text",
               .name_repair = "minimal", progress = FALSE)
  if (is.null(o$range)) {
    args$skip <- o$skip
    args$n_max <- o$max_rows + 1           # read one row past the cap: enough to see that it is exceeded
  } else {
    args$range <- o$range
  }
  txt <- suppressMessages(do.call(readxl::read_excel, args))
  n <- nrow(txt)
  if (n > o$max_rows) {
    stop(sprintf(paste0("the sheet has more than %s rows, the most this reader accepts; export the sheet to ",
                        "CSV and ingest the CSV instead"), format(o$max_rows, big.mark = ",", scientific = FALSE)),
         call. = FALSE)
  }
  if (!ncol(txt) || !n) stop("the Excel sheet is empty", call. = FALSE)
  k <- o$header_rows
  if (n < k) stop("the sheet has ", n, " rows, fewer than header_rows = ", k, call. = FALSE)
  cols <- lapply(txt, as.character)
  h <- vapply(cols, function(x) trimws(replace(x[seq_len(k)], is.na(x[seq_len(k)]), "")), character(k))
  if (k == 1L) h <- matrix(h, nrow = 1L)
  nm <- .sr_xl_names(h)
  body <- lapply(cols, function(x) {
    x <- x[-seq_len(k)]
    x[x %in% o$nullstr] <- NA_character_      # "" and "NA" are null, as in the CSV reader
    x
  })
  guess <- if (n > k) .sr_xl_guess_dates(path, o, length(cols)) else rep(FALSE, length(cols))
  epoch <- .sr_xl_epoch(path)
  out <- lapply(seq_along(body), function(j) {
    if (!length(body[[j]])) character() else .sr_xl_type_col(body[[j]], guess[j], epoch)
  })
  names(out) <- nm
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
}

.sr_read_xlsx <- function(project, sniff, table, ck, options, progress, cancel) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("readxl package required for Excel files", call. = FALSE)
  .sr_col_nocomp(sniff, "Excel")
  path <- normalizePath(sniff$path, winslash = "/", mustWork = TRUE)
  o <- .sr_xl_options(options, path)
  .sr_col_single(project, sniff, ck, progress, cancel, function(con, to) {
    df <- .sr_xl_frame(path, o)
    .sr_col_frame_part(con, df, to, "the Excel sheet")
  })
}

# ---- SPSS, Stata, SAS -----------------------------------------------------------------------------

# The haven reader of a format: function(skip, n_max) -> tibble.
.sr_hv_reader <- function(fmt, path, options) {
  enc <- options$encoding
  if (!is.null(enc) && !(is.character(enc) && length(enc) == 1L && !is.na(enc))) {
    stop("options$encoding must be a single string", call. = FALSE)
  }
  switch(fmt,
    sav = function(skip, n_max) haven::read_sav(path, encoding = enc, user_na = TRUE, skip = skip,
                                                n_max = n_max, .name_repair = "minimal"),
    por = function(skip, n_max) haven::read_por(path, user_na = TRUE, skip = skip, n_max = n_max,
                                                .name_repair = "minimal"),
    dta = function(skip, n_max) haven::read_dta(path, encoding = enc, skip = skip, n_max = n_max,
                                                .name_repair = "minimal"),
    sas7bdat = function(skip, n_max) haven::read_sas(path, encoding = enc, skip = skip, n_max = n_max,
                                                      .name_repair = "minimal"),
    xpt = function(skip, n_max) haven::read_xpt(path, skip = skip, n_max = n_max, .name_repair = "minimal"),
    stop("no statistical-package reader for format ", fmt, call. = FALSE))
}

# A code as text without scientific notation ("100000", not "1e+05").
.sr_hv_code <- function(x) {
  if (is.character(x)) return(x)
  x <- as.numeric(x)
  whole <- is.finite(x) & x == floor(x) & abs(x) < 1e15
  ifelse(whole, sprintf("%.0f", x), as.character(x))
}

# What the metadata read says about every column: its target kind, value labels, user-missing values
# and variable label. Fixed once; every chunk is coerced to it.
.sr_hv_spec <- function(meta) {
  lapply(meta, function(x) {
    lab <- attr(x, "label", exact = TRUE)
    lab <- if (is.character(lab) && length(lab) == 1L && !is.na(lab) && nzchar(lab)) lab else NA_character_
    labels <- attr(x, "labels", exact = TRUE)
    if (!length(labels)) labels <- NULL
    nav <- attr(x, "na_values", exact = TRUE)
    nar <- attr(x, "na_range", exact = TRUE)
    kind <- if (!is.null(labels)) "labelled"
            else if (inherits(x, "POSIXct")) "posixct"
            else if (inherits(x, "Date")) "date"
            else if (inherits(x, "difftime")) "time"
            else if (is.character(x)) "character"
            else if (is.integer(x)) "integer"
            else if (is.logical(x)) "logical"
            else "double"
    tz <- if (kind == "posixct") attr(x, "tzone", exact = TRUE) else NULL
    tz_set <- length(tz) > 0L && !is.na(tz[1L]) && nzchar(tz[1L])
    list(kind = kind, label = lab, labels = labels, na_values = nav, na_range = nar,
         tz = if (tz_set) as.character(tz[1L]) else "UTC", tz_set = tz_set)
  })
}

# The dictionary entry of a column: variable label, value labels (code, label, order), missing codes.
# NULL when the column carries none of them.
.sr_hv_dict_entry <- function(sp) {
  vals <- NULL
  if (!is.null(sp$labels)) {
    vals <- data.frame(code = .sr_hv_code(unname(sp$labels)), label = names(sp$labels),
                       order = seq_along(sp$labels), stringsAsFactors = FALSE)
  }
  miss <- if (length(sp$na_values)) .sr_hv_code(sp$na_values) else NULL
  rng <- if (length(sp$na_range) == 2L) .sr_hv_code(sp$na_range) else NULL
  if (is.na(sp$label) && is.null(vals) && is.null(miss) && is.null(rng)) return(NULL)
  e <- list(label = sp$label, values = vals, missing_codes = miss, kind = sp$kind)
  if (!is.null(rng)) e$missing_range <- rng
  e
}

# Coerce one chunk column to the kind fixed by the metadata.
.sr_hv_coerce <- function(x, sp) {
  if (inherits(x, "haven_labelled_spss")) x <- haven::zap_missing(x)
  kind <- sp$kind
  if (kind %in% c("date", "posixct", "time")) {
    if (kind == "date") return(as.Date(as.numeric(unclass(x)), origin = "1970-01-01"))
    if (kind == "posixct") {
      return(structure(as.numeric(unclass(x)), class = c("POSIXct", "POSIXt"), tzone = sp$tz))
    }
    return(x)
  }
  v <- if (inherits(x, "haven_labelled")) haven::zap_labels(x) else x
  attributes(v) <- NULL
  if (kind == "character") return(as.character(v))
  if (kind == "logical") return(as.logical(v))
  if (!is.character(v)) {                       # user-missing values the chunk still carries
    v <- as.numeric(v)
    if (length(sp$na_values)) v[v %in% as.numeric(sp$na_values)] <- NA
    if (length(sp$na_range) == 2L) v[!is.na(v) & v >= sp$na_range[1L] & v <= sp$na_range[2L]] <- NA
  }
  if (kind == "integer") return(as.integer(v))
  if (kind == "double") return(v)
  # labelled: the labels as text; a code without a label keeps its value (levels = "default"), and a
  # whole-number code is held as an integer so that 100000 reads "100000", not "1e+05"
  lb <- sp$labels
  if (is.character(v)) {
    lb <- stats::setNames(as.character(unname(lb)), names(lb))
  } else if (all(is.na(v) | (v == floor(v) & abs(v) < 2^31)) &&
             all(unname(lb) == floor(unname(lb)) & abs(unname(lb)) < 2^31)) {
    v <- as.integer(v)
    lb <- stats::setNames(as.integer(unname(lb)), names(lb))
  } else {
    lb <- stats::setNames(as.numeric(unname(lb)), names(lb))
  }
  as.character(haven::as_factor(haven::labelled(v, labels = lb), levels = "default"))
}

.sr_read_haven <- function(project, sniff, table, ck, options, progress, cancel) {
  if (!requireNamespace("haven", quietly = TRUE)) stop("haven package required for SPSS, Stata and SAS files", call. = FALSE)
  .sr_col_nocomp(sniff, "this file")
  con <- sr_duck(project)
  path <- normalizePath(sniff$path, winslash = "/", mustWork = TRUE)
  chunk_rows <- options$chunk_rows %||% 500000
  if (!is.numeric(chunk_rows) || length(chunk_rows) != 1L || !is.finite(chunk_rows) || chunk_rows < 1) {
    stop("options$chunk_rows must be a positive number", call. = FALSE)
  }
  rd <- .sr_hv_reader(sniff$format, path, options)
  meta <- rd(0, 0)
  .sr_col_check_names(names(meta), "the file")
  spec <- .sr_hv_spec(meta)
  dict <- Filter(Negate(is.null), lapply(spec, .sr_hv_dict_entry))
  off <- ck$next_offset
  repeat {
    df <- rd(off, chunk_rows)
    n <- nrow(df)
    if (n == 0L && length(ck$parts)) break
    cols <- lapply(seq_along(df), function(i) .sr_hv_coerce(df[[i]], spec[[i]]))
    names(cols) <- names(df)
    out <- as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
    to <- .sr_part_path(ck)
    rows <- as.numeric(.sr_col_frame_part(con, out, to, "the file"))
    off <- off + n
    ck$next_offset <- off
    .sr_part_commit(ck, to, rows)
    if (!is.null(progress)) progress(off, NA_real_)       # rows read; the total is not known up front
    if (n < chunk_rows) break
    if (cancel()) stop(.sr_ingest_cancelled())
  }
  tz <- list()
  for (nm in names(spec)) if (spec[[nm]]$kind == "posixct") tz[[nm]] <- if (spec[[nm]]$tz_set) spec[[nm]]$tz else "(unset)"
  res <- list(schema = .sr_col_schema(con, ck), resumable = length(ck$parts) > 1L, type_widened = character(),
              dict_labels = if (length(dict)) dict else NULL)
  if (length(tz)) res$source_tz <- tz
  res
}
