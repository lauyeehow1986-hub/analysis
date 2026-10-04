# Format, encoding and dialect sniffing for data files. Cheap by design: at most `sample_bytes` of
# the (decompressed) head is read, never the whole file. Nothing here touches the network: a path
# that looks like a URL is refused before any read.

.sr_sniff_url_re <- "^[A-Za-z][A-Za-z0-9+.-]*://"
.sr_sniff_compressions <- c("gz", "bz2", "xz", "zip")
.sr_sniff_delims <- c(",", ";", "\t", "|")

# Formats that carry magic bytes the extension must not override (a file whose extension promises
# one of these but whose bytes do not is sniffed as text, with a warning).
.sr_sniff_magic_exts <- c("parquet", "sqlite", "duckdb", "sav", "xlsx", "xlsm")

# Formats with no (cheap) magic bytes: the extension decides.
.sr_sniff_ext_formats <- c(
  dta = "dta", sas7bdat = "sas7bdat", xpt = "xpt", por = "por", rds = "rds", rdata = "rdata",
  rda = "rdata", xls = "xls", fst = "fst", feather = "feather"
)

#' Is `path` a Windows UNC path (a network share)?
.sr_is_unc <- function(path) grepl("^(\\\\\\\\|//)[^\\\\/]", path)

#' Does `b` carry `bytes` at 0-based `offset`?
.sr_has <- function(b, bytes, offset = 0L) {
  n <- length(bytes)
  length(b) >= offset + n && all(b[offset + seq_len(n)] == bytes)
}

#' Identify a format from the leading bytes `b` (raw). Returns one of the container or format
#' names the magic bytes decide (parquet, feather, sqlite, duckdb, zip, gz, bz2, xz, sav, xls,
#' dta, sas7bdat, xpt, rds, rdata, odm), or NA when the bytes carry no recognised magic.
.sr_magic_bytes <- function(b) {
  has <- function(s, offset = 0L) .sr_has(b, if (is.character(s)) charToRaw(s) else as.raw(s), offset)
  if (has("PAR1")) return("parquet")
  if (has("ARROW1")) return("feather")
  if (has(c(charToRaw("SQLite format 3"), 0L))) return("sqlite")
  if (has("DUCK", 8L)) return("duckdb")
  if (has(c(0x50, 0x4B, 0x03, 0x04))) return("zip")
  if (has(c(0x1F, 0x8B))) return("gz")
  if (has("BZh")) return("bz2")
  if (has(c(0xFD, 0x37, 0x7A, 0x58, 0x5A, 0x00))) return("xz")
  if (has("$FL2") || has("$FL3")) return("sav")
  if (has(c(0xD0, 0xCF, 0x11, 0xE0, 0xA1, 0xB1, 0x1A, 0xE1))) return("xls")
  if (has("<stata_dta>")) return("dta")
  if (has(c(rep(0L, 12L), 0xC2, 0xEA, 0x81, 0x60))) return("sas7bdat")
  if (has("HEADER RECORD*******LIBRARY HEADER RECORD")) return("xpt")
  if (has(c(0x58, 0x0A, 0, 0, 0, 0x02)) || has(c(0x58, 0x0A, 0, 0, 0, 0x03))) return("rds")
  if (any(vapply(c("RDX2\n", "RDX3\n", "RDA2\n", "RDA3\n", "RDB2\n", "RDB3\n"), has, logical(1)))) {
    return("rdata")
  }
  # An <ODM root element, after an optional BOM, XML declaration and comments.
  start <- if (has(c(0xEF, 0xBB, 0xBF))) 4L else 1L
  if (length(b) >= start && !any(b == 0L)) {
    txt <- rawToChar(b[start:length(b)])
    if (grepl("^\\s*<", txt, perl = TRUE, useBytes = TRUE) &&
        grepl("(?s)^\\s*(<\\?xml[^>]*\\?>\\s*)?(<!--.*?-->\\s*)*<(\\w+:)?ODM[\\s>/]", txt,
              perl = TRUE, useBytes = TRUE)) {
      return("odm")
    }
  }
  NA_character_
}

#' Read the first `n` bytes of `path` and identify the format from the magic bytes.
.sr_magic <- function(path, n = 4096L) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  .sr_magic_bytes(readBin(con, "raw", n = n))
}

#' Lower-case extension of `path`.
.sr_ext <- function(path) tolower(tools::file_ext(path))

#' Extension of the data file itself: a compression suffix (data.csv.gz) is looked through.
.sr_ext_inner <- function(path) {
  ext <- .sr_ext(path)
  if (ext %in% .sr_sniff_compressions) ext <- .sr_ext(tools::file_path_sans_ext(path))
  ext
}

#' Validate and normalise a path for sniffing. Refuses URLs, missing paths and directories;
#' reports a UNC path in `unc`.
.sr_sniff_check_path <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("path must be a single non-empty string", call. = FALSE)
  }
  if (grepl(.sr_sniff_url_re, path)) {
    stop("refusing to read a URL: only local files can be read", call. = FALSE)
  }
  if (dir.exists(path)) stop("path is a directory, not a file", call. = FALSE)
  if (!file.exists(path)) stop("file not found", call. = FALSE)
  list(path = normalizePath(path, winslash = "/", mustWork = TRUE), unc = .sr_is_unc(path))
}

#' Open a (possibly compressed) file for reading with `mode`. `member` selects a zip member.
.sr_sniff_conn <- function(path, compression, member, mode, encoding = "native.enc") {
  switch(compression,
    none = file(path, mode, encoding = encoding),
    gz = gzfile(path, mode, encoding = encoding),
    bz2 = bzfile(path, mode, encoding = encoding),
    xz = xzfile(path, mode, encoding = encoding),
    zip = unz(path, member, mode, encoding = encoding),
    stop("unknown compression: ", compression, call. = FALSE)
  )
}

#' The first `n` bytes of the decompressed content (fewer when the content is shorter).
.sr_sniff_head <- function(path, compression, member, n) {
  con <- .sr_sniff_conn(path, compression, member, "rb")
  on.exit(close(con), add = TRUE)
  out <- list()
  got <- 0L
  while (got < n) {
    chunk <- readBin(con, "raw", n = n - got)
    if (!length(chunk)) break
    out[[length(out) + 1L]] <- chunk
    got <- got + length(chunk)
  }
  if (length(out)) do.call(c, out) else raw(0)
}

#' Drop an incomplete trailing UTF-8 sequence (a sample cut mid-character).
.sr_trim_utf8_tail <- function(b) {
  n <- length(b)
  for (k in seq_len(min(3L, n))) {
    v <- as.integer(b[n - k + 1L])
    if (v < 0x80L) break
    if (v >= 0xC0L) {
      need <- if (v >= 0xF0L) 4L else if (v >= 0xE0L) 3L else 2L
      if (k < need) return(b[seq_len(n - k)])
      break
    }
  }
  b
}

#' Decide the encoding of the head bytes `b`. A BOM decides first; otherwise valid UTF-8 is
#' UTF-8 and anything else latin1. Returns the encoding, whether a BOM was found, the bytes after
#' the BOM and any warnings.
.sr_sniff_encoding <- function(b, truncated) {
  warnings <- character()
  bom <- TRUE
  if (.sr_has(b, as.raw(c(0xEF, 0xBB, 0xBF)))) {
    enc <- "UTF-8"
    b <- b[-(1:3)]
  } else if (.sr_has(b, as.raw(c(0xFF, 0xFE)))) {
    enc <- "UTF-16LE"
    b <- b[-(1:2)]
  } else if (.sr_has(b, as.raw(c(0xFE, 0xFF)))) {
    enc <- "UTF-16BE"
    b <- b[-(1:2)]
  } else {
    bom <- FALSE
    enc <- NA_character_
  }
  if (is.na(enc) || enc == "UTF-8") {
    # NULs cannot sit in an R string. They are dropped for sniffing only.
    if (any(b == 0L)) {
      warnings <- "file contains NUL bytes (dropped for sniffing): it may be UTF-16 without a BOM, or not a text file"
      b <- b[b != 0L]
    }
  }
  if (is.na(enc)) {
    probe <- if (truncated) .sr_trim_utf8_tail(b) else b
    enc <- if (validUTF8(rawToChar(probe))) "UTF-8" else "latin1"
  }
  list(encoding = enc, bom = bom, body = b, warnings = warnings)
}

#' Decode the head bytes `b` (BOM already removed) to one UTF-8 string.
.sr_sniff_decode <- function(b, enc, truncated) {
  if (enc == "UTF-8") {
    if (truncated) b <- .sr_trim_utf8_tail(b)
    txt <- rawToChar(b)
    Encoding(txt) <- "UTF-8"
    return(txt)
  }
  if (enc == "latin1") return(iconv(rawToChar(b), "latin1", "UTF-8"))
  if (length(b) %% 2L == 1L) b <- b[-length(b)]
  txt <- iconv(list(b), enc, "UTF-8")
  if (is.na(txt) && length(b) >= 2L) txt <- iconv(list(b[seq_len(length(b) - 2L)]), enc, "UTF-8")
  if (is.na(txt)) "" else txt
}

#' Cut the text into records and fields. Returns the logical flags and the 50-record
#' delimiter evidence the dialect decision needs.
.sr_sniff_scan <- function(txt, truncated) {
  chars <- utf8ToInt(txt)
  chars <- chars[!is.na(chars)]
  inq <- (cumsum(chars == 34L) %% 2L) == 1L
  nl_out <- chars == 10L & !inq
  if (truncated && any(nl_out)) {
    cut <- max(which(nl_out))
    chars <- chars[seq_len(cut)]
    inq <- inq[seq_len(cut)]
    nl_out <- nl_out[seq_len(cut)]
  }
  list(chars = chars, inq = inq, nl_out = nl_out, rec = cumsum(nl_out),
       quoted_newlines = any(chars == 10L & inq))
}

#' Pick the delimiter among , ; tab | : the candidate with the most consistent non-zero count per
#' record over the first 50 records, counted outside quotes. A file with none is one column.
.sr_sniff_delim <- function(sc) {
  n <- length(sc$chars)
  if (n == 0L) return(",")
  first <- sc$rec < 50L
  nrec <- min(50L, if (sc$nl_out[n]) sc$rec[n] else sc$rec[n] + 1L)
  if (nrec < 1L) return(",")
  filled <- sc$chars != 10L & sc$chars != 13L & first
  nonblank <- tabulate(sc$rec[filled] + 1L, nbins = nrec) > 0L
  best <- ","
  best_score <- c(0, 0)
  for (d in .sr_sniff_delims) {
    pos <- which(sc$chars == utf8ToInt(d) & !sc$inq & first)
    counts <- tabulate(sc$rec[pos] + 1L, nbins = nrec)[nonblank]
    nz <- counts[counts > 0L]
    if (!length(nz)) next
    tab <- table(nz)
    modal <- as.integer(names(tab)[which.max(tab)])
    score <- c(mean(counts == modal), modal)
    if (score[1L] > best_score[1L] || (score[1L] == best_score[1L] && score[2L] > best_score[2L])) {
      best <- d
      best_score <- score
    }
  }
  best
}

#' Tokenise the scanned text into a character matrix (records x fields, quotes removed), reading at
#' most `max_rec` records. Returns a 0 x 0 matrix for an empty sample.
.sr_sniff_fields <- function(sc, delim, max_rec = 5000L) {
  keep <- sc$rec < max_rec
  s <- sc$chars[keep]
  s[!sc$inq[keep] & s == utf8ToInt(delim)] <- 1L
  s[sc$nl_out[keep]] <- 2L
  recs <- strsplit(intToUtf8(s), "\002", fixed = TRUE)[[1L]]
  recs <- sub("\r$", "", recs)
  recs <- recs[nzchar(trimws(recs))]
  if (!length(recs)) return(matrix(character(), 0L, 0L))
  fl <- strsplit(paste0(recs, "\001"), "\001", fixed = TRUE)
  len <- lengths(fl)
  flat <- unlist(fl, use.names = FALSE)
  if (any(grepl("\"", flat, fixed = TRUE))) {
    q <- grepl("(?s)^\".*\"$", flat, perl = TRUE)
    flat[q] <- gsub("\"\"", "\"", substr(flat[q], 2L, nchar(flat[q]) - 1L), fixed = TRUE)
  }
  m <- matrix(NA_character_, nrow = length(fl), ncol = max(len))
  m[cbind(rep(seq_along(len), len), sequence(len))] <- flat
  m
}

.sr_sniff_num_re <- "^[-+]?([0-9]+([.,][0-9]*)?|[.,][0-9]+)([eE][-+]?[0-9]+)?$"
.sr_sniff_dmy_re <- "^[0-9]{1,2}[/.-][0-9]{1,2}[/.-][0-9]{2,4}([ T].*)?$"
.sr_sniff_ymd_re <- "^[0-9]{4}[/.-][0-9]{1,2}[/.-][0-9]{1,2}([ T].*)?$"

.sr_sniff_data_like <- function(x) {
  x <- trimws(x)
  grepl(.sr_sniff_num_re, x) | grepl(.sr_sniff_dmy_re, x) | grepl(.sr_sniff_ymd_re, x)
}

#' A header row exists unless a column whose other values look like data (numbers or dates)
#' starts with a value that looks like data too. A one-row file has a header unless that row
#' looks like data.
.sr_sniff_header <- function(m) {
  if (!nrow(m)) return(FALSE)
  first <- m[1L, ]
  if (nrow(m) == 1L) return(!any(.sr_sniff_data_like(first[!is.na(first)])))
  for (j in seq_len(ncol(m))) {
    rv <- trimws(m[-1L, j])
    rv <- rv[!is.na(rv) & nzchar(rv)]
    if (length(rv) && mean(.sr_sniff_data_like(rv)) >= 0.9 && isTRUE(.sr_sniff_data_like(first[j]))) {
      return(FALSE)
    }
  }
  TRUE
}

#' Date order per column: ymd, or dmy / mdy when some value decides it, else ambiguous (no value
#' decides it, or the column mixes day-first and month-first values), with a warning naming the
#' column. Only columns where at least 90% of the non-empty values look like dates are reported.
.sr_sniff_dates <- function(data, names) {
  out <- stats::setNames(character(), character())
  warnings <- character()
  for (j in seq_len(ncol(data))) {
    v <- trimws(data[, j])
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) next
    nm <- names[j]
    is_dmy <- grepl(.sr_sniff_dmy_re, v)
    if (mean(is_dmy) >= 0.9) {
      v <- v[is_dmy]
      f1 <- as.integer(sub("^([0-9]+)[/.-].*$", "\\1", v))
      f2 <- as.integer(sub("^[0-9]+[/.-]([0-9]+)[/.-].*$", "\\1", v))
      day_first <- any(f1 > 12L)
      month_first <- any(f2 > 12L)
      if (day_first && month_first) {
        out[nm] <- "ambiguous"
        warnings <- c(warnings, sprintf(
          "date column '%s' mixes day-first and month-first values (a first field > 12 and a second field > 12); its order is not guessed", nm))
      } else if (day_first) {
        out[nm] <- "dmy"
      } else if (month_first) {
        out[nm] <- "mdy"
      } else {
        out[nm] <- "ambiguous"
        warnings <- c(warnings, sprintf(
          "date column '%s' is ambiguous: no value shows whether the day or the month comes first (all first and second fields <= 12); its order is not guessed", nm))
      }
    } else if (mean(grepl(.sr_sniff_ymd_re, v)) >= 0.9) {
      out[nm] <- "ymd"
    }
  }
  list(date_order = out, warnings = warnings)
}

#' Dialect of delimited text: delimiter, decimal mark, header, date orders, quoted newlines.
.sr_sniff_dialect <- function(txt, truncated) {
  sc <- .sr_sniff_scan(txt, truncated)
  delim <- .sr_sniff_delim(sc)
  m <- .sr_sniff_fields(sc, delim)
  header <- .sr_sniff_header(m)
  data <- if (header && nrow(m) >= 1L) m[-1L, , drop = FALSE] else m
  decimal <- "."
  if (delim == ";" && length(data)) {
    v <- trimws(as.vector(data))
    if (sum(grepl("^-?[0-9]+,[0-9]+$", v)) > sum(grepl("^-?[0-9]+[.][0-9]+$", v))) decimal <- ","
  }
  dates <- list(date_order = stats::setNames(character(), character()), warnings = character())
  if (nrow(data) && ncol(data)) {
    nm <- if (header) m[1L, ] else paste0("V", seq_len(ncol(m)))
    blank <- is.na(nm) | !nzchar(nm)
    nm[blank] <- paste0("V", seq_len(ncol(m)))[blank]
    dates <- .sr_sniff_dates(data, nm)
  }
  list(delim = delim, quote = "\"", decimal = decimal, header = header,
       date_order = dates$date_order, quoted_newlines = sc$quoted_newlines,
       warnings = dates$warnings)
}

#' json or ndjson from the first non-space character and the line structure: a top-level array
#' or a pretty-printed object is json; several lines that are each a whole object is ndjson.
.sr_sniff_json_kind <- function(txt, truncated) {
  lines <- trimws(strsplit(txt, "\n", fixed = TRUE)[[1L]])
  lines <- lines[nzchar(lines)]
  if (truncated && length(lines) > 1L) lines <- lines[-length(lines)]
  if (!length(lines)) return(NA_character_)
  if (startsWith(lines[1L], "[")) return("json")
  if (!startsWith(lines[1L], "{")) return(NA_character_)
  lines <- utils::head(lines, 20L)
  if (length(lines) >= 2L && all(startsWith(lines, "{") & endsWith(lines, "}"))) "ndjson" else "json"
}

#' Sniff a data file's format, compression, encoding and text dialect from at most `sample_bytes`
#' of its head (minimum 64). Magic bytes win over the extension; formats without magic fall back to
#' the extension. Compressed text is sniffed on its decompressed head; a zip with several members
#' returns `zip_members` and `format = NA` until the caller passes `member`. A gzip/bzip2/xz
#' container that holds rds or RData is reported as rds / rdata (with the container compression).
#' Refuses URLs, missing paths and directories.
sr_sniff <- function(path, sample_bytes = 1048576L, member = NULL) {
  chk <- .sr_sniff_check_path(path)
  path <- chk$path
  sample_bytes <- max(64L, as.integer(sample_bytes))
  warnings <- character()
  if (isTRUE(chk$unc)) {
    warnings <- c(warnings, "path is on a network share (UNC); reads are slow and the share may be unavailable later")
  }
  ext <- .sr_ext(path)
  mg <- .sr_magic(path, n = min(4096L, sample_bytes))
  compression <- "none"
  fmt <- NA_character_
  members <- character()
  chosen <- NA_character_

  if (identical(mg, "zip") && ext %in% c("xlsx", "xlsm")) {
    fmt <- "xlsx"
  } else if (!is.na(mg) && mg %in% .sr_sniff_compressions) {
    compression <- mg
  } else if (!is.na(mg)) {
    fmt <- mg
  } else if (ext %in% .sr_sniff_magic_exts) {
    warnings <- c(warnings, sprintf(
      "extension .%s does not match the file's bytes; reading it as text", ext))
  } else if (ext %in% names(.sr_sniff_ext_formats)) {
    fmt <- unname(.sr_sniff_ext_formats[ext])
  }

  result <- function(fmt, ...) {
    base <- list(
      path = path, format = fmt, compression = compression, zip_members = members,
      member = chosen, encoding = NA_character_, bom = NA, delim = NA_character_,
      quote = NA_character_, decimal = NA_character_, header = NA,
      date_order = stats::setNames(character(), character()), quoted_newlines = NA,
      size_bytes = as.numeric(file.size(path)), warnings = warnings)
    mod <- list(...)
    base[names(mod)] <- mod
    base
  }

  if (compression == "zip") {
    listed <- utils::unzip(path, list = TRUE)$Name
    members <- listed[!endsWith(listed, "/")]
    if (!is.null(member)) {
      if (!(member %in% members)) {
        stop("member '", member, "' is not in the zip archive", call. = FALSE)
      }
      chosen <- member
    } else if (length(members) == 1L) {
      chosen <- members
    } else {
      warnings <- c(warnings, sprintf(
        "zip archive has %d members: choose one with `member`", length(members)))
      return(result(NA_character_))
    }
  } else if (!is.null(member)) {
    warnings <- c(warnings, "`member` is ignored: the file is not a zip archive")
  }

  if (!is.na(fmt)) return(result(fmt))

  # Text, possibly compressed: sniff on the decompressed head.
  head <- .sr_sniff_head(path, compression, chosen, sample_bytes)
  inner_ext <- if (compression == "zip") .sr_ext(chosen) else .sr_ext_inner(path)
  if (compression != "none") {
    mg2 <- .sr_magic_bytes(utils::head(head, 4096L))
    if (!is.na(mg2) && mg2 %in% c("rds", "rdata", "odm")) return(result(mg2))
    if (!is.na(mg2) && !(mg2 %in% .sr_sniff_compressions)) {
      warnings <- c(warnings, sprintf("compressed %s content is not supported as text", mg2))
      return(result(NA_character_))
    }
    if (inner_ext %in% c("rds", "rdata", "rda")) {
      return(result(unname(.sr_sniff_ext_formats[inner_ext])))
    }
  }
  if (!length(head)) {
    warnings <- c(warnings, "file is empty")
    return(result(if (inner_ext %in% c("json", "ndjson", "jsonl")) "json" else "csv",
                  encoding = "UTF-8", bom = FALSE, delim = ",", quote = "\"", decimal = ".",
                  header = FALSE, quoted_newlines = FALSE))
  }
  truncated <- length(head) >= sample_bytes
  e <- .sr_sniff_encoding(head, truncated)
  warnings <- c(warnings, e$warnings)
  txt <- .sr_sniff_decode(e$body, e$encoding, truncated)
  if (is.na(txt)) {
    warnings <- c(warnings, "the head of the file could not be decoded")
    txt <- ""
  }

  if (inner_ext %in% c("json", "ndjson", "jsonl")) {
    kind <- .sr_sniff_json_kind(txt, truncated)
    jfmt <- if (inner_ext == "json") kind else "ndjson"
    if (is.na(jfmt)) {
      warnings <- c(warnings, "content does not look like JSON")
      jfmt <- if (inner_ext == "json") "json" else "ndjson"
    }
    return(result(jfmt, encoding = e$encoding, bom = e$bom))
  }

  dia <- .sr_sniff_dialect(txt, truncated)
  warnings <- c(warnings, dia$warnings)
  dia$warnings <- NULL
  do.call(result, c(list(fmt = "csv", encoding = e$encoding, bom = e$bom), dia))
}

#' Is the session's native encoding UTF-8? Text read through a connection with `encoding=` is
#' converted to the native encoding, so it is UTF-8 exactly when this is TRUE (R >= 4.2 on Windows
#' and any UTF-8 locale elsewhere).
.sr_native_utf8 <- function() isTRUE(l10n_info()[["UTF-8"]])

#' Open a text connection that reads the file decompressed and re-encoded to UTF-8, on the selected
#' zip member when there is one. The connection's `encoding` is the sniffed one, so R converts the
#' text to the session's native encoding, which must be UTF-8 (refused otherwise: a conversion
#' to anything else could lose characters). R itself strips the UTF-8, UTF-16LE and UTF-16BE
#' byte-order marks. `sniff` is the `sr_sniff` result for `path`. The caller closes the connection.
.sr_open_text <- function(path, sniff) {
  if (!(sniff$format %in% c("csv", "json", "ndjson"))) {
    stop("not a text format: ", sniff$format, call. = FALSE)
  }
  if (!.sr_native_utf8()) {
    stop("the session's native encoding is not UTF-8: cannot re-encode text to UTF-8", call. = FALSE)
  }
  enc <- sniff$encoding
  cenc <- if (enc == "UTF-8" && isTRUE(sniff$bom)) "UTF-8-BOM" else enc
  .sr_sniff_conn(path, sniff$compression, sniff$member, "rt", encoding = cenc)
}
