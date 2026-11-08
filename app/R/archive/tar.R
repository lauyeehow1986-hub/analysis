# archive/tar.R - a pure-R tar header scanner and member-name safety checks for .srproj import.
#
#   .sr_tar_entries(path)       data.frame(name, type, size) of every member, in order (gzip or plain tar)
#   .sr_archive_safe_path(x)    logical: is each member name safe to extract under a root folder
#   .sr_tar_check(entries)      invisible TRUE, or stops at the first link / unsafe name / duplicate
#
# This file only lists. Member data is skipped in chunks of at most 1 MiB, so a huge member never sits in
# memory. Extraction stays elsewhere and happens only after .sr_tar_check.

.sr_tar_str <- function(b) {
  z <- which(b == as.raw(0L))
  if (length(z)) b <- b[seq_len(z[1] - 1L)]
  if (!length(b)) return("")
  s <- rawToChar(b)
  Encoding(s) <- "UTF-8"
  s
}

# A NUL-terminated header field (name or prefix) as untar2 reads it: up to its LAST non-zero byte. Bytes
# after the first NUL would make the scanner and untar2 read different names, so they are refused.
.sr_tar_field <- function(b, what) {
  z <- which(b == as.raw(0L))
  if (length(z) && any(b[z[1]:length(b)] != as.raw(0L))) {
    stop("bytes after the end of the ", what, " field in tar header", call. = FALSE)
  }
  .sr_tar_str(b)
}

# The name a GNU L entry gives, read exactly as utils:::untar2 reads it (the whole padded block up to its
# last non-zero byte); refused unless that equals the declared-size reading with no embedded NUL.
.sr_tar_longname <- function(body, sz) {
  w <- which(body != as.raw(0L))
  if (!length(w)) stop("empty GNU long name", call. = FALSE)
  full <- body[seq_len(max(w))]
  if (max(w) > sz || any(full == as.raw(0L))) {
    stop("malformed GNU long name (bytes past its size or an embedded NUL)", call. = FALSE)
  }
  nm <- .sr_tar_str(full)
  if (!identical(nm, .sr_tar_str(body[seq_len(sz)]))) stop("malformed GNU long name", call. = FALSE)
  nm
}

# TRUE when an octal header field (up to its first NUL) holds only 0-7 and spaces, as untar2 needs.
.sr_tar_octal_ok <- function(b) {
  z <- which(b == as.raw(0L))
  if (length(z)) b <- b[seq_len(z[1] - 1L)]
  all(b %in% charToRaw("01234567 "))
}

.sr_tar_size <- function(b) {
  if (as.integer(b[1]) >= 128L) {                       # base-256 (GNU large size)
    if (as.integer(b[1]) >= 192L) stop("bad size in tar header", call. = FALSE)
    v <- as.integer(b)
    v[1] <- bitwAnd(v[1], 127L)
    return(sum(v * 256^(rev(seq_along(v)) - 1)))
  }
  s <- .sr_tar_str(b)
  if (!.sr_tar_octal_ok(b)) stop("bad size in tar header", call. = FALSE)   # untar2 refuses these too
  s <- gsub(" ", "", s, fixed = TRUE)                                         # untar2 skips spaces anywhere
  if (!nzchar(s)) return(0)
  sum(as.integer(strsplit(s, "")[[1]]) * 8^((nchar(s) - 1):0))
}

.sr_tar_skip <- function(con, n) {
  chunk <- 1048576
  while (n > 0) {
    k <- min(n, chunk)
    got <- readBin(con, "raw", k)
    if (length(got) < k) stop("truncated tar archive", call. = FALSE)
    n <- n - k
  }
  invisible(NULL)
}

SR_TAR_MAX_ENTRIES <- 1e6
SR_TAR_MAX_MEMBER <- 2^33
SR_TAR_USTAR <- as.raw(c(0x75, 0x73, 0x74, 0x61, 0x72, 0x00, 0x30, 0x30))   # "ustar" NUL "00"

.sr_tar_entries <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con), add = TRUE)
  cap <- 1024L
  name <- character(cap); type <- character(cap); size <- numeric(cap)
  n <- 0L
  longname <- NULL
  i <- 0L
  repeat {
    h <- readBin(con, "raw", 512L)
    if (length(h) == 0L) break
    if (length(h) < 512L) stop("truncated tar archive", call. = FALSE)
    if (all(h == as.raw(0L))) break
    i <- i + 1L
    stored <- tryCatch(.sr_tar_size(h[149:156]), error = function(e) NA_real_)
    h2 <- h
    h2[149:156] <- as.raw(32L)
    if (is.na(stored) || stored != sum(as.integer(h2))) {
      stop(sprintf("not a tar archive (bad header checksum at member %d)", i), call. = FALSE)
    }
    nm <- .sr_tar_field(h[1:100], "name")
    ty <- if (h[157] == as.raw(0L)) "0" else rawToChar(h[157])
    sz <- .sr_tar_size(h[125:136])
    # untar2 parses these (getOct / rawToChar) and stops part-way through an archive on a bad one
    if (!.sr_tar_octal_ok(h[101:108])) stop("bad mode field in tar header at member ", i, call. = FALSE)
    if (!.sr_tar_octal_ok(h[137:148])) stop("bad mtime field in tar header at member ", i, call. = FALSE)
    .sr_tar_field(h[258:262], "magic")
    # Data consumed per type by utils:::untar2 (the extractor) vs this scanner (which skips `padded`):
    #   "0", NUL, raw 0x07   untar2 reads ceiling(size/512) blocks       same
    #   "L", "K", "x", "g"   untar2 reads ceiling(size/512) blocks       same
    #   "1" "2" "3" "4" "5" "6"   untar2 reads NO data                   differs when size > 0
    #   anything else        untar2 stops ("unsupported entry type")
    # so a member that is not a regular file or a GNU long name must declare size 0; otherwise the two
    # would fall out of step and untar2 would read the "data" as headers (a hidden link, say).
    if (!identical(ty, "0") && !identical(ty, "L") && sz != 0) {
      stop(sprintf("tar member %d (type %s) is not a regular file but has data (size %s); a %s with data",
                   i, ty, format(sz), if (identical(ty, "5")) "directory member" else "non-file member"),
           call. = FALSE)
    }
    if (identical(h[258:265], SR_TAR_USTAR)) {
      pre <- .sr_tar_field(h[346:500], "prefix")
      if (nzchar(pre)) nm <- paste0(pre, "/", nm)
    } else if (any(h[346:500] != as.raw(0L))) {          # non-POSIX magic: prefix is ambiguous
      stop("ambiguous ustar prefix in member", call. = FALSE)
    }
    padded <- ceiling(sz / 512) * 512
    if (identical(ty, "L")) {                            # GNU long name for the next member
      if (sz > 65536) stop("oversized GNU long name", call. = FALSE)
      body <- readBin(con, "raw", padded)
      if (length(body) < padded) stop("truncated tar archive", call. = FALSE)
      longname <- tryCatch(.sr_tar_longname(body, sz), error = function(e) {
        stop(conditionMessage(e), " (GNU long name at member ", i, ")", call. = FALSE)
      })
      next
    }
    if (!is.null(longname)) {
      # untar2 hands a long name only to a regular file (a folder keeps its own name and the long name
      # passes on to the next file), so a long name before anything else is refused
      if (!identical(ty, "0")) stop("GNU long name not followed by a regular file", call. = FALSE)
      nm <- longname
      longname <- NULL
    }
    .sr_tar_skip(con, padded)
    n <- n + 1L
    if (n > SR_TAR_MAX_ENTRIES) stop("too many members in tar archive", call. = FALSE)
    if (n > cap) {
      cap <- cap * 2L
      length(name) <- cap; length(type) <- cap; length(size) <- cap
    }
    name[n] <- nm; type[n] <- ty; size[n] <- sz
  }
  if (!is.null(longname)) stop("truncated tar archive", call. = FALSE)
  data.frame(name = name[seq_len(n)], type = type[seq_len(n)], size = as.numeric(size[seq_len(n)]),
             stringsAsFactors = FALSE)
}

# A Windows device name as a whole path segment: the part before the first dot, trailing spaces dropped.
.sr_win_device_seg_re <- paste0("(^|/)(CON|PRN|AUX|NUL|CONIN[$]|CONOUT[$]|CLOCK[$]|(COM|LPT)[1-9\u00b9\u00b2\u00b3])",
                                " *([.][^/]*)?(/|$)")

# Vectorised over x. One trailing slash (a folder) is allowed; an empty segment anywhere is not.
.sr_archive_safe_path <- function(x) {
  n <- as.character(x)
  out <- logical(length(n))
  ok <- !is.na(n)
  ok[ok] <- validUTF8(n[ok])
  if (!any(ok)) return(out)
  v <- n[ok]
  s <- sub("/$", "", v)
  out[ok] <- nzchar(s) & nchar(v, type = "bytes") <= 255L &
    !grepl("[[:cntrl:]]", v) & !grepl("\\", v, fixed = TRUE) & !grepl(":", v, fixed = TRUE) &
    !startsWith(v, "/") & !endsWith(s, "/") & !grepl("//", s, fixed = TRUE) &
    !grepl("(^|/)[.]{1,2}(/|$)", s) &                    # "." and ".." segments
    !grepl("[. ](/|$)", s) &                              # a segment ending in a dot or space
    !grepl(.sr_win_device_seg_re, toupper(s))
  out
}

# Vectorised: each check runs once over all rows; the first offending row is reported, and within a row
# the type comes before the name and the name before the size (the order of the former row loop).
.sr_tar_check <- function(entries) {
  if (nrow(entries) > SR_TAR_MAX_ENTRIES) stop("too many members in tar archive", call. = FALSE)
  nm <- as.character(entries$name); ty <- as.character(entries$type); sz <- as.numeric(entries$size)
  bad_type <- is.na(ty) | !(ty %in% c("0", "5"))
  bad_name <- !.sr_archive_safe_path(nm) | (!bad_type & ty == "0" & endsWith(nm, "/") %in% TRUE)
  over <- is.na(sz) | sz > SR_TAR_MAX_MEMBER
  i <- which(bad_type | bad_name | over)
  if (length(i)) {
    i <- i[1L]; n <- nm[i]
    if (bad_type[i]) stop(sprintf("archive member '%s' is a link or special file (type %s)", n, ty[i]), call. = FALSE)
    if (bad_name[i]) stop(sprintf("unsafe archive member name '%s'", n), call. = FALSE)
    stop(sprintf("archive member '%s' exceeds the size limit", n), call. = FALSE)
  }
  key <- tolower(sub("/+$", "", sub("[. ]+$", "", sub("/+$", "", nm))))
  d <- which(duplicated(key))
  if (length(d)) stop(sprintf("duplicate archive member '%s'", nm[d[1]]), call. = FALSE)
  invisible(TRUE)
}
