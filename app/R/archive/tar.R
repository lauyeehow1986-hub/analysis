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

.sr_tar_size <- function(b) {
  if (as.integer(b[1]) >= 128L) {                       # base-256 (GNU large size)
    if (as.integer(b[1]) >= 192L) stop("bad size in tar header", call. = FALSE)
    v <- as.integer(b)
    v[1] <- bitwAnd(v[1], 127L)
    return(sum(v * 256^(rev(seq_along(v)) - 1)))
  }
  s <- trimws(gsub("[^0-7 ]", "", .sr_tar_str(b)))
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
    stored <- .sr_tar_size(h[149:156])
    h2 <- h
    h2[149:156] <- as.raw(32L)
    if (stored != sum(as.integer(h2))) {
      stop(sprintf("not a tar archive (bad header checksum at member %d)", i), call. = FALSE)
    }
    nm <- .sr_tar_str(h[1:100])
    ty <- if (h[157] == as.raw(0L)) "0" else rawToChar(h[157])
    sz <- .sr_tar_size(h[125:136])
    if (identical(h[258:265], SR_TAR_USTAR)) {
      pre <- .sr_tar_str(h[346:500])
      if (nzchar(pre)) nm <- paste0(pre, "/", nm)
    } else if (any(h[346:500] != as.raw(0L))) {          # non-POSIX magic: prefix is ambiguous
      stop("ambiguous ustar prefix in member", call. = FALSE)
    }
    padded <- ceiling(sz / 512) * 512
    if (identical(ty, "L")) {                            # GNU long name for the next member
      if (sz > 65536) stop("oversized GNU long name", call. = FALSE)
      body <- readBin(con, "raw", padded)
      if (length(body) < padded) stop("truncated tar archive", call. = FALSE)
      longname <- .sr_tar_str(body[seq_len(sz)])
      next
    }
    if (!is.null(longname)) { nm <- longname; longname <- NULL }
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

.sr_win_device_re <- "^(CON|PRN|AUX|NUL|CONIN[$]|CONOUT[$]|(COM|LPT)[1-9\u00b9\u00b2\u00b3])$"

.sr_archive_safe_path <- function(x) {
  vapply(as.character(x), function(n) {
    if (is.na(n) || !validUTF8(n)) return(FALSE)
    s <- sub("/$", "", n)
    if (!nzchar(s)) return(FALSE)
    if (nchar(n, type = "bytes") > 255L) return(FALSE)
    if (grepl("[[:cntrl:]]", n) || grepl("\\", n, fixed = TRUE) || grepl(":", n, fixed = TRUE)) return(FALSE)
    if (startsWith(n, "/")) return(FALSE)
    seg <- strsplit(s, "/", fixed = TRUE)[[1]]
    if (length(seg) == 0L || any(seg %in% c("", ".", ".."))) return(FALSE)
    if (any(grepl("[. ]$", seg))) return(FALSE)
    if (any(grepl(.sr_win_device_re, toupper(sub("[.].*$", "", seg))))) return(FALSE)
    TRUE
  }, logical(1), USE.NAMES = FALSE)
}

.sr_tar_check <- function(entries) {
  if (nrow(entries) > SR_TAR_MAX_ENTRIES) stop("too many members in tar archive", call. = FALSE)
  for (i in seq_len(nrow(entries))) {
    n <- entries$name[i]; t <- entries$type[i]
    if (!t %in% c("0", "5")) {
      stop(sprintf("archive member '%s' is a link or special file (type %s)", n, t), call. = FALSE)
    }
    if (!.sr_archive_safe_path(n) || (t == "0" && endsWith(n, "/"))) {
      stop(sprintf("unsafe archive member name '%s'", n), call. = FALSE)
    }
    if (entries$size[i] > SR_TAR_MAX_MEMBER) {
      stop(sprintf("archive member '%s' exceeds the size limit", n), call. = FALSE)
    }
  }
  key <- tolower(sub("/+$", "", sub("[. ]+$", "", sub("/+$", "", entries$name))))
  d <- which(duplicated(key))
  if (length(d)) stop(sprintf("duplicate archive member '%s'", entries$name[d[1]]), call. = FALSE)
  invisible(TRUE)
}
