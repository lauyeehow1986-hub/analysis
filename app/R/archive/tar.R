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

.sr_tar_entries <- function(path) {
  con <- gzfile(path, "rb")
  on.exit(close(con), add = TRUE)
  name <- character(); type <- character(); size <- numeric()
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
    ty <- rawToChar(h[157])
    if (identical(ty, "\001") || h[157] == as.raw(0L)) ty <- "0"
    sz <- .sr_tar_size(h[125:136])
    if (identical(.sr_tar_str(h[258:262]), "ustar")) {
      pre <- .sr_tar_str(h[346:500])
      if (nzchar(pre)) nm <- paste0(pre, "/", nm)
    }
    padded <- ceiling(sz / 512) * 512
    if (identical(ty, "L")) {                            # GNU long name for the next member
      if (sz > 65536) stop("truncated tar archive", call. = FALSE)
      body <- readBin(con, "raw", padded)
      if (length(body) < padded) stop("truncated tar archive", call. = FALSE)
      longname <- .sr_tar_str(body[seq_len(sz)])
      next
    }
    if (!is.null(longname)) { nm <- longname; longname <- NULL }
    .sr_tar_skip(con, padded)
    name <- c(name, nm); type <- c(type, ty); size <- c(size, sz)
  }
  data.frame(name = name, type = type, size = as.numeric(size), stringsAsFactors = FALSE)
}

.sr_archive_safe_path <- function(x) {
  vapply(as.character(x), function(n) {
    if (is.na(n)) return(FALSE)
    s <- sub("/$", "", n)
    if (!nzchar(s)) return(FALSE)
    if (nchar(n, type = "bytes") > 255L) return(FALSE)
    if (grepl("[[:cntrl:]]", n) || grepl("\\", n, fixed = TRUE) || grepl(":", n, fixed = TRUE)) return(FALSE)
    if (startsWith(n, "/")) return(FALSE)
    seg <- strsplit(s, "/", fixed = TRUE)[[1]]
    if (length(seg) == 0L || any(seg %in% c("", ".", ".."))) return(FALSE)
    TRUE
  }, logical(1), USE.NAMES = FALSE)
}

.sr_tar_check <- function(entries) {
  for (i in seq_len(nrow(entries))) {
    n <- entries$name[i]; t <- entries$type[i]
    if (!t %in% c("0", "5")) {
      stop(sprintf("archive member '%s' is a link or special file (type %s)", n, t), call. = FALSE)
    }
    if (!.sr_archive_safe_path(n)) stop(sprintf("unsafe archive member name '%s'", n), call. = FALSE)
    if (n %in% entries$name[seq_len(i - 1L)]) {
      stop(sprintf("duplicate archive member '%s'", n), call. = FALSE)
    }
  }
  invisible(TRUE)
}
