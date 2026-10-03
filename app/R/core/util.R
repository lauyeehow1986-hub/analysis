# core/util.R - options, mode, time, JSON, hashing and append helpers shared by every core module.

#' Read a StatRouter setting: option sr.<name>, then env var SR_<NAME> (empty counts as unset),
#' then `default`.
sr_opt <- function(name, default = NULL) {
  env <- Sys.getenv(toupper(paste0("SR_", name)), unset = "")
  if (!nzchar(env)) env <- default
  getOption(paste0("sr.", name), env)
}

#' Run mode: "airgap" (default, no network anywhere) or "connected".
sr_mode <- function() {
  m <- sr_opt("mode", "airgap")
  if (!is.character(m) || length(m) != 1L || !m %in% c("airgap", "connected")) {
    stop("sr.mode / SR_MODE must be 'airgap' or 'connected', got: ",
         paste(format(m), collapse = ","), call. = FALSE)
  }
  m
}

#' ISO-8601 UTC timestamp with milliseconds.
.sr_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")

#' One-line JSON string with the project-wide settings.
.sr_json <- function(x) {
  as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null", na = "null"))
}

#' Parse one JSON text into lists (simplifyVector = FALSE: arrays stay lists, null stays NULL).
#' Never use jsonlite::fromJSON / read_json on text read from disk: when the text is not valid JSON
#' they fetch it as a URL (https?://...) or read it as a file path. parse_json() only parses.
.sr_parse_json <- function(txt) jsonlite::parse_json(txt, simplifyVector = FALSE)

#' Lowercase hex SHA-256 of a character string (UTF-8) or a raw vector.
.sr_sha256 <- function(x) {
  if (is.character(x)) x <- charToRaw(enc2utf8(paste(x, collapse = "")))
  paste(as.character(unclass(openssl::sha256(x))), collapse = "")
}

#' Append one LF-terminated line (UTF-8) to `path`, flushing before close.
.sr_write_lines_append <- function(path, line) {
  con <- file(path, "ab")
  on.exit(close(con), add = TRUE)
  writeBin(c(charToRaw(enc2utf8(line)), as.raw(0x0A)), con)
  flush(con)
  invisible(path)
}

#' Raw bytes of a file (raw(0) for a missing or empty file).
.sr_read_bytes <- function(path) {
  if (!file.exists(path) || file.size(path) == 0) return(raw(0))
  readBin(path, "raw", n = file.size(path))
}

#' Split bytes into LF-terminated lines (as UTF-8 strings) and the unterminated tail (raw, maybe
#' empty). `last_lf` is the byte position of the last LF (0 when there is none).
.sr_split_lf <- function(bytes) {
  lf <- which(bytes == as.raw(0x0A))
  last <- if (length(lf)) lf[length(lf)] else 0L
  starts <- c(1L, lf + 1L)[seq_along(lf)]
  lines <- vapply(seq_along(lf), function(i) {
    seg <- if (lf[i] > starts[i]) bytes[starts[i]:(lf[i] - 1L)] else raw(0)
    out <- rawToChar(seg)
    Encoding(out) <- "UTF-8"
    out
  }, character(1))
  tail <- if (last < length(bytes)) bytes[(last + 1L):length(bytes)] else raw(0)
  list(lines = lines, tail = tail, last_lf = last)
}

#' Replace the whole content of `path` with `bytes` atomically: write "<path>.tmp" in the same
#' directory, then rename it over `path`, so a crash leaves either the old or the new file, never a
#' truncated one. On failure the .tmp is removed and the call stops; the target is left as it was.
.sr_write_atomic <- function(path, bytes) {
  tmp <- paste0(path, ".tmp")
  ok <- tryCatch({
    con <- file(tmp, "wb")
    tryCatch({
      writeBin(bytes, con)
      flush(con)
    }, finally = close(con))
    isTRUE(suppressWarnings(file.rename(tmp, path)))
  }, error = function(e) FALSE)
  if (!ok) {
    unlink(tmp)
    stop("cannot replace ", basename(path), " (write to ", basename(tmp), " and rename failed)",
         call. = FALSE)
  }
  invisible(path)
}

#' Cut an unterminated final fragment (a torn append: no trailing LF) off `path`, rewriting the file
#' atomically. Returns the removed bytes, or raw(0) when the file is missing, empty or ends in LF.
.sr_truncate_tail <- function(path) {
  bytes <- .sr_read_bytes(path)
  parts <- .sr_split_lf(bytes)
  if (!length(parts$tail)) return(raw(0))
  .sr_write_atomic(path, if (parts$last_lf > 0L) bytes[seq_len(parts$last_lf)] else raw(0))
  parts$tail
}
