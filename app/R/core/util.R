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
