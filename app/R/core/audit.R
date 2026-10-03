# core/audit.R - tamper-evident, append-only audit log.
#
# One JSON object per line: {ts, action, actor, detail, prev_hash, hash}. Each entry's hash covers the
# previous entry's hash, so an edit, insertion or deletion anywhere breaks the chain from that line on.
# Verification needs nothing but this file and the SHA-256 recipe in .sr_audit_hash().
#
# Torn tail: a crash mid-append can leave a final fragment without its terminating LF. It was never a
# committed entry, so reads and verification ignore it (verify reports `torn = TRUE`) and the next
# append truncates it away, recording "audit.repair" {removed_bytes, removed_sha} before its own entry.
# A complete line that is not a valid entry is never repaired: it stays a verification failure.

.sr_audit_genesis <- "GENESIS"

# Hash of one entry: sha256(prev_hash | canonical JSON of the hashed fields, in fixed order).
.sr_audit_hash <- function(prev_hash, ts, action, actor, detail) {
  .sr_sha256(paste0(prev_hash, "|",
                    .sr_json(list(ts = ts, action = action, actor = actor, detail = detail))))
}

# The LF-terminated lines of the log as UTF-8 (a trailing CR stripped, trailing empty lines dropped),
# with `torn` = whether an unterminated final fragment was ignored. character(0) for a missing file.
.sr_audit_split <- function(log_path) {
  parts <- .sr_split_lf(.sr_read_bytes(log_path))
  lines <- sub("$", "", parts$lines)
  while (length(lines) && !nzchar(lines[length(lines)])) lines <- lines[-length(lines)]
  list(lines = lines, torn = length(parts$tail) > 0L)
}

.sr_audit_lines <- function(log_path) .sr_audit_split(log_path)$lines

# Parse one log line; NULL when it is not a well-formed entry.
.sr_audit_parse <- function(line) {
  e <- tryCatch(.sr_parse_json(line), error = function(err) NULL)
  if (!is.list(e)) return(NULL)
  # Exactly these keys in this order: extra or duplicated keys are tampering (parsers disagree on them).
  need <- c("ts", "action", "actor", "detail", "prev_hash", "hash")
  if (!identical(names(e), need)) return(NULL)
  ok <- vapply(e[setdiff(need, "detail")],function(v) is.character(v) && length(v) == 1L && !is.na(v), logical(1))
  if (!all(ok)) return(NULL)
  e
}

#' Hash of the last complete entry in the log, or "GENESIS" for a missing or empty file. Errors when the
#' last complete line is not a valid entry (a torn fragment after it is ignored).
sr_audit_tip <- function(log_path) {
  lines <- .sr_audit_lines(log_path)
  if (!length(lines)) return(.sr_audit_genesis)
  e <- .sr_audit_parse(lines[length(lines)])
  if (is.null(e)) stop("audit log is corrupt: last line is not a valid entry", call. = FALSE)
  e$hash
}

#' Append one entry. Returns the new entry's hash.
#'
#' @param log_path path to the audit log (parent directories are created)
#' @param action   short verb, e.g. "open", "analyse", "export"
#' @param actor    user id or role acting
#' @param detail   list of extra fields (files, counts, checksums, ...)
#'
#' A torn final fragment is truncated away first and audited as "audit.repair".
sr_audit_append <- function(log_path, action, actor, detail = list()) {
  if (!is.list(detail)) stop("detail must be a list", call. = FALSE)
  dir.create(dirname(log_path), showWarnings = FALSE, recursive = TRUE)
  sr_audit_tip(log_path)  # an unreadable tip fails before anything is changed
  torn <- .sr_truncate_tail(log_path)
  if (length(torn)) {
    .sr_audit_write(log_path, "audit.repair", actor,
                    list(removed_bytes = length(torn), removed_sha = .sr_sha256(torn)))
  }
  .sr_audit_write(log_path, action, actor, detail)
}

# Chain one entry onto the (tail-free) log; returns its hash.
.sr_audit_write <- function(log_path, action, actor, detail) {
  prev <- sr_audit_tip(log_path)
  ts <- .sr_now()
  hash <- .sr_audit_hash(prev, ts, action, actor, detail)
  line <- .sr_json(list(ts = ts, action = action, actor = actor, detail = detail,
                        prev_hash = prev, hash = hash))
  .sr_write_lines_append(log_path, line)
  hash
}

#' Verify the whole chain: list(ok, broken_at = 1-based line or NA, n = number of complete lines,
#' torn = whether an unterminated final fragment was ignored).
sr_audit_verify <- function(log_path) {
  sp <- .sr_audit_split(log_path)
  lines <- sp$lines
  n <- length(lines)
  prev <- .sr_audit_genesis
  for (i in seq_len(n)) {
    e <- .sr_audit_parse(lines[i])
    bad <- is.null(e) ||
      !identical(e$prev_hash, prev) ||
      !identical(e$hash, .sr_audit_hash(e$prev_hash, e$ts, e$action, e$actor, e$detail))
    if (bad) return(list(ok = FALSE, broken_at = i, n = n, torn = sp$torn))
    prev <- e$hash
  }
  list(ok = TRUE, broken_at = NA_integer_, n = n, torn = sp$torn)
}

# The last complete entry (parsed), or NULL when the log is empty or its last line is not an entry.
.sr_audit_last <- function(log_path) {
  lines <- .sr_audit_lines(log_path)
  if (!length(lines)) return(NULL)
  .sr_audit_parse(lines[length(lines)])
}

#' Read the log as a data.frame (ts, action, actor, prev_hash, hash) plus a list-column `detail`.
sr_audit_read <- function(log_path) {
  lines <- .sr_audit_lines(log_path)
  es <- lapply(lines, .sr_audit_parse)
  if (any(vapply(es, is.null, logical(1)))) {
    stop("audit log is corrupt: line ", which(vapply(es, is.null, logical(1)))[1],
         " is not a valid entry", call. = FALSE)
  }
  col <- function(f) vapply(es, function(e) e[[f]], character(1))
  d <- data.frame(ts = col("ts"), action = col("action"), actor = col("actor"),
                  prev_hash = col("prev_hash"), hash = col("hash"),
                  stringsAsFactors = FALSE)
  d$detail <- lapply(es, function(e) e$detail)
  d
}
