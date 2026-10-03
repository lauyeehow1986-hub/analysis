# core/events.R - event-sourced project log (events.jsonl) with a reducer registry and replay.
#
# One JSON object per line: {seq, ts, type, actor, payload}. State is never stored: it is rebuilt by
# folding the events, in seq order, through the reducer registered for each event type. Every append is
# also recorded in the hash-chained audit log (core/audit.R), so the event file and the audit trail
# cross-check each other.
#
# Torn tail: a crash can leave a final line without its terminating LF. Such a fragment was never
# committed. Reads ignore it with a warning; the next append truncates it away and audits the repair.

# type -> function(state, event) -> state
.sr_reducers <- new.env(parent = emptyenv())

#' Register (or replace) the reducer for an event type. `fn(state, event) -> state`.
sr_reducer_register <- function(type, fn) {
  if (!is.character(type) || length(type) != 1L || is.na(type) || !nzchar(type)) {
    stop("type must be a single non-empty string", call. = FALSE)
  }
  if (!is.function(fn)) stop("fn must be a function(state, event)", call. = FALSE)
  assign(type, fn, envir = .sr_reducers)
  invisible(type)
}

.sr_reducer_get <- function(type) {
  if (!is.character(type) || length(type) != 1L || is.na(type) || !exists(type, envir = .sr_reducers, inherits = FALSE)) {
    stop("no reducer registered for event type '", paste(format(type), collapse = ","), "'", call. = FALSE)
  }
  get(type, envir = .sr_reducers, inherits = FALSE)
}

# Raw bytes of a file ("" -> raw(0) for a missing or empty file).
.sr_events_bytes <- function(path) {
  if (!file.exists(path) || file.size(path) == 0) return(raw(0))
  readBin(path, "raw", n = file.size(path))
}

# Split bytes into LF-terminated lines (as UTF-8 strings) and the unterminated tail (raw, maybe empty).
.sr_events_split <- function(bytes) {
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

#' Read events.jsonl as a list of events (list(seq, ts, type, actor, payload)).
#'
#' A missing or empty file is `list()`. An unterminated final fragment (torn write) is dropped with a
#' warning; a malformed line anywhere else is an error.
sr_events_read <- function(events_path) {
  parts <- .sr_events_split(.sr_events_bytes(events_path))
  n <- length(parts$lines)
  events <- vector("list", n)
  for (i in seq_len(n)) {
    ln <- parts$lines[i]
    e <- if (isTRUE(jsonlite::validate(ln))) {
      tryCatch(jsonlite::fromJSON(ln, simplifyVector = FALSE), error = function(err) NULL)
    } else NULL
    if (!is.list(e) || is.null(names(e)) || !is.numeric(e$seq) || !is.character(e$type)) {
      stop("corrupt events.jsonl at line ", i, call. = FALSE)
    }
    events[[i]] <- e
  }
  if (length(parts$tail)) warning("torn event line ", n + 1L, " ignored", call. = FALSE)
  events
}

#' Append one event; returns it. Errors if `type` has no reducer.
#'
#' Repairs a torn tail first (truncating back to the last LF and auditing "events.repair"), then
#' writes the event line and audits "event:<type>".
sr_event_append <- function(events_path, audit_path, type, payload = list(),
                            actor = sr_opt("user", Sys.info()[["user"]])) {
  .sr_reducer_get(type)
  if (!is.list(payload)) stop("payload must be a list", call. = FALSE)
  dir.create(dirname(events_path), showWarnings = FALSE, recursive = TRUE)

  bytes <- .sr_events_bytes(events_path)
  parts <- .sr_events_split(bytes)
  if (length(parts$tail)) {
    keep <- if (parts$last_lf > 0L) bytes[seq_len(parts$last_lf)] else raw(0)
    writeBin(keep, events_path)
    sr_audit_append(audit_path, "events.repair", actor,
                    list(removed_bytes = length(parts$tail), removed_sha = .sr_sha256(parts$tail)))
  }

  prior <- sr_events_read(events_path)
  seq <- if (length(prior)) as.integer(prior[[length(prior)]]$seq) + 1L else 1L
  event <- list(seq = seq, ts = .sr_now(), type = type, actor = actor, payload = payload)
  .sr_write_lines_append(events_path, .sr_json(event))
  sr_audit_append(audit_path, paste0("event:", type), actor,
                  list(seq = seq, payload_sha = .sr_sha256(.sr_json(payload))))
  event
}

#' Fold `events` through their reducers in seq order, starting from `init`.
sr_state_replay <- function(events, init = list()) {
  if (!length(events)) return(init)
  ord <- order(vapply(events, function(e) as.numeric(e$seq), numeric(1)))
  state <- init
  for (e in events[ord]) state <- .sr_reducer_get(e$type)(state, e)
  state
}

# Built-in reducers.
sr_reducer_register("project.created", function(state, event) {
  state$project <- event$payload
  state
})
sr_reducer_register("note", function(state, event) {
  state$notes <- c(state$notes, event$payload$text)
  state
})
