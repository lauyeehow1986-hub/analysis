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

#' Read events.jsonl as a list of events (list(seq, ts, type, actor, payload)).
#'
#' A missing or empty file is `list()`. An unterminated final fragment (torn write) is dropped with a
#' warning; a malformed line anywhere else is an error.
sr_events_read <- function(events_path) {
  parts <- .sr_split_lf(.sr_read_bytes(events_path))
  n <- length(parts$lines)
  events <- vector("list", n)
  for (i in seq_len(n)) {
    ln <- parts$lines[i]
    e <- if (isTRUE(jsonlite::validate(ln))) {
      tryCatch(.sr_parse_json(ln), error = function(err) NULL)
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
#' Order: fail early if the audit tip is unreadable; repair a torn events tail (truncating back to the
#' last LF and auditing "events.repair"); write the event line; audit "event:<type>". If that audit
#' write fails, events.jsonl is restored to its previous bytes before the error is re-thrown, so an
#' event never stays committed without its audit entry and a retry never duplicates it.
sr_event_append <- function(events_path, audit_path, type, payload = list(),
                            actor = sr_opt("user", Sys.info()[["user"]])) {
  .sr_reducer_get(type)
  if (!is.list(payload)) stop("payload must be a list", call. = FALSE)
  dir.create(dirname(events_path), showWarnings = FALSE, recursive = TRUE)
  sr_audit_tip(audit_path)

  torn <- .sr_truncate_tail(events_path)
  if (length(torn)) {
    sr_audit_append(audit_path, "events.repair", actor,
                    list(removed_bytes = length(torn), removed_sha = .sr_sha256(torn)))
  }

  existed <- file.exists(events_path)
  before <- .sr_read_bytes(events_path)
  prior <- sr_events_read(events_path)
  seq <- if (length(prior)) as.integer(prior[[length(prior)]]$seq) + 1L else 1L
  line <- .sr_json(list(seq = seq, ts = .sr_now(), type = type, actor = actor, payload = payload))
  .sr_write_lines_append(events_path, line)
  # Return the event as a re-read parses it (NA -> NULL, vectors -> lists), so live folding == replay.
  event <- .sr_parse_json(line)
  action <- paste0("event:", type)
  tryCatch(
    sr_audit_append(audit_path, action, actor,
                    list(seq = seq, payload_sha = .sr_sha256(.sr_json(payload)))),
    error = function(err) {
      last <- tryCatch(.sr_audit_last(audit_path), error = function(e) NULL)
      audited <- is.list(last) && identical(last$action, action) &&
        identical(suppressWarnings(as.integer(last$detail$seq)), seq)
      if (!audited) .sr_events_rollback(events_path, existed, before, err)
      stop(err)
    })
  event
}

# Put events.jsonl back as it was before an append whose audit entry could not be written.
.sr_events_rollback <- function(events_path, existed, before, err) {
  ok <- tryCatch({
    if (existed) .sr_write_atomic(events_path, before) else unlink(events_path)
    existed || !file.exists(events_path)
  }, error = function(e) FALSE)
  if (!ok) {
    stop(conditionMessage(err), "; and events.jsonl could not be rolled back", call. = FALSE)
  }
  invisible(TRUE)
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
