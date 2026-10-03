# core/events.R - event-sourced project log (events.jsonl) with a reducer registry and replay.
#
# One JSON object per line: {seq, ts, type, actor, payload}. State is never stored: it is rebuilt by
# folding the events, in seq order, through the reducer registered for each event type.
#
# events.jsonl itself is not hash-chained. Every append is recorded in the hash-chained audit log
# (core/audit.R) as "event:<type>" {seq, payload_sha, event_sha}, and .sr_events_audit_check() (run by
# sr_project_verify on every open) matches each event line to exactly one such entry, so an edited,
# deleted, inserted or reordered event line is detected even though the audit chain itself stays valid.
# The one tolerated gap is a single trailing event with no audit entry (a crash between the two writes):
# the next writable open records it as "events.recovered".
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
  ld <- .sr_events_load(events_path)
  if (ld$torn) warning("torn event line ", length(ld$lines) + 1L, " ignored", call. = FALSE)
  ld$events
}

# The complete lines of events.jsonl, their parsed events, and whether a torn fragment was ignored.
# Errors on a malformed line.
.sr_events_load <- function(events_path) {
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
  list(lines = parts$lines, events = events, torn = length(parts$tail) > 0L)
}

# What the audit log records about one event: its seq, the hash of its payload as a re-read parses it,
# and the hash of its whole line (so ts / actor / type edits are caught too).
.sr_event_audit_detail <- function(line, event) {
  list(seq = as.integer(event$seq), payload_sha = .sr_sha256(.sr_json(event$payload)),
       event_sha = .sr_sha256(line))
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
    sr_audit_append(audit_path, action, actor, .sr_event_audit_detail(line, event)),
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

# Match event lines to audit entries. `ld` is .sr_events_load(); `entries` the parsed audit entries
# (NULL for unparseable lines, which the chain check reports). Returns list(problems, unaudited):
# `unaudited` is the seq of a single trailing event with no audit entry (tolerated), else NULL.
.sr_events_audit_check <- function(ld, entries) {
  problems <- character()
  # audit entries about events, grouped by seq
  by_seq <- list()
  for (a in entries) {
    if (!is.list(a) || !(startsWith(a$action, "event:") || identical(a$action, "events.recovered"))) next
    s <- suppressWarnings(as.integer(a$detail$seq))
    if (length(s) != 1L || is.na(s)) {
      problems <- c(problems, sprintf("audit entry '%s' has no valid seq", a$action))
      next
    }
    k <- as.character(s)
    by_seq[[k]] <- c(by_seq[[k]], list(a))
  }

  n <- length(ld$events)
  seqs <- vapply(ld$events, function(e) suppressWarnings(as.integer(e$seq))[1], integer(1))
  bad <- which(is.na(seqs) | seqs != seq_len(n))
  if (length(bad)) {
    i <- bad[1]
    problems <- c(problems, sprintf("events.jsonl seq gap: line %d has seq %s, expected seq %d",
                                    i, format(seqs[i]), i))
  }

  unaudited <- NULL
  for (i in seq_len(n)) {
    e <- ld$events[[i]]
    s <- seqs[i]
    if (is.na(s)) next
    got <- by_seq[[as.character(s)]]
    if (!length(got)) {
      if (i == n && !length(bad)) {
        unaudited <- s
      } else {
        problems <- c(problems, sprintf("event seq %d has no audit entry", s))
      }
      next
    }
    if (length(got) > 1L) {
      problems <- c(problems, sprintf("event seq %d has %d audit entries", s, length(got)))
      next
    }
    a <- got[[1]]
    want <- .sr_event_audit_detail(ld$lines[i], e)
    type_ok <- if (identical(a$action, "events.recovered")) {
      identical(a$detail$type, e$type)
    } else {
      identical(a$action, paste0("event:", e$type))
    }
    same <- type_ok && identical(a$detail$payload_sha, want$payload_sha) &&
      (is.null(a$detail$event_sha) || identical(a$detail$event_sha, want$event_sha))
    if (!same) problems <- c(problems, sprintf("event seq %d does not match its audit entry", s))
  }

  extra <- setdiff(as.integer(names(by_seq)), seqs)
  for (s in sort(extra)) {
    problems <- c(problems, sprintf("audit entry for event seq %d has no event in events.jsonl", s))
  }
  list(problems = problems, unaudited = unaudited)
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
