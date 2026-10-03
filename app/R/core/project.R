# core/project.R - the portable project folder: project.json + events.jsonl + audit.log + data/ fits/ out/.
#
# Everything a project needs lives in one self-contained folder, so a job can be unplugged and resumed on
# another machine. Nothing stored inside the folder is an absolute path: `$paths` is rebuilt from the
# directory you open. State is never stored; it is replayed from events.jsonl (core/events.R).
#
# Locking: a `.lock` file ({host, pid, user, ts}) marks the process that has the project open for writing.
# A lock held by a live process (or by another machine, which we cannot probe) makes the open read-only;
# a stale lock (same host, process gone) is taken over and audited as "lock.takeover".
#
# Opening writes nothing to the audit log except "lock.takeover", "schema.migrated" when migrating, and
# "events.recovered" for a single trailing event whose audit entry a crash prevented (core/events.R).
# Every open runs sr_project_verify() and returns the result as `$verify`; a failed check is reported,
# not refused, so the data can still be inspected.

SR_SCHEMA_VERSION <- 1L

#' Schema migrations, keyed by from-version as a string; each is `function(meta) meta` and must bump
#' `meta$schema_version`. Version 1 is the first schema, so there is nothing to migrate yet.
sr_schema_migrations <- list()

# ---- small helpers --------------------------------------------------------------------------------

.sr_actor <- function() sr_opt("user", Sys.info()[["user"]])

# Write `x` as one LF-terminated line of UTF-8 JSON, replacing the file atomically (<path>.tmp + rename).
.sr_write_json_file <- function(path, x) {
  .sr_write_atomic(path, c(charToRaw(enc2utf8(.sr_json(x))), as.raw(0x0A)))
}

# Read a JSON file written by .sr_write_json_file (UTF-8 regardless of the platform locale).
.sr_read_json_file <- function(path) {
  txt <- rawToChar(readBin(path, "raw", n = file.size(path)))
  Encoding(txt) <- "UTF-8"
  .sr_parse_json(txt)
}

# RFC 4122 version-4 UUID from the OS entropy source via openssl.
.sr_uuid4 <- function() {
  b <- as.integer(openssl::rand_bytes(16))
  b[7] <- bitwOr(bitwAnd(b[7], 0x0F), 0x40)
  b[9] <- bitwOr(bitwAnd(b[9], 0x3F), 0x80)
  h <- sprintf("%02x", b)
  paste0(paste(h[1:4], collapse = ""), "-", paste(h[5:6], collapse = ""), "-",
         paste(h[7:8], collapse = ""), "-", paste(h[9:10], collapse = ""), "-",
         paste(h[11:16], collapse = ""))
}

# ---- paths ----------------------------------------------------------------------------------------

#' Every path of a project folder, derived from `dir` alone (forward slashes).
sr_project_paths <- function(dir) {
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  list(
    dir    = dir,
    json   = file.path(dir, "project.json"),
    events = file.path(dir, "events.jsonl"),
    audit  = file.path(dir, "audit.log"),
    data   = file.path(dir, "data"),
    fits   = file.path(dir, "fits"),
    out    = file.path(dir, "out"),
    lock   = file.path(dir, ".lock")
  )
}

# ---- schema migration -----------------------------------------------------------------------------

#' Bring `meta` (the parsed project.json) up to SR_SCHEMA_VERSION. A newer schema is an error. The
#' migration table is an argument whose default is looked up when called, so tests can swap it.
.sr_migrate <- function(meta, migrations = sr_schema_migrations) {
  v <- suppressWarnings(as.integer(meta$schema_version))
  if (length(v) != 1L || is.na(v)) stop("project.json has no valid schema_version", call. = FALSE)
  if (v > SR_SCHEMA_VERSION) {
    stop("project created by a newer StatRouter (schema ", v, ", this build reads up to ",
         SR_SCHEMA_VERSION, ")", call. = FALSE)
  }
  while (v < SR_SCHEMA_VERSION) {
    fn <- migrations[[as.character(v)]]
    if (!is.function(fn)) stop("no migration from schema version ", v, call. = FALSE)
    meta <- fn(meta)
    nv <- suppressWarnings(as.integer(meta$schema_version))
    if (length(nv) != 1L || is.na(nv) || nv <= v) {
      stop("migration from schema version ", v, " did not advance schema_version", call. = FALSE)
    }
    v <- nv
  }
  meta$schema_version <- v
  meta
}

# ---- lock -----------------------------------------------------------------------------------------

.sr_lock_me <- function() {
  list(host = Sys.info()[["nodename"]], pid = Sys.getpid(), user = Sys.info()[["user"]], ts = .sr_now())
}

# The parsed lock, NULL when there is no lock file, or list(unreadable = TRUE) for a damaged one.
.sr_lock_read <- function(path) {
  if (!file.exists(path)) return(NULL)
  l <- tryCatch(.sr_read_json_file(path), error = function(e) NULL)
  if (!is.list(l) || !is.character(l$host) || length(l$host) != 1L ||
      !.sr_pid_valid(l$pid)) {
    return(list(unreadable = TRUE))
  }
  l
}

# TRUE when `pid` is a valid PID. Out-of-range, fractional or NA values are not PIDs.
.sr_pid_valid <- function(pid) {
  is.numeric(pid) && length(pid) == 1L && is.finite(pid) && pid >= 1 && pid == floor(pid) &&
    pid <= .Machine$integer.max
}

# Only a PID that does not exist (or is not a valid PID) is "not running". Any other probe error, e.g.
# access denied for an elevated, SYSTEM or another user's process, is assumed to mean it is running.
.sr_pid_running <- function(pid) {
  if (!.sr_pid_valid(pid)) return(FALSE)
  tryCatch(isTRUE(ps::ps_is_running(ps::ps_handle(as.integer(pid)))),
           error = function(e) !inherits(e, "no_such_process"))
}

.sr_lock_is_mine <- function(l) {
  is.list(l) && is.null(l$unreadable) &&
    identical(l$host, Sys.info()[["nodename"]]) && identical(suppressWarnings(as.integer(l$pid)), Sys.getpid())
}

# The previous lock's fields as an audit detail.
.sr_lock_detail <- function(l) {
  if (isTRUE(l$unreadable)) return(list(unreadable = TRUE))
  one <- function(v, f) if (is.null(v) || length(v) != 1L) NA else f(v)
  list(host = one(l$host, as.character), pid = one(l$pid, as.integer),
       user = one(l$user, as.character), ts = one(l$ts, as.character))
}

# Decide and (if free) take the lock. Returns list(held, takeover, written, prev): `takeover` is the
# previous lock's detail when we replaced a stale, damaged or forced one, else NULL; `written` is TRUE
# when this call wrote the lock file, and `prev` is the bytes it replaced (NULL when there was none),
# so a caller that fails afterwards can give the lock back (.sr_lock_restore).
.sr_lock_acquire <- function(lock_path, force = FALSE) {
  cur <- .sr_lock_read(lock_path)
  if (is.null(cur)) {
    .sr_write_json_file(lock_path, .sr_lock_me())
    return(list(held = TRUE, takeover = NULL, written = TRUE, prev = NULL))
  }
  if (.sr_lock_is_mine(cur)) return(list(held = TRUE, takeover = NULL, written = FALSE, prev = NULL))
  stale <- isTRUE(cur$unreadable) ||
    (identical(cur$host, Sys.info()[["nodename"]]) && !.sr_pid_running(cur$pid))
  if (force || stale) {
    prev <- .sr_read_bytes(lock_path)
    .sr_write_json_file(lock_path, .sr_lock_me())
    return(list(held = TRUE, takeover = .sr_lock_detail(cur), written = TRUE, prev = prev))
  }
  list(held = FALSE, takeover = NULL, written = FALSE, prev = NULL)
}

# Undo a .sr_lock_acquire() that wrote the lock: put the replaced lock back, or remove ours.
.sr_lock_restore <- function(lock_path, lock) {
  if (!isTRUE(lock$written)) return(invisible(FALSE))
  if (is.null(lock$prev)) unlink(lock_path) else .sr_write_atomic(lock_path, lock$prev)
  invisible(TRUE)
}

# ---- create / open / close / append ---------------------------------------------------------------

#' Create a new project folder at `dir` (created if missing, must be empty) and open it for writing.
#'
#' @return list(paths, meta, state, readonly = FALSE)
sr_project_create <- function(dir, name, profile = sr_profile(), mode = sr_mode()) {
  force(profile)  # evaluate the lazy defaults first: a bad SR_MODE / missing profile must not leave debris
  force(mode)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !nzchar(name)) {
    stop("name must be a single non-empty string", call. = FALSE)
  }
  if (file.exists(dir) && !dir.exists(dir)) stop("not a directory: ", dir, call. = FALSE)
  if (dir.exists(dir) && length(list.files(dir, all.files = TRUE, no.. = TRUE))) {
    stop("project directory is not empty: ", dir, call. = FALSE)
  }
  if (!dir.exists(dir) && !isTRUE(dir.create(dir, showWarnings = FALSE, recursive = TRUE))) {
    stop("cannot create project directory: ", dir, call. = FALSE)
  }
  paths <- sr_project_paths(dir)
  for (d in c(paths$data, paths$fits, paths$out)) {
    if (!isTRUE(dir.create(d, showWarnings = FALSE))) stop("cannot create ", basename(d), "/ in ", dir, call. = FALSE)
  }

  meta <- list(schema_version = SR_SCHEMA_VERSION, id = .sr_uuid4(), name = name,
               created = .sr_now(), profile = profile, mode = mode)
  .sr_write_json_file(paths$json, meta)
  .sr_lock_acquire(paths$lock)
  ev <- sr_event_append(paths$events, paths$audit, "project.created", meta, actor = .sr_actor())
  list(paths = paths, meta = meta, state = sr_state_replay(list(ev)), readonly = FALSE)
}

#' Open an existing project folder. Paths are rebuilt from `dir`; the schema is migrated; the state is
#' replayed from events.jsonl. Takes the lock, or returns a read-only project when it is held elsewhere.
#' `force = TRUE` takes over a live lock (audited).
sr_project_open <- function(dir, force = FALSE) {
  paths <- sr_project_paths(dir)
  if (!file.exists(paths$json)) stop("no project.json in ", dir, call. = FALSE)
  disk <- .sr_read_json_file(paths$json)
  meta <- .sr_migrate(disk)
  migrated <- !identical(as.integer(disk$schema_version), SR_SCHEMA_VERSION)
  state <- sr_state_replay(sr_events_read(paths$events))

  lock <- .sr_lock_acquire(paths$lock, force = force)
  if (lock$held) {
    # If an audit write fails, give the lock back (and project.json, if migrated) before re-throwing,
    # so a failed open never leaves this process holding a lock it does not know about.
    json0 <- .sr_read_bytes(paths$json)
    json_written <- FALSE
    verify <- tryCatch({
      if (!is.null(lock$takeover)) sr_audit_append(paths$audit, "lock.takeover", .sr_actor(), lock$takeover)
      if (migrated) {
        json_written <- TRUE
        .sr_write_json_file(paths$json, meta)
        sr_audit_append(paths$audit, "schema.migrated", .sr_actor(),
                        list(from = as.integer(disk$schema_version), to = SR_SCHEMA_VERSION))
      }
      .sr_project_recover(paths)
    }, error = function(err) {
      if (json_written) try(.sr_write_atomic(paths$json, json0), silent = TRUE)
      try(.sr_lock_restore(paths$lock, lock), silent = TRUE)
      stop(err)
    })
  }
  if (!lock$held) verify <- sr_project_verify(list(paths = paths))
  list(paths = paths, meta = meta, state = state, readonly = !lock$held, verify = verify)
}

#' Cross-check a project's events.jsonl against its audit log.
#'
#' Checks that the audit hash chain verifies, that event seqs run 1, 2, 3, ... with no gap, and that
#' every event line has exactly one audit entry ("event:<type>" or "events.recovered") whose seq, type,
#' payload hash and line hash match, and every such audit entry has its event. A single trailing event
#' with no audit entry is tolerated (a crash between the two writes) and reported as `unaudited`.
#' Torn final fragments of either file are ignored (they were never committed).
#'
#' @return list(ok, problems = character(), unaudited = seq or NULL)
sr_project_verify <- function(project) {
  paths <- project$paths
  problems <- character()
  av <- sr_audit_verify(paths$audit)
  if (!isTRUE(av$ok)) {
    problems <- c(problems, sprintf("audit.log hash chain broken at line %d", av$broken_at))
  }
  ld <- tryCatch(.sr_events_load(paths$events), error = function(e) e)
  if (inherits(ld, "error")) {
    return(list(ok = FALSE, problems = c(problems, conditionMessage(ld)), unaudited = NULL))
  }
  entries <- lapply(.sr_audit_lines(paths$audit), .sr_audit_parse)
  chk <- .sr_events_audit_check(ld, entries)
  problems <- c(problems, chk$problems)
  list(ok = !length(problems), problems = problems, unaudited = chk$unaudited)
}

# Writable open: verify, and record a single trailing unaudited event as "events.recovered" (only when
# that is the sole finding). Returns the (re-run) verification.
.sr_project_recover <- function(paths) {
  v <- sr_project_verify(list(paths = paths))
  if (!v$ok || is.null(v$unaudited)) return(v)
  ld <- .sr_events_load(paths$events)
  n <- length(ld$events)
  e <- ld$events[[n]]
  d <- .sr_event_audit_detail(ld$lines[n], e)
  sr_audit_append(paths$audit, "events.recovered", .sr_actor(),
                  list(seq = d$seq, type = e$type, payload_sha = d$payload_sha, event_sha = d$event_sha))
  sr_project_verify(list(paths = paths))
}

#' Release the lock if this process holds it. Returns TRUE when a lock was removed.
sr_project_close <- function(project) {
  l <- .sr_lock_read(project$paths$lock)
  if (.sr_lock_is_mine(l)) {
    return(invisible(isTRUE(file.remove(project$paths$lock))))
  }
  invisible(FALSE)
}

#' Append an event to a writable project; returns the project with `$state` updated.
sr_project_append <- function(project, type, payload = list()) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  ev <- sr_event_append(project$paths$events, project$paths$audit, type, payload, actor = .sr_actor())
  project$state <- .sr_reducer_get(type)(project$state, ev)
  project
}
