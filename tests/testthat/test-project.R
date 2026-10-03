# Tests for the portable project folder, schema migration and lock (core/project.R).

new_dir <- function() file.path(withr::local_tempdir(.local_envir = parent.frame()), "proj")
this_host <- function() Sys.info()[["nodename"]]
norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)

write_lock <- function(paths, host, pid, user = "someone", ts = "2026-01-01T00:00:00.000Z") {
  .sr_write_json_file(paths$lock, list(host = host, pid = pid, user = user, ts = ts))
}
audit_actions <- function(paths) sr_audit_read(paths$audit)$action

# Create, add a note and close: returns the (closed) project.
make_closed <- function(dir, name = "demo") {
  p <- sr_project_create(dir, name, profile = "laptop")
  p <- sr_project_append(p, "note", list(text = "a"))
  sr_project_close(p)
  p
}

test_that("paths are relative to dir and normalised", {
  d <- new_dir()
  dir.create(d)
  p <- sr_project_paths(d)
  expect_named(p, c("dir", "json", "events", "audit", "data", "fits", "out", "lock"))
  expect_equal(p$dir, norm(d))
  expect_equal(p$json, paste0(norm(d), "/project.json"))
  expect_equal(p$events, paste0(norm(d), "/events.jsonl"))
  expect_equal(p$audit, paste0(norm(d), "/audit.log"))
  expect_equal(p$lock, paste0(norm(d), "/.lock"))
  expect_equal(p$data, paste0(norm(d), "/data"))
  expect_equal(p$fits, paste0(norm(d), "/fits"))
  expect_equal(p$out, paste0(norm(d), "/out"))
})

test_that("create lays out the folder, meta and lock; reopen replays state", {
  d <- new_dir()
  name <- paste0("Caf", intToUtf8(0xE9), " study")
  p <- sr_project_create(d, name, profile = "laptop")
  expect_false(p$readonly)
  expect_true(dir.exists(p$paths$data) && dir.exists(p$paths$fits) && dir.exists(p$paths$out))
  expect_true(file.exists(p$paths$json) && file.exists(p$paths$events) && file.exists(p$paths$audit))
  expect_true(file.exists(p$paths$lock))
  expect_identical(p$meta$schema_version, 1L)
  expect_match(p$meta$id, "^[0-9a-f]{8}-[0-9a-f]{4}-4[0-9a-f]{3}-[89ab][0-9a-f]{3}-[0-9a-f]{12}$")
  expect_equal(p$meta$name, name)
  expect_equal(p$meta$profile, "laptop")
  expect_equal(p$meta$mode, sr_mode())
  expect_match(p$meta$created, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z$")
  expect_equal(p$state$project$name, name)

  p <- sr_project_append(p, "note", list(text = "a"))
  expect_equal(p$state$notes, "a")
  p <- sr_project_append(p, "note", list(text = "b"))
  expect_equal(p$state$notes, c("a", "b"))
  sr_project_close(p)
  expect_false(file.exists(p$paths$lock))

  q <- sr_project_open(d)
  expect_false(q$readonly)
  expect_equal(q$state$notes, c("a", "b"))
  expect_equal(q$state$project$name, name)
  expect_equal(q$meta, p$meta)
  expect_true(sr_audit_verify(q$paths$audit)$ok)
  expect_equal(audit_actions(q$paths), c("event:project.created", "event:note", "event:note"))
  sr_project_close(q)
})

test_that("project.json and the lock are one-line UTF-8 JSON with no absolute paths", {
  d <- new_dir()
  p <- sr_project_create(d, "demo", profile = "laptop")
  for (f in c(p$paths$json, p$paths$lock)) {
    raw <- readBin(f, "raw", n = file.size(f))
    expect_equal(sum(raw == as.raw(0x0A)), 1L)
    expect_equal(raw[length(raw)], as.raw(0x0A))
    expect_true(jsonlite::validate(rawToChar(raw[-length(raw)])))
  }
  expect_false(grepl(norm(d), paste(readLines(p$paths$json, warn = FALSE), collapse = ""), fixed = TRUE))
  expect_false(grepl(norm(d), paste(readLines(p$paths$events, warn = FALSE), collapse = ""), fixed = TRUE))
  p <- sr_project_append(p, "note", list(text = "a"))
  # no takeover happened, so nothing in the folder's history may name this host or path
  for (f in c(p$paths$json, p$paths$events, p$paths$audit)) {
    txt <- paste(readLines(f, warn = FALSE), collapse = "")
    expect_false(grepl(norm(d), txt, fixed = TRUE))
    expect_false(grepl(this_host(), txt, fixed = TRUE))
  }
  lock <- jsonlite::fromJSON(p$paths$lock, simplifyVector = FALSE)
  expect_named(lock, c("host", "pid", "user", "ts"))
  expect_equal(lock$host, this_host())
  expect_equal(as.integer(lock$pid), Sys.getpid())
  sr_project_close(p)
})

test_that("each project gets a distinct id", {
  a <- sr_project_create(new_dir(), "a", profile = "laptop")
  b <- sr_project_create(new_dir(), "b", profile = "laptop")
  expect_false(a$meta$id == b$meta$id)
  sr_project_close(a)
  sr_project_close(b)
})

test_that("create errors on a non-empty directory but accepts an empty or missing one", {
  d <- new_dir()
  dir.create(d)
  writeLines("x", file.path(d, ".hidden"))
  expect_error(sr_project_create(d, "demo", profile = "laptop"))
  d2 <- new_dir()
  dir.create(d2)
  p <- sr_project_create(d2, "demo", profile = "laptop")
  expect_true(file.exists(p$paths$json))
  sr_project_close(p)
  d3 <- file.path(new_dir(), "deep", "er")
  p3 <- sr_project_create(d3, "demo", profile = "laptop")
  expect_true(file.exists(p3$paths$json))
  sr_project_close(p3)
})

test_that("a moved project opens at the new root with identical state and audit tip", {
  d <- new_dir()
  p <- make_closed(d)
  tip <- sr_audit_tip(p$paths$audit)
  root2 <- withr::local_tempdir()
  expect_true(all(file.copy(d, root2, recursive = TRUE)))
  d2 <- file.path(root2, basename(d))
  q <- sr_project_open(d2)
  expect_equal(q$paths$dir, norm(d2))
  expect_false(q$readonly)
  expect_equal(q$state, p$state)
  expect_equal(q$meta, p$meta)
  expect_equal(sr_audit_tip(q$paths$audit), tip)
  sr_project_close(q)
})

test_that("a stale lock from this host is taken over and audited", {
  d <- new_dir()
  p <- make_closed(d)
  write_lock(p$paths, this_host(), 999999L, user = "ghost")
  q <- sr_project_open(d)
  expect_false(q$readonly)
  a <- sr_audit_read(q$paths$audit)
  i <- which(a$action == "lock.takeover")
  expect_length(i, 1L)
  expect_equal(a$detail[[i]]$host, this_host())
  expect_equal(a$detail[[i]]$pid, 999999L)
  expect_equal(a$detail[[i]]$user, "ghost")
  expect_equal(a$detail[[i]]$ts, "2026-01-01T00:00:00.000Z")
  expect_true(sr_audit_verify(q$paths$audit)$ok)
  lock <- jsonlite::fromJSON(q$paths$lock, simplifyVector = FALSE)
  expect_equal(as.integer(lock$pid), Sys.getpid())
  sr_project_close(q)
})

test_that("a live lock from another host makes the project read-only", {
  d <- new_dir()
  p <- make_closed(d)
  write_lock(p$paths, "OTHERHOST", 1L)
  tip <- sr_audit_tip(p$paths$audit)
  q <- sr_project_open(d)
  expect_true(q$readonly)
  expect_equal(q$state$notes, "a")
  expect_error(sr_project_append(q, "note", list(text = "b")), "read-only")
  expect_equal(sr_audit_tip(q$paths$audit), tip)
  # closing a read-only project must not remove someone else's lock
  sr_project_close(q)
  expect_true(file.exists(q$paths$lock))
  expect_equal(jsonlite::fromJSON(q$paths$lock, simplifyVector = FALSE)$host, "OTHERHOST")
})

test_that("a running process on this host holds the lock; once it is gone the lock is stale", {
  exe <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  child <- processx::process$new(exe, c("-e", "Sys.sleep(30)"))
  withr::defer(if (child$is_alive()) child$kill())
  expect_true(child$is_alive())
  d <- new_dir()
  p <- make_closed(d)
  write_lock(p$paths, this_host(), as.integer(child$get_pid()))
  q <- sr_project_open(d)
  expect_true(q$readonly)
  expect_false("lock.takeover" %in% audit_actions(q$paths))
  child$kill()
  child$wait()
  q2 <- sr_project_open(d)
  expect_false(q2$readonly)
  expect_true("lock.takeover" %in% audit_actions(q2$paths))
  sr_project_close(q2)
})

test_that("a process we cannot open (access denied) is assumed to be running", {
  probe <- tryCatch({ ps::ps_handle(4L); NULL }, error = function(e) e)
  skip_if(inherits(probe, "no_such_process"), "PID 4 does not exist here")
  d <- new_dir()
  p <- make_closed(d)
  write_lock(p$paths, this_host(), 4L)
  q <- sr_project_open(d)
  expect_true(q$readonly)
  expect_false("lock.takeover" %in% audit_actions(q$paths))
})

test_that(".sr_pid_running: only a nonexistent process is not running", {
  expect_false(.sr_pid_running(999999L))
  expect_false(.sr_pid_running(NA))
  expect_false(.sr_pid_running(1e10))
  expect_true(.sr_pid_running(Sys.getpid()))
})

test_that("a lock with an out-of-range pid is treated as damaged, without warnings", {
  d <- new_dir()
  p <- make_closed(d)
  write_lock(p$paths, this_host(), 1e10)
  expect_no_warning(q <- sr_project_open(d))
  expect_false(q$readonly)
  a <- sr_audit_read(q$paths$audit)
  expect_true("lock.takeover" %in% a$action)
  sr_project_close(q)
})

test_that("force takes over a live foreign lock and audits it", {
  d <- new_dir()
  p <- make_closed(d)
  write_lock(p$paths, "OTHERHOST", 1L, user = "someone")
  q <- sr_project_open(d, force = TRUE)
  expect_false(q$readonly)
  a <- sr_audit_read(q$paths$audit)
  i <- which(a$action == "lock.takeover")
  expect_length(i, 1L)
  expect_equal(a$detail[[i]]$host, "OTHERHOST")
  sr_project_append(q, "note", list(text = "b"))
  sr_project_close(q)
  expect_false(file.exists(q$paths$lock))
})

test_that("reopening in the same process is not locked out; close releases only our lock", {
  d <- new_dir()
  p <- sr_project_create(d, "demo", profile = "laptop")
  q <- sr_project_open(d)
  expect_false(q$readonly)
  expect_false("lock.takeover" %in% audit_actions(q$paths))
  sr_project_close(q)
  expect_false(file.exists(p$paths$lock))
  # a foreign lock written after we closed survives a stray close
  write_lock(p$paths, "OTHERHOST", 1L)
  sr_project_close(p)
  expect_true(file.exists(p$paths$lock))
})

test_that("a lock that cannot be parsed is treated as stale", {
  d <- new_dir()
  p <- make_closed(d)
  writeLines("not json", p$paths$lock)
  q <- sr_project_open(d)
  expect_false(q$readonly)
  expect_true("lock.takeover" %in% audit_actions(q$paths))
  sr_project_close(q)
})

test_that("opening a folder without project.json errors", {
  d <- withr::local_tempdir()
  expect_error(sr_project_open(d), "project.json")
})

test_that("a newer schema errors and leaves no lock behind", {
  d <- new_dir()
  p <- make_closed(d)
  meta <- .sr_read_json_file(p$paths$json)
  meta$schema_version <- 99L
  .sr_write_json_file(p$paths$json, meta)
  expect_error(sr_project_open(d), "newer StatRouter")
  expect_false(file.exists(p$paths$lock))
})

test_that("migrations run from the file's version, are written back and audited", {
  fake <- list("0" = function(meta) {
    meta$schema_version <- 1L
    meta$migrated <- TRUE
    meta
  })
  env <- environment(.sr_migrate)
  old <- get("sr_schema_migrations", envir = env)
  assign("sr_schema_migrations", fake, envir = env)
  withr::defer(assign("sr_schema_migrations", old, envir = env))

  d <- new_dir()
  p <- make_closed(d)
  meta <- .sr_read_json_file(p$paths$json)
  meta$schema_version <- 0L
  .sr_write_json_file(p$paths$json, meta)

  q <- sr_project_open(d)
  expect_false(q$readonly)
  expect_identical(q$meta$schema_version, 1L)
  expect_true(q$meta$migrated)
  expect_identical(.sr_read_json_file(q$paths$json)$schema_version, 1L)
  a <- sr_audit_read(q$paths$audit)
  i <- which(a$action == "schema.migrated")
  expect_length(i, 1L)
  expect_equal(a$detail[[i]]$from, 0L)
  expect_equal(a$detail[[i]]$to, 1L)
  expect_true(sr_audit_verify(q$paths$audit)$ok)
  sr_project_close(q)
})

test_that("a read-only open migrates in memory only", {
  fake <- list("0" = function(meta) {
    meta$schema_version <- 1L
    meta
  })
  env <- environment(.sr_migrate)
  old <- get("sr_schema_migrations", envir = env)
  assign("sr_schema_migrations", fake, envir = env)
  withr::defer(assign("sr_schema_migrations", old, envir = env))

  d <- new_dir()
  p <- make_closed(d)
  meta <- .sr_read_json_file(p$paths$json)
  meta$schema_version <- 0L
  .sr_write_json_file(p$paths$json, meta)
  write_lock(p$paths, "OTHERHOST", 1L)
  tip <- sr_audit_tip(p$paths$audit)

  q <- sr_project_open(d)
  expect_true(q$readonly)
  expect_identical(q$meta$schema_version, 1L)
  expect_identical(.sr_read_json_file(q$paths$json)$schema_version, 0L)
  expect_equal(sr_audit_tip(q$paths$audit), tip)
})

test_that(".sr_migrate requires a migration for every step and progress", {
  expect_error(.sr_migrate(list(schema_version = 0L), migrations = list()), "no migration")
  stuck <- list("0" = function(meta) meta)
  expect_error(.sr_migrate(list(schema_version = 0L), migrations = stuck), "did not advance")
  expect_identical(.sr_migrate(list(schema_version = 1L), migrations = list())$schema_version, 1L)
  expect_error(.sr_migrate(list(schema_version = 2L), migrations = list()), "newer StatRouter")
})

test_that("a failing create leaves no debris, so a retry works", {
  d <- new_dir()
  withr::with_envvar(c(SR_MODE = "bogus"), {
    expect_error(sr_project_create(d, "demo", profile = "laptop"), "airgap")
  })
  expect_true(!dir.exists(d) || length(list.files(d, all.files = TRUE, no.. = TRUE)) == 0L)
  p <- sr_project_create(d, "demo", profile = "laptop")
  expect_false(p$readonly)
  sr_project_close(p)
})

# ---- final review I2: project.json and .lock are parsed as JSON text only ------------------------

test_that("a .lock naming a file or a URL is unreadable and is never read or fetched (I2)", {
  d <- new_dir()
  p <- make_closed(d)
  # a decoy holding a live foreign lock: reading it would make the open read-only
  decoy <- file.path(dirname(d), "decoy.json")
  .sr_write_json_file(decoy, list(host = "OTHERHOST", pid = 1L, user = "x", ts = "2026-01-01T00:00:00.000Z"))
  marker <- "https://example.invalid/lock.json"
  spy <- local_fetch_spy(c(decoy, marker))
  writeBin(charToRaw(decoy), p$paths$lock)
  expect_true(isTRUE(.sr_lock_read(p$paths$lock)$unreadable))
  q <- sr_project_open(d)
  expect_false(q$readonly)
  expect_true("lock.takeover" %in% audit_actions(q$paths))
  sr_project_close(q)

  writeBin(charToRaw(marker), p$paths$lock)
  expect_true(isTRUE(.sr_lock_read(p$paths$lock)$unreadable))
  expect_identical(spy$calls, character())
})

test_that("a project.json holding a URL fails to open and is never fetched (I2)", {
  d <- new_dir()
  p <- make_closed(d)
  marker <- "https://example.invalid/project.json"
  writeBin(charToRaw(marker), p$paths$json)
  spy <- local_fetch_spy(marker)
  expect_error(sr_project_open(d))
  expect_identical(spy$calls, character())
})

# ---- final review M1: project.json and .lock are replaced atomically -------------------------------

test_that("project.json and .lock writes go through <path>.tmp + rename and leave no .tmp", {
  d <- new_dir()
  spy <- local_rename_spy()
  p <- sr_project_create(d, "demo", profile = "laptop")
  expect_true(all(c(norm(p$paths$json), norm(p$paths$lock)) %in% spy$to))
  expect_false(any(grepl("[.]tmp$", list.files(d, all.files = TRUE, recursive = TRUE))))
  expect_equal(.sr_read_json_file(p$paths$json)$name, "demo")
  sr_project_close(p)
})

# ---- final review I1: a failed audit write on open gives the lock (and project.json) back ---------

test_that("a failing lock.takeover audit restores the previous lock and re-throws (I1)", {
  d <- new_dir()
  p <- make_closed(d)
  write_lock(p$paths, this_host(), 999999L, user = "ghost")
  stale <- readBin(p$paths$lock, "raw", n = file.size(p$paths$lock))
  local({
    local_failing_audit("^lock[.]takeover$")
    expect_error(sr_project_open(d), "simulated audit write failure")
  })
  expect_identical(readBin(p$paths$lock, "raw", n = file.size(p$paths$lock)), stale)
  expect_false(file.exists(paste0(p$paths$lock, ".tmp")))
  q <- sr_project_open(d)                          # a retry takes the stale lock over normally
  expect_false(q$readonly)
  expect_true("lock.takeover" %in% audit_actions(q$paths))
  sr_project_close(q)
})

test_that("a failing schema.migrated audit releases the new lock and restores project.json (I1)", {
  fake <- list("0" = function(meta) {
    meta$schema_version <- 1L
    meta
  })
  env <- environment(.sr_migrate)
  old <- get("sr_schema_migrations", envir = env)
  assign("sr_schema_migrations", fake, envir = env)
  withr::defer(assign("sr_schema_migrations", old, envir = env))

  d <- new_dir()
  p <- make_closed(d)
  meta <- .sr_read_json_file(p$paths$json)
  meta$schema_version <- 0L
  .sr_write_json_file(p$paths$json, meta)
  json0 <- readBin(p$paths$json, "raw", n = file.size(p$paths$json))
  local({
    local_failing_audit("^schema[.]migrated$")
    expect_error(sr_project_open(d), "simulated audit write failure")
  })
  expect_false(file.exists(p$paths$lock))
  expect_identical(readBin(p$paths$json, "raw", n = file.size(p$paths$json)), json0)
  q <- sr_project_open(d)
  expect_false(q$readonly)
  expect_true("schema.migrated" %in% audit_actions(q$paths))
  sr_project_close(q)
})

# ---- final review I3: events.jsonl is cross-checked against the audit log --------------------------

ev_lines <- function(paths) .sr_split_lf(.sr_read_bytes(paths$events))$lines
put_ev_lines <- function(paths, lines) {
  writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), paths$events)
}
# A project with events 1..3 (project.created, note a, note b), closed.
make_three <- function(dir) {
  p <- make_closed(dir)
  p <- sr_project_open(dir)
  p <- sr_project_append(p, "note", list(text = "b"))
  sr_project_close(p)
  p
}

test_that("a clean project verifies ok, on open and directly (I3)", {
  d <- new_dir()
  make_three(d)
  q <- sr_project_open(d)
  expect_true(q$verify$ok)
  expect_identical(q$verify$problems, character())
  v <- sr_project_verify(q)
  expect_true(v$ok)
  expect_identical(v$problems, character())
  sr_project_close(q)
})

test_that("an edited event payload is not ok and names the seq; open still succeeds (I3)", {
  d <- new_dir()
  p <- make_three(d)
  l <- ev_lines(p$paths)
  l[2] <- sub("\"text\":\"a\"", "\"text\":\"z\"", l[2], fixed = TRUE)
  put_ev_lines(p$paths, l)
  q <- sr_project_open(d)
  expect_false(q$readonly)
  expect_false(q$verify$ok)
  expect_true(any(grepl("seq 2", q$verify$problems, fixed = TRUE)))
  expect_false(any(grepl("seq 3", q$verify$problems, fixed = TRUE)))
  expect_false("events.recovered" %in% audit_actions(q$paths))
  sr_project_close(q)
})

test_that("an edited event actor or ts is not ok (the audit entry pins the whole line) (I3)", {
  d <- new_dir()
  p <- make_three(d)
  l <- ev_lines(p$paths)
  l[3] <- sub("\"ts\":\"[^\"]*\"", "\"ts\":\"2020-01-01T00:00:00.000Z\"", l[3])
  put_ev_lines(p$paths, l)
  v <- sr_project_verify(list(paths = p$paths))
  expect_false(v$ok)
  expect_true(any(grepl("seq 3", v$problems, fixed = TRUE)))
})

test_that("a deleted event line is not ok (seq gap and an audit entry with no event) (I3)", {
  d <- new_dir()
  p <- make_three(d)
  put_ev_lines(p$paths, ev_lines(p$paths)[-2])
  v <- sr_project_verify(list(paths = p$paths))
  expect_false(v$ok)
  expect_true(any(grepl("gap", v$problems, fixed = TRUE)))
  expect_true(any(grepl("seq 2", v$problems, fixed = TRUE)))
})

test_that("a deleted last event line is not ok (its audit entry has no event) (I3)", {
  d <- new_dir()
  p <- make_three(d)
  put_ev_lines(p$paths, ev_lines(p$paths)[-3])
  v <- sr_project_verify(list(paths = p$paths))
  expect_false(v$ok)
  expect_true(any(grepl("seq 3", v$problems, fixed = TRUE)))
})

test_that("one trailing unaudited event is ok and recorded as events.recovered on the next writable open (I3)", {
  d <- new_dir()
  p <- make_three(d)
  line <- .sr_json(list(seq = 4L, ts = "2026-01-01T00:00:00.000Z", type = "note", actor = "alice",
                        payload = list(text = "c")))
  put_ev_lines(p$paths, c(ev_lines(p$paths), line))   # crash after the event line, before its audit
  v <- sr_project_verify(list(paths = p$paths))
  expect_true(v$ok)
  expect_equal(v$unaudited, 4L)

  # a read-only open tolerates it but writes nothing
  write_lock(p$paths, "other-host", 4242L)
  r <- sr_project_open(d)
  expect_true(r$readonly)
  expect_true(r$verify$ok)
  expect_false("events.recovered" %in% audit_actions(p$paths))
  unlink(p$paths$lock)

  q <- sr_project_open(d)
  expect_true(q$verify$ok)
  expect_null(q$verify$unaudited)
  a <- sr_audit_read(q$paths$audit)
  i <- which(a$action == "events.recovered")
  expect_length(i, 1L)
  expect_equal(a$detail[[i]]$seq, 4L)
  expect_equal(a$detail[[i]]$type, "note")
  expect_equal(a$detail[[i]]$payload_sha, .sr_sha256(.sr_json(list(text = "c"))))
  expect_equal(q$state$notes, c("a", "b", "c"))
  sr_project_close(q)

  q <- sr_project_open(d)                            # recovered once, never again
  expect_true(q$verify$ok)
  expect_equal(sum(audit_actions(q$paths) == "events.recovered"), 1L)
  q <- sr_project_append(q, "note", list(text = "d"))
  expect_true(sr_project_verify(q)$ok)
  sr_project_close(q)
})

test_that("two trailing unaudited events are not ok and are not recovered (I3)", {
  d <- new_dir()
  p <- make_three(d)
  mk <- function(s) .sr_json(list(seq = s, ts = "2026-01-01T00:00:00.000Z", type = "note",
                                  actor = "alice", payload = list(text = "c")))
  put_ev_lines(p$paths, c(ev_lines(p$paths), mk(4L), mk(5L)))
  q <- sr_project_open(d)
  expect_false(q$verify$ok)
  expect_true(any(grepl("seq 4", q$verify$problems, fixed = TRUE)))
  expect_false("events.recovered" %in% audit_actions(q$paths))
  sr_project_close(q)
})

test_that("a broken audit chain or an unreadable events.jsonl is not ok (I3)", {
  d <- new_dir()
  p <- make_three(d)
  au <- readLines(p$paths$audit, warn = FALSE)
  au[1] <- sub("\"actor\":\"[^\"]*\"", "\"actor\":\"mallory\"", au[1])
  writeBin(charToRaw(paste0(paste(au, collapse = "\n"), "\n")), p$paths$audit)
  v <- sr_project_verify(list(paths = p$paths))
  expect_false(v$ok)
  expect_true(any(grepl("audit.log", v$problems, fixed = TRUE)))

  d2 <- new_dir()
  p2 <- make_three(d2)
  put_ev_lines(p2$paths, c(ev_lines(p2$paths)[1:2], "not json", ev_lines(p2$paths)[3]))
  v2 <- sr_project_verify(list(paths = p2$paths))
  expect_false(v2$ok)
  expect_true(any(grepl("events.jsonl", v2$problems, fixed = TRUE)))
})

test_that("a crash mid audit write is repaired and recovered on the next open (I1 + I3)", {
  d <- new_dir()
  p <- make_three(d)
  b <- readBin(p$paths$audit, "raw", n = file.size(p$paths$audit))
  lf <- which(b == as.raw(0x0A))
  writeBin(b[seq_len(lf[length(lf) - 1L] + 30L)], p$paths$audit)  # event 3's entry torn
  q <- sr_project_open(d)
  expect_true(q$verify$ok)
  acts <- audit_actions(q$paths)
  expect_equal(tail(acts, 2), c("audit.repair", "events.recovered"))
  expect_true(sr_audit_verify(q$paths$audit)$ok)
  expect_equal(q$state$notes, c("a", "b"))
  q <- sr_project_append(q, "note", list(text = "c"))
  expect_true(sr_project_verify(q)$ok)
  sr_project_close(q)
})
