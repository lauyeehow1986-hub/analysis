# Tests for the event log, reducer registry and replay (core/events.R).

sr_reducer_register("counter.add", function(state, event) {
  state$n <- (state$n %||% 0) + event$payload$k
  state
})

new_paths <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  list(ev = file.path(d, "events.jsonl"), au = file.path(d, "audit.log"))
}

read_bytes <- function(p) readBin(p, "raw", n = file.size(p))
append_bytes <- function(p, x) {
  con <- file(p, "ab")
  on.exit(close(con), add = TRUE)
  writeBin(x, con)
}

add_k <- function(p, k) sr_event_append(p$ev, p$au, "counter.add", list(k = k), actor = "alice")

test_that("append, read and replay fold events in order and audit each one", {
  p <- new_paths()
  e1 <- add_k(p, 1)
  add_k(p, 2)
  add_k(p, 3)
  expect_equal(e1$seq, 1L)
  expect_equal(e1$type, "counter.add")
  expect_equal(e1$actor, "alice")
  expect_match(e1$ts, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9:.]+Z$")
  ev <- sr_events_read(p$ev)
  expect_equal(vapply(ev, function(e) e$seq, numeric(1)), 1:3)
  expect_equal(sr_state_replay(ev)$n, 6)
  v <- sr_audit_verify(p$au)
  expect_true(v$ok)
  expect_equal(v$n, 3L)
  d <- sr_audit_read(p$au)
  expect_equal(d$action, rep("event:counter.add", 3))
  expect_equal(d$detail[[2]]$seq, 2)
  expect_equal(d$detail[[2]]$payload_sha, .sr_sha256(.sr_json(list(k = 2))))
})

test_that("replay orders by seq and honours init", {
  p <- new_paths()
  add_k(p, 1)
  add_k(p, 2)
  ev <- rev(sr_events_read(p$ev))
  st <- sr_state_replay(ev, init = list(n = 10))
  expect_equal(st$n, 13)
})

test_that("an unknown type errors on append and on replay", {
  p <- new_paths()
  expect_error(sr_event_append(p$ev, p$au, "nope.nothing", list(), actor = "alice"), "no reducer")
  expect_false(file.exists(p$ev))
  expect_error(sr_state_replay(list(list(seq = 1, type = "nope.nothing", payload = list()))),
               "no reducer")
})

test_that("registering a type twice replaces the reducer", {
  sr_reducer_register("tmp.twice", function(state, event) { state$v <- 1; state })
  sr_reducer_register("tmp.twice", function(state, event) { state$v <- 2; state })
  st <- sr_state_replay(list(list(seq = 1, type = "tmp.twice", payload = list())))
  expect_equal(st$v, 2)
})

test_that("built-in reducers: project.created and note", {
  p <- new_paths()
  sr_event_append(p$ev, p$au, "project.created", list(name = "demo", mode = "airgap"), actor = "alice")
  sr_event_append(p$ev, p$au, "note", list(text = "first"), actor = "alice")
  sr_event_append(p$ev, p$au, "note", list(text = "second"), actor = "alice")
  st <- sr_state_replay(sr_events_read(p$ev))
  expect_equal(st$project$name, "demo")
  expect_equal(st$notes, c("first", "second"))
})

test_that("a torn tail is ignored on read, repaired on append, and audited (Review Focus 1)", {
  p <- new_paths()
  add_k(p, 1)
  add_k(p, 2)
  add_k(p, 3)
  frag <- charToRaw("{\"seq\":4,\"ts\":\"2026-01-01T00:00:00.000Z\",\"ty")
  append_bytes(p$ev, frag)
  expect_warning(ev <- sr_events_read(p$ev), "torn")
  expect_length(ev, 3L)
  expect_warning(st <- sr_state_replay(sr_events_read(p$ev)), "torn")
  expect_equal(st$n, 6)

  e4 <- add_k(p, 4)
  expect_equal(e4$seq, 4L)
  expect_silent(ev2 <- sr_events_read(p$ev))
  expect_equal(sr_state_replay(ev2)$n, 10)
  expect_equal(read_bytes(p$ev)[length(read_bytes(p$ev))], as.raw(0x0A))

  d <- sr_audit_read(p$au)
  expect_true("events.repair" %in% d$action)
  rep_i <- which(d$action == "events.repair")
  expect_equal(d$detail[[rep_i]]$removed_bytes, length(frag))
  expect_equal(d$detail[[rep_i]]$removed_sha, .sr_sha256(frag))
  expect_true(sr_audit_verify(p$au)$ok)
})

test_that("a file with no newline at all is truncated to zero and restarts at seq 1", {
  p <- new_paths()
  append_bytes(p$ev, charToRaw("{\"seq\":1,\"ty"))
  e <- add_k(p, 5)
  expect_equal(e$seq, 1L)
  expect_equal(sr_state_replay(sr_events_read(p$ev))$n, 5)
  d <- sr_audit_read(p$au)
  expect_equal(d$detail[[which(d$action == "events.repair")]]$removed_bytes, 12L)
})

test_that("a malformed line before the last is an error", {
  p <- new_paths()
  add_k(p, 1)
  add_k(p, 2)
  add_k(p, 3)
  lines <- readLines(p$ev, encoding = "UTF-8", warn = FALSE)
  lines[2] <- "{not json"
  writeLines(lines, p$ev, useBytes = TRUE)
  expect_error(sr_events_read(p$ev), "corrupt events.jsonl at line 2")
  expect_error(add_k(p, 4), "corrupt events.jsonl at line 2")
})

test_that("missing and empty files read as no events", {
  p <- new_paths()
  expect_equal(sr_events_read(p$ev), list())
  file.create(p$ev)
  expect_equal(sr_events_read(p$ev), list())
  expect_equal(sr_state_replay(list(), init = list(a = 1)), list(a = 1))
})

test_that("non-ASCII payload text round-trips identically", {
  p <- new_paths()
  txt <- intToUtf8(c(0xB5, 0x6D, 0x6F, 0x6C))
  sr_event_append(p$ev, p$au, "note", list(text = txt), actor = "alice")
  ev <- sr_events_read(p$ev)
  expect_identical(ev[[1]]$payload$text, txt)
  expect_identical(sr_state_replay(ev)$notes, txt)
  expect_true(sr_audit_verify(p$au)$ok)
})

test_that("the returned event is exactly what a re-read yields", {
  p <- new_paths()
  sr_reducer_register("tmp.shape", function(state, event) state)
  e <- sr_event_append(p$ev, p$au, "tmp.shape",
                       list(a = NA, v = 1:3, e = character(0), s = "x"), actor = "alice")
  expect_identical(e, sr_events_read(p$ev)[[1]])
})

test_that("a valid-JSON final line without its LF is torn on read and repaired on append", {
  p <- new_paths()
  add_k(p, 1)
  add_k(p, 2)
  b <- read_bytes(p$ev)
  expect_equal(b[length(b)], as.raw(0x0A))
  writeBin(b[-length(b)], p$ev)
  expect_warning(ev <- sr_events_read(p$ev), "torn")
  expect_length(ev, 1L)
  e <- add_k(p, 3)
  expect_equal(e$seq, 2L)
  expect_silent(ev2 <- sr_events_read(p$ev))
  expect_equal(sr_state_replay(ev2)$n, 4)
  d <- sr_audit_read(p$au)
  expect_true("events.repair" %in% d$action)
  expect_true(sr_audit_verify(p$au)$ok)
})

# ---- final review M1: the torn-tail repair is an atomic rewrite ------------------------------------

test_that("the events torn-tail repair goes through events.jsonl.tmp + rename and leaves no .tmp", {
  p <- new_paths()
  add_k(p, 1)
  append_bytes(p$ev, charToRaw("{\"seq\":2,\"ty"))
  spy <- local_rename_spy()
  add_k(p, 2)
  expect_true(normalizePath(p$ev, winslash = "/", mustWork = FALSE) %in% spy$to)
  expect_false(file.exists(paste0(p$ev, ".tmp")))
  ev <- sr_events_read(p$ev)
  expect_equal(vapply(ev, function(e) e$seq, numeric(1)), 1:2)
  expect_equal(sr_state_replay(ev)$n, 3)
})

# ---- final review I1: a torn or failed audit write never strands or duplicates events -------------

seqs <- function(p) vapply(sr_events_read(p$ev), function(e) as.integer(e$seq), integer(1))

test_that("a torn audit.log tail is repaired and audited; the append commits exactly one event (I1)", {
  p <- new_paths()
  add_k(p, 1)
  add_k(p, 2)
  add_k(p, 3)
  # crash mid audit write: the last audit line (event 3's) lost its end and its LF
  b <- read_bytes(p$au)
  lf <- which(b == as.raw(0x0A))
  cut <- lf[length(lf) - 1L] + 40L
  writeBin(b[seq_len(cut)], p$au)
  frag <- b[(lf[length(lf) - 1L] + 1L):cut]
  expect_true(sr_audit_verify(p$au)$ok)            # the unterminated fragment is not an entry
  expect_true(sr_audit_verify(p$au)$torn)
  e4 <- add_k(p, 4)
  expect_equal(e4$seq, 4L)
  expect_identical(seqs(p), 1:4)
  v <- sr_audit_verify(p$au)
  expect_true(v$ok)
  expect_false(v$torn)
  d <- sr_audit_read(p$au)
  i <- which(d$action == "audit.repair")
  expect_length(i, 1L)
  expect_equal(d$detail[[i]]$removed_bytes, length(frag))
  expect_equal(d$detail[[i]]$removed_sha, .sr_sha256(frag))
  expect_equal(d$action[length(d$action)], "event:counter.add")
  expect_equal(sr_state_replay(sr_events_read(p$ev))$n, 10)
  expect_false(file.exists(paste0(p$au, ".tmp")))
})

test_that("an audit.log that is only a fragment is truncated to empty and restarts the chain (I1)", {
  p <- new_paths()
  writeBin(charToRaw("{\"ts\":\"2026"), p$au)
  e <- add_k(p, 1)
  expect_equal(e$seq, 1L)
  d <- sr_audit_read(p$au)
  expect_equal(d$action, c("audit.repair", "event:counter.add"))
  expect_equal(d$prev_hash[1], "GENESIS")
  expect_true(sr_audit_verify(p$au)$ok)
})

test_that("a complete but invalid last audit line fails the append before the event is written (I1)", {
  p <- new_paths()
  add_k(p, 1)
  add_k(p, 2)
  append_bytes(p$au, charToRaw("not an entry\n"))
  before <- read_bytes(p$ev)
  expect_error(add_k(p, 3), "audit log is corrupt")
  expect_error(add_k(p, 3), "audit log is corrupt")
  expect_identical(read_bytes(p$ev), before)
  expect_false(sr_audit_verify(p$au)$ok)            # tamper evidence: never repaired away
})

test_that("a failing audit append rolls the event line back; retries never duplicate (I1)", {
  p <- new_paths()
  add_k(p, 1)
  add_k(p, 2)
  before <- read_bytes(p$ev)
  local({
    local_failing_audit("^event:")
    expect_error(add_k(p, 3), "simulated audit write failure")
    expect_identical(read_bytes(p$ev), before)
    expect_error(add_k(p, 3), "simulated audit write failure")
    expect_identical(read_bytes(p$ev), before)
  })
  e <- add_k(p, 3)
  expect_equal(e$seq, 3L)
  expect_identical(seqs(p), 1:3)
  expect_equal(sr_state_replay(sr_events_read(p$ev))$n, 6)
  expect_true(sr_audit_verify(p$au)$ok)
  expect_false(file.exists(paste0(p$ev, ".tmp")))
})

test_that("a torn audit write during an append rolls the event back; the next append repairs (I1)", {
  p <- new_paths()
  add_k(p, 1)
  before <- read_bytes(p$ev)
  local({
    local_failing_audit("^event:", torn = TRUE)
    expect_error(add_k(p, 2), "simulated audit write failure")
  })
  expect_identical(read_bytes(p$ev), before)
  e <- add_k(p, 2)
  expect_equal(e$seq, 2L)
  expect_identical(seqs(p), 1:2)
  d <- sr_audit_read(p$au)
  expect_equal(d$action, c("event:counter.add", "audit.repair", "event:counter.add"))
  expect_true(sr_audit_verify(p$au)$ok)
})

test_that("the first event's failed audit removes the events file it created (I1)", {
  p <- new_paths()
  local({
    local_failing_audit("^event:")
    expect_error(add_k(p, 1), "simulated audit write failure")
  })
  expect_false(file.exists(p$ev))
  expect_equal(add_k(p, 1)$seq, 1L)
})
