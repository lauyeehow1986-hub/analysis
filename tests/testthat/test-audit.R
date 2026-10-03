# Tests for the hash-chained audit log (core/audit.R).

new_log <- function() file.path(withr::local_tempdir(.local_envir = parent.frame()), "audit.log")

append3 <- function(p) {
  h1 <- sr_audit_append(p, "open", "alice", list(file = "a.csv", n = 10L))
  h2 <- sr_audit_append(p, "analyse", "alice", list(model = "lm", ok = TRUE))
  h3 <- sr_audit_append(p, "export", "bob", list())
  c(h1, h2, h3)
}

read_raw_lines <- function(p) readLines(p, encoding = "UTF-8", warn = FALSE)
write_raw_lines <- function(x, p) writeLines(enc2utf8(x), p, useBytes = TRUE)

test_that("three appends chain and verify", {
  p <- new_log()
  h <- append3(p)
  expect_length(unique(h), 3L)
  expect_true(all(grepl("^[0-9a-f]{64}$", h)))
  v <- sr_audit_verify(p)
  expect_true(v$ok)
  expect_equal(v$n, 3L)
  expect_true(is.na(v$broken_at))
  d <- sr_audit_read(p)
  expect_equal(nrow(d), 3L)
  expect_equal(d$prev_hash[1], "GENESIS")
  expect_equal(d$prev_hash[2], d$hash[1])
  expect_equal(d$prev_hash[3], d$hash[2])
  expect_equal(sr_audit_tip(p), d$hash[3])
  expect_equal(sr_audit_tip(p), h[3])
  expect_equal(d$action, c("open", "analyse", "export"))
  expect_equal(d$actor, c("alice", "alice", "bob"))
  expect_type(d$detail, "list")
  expect_equal(d$detail[[1]]$file, "a.csv")
})

test_that("stored line has the documented shape and hash formula", {
  p <- new_log()
  h <- sr_audit_append(p, "open", "alice", list(k = "v"))
  e <- jsonlite::fromJSON(read_raw_lines(p)[1], simplifyVector = FALSE)
  expect_equal(names(e), c("ts", "action", "actor", "detail", "prev_hash", "hash"))
  expect_equal(e$hash, h)
  expect_equal(e$prev_hash, "GENESIS")
  expect_match(e$ts, "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[.][0-9]{3}Z$")
  want <- .sr_sha256(paste0("GENESIS", "|",
    .sr_json(list(ts = e$ts, action = e$action, actor = e$actor, detail = e$detail))))
  expect_equal(h, want)
})

test_that("editing one character of an entry breaks the chain at that line", {
  p <- new_log()
  append3(p)
  ln <- read_raw_lines(p)
  ln[2] <- sub("alice", "alicf", ln[2], fixed = TRUE)
  write_raw_lines(ln, p)
  v <- sr_audit_verify(p)
  expect_false(v$ok)
  expect_equal(v$broken_at, 2L)
  expect_equal(v$n, 3L)
})

test_that("deleting a line breaks the chain at the following line", {
  p <- new_log()
  append3(p)
  ln <- read_raw_lines(p)
  write_raw_lines(ln[-2], p)
  v <- sr_audit_verify(p)
  expect_false(v$ok)
  expect_equal(v$broken_at, 2L)
})

test_that("an unparseable line is a break at that line", {
  p <- new_log()
  append3(p)
  ln <- read_raw_lines(p)
  ln[3] <- "this is not json"
  write_raw_lines(ln, p)
  v <- sr_audit_verify(p)
  expect_false(v$ok)
  expect_equal(v$broken_at, 3L)
})

test_that("missing or empty file is a valid empty log", {
  p <- new_log()
  v <- sr_audit_verify(p)
  expect_true(v$ok)
  expect_equal(v$n, 0L)
  expect_true(is.na(v$broken_at))
  expect_equal(sr_audit_tip(p), "GENESIS")
  d <- sr_audit_read(p)
  expect_equal(nrow(d), 0L)
  expect_true(all(c("ts", "action", "actor", "prev_hash", "hash", "detail") %in% names(d)))
  file.create(p)
  expect_true(sr_audit_verify(p)$ok)
  expect_equal(sr_audit_verify(p)$n, 0L)
  expect_equal(sr_audit_tip(p), "GENESIS")
  expect_equal(nrow(sr_audit_read(p)), 0L)
})

test_that("non-ASCII detail round-trips and verifies", {
  p <- new_log()
  mu <- intToUtf8(c(0xB5, 0x6D, 0x6F, 0x6C))
  sr_audit_append(p, "note", "alice", list(unit = mu))
  sr_audit_append(p, "note", "alice", list(unit = mu, n = 2L))
  v <- sr_audit_verify(p)
  expect_true(v$ok)
  expect_equal(v$n, 2L)
  d <- sr_audit_read(p)
  expect_equal(d$detail[[1]]$unit, mu)
})

test_that("tampering with detail is detected", {
  p <- new_log()
  append3(p)
  ln <- read_raw_lines(p)
  ln[1] <- sub("a.csv", "b.csv", ln[1], fixed = TRUE)
  write_raw_lines(ln, p)
  v <- sr_audit_verify(p)
  expect_false(v$ok)
  expect_equal(v$broken_at, 1L)
})

test_that("append creates missing parent directories and rejects a non-list detail", {
  p <- file.path(withr::local_tempdir(), "sub", "dir", "audit.log")
  sr_audit_append(p, "open", "alice")
  expect_true(file.exists(p))
  expect_true(sr_audit_verify(p)$ok)
  expect_error(sr_audit_append(p, "open", "alice", detail = "text"))
})

test_that("an extra key injected into an entry is a break at that line", {
  p <- new_log()
  append3(p)
  ln <- read_raw_lines(p)
  ln[2] <- sub("}$", ",\"extra\":1}", ln[2])
  expect_false(identical(ln[2], read_raw_lines(p)[2]))
  write_raw_lines(ln, p)
  v <- sr_audit_verify(p)
  expect_false(v$ok)
  expect_equal(v$broken_at, 2L)
})

test_that("a duplicated key in an entry is a break at that line", {
  p <- new_log()
  append3(p)
  ln <- read_raw_lines(p)
  ln[2] <- sub("\"actor\":\"alice\"", "\"actor\":\"alice\",\"actor\":\"evil\"", ln[2], fixed = TRUE)
  expect_false(identical(ln[2], read_raw_lines(p)[2]))
  write_raw_lines(ln, p)
  v <- sr_audit_verify(p)
  expect_false(v$ok)
  expect_equal(v$broken_at, 2L)
})
