test_that("sr_opt resolves option, env var, then default", {
  expect_identical(sr_opt("zz", "d"), "d")
  withr::with_envvar(c(SR_ZZ = "e"), expect_identical(sr_opt("zz", "d"), "e"))
  withr::with_envvar(c(SR_ZZ = "e"), {
    withr::with_options(list(sr.zz = "o"), expect_identical(sr_opt("zz", "d"), "o"))
  })
})

test_that("sr_opt treats an empty env string as unset", {
  withr::with_envvar(c(SR_ZZ = ""), expect_identical(sr_opt("zz", "d"), "d"))
})

test_that("sr_mode defaults to airgap and rejects other values", {
  withr::with_envvar(c(SR_MODE = NA), expect_identical(sr_mode(), "airgap"))
  withr::with_envvar(c(SR_MODE = "connected"), expect_identical(sr_mode(), "connected"))
  withr::with_envvar(c(SR_MODE = "bogus"), expect_error(sr_mode()))
})

test_that(".sr_sha256 hashes character and raw input", {
  expect_identical(.sr_sha256("abc"),
    "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
  expect_identical(.sr_sha256(charToRaw("abc")), .sr_sha256("abc"))
})

test_that(".sr_now is an ISO UTC millisecond timestamp", {
  expect_match(.sr_now(), "^[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}[.][0-9]{3}Z$")
})

test_that(".sr_json is one-line, unboxed, null for NA", {
  expect_identical(.sr_json(list(a = 1, b = "x", c = NA)),
                   '{"a":1,"b":"x","c":null}')
  expect_false(grepl("\n", .sr_json(list(a = 1:3, b = list(c = 0.1 + 0.2)))))
  expect_true(is.character(.sr_json(1:2)))
})

test_that(".sr_write_lines_append appends LF-terminated lines", {
  p <- withr::local_tempfile(fileext = ".log")
  .sr_write_lines_append(p, "first")
  .sr_write_lines_append(p, "second")
  b <- readBin(p, "raw", file.size(p))
  expect_identical(b[length(b)], as.raw(0x0A))
  expect_identical(readLines(p), c("first", "second"))
})

# ---- final review M1: whole-file rewrites go through <path>.tmp + rename --------------------------

test_that(".sr_write_atomic writes or replaces a file through <path>.tmp and leaves no .tmp", {
  d <- withr::local_tempdir()
  p <- file.path(d, "f.txt")
  spy <- local_rename_spy()
  .sr_write_atomic(p, charToRaw("one"))
  expect_identical(readBin(p, "raw", 100), charToRaw("one"))
  .sr_write_atomic(p, charToRaw("two!"))
  expect_identical(readBin(p, "raw", 100), charToRaw("two!"))
  .sr_write_atomic(p, raw(0))
  expect_identical(file.size(p), 0)
  expect_identical(spy$to, rep(normalizePath(p, winslash = "/", mustWork = FALSE), 3L))
  expect_identical(list.files(d, all.files = TRUE, no.. = TRUE), "f.txt")
})

test_that(".sr_write_atomic stops, removes its .tmp and leaves the target alone when it cannot replace", {
  d <- withr::local_tempdir()
  target <- file.path(d, "busy")
  dir.create(target)                         # a directory cannot be replaced by a file
  writeLines("keep", file.path(target, "inside.txt"))
  expect_error(.sr_write_atomic(target, charToRaw("x")), "cannot replace")
  expect_false(file.exists(paste0(target, ".tmp")))
  expect_true(dir.exists(target))
  expect_identical(readLines(file.path(target, "inside.txt")), "keep")
})

test_that(".sr_truncate_tail cuts an unterminated tail atomically and returns it", {
  d <- withr::local_tempdir()
  p <- file.path(d, "log")
  writeBin(charToRaw("a\nb\npartial"), p)
  spy <- local_rename_spy()
  tail <- .sr_truncate_tail(p)
  expect_identical(tail, charToRaw("partial"))
  expect_identical(readBin(p, "raw", 100), charToRaw("a\nb\n"))
  expect_identical(.sr_truncate_tail(p), raw(0))            # nothing left to cut: no rewrite
  expect_length(spy$to, 1L)
  expect_identical(list.files(d, all.files = TRUE, no.. = TRUE), "log")
  writeBin(charToRaw("no newline at all"), p)
  expect_identical(.sr_truncate_tail(p), charToRaw("no newline at all"))
  expect_identical(file.size(p), 0)
  expect_identical(.sr_truncate_tail(file.path(d, "missing")), raw(0))
})
