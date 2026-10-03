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
