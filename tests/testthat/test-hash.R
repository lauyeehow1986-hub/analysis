# Tests for core/libsnap.R and core/hash.R: canonical JSON, library snapshot, content hash.

.cjk <- function() intToUtf8(c(0x4E2D, 0x6587))
.umol <- function() intToUtf8(c(0xB5, 0x6D, 0x6F, 0x6C))

.nonascii_spec <- function() {
  spec <- list(z = 1L, a = list(), m = c(1.5, 2.5))
  spec[[.cjk()]] <- .umol()
  spec[[.umol()]] <- .cjk()
  spec$a <- list(b = .cjk(), A = .umol())
  spec
}

test_that("canonical JSON sorts keys recursively and has no whitespace", {
  expect_identical(
    as.character(sr_canonical_json(list(b = 1, a = list(d = 2, c = "x")))),
    '{"a":{"c":"x","d":2},"b":1}'
  )
  a <- list(b = 1, a = list(d = 2, c = "x"), e = list(list(q = 1, p = 2), list(z = 3)))
  b <- list(e = list(list(p = 2, q = 1), list(z = 3)), a = list(c = "x", d = 2), b = 1)
  expect_identical(sr_canonical_json(a), sr_canonical_json(b))
  # unnamed lists keep their order
  expect_identical(sr_canonical_json(list(3, 1, 2)), "[3,1,2]")
})

test_that("key order is bytewise (radix), not locale collation", {
  # locale collation would give _ a B; bytewise gives B _ a
  x <- list(a = 3, B = 1, "_" = 2)
  expect_identical(sr_canonical_json(x), '{"B":1,"_":2,"a":3}')
})

test_that("numbers are written in shortest round-trip form", {
  expect_identical(sr_canonical_json(0.1 + 0.2), "0.30000000000000004")
  expect_identical(sr_canonical_json(0.1), "0.1")
  expect_identical(sr_canonical_json(2), "2")
  expect_identical(sr_canonical_json(-3), "-3")
  expect_identical(sr_canonical_json(1e5), "100000")
  expect_identical(sr_canonical_json(1e20), "1e+20")
  expect_identical(sr_canonical_json(1.5e-10), "1.5e-10")
  expect_identical(sr_canonical_json(-0), "0")
  expect_identical(sr_canonical_json(5L), "5")
  expect_identical(sr_canonical_json(100000L), "100000")
  set.seed(42)
  xs <- c(runif(200), rnorm(200) * 1e6, rexp(100) * 1e-8, 1 / 3, pi, exp(1), 2^60, 5e-324, .Machine$double.xmax)
  for (x in xs) {
    txt <- sr_canonical_json(x)
    expect_identical(as.numeric(txt), x)
    expect_match(txt, "^-?[0-9]+([.][0-9]+)?(e[+-][0-9]{2,3})?$")
  }
})

test_that("scalars unbox, vectors become arrays, empties and NA are handled", {
  expect_identical(sr_canonical_json("x"), '"x"')
  expect_identical(sr_canonical_json(TRUE), "true")
  expect_identical(sr_canonical_json(c(TRUE, FALSE)), "[true,false]")
  expect_identical(sr_canonical_json(c(1L, 2L, 3L)), "[1,2,3]")
  expect_identical(sr_canonical_json(c(1.5, 2)), "[1.5,2]")
  expect_identical(sr_canonical_json(character(0)), "[]")
  expect_identical(sr_canonical_json(list()), "[]")
  expect_identical(sr_canonical_json(setNames(list(), character(0))), "{}")
  expect_identical(sr_canonical_json(NULL), "null")
  expect_identical(sr_canonical_json(NA), "null")
  expect_identical(sr_canonical_json(NA_character_), "null")
  expect_identical(sr_canonical_json(NA_integer_), "null")
  expect_identical(sr_canonical_json(NA_real_), "null")
  expect_identical(sr_canonical_json(c("a", NA)), '["a",null]')
  expect_identical(sr_canonical_json(list(a = NULL, b = NA)), '{"a":null,"b":null}')
  expect_identical(sr_canonical_json(factor(c("lo", "hi", NA))), '["lo","hi",null]')
  expect_identical(sr_canonical_json(factor("lo")), '"lo"')
  # other attributes (not names, class or dim) are ignored
  expect_identical(sr_canonical_json(structure(1, units = "mg")), "1")
})

test_that("strings are JSON-escaped and non-ASCII stays raw UTF-8", {
  bs <- intToUtf8(92)
  q <- intToUtf8(34)
  s <- intToUtf8(c(34, 92, 10, 9, 1, 47, 65))
  expect_identical(
    sr_canonical_json(s),
    paste0(q, bs, q, bs, bs, bs, "n", bs, "t", bs, "u0001", "/A", q)
  )
  # byte-level pin: U+4E2D is e4 b8 ad on the wire, never a unicode escape
  out <- sr_canonical_json(list(x = intToUtf8(0x4E2D)))
  expect_identical(Encoding(out), "UTF-8")
  expect_identical(
    charToRaw(out),
    as.raw(c(0x7b, 0x22, 0x78, 0x22, 0x3a, 0x22, 0xe4, 0xb8, 0xad, 0x22, 0x7d))
  )
  # a Latin-1 marked input is re-encoded to UTF-8
  mu_latin1 <- iconv(intToUtf8(0xB5), from = "UTF-8", to = "latin1")
  expect_identical(
    charToRaw(sr_canonical_json(mu_latin1)),
    as.raw(c(0x22, 0xc2, 0xb5, 0x22))
  )
})

test_that("unsupported or ambiguous input is an error", {
  expect_error(sr_canonical_json(Inf), "non-finite number")
  expect_error(sr_canonical_json(-Inf), "non-finite number")
  expect_error(sr_canonical_json(NaN), "non-finite number")
  expect_error(sr_canonical_json(list(a = c(1, NaN))), "non-finite number")
  expect_error(sr_canonical_json(function(x) x), "function")
  expect_error(sr_canonical_json(new.env()), "environment")
  expect_error(sr_canonical_json(Sys.Date()), "Date")
  expect_error(sr_canonical_json(Sys.time()), "POSIXct")
  expect_error(sr_canonical_json(list(a = 1, 2)), "named")
  expect_error(sr_canonical_json(list(a = 1, a = 2)), "duplicate")
})

test_that("non-ASCII names and values hash identically across collation and processes", {
  spec <- .nonascii_spec()
  h1 <- .sr_sha256(sr_canonical_json(spec))
  h_default <- sr_content_hash("d", spec, 1L, libsnap_hash = "L", hw_fp = "H")

  withr::local_collate("C")
  h2 <- .sr_sha256(sr_canonical_json(spec))
  h_c <- sr_content_hash("d", spec, 1L, libsnap_hash = "L", hw_fp = "H")
  expect_identical(h1, h2)
  expect_identical(h_default, h_c)

  # shuffled construction order does not matter either
  spec2 <- spec[rev(names(spec))]
  expect_identical(sr_content_hash("d", spec2, 1L, libsnap_hash = "L", hw_fp = "H"), h_default)

  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  child <- callr::r(
    function(root) {
      Sys.setlocale("LC_COLLATE", "C")
      source(file.path(root, "app", "global.R"))
      cjk <- intToUtf8(c(0x4E2D, 0x6587))
      umol <- intToUtf8(c(0xB5, 0x6D, 0x6F, 0x6C))
      spec <- list(z = 1L, a = list(), m = c(1.5, 2.5))
      spec[[cjk]] <- umol
      spec[[umol]] <- cjk
      spec$a <- list(b = cjk, A = umol)
      list(
        hash = sr_content_hash("d", spec, 1L, libsnap_hash = "L", hw_fp = "H"),
        json = charToRaw(sr_canonical_json(spec))
      )
    },
    args = list(root = root)
  )
  expect_identical(child$hash, h_default)
  expect_identical(child$json, charToRaw(sr_canonical_json(spec)))
})

test_that("library snapshot records R, snapshot date and core package versions", {
  withr::local_options(sr.cran_snapshot = NULL)
  withr::local_envvar(SR_CRAN_SNAPSHOT = NA)
  snap <- sr_libsnap()
  expect_identical(names(snap), c("r", "snapshot_date", "pkgs"))
  expect_identical(snap$r, R.version.string)
  expect_true(is.na(snap$snapshot_date))
  expect_identical(.sr_core_pkgs(), c("jsonlite", "openssl", "callr", "processx", "ps", "shiny", "bslib",
                                       "survival", "mice", "sandwich", "boot", "Hmisc", "logistf", "ggplot2", "ragg"))
  expect_identical(names(snap$pkgs), .sr_core_pkgs())
  expect_type(snap$pkgs, "character")
  expect_identical(snap$pkgs[["jsonlite"]], as.character(utils::packageVersion("jsonlite")))

  # a package that is not installed is recorded as NA, not an error
  s2 <- sr_libsnap(pkgs = c("jsonlite", "notapkg.sr.test"))
  expect_identical(names(s2$pkgs), c("jsonlite", "notapkg.sr.test"))
  expect_true(is.na(s2$pkgs[["notapkg.sr.test"]]))
  expect_match(sr_canonical_json(list(p = as.list(s2$pkgs))), '"notapkg.sr.test":null', fixed = TRUE)

  withr::local_options(sr.cran_snapshot = "2026-09-01")
  expect_identical(sr_libsnap(pkgs = character(0))$snapshot_date, "2026-09-01")
})

test_that("library snapshot hash is stable and sensitive to every part", {
  snap <- sr_libsnap()
  expect_match(sr_libsnap_hash(snap), "^[0-9a-f]{64}$")
  expect_identical(sr_libsnap_hash(snap), sr_libsnap_hash(snap))
  expect_identical(sr_libsnap_hash(), sr_libsnap_hash(snap))
  s2 <- snap
  s2$pkgs[["jsonlite"]] <- "0.0.1"
  expect_false(identical(sr_libsnap_hash(s2), sr_libsnap_hash(snap)))
  s3 <- snap
  s3$snapshot_date <- "2026-01-01"
  expect_false(identical(sr_libsnap_hash(s3), sr_libsnap_hash(snap)))
  s4 <- snap
  s4$r <- "R version 0.0"
  expect_false(identical(sr_libsnap_hash(s4), sr_libsnap_hash(snap)))
})

test_that("content hash changes with any of data, spec, seed, lib and hw", {
  spec <- list(model = "lm", terms = c("a", "b"))
  base <- sr_content_hash("dh", spec, 1L, libsnap_hash = "lib", hw_fp = "hw")
  expect_match(base, "^[0-9a-f]{64}$")
  expect_identical(base, sr_content_hash("dh", spec, 1L, libsnap_hash = "lib", hw_fp = "hw"))
  expect_identical(
    base,
    .sr_sha256(sr_canonical_json(list(data = "dh", spec = spec, seed = 1L, lib = "lib", hw = "hw")))
  )
  expect_false(identical(base, sr_content_hash("dh2", spec, 1L, libsnap_hash = "lib", hw_fp = "hw")))
  expect_false(identical(base, sr_content_hash("dh", list(model = "glm", terms = c("a", "b")), 1L, libsnap_hash = "lib", hw_fp = "hw")))
  expect_false(identical(base, sr_content_hash("dh", spec, 2L, libsnap_hash = "lib", hw_fp = "hw")))
  expect_false(identical(base, sr_content_hash("dh", spec, 1L, libsnap_hash = "lib2", hw_fp = "hw")))
  expect_false(identical(base, sr_content_hash("dh", spec, 1L, libsnap_hash = "lib", hw_fp = "hw2")))
  # defaults resolve to the live library snapshot and hardware fingerprint
  expect_identical(
    sr_content_hash("dh", spec, 1L),
    sr_content_hash("dh", spec, 1L, libsnap_hash = sr_libsnap_hash(), hw_fp = sr_hw_fingerprint())
  )
})

test_that("named atomic vectors serialise as objects, never as bare arrays", {
  expect_identical(sr_canonical_json(c(a = 1, b = 2)), '{"a":1,"b":2}')
  expect_identical(sr_canonical_json(c(b = 2, a = 1)), '{"a":1,"b":2}')
  expect_false(identical(sr_canonical_json(c(a = 1, b = 2)), sr_canonical_json(c(x = 1, y = 2))))
  expect_identical(sr_canonical_json(c(a = 1)), '{"a":1}')
  expect_identical(sr_canonical_json(c(a = "x")), '{"a":"x"}')
  expect_identical(sr_canonical_json(c(a = TRUE, b = NA)), '{"a":true,"b":null}')
  expect_identical(sr_canonical_json(c(a = 1L, B = 2L)), '{"B":2,"a":1}')
  expect_identical(sr_canonical_json(list(p = c(k = "v"))), '{"p":{"k":"v"}}')
  expect_identical(sr_canonical_json(setNames(character(0), character(0))), "{}")
  # unnamed vectors are unchanged
  expect_identical(sr_canonical_json(c(1, 2)), "[1,2]")
  # a named factor: labels as values, object keyed by names
  f <- factor(c("lo", "hi"))
  names(f) <- c("n1", "n2")
  expect_identical(sr_canonical_json(f), '{"n1":"lo","n2":"hi"}')
  # the same object checks as lists
  expect_error(sr_canonical_json(c(a = 1, 2)), "named")
  expect_error(sr_canonical_json(c(a = 1, a = 2)), "duplicate")
  expect_error(sr_canonical_json(structure(1:2, names = c("a", NA))), "named")
})

test_that("structure that would be silently dropped is an error", {
  expect_error(sr_canonical_json(matrix(1:4, 2)), "unsupported class 'matrix'")
  expect_error(sr_canonical_json(matrix(1:4, 1)), "unsupported class 'matrix'")
  expect_error(sr_canonical_json(array(1:8, c(2, 2, 2))), "unsupported class 'array'")
  expect_error(sr_canonical_json(table(c("a", "b", "a"))), "unsupported class 'table'")
  expect_error(sr_canonical_json(structure(1, class = "foo")), "unsupported class 'foo'")
  expect_error(sr_canonical_json(structure(1:2, class = "integer64")), "unsupported class 'integer64'")
  expect_error(sr_canonical_json(as.difftime(1, units = "secs")), "difftime")
  expect_error(sr_canonical_json(list(m = matrix(1:4, 2))), "unsupported class 'matrix'")
  expect_error(sr_canonical_json(structure(1, class = c("foo", "bar"))), "unsupported class 'foo'")
  # plain vectors and factors are unaffected
  expect_identical(sr_canonical_json(1:4), "[1,2,3,4]")
  expect_identical(sr_canonical_json(factor(c("a", "b"))), '["a","b"]')
  # a data.frame (a classed list) keeps its current behaviour: an object of columns
  expect_identical(sr_canonical_json(data.frame(b = 1:2, a = c("x", "y"))), '{"a":["x","y"],"b":[1,2]}')
})
