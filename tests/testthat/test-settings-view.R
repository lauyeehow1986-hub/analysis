# Tests for the pure side of the Settings panel (ui/settings_view.R).

.sv_root <- function() normalizePath(testthat::test_path("../.."), winslash = "/")

test_that("catalogue defaults match the core's sr_opt calls", {
  cat <- .sr_set_catalogue()
  expect_identical(cat$name, c("mode", "profile", "user", "digits", "router_mask", "jev_key", "jev_url",
                               "jev_model", "jev_timeout", "freeze_full_max_bytes", "srproj_max_total",
                               "cran_snapshot"))
  expect_identical(.sr_set_names(), cat$name)
  src <- unlist(lapply(list.files(file.path(.sv_root(), "app", "R"), "[.]R$", recursive = TRUE, full.names = TRUE),
                       function(f) readLines(f, warn = FALSE)))
  src <- src[!grepl("^\\s*#", src)]
  for (nm in setdiff(cat$name, c("user", "profile", "cran_snapshot"))) {
    pat <- paste0("sr_opt\\(\"", nm, "\", ([^)]+)\\)")
    hits <- unlist(regmatches(src, gregexpr(pat, src)))
    hits <- unique(sub(pat, "\\1", hits))
    expect_gt(length(hits), 0L, label = nm)
    want <- cat$default[[match(nm, cat$name)]]
    for (h in hits) expect_identical(eval(parse(text = h)), want, info = nm)
  }
})

test_that("coerce: each validation message", {
  err <- function(nm, v) tryCatch({ .sr_set_coerce(nm, v); NA_character_ }, error = function(e) conditionMessage(e))
  expect_identical(err("user", "  "), "the actor must not be empty (use Reset)")
  expect_identical(err("digits", "1.5"), "digits must be a whole number, 0 or more")
  expect_identical(err("digits", -1), "digits must be a whole number, 0 or more")
  expect_identical(err("router_mask", "maybe"), "router_mask must be on or off")
  for (nm in c("jev_key", "jev_url", "jev_model")) expect_identical(err(nm, ""), paste0(nm, " must not be empty"))
  expect_identical(err("jev_timeout", 0), "jev_timeout must be a positive number of seconds")
  for (nm in c("freeze_full_max_bytes", "srproj_max_total")) {
    expect_identical(err(nm, -1), paste0(nm, " must be a finite number of bytes, 0 or more"))
    expect_identical(err(nm, Inf), paste0(nm, " must be a finite number of bytes, 0 or more"))
  }
  expect_identical(err("cran_snapshot", "2025-02-30"), "cran_snapshot must be a date YYYY-MM-DD")
  expect_identical(err("cran_snapshot", "2025-1-5"), "cran_snapshot must be a date YYYY-MM-DD")
  expect_identical(.sr_set_coerce("cran_snapshot", "2025-01-05"), "2025-01-05")
  expect_identical(.sr_set_coerce("digits", "3"), 3L)
  expect_identical(.sr_set_coerce("router_mask", "on"), TRUE)
  expect_identical(.sr_set_coerce("jev_timeout", "7.5"), 7.5)
  # the message never repeats the value (it may be the key)
  expect_false(grepl("sekrit", err("jev_timeout", "sekrit"), fixed = TRUE))
})

test_that("coerce: jev_url must be an http(s) URL with a host, checked without any network call", {
  err <- function(v) tryCatch({ .sr_set_coerce("jev_url", v); NA_character_ }, error = function(e) conditionMessage(e))
  msg <- paste0("jev_url must be an http:// or https:// URL with a host name, for example ",
                "https://api.example.org/v1 (no spaces, no user:password@)")
  good <- c("https://api.typesafe.ai/v1/systemone", "http://localhost:8080", "HTTPS://Example.org",
            "https://10.0.0.5/v1?x=1", "http://[::1]:9000/", "https://example.org.", "https://a-b.example.org#f")
  for (u in good) expect_identical(.sr_set_coerce("jev_url", u), u, info = u)
  expect_identical(.sr_set_coerce("jev_url", "  https://example.org/v1  "), "https://example.org/v1")
  bad <- c("example.org", "ftp://example.org", "https://", "https:///v1", "https://exa mple.org",
           "https://user:pw@example.org", "https://example.org:0", "https://example.org:70000",
           "https://-bad-.example.org", "https://a..b", "http//example.org", "file:///C:/x", "javascript:x")
  for (u in bad) expect_identical(err(u), msg, info = u)
  expect_identical(err(""), "jev_url must not be empty")
  # an invalid URL leaves the previous one in place (in air-gap mode too: nothing is contacted)
  withr::local_options(sr.mode = "airgap", sr.jev_url = NULL)
  expect_identical(.sr_set_apply("jev_url", "https://example.org/v1"), "https://example.org/v1")
  expect_identical(getOption("sr.jev_url"), "https://example.org/v1")
  expect_error(.sr_set_apply("jev_url", "not a url"), "jev_url must be an http")
  expect_identical(getOption("sr.jev_url"), "https://example.org/v1")
})

test_that("apply: an invalid mode restores the previous option", {
  withr::local_options(sr.mode = "airgap")
  expect_error(.sr_set_apply("mode", "bogus"), "mode must be airgap or connected")
  expect_identical(getOption("sr.mode"), "airgap")
  withr::local_options(sr.mode = NULL)
  expect_error(.sr_set_apply("mode", "bogus"))
  expect_null(getOption("sr.mode"))
  expect_identical(.sr_set_apply("digits", "4"), 4L)
  expect_identical(getOption("sr.digits"), 4L)
  withr::defer(options(sr.digits = NULL))
  withr::local_options(sr.profile = NULL)
  .sr_set_apply("profile", "laptop")
  expect_identical(sr_profile(), "laptop")
  .sr_set_apply("profile", "auto")
  expect_null(getOption("sr.profile"))
})

test_that("table: sources for edited, option, env and default; profile auto-detected; key shown as set", {
  withr::local_options(sr.digits = NULL, sr.router_mask = NULL, sr.jev_timeout = NULL, sr.profile = NULL,
                       sr.jev_key = NULL, sr.user = NULL, sr.jev_model = NULL, sr.jev_url = NULL)
  withr::local_envvar(SR_JEV_MODEL = "m-env", SR_DIGITS = "", SR_PROFILE = "", SR_JEV_KEY = "", SR_JEV_URL = "")
  options(sr.jev_timeout = 9)
  tb <- .sr_set_table(edited = "digits")
  row <- function(t, nm) t[t$setting == nm, ]
  expect_identical(names(tb), c("setting", "value", "source"))
  expect_identical(nrow(tb), 12L)
  expect_identical(row(tb, "digits")$source, "edited here")
  expect_identical(row(tb, "jev_timeout")$source, "option")
  expect_identical(row(tb, "jev_timeout")$value, "9")
  expect_identical(row(tb, "jev_model")$source, "env")
  expect_identical(row(tb, "jev_model")$value, "m-env")
  expect_identical(row(tb, "jev_url")$source, "default")
  expect_identical(row(tb, "profile")$source, "auto-detected")
  expect_identical(row(tb, "profile")$value, sr_profile())
  expect_identical(row(tb, "jev_key")$value, "not set")
  options(sr.jev_key = "k")
  expect_identical(row(.sr_set_table(), "jev_key")$value, "set")
  options(sr.profile = "laptop")
  expect_identical(row(.sr_set_table(), "profile")$source, "option")
})

test_that("table never shows the key value", {
  withr::local_options(sr.jev_key = "SENTINEL-KEY-123")
  tb <- .sr_set_table(edited = "jev_key")
  expect_false(any(grepl("SENTINEL", unlist(tb), fixed = TRUE)))
  expect_identical(tb$value[tb$setting == "jev_key"], "set")
  withr::local_options(sr.jev_key = NULL)
  withr::local_envvar(SR_JEV_KEY = "SENTINEL-ENV-KEY")
  expect_false(any(grepl("SENTINEL", unlist(.sr_set_table()), fixed = TRUE)))
})

test_that("reset clears the option; resetting mode also clears the key; notes", {
  withr::local_options(sr.digits = 5L, sr.mode = "connected", sr.jev_key = "k")
  .sr_set_reset("digits")
  expect_null(getOption("sr.digits"))
  .sr_set_reset("mode")
  expect_null(getOption("sr.mode"))
  expect_null(getOption("sr.jev_key"))
  expect_match(.sr_set_note("profile"), "fingerprint")
  expect_match(.sr_set_note("cran_snapshot"), "re-baseline")
  expect_null(.sr_set_note("digits"))
})
