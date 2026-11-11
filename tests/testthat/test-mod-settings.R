# Tests for the Settings panel module (ui/mod_settings.R), driven through shiny::testServer.

.ms_args <- function(busy = FALSE) list(busy = function() busy)

test_that("set and reset digits", {
  withr::local_options(sr.digits = NULL)
  withr::defer(options(sr.digits = NULL))
  seen <- .ui_spy_notify()
  shiny::testServer(mod_settings_server, args = .ms_args(), {
    v0 <- version()
    session$setInputs(val_digits = 4, set_digits = 1)
    expect_identical(version(), v0 + 1L)
    expect_identical(getOption("sr.digits"), 4L)
    session$setInputs(reset_digits = 1)
    expect_identical(version(), v0 + 2L)
    expect_null(getOption("sr.digits"))
    session$setInputs(val_digits = -2, set_digits = 2)
    expect_identical(version(), v0 + 2L)
    expect_true(any(grepl("digits must be a whole number", seen$msg, fixed = TRUE)))
  })
})

test_that("connected needs exactly CONNECT", {
  withr::local_options(sr.mode = "airgap")
  withr::defer(options(sr.mode = NULL))
  seen <- .ui_spy_notify()
  shiny::testServer(mod_settings_server, args = .ms_args(), {
    session$setInputs(val_mode = "connected", set_mode = 1)
    expect_identical(sr_mode(), "airgap")
    session$setInputs(connect_word = "connect", connect_ok = 1)
    expect_identical(sr_mode(), "airgap")
    session$setInputs(connect_word = "CONNECT", connect_ok = 2)
    expect_identical(sr_mode(), "connected")
    expect_identical(version(), 1L)
  })
})

test_that("back to airgap clears the key", {
  withr::local_options(sr.mode = "connected", sr.jev_key = "k")
  withr::defer(options(sr.mode = NULL, sr.jev_key = NULL))
  .ui_spy_notify()
  shiny::testServer(mod_settings_server, args = .ms_args(), {
    session$setInputs(val_mode = "airgap", set_mode = 1)
    expect_identical(sr_mode(), "airgap")
    expect_null(getOption("sr.jev_key"))
  })
  options(sr.mode = "connected", sr.jev_key = "k")
  shiny::testServer(mod_settings_server, args = .ms_args(), {
    session$setInputs(reset_mode = 1)
    expect_null(getOption("sr.jev_key"))
  })
})

test_that("settings are locked during a job", {
  withr::local_options(sr.digits = NULL, sr.user = NULL)
  seen <- .ui_spy_notify()
  shiny::testServer(mod_settings_server, args = .ms_args(busy = TRUE), {
    session$setInputs(val_digits = 5, set_digits = 1)
    session$setInputs(val_user = "role-a", set_user = 1)
    session$setInputs(reset_digits = 1)
    expect_identical(version(), 0L)
    expect_null(getOption("sr.digits"))
    expect_null(getOption("sr.user"))
    expect_true(all(startsWith(seen$msg, "Settings: ")))
    expect_true(all(endsWith(seen$msg, .sr_ui_busy_text)))
    expect_length(seen$msg, 3L)
    expect_match(paste(output$editors$html, collapse = " "), "<fieldset disabled", fixed = TRUE)
  })
})

test_that("the key editor is emptied after a set and no output holds it", {
  withr::local_options(sr.jev_key = NULL)
  withr::defer(options(sr.jev_key = NULL))
  .ui_spy_notify()
  sentinel <- "SENTINEL-KEY-xyz"
  shiny::testServer(mod_settings_server, args = .ms_args(), {
    session$setInputs(val_jev_key = sentinel, set_jev_key = 1)
    expect_identical(getOption("sr.jev_key"), sentinel)
    expect_identical(version(), 1L)
    session$flushReact()
    all_out <- c(table = paste(as.character(output$table), collapse = " "),
                 editors = paste(as.character(output$editors), collapse = " "))
    expect_false(any(grepl(sentinel, all_out, fixed = TRUE)))
    expect_match(all_out[["table"]], "set", fixed = TRUE)
    # a set with no key is refused and does not clear an existing key
    session$setInputs(val_jev_key = "", set_jev_key = 2)
    expect_identical(getOption("sr.jev_key"), sentinel)
    session$setInputs(reset_jev_key = 1)
    expect_null(getOption("sr.jev_key"))
  })
})

test_that("an actor set here reaches the worker snapshot, and the key never does", {
  withr::local_options(sr.user = NULL, sr.jev_key = NULL)
  withr::defer(options(sr.user = NULL, sr.jev_key = NULL))
  .ui_spy_notify()
  shiny::testServer(mod_settings_server, args = .ms_args(), {
    session$setInputs(val_user = "role-a", set_user = 1)
    session$setInputs(val_jev_key = "SENTINEL-KEY-xyz", set_jev_key = 1)
    o <- .sr_job_opts()
    expect_identical(o[["sr.user"]], "role-a")
    expect_false("sr.jev_key" %in% names(o))
    expect_false(grepl("SENTINEL", paste(unlist(o), collapse = " "), fixed = TRUE))
  })
})
