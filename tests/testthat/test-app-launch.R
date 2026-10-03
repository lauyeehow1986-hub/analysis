# Launch the real app in a child R process and fetch its page. Needs a free port 7790. The localhost-only
# curl below is a test-only exception to the air-gap lint (tests are not linted).

test_that("tools/run_app.R serves the StatRouter shell", {
  skip_on_cran()
  skip_if_not_installed("processx")
  skip_if_not_installed("curl")
  skip_if_not_installed("withr")
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  out <- withr::local_tempfile(fileext = ".out")
  err <- withr::local_tempfile(fileext = ".err")
  child <- processx::process$new(rscript, c("tools/run_app.R"), wd = root, stdout = out, stderr = err,
                                 env = c("current", SR_PORT = "7790"))
  withr::defer(if (child$is_alive()) child$kill())

  fetch <- function() {
    # A corporate proxy would swallow loopback requests, so bypass proxies explicitly.
    h <- curl::new_handle(proxy = "", noproxy = "*", timeout = 5)
    tryCatch(curl::curl_fetch_memory("http://127.0.0.1:7790", handle = h), error = function(e) NULL)
  }
  res <- NULL
  deadline <- Sys.time() + 30
  while (Sys.time() < deadline && child$is_alive()) {
    res <- fetch()
    if (!is.null(res)) break
    Sys.sleep(0.5)
  }
  info <- if (is.null(res)) paste(c(readLines(err, warn = FALSE), readLines(out, warn = FALSE)), collapse = "\n") else ""
  expect_false(is.null(res), info = info)
  # The page must come from our own child, not a stale server already sitting on the port.
  expect_true(child$is_alive(), info = paste(readLines(err, warn = FALSE), collapse = "\n"))
  expect_equal(res$status_code, 200L)
  body <- rawToChar(res$content)
  expect_match(body, "StatRouter", fixed = TRUE)
  expect_match(body, "Reviewer", fixed = TRUE)
  expect_match(body, "no network", fixed = TRUE)
  expect_match(body, "Coming in P1b-P1d", fixed = TRUE)
})
