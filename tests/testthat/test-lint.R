# Tests for core/lint.R and tools/lint.R: the p-gate and air-gap static lints.

.fx <- function(...) testthat::test_path("fixtures", "lint", ...)

.tmp_r <- function(lines) {
  p <- withr::local_tempfile(fileext = ".R", .local_envir = parent.frame())
  writeLines(lines, p)
  p
}

test_that("p-gate flags a compile() list element that branches on a p-value", {
  r <- sr_lint_pgate(.fx("bad_pgate.R"))
  expect_s3_class(r, "data.frame")
  expect_identical(names(r), c("file", "line", "msg"))
  expect_identical(nrow(r), 1L)
  expect_identical(r$line, 2L)
  expect_identical(r$msg, "compile() branches on a diagnostic test (M1.1)")
})

test_that("p-gate flags a compile <- function with a p_value in a switch condition", {
  r <- sr_lint_pgate(.fx("bad_pgate2.R"))
  expect_gte(nrow(r), 1L)
  expect_true(all(r$line == 2L))
})

test_that("p-gate passes a clean compile() and ignores diagnostics outside compile", {
  r <- sr_lint_pgate(.fx("ok_route.R"))
  expect_identical(nrow(r), 0L)
  expect_identical(names(r), c("file", "line", "msg"))
  expect_type(r$line, "integer")
})

test_that("p-gate handles compile = function assignment and nested functions", {
  p <- .tmp_r(c(
    "compile = function(spec) {",
    "  helper <- function(d) {",
    "    ifelse(d$p < 0.05, 1, 2)",
    "  }",
    "  helper(spec)",
    "}"
  ))
  r <- sr_lint_pgate(p)
  expect_identical(nrow(r), 1L)
  expect_identical(r$line, 3L)
})

test_that("p-gate flags diagnostic calls, dplyr::if_else, and p.value strings", {
  p <- .tmp_r(c(
    "compile <- function(spec) {",
    "  if (bartlett.test(x)$statistic > 1) 1",
    "  dplyr::if_else(res[['p.value']] > .05, 1, 2)",
    "  if (check_normality(x)) 1",
    "  if (identical(nm, \"pvalue\")) 1",
    "}"
  ))
  r <- sr_lint_pgate(p)
  expect_identical(r$line, c(2L, 3L, 4L, 5L))
})

test_that("p-gate does not flag plain rule/design conditions", {
  p <- .tmp_r(c(
    "compile <- function(spec) {",
    "  if (spec$sap$rule == \"x\") 1 else 2",
    "  switch(spec$design$type, a = 1, b = 2)",
    "  ifelse(is.na(spec$n), 0, spec$n)",
    "}"
  ))
  expect_identical(nrow(sr_lint_pgate(p)), 0L)
})

test_that("p-gate: a flagged condition is reported once per construct, not for its body", {
  p <- .tmp_r(c(
    "compile <- function(spec) {",
    "  if (spec$a) {",
    "    shapiro.test(x)",
    "  }",
    "}"
  ))
  expect_identical(nrow(sr_lint_pgate(p)), 0L)
})

test_that("air-gap flags download.file, curl:: and py_available(initialize = TRUE) with lines", {
  r <- sr_lint_airgap(.fx("bad_net.R"))
  expect_identical(nrow(r), 3L)
  expect_identical(r$line, c(3L, 4L, 5L))
  expect_match(r$msg[1], "download.file")
  expect_match(r$msg[2], "curl::")
  expect_match(r$msg[3], "py_available")
  expect_true(all(grepl("air-gap", r$msg, fixed = TRUE)))
})

test_that("air-gap allowlist gives 0 findings for the same content", {
  r <- sr_lint_airgap(.fx("bad_net.R"), allow = "fixtures/lint/bad_net.R")
  expect_identical(nrow(r), 0L)
  expect_identical(names(r), c("file", "line", "msg"))
  # works with backslashes and a character vector
  bs <- intToUtf8(92)
  f <- gsub("/", bs, .fx("bad_net.R"), fixed = TRUE)
  r2 <- sr_lint_airgap(f, allow = c("nope/x.R", "lint/bad_net.R"))
  expect_identical(nrow(r2), 0L)
  # a non-matching allow does not hide findings
  expect_identical(nrow(sr_lint_airgap(.fx("bad_net.R"), allow = "other/bad_net2.R")), 3L)
})

test_that("air-gap ignores string literals, a variable named url, and bare py_available()", {
  p <- .tmp_r(c(
    "x <- \"download.file\"",
    "url <- \"a\"",
    "f <- function(url) paste(url, 1)",
    "g <- list(url = 1)",
    "py_available()",
    "reticulate::py_available(initialize = FALSE)",
    "nm <- c(\"install.packages\", \"curl::x\", \"options(repos = 1)\")"
  ))
  expect_identical(nrow(sr_lint_airgap(p, allow = character())), 0L)
})

test_that("air-gap flags url(), socketConnection, install.packages, py_install, options(repos=)", {
  p <- .tmp_r(c(
    "a <- url(\"x\")",
    "b <- socketConnection(port = 1)",
    "install.packages(\"x\")",
    "reticulate::py_install(\"x\")",
    "options(repos = c(CRAN = \"x\"))",
    "options(digits = 3)",
    "httr::GET(\"x\")",
    "httr2::request(\"x\")",
    "py_available(T)",
    "utils::download.file(\"a\", \"b\")"
  ))
  r <- sr_lint_airgap(p, allow = character())
  expect_identical(r$line, c(1L, 2L, 3L, 4L, 5L, 7L, 8L, 9L, 10L))
})

test_that("air-gap accepts a vector of files and returns the file column", {
  r <- sr_lint_airgap(c(.fx("bad_net.R"), .fx("ok_route.R")))
  expect_identical(nrow(r), 3L)
  expect_true(all(r$file == .fx("bad_net.R")))
})

test_that("tools/lint.R prints LINT OK with status 0 on the real app/R", {
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  withr::local_dir(root)
  rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  out <- suppressWarnings(system2(rs, c("tools/lint.R"), stdout = TRUE, stderr = TRUE))
  st <- attr(out, "status")
  expect_true(is.null(st) || identical(st, 0L))
  expect_true("LINT OK" %in% out)
})

test_that("tools/lint.R exits 1 and prints file:line: msg on a violation", {
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  d <- withr::local_tempdir()
  expect_true(file.copy(.fx("bad_net.R"), file.path(d, "bad_net.R")))
  withr::local_dir(root)
  rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  out <- suppressWarnings(system2(rs, c("tools/lint.R", shQuote(d)), stdout = TRUE, stderr = TRUE))
  expect_identical(attr(out, "status"), 1L)
  expect_false("LINT OK" %in% out)
  expect_true(any(grepl("bad_net.R:3: network call: download.file", out, fixed = TRUE)))
})
