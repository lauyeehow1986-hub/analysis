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
  expect_true(any(grepl("^LINT OK", out)))
})

test_that("tools/lint.R exits 1 and prints file:line: msg on a violation", {
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  d <- withr::local_tempdir()
  expect_true(file.copy(.fx("bad_net.R"), file.path(d, "bad_net.R")))
  withr::local_dir(root)
  rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  out <- suppressWarnings(system2(rs, c("tools/lint.R", shQuote(d)), stdout = TRUE, stderr = TRUE))
  expect_identical(attr(out, "status"), 1L)
  expect_false(any(grepl("^LINT OK", out)))
  expect_true(any(grepl("bad_net.R:3: network call: download.file", out, fixed = TRUE)))
})

# ---- fix round 1: p-gate hardening ------------------------------------------------------------

.pg <- function(lines) sr_lint_pgate(.tmp_r(lines))

test_that("p-gate follows compile bound to a named helper (list element, argument, alias)", {
  r <- .pg(c(
    'cmp <- function(spec) {',
    '  if (shapiro.test(spec$x)$p.value < .05) 1 else 2',
    '}',
    'route <- list(compile = cmp)'
  ))
  expect_identical(r$line, 2L)
  r <- .pg(c(
    '.cmp = function(spec) {',
    '  ifelse(ks.test(spec$x, "pnorm")$p.value > .05, 1, 2)',
    '}',
    'sr_route("a", compile = .cmp)'
  ))
  expect_identical(r$line, 2L)
  r <- .pg(c(
    'cmp <- function(spec) {',
    '  switch(if (d$p_value > .05) "a" else "b", a = 1)',
    '}',
    'compile <- cmp'
  ))
  expect_gte(nrow(r), 1L)
  # a helper that is not bound to compile is not linted
  r <- .pg(c(
    'other <- function(x) shapiro.test(x)',
    'ok <- function(spec) spec$sap$rule',
    'route <- list(compile = ok)'
  ))
  expect_identical(nrow(r), 0L)
})

test_that("p-gate flags any diagnostic call inside compile, closing local-variable taint", {
  r <- .pg(c(
    'compile <- function(spec) {',
    '  normal <- shapiro.test(spec$x)$p.value > .05',
    '  if (normal) 1 else 2',
    '}'
  ))
  expect_identical(r$line, 2L)
  r <- .pg(c(
    'compile <- function(spec) {',
    '  r <- tryCatch(shapiro.test(spec$x), error = function(e) NULL)',
    '  if (is.null(r)) 1 else 2',
    '}'
  ))
  expect_identical(r$line, 2L)
  r <- .pg(c(
    'compile <- function(spec) {',
    '  a <- stats::ad.test(spec$x)',
    '  b <- check_homogeneity(spec$m)',
    '  lapply(spec$x, shapiro.test)',
    '}'
  ))
  expect_identical(r$line, c(2L, 3L, 4L))
  expect_identical(unique(r$msg), "compile() branches on a diagnostic test (M1.1)")
})

test_that("p-gate flags new p-name symbols and strings anywhere; $p is condition-only", {
  r <- .pg(c(
    'compile <- function(spec) {',
    '  pv <- res$pval',
    '  q <- res$p_val',
    '  w <- res[["p.value"]]',
    '}'
  ))
  expect_identical(r$line, c(2L, 3L, 4L))
  expect_identical(nrow(.pg(c(
    'compile <- function(spec) {',
    '  n <- spec$p',
    '  if (n > 2) 1 else 2',
    '}'
  ))), 0L)
  expect_identical(.pg(c(
    'compile <- function(spec) {',
    '  if (res$p < .05) 1 else 2',
    '}'
  ))$line, 2L)
})

test_that("p-gate: one finding per offending line", {
  r <- .pg(c(
    'compile <- function(spec) {',
    '  if (shapiro.test(x)$p.value < .05 && ks.test(x)$p.value < .05) 1 else 2',
    '}'
  ))
  expect_identical(nrow(r), 1L)
})

test_that("p-gate covers case_when, while, and Pr(>F) strings in conditions", {
  r <- .pg(c(
    'compile <- function(spec) {',
    '  dplyr::case_when(res$p < .05 ~ "a", TRUE ~ "b")',
    '  case_when(d$p_value < .05 ~ 1)',
    '  while (res$p > .05) x <- x + 1',
    '  if (tab[["Pr(>F)"]][2] < .05) 1',
    '  if (tab[["Pr(>Chisq)"]][2] < .05) 1',
    '}'
  ))
  expect_identical(r$line, c(2L, 3L, 4L, 5L, 6L))
  r <- .pg(c(
    'compile <- function(spec) {',
    '  case_when(spec$sap$rule == "x" ~ spec$p, TRUE ~ 0)',
    '  i <- 0',
    '  while (i < 3) i <- i + 1',
    '}'
  ))
  expect_identical(nrow(r), 0L)
})

test_that("p-gate flags emitted code that branches on a diagnostic, but not a bare name", {
  r <- .pg(c(
    'compile <- function(spec) {',
    '  sprintf("ifelse(ks.test(%s)$p.value>.05,1,2)", v)',
    '}'
  ))
  expect_identical(r$line, 2L)
  r <- .pg(c(
    'compile <- function(spec) {',
    '  "if (shapiro.test(x)$p.value < .05) wilcox.test(x) else t.test(x)"',
    '}'
  ))
  expect_identical(r$line, 2L)
  r <- .pg(c(
    'compile <- function(spec) {',
    '  list(panel = "cox.zph(fit)", test = "wilcox.test(x)", d = "diff(x)")',
    '}'
  ))
  expect_identical(nrow(r), 0L)
})

.latin1_file <- function(env = parent.frame()) {
  p <- withr::local_tempfile(fileext = ".R", .local_envir = env)
  writeBin(as.raw(c(0x78, 0x20, 0x3c, 0x2d, 0x20, 0x22, 0xe9, 0x22, 0x0a)), p)
  p
}

test_that("p-gate: unreadable or non-UTF-8 files give one finding at line 1", {
  r <- sr_lint_pgate(.latin1_file())
  expect_identical(nrow(r), 1L)
  expect_identical(r$line, 1L)
  expect_match(r$msg, "cannot read or parse")
  r <- sr_lint_pgate(file.path(tempdir(), "no_such_file_zz.R"))
  expect_identical(nrow(r), 1L)
  expect_identical(r$line, 1L)
})

# ---- fix round 1: air-gap gaps ----------------------------------------------------------------

.ag <- function(lines) sr_lint_airgap(.tmp_r(lines), allow = character())

test_that("air-gap flags library/require/loadNamespace of network packages", {
  r <- .ag(c(
    'library(curl)',
    'require("httr")',
    'requireNamespace("httr2", quietly = TRUE)',
    'loadNamespace("RCurl")',
    'attachNamespace("curl")',
    'library(package = "curl")',
    'library(stats)',
    'require(utils)',
    'requireNamespace("jsonlite")'
  ))
  expect_identical(r$line, 1:6)
  expect_match(r$msg[1], "library(curl)", fixed = TRUE)
})

test_that("air-gap flags RCurl::, remotes:: and pak:: namespaces", {
  r <- .ag(c(
    'RCurl::getURL("a")',
    'remotes::install_github("a/b")',
    'pak::pkg_install("a")',
    'stats::sd(1)'
  ))
  expect_identical(r$line, 1:3)
})

test_that("air-gap flags unqualified curl/httr2/RCurl entry points", {
  r <- .ag(c(
    'curl_fetch_memory("a")', 'curl_fetch_disk("a", "b")', 'curl_fetch_stream("a", f)',
    'curl_download("a", "b")', 'req_perform(r)', 'getURL("a")', 'getBinaryURL("a")'
  ))
  expect_identical(r$line, 1:7)
})

test_that("air-gap flags bare references to network functions but not names or accessors", {
  r <- .ag(c(
    'lapply(u, download.file)',
    'dl <- download.file',
    'f <- list(install.packages, socketConnection)',
    'g <- reticulate::py_install',
    'x$download.file',
    'list(download.file = 1)',
    'h <- function(url) url',
    'u <- url'
  ))
  expect_identical(r$line, c(1L, 2L, 3L, 3L, 4L))
})

test_that("air-gap flags package-provisioning and font-download calls", {
  r <- .ag(c(
    'update.packages()', 'install_miniconda()', 'conda_install("a")',
    'virtualenv_create("a")', 'virtualenv_install("a", "b")',
    'bslib::font_google("Lato")', 'font_link("a", "b")'
  ))
  expect_identical(r$line, 1:7)
})

test_that("air-gap: unreadable or non-UTF-8 files give one finding at line 1", {
  r <- sr_lint_airgap(.latin1_file(), allow = character())
  expect_identical(nrow(r), 1L)
  expect_identical(r$line, 1L)
  expect_match(r$msg, "cannot read or parse")
  r <- sr_lint_airgap(file.path(tempdir(), "no_such_file_zz.R"), allow = character())
  expect_identical(nrow(r), 1L)
})

# ---- fix round 1: tools/lint.R robustness -----------------------------------------------------

.lint_run <- function(args = character(), wd = NULL) {
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  rs <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
  if (!is.null(wd)) withr::local_dir(wd)
  out <- suppressWarnings(system2(rs, c(file.path(root, "tools", "lint.R"), args), stdout = TRUE, stderr = TRUE))
  list(out = out, status = if (is.null(attr(out, "status"))) 0L else attr(out, "status"))
}

test_that("tools/lint.R scans the real app from any working directory and reports the count", {
  r <- .lint_run(wd = tempdir())
  expect_identical(r$status, 0L)
  m <- regmatches(r$out, regexec("^LINT OK [(]([0-9]+) files[)]$", r$out))
  m <- m[lengths(m) == 2L]
  expect_length(m, 1L)
  expect_gt(as.integer(m[[1]][2]), 5L)
})

test_that("tools/lint.R exits 2 on a missing directory or a directory with no R files", {
  r <- .lint_run(file.path(tempdir(), "no_such_dir_zz"))
  expect_identical(r$status, 2L)
  expect_false(any(grepl("LINT OK", r$out, fixed = TRUE)))
  d <- withr::local_tempdir()
  r <- .lint_run(d)
  expect_identical(r$status, 2L)
  expect_false(any(grepl("LINT OK", r$out, fixed = TRUE)))
})

# ---- fix round 2: long emitted-code strings ---------------------------------------------------

test_that("p-gate sees the full text of a string longer than 1000 characters", {
  pad <- strrep("a", 1200)
  r <- .pg(c(
    'compile <- function(spec) {',
    paste0('  tpl <- "', pad, ' if (shapiro.test(x)$p.value < .05) wilcox.test(x) else t.test(x)"'),
    '  tpl',
    '}'
  ))
  expect_identical(r$line, 2L)
  r <- .pg(c(
    'compile <- function(spec) {',
    paste0('  tpl <- "', pad, ' cox.zph(fit)"'),
    '  tpl',
    '}'
  ))
  expect_identical(nrow(r), 0L)
})
