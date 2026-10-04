# Run the test suite from the repo root: Rscript tools/run_tests.R [filter]
# An optional first argument is passed to testthat as `filter` (a regex on the file name after
# "test-" and before ".R"), e.g. `Rscript tools/run_tests.R samples`. No argument runs everything.
# NOT_CRAN makes skip_on_cran() a no-op here, so the app-launch test actually runs.
Sys.setenv(NOT_CRAN = "true")
args <- commandArgs(trailingOnly = TRUE)
filter <- if (length(args) >= 1L && nzchar(args[1L])) args[1L] else NULL
testthat::test_dir("tests/testthat", filter = filter, stop_on_failure = TRUE)
