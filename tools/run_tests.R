# Run the whole test suite from the repo root: Rscript tools/run_tests.R
# NOT_CRAN makes skip_on_cran() a no-op here, so the app-launch test actually runs.
Sys.setenv(NOT_CRAN = "true")
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
