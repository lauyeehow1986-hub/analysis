# Run the whole test suite from the repo root: Rscript tools/run_tests.R
testthat::test_dir("tests/testthat", stop_on_failure = TRUE)
