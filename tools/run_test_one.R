# Run one test file of a repo and write its counts (used by tools/run_tests_each.R):
#   Rscript tools/run_test_one.R <repo> <name> <result-file>
# <name> is the part of the file name between "test-" and ".R". The result file gets one line:
# n_expect,failed,errors,skipped,warnings.
args <- commandArgs(trailingOnly = TRUE)
Sys.setenv(NOT_CRAN = "true")
repo <- args[1L]
nm <- args[2L]
res <- args[3L]
testthat::set_max_fails(Inf)
pat <- paste0("^", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", nm), "$")
r <- testthat::test_dir(file.path(repo, "tests", "testthat"), filter = pat, stop_on_failure = FALSE,
                        stop_on_warning = FALSE, reporter = testthat::ProgressReporter$new(show_praise = FALSE))
d <- as.data.frame(r)
counts <- c(sum(d$nb), sum(d$failed), sum(d$error), sum(d$skipped), sum(d$warning))
writeLines(paste(counts, collapse = ","), res)
