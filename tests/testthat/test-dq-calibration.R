# Calibration of the DQ checks on the synthetic cardiac twins: every planted defect is found (recall 1.0)
# and the clean twin raises no warn+ finding outside its by-design allowlist. The answer key (expected
# severities, the allowlist) and the runner live in tools/dq_calibrate.R, which prints the same numbers
# over the full grid of seeds and sizes. Synthetic data only; each run lives in a temp dir.

source(file.path(normalizePath(testthat::test_path("../.."), winslash = "/"), "tools", "dq_calibrate.R"),
       local = TRUE)

# What a calibration cell must satisfy: the answer key lists exactly the planted defects, every one is
# matched by a finding of the same check and column at its expected severity or higher, and the clean
# twin has no warn+ finding outside the allowlist.
.cal_expect_cell <- function(seed, n) {
  lab <- sprintf("seed %d, n %d", seed, n)
  fd <- .cal_run(seed, n, TRUE)
  pl <- attr(fd, "planted")
  expect_identical(paste(SR_CAL_EXPECTED$check, SR_CAL_EXPECTED$column), paste(pl$check, pl$column), info = lab)
  missed <- paste0(SR_CAL_EXPECTED$check, "/", SR_CAL_EXPECTED$column)[!.cal_matched(fd)]
  expect_identical(missed, character(),
                   info = paste0("planted defects not found at the expected severity (", lab, "): ", paste(missed, collapse = ", ")))
  w <- .cal_clean_warn(.cal_run(seed, n, FALSE))
  fp <- paste0(w$check, "/", w$column)[!w$allowed]
  expect_identical(fp, character(),
                   info = paste0("clean-twin warn+ findings outside the allowlist (", lab, "): ", paste(fp, collapse = ", ")))
}

test_that("the answer key is well formed", {
  expect_identical(nrow(SR_CAL_EXPECTED), 17L)
  expect_true(all(SR_CAL_EXPECTED$severity %in% SR_DQ_SEVERITIES))
  expect_true(all(SR_CAL_EXPECTED$check %in% sr_dq_checks()))
  expect_identical(paste(SR_CAL_ALLOW$check, SR_CAL_ALLOW$column),
                   c("missing_col death_date", "missing_col statin_start_date",
                     "missing_dependence death_date", "missing_dependence statin_start_date"))
})

test_that("seeds 1-5 at n = 5000: recall is 1 and the clean twin has no unexpected warn+ finding", {
  for (seed in 1:5) .cal_expect_cell(seed, 5000L)
})

test_that("one seed at n = 200000: recall is 1 and the clean twin has no unexpected warn+ finding", {
  skip_on_cran()
  .cal_expect_cell(1L, 200000L)
})
