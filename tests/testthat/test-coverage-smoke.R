# Smoke test of resample/coverage.R, the CI-coverage simulation harness. Every cell runs with few
# replications (bootstrap cells with B = 50) and returns the documented fields; the same seed gives
# the same result, whatever the number of workers. Coverage itself is NOT asserted here: that is the
# full simulation of tools/coverage_sim.R, recorded in docs/validation.md.

.cov_fields <- c("cell", "reps", "coverage", "mcse", "pass", "bias", "mean_width", "failed", "truth",
                 "errors")

.cov_check <- function(r, cell, reps) {
  expect_identical(names(r), .cov_fields)
  expect_identical(r$cell, cell)
  expect_identical(r$reps, as.integer(reps))
  expect_identical(r$failed, 0L, info = paste(r$errors, collapse = "; "))
  expect_identical(r$errors, character())
  expect_true(is.numeric(r$coverage) && length(r$coverage) == 1L && r$coverage >= 0 && r$coverage <= 1)
  expect_equal(r$mcse, sqrt(r$coverage * (1 - r$coverage) / (r$reps - r$failed)))
  expect_identical(r$pass, abs(r$coverage - 0.95) < 3 * r$mcse)
  expect_true(is.finite(r$bias) && is.finite(r$mean_width) && r$mean_width > 0)
  expect_true(is.finite(r$truth))
}

test_that("the cell table lists the six cells of the plan with their replications, B and m", {
  cells <- sr_coverage_cells()
  expect_identical(cells$cell, c("cc", "mi_mar", "boot", "boot_mi", "cluster_robust", "cluster_boot"))
  expect_identical(cells$reps, c(1000L, 500L, 500L, 500L, 1000L, 500L))
  expect_identical(cells$B, c(NA, NA, 500L, 200L, NA, 500L))
  expect_identical(cells$m, c(NA, 20L, NA, 10L, NA, NA))
  expect_true(all(nzchar(cells$generator)) && all(nzchar(cells$analysis)))
})

test_that("the summary: mcse and pass follow the brief, and coverage 1 is a failure (mcse 0)", {
  s <- .sr_cov_summarise(est = c(1, 2, 3, 4), lo = c(0, 1, 2, 3.5), hi = c(2, 3, 4, 5), truth = 3)
  expect_equal(s$coverage, 0.5)
  expect_equal(s$mcse, sqrt(0.5 * 0.5 / 4))
  expect_true(s$pass)   # |0.5 - 0.95| = 0.45 < 3 * 0.25: four replications decide nothing
  expect_equal(s$bias, 2.5 - 3)
  expect_equal(s$mean_width, mean(c(2, 2, 2, 1.5)))
  low <- .sr_cov_summarise(est = rep(1, 100), lo = c(rep(0, 80), rep(1.5, 20)), hi = rep(2, 100), truth = 1)
  expect_equal(low$coverage, 0.8)
  expect_equal(low$mcse, 0.04)
  expect_false(low$pass)   # 0.15 >= 3 * 0.04
  all_in <- .sr_cov_summarise(est = rep(1, 10), lo = rep(0, 10), hi = rep(2, 10), truth = 1)
  expect_identical(all_in$coverage, 1)
  expect_identical(all_in$mcse, 0)
  expect_false(all_in$pass)
  ok <- .sr_cov_summarise(est = rep(1, 20), lo = c(rep(0, 19), 1.5), hi = rep(2, 20), truth = 1)
  expect_equal(ok$coverage, 0.95)
  expect_true(ok$pass)
  none <- .sr_cov_summarise(numeric(), numeric(), numeric(), truth = 1)
  expect_true(is.na(none$coverage))
  expect_false(none$pass)
})

test_that("the true marginal risk difference of the logistic cell matches a Monte Carlo of the generator", {
  truth <- .sr_cov_rd_truth()
  mc <- withr::with_seed(11, {
    z <- stats::rnorm(1e6)
    mean(stats::plogis(-0.5 + 0.8 + 0.6 * z) - stats::plogis(-0.5 + 0.6 * z))
  })
  expect_lt(abs(truth - mc), 1e-3)
})

test_that("arguments are validated", {
  expect_error(sr_coverage_cell("nope", 5L), "unknown cell")
  expect_error(sr_coverage_cell("cc", 1L), "reps")
  expect_error(sr_coverage_cell("cc", 5L, n = 20L), "n must")
  expect_error(sr_coverage_cell("cc", 5L, B = 50L), "bootstrap")
  expect_error(sr_coverage_cell("boot", 5L, B = 10L), "B")
  expect_error(sr_coverage_cell("cc", 5L, workers = 0L), "workers")
  expect_error(sr_coverage_cell("cc", 5L, seed = 1.5), "seed")
  expect_error(sr_coverage_cell("cc", 5L, m = 3L), "imputation")
  expect_error(sr_coverage_cell("mi_mar", 5L, m = 1L), "m must")
})

test_that("every cell returns the documented fields and repeats exactly with the same seed", {
  for (cell in sr_coverage_cells()$cell) {
    B <- if (cell %in% c("boot", "boot_mi", "cluster_boot")) 50L else NULL
    # boot_mi is the costly cell (B bootstrap samples, each imputed m times): smoke-test it with m = 2
    m <- if (identical(cell, "boot_mi")) 2L else NULL
    r1 <- sr_coverage_cell(cell, 20L, B = B, m = m)
    .cov_check(r1, cell, 20L)
    r2 <- sr_coverage_cell(cell, 20L, B = B, m = m)
    expect_identical(r2, r1, info = cell)
  }
})

test_that("a different seed gives different replications", {
  a <- sr_coverage_cell("cc", 20L, seed = 1L)
  b <- sr_coverage_cell("cc", 20L, seed = 2L)
  expect_false(identical(a$bias, b$bias))
})

test_that("two workers give the same result as one, and the caller's RNG is restored", {
  skip_on_cran()
  set.seed(99, kind = "Mersenne-Twister")
  kind <- RNGkind()
  seed <- .Random.seed
  one <- sr_coverage_cell("cc", 20L, seed = 3L, workers = 1L)
  expect_identical(RNGkind(), kind)
  expect_identical(.Random.seed, seed)
  two <- sr_coverage_cell("cc", 20L, seed = 3L, workers = 2L)
  expect_identical(RNGkind(), kind)
  expect_identical(.Random.seed, seed)
  expect_identical(two, one)
})

test_that("a replication whose fit fails is counted in `failed`, named, and left out of the coverage", {
  setup <- .sr_cov_setup("cc", n = 200L, B = NULL, m = NULL, seed = 1L)
  setup$body <- paste("stop(\"planted failure\")")
  streams <- sr_streams(5L, 3L)
  withr::local_preserve_seed()
  out <- lapply(streams, .sr_cov_rep, setup = setup)
  expect_true(all(!vapply(out, function(o) o$ok, NA)))
  expect_identical(unique(vapply(out, function(o) o$error, "")), "planted failure")
  r <- .sr_cov_collect("cc", out, truth = 0.5)
  expect_identical(r$failed, 3L)
  expect_identical(r$errors, "planted failure (3)")
  expect_true(is.na(r$coverage))
  expect_false(r$pass)
})
