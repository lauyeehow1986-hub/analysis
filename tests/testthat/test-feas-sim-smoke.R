# Smoke test of resample/feas_sim.R, the feasibility SE-validation harness. Every cell runs with few
# replications and returns the documented fields with no failed replication and a finite ratio; the
# same seed gives the same result whatever the number of workers. `pass` is NOT asserted here: that is
# the full simulation of tools/feas_sim.R, recorded in docs/validation.md.

.fsim_fields <- c("cell", "reps", "pred_se", "emp_sd", "ratio", "pass", "failed", "errors")

.fsim_check <- function(r, cell, reps) {
  expect_identical(names(r), .fsim_fields)
  expect_identical(r$cell, cell)
  expect_identical(r$reps, as.integer(reps))
  expect_identical(r$failed, 0L, info = paste(cell, paste(r$errors, collapse = "; ")))
  expect_identical(r$errors, character())
  expect_true(is.numeric(r$pred_se) && length(r$pred_se) == 1L && is.finite(r$pred_se) && r$pred_se > 0,
              info = cell)
  expect_true(is.numeric(r$emp_sd) && length(r$emp_sd) == 1L && is.finite(r$emp_sd) && r$emp_sd > 0,
              info = cell)
  expect_true(is.finite(r$ratio), info = cell)
  expect_equal(r$ratio, r$pred_se / r$emp_sd)
  expect_identical(r$pass, abs(r$ratio - 1) <= 0.10)
}

test_that("the cell table lists the eleven cells of the plan", {
  cells <- sr_feas_sim_cells()
  expect_identical(names(cells), c("cell", "route", "measure", "clustered", "reps", "n", "pred_every"))
  expect_identical(cells$cell, c("md", "rd", "rr", "or", "hr", "rmst", "md_cl", "rd_cl", "rr_cl", "or_cl",
                                 "hr_cl"))
  expect_identical(cells$route, c("lm", "logistic", "modpois", "logistic", "cox", "rmst", "lm", "logistic",
                                  "modpois", "logistic", "cox"))
  expect_identical(cells$measure, c("MD", "RD", "RR", "OR", "HR", "RMST_diff", "MD", "RD", "RR", "OR", "HR"))
  expect_identical(cells$clustered, c(rep(FALSE, 6L), rep(TRUE, 5L)))
  expect_identical(cells$reps, rep(1000L, 11L))
  expect_identical(cells$n, c(400L, 600L, 600L, 600L, 400L, 400L, 400L, 600L, 600L, 600L, 400L))
  expect_identical(cells$pred_every, c(1L, 1L, 1L, 1L, 1L, 10L, 1L, 1L, 1L, 1L, 1L))
})

test_that("arguments are validated", {
  expect_error(sr_feas_sim_cell("nope", 5L), "unknown cell")
  expect_error(sr_feas_sim_cell("md", 1L), "reps")
  expect_error(sr_feas_sim_cell("md", 5L, n = 20L), "n must")
  expect_error(sr_feas_sim_cell("md", 5L, workers = 0L), "workers")
  expect_error(sr_feas_sim_cell("md", 5L, seed = 1.5), "seed")
})

test_that("every cell returns the documented fields with no failed replication and a finite ratio", {
  for (cell in sr_feas_sim_cells()$cell) {
    reps <- if (identical(cell, "rmst")) 10L else 20L
    .fsim_check(sr_feas_sim_cell(cell, reps), cell, reps)
  }
})

test_that("two workers give the same result as one, and the caller's RNG is restored", {
  skip_on_cran()
  set.seed(99, kind = "Mersenne-Twister")
  kind <- RNGkind()
  seed <- .Random.seed
  one <- sr_feas_sim_cell("md", 20L, seed = 3L, workers = 1L)
  expect_identical(RNGkind(), kind)
  expect_identical(.Random.seed, seed)
  two <- sr_feas_sim_cell("md", 20L, seed = 3L, workers = 2L)
  expect_identical(RNGkind(), kind)
  expect_identical(.Random.seed, seed)
  expect_identical(two, one)
})

test_that("rmst is worker-invariant too, with a per-replication prediction seed", {
  skip_on_cran()
  one <- sr_feas_sim_cell("rmst", 12L, seed = 2L, workers = 1L)
  two <- sr_feas_sim_cell("rmst", 12L, seed = 2L, workers = 2L)
  expect_identical(two, one)
})

test_that("the prediction uses the engine seed it is given (rmst Monte Carlo error is not shared)", {
  setup <- .sr_fsim_setup("rmst", 400L, 1L)
  df <- withr::with_seed(5L, setup$gen(400L), .rng_kind = "L'Ecuyer-CMRG")
  a <- .sr_fsim_predict(df, setup, 11L)
  expect_identical(.sr_fsim_predict(df, setup, 11L), a)
  expect_false(identical(.sr_fsim_predict(df, setup, 12L), a))
})

test_that("the stored hr_cl ICC agrees with the closed form within Monte Carlo error", {
  exact <- .sr_fsim_hr_cl_icc_exact()
  expect_equal(exact, 0.16386, tolerance = 1e-4)
  expect_lt(abs(SR_FEAS_SIM_HR_CL_ICC - exact), 0.005)
})
