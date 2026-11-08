test_that(".sr_libsnap_diff lists updated, added and removed items", {
  a <- list(r = "R 4.5.2", snapshot_date = NA, pkgs = c(x = "1.0", y = "2.0", z = NA))
  b <- list(r = "R 4.5.3", snapshot_date = "2026-11-01", pkgs = c(x = "1.1", y = NA, z = "0.1"))
  d <- .sr_libsnap_diff(a, b)
  expect_identical(d$item, c("R", "snapshot_date", "x", "y", "z"))
  expect_identical(d$change, c("updated", "added", "updated", "removed", "added"))
  expect_identical(nrow(.sr_libsnap_diff(a, a)), 0L)
})

test_that(".sr_libsnap_diff accepts pkgs as a list (a replayed baseline)", {
  a <- list(r = "R 4.5.2", snapshot_date = NULL, pkgs = list(x = "1.0", y = NULL, z = NA))
  b <- list(r = "R 4.5.2", snapshot_date = NA, pkgs = c(x = "1.0", y = "2.0", z = NA))
  d <- .sr_libsnap_diff(a, b)
  expect_identical(d$item, "y")
  expect_identical(d$change, "added")
})

test_that("after a freeze, sr_run refuses a changed library until sr_rebaseline", {
  p <- .fx_writer_project()
  p <- sr_run(p, "a_lr")                                    # no freeze: no gate
  p <- sr_freeze(p)
  p <- sr_run(p, "a_lr")                                    # same library: runs (cached)
  withr::local_options(sr.cran_snapshot = "2099-01-01")
  n_ev <- length(.sr_events_load(p$paths$events)$events)
  expect_error(sr_run(p, "a_lr"), "library differs from the baseline of freeze 'submission-v1'.*sr_rebaseline")
  expect_identical(length(.sr_events_load(p$paths$events)$events), n_ev)   # refused before any event
  d <- sr_rebaseline_diff(p)
  expect_identical(d$item, "snapshot_date")
  expect_error(sr_rebaseline(p, "  "), "reason")
  p <- sr_rebaseline(p, "CRAN snapshot moved for the revision")
  expect_identical(sr_lib_baseline(p)$from, "rebaseline")
  expect_identical(sr_lib_baseline(p)$hash, sr_libsnap_hash())
  p <- sr_run(p, "a_lr")
  expect_true(p$state$runs$a_lr$ok)
  expect_error(sr_rebaseline(p, "again"), "matches the baseline")
})

test_that("re-baseline without a freeze is refused", {
  p <- .fx_writer_project()
  expect_null(sr_lib_baseline(p))
  expect_error(sr_rebaseline_diff(p), "no freeze yet")
  expect_error(sr_rebaseline(p, "x"), "no freeze yet")
})
