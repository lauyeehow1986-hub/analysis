test_that("sr_freeze_diff shows a re-imported table version added after the submission freeze", {
  p <- sr_freeze(.fx_writer_project())
  df <- .fx_writer_frame(); df$age[1] <- df$age[1] + 1
  csv <- file.path(withr::local_tempdir(), "t_fix.csv"); .fx_write_csv(df, csv)
  p <- sr_reimport(p, "t", csv, key = "id")
  p <- sr_source_hash(p, "t_r1")
  p <- sr_freeze(p)
  d <- sr_freeze_diff(p, "submission-v1", "revision-r1")
  row <- d$tables[d$tables$table == "t_r1", ]
  expect_identical(row$change, "added"); expect_identical(row$base, "t"); expect_identical(row$version, 1L)
  expect_identical(d$tables$change[d$tables$table == "t"], "same")
  expect_false(d$sap$changed)
  expect_identical(nrow(d$library), 0L)
  expect_true(all(d$analyses$change == "same"))
})

test_that("sr_freeze_diff against now sees a library change and a re-run", {
  p <- sr_freeze(.fx_writer_project())
  withr::local_options(sr.cran_snapshot = "2099-01-01")
  p <- sr_rebaseline(p, "snapshot moved")
  p <- sr_run(p, "a_lr")
  d <- sr_freeze_diff(p, "submission-v1")
  expect_identical(d$library$item, "snapshot_date")
  expect_identical(d$analyses$change[d$analyses$analysis == "a_lr"], "rerun")
  r <- sr_freeze_results_diff(p, "submission-v1")
  expect_identical(r$status[r$analysis == "a_lr"], "identical")
  expect_identical(r$max_abs_diff[r$analysis == "a_lr"], 0)
  expect_identical(r$status[r$analysis == "a_cox"], "same_run")
})

test_that(".sr_fz_numbers and the changed / structure / missing statuses", {
  expect_identical(.sr_fz_numbers(list(a = 1L, b = list(c = 2.5, d = "x"))), c(a = 1, b.c = 2.5))
  p <- sr_freeze(.fx_writer_project())
  withr::local_options(sr.cran_snapshot = "2099-01-01")
  p <- sr_rebaseline(p, "snapshot moved")
  p <- sr_run(p, "a_lr")
  f <- file.path(p$paths$fits, paste0(p$state$runs$a_lr$ok_key, ".rds"))
  s <- readRDS(f)
  s$value <- rapply(s$value, function(z) z * 1.01, classes = "numeric", how = "replace"); saveRDS(s, f)
  r <- sr_freeze_results_diff(p, "submission-v1")
  expect_identical(r$status[r$analysis == "a_lr"], "changed")
  expect_gt(r$max_rel_diff[r$analysis == "a_lr"], 0.009)
  file.remove(file.path(p$paths$fits, paste0(sr_freeze_manifest(p, "submission-v1")$runs$a_rmst$ok_key, ".rds")))
  p$state$runs$a_rmst$ok_key <- "0000"
  r <- sr_freeze_results_diff(p, "submission-v1")
  expect_identical(r$status[r$analysis == "a_rmst"], "missing")
})

test_that("results diff: structure, not_run, NA handling", {
  p <- sr_freeze(.fx_writer_project())
  withr::local_options(sr.cran_snapshot = "2099-01-01")
  p <- sr_rebaseline(p, "snapshot moved")
  p <- sr_run(p, "a_lr")
  f <- file.path(p$paths$fits, paste0(p$state$runs$a_lr$ok_key, ".rds"))
  s <- readRDS(f); s$value <- list(extra = 1); saveRDS(s, f)
  r <- sr_freeze_results_diff(p, "submission-v1")
  expect_identical(r$status[r$analysis == "a_lr"], "structure")
  expect_true(is.na(r$max_abs_diff[r$analysis == "a_lr"]))
  p$state$runs$a_cox$ok_key <- NULL
  r <- sr_freeze_results_diff(p, "submission-v1")
  expect_identical(r$status[r$analysis == "a_cox"], "not_run")
})

test_that("an old manifest with an unnamed libsnap pkgs does not crash the diff", {
  p <- sr_freeze(.fx_writer_project())
  now <- .sr_freeze_now(p)
  expect_identical(now$libsnap_hash, sr_freeze_manifest(p, "submission-v1")$libsnap_hash)
  old <- sr_freeze_manifest(p, "submission-v1")
  old$libsnap$pkgs <- unname(old$libsnap$pkgs)
  old$libsnap_hash <- "deadbeef"
  d <- .sr_freeze_diff_manifests(old, now)
  expect_true("packages" %in% d$library$item)
  expect_identical(d$library$from[d$library$item == "packages"], "unknown")
  old$libsnap_hash <- now$libsnap_hash
  expect_identical(nrow(.sr_freeze_diff_manifests(old, now)$library), 0L)
})
