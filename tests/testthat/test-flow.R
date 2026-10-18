# Tests for profile/flow.R: the participant flow (row counts through the exclusion steps) and the
# CONSORT-style diagram. Synthetic data only; everything lives under withr::local_tempdir().

.fl_roles <- c(patient_id = "id", surgery_date = "time_zero", statin_start_date = "exposure_date",
               statin = "exposure", death_date = "event_date", mortality_30d = "outcome")

# The cardiac project with the study roles.
.fl_cardiac <- function(env = parent.frame()) {
  p <- .fx_cardiac_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  sr_roles_set(p, "cardiac", .fl_roles)
}

.fl_drop <- function(where, reason, ...) {
  list(op = "drop_cases", args = c(list(where = where), list(...)), reason = reason)
}

.fl_age <- list(col = "age", op = "==", value = 250)
.fl_glu <- list(col = "glucose_mmol", op = ">=", value = 60)
.fl_out <- list(col = "mortality_30d", op = "==", value = 1)

# Lock the SAP as the gate reads it (sr_blind() looks at state$sap$locked).
.fl_lock <- function(p) {
  p$state$sap <- list(locked = TRUE)
  p
}

.fl_png_path <- function(env = parent.frame()) {
  file.path(withr::local_tempdir(.local_envir = env), "out", "flow.png")
}

test_that("two drop_cases steps give two flow rows whose counts chain", {
  p <- .fl_cardiac()
  n0 <- as.numeric(p$state$tables$cardiac$store$rows)
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_age, "implausible age"))
  # exact-duplicate removal cannot be written in the recipe DSL: five rows carry the glucose sentinel
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_glu, "glucose sentinel value (60 mmol/L)"))
  fl <- sr_flow(p, "cardiac")
  expect_s3_class(fl, "data.frame")
  expect_identical(names(fl), c("step", "label", "n_before", "n_after", "n_excluded", "reason"))
  expect_identical(nrow(fl), 2L)
  expect_equal(fl$n_excluded, c(3, 5))
  expect_equal(fl$n_before, c(n0, n0 - 3))
  expect_equal(fl$n_after, c(n0 - 3, n0 - 8))
  expect_equal(fl$n_after[1L], fl$n_before[2L])
  expect_identical(fl$reason, c("implausible age", "glucose sentinel value (60 mmol/L)"))
  expect_equal(attr(fl, "n_total"), n0)
  expect_identical(fl$step, c("s1", "s2"))
})

test_that("only active drop_cases steps count; other ops and undone steps do not", {
  p <- .fl_cardiac()
  p <- sr_recipe_add(p, "cardiac", list(op = "set_missing", args = list(col = "weight_kg",
                                        where = list(col = "weight_kg", op = ">", value = 200)), reason = "implausible"))
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_age, "implausible age"))
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_glu, "glucose sentinel"))
  p <- sr_recipe_undo(p)
  fl <- sr_flow(p, "cardiac")
  expect_identical(nrow(fl), 1L)
  expect_equal(fl$n_excluded, 3)
  expect_identical(fl$step, "s2")
})

test_that("a flow with no steps has no rows and carries the total", {
  p <- .fl_cardiac()
  n0 <- as.numeric(p$state$tables$cardiac$store$rows)
  fl <- sr_flow(p, "cardiac")
  expect_identical(nrow(fl), 0L)
  expect_identical(names(fl), c("step", "label", "n_before", "n_after", "n_excluded", "reason"))
  expect_equal(attr(fl, "n_total"), n0)
  expect_error(sr_flow(p, "nope"), "not in the store")
})

test_that("cohort flow rows come first when the state holds them", {
  p <- .fl_cardiac()
  n0 <- as.numeric(p$state$tables$cardiac$store$rows)
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_age, "implausible age"))
  p$state$cohorts <- list(cardiac = list(flow = data.frame(
    step = "c1", label = "Adults", n_before = n0, n_after = n0 - 10, n_excluded = 10,
    reason = "under 18", stringsAsFactors = FALSE)))
  fl <- sr_flow(p, "cardiac")
  expect_identical(fl$step, c("c1", "s1"))
  expect_identical(fl$reason, c("under 18", "implausible age"))
  expect_equal(fl$n_excluded, c(10, 3))
})

test_that("a branch bound to another table gives no recipe rows", {
  p <- .fl_cardiac()
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_age, "implausible age"))
  small <- data.frame(id = 1:5, x = c(1, 2, 3, 4, 5))
  csv <- file.path(dirname(p$paths$dir), "small.csv")
  .fx_write_csv(small, csv)
  p <- sr_ingest(p, csv, table = "small")
  fl <- sr_flow(p, "small")
  expect_identical(nrow(fl), 0L)
  expect_equal(attr(fl, "n_total"), 5)
})

test_that("labels are ASCII; a reason passes through as UTF-8", {
  p <- .fl_cardiac()
  reason <- paste0("implausible age ", intToUtf8(c(0x00e9, 0x4e2d)))
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_age, reason))
  fl <- sr_flow(p, "cardiac")
  expect_false(anyNA(iconv(fl$label, "UTF-8", "ASCII")))
  expect_false(anyNA(iconv(fl$step, "UTF-8", "ASCII")))
  expect_identical(fl$reason, reason)
  expect_identical(Encoding(fl$reason), "UTF-8")
})

test_that("while blind, an outcome-dependent exclusion and the rows after it hide their counts", {
  p <- .fl_cardiac()
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_age, "implausible age"))
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_out, "excluded deaths (deliberate)", confirm_outcome_dependent = TRUE))
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_glu, "glucose sentinel"))
  expect_true(sr_blind(p))
  fl <- sr_flow(p, "cardiac")
  expect_identical(nrow(fl), 3L)
  expect_equal(fl$n_excluded[1L], 3)
  for (col in c("n_before", "n_after", "n_excluded")) expect_true(all(is.na(fl[[col]][2:3])), info = col)
  expect_false(anyNA(fl$n_before[1L]))
  expect_false(anyNA(fl$n_after[1L]))
  suffix <- " (count hidden until SAP lock)"
  expect_false(grepl(suffix, fl$label[1L], fixed = TRUE))
  expect_true(all(endsWith(fl$label[2:3], suffix)))
  # the reasons stay visible
  expect_identical(fl$reason[2L], "excluded deaths (deliberate)")

  # after the lock every count is shown
  fl2 <- sr_flow(.fl_lock(p), "cardiac")
  expect_false(anyNA(fl2$n_after))
  expect_true(fl2$n_excluded[2L] > 0)
  expect_false(any(grepl("hidden", fl2$label)))
  expect_equal(fl2$n_after[1L], fl2$n_before[2L])
  expect_equal(fl2$n_after[2L], fl2$n_before[3L])
})

test_that("sr_flow_plot writes a PNG; a flow with no steps is one box", {
  p <- .fl_cardiac()
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_age, "implausible age"))
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_glu, "glucose sentinel value (60 mmol/L)"))
  fl <- sr_flow(p, "cardiac")
  path <- .fl_png_path()
  out <- sr_flow_plot(fl, path)
  expect_identical(out, path)
  expect_true(file.exists(path))
  expect_gt(file.size(path), 10000)
  expect_false(file.exists(paste0(path, ".tmp")))
  if (requireNamespace("png", quietly = TRUE)) {
    d <- dim(png::readPNG(path))
    expect_true(all(d > 0))
    # width 7 in at 300 dpi; the height is computed from the number of boxes
    expect_equal(d[2L], 7 * 300)
    expect_gt(d[1L], 1000)
  }

  # a taller diagram for more boxes, and an explicit height wins
  path2 <- .fl_png_path()
  sr_flow_plot(sr_flow(sr_recipe_add(p, "cardiac", .fl_drop(list(col = "sbp", op = "==", value = -99), "sentinel sbp")),
                       "cardiac"), path2)
  if (requireNamespace("png", quietly = TRUE)) {
    expect_gt(dim(png::readPNG(path2))[1L], dim(png::readPNG(path))[1L])
    path3 <- .fl_png_path()
    sr_flow_plot(fl, path3, width = 6, height = 3)
    expect_equal(dim(png::readPNG(path3))[1:2], c(3 * 300, 6 * 300))
  }

  p0 <- .fl_cardiac()
  fl0 <- sr_flow(p0, "cardiac")
  path0 <- .fl_png_path()
  sr_flow_plot(fl0, path0)
  expect_gt(file.size(path0), 5000)
  if (requireNamespace("png", quietly = TRUE)) expect_true(all(dim(png::readPNG(path0)) > 0))
})

test_that("the diagram draws hidden counts and long non-ASCII reasons, and refuses bad input", {
  p <- .fl_cardiac()
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_age, paste0("implausible age ", intToUtf8(0x00e9),
                                          " and a long reason that has to wrap over several lines of the side box")))
  p <- sr_recipe_add(p, "cardiac", .fl_drop(.fl_out, "excluded deaths", confirm_outcome_dependent = TRUE))
  fl <- sr_flow(p, "cardiac")
  path <- .fl_png_path()
  expect_silent(sr_flow_plot(fl, path))
  expect_gt(file.size(path), 10000)

  expect_error(sr_flow_plot(list(), path), "flow")
  expect_error(sr_flow_plot(fl, c(path, path)), "path")
  expect_error(sr_flow_plot(fl, file.path(dirname(path), "flow.txt")), "[.]png")
  expect_error(sr_flow_plot(fl, path, width = -1), "width")
  nt <- fl[0, ]
  attr(nt, "n_total") <- NULL
  expect_error(sr_flow_plot(nt, path), "n_total")
})

# ---- small-cell masking (k) -------------------------------------------------------------------------

test_that(".sr_fl_n masks counts of 1 to k - 1 only when k is given; zero and text are left alone", {
  expect_identical(.sr_fl_n(c(0, 1, 4, 5, 120, NA)), c("0", "1", "4", "5", "120", "hidden"))
  expect_identical(.sr_fl_n(c(0, 1, 4, 5, 120, NA), 5L), c("0", "<5", "<5", "5", "120", "hidden"))
  expect_identical(.sr_fl_n(c("<5", "40", NA), 5L), c("<5", "40", "hidden"))
})

test_that("sr_flow_plot masks small counts with k, leaves the default alone, and rejects a bad k", {
  fl <- data.frame(step = "s1", label = "After age filter", n_before = 120, n_after = 117, n_excluded = 3,
                   reason = "age out of range", stringsAsFactors = FALSE)
  attr(fl, "n_total") <- 120
  big <- fl
  big$n_after <- 100
  big$n_excluded <- 20
  p0 <- .fl_png_path()
  p1 <- .fl_png_path()
  p2 <- .fl_png_path()
  sr_flow_plot(fl, p0)
  sr_flow_plot(fl, p1, k = NULL)
  expect_identical(.sr_sha256(.sr_read_bytes(p0)), .sr_sha256(.sr_read_bytes(p1)))
  sr_flow_plot(fl, p2, k = 5L)
  expect_true(file.exists(p2))
  expect_false(identical(.sr_sha256(.sr_read_bytes(p0)), .sr_sha256(.sr_read_bytes(p2))))
  # no small count: k changes nothing
  q0 <- .fl_png_path()
  q1 <- .fl_png_path()
  sr_flow_plot(big, q0)
  sr_flow_plot(big, q1, k = 5L)
  expect_identical(.sr_sha256(.sr_read_bytes(q0)), .sr_sha256(.sr_read_bytes(q1)))
  # a zero exclusion stays "0": same picture with and without k
  z <- fl
  z$n_after <- 120
  z$n_excluded <- 0
  z0 <- .fl_png_path()
  z1 <- .fl_png_path()
  sr_flow_plot(z, z0)
  sr_flow_plot(z, z1, k = 5L)
  expect_identical(.sr_sha256(.sr_read_bytes(z0)), .sr_sha256(.sr_read_bytes(z1)))
  expect_error(sr_flow_plot(fl, .fl_png_path(), k = 1), "k must be")
  expect_error(sr_flow_plot(fl, .fl_png_path(), k = "5"), "k must be")
})
