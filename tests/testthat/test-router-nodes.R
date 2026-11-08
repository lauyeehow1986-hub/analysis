# Tests for router/nodes/*.R: the P1 decision nodes. Synthetic data only.

.nd_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

# 300 rows; death alternates 0/1; bmi_6m is a function of death (the planted post-outcome column);
# age, sex and smoker are balanced against it.
.nd_df <- function() {
  n <- 300L
  death <- rep(0:1, n / 2L)
  data.frame(id = seq_len(n), age = round(50 + 10 * sin(seq_len(n)), 1),
             sex = rep(c("F", "F", "M", "M"), n / 4L), smoker = rep(c(0L, 1L, 1L, 0L), n / 4L),
             bmi_6m = death * 3L + 2L, death = death, fu_days = 100L + (seq_len(n) * 7L) %% 90L,
             stringsAsFactors = FALSE)
}

.nd_roles <- c(id = "id", age = "covariate", sex = "covariate", smoker = "covariate",
               bmi_6m = "covariate", death = "outcome", fu_days = "covariate")

.nd_pico <- function(p, ...) sr_pico_set(p, population = "adults", outcome = "Death", ...)

.nd_ready <- function(env = parent.frame()) {
  .fx_ready_project(.nd_df(), "t", .nd_roles, env = env)
}

# A project with `df` ingested as table `table`, roles set, no DQ run.
.nd_small <- function(df, roles, table = "t", env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = table, options = list(drive_info = .nd_drive))
  sr_roles_set(p, table, roles)
}

.nd_row <- function(p, key) {
  tb <- sr_router_table(p)
  tb[tb$key == key, , drop = FALSE]
}

test_that("the P1 nodes are registered in dependency order with their citations", {
  ids <- c("design", "purpose", "guideline", "outcome_col", "exposure_col", "outcome_type",
           "primary_route", "covariate_timing")
  expect_true(all(ids %in% sr_nodes()))
  expect_identical(sr_nodes()[sr_nodes() %in% ids], ids)
  expect_identical(sr_node_get("design")$cite, "vonelm2007")
  expect_identical(sr_node_get("guideline")$cite, "vonelm2007")
  expect_identical(sr_node_get("exposure_col")$scope, "table")
  expect_identical(sr_node_get("covariate_timing")$scope, "column")
  expect_true(sr_node_get("outcome_col")$facts_columns)
})

test_that("guideline follows the design by rule", {
  p <- .nd_pico(.nd_ready())
  p <- sr_router_decide(p, "design", "rct")
  p <- sr_router_run(p)
  expect_identical(sr_router_choice(p, "guideline"), "consort")
  expect_identical(.nd_row(p, "guideline")$resolver, "rules")
  p <- sr_router_decide(p, "design", "case_control")
  p <- sr_router_run(p)
  expect_identical(sr_router_choice(p, "guideline"), "strobe")
  p <- sr_router_decide(p, "design", "other")
  p <- sr_router_run(p)
  expect_identical(sr_router_choice(p, "guideline"), "none")
})

test_that("purpose takes the stated aim", {
  p <- .nd_pico(.nd_ready(), aim = "describe")
  p <- sr_router_run(p)
  expect_identical(sr_router_choice(p, "purpose"), "describe")
  expect_identical(.nd_row(p, "purpose")$resolver, "rules")
  expect_identical(.nd_row(p, "purpose")$reason, "the stated aim")
  p2 <- sr_router_run(.nd_pico(.nd_ready()))
  expect_identical(.nd_row(p2, "purpose")$status, "pending")
})

test_that("outcome_col comes from a confirmed role, else an exact name match", {
  p <- sr_router_run(.nd_pico(.nd_ready(), aim = "explain-causal"))
  expect_identical(sr_router_choice(p, "outcome_col", "t"), "death")
  expect_identical(.nd_row(p, "outcome_col@t")$resolver, "rules")
  expect_identical(.nd_row(p, "outcome_col@t")$reason, "confirmed role")
  # no outcome role: the PICO outcome "Death" matches the column name
  p0 <- sr_roles_set(.nd_ready(), "t", c(id = "id"))
  p0 <- sr_router_run(.nd_pico(p0, aim = "explain-causal"))
  expect_identical(sr_router_choice(p0, "outcome_col", "t"), "death")
  # "mortality" matches no column
  p1 <- sr_roles_set(.nd_ready(), "t", c(id = "id"))
  p1 <- sr_router_run(sr_pico_set(p1, population = "adults", outcome = "mortality", aim = "explain-causal"))
  expect_identical(.nd_row(p1, "outcome_col@t")$status, "pending")
})

test_that("exposure_col does not apply to a describe study", {
  p <- sr_router_run(.nd_pico(.nd_ready(), aim = "describe"))
  expect_false("exposure_col@t" %in% sr_router_table(p)$key)
  p2 <- sr_router_run(.nd_pico(.nd_ready(), aim = "explain-causal"))
  expect_true("exposure_col@t" %in% sr_router_table(p2)$key)
})

test_that("exposure_col comes from the exposure role", {
  p <- sr_roles_set(.nd_ready(), "t", c(id = "id", death = "outcome", smoker = "exposure"))
  p <- sr_router_run(.nd_pico(p, aim = "explain-causal"))
  expect_identical(sr_router_choice(p, "exposure_col", "t"), "smoker")
})

test_that("outcome and exposure cannot be the same column", {
  p <- sr_router_run(.nd_pico(.nd_ready(), aim = "explain-causal"))
  expect_identical(sr_router_choice(p, "outcome_col", "t"), "death")
  cnd <- tryCatch(sr_router_decide(p, "exposure_col", "death", table = "t"), sr_router_vetoed = function(c) c)
  expect_s3_class(cnd, "sr_router_vetoed")
  expect_match(cnd$reason, "outcome")
  p <- sr_router_decide(p, "exposure_col", "smoker", table = "t")
  # the other way round: an outcome override equal to the exposure is vetoed
  cnd2 <- tryCatch(sr_router_decide(p, "outcome_col", "smoker", reason = "x", table = "t"),
                   sr_router_vetoed = function(c) c)
  expect_s3_class(cnd2, "sr_router_vetoed")
  # none for the exposure is allowed
  p <- sr_router_decide(p, "exposure_col", "none", reason = "no exposure", table = "t")
  expect_identical(sr_router_choice(p, "exposure_col", "t"), "none")
})

test_that("outcome_type by rule and its vetoes", {
  p <- sr_router_run(.nd_pico(.nd_ready(), aim = "explain-causal"))
  expect_identical(sr_router_choice(p, "outcome_type", "t"), "binary")
  # time / event roles give time_to_event
  df <- .nd_df()
  df$ev <- df$death
  pt <- .nd_small(df, c(id = "id", death = "outcome", fu_days = "time", ev = "event"))
  pt <- sr_router_run(.nd_pico(pt, aim = "explain-causal"))
  expect_identical(sr_router_choice(pt, "outcome_type", "t"), "time_to_event")
  # an event_date role alone also gives time_to_event
  df2 <- data.frame(id = 1:20, y = rep(0:1, 10), d = as.character(as.Date("2020-01-01") + 1:20))
  pe <- .nd_small(df2, c(id = "id", y = "outcome", d = "event_date"))
  pe <- sr_router_run(.nd_pico(pe, aim = "explain-causal"))
  expect_identical(sr_router_choice(pe, "outcome_type", "t"), "time_to_event")
  # an integer count outcome is left to the user; a 5-level outcome cannot be binary
  df3 <- data.frame(id = 1:300, cnt = rep(0:4, 60))
  pc <- .nd_small(df3, c(id = "id", cnt = "outcome"))
  pc <- sr_router_run(.nd_pico(pc, aim = "explain-causal"))
  expect_identical(.nd_row(pc, "outcome_type@t")$status, "pending")
  cnd <- tryCatch(sr_router_decide(pc, "outcome_type", "binary", table = "t"), sr_router_vetoed = function(c) c)
  expect_s3_class(cnd, "sr_router_vetoed")
  expect_match(cnd$reason, "distinct values")
  # time_to_event without the roles is vetoed
  cnd2 <- tryCatch(sr_router_decide(pc, "outcome_type", "time_to_event", table = "t"),
                   sr_router_vetoed = function(c) c)
  expect_s3_class(cnd2, "sr_router_vetoed")
  expect_match(cnd2$reason, "no time and event roles are confirmed")
  # a non-integer numeric outcome is continuous
  df4 <- data.frame(id = 1:30, y = (1:30) / 7)
  pn <- .nd_small(df4, c(id = "id", y = "outcome"))
  pn <- sr_router_run(.nd_pico(pn, aim = "explain-causal"))
  expect_identical(sr_router_choice(pn, "outcome_type", "t"), "continuous")
})

test_that("primary_route is advisory and purpose-checked", {
  p <- sr_router_run(.nd_pico(.nd_ready(), aim = "explain-causal"))
  expect_identical(sr_router_choice(p, "primary_route", "t"), "logistic")
  expect_match(.nd_row(p, "primary_route@t")$reason, "logistic")
  cnd <- tryCatch(sr_router_decide(p, "primary_route", "table1", reason = "x", table = "t"),
                  sr_router_vetoed = function(c) c)
  expect_s3_class(cnd, "sr_router_vetoed")
  expect_match(cnd$reason, "table1")
  # a route of the right purpose is accepted, as an override of the rule
  p <- sr_router_decide(p, "primary_route", "lm", reason = "treat as continuous", table = "t")
  expect_identical(sr_router_choice(p, "primary_route", "t"), "lm")
  # none is always allowed
  p <- sr_router_decide(p, "primary_route", "none", reason = "no P1 route", table = "t")
  expect_identical(sr_router_choice(p, "primary_route", "t"), "none")
  # describe gives table1
  pd <- sr_router_run(.nd_pico(.nd_ready(), aim = "describe"))
  expect_identical(sr_router_choice(pd, "primary_route", "t"), "table1")
  # predict has no P1 route: the user decides
  pp <- sr_router_run(.nd_pico(.nd_ready(), aim = "predict"))
  expect_identical(.nd_row(pp, "primary_route@t")$status, "pending")
})

test_that("covariate timing uses the DQ leakage findings", {
  p <- sr_router_run(.nd_pico(.nd_ready(), aim = "explain-causal"))
  expect_identical(sr_router_choice(p, "covariate_timing", "t", "bmi_6m"), "post_outcome")
  expect_identical(.nd_row(p, "covariate_timing@t:bmi_6m")$resolver, "rules")
  expect_match(.nd_row(p, "covariate_timing@t:bmi_6m")$reason, "[0-9a-f]{12}")
  expect_identical(.nd_row(p, "covariate_timing@t:age")$status, "pending")
  expect_false("covariate_timing@t:death" %in% sr_router_table(p)$key)
  expect_false("covariate_timing@t:id" %in% sr_router_table(p)$key)
  # the same data gives the same finding ids: still decided
  p2 <- sr_dq_run(p, "t")
  expect_identical(.nd_row(p2, "covariate_timing@t:bmi_6m")$status, "decided")
  # a run without the leakage check drops its finding ids: stale
  p3 <- sr_dq_run(p, "t", checks = "dup_rows")
  expect_identical(.nd_row(p3, "covariate_timing@t:bmi_6m")$status, "stale")
  # a describe study has no covariate timing
  pd <- sr_router_run(.nd_pico(.nd_ready(), aim = "describe"))
  expect_false(any(sr_router_table(pd)$node == "covariate_timing"))
})

test_that("covariate timing without a DQ run has no findings and does not error", {
  p <- .nd_small(.nd_df(), .nd_roles)
  p <- sr_router_run(.nd_pico(p, aim = "explain-causal"))
  expect_identical(.nd_row(p, "covariate_timing@t:bmi_6m")$status, "pending")
})

test_that("the design question is metadata only", {
  p <- .nd_pico(.nd_ready(), aim = "explain-causal")
  pv <- sr_router_preview(p, "design")
  expect_false(is.null(pv))
  expect_match(pv$body, "adults", fixed = TRUE)
  expect_match(pv$body, "outcome: Death", fixed = TRUE)
  expect_match(pv$body, "outcome=1", fixed = TRUE)
  expect_match(pv$body, "covariate=5", fixed = TRUE)
  for (v in c("\\\"F\\\"", "\\\"M\\\"", "SECRET", "61.5")) expect_false(grepl(v, pv$body, fixed = TRUE))
})

test_that("the column question names the columns and masks them on request", {
  p <- sr_router_run(.nd_pico(.nd_ready(), aim = "explain-causal"))
  pv <- sr_router_preview(p, "outcome_col", "t")
  expect_match(pv$text, "column.c006: name=death", fixed = TRUE)
  expect_match(pv$body, "none", fixed = TRUE)
  pm <- sr_router_preview(p, "outcome_col", "t", mask = TRUE)
  expect_false(grepl("bmi_6m", pm$body, fixed = TRUE))
})

test_that("300 columns: the column nodes skip the model", {
  df <- as.data.frame(matrix(seq_len(300L * 4L), nrow = 4L))
  names(df) <- sprintf("v%03d", seq_len(300L))
  p <- .nd_small(df, c(v001 = "id"))
  p <- .nd_pico(p, aim = "explain-causal")
  expect_null(sr_router_preview(p, "outcome_col", "t"))
  expect_null(sr_router_preview(p, "exposure_col", "t"))
})
