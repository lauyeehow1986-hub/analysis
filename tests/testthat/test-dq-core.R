# Tests for profile/dq.R, dq_conformance.R and dq_completeness.R: the findings schema, the registry, the
# runner, and the conformance and completeness checks. Synthetic data only; everything lives under
# withr::local_tempdir().

.dq_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

# The cardiac project with its dictionary stored (the dictionary describes the intended data, so it is
# the same for the defect version and the clean twin).
.dq_cardiac <- function(defects = TRUE, n = 2000L, env = parent.frame()) {
  p <- .fx_cardiac_project(n = n, defects = defects, env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), "cardiac_dict.csv")
  .fx_write_csv(sr_sample_cardiac(n = n, defects = defects)$dict, csv)
  sr_dict_set(p, "cardiac", sr_dict_read_csv(csv), "csv")
}

# Ingest a data frame as `table` into an open project.
.dq_ingest <- function(p, df, table) {
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  sr_ingest(p, csv, table = table, options = list(drive_info = .dq_drive))
}

.dq_small <- function(df, table = "small", env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  .dq_ingest(p, df, table)
}

# One dictionary row per column of `df`; `...` are per-variable overrides given as a list of lists.
.dq_dict <- function(variable, type = "text", min = "", max = "", levels = NULL, missing_codes = NULL) {
  n <- length(variable)
  d <- data.frame(variable = variable, label = variable, type = type, units = "", min = min, max = max,
                  stringsAsFactors = FALSE)
  empty <- data.frame(code = character(), label = character(), stringsAsFactors = FALSE)
  d$levels <- if (is.null(levels)) rep(list(empty), n) else levels
  d$missing_codes <- if (is.null(missing_codes)) rep(list(character()), n) else missing_codes
  d$source <- "test"
  d
}

.dq_find <- function(f, check, column = NULL) {
  f[f$check == check & (is.null(column) | f$column %in% column), , drop = FALSE]
}

# ---- schema ----------------------------------------------------------------------------------------

test_that("the empty findings frame has the specified columns and types", {
  e <- sr_findings_empty()
  expect_identical(names(e), c("id", "check", "domain", "column", "severity", "n_affected", "p_affected",
                               "message", "where", "examples", "detail"))
  expect_identical(vapply(e[1:8], function(v) class(v)[1L], ""),
                   c(id = "character", check = "character", domain = "character", column = "character",
                     severity = "character", n_affected = "integer", p_affected = "numeric", message = "character"))
  expect_true(is.list(e$where) && is.list(e$examples) && is.list(e$detail))
  expect_identical(nrow(e), 0L)
  expect_identical(SR_DQ_DOMAINS, c("conformance", "completeness", "plausibility", "uniqueness", "structure", "leakage"))
  expect_identical(SR_DQ_SEVERITIES, c("info", "warn", "block"))
})

test_that("a finding id is the first 12 hex characters of sha256(check|column|where json)", {
  w <- .sr_where_leaf("sbp", "==", -99)
  f <- .sr_dq_finding("missing_codes", "completeness", "sbp", "warn", 5L, 0.1, "m", where = w)
  wj <- as.character(jsonlite::toJSON(w, auto_unbox = TRUE, digits = NA, null = "null"))
  expect_identical(f$id, substr(.sr_sha256(paste("missing_codes", "sbp", wj, sep = "|")), 1L, 12L))
  expect_match(f$id, "^[0-9a-f]{12}$")
  # a different filter is a different finding
  g <- .sr_dq_finding("missing_codes", "completeness", "sbp", "warn", 5L, 0.1, "m",
                      where = .sr_where_leaf("sbp", "==", -999))
  expect_false(identical(f$id, g$id))
})

# ---- registry --------------------------------------------------------------------------------------

test_that("the nine checks are registered in a fixed order", {
  expect_identical(sr_dq_checks(), c("coercion", "dict_type", "dict_levels", "dict_range", "missing_col",
                                     "missing_row", "missing_codes", "missing_pattern", "missing_dependence"))
})

test_that("an unknown check name is an error and registration validates its arguments", {
  p <- .dq_cardiac(n = 300L)
  expect_error(sr_dq_run(p, "cardiac", checks = c("coercion", "no_such_check")), "no_such_check")
  expect_error(.sr_dq_register("Bad Name", "structure", function(ctx) NULL), "snake_case")
  expect_error(.sr_dq_register("ok_name", "nonsense", function(ctx) NULL), "domain")
  expect_error(.sr_dq_register("ok_name", "structure", "not a function"), "function")
})

test_that("a check that throws becomes one check_error finding and the run continues", {
  p <- .dq_cardiac(n = 400L)
  .sr_dq_register("boom", "structure", function(ctx) stop("kaboom"))
  withr::defer(.sr_dq_unregister("boom"))
  .sr_dq_register("boom2", "structure", function(ctx) stop("kaboom two"))
  withr::defer(.sr_dq_unregister("boom2"))
  p <- sr_dq_run(p, "cardiac")
  f <- sr_dq_findings(p, "cardiac")
  ce <- .dq_find(f, "check_error")
  expect_identical(nrow(ce), 2L)
  expect_true(all(ce$severity == "info"))
  expect_match(ce$message[1L], "boom")
  expect_match(ce$message[1L], "kaboom")
  expect_identical(length(unique(ce$id)), 2L)     # two failing checks, two ids
  expect_gt(nrow(.dq_find(f, "missing_col")), 0L)  # the other checks still ran
  expect_true("boom" %in% p$state$dq$cardiac$checks)
})

test_that("a check that returns a malformed frame is reported as check_error", {
  p <- .dq_cardiac(n = 300L)
  .sr_dq_register("badout", "structure", function(ctx) data.frame(x = 1))
  withr::defer(.sr_dq_unregister("badout"))
  .sr_dq_register("baddomain", "structure", function(ctx) {
    .sr_dq_finding("baddomain", "nonsense", "*", "info", 0L, 0, "m")
  })
  withr::defer(.sr_dq_unregister("baddomain"))
  p <- sr_dq_run(p, "cardiac", checks = c("badout", "baddomain"))
  f <- sr_dq_findings(p, "cardiac")
  expect_identical(f$check, c("check_error", "check_error"))
  expect_match(f$message[1L], "badout")
  expect_match(f$message[2L], "domain")
})

test_that("named checks run only those, in registration order", {
  p <- .dq_cardiac(n = 300L)
  p <- sr_dq_run(p, "cardiac", checks = c("missing_col", "coercion"))
  expect_identical(unlist(p$state$dq$cardiac$checks), c("coercion", "missing_col"))
  f <- sr_dq_findings(p, "cardiac")
  expect_true(all(f$check %in% c("coercion", "missing_col")))
})

# ---- the planted defects ---------------------------------------------------------------------------

test_that("coercion flags the numeric column that holds 'pending'", {
  p <- sr_dq_run(.dq_cardiac(), "cardiac")
  f <- .dq_find(sr_dq_findings(p, "cardiac"), "coercion")
  expect_identical(f$column, "creatinine_umol")
  expect_identical(f$severity, "warn")
  expect_identical(f$domain, "conformance")
  expect_true("pending" %in% f$examples[[1L]])
  expect_identical(f$n_affected, 10L)                          # round(0.005 * 2000)
  expect_equal(f$p_affected, 10 / 2000)
  expect_identical(f$where[[1L]], list(col = "creatinine_umol", op = "in", value = "pending"))
})

test_that("dict_levels flags nyha 'V', dict_range flags the impossible values, dict_type is quiet", {
  p <- sr_dq_run(.dq_cardiac(), "cardiac")
  f <- sr_dq_findings(p, "cardiac")
  lv <- .dq_find(f, "dict_levels")
  expect_identical(lv$column, "nyha")
  expect_identical(lv$severity, "warn")
  expect_true("V" %in% lv$examples[[1L]])
  expect_identical(lv$n_affected, 20L)                         # round(0.01 * 2000)
  expect_identical(lv$where[[1L]]$all[[1L]], list(col = "nyha", op = "not_na"))
  expect_identical(lv$where[[1L]]$all[[2L]], list(col = "nyha", op = "not_in", value = c("I", "II", "III", "IV")))
  rg <- .dq_find(f, "dict_range")
  expect_true(all(c("age", "sbp", "glucose_mmol", "surgery_date", "creatinine_umol") %in% rg$column))
  expect_true(all(rg$severity == "warn"))
  age <- rg[rg$column == "age", ]
  expect_identical(age$n_affected, 3L)
  expect_true("250.0" %in% age$examples[[1L]] || "250" %in% age$examples[[1L]])
  expect_identical(age$where[[1L]], list(any = list(list(col = "age", op = "<", value = 18),
                                                  list(col = "age", op = ">", value = 110))))
  sbp <- rg[rg$column == "sbp", ]
  expect_identical(sbp$where[[1L]]$any[[1L]], list(col = "sbp", op = "<", value = 60))
  sd <- rg[rg$column == "surgery_date", ]
  expect_identical(sd$where[[1L]]$any[[2L]], list(col = "surgery_date", op = ">", value = "2025-12-31"))
  # the dictionary agrees with the store types on the planted table except where it is stored as text
  ty <- .dq_find(f, "dict_type")
  expect_identical(ty$column, "creatinine_umol")
  expect_identical(ty$severity, "warn")
})

test_that("missing_codes flags sbp at -99 with an equality filter", {
  p <- sr_dq_run(.dq_cardiac(), "cardiac")
  f <- .dq_find(sr_dq_findings(p, "cardiac"), "missing_codes")
  expect_identical(f$column, "sbp")
  expect_identical(f$severity, "warn")
  expect_identical(f$n_affected, 40L)                          # round(0.02 * 2000)
  expect_true("-99" %in% f$examples[[1L]])
  expect_identical(f$where[[1L]], list(col = "sbp", op = "==", value = -99))
  expect_identical(f$detail[[1L]]$codes$code, -99)
  expect_identical(f$detail[[1L]]$codes$n, 40)
})

test_that("finding ids are identical across two runs and the stored file round-trips exactly", {
  p <- .dq_cardiac()
  p1 <- sr_dq_run(p, "cardiac")
  f1 <- sr_dq_findings(p1, "cardiac")
  p2 <- sr_dq_run(p1, "cardiac")
  f2 <- sr_dq_findings(p2, "cardiac")
  expect_false(identical(p1$state$dq$cardiac$run_id, p2$state$dq$cardiac$run_id))
  expect_identical(f1$id, f2$id)
  expect_identical(f1, f2)
  expect_identical(anyDuplicated(f1$id), 0L)
  expect_true(all(nzchar(f1$message)))
  # a fresh project replayed from its events reads the same findings
  expect_identical(sr_dq_findings(p2, "cardiac"), f2)
})

test_that("the run is recorded: file, sha256, counts, run id", {
  p <- sr_dq_run(.dq_cardiac(), "cardiac")
  e <- p$state$dq$cardiac
  expect_match(e$run_id, "^[0-9]{8}T[0-9]{9}-[0-9a-f]{4}$")
  expect_identical(e$file, paste0("data/dq/cardiac-", e$run_id, ".json"))
  file <- file.path(p$paths$dir, e$file)
  expect_true(file.exists(file))
  expect_false(file.exists(paste0(file, ".tmp")))
  expect_identical(e$sha256, sr_sha256_file(file))
  f <- sr_dq_findings(p, "cardiac")
  expect_identical(e$n_findings, nrow(f))
  expect_identical(e$by_severity$info, sum(f$severity == "info"))
  expect_identical(e$by_severity$warn, sum(f$severity == "warn"))
  expect_identical(e$by_severity$block, sum(f$severity == "block"))
  expect_identical(unlist(e$checks), sr_dq_checks())
  expect_identical(e$store_hash, sr_profile_table(p, "cardiac")$store_hash)
  # the event itself carries the same payload
  ev <- Filter(function(x) x$type == "dq.run", sr_events_read(p$paths$events))
  expect_length(ev, 1L)
  expect_identical(ev[[1L]]$payload$sha256, e$sha256)
})

test_that("an altered findings file or a missing run is an error", {
  p <- .dq_cardiac(n = 300L)
  expect_error(sr_dq_findings(p, "cardiac"), "no DQ run")
  p <- sr_dq_run(p, "cardiac")
  file <- file.path(p$paths$dir, p$state$dq$cardiac$file)
  writeLines("{}", file)
  expect_error(sr_dq_findings(p, "cardiac"), "SHA-256")
  unlink(file)
  expect_error(sr_dq_findings(p, "cardiac"), "missing")
})

test_that("dq.run keeps each table's latest run and never replaces another table", {
  p <- .dq_cardiac(n = 300L)
  p <- .dq_ingest(p, data.frame(a = 1:50, b = rep(c("x", "y"), 25), stringsAsFactors = FALSE), "other")
  p <- sr_dq_run(p, "cardiac", checks = "missing_col")
  p <- sr_dq_run(p, "other", checks = "missing_col")
  expect_setequal(names(p$state$dq), c("cardiac", "other"))
  r1 <- p$state$dq$cardiac$run_id
  p <- sr_dq_run(p, "other", checks = "missing_col")
  expect_identical(p$state$dq$cardiac$run_id, r1)
  expect_identical(p$state$dq$other$table, "other")
})

# ---- the clean twin --------------------------------------------------------------------------------

test_that("on the clean twin no conformance or code check fires, and nothing warns except structural missingness", {
  p <- sr_dq_run(.dq_cardiac(defects = FALSE), "cardiac")
  f <- sr_dq_findings(p, "cardiac")
  for (ch in c("coercion", "dict_type", "dict_levels", "dict_range", "missing_codes", "missing_row")) {
    expect_identical(nrow(.dq_find(f, ch)), 0L, info = ch)
  }
  expect_identical(nrow(f[f$check == "check_error", ]), 0L)
  hi <- f[f$severity != "info", ]
  # death_date and statin_start_date are missing by design (no death, no statin) and their missingness
  # follows follow-up and statin exposure: the only thing a DQ run may warn about here
  expect_setequal(unique(hi$check), c("missing_col", "missing_dependence"))
  expect_setequal(unique(hi$column), c("death_date", "statin_start_date"))
  # ordinary missingness (hba1c ~3%) is below every threshold
  expect_identical(nrow(f[f$column == "hba1c_pct", ]), 0L)
})

# ---- missing_col -----------------------------------------------------------------------------------

test_that("missing_col thresholds", {
  n <- 400L
  k <- seq_len(n)
  df <- data.frame(id = k, below = ifelse(k <= 19, NA, 1), info = ifelse(k <= 20, NA, 1),
                   warn = ifelse(k <= 120, NA, 1), blockme = ifelse(k <= 120, NA, 1),
                   protected_ok = ifelse(k <= 40, NA, 1))
  p <- .dq_small(df)
  p <- sr_roles_set(p, "small", c(blockme = "exposure", protected_ok = "outcome"))
  p <- sr_dq_run(p, "small", checks = "missing_col")
  f <- sr_dq_findings(p, "small")
  sev <- stats::setNames(f$severity, f$column)
  expect_false("below" %in% f$column)
  expect_identical(sev[["info"]], "info")
  expect_identical(sev[["warn"]], "warn")
  expect_identical(sev[["blockme"]], "block")
  expect_identical(sev[["protected_ok"]], "info")      # 10% on a protected role stays info
  w <- f$where[[match("warn", f$column)]]
  expect_identical(w, list(col = "warn", op = "is_na"))
  expect_identical(f$n_affected[f$column == "warn"], 120L)
  expect_equal(f$p_affected[f$column == "warn"], 0.3)
})

# ---- missing_row -----------------------------------------------------------------------------------

test_that("missing_row counts rows with more than half their fields missing; info under 1%, else warn", {
  mk <- function(n, bad) {
    df <- as.data.frame(matrix(1, nrow = n, ncol = 10, dimnames = list(NULL, paste0("v", 1:10))))
    df[seq_len(bad), 1:6] <- NA
    df[bad + seq_len(5L), 1:5] <- NA     # exactly half: not more than half
    df
  }
  p <- sr_dq_run(.dq_small(mk(400L, 2L), "few"), "few", checks = "missing_row")
  f <- sr_dq_findings(p, "few")
  expect_identical(f$n_affected, 2L)
  expect_identical(f$severity, "info")     # 0.5% < 1%
  expect_null(f$where[[1L]])
  p2 <- sr_dq_run(.dq_small(mk(400L, 8L), "many"), "many", checks = "missing_row")
  f2 <- sr_dq_findings(p2, "many")
  expect_identical(f2$n_affected, 8L)
  expect_identical(f2$severity, "warn")    # 2%
  p3 <- sr_dq_run(.dq_small(mk(400L, 0L), "none"), "none", checks = "missing_row")
  expect_identical(nrow(sr_dq_findings(p3, "none")), 0L)
})

# ---- missing_codes ---------------------------------------------------------------------------------

test_that("missing_codes needs 0.5% of the values AND a value outside the robust range", {
  set.seed(2)
  n <- 2000L
  x <- round(stats::rnorm(n, 60, 10))
  inside <- round(stats::rnorm(n, 0, 3))     # -1 is an ordinary value here
  inside[sample.int(n, 40L)] <- -1
  rare <- round(stats::rnorm(n, 60, 10))
  rare[sample.int(n, 5L)] <- 999              # 0.25%
  two <- round(stats::rnorm(n, 60, 10))
  two[1:30] <- 999
  two[31:50] <- -999
  binary <- sample(0:1, n, replace = TRUE)
  binary[1:20] <- -9                          # MAD == 0: any other value is outside
  x[sample.int(n, 60L)] <- -99
  df <- data.frame(x = x, inside = inside, rare = rare, two = two, binary = binary)
  p <- sr_dq_run(.dq_small(df), "small", checks = "missing_codes")
  f <- sr_dq_findings(p, "small")
  expect_setequal(f$column, c("x", "two", "binary"))
  expect_identical(f$where[f$column == "x"][[1L]], list(col = "x", op = "==", value = -99))
  tw <- f[f$column == "two", ]
  expect_identical(tw$where[[1L]], list(col = "two", op = "in", value = c(999, -999)))
  expect_identical(tw$n_affected, 50L)
  expect_identical(tw$detail[[1L]]$codes$n, c(30, 20))
  expect_identical(f$where[f$column == "binary"][[1L]], list(col = "binary", op = "==", value = -9))
  expect_true(all(f$severity == "warn" & f$domain == "completeness"))
})

test_that("a dictionary missing code is a candidate; text sentinels need 0.1% of the values", {
  n <- 2000L
  score <- round(stats::rnorm(n, 50, 5))
  score[1:30] <- 888
  txt <- rep(c("alpha", "beta", "gamma"), length.out = n)
  txt[1:6] <- c("N/A", "n/a", "?", "unknown", "", "-")        # "" is read back as missing
  one <- rep(c("alpha", "beta"), length.out = n)
  one[1] <- "NA"                                              # 1 of 2000 = 0.05% < 0.1%
  df <- data.frame(score = score, txt = txt, one = one, stringsAsFactors = FALSE)
  p <- .dq_small(df)
  d <- .dq_dict(c("score", "txt", "one"), type = c("numeric", "text", "text"),
                missing_codes = list("888", character(), character()))
  p <- sr_dict_set(p, "small", d, "manual")
  p <- sr_dq_run(p, "small", checks = "missing_codes")
  f <- sr_dq_findings(p, "small")
  expect_setequal(f$column, c("score", "txt"))
  expect_identical(f$where[f$column == "score"][[1L]], list(col = "score", op = "==", value = 888))
  tx <- f[f$column == "txt", ]
  expect_true(all(c("N/A", "n/a", "?", "unknown", "-") %in% tx$detail[[1L]]$codes$code))
  expect_identical(tx$where[[1L]]$op, "in")
  expect_identical(tx$severity, "warn")
})

# ---- missing_pattern and missing_dependence --------------------------------------------------------

test_that("missing_pattern lists the commonest joint patterns of the sample as one info finding", {
  p <- sr_dq_run(.dq_cardiac(), "cardiac", checks = "missing_pattern")
  f <- sr_dq_findings(p, "cardiac")
  expect_identical(nrow(f), 1L)
  expect_identical(f$severity, "info")
  expect_identical(f$column, "*")
  pt <- f$detail[[1L]]$patterns
  expect_lte(nrow(pt), 10L)
  expect_identical(pt$rank, seq_len(nrow(pt)))
  expect_true(all(diff(pt$n) <= 0))
  expect_true(all(pt$column == "*"))
  expect_true(any(grepl("death_date", pt$missing_columns)))
  # a complete table has no pattern to report
  p2 <- sr_dq_run(.dq_small(data.frame(a = 1:100, b = 100:1)), "small", checks = "missing_pattern")
  expect_identical(nrow(sr_dq_findings(p2, "small")), 0L)
})

test_that("missing_dependence reports the largest SMD at >= 0.2 (info) and >= 0.5 (warn)", {
  set.seed(3)
  n <- 2000L
  x <- stats::rnorm(n)
  noise <- stats::rnorm(n)
  gr <- sample(c("a", "b"), n, replace = TRUE)
  mk <- function(shift) {
    miss <- stats::runif(n) < stats::plogis(-2.2 + shift * x)
    y <- stats::rnorm(n)
    y[miss] <- NA
    data.frame(x = x, noise = noise, gr = gr, y = y, stringsAsFactors = FALSE)
  }
  strong <- sr_dq_run(.dq_small(mk(2), "strong"), "strong", checks = "missing_dependence")
  fs <- sr_dq_findings(strong, "strong")
  expect_identical(fs$column, "y")
  expect_identical(fs$severity, "warn")
  expect_identical(fs$detail[[1L]]$dependence$covariate[1L], "x")
  expect_gte(fs$detail[[1L]]$dependence$smd[1L], 0.5)
  none <- sr_dq_run(.dq_small(mk(0), "none"), "none", checks = "missing_dependence")
  expect_identical(nrow(sr_dq_findings(none, "none")), 0L)
})

test_that("missing_dependence on the cardiac data finds follow-up behind death_date and statin behind its start date", {
  p <- sr_dq_run(.dq_cardiac(defects = FALSE), "cardiac", checks = "missing_dependence")
  f <- sr_dq_findings(p, "cardiac")
  expect_setequal(f$column, c("death_date", "statin_start_date"))
  expect_true(all(f$severity == "warn"))
  expect_identical(f$detail[[which(f$column == "statin_start_date")]]$dependence$covariate[1L], "statin")
})

# ---- the outcome-blind rule ------------------------------------------------------------------------

test_that("the findings of a blind run pass the outcome-blind gate", {
  p <- .dq_cardiac()
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure", patient_id = "id",
                                    death_date = "event_date", statin_start_date = "covariate"))
  expect_true(sr_blind(p))
  p <- sr_dq_run(p, "cardiac")
  f <- sr_dq_findings(p, "cardiac")
  expect_silent(.sr_assert_blind(f, "mortality_30d", other_cols = "statin"))
  expect_silent(.sr_assert_blind(f, "mortality_30d", other_cols = setdiff(names(sr_roles(p, "cardiac")), "mortality_30d")))
  expect_identical(nrow(f[f$check == "check_error", ]), 0L)
  # the outcome gets its marginals only: nothing about it in a pattern, a dependence or a code finding
  expect_false(any(grepl("mortality_30d", f$message[f$check %in% c("missing_pattern", "missing_dependence")])))
  pt <- f$detail[[match("missing_pattern", f$check)]]$patterns
  expect_false(any(grepl("mortality_30d", pt$missing_columns)))
})

test_that("a check whose output would cross the outcome becomes check_error while blind", {
  p <- .dq_cardiac(n = 300L)
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure"))
  .sr_dq_register("leaky", "leakage", function(ctx) {
    .sr_dq_finding("leaky", "leakage", "*", "info", 0L, 0, "m",
                   detail = list(by_outcome = data.frame(column = "*", x = 1)))
  })
  withr::defer(.sr_dq_unregister("leaky"))
  p <- sr_dq_run(p, "cardiac", checks = "leaky")
  f <- sr_dq_findings(p, "cardiac")
  expect_identical(f$check, "check_error")
  expect_match(f$message, "outcome-blind gate")
})

test_that("missing_dependence for an outcome with >= 5% missing uses only is.na(outcome), never its values", {
  set.seed(4)
  n <- 1200L
  x <- stats::rnorm(n)
  z <- stats::rnorm(n)
  miss <- stats::runif(n) < stats::plogis(-1.8 + 2 * x)          # missingness of the outcome follows x
  mk <- function(vals) {
    y <- vals
    y[miss] <- NA
    data.frame(x = x, z = z, y = y)
  }
  y_a <- stats::rbinom(n, 1L, 0.3)
  y_b <- stats::rbinom(n, 1L, stats::plogis(2 * z + x))           # other values, correlated with z and x
  run <- function(df, table) {
    p <- .dq_small(df, table)
    p <- sr_roles_set(p, table, c(y = "outcome", x = "covariate", z = "covariate"))
    p <- sr_dq_run(p, table, checks = "missing_dependence")
    sr_dq_findings(p, table)
  }
  fa <- run(mk(y_a), "ya")
  fb <- run(mk(y_b), "yb")
  expect_identical(fa$column, "y")
  expect_identical(fa$severity, "warn")
  expect_identical(fa$detail[[1L]]$dependence$covariate[1L], "x")
  expect_false("y" %in% fa$detail[[1L]]$dependence$covariate)
  # different outcome VALUES, same missingness: identical findings
  expect_identical(fa, fb)
  expect_silent(.sr_assert_blind(fa, "y", other_cols = c("x", "z")))
  # and an outcome whose values depend on x but whose missingness is random shows nothing
  rnd <- stats::runif(n) < 0.15
  y_c <- stats::rbinom(n, 1L, stats::plogis(3 * x))
  y_c[rnd] <- NA
  fc <- run(data.frame(x = x, z = z, y = y_c), "yc")
  expect_identical(nrow(fc), 0L)
})
