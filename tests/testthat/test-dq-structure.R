# Tests for profile/dq_structure.R and profile/dq_leakage.R: the uniqueness checks (dup_rows, dup_id), the
# structure checks (nzv, high_card, collinear, cluster_detect, offset_detect) and the outcome-blind
# leakage checks (leakage, separation, post_outcome_date). Synthetic data only; everything lives under
# withr::local_tempdir().

.st_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

.st_checks <- c("dup_rows", "dup_id", "nzv", "high_card", "collinear", "cluster_detect", "offset_detect",
                "leakage", "separation", "post_outcome_date")

# The roles of the cardiac study with the 30-day mortality as the outcome.
.st_roles <- c(patient_id = "id", surgery_date = "time_zero", statin_start_date = "exposure_date",
               statin = "exposure", death_date = "event_date", mortality_30d = "outcome")

.st_cardiac <- function(defects = TRUE, n = 2000L, roles = .st_roles, env = parent.frame()) {
  p <- .fx_cardiac_project(n = n, defects = defects, env = env)
  withr::defer(sr_duck_close(p), envir = env)
  if (length(roles)) p <- sr_roles_set(p, "cardiac", roles)
  p
}

.st_small <- function(df, table = "small", roles = NULL, env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = table, options = list(drive_info = .st_drive))
  if (length(roles)) p <- sr_roles_set(p, table, roles)
  p
}

# Run the named checks on `table` and read the findings back.
.st_run <- function(p, table, checks = .st_checks) {
  sr_dq_findings(sr_dq_run(p, table, checks = checks), table)
}

# The findings of one check (and optionally of some columns) from a run of just that check.
.st_one <- function(p, table, check, column = NULL) {
  f <- .st_run(p, table, check)
  f[f$check == check & (is.null(column) | f$column %in% column), , drop = FALSE]
}

# A project whose SAP is locked, through a throwaway sap.locked reducer that is removed on exit.
.st_unblind <- function(p, env = parent.frame()) {
  expect_false(exists("sap.locked", envir = .sr_reducers, inherits = FALSE))   # P1d owns the real one
  sr_reducer_register("sap.locked", function(state, event) {
    state$sap$locked <- TRUE
    state
  })
  withr::defer(rm(list = "sap.locked", envir = .sr_reducers), envir = env)
  sr_project_append(p, "sap.locked", list())
}

# ---- registration ----------------------------------------------------------------------------------

test_that("the ten checks are registered after the plausibility checks, in a fixed order", {
  reg <- sr_dq_checks()
  expect_identical(reg[reg %in% .st_checks], .st_checks)          # ordered containment
  expect_gt(min(match(.st_checks, reg)), match("immortal_time", reg))
  dom <- vapply(.st_checks, function(ch) get(ch, envir = .sr_dq_checks, inherits = FALSE)$domain, "")
  expect_identical(unname(dom), c("uniqueness", "uniqueness", rep("structure", 5L), rep("leakage", 3L)))
})

# ---- uniqueness ------------------------------------------------------------------------------------

test_that("dup_rows counts the extra copies of exact duplicate rows (5 planted)", {
  p <- .st_cardiac()
  f <- .st_one(p, "cardiac", "dup_rows")
  expect_identical(nrow(f), 1L)
  expect_identical(f$domain, "uniqueness")
  expect_identical(f$severity, "warn")
  expect_identical(f$column, "*")
  expect_identical(f$n_affected, 5L)                              # 5 surplus rows over 5 duplicated rows
  expect_equal(f$p_affected, 5 / 2000)
  expect_null(f$where[[1L]])
  expect_equal(f$detail[[1L]]$duplicates$groups, 5)
  expect_equal(f$detail[[1L]]$duplicates$extra_rows, 5)
  expect_identical(f$detail[[1L]]$duplicates$column, "*")
})

test_that("dup_id counts surplus rows of an id that repeats on rows that are not exact duplicates (4 planted)", {
  p <- .st_cardiac()
  f <- .st_one(p, "cardiac", "dup_id")
  expect_identical(nrow(f), 1L)
  expect_identical(f$domain, "uniqueness")
  expect_identical(f$severity, "block")
  expect_identical(f$column, "patient_id")
  expect_identical(f$n_affected, 4L)                              # the 5 exact duplicates are not counted
  expect_equal(f$p_affected, 4 / 2000)
  src <- sr_sample_cardiac(n = 2000L)$data
  rep_ids <- unique(src$patient_id[duplicated(src$patient_id) & !duplicated(src)])
  expect_length(rep_ids, 4L)
  expect_setequal(f$where[[1L]]$value, rep_ids)
  expect_identical(f$where[[1L]]$op, "in")
  expect_equal(f$detail[[1L]]$ids$n_ids, 4)
})

test_that("exact duplicates alone are dup_rows, not dup_id; no id role means no dup_id", {
  df <- data.frame(id = c(1:30, 5L, 6L), x = c(as.numeric(1:30), 5, 6))
  p <- .st_small(df, roles = c(id = "id"))
  f <- .st_run(p, "small", c("dup_rows", "dup_id"))
  expect_identical(f$n_affected[f$check == "dup_rows"], 2L)
  expect_false("dup_id" %in% f$check)
  p2 <- .st_small(data.frame(id = c(1:10, 1L), x = 1:11), table = "t2")
  expect_identical(nrow(.st_one(p2, "t2", "dup_id")), 0L)
  p3 <- .st_small(data.frame(id = c(1:10, 1L), x = 1:11), table = "t3", roles = c(id = "id"))
  f3 <- .st_one(p3, "t3", "dup_id")
  expect_identical(f3$n_affected, 1L)
  expect_identical(f3$where[[1L]], list(col = "id", op = "in", value = 1))      # numeric id: numeric filter
})

# ---- structure -------------------------------------------------------------------------------------

test_that("nzv flags const_flag (warn: not a protected role) and nothing ordinary", {
  p <- .st_cardiac()
  f <- .st_one(p, "cardiac", "nzv")
  expect_true("const_flag" %in% f$column)
  cf <- f[f$column == "const_flag", ]
  expect_identical(cf$severity, "warn")
  expect_identical(cf$domain, "structure")
  expect_identical(cf$n_affected, 2000L)
  expect_false(any(c("sex", "site", "age", "patient_id", "notes", "diabetes") %in% f$column))
})

test_that("nzv is info for a protected role and applies the caret rule (ratio > 19, < 10% unique)", {
  set.seed(7)
  df <- data.frame(a = c(rep("y", 190), rep("n", 10)),            # ratio 19: not > 19
                   b = c(rep("y", 191), rep("n", 9)),             # ratio 21.2: flagged
                   c = c(rep(0L, 197), 1L, 2L, 3L),               # ratio 197
                   d = rep(5, 200),                               # constant
                   e = stats::rnorm(200))
  p <- .st_small(df, roles = c(b = "exposure"))
  f <- .st_one(p, "small", "nzv")
  expect_setequal(f$column, c("b", "c", "d"))
  expect_identical(f$severity[f$column == "b"], "info")           # protected
  expect_identical(f$severity[f$column == "c"], "warn")
  expect_identical(f$severity[f$column == "d"], "warn")
  expect_match(f$message[f$column == "d"], "constant")
})

test_that("high_card flags the free-text notes column, not ids or low-cardinality text", {
  p <- .st_cardiac()
  f <- .st_one(p, "cardiac", "high_card")
  expect_identical(f$column, "notes")
  expect_identical(f$severity, "info")
  expect_identical(f$domain, "structure")
  expect_identical(f$n_affected, 2000L)
  expect_null(f$where[[1L]])
  # an unlabelled id column is flagged; the id role exempts it
  df <- data.frame(pid = sprintf("P%04d", 1:300), grp = rep(c("a", "b", "c"), 100), stringsAsFactors = FALSE)
  expect_identical(.st_one(.st_small(df), "small", "high_card")$column, "pid")
  expect_identical(nrow(.st_one(.st_small(df, table = "t2", roles = c(pid = "id")), "t2", "high_card")), 0L)
})

test_that("collinear flags bmi / bmi_copy on the sample and keeps the outcome out while blind", {
  p <- .st_cardiac()
  f <- .st_one(p, "cardiac", "collinear")
  pair <- f[f$column == "bmi_copy", ]
  expect_identical(nrow(pair), 1L)
  expect_identical(pair$severity, "warn")
  expect_identical(pair$domain, "structure")
  expect_identical(pair$detail[[1L]]$pair$other, "bmi")
  expect_gte(pair$detail[[1L]]$pair$r, 0.98)
  expect_false(any(f$column == "mortality_30d" | vapply(f$detail, function(d) "mortality_30d" %in% d$pair$other, NA)))
  # the id is the same whatever the run
  expect_identical(.st_one(p, "cardiac", "collinear")$id, f$id)
  # a column equal to the outcome is collinear with it only once the outcome may be looked at
  y <- rep(0:1, 100)
  df <- data.frame(y = y, copy = y * 2 + 1, x = sin(seq_len(200)))
  p2 <- .st_small(df, roles = c(y = "outcome"))
  expect_identical(nrow(.st_one(p2, "small", "collinear")), 0L)
  p3 <- .st_unblind(p2)
  f3 <- .st_one(p3, "small", "collinear")
  expect_identical(nrow(f3), 1L)
  expect_identical(f3$column, "copy")
})

test_that("cluster_detect suggests site as the cluster role", {
  p <- .st_cardiac()
  f <- .st_one(p, "cardiac", "cluster_detect")
  expect_identical(f$column, "site")
  expect_identical(f$severity, "info")
  expect_identical(f$domain, "structure")
  expect_match(f$message, "cluster")
  # a confirmed cluster role silences the suggestion
  p2 <- .st_cardiac(roles = c(.st_roles, site = "cluster"))
  expect_identical(nrow(.st_one(p2, "cardiac", "cluster_detect")), 0L)
})

test_that("cluster_detect needs 2 to 200 levels with at least 5 rows each, or a repeating id", {
  df <- data.frame(site = rep(c("a", "b"), c(5, 4)), x = 1:9)
  expect_identical(nrow(.st_one(.st_small(df), "small", "cluster_detect")), 0L)          # level b has 4 rows
  df2 <- data.frame(site = rep(c("a", "b"), c(5, 5)), x = 1:10)
  expect_identical(.st_one(.st_small(df2, table = "t2"), "t2", "cluster_detect")$column, "site")
  # a repeating id (repeated measures): 100 patients with 3 visits each
  df3 <- data.frame(pid = rep(1:100, each = 3), v = rep(1:3, 100))
  f3 <- .st_one(.st_small(df3, table = "t3", roles = c(pid = "id")), "t3", "cluster_detect")
  expect_identical(f3$column, "pid")
  expect_match(f3$message, "repeats")
  # a handful of duplicated ids (dup_id's business) is not repeated measures
  df4 <- data.frame(pid = c(1:200, 1:2), v = 1:202)
  expect_identical(nrow(.st_one(.st_small(df4, table = "t4", roles = c(pid = "id")), "t4", "cluster_detect")), 0L)
})

test_that("offset_detect suggests a positive follow-up column as the offset role", {
  p <- .st_cardiac()
  f <- .st_one(p, "cardiac", "offset_detect")
  expect_identical(f$column, "followup_days")
  expect_identical(f$severity, "info")
  expect_identical(f$domain, "structure")
  # a zero in the column, or an offset role already set, silences it
  df <- data.frame(person_years = c(0, 1.5, 2, 3), x = 1:4)
  expect_identical(nrow(.st_one(.st_small(df), "small", "offset_detect")), 0L)
  df2 <- data.frame(person_years = c(0.5, 1.5, 2, 3), x = 1:4)
  expect_identical(nrow(.st_one(.st_small(df2, table = "t2", roles = c(person_years = "offset")), "t2",
                                "offset_detect")), 0L)
  expect_identical(.st_one(.st_small(df2, table = "t3"), "t3", "offset_detect")$column, "person_years")
})

# ---- leakage ---------------------------------------------------------------------------------------

test_that("the AUC helper is the rank-based Mann-Whitney statistic", {
  y <- c(0L, 0L, 0L, 1L, 1L, 1L)
  expect_identical(.sr_dq_auc(c(1, 2, 3, 4, 5, 6), y, min_n = 3L), 1)
  expect_identical(.sr_dq_auc(c(6, 5, 4, 3, 2, 1), y, min_n = 3L), 0)
  expect_identical(.sr_dq_auc(rep(1, 6), y, min_n = 3L), NA_real_)                       # no ordering
  expect_identical(.sr_dq_auc(1:6, c(0L, 0L, 0L, 0L, 0L, 1L), min_n = 3L), NA_real_)     # too few events
  # ties count half: events 2, 3, 4 against non-events 1, 2, 3 give 1.5 + 2.5 + 3 = 7 of 9 pairs
  x <- c(1, 2, 2, 3, 3, 4)
  y2 <- c(0L, 0L, 1L, 0L, 1L, 1L)
  expect_equal(.sr_dq_auc(x, y2, min_n = 3L), 7 / 9)
})

test_that("leakage flags lab_status while blind, with no AUC, no counts and an empty detail", {
  p <- .st_cardiac()
  expect_true(sr_blind(p))
  f <- .st_run(p, "cardiac", c("leakage", "separation"))
  lk <- f[f$check == "leakage", ]
  expect_identical(lk$column, "lab_status")
  expect_identical(lk$severity, "block")
  expect_identical(lk$domain, "leakage")
  expect_match(lk$message, "is near-deterministic for the outcome", fixed = TRUE)
  expect_true(is.na(lk$n_affected) && is.na(lk$p_affected))
  expect_identical(lk$examples[[1L]], character())
  expect_identical(lk$detail[[1L]], list())
  expect_null(lk$where[[1L]])
  expect_no_match(lk$message, "[0-9]")
  expect_no_error(.sr_assert_blind(lk, "mortality_30d", setdiff(names(.st_roles), "mortality_30d")))
  expect_no_error(.sr_assert_blind(f, "mortality_30d", setdiff(names(sr_sample_cardiac(n = 2000L)$data), "mortality_30d")))
})

test_that("separation names the column only while blind", {
  p <- .st_cardiac()
  sp <- .st_one(p, "cardiac", "separation")
  expect_setequal(sp$column, c("lab_status", "nyha"))              # 'deceased' and the planted level 'V'
  expect_true(all(sp$severity == "warn"))
  expect_true(all(sp$domain == "leakage"))
  expect_true(all(is.na(sp$n_affected)) && all(is.na(sp$p_affected)))
  expect_true(all(lengths(sp$examples) == 0L))
  expect_true(all(lengths(sp$detail) == 0L))
  expect_true(all(vapply(sp$where, is.null, NA)))
  expect_no_match(sp$message, "deceased|\\bV\\b|[0-9]")
})

test_that("unblinded, leakage carries the AUC and separation lists the levels", {
  p <- .st_unblind(.st_cardiac())
  expect_false(sr_blind(p))
  f <- .st_run(p, "cardiac", c("leakage", "separation"))
  lk <- f[f$check == "leakage" & f$column == "lab_status", ]
  expect_identical(nrow(lk), 1L)
  expect_identical(lk$severity, "block")
  expect_gte(lk$detail[[1L]]$auc$auc, 0.99)
  expect_identical(lk$detail[[1L]]$auc$column, "lab_status")
  expect_false(is.na(lk$n_affected))
  expect_match(lk$message, "is near-deterministic for the outcome", fixed = TRUE)
  sp <- f[f$check == "separation", ]
  expect_setequal(sp$column, c("lab_status", "nyha"))
  lab <- sp[sp$column == "lab_status", ]
  expect_true("deceased" %in% lab$detail[[1L]]$levels$level)
  lv <- lab$detail[[1L]]$levels                                    # every level is pure here: the survivors read 0
  expect_identical(lv$outcome[lv$level == "deceased"], "1")
  expect_true(all(lv$outcome[lv$level != "deceased"] == "0"))
  expect_true("V" %in% sp$detail[[which(sp$column == "nyha")]]$levels$level)
  expect_false(is.na(lab$n_affected))
})

test_that("leakage candidates exclude the id, event and time roles, and need a binary outcome", {
  y <- rep(0:1, 150)
  df <- data.frame(y = y, pid = 1:300, ev = y, tt = y * 10 + 1, noise = sin(seq_len(300)), direct = y * 3 + 2)
  roles <- c(y = "outcome", pid = "id", ev = "event", tt = "time")
  f <- .st_one(.st_small(df, roles = roles), "small", "leakage")
  expect_identical(f$column, "direct")
  # no outcome role, or an outcome that is not binary: no leakage / separation findings
  expect_identical(nrow(.st_run(.st_small(df, table = "t2"), "t2", c("leakage", "separation"))), 0L)
  df$y3 <- rep(0:2, 100)
  f3 <- .st_run(.st_small(df, table = "t3", roles = c(y3 = "outcome")), "t3", c("leakage", "separation"))
  expect_identical(nrow(f3), 0L)
})

test_that("post_outcome_date flags a date mostly after the event date: a flag while blind, counted unblinded", {
  n <- 100
  ev <- as.Date("2020-01-01") + seq_len(n)
  df <- data.frame(ev_date = ev, before = ev - 5, after = ev + 5, y = rep(0:1, 50))
  df$after[1:20] <- NA
  roles <- c(ev_date = "event_date", y = "outcome")
  ps <- .st_small(df, roles = roles)
  fb <- .st_one(ps, "small", "post_outcome_date")
  expect_identical(fb$column, "after")
  expect_true(is.na(fb$n_affected) && is.na(fb$p_affected))
  expect_match(fb$message, "withheld until the SAP is locked", fixed = TRUE)
  expect_false(grepl("ev_date", fb$message, fixed = TRUE))
  f <- .st_one(.st_unblind(ps), "small", "post_outcome_date")
  expect_identical(f$column, "after")
  expect_identical(f$severity, "warn")
  expect_identical(f$domain, "leakage")
  expect_identical(f$n_affected, 80L)
  expect_null(f$where[[1L]])
  p <- .st_cardiac()
  expect_identical(nrow(.st_one(p, "cardiac", "post_outcome_date")), 0L)
})

# ---- the whole run ---------------------------------------------------------------------------------

test_that("a blind run of the ten checks on the planted cohort has no check_error and passes the gate", {
  p <- .st_cardiac()
  f <- .st_run(p, "cardiac")
  expect_false("check_error" %in% f$check)
  expect_setequal(unique(f$check), c("dup_rows", "dup_id", "nzv", "high_card", "collinear", "cluster_detect",
                                     "offset_detect", "leakage", "separation"))
  expect_true(all(is.na(f$n_affected[f$check %in% c("leakage", "separation")])))
  expect_identical(f$id[f$check == "leakage"], .st_run(p, "cardiac")$id[f$check == "leakage"])
})

test_that("the clean twin has no warn-or-above finding and no nzv / high_card at n = 2000 and 50000", {
  for (n in c(2000L, 50000L)) {
    p <- .st_cardiac(defects = FALSE, n = n)
    f <- .st_run(p, "cardiac")
    expect_false("check_error" %in% f$check, label = paste("n =", n))
    expect_identical(f$check[f$severity != "info"], character(), label = paste("n =", n))
    # the generator keeps const_flag 60/40 and notes to 8 phrases in the clean twin, so neither fires
    expect_identical(sum(f$check %in% c("nzv", "high_card")), 0L, label = paste("n =", n))
  }
})

# ---- fix round 1 ------------------------------------------------------------------------------------

test_that("blind leakage and separation never test the exposure roles; unblinded they do", {
  y <- rep(0:1, 100)
  df <- data.frame(y = y, trt = ifelse(y == 1, "Y", "N"), trt_copy = ifelse(y == 1, "Y", "N"),
                   start = as.Date("2020-01-01") + y * 100 + rep(0:49, 4), noise = sin(seq_len(200)))
  roles <- c(y = "outcome", trt = "exposure", start = "exposure_date")
  p <- .st_small(df, roles = roles)
  expect_true(sr_blind(p))
  f <- .st_run(p, "small", c("leakage", "separation"))
  expect_true(all(c("leakage", "separation") %in% f$check[f$column == "trt_copy"]))   # an ordinary column is tested
  expect_false(any(f$column %in% c("trt", "start")))                                 # the exposure roles are not
  p2 <- .st_unblind(p)
  f2 <- .st_run(p2, "small", c("leakage", "separation"))
  expect_true(all(c("leakage", "separation") %in% f2$check[f2$column == "trt"]))
  expect_true("leakage" %in% f2$check[f2$column == "start"])
})

test_that("a separation finding keeps its id across the SAP lock", {
  p <- .st_cardiac()
  blind <- .st_one(p, "cardiac", "separation")
  p2 <- .st_unblind(p)
  open <- .st_one(p2, "cardiac", "separation")
  expect_false(is.null(open$where[[which(open$column == "lab_status")]]))             # unblinded carries a where
  expect_identical(open$id[order(open$column)], blind$id[order(blind$column)])
})

test_that("collinear needs at least 30 complete pairs", {
  a <- c(seq_len(100) + 0.5, rep(NA, 100))
  b <- c(rep(NA, 97), a[98:100], rep(NA, 100))                                         # 3 rows in common, r = 1
  b[98:100] <- a[98:100] * 2
  g <- c(rep(NA, 100), seq_len(100) + 0.25)
  h <- c(rep(NA, 100), (seq_len(100) + 0.25) * 3)                                      # 100 rows in common
  df <- data.frame(a = a, b = b, g = g, h = h)
  f <- .st_one(.st_small(df), "small", "collinear")
  expect_identical(f$column, "h")
  expect_identical(f$detail[[1L]]$pair$other, "g")
})

test_that("the throwaway sap.locked reducer is gone after the unblinding tests", {
  expect_false(exists("sap.locked", envir = .sr_reducers, inherits = FALSE))
})
