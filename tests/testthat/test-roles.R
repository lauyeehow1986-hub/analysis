# Tests for intake/roles.R: variable roles (roles.set), rule-based suggestions, and the outcome-blind
# gate (sr_blind, .sr_assert_blind). Synthetic data only; everything lives under withr::local_tempdir().

.rl_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

.rl_cardiac <- function(env = parent.frame(), n = 300L) {
  p <- .fx_cardiac_project(n = n, env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

# Ingest `df` as `table` into an open project.
.rl_ingest <- function(p, df, table) {
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  sr_ingest(p, csv, table = table, options = list(drive_info = .rl_drive))
}

# A tiny table whose column names say nothing; text column `loc` with `k` distinct values.
.rl_small <- function(p, table = "small", k = 6L) {
  df <- data.frame(loc = rep(LETTERS[seq_len(k)], length.out = 60L), x = seq_len(60L) / 3,
                   stringsAsFactors = FALSE)
  .rl_ingest(p, df, table)
}

.rl_dict <- function(variable, label, type = "text") {
  n <- length(variable)
  d <- data.frame(variable = variable, label = label, type = type, units = "", min = "", max = "",
                  stringsAsFactors = FALSE)
  d$levels <- rep(list(data.frame(code = character(), label = character(), stringsAsFactors = FALSE)), n)
  d$missing_codes <- rep(list(character()), n)
  d$source <- "test"
  d
}

.rl_role_of <- function(s, col) s$role[s$column == col]

# ---- constants -------------------------------------------------------------------------------------

test_that("the role vocabularies are exactly as specified", {
  expect_identical(SR_ROLES, c("outcome", "exposure", "id", "cluster", "time_zero", "exposure_date",
                               "event_date", "censor_date", "time", "event", "offset", "weight",
                               "covariate", "ignore"))
  expect_identical(SR_PROTECTED_ROLES, c("outcome", "exposure", "id", "cluster", "time_zero", "time", "event"))
  expect_true(all(SR_PROTECTED_ROLES %in% SR_ROLES))
})

# ---- suggestions -----------------------------------------------------------------------------------

test_that("suggestions on the cardiac sample follow the rules", {
  p <- .rl_cardiac()
  s <- sr_roles_suggest(p, "cardiac")
  expect_identical(names(s), c("column", "role", "reason"))
  expect_identical(s$column, vapply(sr_store_manifest(p, "cardiac")$schema, function(x) x$name, ""))
  expect_true(all(s$role %in% SR_ROLES))
  expect_true(all(nzchar(s$reason)))
  expect_identical(.rl_role_of(s, "patient_id"), "id")
  expect_identical(.rl_role_of(s, "site"), "cluster")
  expect_identical(.rl_role_of(s, "death_date"), "event_date")
  expect_identical(.rl_role_of(s, "followup_days"), "offset")
  expect_false(.rl_role_of(s, "weight_kg") == "weight")
  expect_identical(.rl_role_of(s, "weight_kg"), "covariate")
  # other date columns are covariates
  expect_identical(.rl_role_of(s, "admit_date"), "covariate")
  expect_identical(.rl_role_of(s, "surgery_date"), "covariate")
  expect_identical(.rl_role_of(s, "age"), "covariate")
})

test_that("outcome and exposure are never suggested, for any column of the sample", {
  p <- .rl_cardiac()
  s <- sr_roles_suggest(p, "cardiac")
  expect_false(any(s$role %in% c("outcome", "exposure")))
  expect_false(.rl_role_of(s, "mortality_30d") %in% c("outcome", "exposure"))
  expect_false(.rl_role_of(s, "statin") %in% c("outcome", "exposure"))
})

test_that("the reason names the rule", {
  p <- .rl_cardiac()
  s <- sr_roles_suggest(p, "cardiac")
  expect_match(s$reason[s$column == "patient_id"], "id pattern")
  expect_match(s$reason[s$column == "site"], "[0-9]+ distinct values \\(2-200\\)")
})

test_that("a cluster-looking column with one distinct value is not a cluster", {
  p <- .fx_project()
  withr::defer(sr_duck_close(p))
  p <- .rl_ingest(p, data.frame(site = rep("A", 20L), hospital = sprintf("H%02d", seq_len(20L)),
                                v = seq_len(20L)), "one")
  s <- sr_roles_suggest(p, "one")
  expect_identical(.rl_role_of(s, "site"), "covariate")
  expect_identical(.rl_role_of(s, "hospital"), "cluster")
})

test_that("weight$ matches a sampling weight but not weight_kg or wt_kg", {
  p <- .fx_project()
  withr::defer(sr_duck_close(p))
  p <- .rl_ingest(p, data.frame(weight = c(0.5, 1, 1.5, 2), weight_kg = c(60, 70, 80, 90),
                                wt_kg = c(61, 71, 81, 91), ipw_weight = c(1, 2, 3, 4)), "w")
  s <- sr_roles_suggest(p, "w")
  expect_identical(.rl_role_of(s, "weight"), "weight")
  expect_identical(.rl_role_of(s, "ipw_weight"), "weight")
  expect_identical(.rl_role_of(s, "weight_kg"), "covariate")
  expect_identical(.rl_role_of(s, "wt_kg"), "covariate")
})

test_that("a dictionary label drives a cluster suggestion", {
  p <- .rl_cardiac()
  p <- .rl_small(p, "small")
  expect_identical(.rl_role_of(sr_roles_suggest(p, "small"), "loc"), "covariate")
  p <- sr_dict_set(p, "small", .rl_dict(c("loc", "x"), c("Hospital site", "Measurement")), source = "test")
  s <- sr_roles_suggest(p, "small")
  expect_identical(.rl_role_of(s, "loc"), "cluster")
  expect_match(s$reason[s$column == "loc"], "6 distinct values")
  expect_identical(.rl_role_of(s, "x"), "covariate")
})

test_that("a broken dictionary falls back to names with a warning naming the table", {
  p <- .rl_cardiac()
  p <- .rl_small(p, "small")
  p <- sr_dict_set(p, "small", .rl_dict(c("loc", "x"), c("Hospital site", "Measurement")), source = "test")
  writeLines("tampered", .sr_dict_file(p, "small"))
  expect_error(sr_dict_get(p, "small"), "SHA-256")
  expect_warning(s <- sr_roles_suggest(p, "small"), "small")
  expect_identical(.rl_role_of(s, "loc"), "covariate")   # the label is no longer available
  expect_identical(nrow(s), 2L)
})

test_that("suggestions need an existing table", {
  p <- .rl_cardiac()
  expect_error(sr_roles_suggest(p, "nope"), "nope")
})

# ---- setting roles ---------------------------------------------------------------------------------

test_that("sr_roles is an empty named character vector until roles are set", {
  p <- .rl_cardiac()
  r <- sr_roles(p, "cardiac")
  expect_identical(r, stats::setNames(character(0), character(0)))
  expect_identical(sr_role_cols(p, "cardiac", "outcome"), character(0))
})

test_that("sr_roles_set stores a partial map and sr_role_cols reads it back", {
  p <- .rl_cardiac()
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure", patient_id = "id",
                                    site = "cluster", age = "covariate", sex = "covariate"))
  r <- sr_roles(p, "cardiac")
  expect_identical(r[["mortality_30d"]], "outcome")
  expect_identical(length(r), 6L)
  expect_identical(sr_role_cols(p, "cardiac", "outcome"), "mortality_30d")
  expect_identical(sr_role_cols(p, "cardiac", "exposure"), "statin")
  expect_identical(sr_role_cols(p, "cardiac", "covariate"), c("age", "sex"))
  expect_identical(sr_role_cols(p, "cardiac", "weight"), character(0))
  expect_identical(p$state$roles$cardiac, r)
})

test_that("setting roles again replaces that table map and leaves other tables alone", {
  p <- .rl_cardiac()
  p <- .rl_small(p, "small")
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", age = "covariate"))
  p <- sr_roles_set(p, "small", c(loc = "cluster"))
  p <- sr_roles_set(p, "cardiac", c(statin = "exposure"))
  expect_identical(names(sr_roles(p, "cardiac")), "statin")
  expect_identical(sr_roles(p, "small"), c(loc = "cluster"))
  p <- sr_roles_set(p, "cardiac", stats::setNames(character(0), character(0)))
  expect_identical(length(sr_roles(p, "cardiac")), 0L)
  expect_identical(sr_roles(p, "small"), c(loc = "cluster"))
})

test_that("sr_roles_set refuses two outcomes, an unknown role, an unknown column, and bad shapes", {
  p <- .rl_cardiac()
  e <- expect_error(sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "outcome")), "outcome")
  expect_match(conditionMessage(e), "mortality_30d")
  expect_match(conditionMessage(e), "statin")
  expect_error(sr_roles_set(p, "cardiac", c(age = "banana")), "banana")
  expect_error(sr_roles_set(p, "cardiac", c(not_a_column = "covariate")), "not_a_column")
  expect_error(sr_roles_set(p, "cardiac", c(Age = "covariate")), "Age")          # names are case-sensitive
  expect_error(sr_roles_set(p, "cardiac", c(age = "covariate", age = "ignore")), "age")   # duplicate
  expect_error(sr_roles_set(p, "cardiac", c(age = NA_character_)), "age")
  expect_error(sr_roles_set(p, "cardiac", c("covariate")), "named")
  expect_error(sr_roles_set(p, "cardiac", stats::setNames("covariate", "")), "name")
  expect_error(sr_roles_set(p, "cardiac", list(age = "covariate")), "character")
  expect_error(sr_roles_set(p, "nope", c(age = "covariate")), "nope")
  # nothing was recorded by the refusals
  expect_identical(length(sr_roles(p, "cardiac")), 0L)
})

test_that("sr_role_cols validates the role", {
  p <- .rl_cardiac()
  expect_error(sr_role_cols(p, "cardiac", "banana"), "banana")
  expect_error(sr_role_cols(p, "cardiac", c("id", "outcome")), "role")
})

test_that("roles survive a reopen", {
  p <- .rl_cardiac()
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure", patient_id = "id"))
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure", patient_id = "id",
                                    site = "cluster"))
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q))
  withr::defer(sr_project_close(q))
  expect_identical(sr_roles(q, "cardiac"), sr_roles(p, "cardiac"))
  expect_identical(sr_role_cols(q, "cardiac", "cluster"), "site")
  expect_identical(sr_roles(q, "cardiac")[["statin"]], "exposure")
})

# ---- sr_blind --------------------------------------------------------------------------------------

test_that("sr_blind is TRUE on a new project and FALSE once a sap.locked event is replayed", {
  p <- .fx_project()
  withr::defer(sr_duck_close(p))
  expect_true(sr_blind(p))
  expect_true(sr_blind(list(state = list())))
  p <- sr_project_append(p, "sap.locked", list())                             # the real reducer (sap/sap.R)
  expect_false(sr_blind(p))
  expect_true(isTRUE(p$state$sap$locked))
})

# ---- .sr_assert_blind ------------------------------------------------------------------------------

test_that(".sr_assert_blind refuses outcome statistics by element name, however deeply nested", {
  expect_error(.sr_assert_blind(list(a = list(auc = 0.9)), "mortality_30d"), "a\\$auc")
  expect_error(.sr_assert_blind(list(x = list(y = list(OR = 1))), "mortality_30d"), "x\\$y\\$OR")
  for (nm in c("auc", "or", "rr", "hr", "estimate", "effect", "crosstab", "by_outcome", "smd_by_outcome")) {
    x <- stats::setNames(list(1), nm)
    expect_error(.sr_assert_blind(x, "mortality_30d"), nm, info = nm)
    expect_error(.sr_assert_blind(list(top = x), "mortality_30d"), nm, info = nm)
    expect_error(.sr_assert_blind(list(top = stats::setNames(list(1), toupper(nm))), "mortality_30d"), info = nm)
  }
  expect_error(.sr_assert_blind(list(list(list(Estimate = 1))), "mortality_30d"), "Estimate")
  expect_error(.sr_assert_blind(data.frame(or = 1, n = 2), "mortality_30d"), "or")
})

test_that(".sr_assert_blind matches whole names only and never looks at cell values", {
  expect_silent(.sr_assert_blind(list(order = 1, n_or_more = 2, orbit = 3, hrs = 4), "mortality_30d"))
  expect_silent(.sr_assert_blind(list(order = 1), "mortality_30d"))
  expect_silent(.sr_assert_blind(list(n_missing = 3), "mortality_30d"))
  expect_silent(.sr_assert_blind(list(note = "auc", v = c("or", "hr")), "mortality_30d"))
  x <- list(n_missing = 3)
  expect_identical(.sr_assert_blind(x, "mortality_30d"), x)
  expect_identical(withVisible(.sr_assert_blind(x, "mortality_30d"))$visible, FALSE)
})

test_that(".sr_assert_blind refuses a data.frame that crosses the outcome with another column", {
  df <- data.frame(mortality_30d = c(0, 1), statin = c(1, 0), n = c(10, 20))
  expect_error(.sr_assert_blind(df, "mortality_30d", other_cols = "statin"), "mortality_30d")
  expect_error(.sr_assert_blind(list(tab = df), "mortality_30d", other_cols = "statin"), "tab")
  # without other_cols there is nothing to cross with
  expect_silent(.sr_assert_blind(df, "mortality_30d"))
  # a data.frame with the outcome but no other role column is fine
  expect_silent(.sr_assert_blind(data.frame(mortality_30d = 1, n = 2), "mortality_30d", other_cols = "statin"))
})

test_that(".sr_assert_blind allows a per-column profile that lists the outcome as a value", {
  prof <- data.frame(column = c("mortality_30d", "statin"), n_missing = 1:2)
  expect_silent(.sr_assert_blind(prof, "mortality_30d", other_cols = "statin"))
  expect_silent(.sr_assert_blind(list(profile = prof), "mortality_30d", other_cols = "statin"))
})

test_that(".sr_assert_blind refuses a table or matrix whose dimnames cross the outcome", {
  x <- table(mortality_30d = c(0, 1, 1, 0), statin = c(1, 1, 0, 0))
  expect_error(.sr_assert_blind(x, "mortality_30d", other_cols = "statin"), "mortality_30d")
  m <- matrix(0, 2, 2, dimnames = list(c("mortality_30d", "x"), c("statin", "y")))
  expect_error(.sr_assert_blind(list(m = m), "mortality_30d", other_cols = "statin"), "m")
  ok <- matrix(0, 2, 2, dimnames = list(c("a", "b"), c("statin", "y")))
  expect_silent(.sr_assert_blind(ok, "mortality_30d", other_cols = "statin"))
  # a one-way table of the outcome alone is not a crossing
  expect_silent(.sr_assert_blind(table(mortality_30d = c(0, 1, 1)), "mortality_30d", other_cols = "statin"))
})

test_that(".sr_assert_blind accepts degenerate inputs", {
  expect_silent(.sr_assert_blind(NULL, "mortality_30d"))
  expect_silent(.sr_assert_blind(list(), "mortality_30d"))
  expect_silent(.sr_assert_blind(1:3, "mortality_30d"))
  expect_silent(.sr_assert_blind(list(1, "a"), "mortality_30d"))
})

# ---- fix round 1 ------------------------------------------------------------------------------------

test_that(".sr_assert_blind does not recurse forever on POSIXlt (top level and nested)", {
  lt <- as.POSIXlt(as.POSIXct("2024-03-01 10:00:00", tz = "UTC"))
  expect_silent(.sr_assert_blind(lt, "mort"))
  expect_silent(.sr_assert_blind(list(a = lt), "mort"))
  expect_silent(.sr_assert_blind(list(range = list(from = lt, to = lt)), "mort", other_cols = "x"))
  expect_error(.sr_assert_blind(list(a = lt, auc = 1), "mort"), "auc")
})

test_that(".sr_assert_blind pins the behaviour for named atomic vectors", {
  expect_error(.sr_assert_blind(c(OR = 3, ICU = 5), "mort"), "OR")
  expect_silent(.sr_assert_blind(c(order = 1), "mort"))
})

test_that("a DATE column is never suggested as a cluster", {
  p <- .fx_project()
  withr::defer(sr_duck_close(p))
  d <- format(as.Date("2024-01-01") + (0:29) * 3)
  p <- .rl_ingest(p, data.frame(hospital_admit_date = d, hospital_code = rep(LETTERS[1:3], 10L),
                                v = seq_len(30L)), "dt")
  expect_match(sr_store_manifest(p, "dt")$schema[[1L]]$type, "^DATE")
  s <- sr_roles_suggest(p, "dt")
  expect_identical(.rl_role_of(s, "hospital_admit_date"), "covariate")
  expect_match(s$reason[s$column == "hospital_admit_date"], "date column")
  expect_identical(.rl_role_of(s, "hospital_code"), "cluster")
})

test_that("reasons say label when the dictionary label made the match", {
  p <- .fx_project()
  withr::defer(sr_duck_close(p))
  d <- format(as.Date("2024-01-01") + (0:29) * 3)
  p <- .rl_ingest(p, data.frame(loc = rep(LETTERS[1:3], 10L), w = seq_len(30L) / 10, d1 = d, d2 = d), "lab")
  p <- sr_dict_set(p, "lab", .rl_dict(c("loc", "w", "d1", "d2"),
                                      c("Hospital site", "sampling weight", "Death event", "Visit")),
                   source = "test")
  s <- sr_roles_suggest(p, "lab")
  why <- function(col) s$reason[s$column == col]
  expect_identical(.rl_role_of(s, "loc"), "cluster")
  expect_match(why("loc"), "label matches cluster pattern")
  expect_identical(.rl_role_of(s, "w"), "weight")
  expect_match(why("w"), "label matches weight pattern")
  expect_identical(.rl_role_of(s, "d1"), "event_date")
  expect_match(why("d1"), "label")
  expect_identical(.rl_role_of(s, "d2"), "covariate")
  expect_match(why("d2"), "no event keyword in its name or label")
})
