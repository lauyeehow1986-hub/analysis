# Tests for profile/profile.R: out-of-core column profiles, the reproducible sample, the memory
# estimator. Synthetic data only; everything lives under withr::local_tempdir().

.pf_cardiac <- function(env = parent.frame(), n = 2000L) {
  p <- .fx_cardiac_project(n = n, env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

.pf_row <- function(cols, name) cols[cols$column == name, , drop = FALSE]

# The columns a blind outcome row may keep.
.pf_marginals <- c("column", "type", "n", "n_missing", "p_missing", "n_distinct", "min", "max", "top_values")

# ---- exact statistics -------------------------------------------------------------------------------

test_that("the profile has one row per column with the specified fields and types", {
  p <- .pf_cardiac()
  pr <- sr_profile_table(p, "cardiac")
  expect_named(pr, c("columns", "sample_file", "store_hash", "exact", "rows"))
  cols <- pr$columns
  expect_identical(names(cols), c("column", "type", "n", "n_missing", "p_missing", "n_distinct", "min", "max",
                                  "mean", "sd", "q01", "q25", "q50", "q75", "q99", "top_values", "mean_nchar",
                                  "p_numeric_parsable", "p_date_parsable"))
  expect_identical(cols$column, vapply(sr_store_manifest(p, "cardiac")$schema, function(s) s$name, ""))
  expect_true(pr$exact)
  expect_identical(pr$store_hash, sr_store_hash(sr_store_manifest(p, "cardiac")))
  expect_true(file.exists(pr$sample_file))
  expect_true(all(cols$n == 2000))
  expect_type(cols$min, "character")
  expect_type(cols$max, "character")
  expect_type(cols$mean, "double")
  expect_type(cols$q50, "double")
  expect_type(cols$top_values, "list")
  tv <- cols$top_values[[which(cols$column == "site")]]
  expect_s3_class(tv, "data.frame")
  expect_identical(names(tv), c("value", "n"))
  expect_lte(nrow(tv), 10L)
  expect_false(is.unsorted(rev(tv$n)))
  expect_identical(sum(tv$n), 2000)
})

test_that("p_missing equals the NA proportion of the source column", {
  p <- .pf_cardiac()
  src <- sr_sample_cardiac(n = 2000L)$data
  cols <- sr_profile_table(p, "cardiac")$columns
  for (v in c("death_date", "hba1c_pct", "statin_start_date", "age")) {
    r <- .pf_row(cols, v)
    expect_equal(r$p_missing, mean(is.na(src[[v]])), tolerance = 1e-12, info = v)
    expect_equal(r$n_missing, sum(is.na(src[[v]])), info = v)
  }
  expect_gt(.pf_row(cols, "death_date")$p_missing, 0)
})

test_that("q50 and the other quantiles of a numeric column are quantile(type = 7)", {
  p <- .pf_cardiac()
  src <- sr_sample_cardiac(n = 2000L)$data
  r <- .pf_row(sr_profile_table(p, "cardiac")$columns, "age")
  expect_equal(r$q50, unname(stats::quantile(src$age, 0.5, type = 7)), tolerance = 1e-9)
  want <- unname(stats::quantile(src$age, c(0.01, 0.25, 0.75, 0.99), type = 7))
  expect_equal(c(r$q01, r$q25, r$q75, r$q99), want, tolerance = 1e-9)
  expect_equal(r$mean, mean(src$age), tolerance = 1e-9)
  expect_equal(r$sd, stats::sd(src$age), tolerance = 1e-9)
  expect_identical(r$min, as.character(min(src$age)))
  expect_identical(r$max, as.character(max(src$age)))
  expect_equal(r$n_distinct, length(unique(src$age)))
})

test_that("a date column has ISO min and max and no numeric statistics", {
  p <- .pf_cardiac()
  src <- sr_sample_cardiac(n = 2000L)$data
  r <- .pf_row(sr_profile_table(p, "cardiac")$columns, "admit_date")
  expect_identical(r$min, format(min(src$admit_date)))
  expect_identical(r$max, format(max(src$admit_date)))
  expect_true(all(is.na(c(r$mean, r$sd, r$q01, r$q25, r$q50, r$q75, r$q99, r$mean_nchar,
                          r$p_numeric_parsable, r$p_date_parsable))))
})

test_that("a VARCHAR column reports parse shares over non-missing values, and no numeric statistics", {
  p <- .pf_cardiac()
  cols <- sr_profile_table(p, "cardiac")$columns
  r <- .pf_row(cols, "creatinine_umol")
  expect_identical(r$type, "VARCHAR")
  expect_equal(r$p_numeric_parsable, 0.995, tolerance = 1e-9)
  expect_lt(r$p_date_parsable, 0.05)
  expect_gt(r$mean_nchar, 1)
  expect_true(all(is.na(c(r$mean, r$sd, r$q01, r$q25, r$q50, r$q75, r$q99))))
  # numeric and date columns carry neither
  a <- .pf_row(cols, "age")
  expect_true(is.na(a$mean_nchar) && is.na(a$p_numeric_parsable) && is.na(a$p_date_parsable))
  # a text column is not numeric
  s <- .pf_row(cols, "site")
  expect_equal(s$p_numeric_parsable, 0)
})

test_that("a column with no non-missing value has NA parse shares, not NaN", {
  p <- .fx_project()
  withr::defer(sr_duck_close(p))
  df <- data.frame(id = 1:30, empty = NA_character_, stringsAsFactors = FALSE)
  csv <- file.path(dirname(p$paths$dir), "e.csv")
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = "e")
  r <- .pf_row(sr_profile_table(p, "e")$columns, "empty")
  expect_equal(r$n_missing, 30)
  expect_equal(r$p_missing, 1)
  if (identical(r$type, "VARCHAR")) {
    expect_true(is.na(r$p_numeric_parsable) && is.na(r$p_date_parsable))
  }
})

test_that("all scalar statistics come from one aggregate query, plus one top-values query per column", {
  p <- .pf_cardiac()
  seen <- character()
  orig <- get("sr_duck_query", envir = globalenv())
  assign("sr_duck_query", function(con, sql) {
    seen <<- c(seen, sql)
    orig(con, sql)
  }, envir = globalenv())
  withr::defer(assign("sr_duck_query", orig, envir = globalenv()))
  pr <- sr_profile_table(p, "cardiac")
  expect_equal(sum(grepl("count(*) AS n_rows", seen, fixed = TRUE)), 1L)
  expect_equal(sum(grepl("GROUP BY 1) ORDER BY c DESC, v LIMIT 10", seen, fixed = TRUE)), nrow(pr$columns))
})

# ---- cache ------------------------------------------------------------------------------------------

test_that("the cache file is written once and reused", {
  p <- .pf_cardiac()
  a <- sr_profile_table(p, "cardiac")
  cache <- list.files(file.path(p$paths$dir, "data", "profile"), full.names = TRUE)
  expect_length(cache, 1L)
  expect_match(basename(cache), sprintf("^cardiac-%s[.]json$", substr(a$store_hash, 1L, 12L)))
  t0 <- file.mtime(cache)
  Sys.sleep(1.1)
  b <- sr_profile_table(p, "cardiac")
  expect_identical(file.mtime(cache), t0)
  expect_identical(b$columns, a$columns)
  expect_identical(b$store_hash, a$store_hash)
})

test_that("a cache made before an outcome role was set is not reused while blind", {
  p <- .pf_cardiac()
  a <- sr_profile_table(p, "cardiac")
  expect_false(is.na(.pf_row(a$columns, "mortality_30d")$mean))
  expect_false(is.na(.pf_row(a$columns, "mortality_30d")$sd))
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure"))
  b <- sr_profile_table(p, "cardiac")
  r <- .pf_row(b$columns, "mortality_30d")
  expect_true(is.na(r$mean))
  expect_true(is.na(r$sd))
  cache <- list.files(file.path(p$paths$dir, "data", "profile"), full.names = TRUE)
  expect_identical(.sr_read_json_file(cache)$redacted_cols, list("mortality_30d"))
})

test_that("the cache is recomputed when the exact-vs-approximate decision changes", {
  p <- .pf_cardiac()
  expect_true(sr_profile_table(p, "cardiac")$exact)
  cache <- list.files(file.path(p$paths$dir, "data", "profile"), full.names = TRUE)
  expect_true(.sr_read_json_file(cache)$exact)
  withr::local_options(sr.profile_exact_max = 100)
  seen <- character()
  orig <- get("sr_duck_query", envir = globalenv())
  assign("sr_duck_query", function(con, sql) {
    seen <<- c(seen, sql)
    orig(con, sql)
  }, envir = globalenv())
  withr::defer(assign("sr_duck_query", orig, envir = globalenv()))
  expect_false(sr_profile_table(p, "cardiac")$exact)
  # a stale exact cache would have been reused: no approximate query would run and the file would say TRUE
  expect_true(any(grepl("approx_quantile", seen, fixed = TRUE)))
  expect_false(.sr_read_json_file(cache)$exact)
  # and back again: the approximate cache is not reused for an exact request
  withr::local_options(sr.profile_exact_max = 1e6)
  seen <- character()
  expect_true(sr_profile_table(p, "cardiac")$exact)
  expect_true(any(grepl("quantile_cont", seen, fixed = TRUE)))
  expect_true(.sr_read_json_file(cache)$exact)
})

test_that("a damaged cache file is recomputed", {
  p <- .pf_cardiac()
  a <- sr_profile_table(p, "cardiac")
  cache <- list.files(file.path(p$paths$dir, "data", "profile"), full.names = TRUE)
  writeLines("not json", cache)
  b <- sr_profile_table(p, "cardiac")
  expect_identical(b$columns, a$columns)
})

# ---- approximate path -------------------------------------------------------------------------------

test_that("above sr.profile_exact_max the approximate aggregates run and stay close", {
  p <- .pf_cardiac()
  exact <- sr_profile_table(p, "cardiac")
  expect_true(exact$exact)
  q <- .fx_cardiac_project(n = 2000L)
  withr::defer(sr_duck_close(q))
  withr::local_options(sr.profile_exact_max = 100)
  seen <- character()
  orig <- get("sr_duck_query", envir = globalenv())
  assign("sr_duck_query", function(con, sql) {
    seen <<- c(seen, sql)
    orig(con, sql)
  }, envir = globalenv())
  withr::defer(assign("sr_duck_query", orig, envir = globalenv()))
  approx <- sr_profile_table(q, "cardiac")
  expect_false(approx$exact)
  expect_true(any(grepl("approx_count_distinct", seen, fixed = TRUE)))
  expect_true(any(grepl("approx_quantile", seen, fixed = TRUE)))
  expect_false(any(grepl("quantile_cont", seen, fixed = TRUE)))
  e <- .pf_row(exact$columns, "age")
  a <- .pf_row(approx$columns, "age")
  expect_equal(a$n_distinct, e$n_distinct, tolerance = 0.1)
  for (f in c("q01", "q25", "q50", "q75", "q99")) {
    expect_lte(abs(a[[f]] - e[[f]]), 0.1 * (e$q99 - e$q01), label = f)
  }
  expect_equal(a$mean, e$mean, tolerance = 1e-9)
  expect_equal(.pf_row(approx$columns, "site")$n_distinct, 6, tolerance = 0.1)
})

# ---- sample -----------------------------------------------------------------------------------------

test_that("the same seed gives an identical, byte-stable sample and a different seed differs", {
  p <- .pf_cardiac()
  a <- sr_profile_table(p, "cardiac", sample_n = 300L, seed = 1L)
  expect_match(a$sample_file, sprintf("data/sample/cardiac-%s-1[.]parquet$", substr(a$store_hash, 1L, 12L)))
  s1 <- sr_sample(p, "cardiac", seed = 1L)
  expect_equal(nrow(s1), 300L)
  md5 <- unname(tools::md5sum(a$sample_file))
  unlink(a$sample_file)
  b <- sr_profile_table(p, "cardiac", sample_n = 300L, seed = 1L)
  expect_identical(unname(tools::md5sum(b$sample_file)), md5)
  expect_identical(sr_sample(p, "cardiac", seed = 1L), s1)
  c2 <- sr_profile_table(p, "cardiac", sample_n = 300L, seed = 2L)
  expect_false(identical(sr_sample(p, "cardiac", seed = 2L), s1))
  expect_false(identical(unname(tools::md5sum(c2$sample_file)), md5))
  # a sample is a subset of the table, ordered by its columns
  expect_true(all(s1$patient_id %in% sr_sample_cardiac(n = 2000L)$data$patient_id))
})

test_that("an existing sample of the right size is reused, one of the wrong size is regenerated", {
  p <- .pf_cardiac()
  a <- sr_profile_table(p, "cardiac", sample_n = 300L, seed = 1L)
  t0 <- file.mtime(a$sample_file)
  Sys.sleep(1.1)
  b <- sr_profile_table(p, "cardiac", sample_n = 300L, seed = 1L)
  expect_identical(file.mtime(b$sample_file), t0)
  d <- sr_profile_table(p, "cardiac", sample_n = 500L, seed = 1L)
  expect_identical(d$sample_file, a$sample_file)
  expect_equal(nrow(sr_sample(p, "cardiac", seed = 1L)), 500L)
  # a sample larger than the table is the whole table
  e <- sr_profile_table(p, "cardiac", sample_n = 100000L, seed = 1L)
  expect_equal(nrow(sr_sample(p, "cardiac", seed = 1L)), 2000L)
  expect_true(file.exists(e$sample_file))
  expect_length(list.files(dirname(e$sample_file), pattern = "[.]tmp$"), 0L)
})

test_that("sr_sample makes the default sample when none exists", {
  p <- .pf_cardiac()
  expect_false(dir.exists(file.path(p$paths$dir, "data", "sample")))
  s <- sr_sample(p, "cardiac", seed = 7L)
  expect_equal(nrow(s), 2000L)
  expect_length(list.files(file.path(p$paths$dir, "data", "sample"), pattern = "-7[.]parquet$"), 1L)
})

test_that("the sample leaves the application connection's thread setting untouched", {
  p <- .pf_cardiac()
  con <- sr_duck(p)
  before <- DBI::dbGetQuery(con, "SELECT current_setting('threads') AS t")$t
  expect_error(sr_profile_table(p, "nope"), "no manifest")
  sr_profile_table(p, "cardiac", sample_n = 300L)
  expect_identical(DBI::dbGetQuery(sr_duck(p), "SELECT current_setting('threads') AS t")$t, before)
})

test_that("bad arguments are refused", {
  p <- .pf_cardiac()
  expect_error(sr_profile_table(p, "cardiac", sample_n = 0), "sample_n")
  expect_error(sr_profile_table(p, "cardiac", sample_n = 1.5), "sample_n")
  expect_error(sr_profile_table(p, "cardiac", seed = -1), "seed")
  expect_error(sr_sample(p, "cardiac", seed = NA), "seed")
  expect_error(sr_profile_table(p, "Bad Name"), "invalid table name")
})

# ---- outcome-blind ----------------------------------------------------------------------------------

test_that("while blind, the outcome row carries only marginals and passes the blind gate", {
  p <- .pf_cardiac()
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure"))
  expect_true(sr_blind(p))
  pr <- sr_profile_table(p, "cardiac")
  r <- .pf_row(pr$columns, "mortality_30d")
  expect_true(all(is.na(r[setdiff(names(r), .pf_marginals)])))
  expect_true(all(is.na(c(r$mean, r$sd, r$q01, r$q25, r$q50, r$q75, r$q99, r$mean_nchar,
                          r$p_numeric_parsable, r$p_date_parsable))))
  expect_false(any(is.na(r[c("type", "n", "n_missing", "p_missing", "n_distinct", "min", "max")])))
  expect_identical(sum(r$top_values[[1L]]$n), 2000)
  expect_no_error(.sr_assert_blind(pr, "mortality_30d", other_cols = "statin"))
  expect_no_error(.sr_assert_blind(pr$columns, "mortality_30d", other_cols = "statin"))
  # every other column is profiled in full, the exposure included
  expect_false(is.na(.pf_row(pr$columns, "statin")$mean))
  expect_false(is.na(.pf_row(pr$columns, "age")$q50))
  # the same holds for the cached copy
  pr2 <- sr_profile_table(p, "cardiac")
  expect_true(is.na(.pf_row(pr2$columns, "mortality_30d")$mean))
  expect_no_error(.sr_assert_blind(pr2, "mortality_30d", other_cols = "statin"))
})

test_that("a text outcome column is redacted too (no mean_nchar, no parse shares)", {
  p <- .pf_cardiac()
  p <- sr_roles_set(p, "cardiac", c(creatinine_umol = "outcome", statin = "exposure"))
  r <- .pf_row(sr_profile_table(p, "cardiac")$columns, "creatinine_umol")
  expect_true(all(is.na(c(r$mean_nchar, r$p_numeric_parsable, r$p_date_parsable))))
  expect_false(is.na(r$n_distinct))
})

test_that("once the SAP is locked the outcome row is filled in", {
  p <- .pf_cardiac()
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure"))
  blind <- sr_profile_table(p, "cardiac")
  expect_true(is.na(.pf_row(blind$columns, "mortality_30d")$mean))
  expect_false(exists("sap.locked", envir = .sr_reducers, inherits = FALSE))   # P1d owns the real one
  sr_reducer_register("sap.locked", function(state, event) {
    state$sap$locked <- TRUE
    state
  })
  withr::defer(rm(list = "sap.locked", envir = .sr_reducers))
  p <- sr_project_append(p, "sap.locked", list())
  expect_false(sr_blind(p))
  open <- sr_profile_table(p, "cardiac")
  src <- sr_sample_cardiac(n = 2000L)$data
  r <- .pf_row(open$columns, "mortality_30d")
  expect_equal(r$mean, mean(src$mortality_30d), tolerance = 1e-9)
  expect_equal(r$sd, stats::sd(src$mortality_30d), tolerance = 1e-9)
})

# ---- memory estimate --------------------------------------------------------------------------------

test_that("5e8 rows do not fit the locked16 budget and route glm to bigglm; 2000 rows are in memory", {
  p <- .pf_cardiac()
  cols <- c("age", "sbp", "weight_kg", "sex", "site")
  big <- sr_mem_estimate(p, "cardiac", cols, "glm", profile = "locked16", override_rows = 5e8)
  expect_false(big$fits)
  expect_identical(big$route, "bigglm")
  expect_identical(big$budget_gb, 10)
  expect_gt(big$need_gb, 8)
  expect_named(big, c("need_gb", "budget_gb", "fits", "route", "reason", "rows", "p"))
  expect_match(big$reason, "GB")
  expect_match(big$reason, "bigglm")
  expect_match(big$reason, "locked16")
  small <- sr_mem_estimate(p, "cardiac", cols, "glm", profile = "locked16")
  expect_true(small$fits)
  expect_identical(small$route, "in_memory")
  expect_lt(small$need_gb, 0.1)
  expect_equal(small$rows, 2000)
  expect_match(small$reason, "fits in memory")
})

test_that("the other methods route cox to bam and ml to subsample when the data do not fit", {
  p <- .pf_cardiac()
  cols <- c("age", "sbp", "weight_kg")
  expect_identical(sr_mem_estimate(p, "cardiac", cols, "cox", profile = "locked16", override_rows = 5e8)$route, "bam")
  expect_identical(sr_mem_estimate(p, "cardiac", cols, "ml", profile = "locked16", override_rows = 5e8)$route, "subsample")
  expect_identical(sr_mem_estimate(p, "cardiac", cols, "lm", profile = "locked16", override_rows = 5e8)$route, "bigglm")
  for (m in c("glm", "lm", "cox", "ml")) {
    expect_identical(sr_mem_estimate(p, "cardiac", cols, m, profile = "locked16")$route, "in_memory", info = m)
  }
})

test_that("the estimate follows the formula and the profile budget", {
  p <- .pf_cardiac()
  ty <- sr_profile_table(p, "cardiac")$columns
  expect_identical(ty$type[ty$column %in% c("age", "weight_kg")], c("BIGINT", "DOUBLE"))
  # two numeric columns: age (BIGINT) and weight_kg (DOUBLE), 8 bytes each; p defaults to 2
  e <- sr_mem_estimate(p, "cardiac", c("age", "weight_kg"), "glm", override_rows = 1e9)
  expect_equal(e$p, 2)
  expect_equal(e$need_gb, (1e9 * 16 + 3 * 1e9 * 2 * 8) / 1024^3, tolerance = 1e-9)
  # the laptop budget is larger than locked16's
  lap <- sr_mem_estimate(p, "cardiac", c("age", "weight_kg"), "glm", profile = "laptop", override_rows = 1e9)
  low <- sr_mem_estimate(p, "cardiac", c("age", "weight_kg"), "glm", profile = "locked16", override_rows = 1e9)
  expect_identical(lap$budget_gb, 18)
  expect_identical(low$budget_gb, 10)
  expect_equal(lap$need_gb, low$need_gb)
  # an explicit p replaces the default; a categorical column adds levels - 1 to the default
  expect_equal(sr_mem_estimate(p, "cardiac", c("age", "weight_kg"), "glm", p = 50, override_rows = 1e6)$p, 50)
  cat_est <- sr_mem_estimate(p, "cardiac", c("age", "site"), "glm", override_rows = 1e6)
  expect_equal(cat_est$p, 2 + 6 - 1)
  # a VARCHAR column costs mean_nchar + 8 bytes per row
  prof <- sr_profile_table(p, "cardiac")$columns
  nch <- .pf_row(prof, "site")$mean_nchar
  expect_equal(cat_est$need_gb, (1e6 * (8 + nch + 8) + 3 * 1e6 * cat_est$p * 8) / 1024^3, tolerance = 1e-9)
})

test_that("the estimate refuses unknown columns and bad arguments", {
  p <- .pf_cardiac()
  expect_error(sr_mem_estimate(p, "cardiac", c("age", "no_such_col")), "no_such_col")
  expect_error(sr_mem_estimate(p, "cardiac", character()), "cols")
  expect_error(sr_mem_estimate(p, "cardiac", NA_character_), "cols")
  expect_error(sr_mem_estimate(p, "cardiac", "age", "svm"), "should be one of")
  expect_error(sr_mem_estimate(p, "cardiac", "age", p = 0), "p must")
  expect_error(sr_mem_estimate(p, "cardiac", "age", override_rows = -1), "override_rows")
  expect_error(sr_mem_estimate(p, "cardiac", "age", profile = "toaster"), "unknown profile")
  expect_error(sr_mem_estimate(p, "nope", "age"), "no manifest")
})

test_that("the estimate works for a blind outcome column", {
  p <- .pf_cardiac()
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure"))
  e <- sr_mem_estimate(p, "cardiac", c("mortality_30d", "age", "statin"), "glm")
  expect_true(e$fits)
  expect_no_error(.sr_assert_blind(e, "mortality_30d", other_cols = "statin"))
})

# ---- fix round 1 ------------------------------------------------------------------------------------

test_that("while blind, sr_sample drops the outcome column and passes the blind gate", {
  p <- .pf_cardiac()
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure"))
  expect_true(sr_blind(p))
  sr_profile_table(p, "cardiac", sample_n = 300L)
  s <- sr_sample(p, "cardiac")
  expect_false("mortality_30d" %in% names(s))
  expect_true(all(c("statin", "age", "patient_id") %in% names(s)))
  expect_equal(nrow(s), 300L)
  expect_no_error(.sr_assert_blind(s, "mortality_30d", other_cols = "statin"))
  # a sample made on demand (no file yet) is filtered too
  s2 <- sr_sample(p, "cardiac", seed = 9L)
  expect_false("mortality_30d" %in% names(s2))
})

test_that("once the SAP is locked sr_sample returns the outcome column", {
  p <- .pf_cardiac()
  p <- sr_roles_set(p, "cardiac", c(mortality_30d = "outcome", statin = "exposure"))
  expect_false("mortality_30d" %in% names(sr_sample(p, "cardiac")))
  sr_reducer_register("sap.locked", function(state, event) {
    state$sap$locked <- TRUE
    state
  })
  withr::defer(rm(list = "sap.locked", envir = .sr_reducers))
  p <- sr_project_append(p, "sap.locked", list())
  expect_false(sr_blind(p))
  s <- sr_sample(p, "cardiac")
  expect_true("mortality_30d" %in% names(s))
  expect_identical(names(s), vapply(sr_store_manifest(p, "cardiac")$schema, function(x) x$name, ""))
})

test_that("the sample connection is single-threaded, locked, and has its own spill dir and half the memory", {
  p <- .pf_cardiac()
  main <- sr_duck(p)
  one <- .sr_prof_single_con(p)
  withr::defer(DBI::dbDisconnect(one, shutdown = TRUE))
  get1 <- function(con, name) DBI::dbGetQuery(con, sprintf("SELECT current_setting('%s') AS v", name))$v
  expect_equal(as.integer(get1(one, "threads")), 1L)
  expect_true(as.logical(get1(one, "lock_configuration")))
  expect_false(as.logical(get1(one, "autoinstall_known_extensions")))
  expect_false(as.logical(get1(one, "autoload_known_extensions")))
  expect_false(as.logical(get1(one, "allow_community_extensions")))
  tmp <- normalizePath(get1(one, "temp_directory"), winslash = "/", mustWork = FALSE)
  data <- normalizePath(file.path(p$paths$dir, "data"), winslash = "/", mustWork = FALSE)
  expect_identical(tmp, file.path(data, "tmp", "sample"))
  expect_true(dir.exists(tmp))
  expect_false(identical(tmp, normalizePath(get1(main, "temp_directory"), winslash = "/", mustWork = FALSE)))
  # memory: half of the application connection's limit (both printed by DuckDB as "<x> GiB")
  gib <- function(s) as.numeric(sub("[[:space:]]*GiB$", "", s))
  expect_match(get1(one, "memory_limit"), "GiB$")
  expect_equal(gib(get1(one, "memory_limit")), gib(get1(main, "memory_limit")) / 2, tolerance = 0.02)
  expect_equal(as.integer(get1(main, "threads")), sr_profile_caps(p$meta$profile)$duckdb_threads)
  # the lock holds
  expect_error(DBI::dbExecute(one, "SET threads = 4"), "locked")
})

test_that("the sample connection is closed after a sample, and after a failure", {
  p <- .pf_cardiac()
  handles <- list()
  orig <- get(".sr_prof_single_con", envir = globalenv())
  assign(".sr_prof_single_con", function(project) {
    con <- orig(project)
    handles[[length(handles) + 1L]] <<- con
    con
  }, envir = globalenv())
  withr::defer(assign(".sr_prof_single_con", orig, envir = globalenv()))
  sr_profile_table(p, "cardiac", sample_n = 300L)
  expect_length(handles, 1L)
  expect_false(DBI::dbIsValid(handles[[1L]]))
  # a failure inside the sample (a part file has gone) still closes it
  m <- sr_store_manifest(p, "cardiac")
  hash <- sr_store_hash(m)
  unlink(.sr_prof_sample_path(p, "cardiac", hash, 5L))
  part <- file.path(sr_store_dir(p, "cardiac"), m$parts[[1L]]$file)
  file.rename(part, paste0(part, ".away"))
  withr::defer(file.rename(paste0(part, ".away"), part))
  expect_error(.sr_prof_ensure_sample(p, "cardiac", m, hash, 300L, 5L), "missing part")
  expect_length(handles, 2L)
  expect_false(DBI::dbIsValid(handles[[2L]]))
  expect_length(list.files(file.path(p$paths$dir, "data", "sample"), pattern = "[.]tmp$"), 0L)
})

test_that("sr_mem_estimate has no side effect on the sample directory", {
  p <- .pf_cardiac()
  e <- sr_mem_estimate(p, "cardiac", c("age", "site"), "glm")
  expect_true(e$fits)
  expect_false(dir.exists(file.path(p$paths$dir, "data", "sample")))
  expect_length(list.files(file.path(p$paths$dir, "data", "profile")), 1L)
})

test_that("quantile levels do not depend on options(OutDec)", {
  p <- .pf_cardiac()
  withr::local_options(OutDec = ",")
  src <- sr_sample_cardiac(n = 2000L)$data
  r <- .pf_row(sr_profile_table(p, "cardiac")$columns, "age")
  expect_equal(r$q01, unname(stats::quantile(src$age, 0.01, type = 7)), tolerance = 1e-9)
  expect_equal(r$q99, unname(stats::quantile(src$age, 0.99, type = 7)), tolerance = 1e-9)
})
