# Tests for the synthetic cardiac cohort (samples/gen_cardiac.R) and the fixture helpers.

NYHA <- c("I", "II", "III", "IV")
cols <- c("patient_id", "site", "age", "sex", "weight_kg", "height_cm", "bmi", "sbp",
          "creatinine_umol", "hba1c_pct", "glucose_mmol", "nyha", "diabetes", "admit_date",
          "surgery_date", "discharge_date", "death_date", "mortality_30d", "statin_start_date",
          "statin", "followup_days", "notes", "const_flag", "bmi_copy", "lab_status")

test_that("the same seed gives identical data and a different seed does not", {
  a <- sr_sample_cardiac(n = 300L, seed = 7L)
  b <- sr_sample_cardiac(n = 300L, seed = 7L)
  d <- sr_sample_cardiac(n = 300L, seed = 8L)
  expect_identical(a, b)
  expect_false(identical(a$data, d$data))
  expect_identical(sr_sample_cardiac(n = 300L, seed = 7L, defects = FALSE),
                   sr_sample_cardiac(n = 300L, seed = 7L, defects = FALSE))
})

test_that("the cohort has the specified columns, ids and dictionary", {
  s <- sr_sample_cardiac(n = 500L)
  expect_identical(names(s$data), cols)
  expect_equal(nrow(s$data), 500L)
  expect_identical(s$data$patient_id[1:2], c("P000001", "P000002"))
  expect_length(unique(s$data$site), 6L)
  expect_identical(names(s$dict), c("variable", "label", "type", "units", "min", "max", "levels"))
  expect_setequal(s$dict$variable, cols)
  expect_identical(s$dict$levels[s$dict$variable == "nyha"], "I|II|III|IV")
  expect_identical(names(s$planted), c("check", "column", "rows_hint"))
})

test_that("the clean twin has no planted defects", {
  s <- sr_sample_cardiac(n = 2000L, defects = FALSE)
  df <- s$data
  expect_equal(nrow(s$planted), 0L)
  expect_false(any(df$sbp == -99, na.rm = TRUE))
  expect_true(all(df$admit_date <= df$surgery_date & df$surgery_date <= df$discharge_date))
  expect_true(all(df$nyha %in% NYHA))
  expect_true(is.numeric(df$creatinine_umol))
  expect_false(any(duplicated(df)))
  expect_false(any(duplicated(df$patient_id)))
  expect_true(max(df$age) < 100 && min(df$age) >= 18)
  expect_true(max(df$surgery_date) < as.Date("2030-01-01"))
  expect_true(all(df$statin_start_date[df$statin == 1] <= df$surgery_date[df$statin == 1]))
  expect_true(all(is.na(df$statin_start_date[df$statin == 0])))
  expect_true(max(df$glucose_mmol) < 20)
  expect_true(max(df$weight_kg) < 160)
  expect_true(all(abs(df$weight_kg / ((df$height_cm / 100)^2 * df$bmi) - 1) < 0.02))
  expect_true(min(df$creatinine_umol) > 20)
  # not near-constant, not high cardinality, not collinear, not leaking
  expect_lt(max(table(df$const_flag)) / nrow(df), 0.9)
  expect_lt(length(unique(df$notes)) / nrow(df), 0.1)
  expect_lt(abs(cor(df$bmi, df$bmi_copy)), 0.5)
  expect_false(any(df$lab_status == "deceased"))
  expect_true(any(df$mortality_30d == 1) && any(df$mortality_30d == 0))
  # no heaping: the share of sbp values ending in 0 stays near the natural 10%
  expect_lt(mean(df$sbp %% 10 == 0), 0.2)
})

test_that("the clean twin is the defect cohort before planting (same base draws)", {
  a <- sr_sample_cardiac(n = 600L, seed = 3L, defects = FALSE)$data
  s <- sr_sample_cardiac(n = 600L, seed = 3L, defects = TRUE)
  b <- s$data
  hint <- s$planted$rows_hint[s$planted$check == "dup_rows"]
  dup <- as.integer(strsplit(sub("^.*rows ", "", hint), ",", fixed = TRUE)[[1]])
  expect_length(dup, 5L)
  # every column no defect touches is identical, except on the 5 rows overwritten by duplicates
  untouched <- setdiff(names(a), s$planted$column)
  expect_gt(length(untouched), 10L)
  for (col in untouched) expect_identical(a[-dup, col], b[-dup, col], label = col)
  # a touched column differs only where a defect was planted (spot check: age, 3 rows + dups)
  expect_lte(sum(a$age != b$age), 8L)
})

test_that("the clean twin stays within its own dictionary on several seeds", {
  for (sd in 1:12) {
    s <- sr_sample_cardiac(n = 2000L, seed = sd, defects = FALSE)
    # BMI is bounded by the generator, so the dictionary minimum is a sensible clinical one
    expect_gte(as.numeric(s$dict$min[s$dict$variable == "bmi"]), 15)
    expect_gte(as.numeric(s$dict$min[s$dict$variable == "bmi_copy"]), 15)
    expect_gte(min(s$data$bmi), 15)
    for (k in seq_len(nrow(s$dict))) {
      v <- s$dict$variable[k]
      x <- s$data[[v]]
      type <- s$dict$type[k]
      lab <- paste0(v, " (seed ", sd, ")")
      if (type %in% c("numeric", "date") && nzchar(s$dict$min[k])) {
        conv <- if (type == "date") as.Date else as.numeric
        expect_true(all(x >= conv(s$dict$min[k]) & x <= conv(s$dict$max[k]), na.rm = TRUE), label = lab)
      }
      if (type %in% c("categorical", "ordinal", "binary")) {
        expect_true(all(as.character(x) %in% strsplit(s$dict$levels[k], "|", fixed = TRUE)[[1]]),
                    label = lab)
      }
    }
  }
})

test_that("the clean twin has no outcome-separating column other than the planted leak", {
  for (defects in c(FALSE, TRUE)) {
    df <- sr_sample_cardiac(n = 2000L, seed = 1L, defects = defects)$data
    expect_false(identical(as.integer(!is.na(df$death_date)), as.integer(df$mortality_30d)))
    expect_false(identical(as.integer(df$followup_days <= 30), as.integer(df$mortality_30d)))
    # some survivors die later (death after day 30), some survivors are censored before day 30
    late <- !is.na(df$death_date) & df$mortality_30d == 0
    expect_gt(sum(late), 20L)
    expect_true(all(as.integer(df$death_date[late] - df$surgery_date[late]) > 30L))
    cens <- df$followup_days < 30 & df$mortality_30d == 0
    expect_gt(sum(cens), 20L)
    expect_true(all(df$followup_days[df$mortality_30d == 1] <= 30L))
  }
})

test_that("planted lists one row per defect with the specified checks", {
  p <- sr_sample_cardiac(n = 1000L)$planted
  expect_equal(nrow(p), 17L)
  expect_setequal(p$check, c("coercion", "dict_levels", "missing_codes", "range", "unit_mix",
                             "heaping", "date_order", "future_date", "immortal_time", "dup_rows",
                             "dup_id", "nzv", "high_card", "collinear", "leakage", "outlier"))
  expect_equal(sum(p$check == "unit_mix"), 2L)
  expect_true(all(nzchar(p$rows_hint)))
  expect_identical(p$column[p$check == "dup_rows"], "*")
})

test_that("each planted defect is verifiable from the data", {
  n <- 2000L
  s <- sr_sample_cardiac(n = n)
  df <- s$data
  # coercion: 0.5% "pending", so the column is text
  expect_type(df$creatinine_umol, "character")
  expect_equal(sum(df$creatinine_umol == "pending"), 10L)
  # dict_levels: 1% "V"
  expect_equal(sum(df$nyha == "V"), 20L)
  expect_true(all(df$nyha %in% c(NYHA, "V")))
  # missing_codes: 2% of sbp are -99
  expect_equal(sum(df$sbp == -99), 40L)
  # range: three rows at age 250
  expect_equal(sum(df$age == 250), 3L)
  # unit_mix: lb and mg/dL rows are separable
  cr <- suppressWarnings(as.numeric(df$creatinine_umol))
  expect_equal(sum(cr < 10, na.rm = TRUE), 60L)
  expect_gt(min(cr[cr >= 10], na.rm = TRUE), 20)
  ratio <- df$weight_kg / ((df$height_cm / 100)^2 * df$bmi)  # ~1 in kg, ~2.2 in lb
  expect_equal(sum(ratio > 1.8), 40L)
  # heaping: >50% of positive sbp are multiples of 10
  ok <- df$sbp > 0
  expect_gt(mean(df$sbp[ok] %% 10 == 0), 0.5)
  # date_order: 1% of rows discharged before admission
  expect_equal(sum(df$discharge_date < df$admit_date), 20L)
  # future_date: two surgeries in 2099
  expect_equal(sum(format(df$surgery_date, "%Y") == "2099"), 2L)
  # immortal_time: 40% of exposed rows start the statin after surgery
  ex <- df$statin == 1
  expect_equal(sum(df$statin_start_date[ex] > df$surgery_date[ex]), round(0.4 * sum(ex)))
  # dup_rows: exactly 5 exact duplicates
  expect_equal(sum(duplicated(df)), 5L)
  # dup_id: 4 repeated ids whose rows differ in data
  u <- unique(df)
  expect_equal(sum(duplicated(u$patient_id)), 4L)
  rep_ids <- u$patient_id[duplicated(u$patient_id)]
  for (id in rep_ids) expect_equal(nrow(unique(u[u$patient_id == id, ])), sum(u$patient_id == id))
  # nzv: 99.8% one value
  expect_gte(max(table(df$const_flag)) / n, 0.998)
  # high_card: free text, > 50% unique
  expect_gt(length(unique(df$notes)) / n, 0.5)
  # collinear: bmi_copy is bmi plus tiny noise
  expect_gt(cor(df$bmi, df$bmi_copy), 0.9999)
  # leakage: deceased iff mortality_30d == 1
  expect_true(all(df$lab_status[df$mortality_30d == 1] == "deceased"))
  expect_false(any(df$lab_status[df$mortality_30d == 0] == "deceased"))
  # outlier: five glucose values at 60
  expect_equal(sum(df$glucose_mmol == 60), 5L)
})

test_that("planted defects keep their counts on other seeds and sizes (disjoint row sets)", {
  s <- sr_sample_cardiac(n = 2000L, seed = 11L)$data
  expect_equal(sum(s$age == 250), 3L)
  expect_equal(sum(duplicated(s)), 5L)
  expect_equal(sum(s$sbp == -99), 40L)
  expect_equal(sum(duplicated(unique(s)$patient_id)), 4L)
  s2 <- sr_sample_cardiac(n = 400L, seed = 5L)$data
  expect_equal(sum(s2$age == 250), 3L)
  expect_equal(sum(duplicated(s2)), 5L)
  expect_equal(sum(s2$glucose_mmol == 60), 5L)
  expect_equal(sum(duplicated(unique(s2)$patient_id)), 4L)
  expect_equal(sum(s2$sbp == -99), 8L)
})

test_that("mortality rises with age, creatinine and NYHA class", {
  df <- sr_sample_cardiac(n = 4000L, seed = 2L, defects = FALSE)$data
  fit <- stats::glm(mortality_30d ~ age + creatinine_umol + nyha, data = df, family = binomial())
  expect_gt(stats::coef(fit)[["age"]], 0)
  expect_gt(stats::coef(fit)[["creatinine_umol"]], 0)
  expect_gt(stats::coef(fit)[["nyhaIV"]], 0)
  expect_gt(mean(df$mortality_30d), 0.02)
  expect_lt(mean(df$mortality_30d), 0.12)
})

test_that("no death date precedes surgery or the statin start, with or without planted defects", {
  for (sd in 1:12) for (defects in c(FALSE, TRUE)) {
    df <- sr_sample_cardiac(n = 2000L, seed = sd, defects = defects)$data
    lab <- paste0("seed ", sd, " defects ", defects)
    expect_true(all(df$death_date > df$surgery_date, na.rm = TRUE), label = lab)
    expect_true(all(df$statin_start_date <= df$death_date, na.rm = TRUE), label = lab)
    expect_true(all(df$death_date <= df$followup_days + df$surgery_date, na.rm = TRUE), label = lab)
    late <- !is.na(df$death_date) & df$mortality_30d == 0
    expect_true(all(as.integer(df$death_date[late] - df$surgery_date[late]) > 30L), label = lab)
  }
})

test_that(".fx_write_csv round-trips non-ASCII text as UTF-8", {
  d <- withr::local_tempdir()
  mu <- intToUtf8(181L)
  df <- data.frame(x = c(paste0(mu, "mol/L"), "plain"), y = 1:2, stringsAsFactors = FALSE)
  path <- file.path(d, "u.csv")
  .fx_write_csv(df, path)
  back <- utils::read.csv(path, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  expect_identical(back$x, df$x)
  bytes <- readBin(path, "raw", file.size(path))
  expect_true(as.raw(0xC2) %in% bytes)
})

test_that(".fx_project returns an open writable project that is cleaned up", {
  f <- function() {
    p <- .fx_project()
    expect_false(isTRUE(p$readonly))
    expect_true(dir.exists(p$paths$data))
    p$paths$dir
  }
  d <- f()
  expect_false(dir.exists(d))
})

test_that(".fx_cardiac_project returns a project and skips ingest until sr_ingest exists", {
  p <- .fx_cardiac_project(n = 150L, defects = FALSE)
  expect_false(isTRUE(p$readonly))
  if (!exists("sr_ingest")) expect_null(p$state$tables$cardiac)
})
