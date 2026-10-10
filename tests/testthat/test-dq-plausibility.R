# Tests for profile/dq_plausibility.R: the plausibility template and the eight plausibility checks
# (range, unit_mix, heaping, outlier, future_date, old_date, date_order, immortal_time). Synthetic data
# only; everything lives under withr::local_tempdir().

.pl_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

.pl_checks <- c("range", "unit_mix", "heaping", "outlier", "future_date", "old_date", "date_order",
                "immortal_time")

# The roles of the cardiac study: surgery is time zero, the statin start is the exposure date.
.pl_roles <- c(patient_id = "id", surgery_date = "time_zero", statin_start_date = "exposure_date",
               statin = "exposure", death_date = "event_date")

# One dictionary row per variable, numeric by default.
.pl_dict <- function(variable, type = "numeric", min = "", max = "") {
  d <- data.frame(variable = variable, label = variable, type = type, units = "", min = min, max = max,
                  stringsAsFactors = FALSE)
  d$levels <- rep(list(data.frame(code = character(), label = character(), stringsAsFactors = FALSE)),
                  length(variable))
  d$missing_codes <- rep(list(character()), length(variable))
  d$source <- "test"
  d
}

# The cardiac project, optionally with its dictionary and the study roles. A dictionary describes the
# intended data, so it is the same for the defect version and the clean twin.
.pl_cardiac <- function(defects = TRUE, n = 2000L, dict = TRUE, roles = .pl_roles, env = parent.frame()) {
  p <- .fx_cardiac_project(n = n, defects = defects, env = env)
  withr::defer(sr_duck_close(p), envir = env)
  if (dict) {
    csv <- file.path(dirname(p$paths$dir), "cardiac_dict.csv")
    .fx_write_csv(sr_sample_cardiac(n = n, defects = defects)$dict, csv)
    p <- sr_dict_set(p, "cardiac", sr_dict_read_csv(csv), "csv")
  }
  if (length(roles)) p <- sr_roles_set(p, "cardiac", roles)
  p
}

.pl_small <- function(df, table = "small", roles = NULL, env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = table, options = list(drive_info = .pl_drive))
  if (length(roles)) p <- sr_roles_set(p, table, roles)
  p
}

.pl_run <- function(p, table, checks = .pl_checks) {
  sr_dq_findings(sr_dq_run(p, table, checks = checks), table)
}

.pl_find <- function(f, check, column = NULL) {
  f[f$check == check & (is.null(column) | f$column %in% column), , drop = FALSE]
}

# The "earlier later" key of each date_order finding.
.pl_pair_keys <- function(f) vapply(f$detail, function(x) paste(x$pair$earlier, x$pair$later), "")

# ---- the template ----------------------------------------------------------------------------------

test_that("the plausibility template lists the eight analytes with affine alternative units", {
  tp <- .sr_pl_template()
  expect_identical(vapply(tp, function(a) a$analyte, ""),
                   c("age", "weight", "height", "bmi", "sbp", "creatinine", "glucose", "hba1c"))
  by <- stats::setNames(tp, vapply(tp, function(a) a$analyte, ""))
  expect_identical(by$age$hard, c(0, 120))
  expect_identical(by$age$soft, c(18, 100))
  expect_identical(by$creatinine$unit, "umol/L")
  expect_identical(by$creatinine$hard, c(10, 2000))
  expect_equal(by$creatinine$alt$factor, 1 / 88.4, tolerance = 1e-12)
  expect_identical(by$creatinine$alt$offset, 0)
  expect_identical(by$weight$alt$unit, "lb")
  expect_identical(by$weight$alt$factor, 2.20462)
  expect_identical(by$height$alt$factor, 0.01)
  expect_identical(by$glucose$alt$factor, 18.016)
  # hba1c in mmol/mol: ifcc = 10.929 * (ngsp - 2.15), affine
  expect_identical(by$hba1c$alt$unit, "mmol/mol")
  expect_identical(by$hba1c$alt$factor, 10.929)
  expect_equal(by$hba1c$alt$offset, -10.929 * 2.15, tolerance = 1e-12)
  expect_identical(by$bmi$name_regex, "bmi")
  expect_identical(nrow(by$bmi$alt), 0L)
})

test_that("analyte names are token-anchored: abbreviations never match inside other words", {
  an <- function(nm) vapply(nm, function(n) { a <- .sr_pl_analyte(n); if (is.null(a)) NA_character_ else a$analyte }, "")
  pos <- c(weight_kg = "weight", Weight = "weight", wt = "weight", wt_kg = "weight", birth_weight = "weight",
           height_cm = "height", ht = "height", ht_cm = "height", age = "age", age_years = "age",
           creatinine_umol = "creatinine", creat = "creatinine", creat_peak = "creatinine",
           glucose_mmol = "glucose", gluc_fasting = "glucose", sbp = "sbp", sbp_right_arm = "sbp",
           systolic_bp = "sbp", systolic = "sbp", hba1c = "hba1c", a1c = "hba1c", bmi = "bmi")
  expect_identical(unname(an(names(pos))), unname(pos))
  neg <- c("right_atrial_pressure", "right_ventricle_size", "light_score", "night_shift", "fwt", "bwt_g",
           "creatine_kinase", "glucagon", "pulmonary_systolic_pressure", "agent_code", "page_count", "wtf",
           "weighted_score")
  expect_true(all(is.na(an(neg))))
})

test_that("the eight checks are registered after the nine Task 13 checks, in a fixed order", {
  reg <- sr_dq_checks()
  # ordered containment: later tasks register more checks after these
  expect_identical(reg[reg %in% .pl_checks], .pl_checks)
  expect_gt(min(match(.pl_checks, reg)), match("missing_dependence", reg))
  expect_identical(utils::head(reg, 9L)[c(1L, 9L)], c("coercion", "missing_dependence"))
})

# ---- range -----------------------------------------------------------------------------------------

test_that("range flags the three impossible ages as a block, with a filter that selects them", {
  p <- .pl_cardiac()
  all <- .pl_run(p, "cardiac", "range")
  f <- .pl_find(all, "range", "age")
  expect_identical(nrow(f), 1L)
  expect_identical(f$severity, "block")
  expect_identical(f$domain, "plausibility")
  expect_identical(f$n_affected, 3L)
  expect_equal(f$p_affected, 3 / 2000)
  expect_true("250" %in% f$examples[[1L]])
  expect_identical(f$where[[1L]], list(any = list(list(col = "age", op = "<", value = 18),
                                                  list(col = "age", op = ">", value = 110))))   # the dictionary range
  expect_identical(f$detail[[1L]]$limits$column, "age")
  # the missing-value code -99 is also an impossible sbp; the text column is read through TRY_CAST
  sbp <- .pl_find(all, "range", "sbp")
  expect_identical(sbp$severity, "block")
  expect_identical(sbp$n_affected, 40L)
  # the planted mg/dL creatinines (0.5 to 3.4) are impossible umol/L values, so they block too
  expect_identical(.pl_find(all, "range", "creatinine_umol")$n_affected, 60L)
})

test_that("a hard-range value blocks, a soft-range one is info, and the two never overlap", {
  set.seed(21)
  df <- data.frame(weight_kg = c(round(stats::rnorm(998, 80, 10), 1), 25, 400),
                   age = c(round(stats::runif(998, 40, 80)), 10, 110))
  p <- .pl_small(df)
  f <- .pl_run(p, "small", "range")
  w <- f[f$column == "weight_kg", ]
  expect_setequal(w$severity, c("block", "info"))
  expect_identical(w$n_affected[w$severity == "block"], 1L)       # 400 > 300
  expect_identical(w$n_affected[w$severity == "info"], 1L)        # 25 is inside [20, 300], outside [35, 200]
  wi <- w$where[[which(w$severity == "info")]]
  expect_identical(wi$any[[1L]]$all[[1L]], list(col = "weight_kg", op = ">=", value = 20))
  expect_identical(wi$any[[1L]]$all[[2L]], list(col = "weight_kg", op = "<", value = 35))
  a <- f[f$column == "age", ]
  expect_identical(a$severity, "info")                              # 10 and 110 are inside [0, 120]
  expect_identical(a$n_affected, 2L)
  expect_identical(a$where[[1L]]$any[[2L]]$all[[1L]], list(col = "age", op = ">", value = 100))
})

test_that("a dictionary min / max replaces both the hard and the soft range, with no info tier", {
  df <- data.frame(age = c(rep(50, 300), 112, 15))
  p <- .pl_small(df)
  # no dictionary: 112 and 15 are inside the hard range [0, 120] and outside the soft one
  f0 <- .pl_run(p, "small", "range")
  expect_identical(f0$severity, "info")
  expect_identical(f0$n_affected, 2L)
  p1 <- sr_dict_set(p, "small", .pl_dict("age", min = "18", max = "110"), "manual")
  f1 <- .pl_run(p1, "small", "range")
  expect_identical(nrow(f1), 1L)
  expect_identical(f1$severity, "block")
  expect_identical(f1$n_affected, 2L)
  expect_identical(f1$detail[[1L]]$limits$source, "dictionary")
  # a dictionary with only a minimum replaces that side alone
  p2 <- sr_dict_set(p, "small", .pl_dict("age", min = "16"), "manual")
  f2 <- .pl_run(p2, "small", "range")
  expect_setequal(f2$severity, c("block", "info"))
  expect_identical(f2$n_affected[f2$severity == "block"], 1L)       # 15 < 16
  expect_identical(f2$n_affected[f2$severity == "info"], 1L)        # 112 > soft 100, inside hard 120
})

# ---- unit_mix --------------------------------------------------------------------------------------

test_that("unit_mix finds the pounds in weight_kg and the mg/dL in creatinine_umol", {
  p <- .pl_cardiac()
  d <- sr_sample_cardiac(n = 2000L)$data
  clean <- sr_sample_cardiac(n = 2000L, defects = FALSE)$data
  f <- .pl_run(p, "cardiac", "unit_mix")
  # the five glucose values of 60 also read as 60 mg/dL (5/2000 is above the 0.155% share): unit_mix reports
  # them next to outlier, which still flags them
  expect_setequal(f$column, c("weight_kg", "creatinine_umol", "glucose_mmol"))
  expect_true("glucose_mmol" %in% .pl_run(p, "cardiac", "outlier")$column)
  expect_true(all(f$severity == "warn" & f$domain == "plausibility"))
  # 40 weights were multiplied by 2.2046; only those above the soft maximum (200) are detectable, the
  # rest sit inside the plausible kg range. The clean twin has no weight above 140.
  expect_lt(max(clean$weight_kg), 200)
  detectable <- sum(d$weight_kg > 200)
  expect_gte(detectable, 3L)
  w <- f[f$column == "weight_kg", ]
  expect_lte(abs(w$n_affected - detectable), 0.2 * detectable)
  expect_identical(w$n_affected, as.integer(detectable))
  expect_identical(w$detail[[1L]]$units$alt_unit, "lb")
  expect_identical(w$detail[[1L]]$units$column, "weight_kg")
  # 60 creatinine values are in mg/dL (0.5 to 3.4), all outside [30, 1000]
  cr <- f[f$column == "creatinine_umol", ]
  expect_lte(abs(cr$n_affected - 60), 0.2 * 60)
  expect_identical(cr$detail[[1L]]$units$alt_unit, "mg/dL")
  expect_true(all(as.numeric(cr$examples[[1L]]) < 4))
  # the filter selects the alt-unit rows: inside the converted range and outside the primary soft one
  expect_identical(cr$where[[1L]]$all[[1L]]$op, "between")
  expect_identical(cr$where[[1L]]$all[[1L]]$col, "creatinine_umol")
  expect_equal(cr$where[[1L]]$all[[1L]]$value, c(30, 1000) / 88.4, tolerance = 1e-10)
  expect_identical(cr$where[[1L]]$all[[2L]]$any[[1L]], list(col = "creatinine_umol", op = "<", value = 30))
})

test_that("unit_mix needs 3 values and a quarter of a percent, and ignores values inside the primary range", {
  set.seed(31)
  mk <- function(n, alt) {
    w <- pmin(pmax(round(stats::rnorm(n, 80, 10), 1), 50), 120)
    w[seq_along(alt)] <- alt
    data.frame(weight_kg = w, height_cm = pmin(pmax(round(stats::rnorm(n, 170, 8)), 150), 190))
  }
  expect_identical(nrow(.pl_run(.pl_small(mk(1000, c(250, 300))), "small", "unit_mix")), 0L)        # 2 values
  expect_identical(nrow(.pl_run(.pl_small(mk(2000, c(250, 300, 350))), "small", "unit_mix")), 0L)    # 0.15%
  f <- .pl_run(.pl_small(mk(1000, c(250, 300, 350))), "small", "unit_mix")                            # 0.3%
  expect_identical(f$n_affected, 3L)
  # 150 lb is 68 kg: inside the plausible range, so it is not an alt-unit value
  expect_identical(nrow(.pl_run(.pl_small(mk(1000, rep(150, 50))), "small", "unit_mix")), 0L)
  # the metre in a height_cm column
  h <- data.frame(height_cm = c(round(stats::rnorm(990, 170, 8)), rep(1.7, 10)))
  fh <- .pl_run(.pl_small(h), "small", "unit_mix")
  expect_identical(fh$n_affected, 10L)
  expect_identical(fh$detail[[1L]]$units$alt_unit, "m")
  # hba1c in mmol/mol (an affine conversion with an offset): 53 mmol/mol is 7%
  hb <- data.frame(hba1c_pct = c(round(stats::rnorm(990, 6, 1), 1), rep(53, 10)))
  fb <- .pl_run(.pl_small(hb), "small", "unit_mix")
  expect_identical(fb$n_affected, 10L)
  expect_identical(fb$detail[[1L]]$units$alt_unit, "mmol/mol")
})

# ---- heaping ---------------------------------------------------------------------------------------

test_that("heaping flags sbp, the column whose values were rounded to tens, and nothing else", {
  p <- .pl_cardiac()
  f <- .pl_run(p, "cardiac", "heaping")
  expect_identical(f$column, "sbp")
  expect_identical(f$severity, "warn")
  expect_identical(f$domain, "plausibility")
  h <- f$detail[[1L]]$heaping
  expect_gt(h$observed, 0.6)
  expect_identical(h$expected, 0.2)
  expect_identical(h$column, "sbp")
  expect_null(f$where[[1L]])
})

test_that("heaping needs 200 integer values, 20 distinct ones, and an observed share of 0.40", {
  x <- rep(seq(10, 200, by = 5), length.out = 400)             # 39 distinct, all multiples of 5
  expect_identical(nrow(.pl_run(.pl_small(data.frame(x = x)), "small", "heaping")), 1L)
  expect_identical(nrow(.pl_run(.pl_small(data.frame(x = x[1:199])), "small", "heaping")), 0L)    # < 200 values
  few <- rep(c(10, 15, 20, 25, 30), length.out = 400)           # 5 distinct values: a short scale
  expect_identical(nrow(.pl_run(.pl_small(data.frame(x = few)), "small", "heaping")), 0L)
  dec <- rep(seq(10, 200, by = 5), length.out = 400) + 0.5       # not integer valued
  expect_identical(nrow(.pl_run(.pl_small(data.frame(x = dec)), "small", "heaping")), 0L)
  set.seed(11)
  below <- c(sample(c(0, 5), 150, replace = TRUE) + 10 * sample(1:20, 150, replace = TRUE),
             sample(c(1:4, 6:9), 250, replace = TRUE) + 10 * sample(1:20, 250, replace = TRUE))
  expect_identical(nrow(.pl_run(.pl_small(data.frame(x = below)), "small", "heaping")), 0L)   # 37.5%
  # an id column is not a measurement
  ids <- data.frame(pid = seq(100, 4095, by = 5))
  p <- .pl_small(ids, roles = c(pid = "id"))
  expect_identical(nrow(.pl_run(p, "small", "heaping")), 0L)
})

# ---- outlier ---------------------------------------------------------------------------------------

test_that("outlier flags the five glucose values of 60 mmol/L and only reports, never deletes", {
  p <- .pl_cardiac()
  f <- .pl_run(p, "cardiac", "outlier")
  g <- f[f$column == "glucose_mmol", ]
  expect_identical(nrow(g), 1L)
  expect_identical(g$severity, "info")
  expect_identical(g$n_affected, 5L)
  expect_true(60 %in% as.numeric(g$examples[[1L]]))
  expect_gt(g$detail[[1L]]$robust$z_cut, 5)
  expect_true(all(f$severity == "info"))
})

test_that("outlier needs both a robust z above 6 and a value outside the plausible range", {
  set.seed(5)
  sbp <- round(stats::rnorm(500, 130, 15))
  sbp[1:2] <- c(250, 290)     # 250 is a robust outlier but inside the soft range [70, 250]; 290 is outside it
  f <- .pl_run(.pl_small(data.frame(sbp = sbp)), "small", "outlier")
  expect_identical(f$n_affected, 1L)
  expect_true("290" %in% f$examples[[1L]])
  expect_false("250" %in% f$examples[[1L]])
  # a wide column with no outlier, and a constant column (MAD = 0), give nothing
  wide <- data.frame(sbp = round(stats::runif(500, 90, 200)))
  expect_identical(nrow(.pl_run(.pl_small(wide), "small", "outlier")), 0L)
  const <- data.frame(sbp = c(rep(120, 400), 280))
  expect_identical(nrow(.pl_run(.pl_small(const), "small", "outlier")), 0L)
})

# ---- dates -----------------------------------------------------------------------------------------

test_that("future_date blocks the two surgery dates in 2099", {
  withr::local_options(sr.dq_as_of = "2026-10-04")
  p <- .pl_cardiac()
  f <- .pl_run(p, "cardiac", "future_date")
  expect_identical(f$column, "surgery_date")
  expect_identical(f$severity, "block")
  expect_identical(f$n_affected, 2L)
  expect_true(all(grepl("^2099-", f$examples[[1L]])))
  as_of <- f$detail[[1L]]$as_of
  expect_identical(as_of$column, "surgery_date")
  expect_identical(as_of$as_of, "2026-10-04")
  # ">= run date + 2" selects the same rows as the counted "date > run date + 1", for dates and timestamps
  expect_identical(f$where[[1L]], list(col = "surgery_date", op = ">=", value = "2026-10-06"))
})

test_that("a future_date id does not depend on the run date", {
  p <- .pl_cardiac()
  withr::local_options(sr.dq_as_of = "2026-10-04")
  a <- .pl_run(p, "cardiac", "future_date")
  withr::local_options(sr.dq_as_of = "2027-03-01")
  b <- .pl_run(p, "cardiac", "future_date")
  expect_identical(a$id, b$id)
  expect_false(identical(a$where, b$where))                       # the literal cutoff still differs
  expect_identical(b$detail[[1L]]$as_of$as_of, "2027-03-01")
})

test_that("future_date compares against the run date plus one day; old_date against 1900-01-01", {
  withr::local_options(sr.dq_as_of = "2020-06-15")
  df <- data.frame(d = c("2020-06-15", "2020-06-16", "2020-06-17", "2021-01-01", "1900-01-01", "1899-12-31"),
                   t = c("2020-06-16 10:00:00", "2020-06-17 10:00:00", "2019-01-01 00:00:00", "2019-01-01 00:00:00",
                         "2019-01-01 00:00:00", "2019-01-01 00:00:00"), stringsAsFactors = FALSE)
  p <- .pl_small(df)
  f <- .pl_run(p, "small", c("future_date", "old_date"))
  fd <- .pl_find(f, "future_date")
  expect_identical(fd$column, c("d", "t"))
  expect_identical(fd$n_affected, c(2L, 1L))                      # 06-17 and 2021 / the 06-17 timestamp
  expect_true(all(fd$severity == "block"))
  expect_identical(fd$detail[[1L]]$as_of$as_of, "2020-06-15")
  expect_identical(fd$where[[1L]], list(col = "d", op = ">=", value = "2020-06-17"))
  expect_identical(fd$where[[2L]], list(col = "t", op = ">=", value = "2020-06-17"))   # the 06-16 10:00 row is not counted
  od <- .pl_find(f, "old_date")
  expect_identical(od$column, "d")
  expect_identical(od$n_affected, 1L)                             # 1899-12-31 only; 1900-01-01 is the limit
  expect_identical(od$severity, "warn")
  expect_identical(od$where[[1L]], list(col = "d", op = "<", value = "1900-01-01"))
})

test_that("date_order flags discharge before admission (and surgery), with one finding per pair", {
  p <- .pl_cardiac()
  d <- sr_sample_cardiac(n = 2000L)$data
  f <- .pl_run(p, "cardiac", "date_order")
  expect_true(all(f$severity == "warn" & f$domain == "plausibility"))
  expect_true("discharge_date" %in% f$column)
  expect_identical(anyDuplicated(f$id), 0L)
  key <- .pl_pair_keys(f)
  expect_true(all(c("admit_date discharge_date", "surgery_date discharge_date") %in% key))
  n_ad <- sum(d$discharge_date < d$admit_date, na.rm = TRUE)
  expect_gte(n_ad, 20L)
  expect_identical(f$n_affected[key == "admit_date discharge_date"], as.integer(n_ad))
  expect_identical(f$column[key == "admit_date discharge_date"], "discharge_date")
  # every other pair is in order: death after surgery, surgery after admission
  expect_false(any(key %in% c("surgery_date death_date", "admit_date surgery_date")))
})

test_that("date_order pairs by role, then by name; equal dates are in order; birth precedes every date", {
  df <- data.frame(
    dob = as.character(as.Date("1950-01-01") + c(0, 0, 0, 26000, 0)),
    op_start = as.character(as.Date("2020-01-01") + 0:4),
    died = as.character(as.Date("2020-01-01") + c(0, 1, 2, -3, 4)),     # row 1 equal, row 4 before
    censor = as.character(as.Date("2020-01-01") + c(5, 5, 5, 5, -1)),
    admit_date = as.character(as.Date("2020-01-01") + 0:4),
    discharge_date = as.character(as.Date("2020-01-01") + 0:4),         # equal to admission: in order
    stringsAsFactors = FALSE)
  p <- .pl_small(df, roles = c(op_start = "time_zero", died = "event_date", censor = "censor_date"))
  f <- .pl_run(p, "small", "date_order")
  key <- .pl_pair_keys(f)
  expect_setequal(key, c("op_start died", "op_start censor", "dob op_start", "dob died", "dob censor",
                         "dob admit_date", "dob discharge_date"))
  expect_identical(f$n_affected[key == "op_start died"], 1L)
  expect_identical(f$n_affected[key == "op_start censor"], 1L)
  expect_identical(f$n_affected[key == "dob admit_date"], 1L)      # the 2021 birth date
  expect_false("admit_date discharge_date" %in% key)
  # without a time_zero role no pair is guessed
  p2 <- .pl_small(df[, c("op_start", "died", "censor")], table = "nozero")
  expect_identical(nrow(.pl_run(p2, "nozero", "date_order")), 0L)
})

test_that("immortal_time flags the statin start dates that fall after surgery", {
  p <- .pl_cardiac()
  d <- sr_sample_cardiac(n = 2000L)$data
  f <- .pl_run(p, "cardiac", "immortal_time")
  expect_identical(f$column, "statin_start_date")
  expect_identical(f$severity, "warn")
  expect_identical(f$domain, "plausibility")
  after <- sum(d$statin_start_date > d$surgery_date, na.rm = TRUE)
  expect_identical(f$n_affected, as.integer(after))
  exposed <- sum(!is.na(d$statin_start_date) & !is.na(d$surgery_date))
  im <- f$detail[[1L]]$immortal
  expect_identical(im$n_exposed, as.numeric(exposed))
  expect_identical(im$time_zero, "surgery_date")
  expect_gt(im$p_after, 0.3)
  expect_match(f$message, "time-varying exposure")
  expect_match(f$message, "landmark")
  expect_null(f$where[[1L]])
})

test_that("immortal_time needs both roles and more than 1% of the exposed rows", {
  mk <- function(late) {
    z <- as.Date("2020-01-01") + 0:399
    e <- z - 30
    e[seq_len(late)] <- z[seq_len(late)] + 10
    e[301:400] <- NA                                    # 100 unexposed rows; 300 exposed
    data.frame(tz = as.character(z), ex = as.character(e), stringsAsFactors = FALSE)
  }
  roles <- c(tz = "time_zero", ex = "exposure_date")
  expect_identical(nrow(.pl_run(.pl_small(mk(3), roles = roles), "small", "immortal_time")), 0L)      # 3 / 300 = 1%
  expect_identical(nrow(.pl_run(.pl_small(mk(4), roles = roles), "small", "immortal_time")), 1L)      # 1.3%
  expect_identical(nrow(.pl_run(.pl_small(mk(0), roles = roles), "small", "immortal_time")), 0L)
  expect_identical(nrow(.pl_run(.pl_small(mk(30)), "small", "immortal_time")), 0L)                    # no roles
  expect_identical(nrow(.pl_run(.pl_small(mk(30), roles = c(tz = "time_zero")), "small", "immortal_time")), 0L)
})

# ---- the outcome-blind rule ------------------------------------------------------------------------

test_that("a blind run of the plausibility checks passes the gate and gives the outcome only its marginals", {
  roles <- c(.pl_roles, mortality_30d = "outcome")
  p <- .pl_cardiac(roles = roles)
  expect_true(sr_blind(p))
  f <- .pl_run(p, "cardiac", .pl_checks)
  expect_identical(nrow(f[f$check == "check_error", ]), 0L)
  expect_silent(.sr_assert_blind(f, "mortality_30d",
                                 other_cols = setdiff(names(sr_roles(p, "cardiac")), "mortality_30d")))
  expect_false(any(grepl("mortality_30d", f$message)))
  # the same findings as without an outcome role: nothing about the outcome crosses into them
  p0 <- .pl_cardiac()
  f0 <- .pl_run(p0, "cardiac", .pl_checks)
  expect_identical(f$id, f0$id)
  expect_identical(f$n_affected, f0$n_affected)
})

test_that("while blind an outcome column gets a range check but no date_order or outlier", {
  set.seed(8)
  df <- data.frame(
    glucose = c(round(stats::rnorm(298, 6, 1), 1), 80, 90),                     # the outcome, two impossible values
    admit = as.character(as.Date("2020-01-01") + 0:299),
    outcome_date = as.character(as.Date("2020-01-01") + c(-5, -5, 1:298)),    # two before admission
    stringsAsFactors = FALSE)
  p <- .pl_small(df, roles = c(glucose = "outcome", outcome_date = "event_date", admit = "time_zero"))
  f <- .pl_run(p, "small", .pl_checks)
  expect_identical(nrow(f[f$check == "check_error", ]), 0L)
  r <- .pl_find(f, "range", "glucose")
  expect_identical(r$severity, "block")                                          # 80 and 90 are above the hard maximum 50
  expect_identical(r$n_affected, 2L)
  expect_identical(nrow(.pl_find(f, "outlier", "glucose")), 0L)
  # a date_order pair is an ordinary relation, so it is reported when neither column is the outcome ...
  expect_true(any(f$check == "date_order"))
  # ... and is skipped when one of them is
  p2 <- .pl_small(df, table = "small2", roles = c(outcome_date = "outcome", admit = "time_zero"))
  f2 <- .pl_run(p2, "small2", .pl_checks)
  expect_identical(nrow(f2[f2$check == "date_order", ]), 0L)
  expect_identical(nrow(f2[f2$check == "check_error", ]), 0L)
})

# ---- the clean twin --------------------------------------------------------------------------------

test_that("on the clean twin no plausibility check reaches warn, at 2,000 and at 50,000 rows", {
  # with the dictionary (its min / max replace the template ranges) and, at 2,000 rows, without it
  for (cfg in list(list(n = 2000L, dict = TRUE), list(n = 50000L, dict = TRUE), list(n = 2000L, dict = FALSE))) {
    p <- .pl_cardiac(defects = FALSE, n = cfg$n, dict = cfg$dict)
    f <- .pl_run(p, "cardiac", .pl_checks)
    lab <- paste0("n=", cfg$n, " dict=", cfg$dict)
    expect_identical(nrow(f[f$check == "check_error", ]), 0L, info = lab)
    expect_identical(nrow(f[f$severity != "info", ]), 0L, info = lab)
    expect_identical(nrow(f), 0L, info = lab)
  }
})

test_that("on the defect cohort every plausibility check fires on its planted column", {
  p <- .pl_cardiac()
  p <- sr_dq_run(p, "cardiac", checks = .pl_checks)
  f <- sr_dq_findings(p, "cardiac")
  expect_identical(nrow(f[f$check == "check_error", ]), 0L)
  expect_true(all(f$domain == "plausibility"))
  expect_identical(anyDuplicated(f$id), 0L)
  expect_true(all(vapply(f$detail, function(d) all(vapply(d, function(x) "column" %in% names(x), TRUE)), TRUE)))
  expect_setequal(f$check, c("range", "unit_mix", "heaping", "outlier", "future_date", "date_order",
                             "immortal_time"))
  # the findings survive the stored JSON round trip exactly, and a second run gives the same ids
  f2 <- sr_dq_findings(sr_dq_run(p, "cardiac", checks = .pl_checks), "cardiac")
  expect_identical(f, f2)
})
