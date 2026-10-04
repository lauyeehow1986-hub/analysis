# Synthetic cardiac-surgery cohort with planted data-quality defects (and a clean twin).
#
#   sr_sample_cardiac(n = 2000L, seed = 1L, defects = TRUE)
#     -> list(data = data.frame, dict = data.frame, planted = data.frame(check, column, rows_hint))
#
# Base R only; synthetic data only. The clean twin (`defects = FALSE`) is the same generator with the
# same base draws and nothing planted: the defect step runs after the base cohort is complete, on
# its own random draws. Where a planted defect would interfere with another (an exact duplicate
# copying a row that carries a defect, a repeated id sitting on a row that is also a duplicate), the
# defects sit on disjoint row sets, so every planted count can be re-derived from the data alone.
#
# Choices that keep the clean twin genuinely clean (no check should fire on it):
#   * `const_flag` is a 60/40 two-level flag (the defect version makes it 99.8% one value);
#   * `notes` draws from 8 short phrases (the defect version makes them mostly unique);
#   * `bmi_copy` is an independent draw from the BMI distribution (the defect version is bmi + noise);
#   * `lab_status` is normal / abnormal / repeat, independent of mortality (the defect version
#     writes "deceased" iff mortality_30d == 1);
#   * `creatinine_umol` is numeric, `nyha` stays within I-IV, every date is ordered and in the past.
# `hba1c_pct` has ~3% genuinely missing values in both versions (ordinary missingness, not a defect).
# The dictionary describes the intended data, so it is identical for both versions.

SR_SAMPLE_NYHA <- c("I", "II", "III", "IV")
SR_SAMPLE_SITES <- paste0("SITE_", LETTERS[1:6])
SR_SAMPLE_NOTES <- c("Uneventful recovery", "Mild post-op AF, resolved", "Delayed chest closure",
                     "Re-exploration for bleeding", "Prolonged ventilation", "Wound check normal",
                     "Routine follow-up arranged", "Transfused 2 units")

.sr_sample_dict <- function() {
  row <- function(variable, label, type, units = "", min = "", max = "", levels = "") {
    data.frame(variable = variable, label = label, type = type, units = units, min = min, max = max,
               levels = levels, stringsAsFactors = FALSE)
  }
  d <- "2015-01-01"
  e <- "2025-12-31"
  do.call(rbind, list(
    row("patient_id", "Patient identifier", "id"),
    row("site", "Operating site", "categorical", levels = paste(SR_SAMPLE_SITES, collapse = "|")),
    row("age", "Age at surgery", "numeric", "years", "18", "110"),
    row("sex", "Sex", "categorical", levels = "F|M"),
    row("weight_kg", "Body weight", "numeric", "kg", "30", "250"),
    row("height_cm", "Body height", "numeric", "cm", "120", "220"),
    row("bmi", "Body mass index", "numeric", "kg/m2", "15", "70"),
    row("sbp", "Systolic blood pressure", "numeric", "mmHg", "60", "260"),
    row("creatinine_umol", "Serum creatinine", "numeric", "umol/L", "20", "1500"),
    row("hba1c_pct", "Glycated haemoglobin", "numeric", "%", "3", "18"),
    row("glucose_mmol", "Fasting glucose", "numeric", "mmol/L", "1.5", "40"),
    row("nyha", "NYHA class", "ordinal", levels = paste(SR_SAMPLE_NYHA, collapse = "|")),
    row("diabetes", "Diabetes mellitus", "binary", "", "0", "1", "0|1"),
    row("admit_date", "Admission date", "date", "", d, e),
    row("surgery_date", "Surgery date (time zero)", "date", "", d, e),
    row("discharge_date", "Discharge date", "date", "", d, e),
    row("death_date", "Date of death", "date", "", d, e),
    row("mortality_30d", "Death within 30 days of surgery", "binary", "", "0", "1", "0|1"),
    row("statin_start_date", "Statin start date", "date", "", d, e),
    row("statin", "Statin exposure", "binary", "", "0", "1", "0|1"),
    row("followup_days", "Follow-up from surgery", "numeric", "days", "0", "4000"),
    row("notes", "Free-text clinical note", "text"),
    row("const_flag", "Registry flag", "categorical", levels = "Y|N"),
    row("bmi_copy", "Second BMI measurement", "numeric", "kg/m2", "15", "70"),
    row("lab_status", "Laboratory status", "categorical",
        levels = "normal|abnormal|repeat|deceased")
  ))
}

.sr_sample_hint <- function(rows, text = NULL) {
  if (!is.null(text)) return(text)
  rows <- sort(rows)
  paste0("n=", length(rows), if (length(rows) <= 10L) paste0(" rows ", paste(rows, collapse = ",")) else "")
}

sr_sample_cardiac <- function(n = 2000L, seed = 1L, defects = TRUE) {
  n <- as.integer(n)
  stopifnot(length(n) == 1L, !is.na(n), n >= 100L, is.logical(defects), length(defects) == 1L)
  # hand the caller's RNG stream back untouched when there is one
  if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
    old_seed <- get(".Random.seed", envir = globalenv())
    on.exit(assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
  }
  set.seed(as.integer(seed))
  clamp <- function(x, lo, hi) pmin(pmax(x, lo), hi)

  # ---- base cohort (identical for the clean twin and the defect version) ----------------------
  patient_id <- sprintf("P%06d", seq_len(n))
  site <- sample(SR_SAMPLE_SITES, n, replace = TRUE, prob = c(.30, .20, .20, .15, .10, .05))
  sex <- sample(c("F", "M"), n, replace = TRUE, prob = c(.30, .70))
  age <- as.integer(clamp(round(stats::rnorm(n, 64, 11)), 30, 90))
  female <- sex == "F"
  height_cm <- clamp(round(stats::rnorm(n, ifelse(female, 158, 170), 6.5)), 140, 200)
  weight_kg <- clamp(round(stats::rnorm(n, ifelse(female, 63, 79), 11), 1), 38, 140)
  # bound BMI at 15 (consumes no RNG): raise a too-light weight to the BMI-15 weight, rounded up
  weight_kg <- pmax(weight_kg, ceiling(15 * (height_cm / 100)^2 * 10) / 10)
  bmi <- round(weight_kg / (height_cm / 100)^2, 1)
  nyha <- sample(SR_SAMPLE_NYHA, n, replace = TRUE, prob = c(.20, .40, .28, .12))
  nyha_num <- match(nyha, SR_SAMPLE_NYHA)
  diabetes <- stats::rbinom(n, 1L, stats::plogis(-1.2 + 0.02 * (age - 64) + 0.03 * (bmi - 27)))
  sbp <- as.integer(clamp(round(stats::rnorm(n, 128 + 0.2 * (age - 64), 17)), 85, 215))
  creatinine_umol <- clamp(round(stats::rnorm(
    n, 85 + 0.6 * (age - 64) + ifelse(female, 0, 6) + 4 * (nyha_num - 2), 20)), 45, 300)
  hba1c_pct <- clamp(round(ifelse(diabetes == 1L, stats::rnorm(n, 7.6, 1.1), stats::rnorm(n, 5.5, 0.4)), 1), 4, 13)
  hba1c_pct[sample.int(n, round(0.03 * n))] <- NA_real_
  glucose_mmol <- clamp(round(stats::rnorm(n, 5.7 + 2.3 * diabetes, 1.0), 1), 3.2, 15)

  lp <- -3.3 + 0.05 * (age - 64) + 0.015 * (creatinine_umol - 88) + 0.5 * (nyha_num - 2)
  mortality_30d <- stats::rbinom(n, 1L, stats::plogis(lp))
  dead <- mortality_30d == 1L

  surgery_date <- as.Date("2018-01-01") + sample.int(2190L, n, replace = TRUE) - 1L
  admit_date <- surgery_date - sample(0:3, n, replace = TRUE, prob = c(.50, .30, .15, .05))
  discharge_date <- surgery_date + 3L + stats::rpois(n, 4)
  death_date <- as.Date(rep(NA_real_, n))
  death_date[dead] <- surgery_date[dead] + sample(1:30, sum(dead), replace = TRUE)
  discharge_date[dead] <- death_date[dead]
  # Neither death_date nor followup_days may mirror the 30-day outcome: ~3% of survivors die later
  # (death after day 30, mortality_30d stays 0) and ~4% of the rest are lost to follow-up before day 30.
  late <- !dead & stats::rbinom(n, 1L, 0.03) == 1L
  death_date[late] <- surgery_date[late] + sample(31:400, sum(late), replace = TRUE)
  cens <- !dead & !late & stats::rbinom(n, 1L, 0.04) == 1L
  fu_cap <- as.integer(as.Date("2025-12-31") - surgery_date)
  followup_days <- pmin(sample(30:1500, n, replace = TRUE), fu_cap)
  followup_days[cens] <- sample(5:29, sum(cens), replace = TRUE)
  has_death <- !is.na(death_date)
  followup_days[has_death] <- as.integer(death_date[has_death] - surgery_date[has_death])
  followup_days <- as.integer(followup_days)

  statin <- stats::rbinom(n, 1L, stats::plogis(-0.9 + 0.8 * diabetes + 0.015 * (age - 64)))
  statin_start_date <- as.Date(rep(NA_real_, n))
  ex <- statin == 1L
  statin_start_date[ex] <- surgery_date[ex] - sample(7:730, sum(ex), replace = TRUE)

  notes <- sample(SR_SAMPLE_NOTES, n, replace = TRUE)
  const_flag <- sample(c("Y", "N"), n, replace = TRUE, prob = c(.6, .4))
  bmi_copy <- clamp(round(stats::rnorm(n, 27, 4.5), 1), 15, 50)
  lab_status <- sample(c("normal", "abnormal", "repeat"), n, replace = TRUE, prob = c(.70, .20, .10))

  planted <- data.frame(check = character(), column = character(), rows_hint = character(),
                        stringsAsFactors = FALSE)
  dup_src <- dup_tgt <- id_rows <- id_donors <- integer()

  # ---- planted defects (own random draws, after the base cohort) ------------------------------
  if (isTRUE(defects)) {
    cnt <- function(p) max(1L, as.integer(round(n * p)))
    # No planted defect lands on a row with a death date (a 2099 surgery or a late statin start
    # there would create an unplanted death-before-surgery anomaly).
    alive <- which(!dead & is.na(death_date))

    # Rows reserved for the row-level defects that must not overlap anything else: unexposed
    # survivors, so no immortal-time or leakage row is ever copied, renumbered or moved to 2099.
    p0 <- sample(intersect(alive, which(statin == 0L)))
    dup_src <- p0[1:5]
    dup_tgt <- p0[6:10]
    id_rows <- p0[11:14]
    id_donors <- p0[15:18]
    fut_rows <- p0[19:20]

    pool <- sample(setdiff(alive, p0[1:20]))
    pos <- 0L
    take <- function(k) {
      r <- pool[pos + seq_len(k)]
      pos <<- pos + k
      r
    }
    r_miss <- take(cnt(0.02))
    r_age <- take(3L)
    r_levels <- take(cnt(0.01))
    r_pending <- take(cnt(0.005))
    r_mgdl <- take(cnt(0.03))
    r_lb <- take(cnt(0.02))
    r_order <- take(cnt(0.01))
    r_outlier <- take(5L)
    r_nzv <- take(cnt(0.002))
    r_heap <- sample(setdiff(which(is.na(death_date)), r_miss), cnt(0.60))
    r_immortal <- sample(which(statin == 1L & is.na(death_date)), round(0.4 * sum(statin == 1L)))

    # numeric columns
    age[r_age] <- 250L
    sbp[r_heap] <- as.integer(round(sbp[r_heap] / 10) * 10)
    sbp[r_miss] <- -99L
    weight_kg[r_lb] <- round(weight_kg[r_lb] * 2.2046, 1)
    creatinine_umol[r_mgdl] <- round(creatinine_umol[r_mgdl] / 88.4, 2)
    glucose_mmol[r_outlier] <- 60
    # text columns
    creatinine_umol <- as.character(creatinine_umol)
    creatinine_umol[r_pending] <- "pending"
    nyha[r_levels] <- "V"
    notes <- paste0(notes, "; ref ", sprintf("%06d", sample.int(999999L, n)))
    const_flag[] <- "Y"
    const_flag[r_nzv] <- "N"
    bmi_copy <- bmi + stats::rnorm(n, 0, 0.01)
    lab_status[dead] <- "deceased"
    # dates
    discharge_date[r_order] <- admit_date[r_order] - sample(1:10, length(r_order), replace = TRUE)
    # only surgery_date moves, so these rows also have surgery after discharge (and statin NA)
    surgery_date[fut_rows] <- as.Date("2099-01-01") + sample(0:300, length(fut_rows))
    statin_start_date[r_immortal] <- surgery_date[r_immortal] +
      sample(1:60, length(r_immortal), replace = TRUE)

    h <- .sr_sample_hint
    planted <- data.frame(
      check = c("coercion", "dict_levels", "missing_codes", "range", "unit_mix", "unit_mix",
                "heaping", "date_order", "future_date", "immortal_time", "dup_rows", "dup_id",
                "nzv", "high_card", "collinear", "leakage", "outlier"),
      column = c("creatinine_umol", "nyha", "sbp", "age", "weight_kg", "creatinine_umol", "sbp",
                 "discharge_date", "surgery_date", "statin_start_date", "*", "patient_id",
                 "const_flag", "notes", "bmi_copy", "lab_status", "glucose_mmol"),
      rows_hint = c(h(r_pending), h(r_levels), h(r_miss), h(r_age), h(r_lb), h(r_mgdl),
                    h(r_heap, paste0("n=", length(r_heap), " (rounded to a multiple of 10)")),
                    h(r_order), h(fut_rows, paste0(h(fut_rows), "; surgery is also after discharge")), h(r_immortal), h(dup_tgt), h(id_rows),
                    h(r_nzv, paste0("n=", length(r_nzv), " minority rows")),
                    h(NULL, "all rows (unique suffix per note)"),
                    h(NULL, "all rows (bmi + N(0, 0.01))"),
                    h(which(dead), paste0("n=", sum(dead), " deceased rows")),
                    h(r_outlier)),
      stringsAsFactors = FALSE)
  }

  data <- data.frame(
    patient_id = patient_id, site = site, age = age, sex = sex, weight_kg = weight_kg,
    height_cm = height_cm, bmi = bmi, sbp = sbp, creatinine_umol = creatinine_umol,
    hba1c_pct = hba1c_pct, glucose_mmol = glucose_mmol, nyha = nyha, diabetes = diabetes,
    admit_date = admit_date, surgery_date = surgery_date, discharge_date = discharge_date,
    death_date = death_date, mortality_30d = mortality_30d, statin_start_date = statin_start_date,
    statin = statin, followup_days = followup_days, notes = notes, const_flag = const_flag,
    bmi_copy = bmi_copy, lab_status = lab_status, stringsAsFactors = FALSE)

  if (isTRUE(defects)) {
    data$patient_id[id_rows] <- data$patient_id[id_donors]  # repeated ids, different data
    data[dup_tgt, ] <- data[dup_src, ]                       # exact duplicates (patient_id included)
  }
  rownames(data) <- NULL
  list(data = data, dict = .sr_sample_dict(), planted = planted)
}
