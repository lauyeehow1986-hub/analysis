# DQ calibration on the synthetic cardiac twins (manual tool; the unit tests run a small slice of it):
#   Rscript tools/dq_calibrate.R [--seeds=1:20] [--n=2000,50000,500000] [--csv=out.csv]
# For every (n, seed) it runs all registered DQ checks on the cohort with planted defects and on its
# clean twin, then prints, per n:
#   * recall: for each planted defect, the seeds where a finding with the same check and column exists
#     at the expected severity or higher;
#   * the clean-twin firing rate: for each check, the seeds where it raises a warn+ finding, and how
#     many of those findings are false positives (outside the by-design allowlist below).
# The full grid (20 seeds x 3 sizes) takes a while at n = 500000; pass --n / --seeds for a slice.
# Synthetic data only. Run from anywhere; the script finds the repo from its own location.

# ---- the answer key ---------------------------------------------------------------------------------

# The minimum severity each planted defect must be reported at (one row per row of `planted`).
SR_CAL_EXPECTED <- data.frame(
  check = c("coercion", "dict_levels", "missing_codes", "range", "unit_mix", "unit_mix", "heaping",
            "date_order", "future_date", "immortal_time", "dup_rows", "dup_id", "nzv", "high_card",
            "collinear", "leakage", "outlier"),
  column = c("creatinine_umol", "nyha", "sbp", "age", "weight_kg", "creatinine_umol", "sbp",
             "discharge_date", "surgery_date", "statin_start_date", "*", "patient_id", "const_flag",
             "notes", "bmi_copy", "lab_status", "glucose_mmol"),
  severity = c("warn", "warn", "warn", "block", "warn", "warn", "warn", "warn", "block", "warn",
               "warn", "block", "warn", "info", "warn", "block", "info"),
  stringsAsFactors = FALSE)

# Warn+ findings the clean twin legitimately raises: death_date and statin_start_date are missing by
# design (alive patients have no death date, unexposed patients no statin start).
SR_CAL_ALLOW <- data.frame(
  check = rep(c("missing_col", "missing_dependence"), each = 2L),
  column = rep(c("death_date", "statin_start_date"), 2L),
  stringsAsFactors = FALSE)

# The roles of the cardiac study, with the 30-day mortality as the outcome.
SR_CAL_ROLES <- c(patient_id = "id", surgery_date = "time_zero", statin_start_date = "exposure_date",
                  statin = "exposure", death_date = "event_date", mortality_30d = "outcome")

.cal_rank <- function(sev) match(sev, c("info", "warn", "block"))

# ---- one run ----------------------------------------------------------------------------------------

# Generate the cohort, ingest it into a throwaway project, attach the dictionary and roles, run every
# DQ check and return the findings (the project is closed and removed on exit).
.cal_run <- function(seed, n, defects) {
  s <- sr_sample_cardiac(n = n, seed = seed, defects = defects)
  base <- tempfile("sr_cal_")
  dir.create(base)
  p <- sr_project_create(file.path(base, "proj"), "calibration", profile = "laptop")
  on.exit({
    try(sr_duck_close(p), silent = TRUE)
    try(sr_project_close(p), silent = TRUE)
    unlink(base, recursive = TRUE)
  }, add = TRUE)
  csv <- file.path(base, "cardiac.csv")
  utils::write.csv(s$data, csv, row.names = FALSE, fileEncoding = "UTF-8")
  p <- sr_ingest(p, csv, table = "cardiac")
  dcsv <- file.path(base, "cardiac_dict.csv")
  utils::write.csv(s$dict, dcsv, row.names = FALSE, fileEncoding = "UTF-8")
  p <- sr_dict_set(p, "cardiac", sr_dict_read_csv(dcsv), "csv")
  p <- sr_roles_set(p, "cardiac", SR_CAL_ROLES)
  t0 <- Sys.time()
  p <- sr_dq_run(p, "cardiac")
  f <- sr_dq_findings(p, "cardiac")
  attr(f, "secs") <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  attr(f, "planted") <- s$planted
  f
}

# Per row of SR_CAL_EXPECTED: is there a finding with that check and column at that severity or higher?
.cal_matched <- function(f) {
  vapply(seq_len(nrow(SR_CAL_EXPECTED)), function(i) {
    h <- f[f$check == SR_CAL_EXPECTED$check[i] & f$column == SR_CAL_EXPECTED$column[i], , drop = FALSE]
    nrow(h) > 0L && any(.cal_rank(h$severity) >= .cal_rank(SR_CAL_EXPECTED$severity[i]))
  }, logical(1))
}

# The warn+ findings of a clean-twin run, with a flag for the by-design ones.
.cal_clean_warn <- function(f) {
  w <- f[f$severity != "info", c("check", "column", "severity"), drop = FALSE]
  w$allowed <- paste(w$check, w$column) %in% paste(SR_CAL_ALLOW$check, SR_CAL_ALLOW$column)
  w
}

# ---- the grid ---------------------------------------------------------------------------------------

.cal_grid <- function(seeds, ns, say = function(...) invisible()) {
  rows <- list()
  for (n in ns) {
    for (seed in seeds) {
      fd <- .cal_run(seed, n, TRUE)
      fc <- .cal_run(seed, n, FALSE)
      say(sprintf("n=%d seed=%d (%.1fs + %.1fs)", n, seed, attr(fd, "secs"), attr(fc, "secs")))
      rows[[length(rows) + 1L]] <- list(n = n, seed = seed, matched = .cal_matched(fd),
                                        clean = .cal_clean_warn(fc), secs = attr(fd, "secs") + attr(fc, "secs"))
    }
  }
  rows
}

.cal_report <- function(rows, ns) {
  for (n in ns) {
    r <- Filter(function(x) x$n == n, rows)
    k <- length(r)
    cat(sprintf("\n== n = %d, %d seed(s), %.0fs of checks ==\n", n, k, sum(vapply(r, function(x) x$secs, 0))))
    hit <- rowSums(vapply(r, function(x) x$matched, logical(nrow(SR_CAL_EXPECTED))))
    cat("recall (seeds with a matching finding at the expected severity or higher):\n")
    for (i in seq_len(nrow(SR_CAL_EXPECTED))) {
      cat(sprintf("  %-14s %-18s %-5s %2d/%d%s\n", SR_CAL_EXPECTED$check[i], SR_CAL_EXPECTED$column[i],
                  SR_CAL_EXPECTED$severity[i], hit[i], k, if (hit[i] < k) "  <-- MISSED" else ""))
    }
    cat(sprintf("overall recall: %.3f\n", sum(hit) / (k * nrow(SR_CAL_EXPECTED))))
    w <- do.call(rbind, lapply(seq_along(r), function(i) {
      if (nrow(r[[i]]$clean)) cbind(seed = r[[i]]$seed, r[[i]]$clean) else NULL
    }))
    cat("clean twin, warn+ firing per check (seeds firing / seeds; false positives are outside the allowlist):\n")
    if (is.null(w) || !nrow(w)) {
      cat("  (none)\n")
    } else {
      key <- unique(w[, c("check", "column", "allowed")])
      key <- key[order(key$allowed, key$check, key$column), ]
      for (i in seq_len(nrow(key))) {
        m <- w$check == key$check[i] & w$column == key$column[i]
        cat(sprintf("  %-20s %-18s %2d/%d  %s\n", key$check[i], key$column[i], length(unique(w$seed[m])), k,
                    if (key$allowed[i]) "allowed" else "FALSE POSITIVE"))
      }
    }
    cat(sprintf("clean false positives (findings outside the allowlist): %d\n",
                if (is.null(w)) 0L else sum(!w$allowed)))
  }
  invisible(NULL)
}

.cal_long <- function(rows) {
  do.call(rbind, lapply(rows, function(x) {
    rec <- data.frame(n = x$n, seed = x$seed, kind = "planted", check = SR_CAL_EXPECTED$check,
                      column = SR_CAL_EXPECTED$column, severity = SR_CAL_EXPECTED$severity,
                      value = as.integer(x$matched), stringsAsFactors = FALSE)
    fp <- if (nrow(x$clean)) {
      data.frame(n = x$n, seed = x$seed, kind = "clean", check = x$clean$check, column = x$clean$column,
                 severity = x$clean$severity, value = as.integer(!x$clean$allowed), stringsAsFactors = FALSE)
    } else NULL
    rbind(rec, fp)
  }))
}

# ---- main -------------------------------------------------------------------------------------------

if (sys.nframe() == 0L && !interactive()) {
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
  root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
  setwd(root)
  source(file.path(root, "app", "global.R"), encoding = "UTF-8")
  source(file.path(root, "samples", "gen_cardiac.R"), encoding = "UTF-8")
  arg <- function(key, default) {
    hit <- grep(paste0("^--", key, "="), commandArgs(TRUE), value = TRUE)
    if (length(hit)) sub("^[^=]*=", "", hit[1L]) else default
  }
  seeds <- eval(parse(text = arg("seeds", "1:20")))
  ns <- as.integer(strsplit(arg("n", "2000,50000,500000"), ",", fixed = TRUE)[[1L]])
  csv <- arg("csv", "")
  rows <- .cal_grid(seeds, ns, say = function(m) cat(m, "\n", sep = ""))
  .cal_report(rows, ns)
  if (nzchar(csv)) utils::write.csv(.cal_long(rows), csv, row.names = FALSE)
}
