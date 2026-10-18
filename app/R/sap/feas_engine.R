# sap/feas_engine.R - the engine of the study feasibility verdict (spec catalogue O): can the data
# estimate a primary analysis precisely enough? It runs before the SAP lock, so it is outcome-blind
# by construction.
#
# Inputs (.sr_feas_inputs). From the frame .sr_extract_frame() returns for sr_spec_cols(spec), the
# engine takes only what catalogue A already shows while blind:
#   * the analysed n (rows after exclude_rows, complete cases over every spec column);
#   * the outcome's OWN marginals (risk, SD, total events and follow-up, the (time, event) pairs of
#     the pooled exposure-blind Kaplan-Meier curve), over EVERY row with a non-missing outcome, coded
#     by the route's own rule. Expected events among the complete cases are events_total * n_cc /
#     n_obs, never a count;
#   * exposure marginals and the exposure-on-covariate structure (R2 of the exposure model, the
#     propensity overlap, separation) on the complete-case covariate frame;
#   * cluster sizes; the parameter count of the compiled design.
# It never computes an outcome-by-exposure or outcome-by-covariate statistic.
#
# Byte-identical under outcome permutation: the outcome values are sorted (survival: the pairs) before
# any sum, and the complete-case covariate frame (exposure, term variables, cluster, strata) is sorted
# by all its columns before the term code and the exposure models see it. No result depends on the
# row order of the table, so permuting the outcome among its non-missing rows changes nothing.
#
# The design comes from the route's own term builder: sr_terms_code() setup + design, checked by
# .sr_emit_check() and evaluated in a fresh environment under globalenv() with sr_data bound to the
# sorted covariate frame, for a covariate-only spec (the exposure model) and for the full spec minus
# the outcome (the parameter count). No outcome column is ever bound there.
#
# Nothing here raises for a design the data cannot carry: a coding failure is the blocker
# "outcome: <message>", a term-code or exposure-model failure the blocker "design: <message>", and
# the fields that depend on it are NA. Levels and counts are still reported.

SR_FEAS_Z_CI <- 1.96                     # 95% CI half-width multiplier
SR_FEAS_Z_MDE <- 2.80                    # minimum detectable effect at 80% power, two-sided 5%
SR_FEAS_PS_RANGE <- c(0.01, 0.99)        # propensities outside this range are near-positivity violations
SR_FEAS_PS_SHARE <- 0.05                 # warn when more than this share of rows is outside the range
SR_FEAS_EPV_WARN <- 10                   # events per parameter below this: the sparse-data warning
SR_FEAS_MIN_CLUSTERS <- 30               # fewer clusters: the cluster-robust SE warning
SR_FEAS_SEP_EPS <- 1e-8                  # a fitted propensity this close to 0 or 1 is separation
SR_FEAS_R2X_MAX <- 0.999                 # a continuous exposure with R2 on the covariates this high is separation

#' The outcome kind of route ids, vectorised: "continuous" (lm*), "binary" (logistic*, modpois*),
#' "surv" (cox*, rmst*), "none" for any other route (table1, test routes).
.sr_feas_kind <- function(route_id) {
  route_id <- as.character(route_id)
  out <- rep("none", length(route_id))
  out[grepl("^lm", route_id)] <- "continuous"
  out[grepl("^(logistic|modpois)", route_id)] <- "binary"
  out[grepl("^(cox|rmst)", route_id)] <- "surv"
  out
}

# ---- helpers ---------------------------------------------------------------------------------------

# d with its rows sorted by all its columns (radix: C-locale, the same on every machine), row names reset.
.sr_feas_sort <- function(d) {
  if (ncol(d) && nrow(d) > 1L) {
    d <- d[do.call(order, c(unname(as.list(d)), list(method = "radix"))), , drop = FALSE]
  }
  rownames(d) <- NULL
  d
}

# The levels of x the way .h_levels orders them (sorted in the original type, as text, `ref` first
# when it is observed), without its refusals: an absent ref or a single level is a blocker elsewhere.
.sr_feas_levels <- function(x, ref = NULL) {
  lv <- as.character(sort(unique(x[!is.na(x)])))
  if (!is.null(ref) && ref %in% lv) lv <- c(ref, setdiff(lv, ref))
  lv
}

# The coded outcome, by the rule of sr_outcome_code(): list(y) for binary (0/1 integer) and continuous
# (numeric), list(time, event) for survival. Stops with the route's message when it cannot code.
.sr_feas_code <- function(df, spec, kind) {
  col <- function(field) df[[.sr_one_string(spec[[field]], paste0("spec$", field))]]
  ev <- function(x, value) {
    v <- spec[[value]]
    if (is.null(v)) return(.h_bin(x))
    if (!is.atomic(v) || length(v) != 1L || is.na(v)) stop(value, " must be one non-NA value", call. = FALSE)
    as.integer(x == v)
  }
  switch(kind,
    binary = list(y = ev(col("outcome"), "event_value")),
    continuous = list(y = .h_num(col("outcome"), spec[["outcome"]])),
    surv = list(time = .h_num(col("time"), spec[["time"]]), event = ev(col("status"), "status_event")))
}

# The outcome fields, all NA (pairs NULL): what an outcome that cannot be coded reports.
.sr_feas_na_outcome <- function() {
  list(sd = NA_real_, risk = NA_real_, events_total = NA_real_, events_cc = NA_real_,
       followup_total = NA_real_, max_time = NA_real_, pairs = NULL)
}

# The outcome marginals of a coded outcome over its non-missing values, sorted first. Returns
# list(n_obs, outcome); outcome fields that do not apply to the kind are NA (pairs NULL), and
# events_cc is filled in by the caller once n_cc is known.
.sr_feas_marginals <- function(code, kind) {
  out <- .sr_feas_na_outcome()
  if (kind == "surv") {
    ok <- !is.na(code$time) & !is.na(code$event)
    pr <- data.frame(time = code$time[ok], event = code$event[ok])
    pr <- pr[order(pr$time, pr$event, method = "radix"), , drop = FALSE]
    rownames(pr) <- NULL
    n_obs <- nrow(pr)
    out$events_total <- as.numeric(sum(pr$event))
    out$followup_total <- sum(pr$time)
    if (n_obs) out$max_time <- max(pr$time)
    out["pairs"] <- list(pr)
    return(list(n_obs = n_obs, outcome = out))
  }
  y <- sort(code$y)                                    # sort drops the NA
  if (kind == "binary") {
    if (length(y)) out$risk <- mean(y)
    out$events_total <- as.numeric(sum(y))
  } else {
    out$sd <- if (length(y) > 1L) stats::sd(y) else NA_real_
  }
  list(n_obs = length(y), outcome = out)
}

# Rows with a non-missing raw outcome (when the outcome could not be coded); NA when the spec names none.
.sr_feas_n_raw <- function(df, spec, kind) {
  oc <- if (kind == "surv") c(spec[["time"]], spec[["status"]]) else spec[["outcome"]]
  oc <- .sr_spec_chr(oc)
  if (!length(oc) || !all(oc %in% names(df))) return(NA_integer_)
  sum(stats::complete.cases(df[, oc, drop = FALSE]))
}

# The covariate-only spec of the exposure model: the exposure and the outcome fields removed,
# interactions involving the exposure dropped, strata added as factor terms.
.sr_feas_cov_spec <- function(spec) {
  ex <- .sr_spec_chr(spec[["exposure"]])
  s <- spec
  s[c("outcome", "time", "status", "exposure", "exposure_ref", "exclude_rows")] <- NULL
  s$interactions <- Filter(function(ia) !any(as.character(ia) %in% ex), spec[["interactions"]])
  tv <- .sr_spec_chr(lapply(spec[["terms"]], function(t) t[["var"]]))
  st <- setdiff(.sr_spec_chr(spec[["strata"]]), tv)
  s$terms <- c(spec[["terms"]], lapply(st, function(v) list(var = v, type = "factor")))
  s
}

# The model matrix of `spec`'s compiled design on `data`: sr_terms_code() setup + design, checked as
# analysis code, evaluated in a fresh environment under globalenv() with sr_data bound to `data`.
.sr_feas_design <- function(spec, data) {
  tc <- sr_terms_code(spec)
  code <- paste(tc$setup, tc$design, sep = "\n")
  .sr_emit_check(code, "analysis")
  env <- new.env(parent = globalenv())
  env$sr_data <- data
  eval(parse(text = code, keep.source = FALSE), envir = env)
  des <- env$.h_design(data)
  if (identical(tc$rhs, "1")) return(matrix(1, nrow(data), 1L))
  mm <- stats::model.matrix(stats::as.formula(paste("~", tc$rhs)), des)
  rownames(mm) <- NULL
  mm
}

# The exposure model of one contrast: z (the exposure, or 1{x = level} among rows in {level, ref}) on
# the covariate design X. r2x is the R2 of lm.fit (0 with no covariates, NA when z is constant);
# for a binary z, a binomial glm.fit (maxit 50, warnings suppressed) gives ps_outside and separated;
# for a continuous one, ps_outside is NA and separated is r2x >= SR_FEAS_R2X_MAX.
.sr_feas_fit <- function(X, z, binary) {
  tss <- sum((z - mean(z))^2)
  r2 <- if (ncol(X) <= 1L) 0 else if (tss > 0) 1 - sum(stats::lm.fit(X, z)$residuals^2) / tss else NA_real_
  if (!binary) {
    return(list(r2x = r2, ps_outside = NA_real_, separated = !is.na(r2) && r2 >= SR_FEAS_R2X_MAX))
  }
  g <- suppressWarnings(stats::glm.fit(X, z, family = stats::binomial(),
                                       control = stats::glm.control(maxit = 50)))
  p <- unname(g$fitted.values)
  list(r2x = r2, ps_outside = mean(p < SR_FEAS_PS_RANGE[1L] | p > SR_FEAS_PS_RANGE[2L]),
       separated = !isTRUE(g$converged) || any(p < SR_FEAS_SEP_EPS | p > 1 - SR_FEAS_SEP_EPS))
}

.sr_feas_no_contrast <- function() {
  data.frame(level = character(), n_k = integer(), n_ref = integer(), r2x = numeric(),
             ps_outside = numeric(), separated = logical(), stringsAsFactors = FALSE)
}

# ---- the inputs ------------------------------------------------------------------------------------

#' The outcome-blind inputs of the feasibility verdict of one analysis. Pure.
#' @param df the frame .sr_extract_frame() returns for sr_spec_cols(spec). Every result except the
#'   effect of spec$exclude_rows is independent of its row order; exclude_rows positions refer to
#'   the .sr_extract_frame() order (rows sorted by every column), the order the run's extract has.
#' @param spec the analysis spec
#' @param route the route (an sr_route) or its id
#' @return a list:
#'   * kind: "continuous" | "binary" | "surv" | "none" (.sr_feas_kind of the route id).
#'   * n_rows: rows after exclude_rows.
#'   * n_cc: complete cases over every spec column present (sr_spec_cols, incl. mi$aux).
#'   * n_obs: rows with a non-missing coded outcome (surv: time and event); the raw non-missing count
#'     when the outcome cannot be coded; NA when a spec column is missing; kind "none": the non-NA
#'     count of spec$outcome when present, else NA.
#'   * outcome: list(sd, risk, events_total, events_cc, followup_total, max_time, pairs). sd:
#'     continuous only. risk: binary only. events_total: binary sum(y), surv sum(event).
#'     events_cc = events_total * n_cc / n_obs (binary, surv). followup_total, max_time and pairs
#'     (data.frame(time, event) sorted by time then event): surv only. Fields not of the kind are
#'     NA (pairs NULL); an outcome that cannot be coded is all NA plus the blocker "outcome: <msg>".
#'   * exposure: NULL unless spec$exposure is one column, else list(var, type, ref, levels,
#'     variance). type: "factor" (exposure_ref set, or an rmst route), else "binary" (numeric or
#'     logical with values in {0, 1}), else "linear". ref: exposure_ref, else the first level (NA for
#'     linear). levels: data.frame(level, n_all, n_cc) -- factor: the sorted observed levels with
#'     ref first; binary: "0","1" (logical: "FALSE","TRUE"); linear: zero rows. n_all counts the
#'     frame after exclude_rows, n_cc the complete cases. variance: the population variance over the
#'     complete cases (binary, linear), NA for factor.
#'   * contrasts_in: data.frame(level, n_k, n_ref, r2x, ps_outside, separated), one row per
#'     non-reference level (binary, factor) or one row with level NA, n_k = n_cc, n_ref NA (linear);
#'     zero rows without an exposure. r2x: R2 of the exposure (or the level indicator among rows at
#'     level or ref) on the covariate design, 0 with no covariates, NA when constant. ps_outside: the
#'     share of fitted propensities outside SR_FEAS_PS_RANGE (NA for linear). separated: the glm did
#'     not converge or a propensity is within SR_FEAS_SEP_EPS of 0 or 1 (linear: r2x >=
#'     SR_FEAS_R2X_MAX). n_k or n_ref 0, or a design failure, gives NA r2x, ps_outside, separated.
#'   * clusters: NULL unless spec$layers$clustered, else list(G, m_bar = n_cc / G, cv = the
#'     population SD of the cluster sizes / m_bar); all NA when the cluster column is unavailable.
#'   * params: the column count of the full design (spec minus the outcome fields), minus 1 for surv
#'     (no intercept); rmst: the number of exposure levels; NA when the design fails.
#'   * blockers: unique character; "outcome: ..." or "design: ..." (exclude_rows beyond the data,
#'     missing columns, an unobserved reference level, a term-code or exposure-model error).
#'   For kind "none" only n_rows, n_cc and n_obs are computed (outcome, exposure, contrasts_in and
#'   clusters NULL, params NA).
.sr_feas_inputs <- function(df, spec, route) {
  rid <- if (is.character(route)) route else route$id
  kind <- .sr_feas_kind(rid)
  blockers <- character()
  block <- function(...) blockers <<- c(blockers, paste0(...))

  # 1. exclude_rows, by position in the frame as handed over
  ex <- .sr_exclude_rows(spec)
  ex <- ex[!is.na(ex)]
  if (length(ex)) {
    if (max(ex) > nrow(df)) {
      block("design: exclude_rows names row ", max(ex), " but the data have ", nrow(df), " rows")
    }
    keep <- unique(ex[ex >= 1L & ex <= nrow(df)])
    if (length(keep)) df <- df[-keep, , drop = FALSE]
  }
  rownames(df) <- NULL
  n_rows <- nrow(df)
  want <- tryCatch(sr_spec_cols(spec), error = function(e) names(df))
  cols <- intersect(want, names(df))
  cc <- if (length(cols)) stats::complete.cases(df[, cols, drop = FALSE]) else rep(TRUE, n_rows)
  n_cc <- sum(cc)
  skeleton <- function(n_obs) {
    list(kind = kind, n_rows = n_rows, n_cc = n_cc, n_obs = n_obs, outcome = NULL, exposure = NULL,
         contrasts_in = NULL, clusters = NULL, params = NA_integer_, blockers = unique(blockers))
  }
  if (kind == "none") {
    oc <- .sr_spec_chr(spec[["outcome"]])
    return(skeleton(if (length(oc) == 1L && oc %in% names(df)) sum(!is.na(df[[oc]])) else NA_integer_))
  }
  miss <- setdiff(want, names(df))
  if (length(miss)) {
    block("design: column(s) not in the data: ", paste(miss, collapse = ", "))
    return(skeleton(NA_integer_))
  }

  # 2-4. the outcome, coded by the route's rule; its marginals over every non-missing value
  code <- tryCatch(.sr_feas_code(df, spec, kind), error = function(e) {
    block("outcome: ", conditionMessage(e))
    NULL
  })
  if (is.null(code)) {
    n_obs <- .sr_feas_n_raw(df, spec, kind)
    outcome <- .sr_feas_na_outcome()
  } else {
    mg <- .sr_feas_marginals(code, kind)
    n_obs <- mg$n_obs
    outcome <- mg$outcome
    if (kind != "continuous" && n_obs > 0L) outcome$events_cc <- outcome$events_total * n_cc / n_obs
  }

  # 5. the complete-case covariate frame, sorted by all its columns
  exv <- .sr_spec_chr(spec[["exposure"]])
  tv <- tryCatch(.sr_spec_chr(lapply(spec[["terms"]], function(t) t[["var"]])), error = function(e) character())
  cov_cols <- intersect(unique(c(exv, tv, .sr_spec_chr(spec[["cluster"]]), .sr_spec_chr(spec[["strata"]]))),
                        names(df))
  cf <- .sr_feas_sort(df[cc, cov_cols, drop = FALSE])
  rmst <- grepl("^rmst", rid)

  # 6. the compiled designs: covariates only (the exposure model), and the full spec minus the outcome
  design <- function(s) {
    tryCatch(.sr_feas_design(s, cf), error = function(e) {
      block("design: ", conditionMessage(e))
      NULL
    })
  }
  X <- design(.sr_feas_cov_spec(spec))
  params <- NA_integer_
  if (!rmst) {
    full <- spec
    full[c("outcome", "time", "status", "exclude_rows")] <- NULL
    mm <- design(full)
    if (!is.null(mm)) params <- as.integer(ncol(mm) - (kind == "surv"))
  }

  # 7-8. the exposure: type, levels (reference first) and the exposure model of each contrast
  exposure <- NULL
  contrasts_in <- .sr_feas_no_contrast()
  if (length(exv) == 1L) {
    x_all <- df[[exv]]
    ref <- spec[["exposure_ref"]]
    has_ref <- length(ref) == 1L && !is.na(ref) && nzchar(as.character(ref))
    bin01 <- (is.numeric(x_all) || is.logical(x_all)) && all(x_all[!is.na(x_all)] %in% c(0, 1))
    type <- if (has_ref || rmst) "factor" else if (bin01) "binary" else "linear"
    x_cc <- cf[[exv]]
    lv <- switch(type,
      factor = .sr_feas_levels(x_all, if (has_ref) as.character(ref) else NULL),
      binary = if (is.logical(x_all)) c("FALSE", "TRUE") else c("0", "1"),
      linear = character())
    ref_lv <- if (has_ref) as.character(ref) else if (length(lv)) lv[1L] else NA_character_
    if (has_ref && !(ref_lv %in% lv)) {
      block("design: reference level '", ref_lv, "' is not among the observed levels")
    }
    lv_df <- data.frame(level = lv, n_all = tabulate(match(as.character(x_all), lv), length(lv)),
                         n_cc = tabulate(match(as.character(x_cc), lv), length(lv)), stringsAsFactors = FALSE)
    xs <- if (type == "factor") NULL else tryCatch(.h_num(x_cc, exv), error = function(e) NULL)
    variance <- if (length(xs)) mean((xs - mean(xs))^2) else NA_real_
    exposure <- list(var = exv, type = type, ref = ref_lv, levels = lv_df, variance = variance)

    na_fit <- list(r2x = NA_real_, ps_outside = NA_real_, separated = NA)
    fit <- function(rows, z, binary) {
      if (is.null(X)) return(na_fit)
      tryCatch(.sr_feas_fit(X[rows, , drop = FALSE], z, binary), error = function(e) {
        block("design: ", conditionMessage(e))
        na_fit
      })
    }
    rows <- list()
    if (type == "linear") {
      f <- if (is.null(xs)) na_fit else fit(seq_len(n_cc), xs, FALSE)
      rows[[1L]] <- c(list(level = NA_character_, n_k = n_cc, n_ref = NA_integer_), f)
    } else {
      key <- as.character(x_cc)
      for (l in setdiff(lv, ref_lv)) {
        n_k <- sum(key == l)
        n_ref <- sum(key == ref_lv)
        sel <- which(key %in% c(l, ref_lv))
        f <- if (n_k == 0L || n_ref == 0L) na_fit else fit(sel, as.numeric(key[sel] == l), TRUE)
        rows[[length(rows) + 1L]] <- c(list(level = l, n_k = n_k, n_ref = n_ref), f)
      }
    }
    if (length(rows)) {
      contrasts_in <- data.frame(
        level = vapply(rows, function(r) r$level, ""), n_k = vapply(rows, function(r) as.integer(r$n_k), 0L),
        n_ref = vapply(rows, function(r) as.integer(r$n_ref), 0L), r2x = vapply(rows, function(r) r$r2x, 0),
        ps_outside = vapply(rows, function(r) r$ps_outside, 0),
        separated = vapply(rows, function(r) as.logical(r$separated), NA), stringsAsFactors = FALSE)
    }
    if (rmst) params <- nrow(lv_df)
  }

  # cluster sizes among the complete cases
  clusters <- NULL
  if (isTRUE(spec[["layers"]][["clustered"]])) {
    cl <- .sr_spec_chr(spec[["cluster"]])
    clusters <- list(G = NA_integer_, m_bar = NA_real_, cv = NA_real_)
    if (length(cl) == 1L && cl %in% names(cf) && n_cc > 0L) {
      m <- sort(as.numeric(tabulate(match(cf[[cl]], unique(cf[[cl]])))))
      m_bar <- n_cc / length(m)
      clusters <- list(G = length(m), m_bar = m_bar, cv = sqrt(mean((m - m_bar)^2)) / m_bar)
    }
  }

  list(kind = kind, n_rows = n_rows, n_cc = n_cc, n_obs = n_obs, outcome = outcome, exposure = exposure,
       contrasts_in = contrasts_in, clusters = clusters, params = params, blockers = unique(blockers))
}

# The engine's own frames, by path in the inputs or in a verdict (.sr_feas_compute). Their column names are
# the engine's fixed vocabulary (time / event, level / n_all / n_cc, n_k / n_ref / measure / scale ...), never
# data columns.
.sr_feas_struct_paths <- list(c("outcome", "pairs"), c("exposure", "levels"), "contrasts_in", "contrasts")

#' `x` (inputs or an assessment) through the outcome-blind gate (.sr_assert_blind), with the
#' analysis's outcome columns (outcome, time, status) against its other spec columns. `spec` is
#' required. The engine's own frames (.sr_feas_struct_paths) are walked as plain named lists, not as
#' data.frames: a spec column that happens to share one of their column names (a covariate named
#' `event`, an outcome named `level`) is not taken for a crossing, while every column name is still
#' checked against the forbidden outcome-statistic names (a `contrasts_in$or` is refused) and the
#' values are still walked. Returns x invisibly; stops naming the offending element.
.sr_feas_blind_frame <- function(x, spec) {
  if (missing(spec) || !is.list(spec)) stop("spec must be the analysis spec (a list)", call. = FALSE)
  oc <- c(.sr_spec_chr(spec[["outcome"]]), .sr_spec_chr(spec[["time"]]), .sr_spec_chr(spec[["status"]]))
  chk <- x
  if (is.list(chk) && !is.data.frame(chk)) {
    for (p in .sr_feas_struct_paths) {
      el <- chk
      for (k in p) el <- if (is.list(el) && !is.data.frame(el)) el[[k]] else NULL
      if (is.data.frame(el)) chk[[p]] <- as.list(el)      # names kept: only the crossing check is off
    }
  }
  .sr_assert_blind(chk, oc, setdiff(sr_spec_cols(spec), oc))
  invisible(x)
}

# ---- the computation: SEs, design effect, MDE, blockers, tiers ---------------------------------------
#
# From the inputs only (never the data): the null-effect standard error of each (contrast x measure)
# on the metric of its reporting scale (log for ratios), the design effect of a clustered analysis,
# the 95% CI half-width and the minimum detectable effect, the blockers and warnings, and the tier
# against the smallest clinically important difference delta (optional).

SR_FEAS_RATIO_MEASURES <- c("OR", "RR", "HR")

.sr_feas_ratio <- function(measure) measure %in% SR_FEAS_RATIO_MEASURES

# TRUE for one finite number.
.sr_feas_num1 <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)

# TRUE for one non-empty, non-blank string.
.sr_feas_text <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(trimws(x))

#' The effect measures a route reports (the names of routes/result.R), for the verdict: lm* MD;
#' logistic* OR, plus RD and RR when the marginal block runs (.sr_glm_marg_on) and the exposure is
#' binary or a factor; modpois* RR, plus RD under the same condition; cox* HR; rmst RMST_diff;
#' character() for any other route (table1, test routes).
.sr_feas_measures <- function(route_id, spec, exposure_type) {
  id <- as.character(route_id)
  marg <- .sr_glm_marg_on(spec) && length(exposure_type) == 1L && exposure_type %in% c("binary", "factor")
  if (grepl("^lm", id)) {
    "MD"
  } else if (grepl("^logistic", id)) {
    c("OR", if (marg) c("RD", "RR"))
  } else if (grepl("^modpois", id)) {
    c("RR", if (marg) "RD")
  } else if (grepl("^cox", id)) {
    "HR"
  } else if (grepl("^rmst", id)) {
    "RMST_diff"
  } else {
    character()
  }
}

#' Refuses a malformed delta or assumption; returns TRUE invisibly. `delta` is NULL or
#' list(value, measure, source): measure one of `measures`, source non-empty text, a ratio measure
#' with value > 0 and not 1, a difference measure with value > 0 (absolute). `assumptions` names only
#' r2_y and icc, each list(value, justification) with non-empty text; r2_y in [0, 1) for a continuous
#' outcome only; icc in [0, 1), required when `clustered` and refused otherwise.
.sr_feas_check_args <- function(delta, assumptions, kind, measures, clustered) {
  stopf <- function(...) stop(..., call. = FALSE)
  if (!is.null(delta)) {
    if (!is.list(delta) || !setequal(names(delta), c("value", "measure", "source")) || anyDuplicated(names(delta))) {
      stopf("delta must be NULL or list(value, measure, source)")
    }
    m <- delta[["measure"]]
    if (!(.sr_feas_text(m) && m %in% measures)) {
      stopf("delta$measure must be one of: ",
            if (length(measures)) paste(measures, collapse = ", ") else "(this analysis reports no effect measure)")
    }
    if (!.sr_feas_text(delta[["source"]])) stopf("delta$source must be non-empty text (where the difference comes from)")
    v <- delta[["value"]]
    if (!.sr_feas_num1(v)) stopf("delta$value must be one finite number")
    if (.sr_feas_ratio(m)) {
      if (v <= 0 || v == 1) stopf("a ratio delta must be greater than 0 and not 1 (", m, ")")
    } else if (v <= 0) {
      stopf("a difference delta must be greater than 0 (as an absolute value, ", m, ")")
    }
  }
  if (is.null(assumptions)) assumptions <- list()
  if (!is.list(assumptions)) stopf("assumptions must be a list naming only r2_y and icc")
  nm <- names(assumptions)
  if (length(assumptions) && (is.null(nm) || anyNA(nm) || !all(nm %in% c("r2_y", "icc")) || anyDuplicated(nm))) {
    stopf("assumptions may name only r2_y and icc")
  }
  each <- function(a, field) {
    if (!is.list(a) || !setequal(names(a), c("value", "justification")) || anyDuplicated(names(a))) {
      stopf("assumptions$", field, " must be list(value, justification)")
    }
    if (!.sr_feas_text(a[["justification"]])) stopf("assumptions$", field, " needs a non-empty justification")
    v <- a[["value"]]
    if (!.sr_feas_num1(v) || v < 0 || v >= 1) stopf("assumptions$", field, " value must be a number in [0, 1)")
  }
  if (!is.null(assumptions[["r2_y"]])) {
    if (!identical(kind, "continuous")) stopf("r2_y applies to a continuous outcome only")
    each(assumptions[["r2_y"]], "r2_y")
  }
  if (!is.null(assumptions[["icc"]])) {
    if (!isTRUE(clustered)) stopf("icc applies to a clustered analysis only")
    each(assumptions[["icc"]], "icc")
  } else if (isTRUE(clustered)) {
    stopf("a clustered analysis needs assumptions$icc (value and justification)")
  }
  invisible(TRUE)
}

#' The SD of the RMST difference between a group of `n_k` and one of `n_ref` subjects drawn from the
#' pooled exposure-blind (time, event) `pairs`: `sims` times, draw both groups with replacement from
#' the sorted pairs, take each group's RMST up to `tau` (summary(survfit)$table[["rmean"]]) and
#' difference them. Draws come from stream 1 of sr_streams(seed, 1), the smaller group first, so
#' swapping the groups only flips the sign of every difference. The caller's RNG kind and
#' .Random.seed are restored. NA when it cannot be computed.
.sr_feas_rmst_se <- function(pairs, n_k, n_ref, tau, sims, seed) {
  if (!is.data.frame(pairs) || !all(c("time", "event") %in% names(pairs))) return(NA_real_)
  pr <- pairs[order(pairs$time, pairs$event, method = "radix"), c("time", "event"), drop = FALSE]
  rownames(pr) <- NULL
  n <- nrow(pr)
  sims <- as.integer(sims)
  if (n < 2L || !.sr_feas_num1(n_k) || !.sr_feas_num1(n_ref) || n_k < 1 || n_ref < 1 || !.sr_feas_num1(tau) ||
      is.na(sims) || sims < 2L) {
    return(NA_real_)
  }
  stream <- sr_streams(seed, 1L)[[1L]]
  genv <- globalenv()
  had_seed <- exists(".Random.seed", envir = genv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = genv, inherits = FALSE) else NULL
  old_kind <- RNGkind()
  on.exit({
    suppressWarnings(RNGkind(old_kind[1L], old_kind[2L], old_kind[3L]))
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = genv)
    } else if (exists(".Random.seed", envir = genv, inherits = FALSE)) {
      rm(".Random.seed", envir = genv)
    }
  }, add = TRUE)
  assign(".Random.seed", stream, envir = genv)
  rmean <- function(i) {
    fit <- survival::survfit(survival::Surv(time, event) ~ 1, data = pr[i, , drop = FALSE])
    summary(fit, rmean = tau)$table[["rmean"]]
  }
  lo <- as.integer(min(n_k, n_ref))
  hi <- as.integer(max(n_k, n_ref))
  flip <- n_k > n_ref
  d <- vapply(seq_len(sims), function(s) {
    a <- rmean(sample.int(n, lo, replace = TRUE))
    b <- rmean(sample.int(n, hi, replace = TRUE))
    if (flip) b - a else a - b
  }, 0)
  if (sum(is.finite(d)) < 2L) return(NA_real_)
  stats::sd(d[is.finite(d)])
}

# The RMST simulation count: option sr.feas_rmst_sims, else env SR_FEAS_RMST_SIMS, else 500.
.sr_feas_rmst_sims <- function() {
  s <- suppressWarnings(as.integer(as.numeric(getOption("sr.feas_rmst_sims", Sys.getenv("SR_FEAS_RMST_SIMS", 500)))))
  if (length(s) != 1L || is.na(s) || s < 2L) 500L else s
}

# The seed of the RMST simulation: spec$seed, else the route schema's default seed, else 1.
.sr_feas_seed <- function(spec, route) {
  s <- spec[["seed"]]
  whole <- function(z) .sr_feas_num1(z) && z == round(z) && abs(z) <= .Machine$integer.max   # sr_streams' rule
  if (!whole(s)) s <- if (is.list(route)) route$spec_schema[["seed"]][["default"]] else NULL
  if (!whole(s)) 1L else s
}

# The tiers a delta can give, best first (the overall tier is the worst row).
.sr_feas_delta_tiers <- c("adequate", "limited", "inadequate")

#' The study feasibility verdict of one analysis, from its outcome-blind `inputs` (.sr_feas_inputs).
#' Pure. `delta` is NULL or list(value, measure, source) and `assumptions` a list of r2_y and icc
#' (each list(value, justification)); both are checked by .sr_feas_check_args, which stops.
#' @return list(contrasts, blockers, warnings, tier, advice):
#'   * contrasts: data.frame, one row per (contrast x measure): contrast (label), level (the exposure
#'     level, NA for a linear exposure), measure (MD, RD, RR, OR, HR, RMST_diff), scale ("log" for the
#'     ratio measures, "identity" for the others), se (the null-effect standard error ON that scale,
#'     design effect included), h (95% CI half-width) and mde (80% power minimum detectable effect),
#'     both on the reporting scale (exponentiated for a ratio), se_upper (the SE with n_obs in place
#'     of n_cc under multiple imputation, else NA), n_k and n_ref (the group sizes; n_ref NA for a
#'     linear exposure), r2x, deff (the design effect, 1 when unclustered), tier (the row's tier).
#'   * blockers: character, each naming its cause (tier cannot_run whatever delta is).
#'   * warnings: character.
#'   * tier: one of SR_FEAS_TIERS, the overall tier (see below).
#'   * advice: the decisions the verdict allows: "proceed"; "revise", "stop", "exploratory" when
#'     inadequate; "revise", "stop" when cannot_run.
#' Tier: cannot_run with blockers (every row too), else not_applicable without effect measures (or
#' without a contrast), else precision_only without delta. With delta each row of delta$measure is
#' adequate (2.80 se <= d), limited (2.80 se > d >= 1.96 se) or inadequate (1.96 se > d), with d =
#' |log delta| for a ratio and delta for a difference; the other rows are precision_only; the overall
#' tier is the worst delta-measure row.
.sr_feas_compute <- function(inputs, spec, route, delta = NULL, assumptions = list()) {
  rid <- if (is.character(route)) route else route$id
  ex <- inputs$exposure
  out <- inputs$outcome
  ci <- inputs$contrasts_in
  measures <- .sr_feas_measures(rid, spec, ex$type)
  clustered <- !is.null(inputs$clusters)
  .sr_feas_check_args(delta, assumptions, inputs$kind, measures, clustered)
  mi <- identical(spec[["layers"]][["missing"]], "mi")
  n_cc <- inputs$n_cc
  r2_y <- if (is.null(assumptions[["r2_y"]])) 0 else assumptions[["r2_y"]][["value"]]
  icc <- if (is.null(assumptions[["icc"]])) NA_real_ else assumptions[["icc"]][["value"]]
  fnum <- function(x) is.numeric(x) && length(x) == 1L && !is.na(x)

  # blockers: the coding / design blockers of the inputs first, then those of the numbers
  blockers <- as.character(inputs$blockers)
  block <- function(...) blockers <<- c(blockers, paste0(...))
  warns <- character()
  warn <- function(...) warns <<- c(warns, paste0(...))
  kind <- inputs$kind
  if (kind != "none") {
    if (n_cc == 0L) block("there is no complete-case row")
    lv <- ex$levels
    if (is.null(ex)) {
      block("the analysis has no single exposure column")
    } else if (ex$type == "linear") {
      if (fnum(ex$variance) && ex$variance == 0) block("the exposure does not vary")
    } else {
      if (sum(lv$n_cc > 0L) < 2L) block("fewer than 2 exposure levels have a complete-case row")
      for (l in lv$level[lv$n_all > 0L & lv$n_cc == 0L]) block("exposure level '", l, "' has no complete-case row")
    }
    if (kind == "binary") {
      if (fnum(out$risk) && out$risk %in% c(0, 1)) {
        block("the outcome is constant: ", if (out$risk == 0) "it has no events (marginal risk 0)" else
          "every row is an event (marginal risk 1)")
      }
    } else if (kind == "surv") {
      if (fnum(out$events_total) && out$events_total == 0) block("the outcome has no events")
    } else if (kind == "continuous") {
      if (fnum(out$sd) && out$sd == 0) block("the outcome is constant (SD is 0)")
    }
    p <- inputs$params
    epv_n <- switch(kind,
      binary = if (fnum(out$events_cc)) min(out$events_cc, n_cc - out$events_cc) else NA_real_,
      surv = out$events_cc, NA_real_)
    if (fnum(p) && kind == "continuous" && p >= n_cc) {
      block(p, " model parameters need fewer than the ", n_cc, " complete-case rows")
    } else if (fnum(p) && fnum(epv_n) && p > epv_n) {
      block(p, " model parameters exceed the ", format(round(epv_n, 1)), " expected ",
            if (kind == "binary") "events (or non-events)" else "events", " among the complete cases")
    }
    if (!is.null(ci) && nrow(ci) && any(ci$separated, na.rm = TRUE)) {
      block("the exposure is perfectly predicted by the covariates (positivity)")
    }
    if (grepl("^rmst", rid) && fnum(out$max_time) && fnum(spec[["tau"]]) && spec[["tau"]] > out$max_time) {
      block("tau (", format(spec[["tau"]]), ") is beyond the longest follow-up (", format(out$max_time), ")")
    }

    # warnings
    if (!is.null(ci) && nrow(ci) && any(ci$ps_outside > SR_FEAS_PS_SHARE, na.rm = TRUE)) {
      warn("near-positivity violation: more than ", format(100 * SR_FEAS_PS_SHARE), "% of the rows have a propensity outside [",
           SR_FEAS_PS_RANGE[1L], ", ", SR_FEAS_PS_RANGE[2L], "]")
    }
    if (fnum(p) && p > 0 && fnum(epv_n) && epv_n / p < SR_FEAS_EPV_WARN) {
      warn("events per parameter (", format(round(epv_n / p, 1)), ") is below ", SR_FEAS_EPV_WARN,
           ": sparse-data bias is likely",
           if (grepl("^logistic", rid) && !.sr_glm_firth(spec)) "; consider pre-specifying penalty = \"firth\"" else "")
    }
    if (clustered && fnum(inputs$clusters$G) && inputs$clusters$G < SR_FEAS_MIN_CLUSTERS) {
      warn("fewer than ", SR_FEAS_MIN_CLUSTERS, " clusters (", inputs$clusters$G,
           "): cluster-robust standard errors are unreliable")
    }
  }

  # the rows: one per contrast x measure
  seed <- .sr_feas_seed(spec, route)
  sims <- .sr_feas_rmst_sims()
  f_mi <- if (mi && fnum(inputs$n_obs) && n_cc > 0L) inputs$n_obs / n_cc else NA_real_
  deff_for <- function(f) {
    if (!clustered) return(1)
    cl <- inputs$clusters
    if (!(fnum(cl$cv) && fnum(cl$m_bar)) || is.na(icc)) return(NA_real_)
    1 + ((cl$cv^2 + 1) * cl$m_bar * f - 1) * icc
  }
  # the null-effect SE of measure m for contrast i with the sample scaled by f, before the design effect
  se_for <- function(m, i, f) {
    nk <- ci$n_k[i]
    nr <- ci$n_ref[i]
    r2 <- ci$r2x[i]
    fac <- ex$type == "factor"
    if (m == "RMST_diff") {
      if (length(blockers)) return(NA_real_)             # already cannot_run: no simulation
      return(.sr_feas_rmst_se(out$pairs, round(nk * f), round(nr * f), spec[["tau"]], sims, seed))
    }
    v <- if (fac) {
      if (nk > 0L && nr > 0L) (1 - r2) / (1 / nk + 1 / nr) else NA_real_
    } else {
      n_cc * ex$variance * (1 - r2)
    }
    v <- v * f
    pi <- out$risk
    s <- switch(m,
      MD = out$sd * sqrt(1 - r2_y) / sqrt(v),
      RD = sqrt(pi * (1 - pi) / v),
      RR = sqrt((1 - pi) / (pi * v)),
      OR = 1 / sqrt(v * pi * (1 - pi)),
      HR = {
        pk <- nk / (nk + nr)
        D <- if (fac) out$events_cc * (nk + nr) / n_cc else out$events_cc
        var_x <- if (fac) pk * (1 - pk) else ex$variance
        1 / sqrt(D * f * var_x * (1 - r2))
      })
    if (is.finite(s) && s >= 0) s else NA_real_
  }
  rows <- list()
  if (length(measures) && !is.null(ex) && !is.null(ci) && nrow(ci)) {
    for (i in seq_len(nrow(ci))) {
      lab <- if (ex$type == "linear") paste0(ex$var, " (per unit)") else paste0(ex$var, ": ", ci$level[i], " vs ", ex$ref)
      for (m in measures) {
        ratio <- .sr_feas_ratio(m)
        deff <- deff_for(1)
        se <- se_for(m, i, 1) * sqrt(deff)
        up <- if (mi && fnum(f_mi)) se_for(m, i, f_mi) * sqrt(deff_for(f_mi)) else NA_real_
        zs <- c(SR_FEAS_Z_CI, SR_FEAS_Z_MDE) * se
        rows[[length(rows) + 1L]] <- data.frame(
          contrast = lab, level = ci$level[i], measure = m, scale = if (ratio) "log" else "identity",
          se = se, h = if (ratio) exp(zs[1L]) else zs[1L], mde = if (ratio) exp(zs[2L]) else zs[2L],
          se_upper = up, n_k = ci$n_k[i], n_ref = ci$n_ref[i], r2x = ci$r2x[i], deff = deff,
          tier = NA_character_, stringsAsFactors = FALSE)
      }
    }
  }
  contrasts <- if (length(rows)) do.call(rbind, rows) else data.frame(
    contrast = character(), level = character(), measure = character(), scale = character(),
    se = numeric(), h = numeric(), mde = numeric(), se_upper = numeric(), n_k = integer(), n_ref = integer(),
    r2x = numeric(), deff = numeric(), tier = character(), stringsAsFactors = FALSE)
  rownames(contrasts) <- NULL
  if (!length(blockers) && nrow(contrasts) && anyNA(contrasts$se)) {
    bad <- contrasts[is.na(contrasts$se), , drop = FALSE]
    block("the precision of ", paste0(bad$contrast, " (", bad$measure, ")", collapse = ", "), " could not be computed")
  }

  # the tiers
  tier_row <- function(m, se) {
    if (is.null(delta) || m != delta$measure) return("precision_only")
    d <- if (.sr_feas_ratio(m)) abs(log(delta$value)) else delta$value
    if (SR_FEAS_Z_MDE * se <= d) "adequate" else if (SR_FEAS_Z_CI * se > d) "inadequate" else "limited"
  }
  if (length(blockers)) {
    contrasts$tier <- rep("cannot_run", nrow(contrasts))
    tier <- "cannot_run"
  } else if (!length(measures) || !nrow(contrasts)) {
    tier <- "not_applicable"
  } else {
    contrasts$tier <- mapply(tier_row, contrasts$measure, contrasts$se, USE.NAMES = FALSE)
    tier <- if (is.null(delta)) "precision_only" else {
      dt <- contrasts$tier[contrasts$measure == delta$measure]
      .sr_feas_delta_tiers[max(match(dt, .sr_feas_delta_tiers))]
    }
  }
  advice <- switch(tier, inadequate = c("revise", "stop", "exploratory"), cannot_run = c("revise", "stop"), "proceed")
  list(contrasts = contrasts, blockers = unique(blockers), warnings = warns, tier = tier, advice = advice)
}
