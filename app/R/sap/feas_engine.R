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
#' @param df the frame .sr_extract_frame() returns for sr_spec_cols(spec) (any row order)
#' @param spec the analysis spec
#' @param route the route (an sr_route) or its id
#' @return list(kind, n_rows, n_cc, n_obs, outcome, exposure, contrasts_in, clusters, params,
#'   blockers); see the Task 1 report of P1c.1 for every field. For kind "none" only n_rows, n_cc and
#'   n_obs are computed (outcome, exposure, contrasts_in and clusters NULL, params NA).
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

# The engine's own frames, by path in the inputs. Their column names are the engine's fixed vocabulary
# (time / event, level / n_all / n_cc, n_k / n_ref / ...), never data columns.
.sr_feas_struct_paths <- list(c("outcome", "pairs"), c("exposure", "levels"), "contrasts_in")

#' `x` (inputs or an assessment) through the outcome-blind gate (.sr_assert_blind), with the
#' analysis's outcome columns (outcome, time, status) against its other spec columns. Without a spec
#' only the names are checked. The engine's own frames (.sr_feas_struct_paths) are walked with their
#' column names dropped, so a spec column that happens to share one (a covariate named `event`, an
#' outcome named `level`) is not taken for a crossing; their values are still walked. Returns x
#' invisibly; stops naming the offending element.
.sr_feas_blind_frame <- function(x, spec = NULL) {
  oc <- c(.sr_spec_chr(spec[["outcome"]]), .sr_spec_chr(spec[["time"]]), .sr_spec_chr(spec[["status"]]))
  chk <- x
  if (is.list(chk) && !is.data.frame(chk)) {
    for (p in .sr_feas_struct_paths) {
      el <- chk
      for (k in p) el <- if (is.list(el) && !is.data.frame(el)) el[[k]] else NULL
      if (is.data.frame(el)) chk[[p]] <- unname(as.list(el))
    }
  }
  .sr_assert_blind(chk, oc, setdiff(sr_spec_cols(spec), oc))
  invisible(x)
}
