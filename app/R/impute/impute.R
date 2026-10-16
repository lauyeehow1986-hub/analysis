# impute/impute.R - multiple imputation for the model routes (layers$missing = "mi"), and the
# descriptive missing-data summary.
#
# sr_mi_code() writes the wrapper every route shares. The emitted code, in order:
#   1. the setup of sr_terms_code() on `sr_data`: factor levels and spline knots are fixed from the
#      observed data BEFORE anything is imputed, so every imputed dataset has the same design columns
#      and the same coefficient names;
#   2. the imputation frame `.mi_d`, under generated names: the raw value of every model term (t1, t2,
#      ...: numeric, or a factor with the fixed levels), the outcome (.y; a 0/1 factor for a binary
#      outcome), and the auxiliary columns of spec$mi$aux (.a1, ...), which only predict. For a
#      survival model the time is not a predictor: the event indicator (.event) and the Nelson-Aalen
#      cumulative hazard (.na, mice::nelsonaalen) stand in for it, and the strata columns (.s1, ...)
#      are imputed as categorical columns. Rows with a missing time or status are dropped before the
#      imputation and counted (they are never imputed);
#   3. mice::mice(.mi_d, m, maxit, printFlag = FALSE) under the RNG stream of the run preamble, with
#      the methods pmm (numeric), logreg (two-level factor) and polyreg (unordered factor);
#   4. for each completed dataset: the imputed raw values go back under the user's column names, the
#      design is rebuilt by .h_design() ("impute, then transform": splines, squares and interactions
#      are computed from the imputed raw values with the fixed knots), and the route's fit code runs;
#   5. .h_rubin pools the estimates (impute/pool.R) with the complete-data df the fit reports
#      (n - p for a linear model, Inf otherwise).
# For lm and glm the outcome is in the imputation model and the rows with an imputed outcome are
# analysed (ruling R25b).
#
# spec$mi: m (a whole number of at least 2, or "auto"), maxit (default 10), aux (column names).
# m = "auto" runs a pilot of 20 imputations, takes the largest fraction of missing information among
# the primary terms and reimputes with the two-stage m of .h_auto_m (von Hippel 2020).
#
# Refused at compile, naming the combination (ruling R25a): MI with clustered data
# (layers$clustered = TRUE) and MI with the Firth penalty.

# ---- the layer and its validation ---------------------------------------------------------------

# Whether the spec asks for multiple imputation.
.sr_mi_on <- function(spec) identical(spec[["layers"]][["missing"]], "mi")

# Whether the multiple imputation is pre-specified: the number of imputations is given.
.sr_mi_ready <- function(spec) {
  .sr_mi_on(spec) && is.list(spec[["mi"]]) && !is.null(spec[["mi"]][["m"]])
}

# The entry of a route's "does not implement the layer(s)" message for the missing-data layer: NULL
# for complete cases and for a pre-specified multiple imputation.
.sr_mi_layer_todo <- function(spec) {
  mis <- spec[["layers"]][["missing"]]
  if (is.null(mis) || identical(mis, "cc") || .sr_mi_ready(spec)) return(NULL)
  if (identical(mis, "mi")) {
    return("missing = mi (multiple imputation) without spec$mi$m (the number of imputations: a whole number of at least 2, or \"auto\")")
  }
  paste0("missing = ", paste(format(mis), collapse = ","))
}

# Validation of spec$mi and of the combinations multiple imputation does not support; stops with the
# reason. `id` is the route id for the messages.
.sr_mi_check <- function(spec, id) {
  mi <- spec[["mi"]]
  whole <- function(x, lo) {
    is.numeric(x) && length(x) == 1L && !is.na(x) && is.finite(x) && x == round(x) && x >= lo
  }
  m <- mi[["m"]]
  if (!(identical(m, "auto") || whole(m, 2))) {
    stop("spec$mi$m must be a whole number of at least 2, or \"auto\" (the number of imputations)", call. = FALSE)
  }
  maxit <- mi[["maxit"]]
  if (!is.null(maxit) && !whole(maxit, 1)) {
    stop("spec$mi$maxit must be a whole number of at least 1 (the iterations of the chained equations; default 10)",
         call. = FALSE)
  }
  aux <- mi[["aux"]]
  if (!is.null(aux)) {
    aux <- unlist(aux)
    if (!is.character(aux) || anyNA(aux) || any(!nzchar(aux)) || anyDuplicated(aux)) {
      stop("spec$mi$aux must be a character vector of distinct column names (the auxiliary columns)", call. = FALSE)
    }
    model <- c(.sr_spec_chr(spec[["outcome"]]), .sr_spec_chr(spec[["time"]]), .sr_spec_chr(spec[["status"]]),
               .sr_spec_chr(spec[["exposure"]]),
               .sr_spec_chr(unlist(lapply(spec[["terms"]], function(t) t[["var"]]))),
               .sr_spec_chr(spec[["cluster"]]), .sr_spec_chr(spec[["strata"]]))
    both <- intersect(aux, model)
    if (length(both)) {
      stop("spec$mi$aux names column(s) that are already in the model: ", paste(both, collapse = ", "),
           " (an auxiliary column only predicts the missing values)", call. = FALSE)
    }
  }
  if (isTRUE(spec[["layers"]][["clustered"]])) {
    stop(id, "@1 does not combine multiple imputation (layers$missing = \"mi\") with clustered data ",
         "(layers$clustered = TRUE): the imputation model would have to respect the clusters", call. = FALSE)
  }
  if (identical(spec[["penalty"]], "firth")) {
    stop(id, "@1 does not combine multiple imputation (layers$missing = \"mi\") with the Firth penalty ",
         "(penalty = \"firth\"): profile-likelihood intervals do not pool by Rubin's rules", call. = FALSE)
  }
  invisible(TRUE)
}

# ---- helpers inlined into the emitted code ------------------------------------------------------

#' A column of the imputation frame that is not a model term: numeric (or logical) as numeric,
#' anything else (and everything when `cat` is TRUE) as a factor of its observed values.
.sr_h_mi_col <- function(x, cat = FALSE) {
  if (!cat && (is.numeric(x) || is.logical(x))) return(as.numeric(x))
  factor(as.character(x), levels = as.character(sort(unique(x[!is.na(x)]))))
}

#' Impute `d` m times, fit every completed dataset and pool. Inlined as .h_mi_run.
#' @param d the imputation frame
#' @param m,maxit the number of imputations and of iterations
#' @param fit_one function(completed) -> list(est, vcov, dfcom, aliased, scal, num, flags, notes):
#'   named coefficients and their covariance, the complete-data df, the aliased coefficient names,
#'   optional scalar estimates data.frame(key, est, se, prim) pooled one by one, a named numeric
#'   vector to average, a named logical vector to combine by any(), and notes to collect
#' @param rubin the pooling function (.h_rubin)
#' @param need the columns of `d` the analysis reads (they must be complete after the imputation)
#' @param prim the coefficient names that are primary-role terms (NULL: all of them)
#' @return list(coef, scal, fmi_max, coef_names, num, flags, notes, aliased, methods, predictors,
#'   n_missing, n_logged, m, maxit, dfcom)
.sr_h_mi_run <- function(d, m, maxit, fit_one, rubin, need, prim = NULL) {
  imp <- suppressWarnings(mice::mice(d, m = m, maxit = maxit, printFlag = FALSE,
                                     defaultMethod = c("pmm", "logreg", "polyreg", "polr")))
  fits <- vector("list", m)
  for (i in seq_len(m)) {
    ci <- mice::complete(imp, i)
    left <- need[vapply(need, function(v) anyNA(ci[[v]]), logical(1))]
    if (length(left)) {
      stop("the imputation left missing values in: ", paste(left, collapse = ", "),
           " (mice could not impute them, for example a constant or collinear column)")
    }
    fits[[i]] <- fit_one(ci)
  }
  dfs <- vapply(fits, function(f) as.numeric(f$dfcom), numeric(1))
  if (!all(dfs == dfs[1L])) {
    stop("design differs across imputations: the complete-data degrees of freedom are not the same")
  }
  dfcom <- dfs[1L]
  coef <- rubin(lapply(fits, function(f) f$est), lapply(fits, function(f) f$vcov), dfcom)
  scal <- NULL
  first <- fits[[1L]]
  if (!is.null(first$scal) && nrow(first$scal)) {
    keys <- as.character(first$scal$key)
    for (f in fits) {
      if (!identical(as.character(f$scal$key), keys)) {
        stop("design differs across imputations: the pooled quantities are not the same")
      }
    }
    scal <- rubin(lapply(fits, function(f) stats::setNames(as.numeric(f$scal$est), keys)),
                  lapply(fits, function(f) {
                    v <- diag(as.numeric(f$scal$se)^2, nrow = length(keys))
                    dimnames(v) <- list(keys, keys)
                    v
                  }), dfcom)
    scal$prim <- as.logical(first$scal$prim)
  }
  fm <- c(if (is.null(prim)) coef$fmi else coef$fmi[coef$term %in% prim],
          if (!is.null(scal)) scal$fmi[scal$prim])
  fm <- fm[is.finite(fm)]
  num <- NULL
  if (length(first$num)) {
    num <- stats::setNames(numeric(length(first$num)), names(first$num))
    for (f in fits) num <- num + as.numeric(f$num[names(num)])
    num <- num / m
  }
  flags <- NULL
  if (length(first$flags)) {
    flags <- stats::setNames(logical(length(first$flags)), names(first$flags))
    for (f in fits) flags <- flags | as.logical(f$flags[names(flags)])
  }
  notes <- character()
  for (f in fits) notes <- unique(c(notes, f$notes))
  list(coef = coef, scal = scal, fmi_max = if (length(fm)) max(fm) else 0,
       coef_names = lapply(fits, function(f) names(f$est)), num = num, flags = flags, notes = notes,
       aliased = first$aliased, methods = imp$method, predictors = imp$predictorMatrix,
       n_missing = vapply(d, function(x) sum(is.na(x)), numeric(1)),
       n_logged = if (is.null(imp$loggedEvents)) 0L else nrow(imp$loggedEvents),
       m = as.integer(m), maxit = as.integer(maxit), dfcom = dfcom)
}

#' The rows of a marginal block from the pooled scalars of .h_mi_run: one row per contrast with the
#' averaged standardised means and the pooled difference (keys rd<j>) and log ratio (keys lrr<j>).
#' Inlined as .h_mi_marg.
.sr_h_mi_marg <- function(scal, num, levels) {
  one <- function(key, col) {
    r <- scal[scal$term == key, , drop = FALSE]
    if (nrow(r)) r[[col]][1L] else NA_real_
  }
  out <- NULL
  for (j in seq_along(levels)) {
    rd <- paste0("rd", j)
    rr <- paste0("lrr", j)
    out <- rbind(out, data.frame(
      level = levels[j], p1 = num[[paste0("p1_", j)]], p0 = num[[paste0("p0_", j)]],
      rd = one(rd, "est"), rd_se = one(rd, "se"), log_rr = one(rr, "est"), log_rr_se = one(rr, "se"),
      rd_lo = one(rd, "lo"), rd_hi = one(rd, "hi"), rd_p = one(rd, "p"), rd_mcse = one(rd, "mcse"),
      log_rr_lo = one(rr, "lo"), log_rr_hi = one(rr, "hi"), log_rr_p = one(rr, "p"),
      log_rr_mcse = one(rr, "mcse"), stringsAsFactors = FALSE))
  }
  out
}

# ---- the emitted wrapper ------------------------------------------------------------------------

#' The multiple-imputation wrapper of a model route.
#'
#' The emitted code defines `des` (the design of the observed rows kept), `.mi_base` (their raw
#' columns), `.mi_dropped`, then runs `pre`, imputes, fits and pools, and leaves
#'   .mi       the value of .h_mi_run (pooled `coef`, pooled `scal`, averaged `num`, `flags`, ...)
#'   .mi_info  the `mi` field of the route value: m, maxit, m_pilot, fmi_max, m_final, m_capped,
#'             columns (column, source, role, method, n_missing), predictors, coef_names, knots,
#'             n_dropped, n_logged, dfcom, aux.
#' The route appends its own value expression.
#' @param spec the analysis spec (spec$mi validated by .sr_mi_check)
#' @param fit_code lines fitting one completed dataset: they see `des` (the completed design) and end
#'   in list(est, vcov, dfcom, aliased, scal, num, flags, notes), see .sr_h_mi_run
#' @param kind the outcome coding: "continuous", "binary" or "surv"
#' @param helpers further inlined helpers the fit code needs
#' @param pre lines run on the observed rows before the imputation (checks, descriptive figures)
#' @param group TRUE when the first term is the exposure as a grouping factor and every pooled
#'   estimate is a primary term (RMST)
#' @return one string of R code
sr_mi_code <- function(spec, fit_code, kind = c("continuous", "binary", "surv"), helpers = NULL,
                       pre = NULL, group = FALSE) {
  kind <- match.arg(kind)
  if (!is.character(fit_code) || !length(fit_code) || anyNA(fit_code)) {
    stop("sr_mi_code: fit_code must be the lines that fit one completed dataset", call. = FALSE)
  }
  mi <- spec[["mi"]]
  auto <- identical(mi[["m"]], "auto")
  maxit <- if (is.null(mi[["maxit"]])) 10L else as.integer(mi[["maxit"]])
  aux <- .sr_spec_chr(mi[["aux"]])
  st <- if (kind == "surv") .sr_spec_chr(spec[["strata"]]) else character()
  tc <- sr_terms_code(spec)
  plan <- .sr_term_plan(spec)

  col <- character()
  src <- character()
  role <- character()
  frame <- character()
  back <- character()
  after <- character()
  for (i in seq_along(plan)) {
    it <- plan[[i]]
    raw <- sprintf(".mi_base[[%s]]", .sr_lit(it$var))
    fac <- identical(it$type, "factor")
    frame <- c(frame, sprintf(".mi_d[[%s]] <- %s", .sr_lit(it$id), if (fac) {
      sprintf("factor(as.character(%s), levels = .sr_fixed$levels[[%s]])", raw, .sr_lit(it$id))
    } else {
      sprintf(".h_num(%s, %s)", raw, .sr_lit(it$var))
    }))
    back <- c(back, sprintf("  raw[[%s]] <- %s", .sr_lit(it$var), if (fac) {
      sprintf("as.character(ci[[%s]])", .sr_lit(it$id))
    } else {
      sprintf("ci[[%s]]", .sr_lit(it$id))
    }))
    col <- c(col, it$id)
    src <- c(src, it$var)
    role <- c(role, if (identical(it$role, "exposure") || (isTRUE(group) && i == 1L)) "exposure" else "term")
  }
  if (kind == "surv") {
    frame <- c(frame,
      ".mi_d[[\".event\"]] <- as.numeric(des[[\".event\"]])",
      paste0(".mi_d[[\".na\"]] <- mice::nelsonaalen(data.frame(.time = des[[\".time\"]], ",
             ".event = des[[\".event\"]]), .time, .event)"))
    after <- c(after, "  out[[\".time\"]] <- des[[\".time\"]]", "  out[[\".event\"]] <- des[[\".event\"]]")
    col <- c(col, ".event", ".na")
    src <- c(src, spec[["status"]], "(Nelson-Aalen cumulative hazard)")
    role <- c(role, "event", "nelson_aalen")
  } else {
    frame <- c(frame, if (kind == "binary") {
      ".mi_d[[\".y\"]] <- factor(des[[\".y\"]], levels = c(0L, 1L))"
    } else {
      ".mi_d[[\".y\"]] <- des[[\".y\"]]"
    })
    after <- c(after, if (kind == "binary") {
      "  out[[\".y\"]] <- as.integer(as.character(ci[[\".y\"]]))"
    } else {
      "  out[[\".y\"]] <- ci[[\".y\"]]"
    })
    col <- c(col, ".y")
    src <- c(src, spec[["outcome"]])
    role <- c(role, "outcome")
  }
  for (i in seq_along(st)) {
    nm <- paste0(".s", i)
    frame <- c(frame, sprintf(".mi_d[[%s]] <- .h_mi_col(.mi_base[[%s]], TRUE)", .sr_lit(nm), .sr_lit(st[i])))
    after <- c(after, sprintf("  out[[%s]] <- ci[[%s]]", .sr_lit(nm), .sr_lit(nm)))
    col <- c(col, nm)
    src <- c(src, st[i])
    role <- c(role, "strata")
  }
  for (i in seq_along(aux)) {
    nm <- paste0(".a", i)
    frame <- c(frame, sprintf(".mi_d[[%s]] <- .h_mi_col(.mi_base[[%s]])", .sr_lit(nm), .sr_lit(aux[i])))
    col <- c(col, nm)
    src <- c(src, aux[i])
    role <- c(role, "aux")
  }
  prim <- if (isTRUE(group) || !length(plan) || !identical(plan[[1L]]$role, "exposure")) {
    "NULL"
  } else if (identical(plan[[1L]]$type, "factor")) {
    "paste0(\"t1f\", .sr_fixed$levels[[\"t1\"]][-1L])"
  } else {
    "\"t1\""
  }
  run <- function(m_expr) {
    sprintf(".h_mi_run(.mi_d, %s, %s, .h_mi_fit, .h_rubin, .mi_need, .mi_prim)", m_expr, .sr_lit(maxit))
  }
  code <- c(
    tc$setup,
    if (length(aux)) c(
      sprintf(".miss <- setdiff(%s, names(sr_data))", .sr_lit(aux)),
      "if (length(.miss)) stop(\"column(s) not in the data: \", paste(.miss, collapse = \", \"))"
    ),
    .sr_inline(.sr_h_rubin, ".h_rubin"),
    .sr_inline(.sr_h_mi_run, ".h_mi_run"),
    if (auto) .sr_inline(.sr_h_auto_m, ".h_auto_m"),
    if (length(st) || length(aux)) .sr_inline(.sr_h_mi_col, ".h_mi_col"),
    helpers,
    tc$design,
    "des <- .h_design(sr_data)",
    sr_outcome_code(spec, kind),
    if (kind == "surv") {
      c(".mi_keep <- !is.na(des[[\".time\"]]) & !is.na(des[[\".event\"]])",
        "if (!any(.mi_keep)) stop(\"no rows left: every row has a missing time or status\")")
    } else {
      ".mi_keep <- rep(TRUE, nrow(des))"
    },
    ".mi_dropped <- sum(!.mi_keep)",
    "des <- des[.mi_keep, , drop = FALSE]",
    ".mi_base <- as.data.frame(sr_data)[.mi_keep, , drop = FALSE]",
    pre,
    ".mi_d <- list()",
    frame,
    ".mi_d <- data.frame(.mi_d, check.names = FALSE, stringsAsFactors = FALSE)",
    ".h_mi_des <- function(ci) {",
    "  raw <- .mi_base",
    back,
    "  out <- .h_design(raw)",
    after,
    "  out",
    "}",
    ".h_mi_fit <- function(ci) {",
    "  des <- .h_mi_des(ci)",
    paste0("  ", fit_code),
    "}",
    sprintf(".mi_need <- %s", .sr_lit(col[role != "aux"])),
    sprintf(".mi_prim <- %s", prim),
    if (auto) {
      c(sprintf(".mi_pilot <- %s", run("20L")),
        ".mi_auto <- .h_auto_m(.mi_pilot$fmi_max)",
        sprintf(".mi <- %s", run(".mi_auto$m_final")))
    } else {
      m <- .sr_lit(as.integer(mi[["m"]]))
      c(sprintf(".mi <- %s", run(m)),
        sprintf(".mi_auto <- list(m_pilot = NA_integer_, fmi_max = .mi$fmi_max, m_final = %s, m_capped = FALSE)", m))
    },
    sprintf(".mi_cols <- data.frame(column = %s, source = %s, role = %s, stringsAsFactors = FALSE)",
            .sr_lit(col), .sr_lit(src), .sr_lit(role)),
    ".mi_cols$method <- unname(.mi$methods[.mi_cols$column])",
    ".mi_cols$n_missing <- unname(.mi$n_missing[.mi_cols$column])",
    paste0(".mi_info <- list(m = .mi$m, maxit = .mi$maxit, m_pilot = .mi_auto$m_pilot, ",
           "fmi_max = .mi_auto$fmi_max, m_final = .mi_auto$m_final, m_capped = .mi_auto$m_capped, ",
           "columns = .mi_cols, predictors = .mi$predictors, coef_names = .mi$coef_names, ",
           "knots = .sr_fixed$knots, n_dropped = .mi_dropped, n_logged = .mi$n_logged, ",
           "dfcom = .mi$dfcom, aux = ", .sr_lit(aux), ")")
  )
  paste(code, collapse = "\n")
}

# ---- the presented pieces every route shares ----------------------------------------------------

#' The imputation table and the fragments of a multiply imputed result.
#' @param value the route value (its `mi` field is the .mi_info of sr_mi_code)
#' @param kind "lm" (identity scale, t on the Barnard-Rubin df), "glm" or "cox" (log scale, normal),
#'   "rmst" (identity scale, normal)
#' @return list(table, fragments): fragments mi_note, mi_auto (m = "auto" only) and mi_logged (only
#'   when mice logged events)
.sr_mi_present <- function(value, kind = c("lm", "glm", "cox", "rmst")) {
  kind <- match.arg(kind)
  mi <- value$mi
  cols <- mi$columns
  n <- as.numeric(value$n)
  nmis <- as.numeric(cols$n_missing)
  meth <- as.character(cols$method)
  role_label <- c(exposure = "exposure", term = "model term", outcome = "outcome", event = "event indicator",
                  nelson_aalen = "Nelson-Aalen cumulative hazard", strata = "stratum", aux = "auxiliary (predictor only)")
  table <- data.frame(
    Column = cols$source, Role = unname(role_label[cols$role]),
    Missing = sprintf("%d (%s%%)", as.integer(round(nmis)), sr_fmt_num(if (n > 0) 100 * nmis / n else NA_real_, digits = 1L)),
    Method = ifelse(nzchar(meth), meth, "(none: complete)"),
    stringsAsFactors = FALSE)
  imputed <- nzchar(meth)
  how <- if (any(imputed)) {
    paste0("imputation methods: ", paste(sprintf("%s (%s)", cols$source[imputed], meth[imputed]), collapse = ", "),
           " (pmm = predictive mean matching for a numeric column, logreg = logistic regression for a two-level ",
           "factor, polyreg = multinomial regression for an unordered factor)")
  } else {
    "no column had a missing value, so nothing was imputed"
  }
  who <- if (kind %in% c("cox", "rmst")) {
    sprintf(paste0("The event indicator and the Nelson-Aalen cumulative hazard are predictors of the imputation model ",
                   "(the time itself is not); time and status are never imputed: %d row(s) with a missing time or ",
                   "status were dropped before the imputation."), as.integer(round(mi$n_dropped)))
  } else {
    sprintf("The outcome is part of the imputation model and the %d row(s) with an imputed outcome are analysed.",
            as.integer(round(nmis[cols$role == "outcome"][1L])))
  }
  extra <- c(
    if (any(cols$role == "strata")) "The strata columns are imputed as categorical columns.",
    if (any(cols$role == "aux")) {
      sprintf("The auxiliary column(s) %s predict the missing values and are not in the analysis model.",
              paste(cols$source[cols$role == "aux"], collapse = ", "))
    })
  pool <- switch(kind,
    lm = "on the identity scale, with t intervals on the Barnard-Rubin degrees of freedom",
    glm = "on the log scale (ratios are exponentiated after pooling; a marginal risk difference is pooled on the identity scale), with normal intervals",
    cox = "on the log scale (hazard ratios are exponentiated after pooling), with normal intervals",
    rmst = "on the identity scale, with normal intervals")
  note <- paste(c(
    sprintf("Missing values were multiply imputed by chained equations (mice): m = %d imputed datasets, %d iteration(s) each; %s.",
            as.integer(round(mi$m)), as.integer(round(mi$maxit)), how),
    who, extra,
    paste0("Assumption (impute, then transform): the raw values were imputed and the derived terms (splines, squares, ",
           "interactions) were computed afterwards, with the knots and factor levels fixed from the observed data."),
    sprintf("The estimates of the %d analyses were pooled with Rubin's rules %s; the reported Monte Carlo error is sqrt(B / m) on the pooling scale.",
            as.integer(round(mi$m)), pool)), collapse = " ")
  frag <- list(mi_note = note)
  if (!is.na(mi$m_pilot)) {
    frag$mi_auto <- sprintf(
      paste0("The number of imputations was chosen in two stages (von Hippel 2020): a pilot of %d imputations gave a ",
             "largest fraction of missing information of %s among the primary terms, for which the rule asks for m = %d%s; ",
             "the reported analysis reimputed with that m."),
      as.integer(round(mi$m_pilot)), sr_fmt_num(mi$fmi_max, digits = 3L), as.integer(round(mi$m_final)),
      if (isTRUE(mi$m_capped)) " (the cap of 200: the rule asked for more)" else "")
  }
  if (isTRUE(mi$n_logged > 0)) {
    frag$mi_logged <- sprintf(
      "mice logged %d event(s): a constant or collinear column was left out of an imputation model.",
      as.integer(round(mi$n_logged)))
  }
  list(table = table, fragments = frag)
}

# ---- the missing-data summary -------------------------------------------------------------------

#' Missing values of a stored table: per column, and the most frequent missingness patterns.
#' Descriptive only: it does not test or choose anything.
#' @param project an open project
#' @param table name of a table of the project store
#' @param cols the columns to summarise
#' @return list(columns = data.frame(column, n_missing, pct_missing), patterns = data.frame(missing,
#'   n_missing_cols, n, pct)): the 10 most frequent patterns, most frequent first; `missing` names the
#'   columns missing in the pattern (in the order of `cols`, "" for the complete rows)
sr_missing_summary <- function(project, table, cols) {
  .sr_store_check_table(table)
  if (!is.character(cols) || !length(cols) || anyNA(cols) || any(!nzchar(cols))) {
    stop("cols must be a character vector naming at least one column", call. = FALSE)
  }
  cols <- unique(cols)
  if (!(table %in% sr_store_tables(project))) {
    stop("table '", table, "' is not in the project store", call. = FALSE)
  }
  have <- vapply(sr_store_manifest(project, table)$schema, function(s) s$name, "")
  unknown <- setdiff(cols, have)
  if (length(unknown)) {
    stop("unknown column(s) for table '", table, "': ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  sr_store_view(project, table)
  con <- sr_duck(project)
  qc <- as.character(DBI::dbQuoteIdentifier(con, cols))
  qt <- as.character(DBI::dbQuoteIdentifier(con, table))
  k <- length(cols)
  cnt <- sr_duck_query(con, sprintf("SELECT count(*) AS n_rows, %s FROM %s",
                                    paste(sprintf("count(%s) AS c%d", qc, seq_len(k)), collapse = ", "), qt))
  n <- as.numeric(cnt$n_rows[1L])
  n_missing <- n - vapply(seq_len(k), function(i) as.numeric(cnt[[paste0("c", i)]][1L]), numeric(1))
  columns <- data.frame(column = cols, n_missing = n_missing,
                        pct_missing = if (n > 0) 100 * n_missing / n else rep(NA_real_, k),
                        stringsAsFactors = FALSE)
  flags <- paste0("m", seq_len(k))
  pat <- sr_duck_query(con, sprintf(
    "SELECT %s, count(*) AS n_rows FROM %s GROUP BY %s ORDER BY n_rows DESC, %s LIMIT 10",
    paste(sprintf("(%s IS NULL) AS %s", qc, flags), collapse = ", "), qt,
    paste(seq_len(k), collapse = ", "), paste(flags, collapse = ", ")))
  miss <- as.matrix(pat[flags]) == TRUE
  patterns <- data.frame(
    missing = vapply(seq_len(nrow(pat)), function(i) paste(cols[miss[i, ]], collapse = ", "), character(1)),
    n_missing_cols = as.numeric(rowSums(miss)), n = as.numeric(pat$n_rows),
    pct = if (n > 0) 100 * as.numeric(pat$n_rows) / n else rep(NA_real_, nrow(pat)),
    stringsAsFactors = FALSE)
  list(columns = columns, patterns = patterns)
}
