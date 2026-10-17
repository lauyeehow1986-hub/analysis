# routes/lm.R - the linear regression routes: lm@1 (explain-causal) and its describe twin
# lm_describe@1, which share the compile and present functions. The model is
#   outcome ~ exposure + terms (+ interactions)
# fitted by stats::lm on the complete cases (rows with a missing outcome, term or cluster value are
# dropped and counted), with the standard errors of .h_vcov (model, robust HC3 or cluster HC1).
#
# The compiled code is data-free: sr_terms_code() supplies the setup (factor levels and spline knots
# fixed from `sr_data`) and the design builder, so every column the model sees is a generated name
# (t1, t2f, t3_s1, ...), never user text. The value it returns is plain data: the coefficient table
# (derived names), the fit summary and the figure data. present() maps the derived names back to the
# user variables with sr_coef_source(), never by guessing from the text.
#
# Intervals: a t interval on the residual degrees of freedom; for se = "cluster" on min(residual df,
# G - 1), G the number of clusters (the usual small-sample choice).
#
# Effects: the exposure coefficient(s) are role "primary", measure "MD", scale "difference" -- one row
# for a linear exposure, one per non-reference level for a factor exposure (term "<exposure>: <level>
# vs <reference>", level = the level). Every other coefficient is role "covariate", measure "coef"
# (the Table 2 flag is added by sr_result_check). An interaction coefficient is not the main effect of
# anything: it is role "covariate" too, and when the exposure takes part in an interaction the
# exposure row is marked conditional (it is the effect where the interacting variable is 0 or at its
# reference level).
#
# Multiple imputation (layers$missing = "mi" with spec$mi; impute/impute.R): the model is fitted in
# every completed dataset (the outcome is imputed too and those rows are analysed) and pooled by
# Rubin's rules, t intervals on the Barnard-Rubin df (complete-data df = the residual df). There is no
# R-squared, residual SD or residual figure under MI. MI with clustered data is refused.
#
# The bootstrap (layers$resampling = "bootstrap", spec$boot) is resample/boot.R: the primary rows take
# bootstrap intervals; with clustered data spec$boot_type = "cluster" resamples whole clusters.
#
# Not yet implemented (refused at compile, never silently ignored): weights, matching and the
# bayesian framework.
#
# Marginal effect (marginal = TRUE, default FALSE): the standardised mean difference by g-computation
# (routes/marginal.R), reported as an extra role-primary MD row "<exposure> (marginal)" and a
# "marginal" table, with a delta-method interval (t on the same df as the coefficients). Without an
# exposure interaction it equals the coefficient; with one it is the average effect, which the
# coefficient row is not. standardize_over = "analysis" (default) averages over the rows the model
# was fitted on, "all" also over the rows lost only to a missing outcome. The exposure must be binary
# (0/1) or categorical (exposure_ref); a continuous exposure with marginal = TRUE is refused at run time.

.sr_lm_ses <- c("model", "robust", "cluster")

# The effective SE type of a spec.
.sr_lm_se <- function(spec) {
  se <- spec[["se"]]
  if (is.null(se)) "model" else se
}

# Cross-field validation of a spec (the schema has been checked already); stops with the reason.
.sr_lm_check <- function(spec) {
  one <- function(x) is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)
  if (!one(spec[["outcome"]])) stop("outcome must be one column name", call. = FALSE)
  if (!one(spec[["exposure"]])) stop("exposure must be one column name", call. = FALSE)
  if (identical(spec[["outcome"]], spec[["exposure"]])) {
    stop("the outcome and the exposure must be different columns", call. = FALSE)
  }
  .sr_exclude_check(spec)
  if (identical(.sr_lm_se(spec), "cluster")) {
    if (!one(spec[["cluster"]])) {
      stop("se = \"cluster\" needs a cluster column (spec$cluster)", call. = FALSE)
    }
    if (!isTRUE(spec[["layers"]][["clustered"]])) {
      stop("se = \"cluster\" needs layers$clustered = TRUE", call. = FALSE)
    }
  }
  invisible(TRUE)
}

# The estimand, a function of the spec so that it follows the missing-data layer; it is evaluated by
# sr_spec_validate() once the schema holds, which is where the cross-field checks run too (ruling R18:
# the route cross-field validation lives in this function-form estimand, via .sr_lm_check()).
.sr_lm_estimand <- function(spec) {
  .sr_lm_check(spec)
  mi <- identical(spec[["layers"]][["missing"]], "mi")
  marg <- isTRUE(spec[["marginal"]])
  sr_estimand(population = if (mi) "all analysed (MI)" else "complete cases",
              variables = c(spec[["exposure"]], spec[["outcome"]]),
              intercurrent = "not applicable",
              summary = if (marg) "marginal (standardised) mean difference" else "adjusted mean difference",
              level = if (marg) "marginal" else "conditional")
}

# Layers the route does not implement yet: stop rather than analyse as if they were absent.
.sr_lm_layers_check <- function(spec) {
  L <- spec[["layers"]]
  todo <- c(
    .sr_mi_layer_todo(spec),
    if (!is.null(L[["weights"]]) && !identical(L[["weights"]], "none")) paste0("weights = ", L[["weights"]]),
    if (isTRUE(L[["matching"]])) "matching",
    if (!is.null(L[["resampling"]]) && !(L[["resampling"]] %in% c("none", "bootstrap"))) paste0("resampling = ", L[["resampling"]]),
    if (!is.null(L[["framework"]]) && !identical(L[["framework"]], "frequentist")) paste0("framework = ", L[["framework"]])
  )
  if (length(todo)) {
    stop("lm@1 does not implement the layer(s): ", paste(todo, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

# ---- compile ------------------------------------------------------------------------------------

.sr_lm_compile <- function(spec) {
  .sr_lm_check(spec)
  .sr_lm_layers_check(spec)
  if (.sr_boot_on(spec)) return(.sr_boot_compile(spec, .sr_lm_compile, "lm", "lm"))
  if (.sr_mi_on(spec)) return(.sr_lm_mi_compile(spec))
  se <- .sr_lm_se(spec)
  tc <- sr_terms_code(spec)
  cl <- if (identical(se, "cluster")) spec[["cluster"]] else NULL
  rhs_vars <- all.vars(stats::as.formula(paste("~", tc$rhs)))
  mg <- .sr_marg_code(spec, rhs_vars, on = isTRUE(spec[["marginal"]]), strict = TRUE)
  used <- c(".y", rhs_vars, cl)
  cluster_arg <- if (is.null(cl)) "NULL" else sprintf("des[[%s]]", .sr_lit(cl))
  code <- c(
    tc$setup,
    .sr_inline(.sr_h_vcov, ".h_vcov"),
    .sr_inline(.sr_h_coef_table, ".h_coef_table"),
    if (isTRUE(spec[["marginal"]])) .sr_inline(.sr_h_gcomp, ".h_gcomp"),
    tc$design,
    "des <- .h_design(sr_data)",
    sr_outcome_code(spec, "continuous"),
    mg$pre,
    sprintf(".keep <- stats::complete.cases(des[%s])", .sr_lit(used)),
    "if (!any(.keep)) stop(\"no complete rows: every row has a missing value in the model\")",
    "des <- des[.keep, , drop = FALSE]",
    sprintf("fit <- stats::lm(.y ~ %s, data = des)", tc$rhs),
    "if (fit$df.residual < 1) {",
    "  stop(\"no residual degrees of freedom: \", nrow(des), \" complete rows for \", length(stats::coef(fit)), \" coefficients\")",
    "}",
    ".cf <- stats::coef(fit)",
    ".ok <- !is.na(.cf)",
    ".nm <- names(.cf)[.ok]",
    ".df <- fit$df.residual",
    ".ncl <- NA_real_",
    if (!is.null(cl)) c(
      sprintf(".ncl <- length(unique(des[[%s]]))", .sr_lit(cl)),
      "if (.ncl < 2L) stop(\"se = cluster needs at least two clusters (found \", .ncl, \")\")",
      ".df <- min(.df, .ncl - 1)"
    ),
    sprintf(".V <- .h_vcov(fit, %s, %s)[.nm, .nm, drop = FALSE]", .sr_lit(se), cluster_arg),
    mg$post,
    "tab <- .h_coef_table(.cf[.ok], .V, df = .df)",
    ".sm <- summary(fit)",
    paste0("list(coef = tab, aliased = names(.cf)[!.ok], n = nrow(des), n_total = nrow(sr_data), ",
           "df = .df, df_resid = fit$df.residual, sigma = .sm$sigma, r2 = .sm$r.squared, ",
           "adj_r2 = .sm$adj.r.squared, se_type = ", .sr_lit(se), ", n_clusters = .ncl, level = 0.95, ",
           "marginal = ", mg$value, ", ",
           "levels = .sr_fixed$levels, ",
           "fig = data.frame(fitted = unname(stats::fitted(fit)), resid = unname(stats::residuals(fit)), ",
           "std_resid = unname(stats::rstandard(fit)), leverage = unname(stats::hatvalues(fit)), ",
           "cooks = unname(stats::cooks.distance(fit)), stringsAsFactors = FALSE))")
  )
  paste(code, collapse = "\n")
}

# The compile under multiple imputation: the wrapper of sr_mi_code() around one lm fit per completed
# dataset; the pooled table has the columns of .h_rubin (est, se, lo, hi, p, df, riv, lambda, fmi, mcse).
.sr_lm_mi_compile <- function(spec) {
  .sr_mi_check(spec, "lm")
  se <- .sr_lm_se(spec)
  tc <- sr_terms_code(spec)
  marg <- isTRUE(spec[["marginal"]])
  mg <- .sr_marg_mi_code(spec, on = marg, strict = TRUE, ratio = FALSE)
  fit <- c(
    sprintf("fit <- stats::lm(.y ~ %s, data = des)", tc$rhs),
    "if (fit$df.residual < 1) {",
    "  stop(\"no residual degrees of freedom: \", nrow(des), \" rows for \", length(stats::coef(fit)), \" coefficients\")",
    "}",
    ".cf <- stats::coef(fit)",
    ".ok <- !is.na(.cf)",
    ".nm <- names(.cf)[.ok]",
    sprintf(".V <- .h_vcov(fit, %s, NULL)[.nm, .nm, drop = FALSE]", .sr_lit(se)),
    mg$fit,
    paste0("list(est = .cf[.ok], vcov = .V, dfcom = fit$df.residual, aliased = names(.cf)[!.ok], ",
           "scal = .sc, num = .num, flags = NULL, notes = character())"))
  code <- c(
    sr_mi_code(spec, fit, "continuous", helpers = c(.sr_inline(.sr_h_vcov, ".h_vcov"), if (marg) mg$helpers),
               pre = mg$pre),
    mg$post,
    paste0("list(coef = .mi$coef, aliased = .mi$aliased, n = nrow(des), n_total = nrow(sr_data), ",
           "df = .mi$dfcom, df_resid = .mi$dfcom, sigma = NA_real_, r2 = NA_real_, adj_r2 = NA_real_, ",
           "se_type = ", .sr_lit(se), ", n_clusters = NA_real_, level = 0.95, marginal = ", mg$value, ", ",
           "levels = .sr_fixed$levels, fig = NULL, mi = .mi_info)"))
  paste(code, collapse = "\n")
}

# ---- present ------------------------------------------------------------------------------------

# "NA" for NA / NaN, else the shared number format.
.sr_lm_num <- function(x) {
  x <- as.numeric(x)
  x[is.nan(x)] <- NA_real_
  sr_fmt_num(x)
}

# The user-facing label of each coefficient: `coef` the derived name, `src` its sr_coef_source() row.
.sr_lm_labels <- function(coef, src, ref_of) {
  vapply(seq_along(coef), function(i) {
    v <- src$source_var[i]
    lv <- src$level[i]
    switch(src$kind[i],
      linear = v,
      factor = {
        ref <- ref_of(coef[i])
        if (is.na(ref)) sprintf("%s: %s", v, lv) else sprintf("%s: %s vs %s", v, lv, ref)
      },
      quad = if (grepl("_q$", coef[i])) paste0(v, "^2") else v,
      rcs = if (grepl("_s[0-9]+$", coef[i])) {
        sprintf("%s (spline %s)", v, sub("^.*_s", "", coef[i]))
      } else v,
      interaction = if (is.na(lv)) v else sprintf("%s [%s]", v, lv),
      v
    )
  }, character(1))
}

.sr_lm_se_text <- function(value) {
  switch(value$se_type,
    model = "model-based standard errors",
    robust = "robust (HC3) standard errors",
    cluster = sprintf("cluster-robust (HC1) standard errors, %d clusters, t intervals on %s df",
                      as.integer(round(value$n_clusters)), format(value$df)),
    value$se_type)
}

.sr_lm_present <- function(value, spec) {
  tc <- sr_terms_code(spec)
  tab <- value$coef
  exposure <- spec[["exposure"]]
  ia <- spec[["interactions"]]
  has_ia <- any(vapply(ia, function(x) exposure %in% x, logical(1)))
  ref_of <- function(coef) {
    id <- sub("f.*$", "", coef)
    lv <- value$levels[[id]]
    if (is.null(lv)) NA_character_ else as.character(lv[1L])
  }
  if (length(value$aliased)) {
    asrc <- sr_coef_source(value$aliased, tc$coef_map)
    if (any(asrc$source_var == exposure & asrc$kind %in% c("linear", "factor"))) {
      stop("the exposure '", exposure, "' is collinear with other terms of the model; its effect is not estimable",
           call. = FALSE)
    }
  }
  is_int <- tab$term == "(Intercept)"
  b <- tab[!is_int, , drop = FALSE]
  src <- if (nrow(b)) sr_coef_source(b$term, tc$coef_map) else NULL
  mi <- !is.null(value$mi)
  if (nrow(b)) {
    lab <- .sr_lm_labels(b$term, src, ref_of)
    prim <- src$source_var == exposure & src$kind %in% c("linear", "factor")
    effects <- data.frame(
      term = lab, measure = ifelse(prim, "MD", "coef"), est = b$est, lo = b$lo, hi = b$hi, p = b$p,
      scale = "difference", level = ifelse(is.na(src$level), NA_character_, src$level),
      mcse = if (mi) b$mcse else NA_real_, role = ifelse(prim, "primary", "covariate"), conditional = prim & has_ia,
      ci_method = if (mi) "rubin" else c(model = "t_model", robust = "t_robust_hc3", cluster = "t_cluster_hc1")[[value$se_type]],
      n = value$n, events = NA_real_, stringsAsFactors = FALSE)
  } else {
    effects <- sr_effects_empty()
    lab <- character()
  }
  rownames(effects) <- NULL

  all_lab <- c(if (any(is_int)) "(Intercept)", lab)
  all_tab <- rbind(tab[is_int, , drop = FALSE], b)
  ci_name <- "Estimate (95% CI)"
  coefs <- data.frame(
    Term = all_lab,
    Est = sr_fmt_ci(all_tab$est, ifelse(is.nan(all_tab$lo), NA_real_, all_tab$lo),
                    ifelse(is.nan(all_tab$hi), NA_real_, all_tab$hi)),
    SE = .sr_lm_num(all_tab$se),
    P = ifelse(is.na(all_tab$p), "NA", sr_fmt_p(pmin(pmax(all_tab$p, 0), 1))),
    stringsAsFactors = FALSE)
  names(coefs)[2L] <- ci_name
  rownames(coefs) <- NULL
  fit_tab <- data.frame(
    Statistic = c("Rows analysed", "Rows excluded (missing)", "R-squared", "Adjusted R-squared",
                  "Residual SD", "Residual df", "Interval df"),
    Value = c(sprintf("%d", as.integer(round(value$n))),
              sprintf("%d", as.integer(round(value$n_total - value$n))),
              .sr_lm_num(value$r2), .sr_lm_num(value$adj_r2), .sr_lm_num(value$sigma),
              format(value$df_resid), format(value$df)),
    stringsAsFactors = FALSE)

  frag <- list(lm_note = sprintf(
    "Linear regression of %s on %s, adjusted for the other terms of the model; %d row(s) analysed, %d excluded for a missing value; %s.",
    spec[["outcome"]], exposure, as.integer(round(value$n)), as.integer(round(value$n_total - value$n)),
    .sr_lm_se_text(value)))
  frag$exclude_rows <- .sr_exclude_fragment(spec)
  if (mi) {
    fit_tab <- data.frame(
      Statistic = c("Rows analysed", "Imputed datasets (m)", "Complete-data residual df"),
      Value = c(sprintf("%d", as.integer(round(value$n))), sprintf("%d", as.integer(round(value$mi$m))),
                format(value$df_resid)),
      stringsAsFactors = FALSE)
    frag$lm_note <- sprintf(
      "Linear regression of %s on %s, adjusted for the other terms of the model, in each of %d imputed datasets of %d row(s); %s; pooled by Rubin's rules.",
      spec[["outcome"]], exposure, as.integer(round(value$mi$m)), as.integer(round(value$n)), .sr_lm_se_text(value))
  }
  if (length(value$aliased)) {
    alab <- .sr_lm_labels(value$aliased, asrc, ref_of)
    frag$lm_aliased <- sprintf("Dropped as perfectly collinear with the other terms: %s.",
                               paste(alab, collapse = ", "))
  }
  if (has_ia) {
    frag$lm_interaction <- paste("The exposure takes part in an interaction: its main-effect row is the effect",
                                 "where the interacting variable is 0 (or at its reference level), not an average effect.")
  }
  tables <- list(coefficients = coefs, model_fit = fit_tab)
  if (!is.null(value$marginal) && !is.null(value$marginal$rows)) {
    ci_base <- c(model = "t_model", robust = "t_robust_hc3", cluster = "t_cluster_hc1")[[value$se_type]]
    mp <- .sr_marg_present(value, exposure, "mean", ci_base)
    effects <- rbind(effects, mp$effects)
    rownames(effects) <- NULL
    tables$marginal <- mp$table
    frag$lm_marginal <- sprintf(
      "Marginal mean difference by g-computation over %d row(s) (standardize_over = %s); delta-method interval.",
      as.integer(round(value$marginal$n_over)), value$marginal$over)
    if (mi) {
      frag$lm_marginal <- sprintf(
        "Marginal mean difference by g-computation in each imputed dataset over its %d row(s), delta-method variance, pooled by Rubin's rules.",
        as.integer(round(value$marginal$n_over)))
    }
  }
  if (mi) {
    mp <- .sr_mi_present(value, "lm")
    tables$imputation <- mp$table
    frag <- c(frag, mp$fragments)
    return(list(tables = tables, figures = list(), fragments = frag, effects = effects))
  }
  list(tables = tables, figures = list(resid_panel = value$fig),
       fragments = frag, effects = effects)
}

# ---- the routes ---------------------------------------------------------------------------------

.sr_lm_schema <- function() {
  list(
    outcome = list(type = "chr", required = TRUE),
    exposure = list(type = "chr", required = TRUE),
    terms = list(type = "list", required = FALSE),
    interactions = list(type = "list", required = FALSE),
    se = list(type = "chr", required = FALSE, values = .sr_lm_ses),
    cluster = list(type = "chr", required = FALSE),
    marginal = list(type = "lgl", required = FALSE, default = FALSE),
    standardize_over = list(type = "chr", required = FALSE, values = c("analysis", "all")),
    exclude_rows = list(type = "num", required = FALSE),
    layers = list(type = "list", required = TRUE),
    seed = list(type = "num", required = TRUE)
  )
}

# The present step of both lm routes: the bootstrap bounds when the value has them (resample/boot.R).
.sr_lm_present_boot <- function(value, spec) .sr_boot_present(value, spec, .sr_lm_present, "lm")

.sr_lm_route <- function() {
  sr_route(id = "lm", version = 1L, family = "regression", purpose = "explain-causal",
           estimand = .sr_lm_estimand, spec_schema = .sr_lm_schema(),
           compile = .sr_lm_compile, present = .sr_lm_present_boot)
}

.sr_lm_describe_route <- function() {
  sr_route(id = "lm_describe", version = 1L, family = "regression", purpose = "describe",
           estimand = .sr_lm_estimand, spec_schema = .sr_lm_schema(),
           compile = .sr_lm_compile, present = .sr_lm_present_boot)
}

if (!(.sr_route_key("lm", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_lm_route())
}
if (!(.sr_route_key("lm_describe", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
  sr_route_register(.sr_lm_describe_route())
}
