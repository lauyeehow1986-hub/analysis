# diagnostics/checks.R - the model diagnostics, each judged by an effect size, never by a p-value.
#
# A diagnostic is emitted code of kind "diagnostic" (routes/emit.R): it may run a test such as
# cox.zph, but only to REPORT its statistic; no branch, subscript or verdict depends on it. The
# verdict ok / warn / fail compares one effect-size value with the fixed thresholds of the catalogue
# (sr_diag_checks), using exactly the > / >= / < of the table. Each check returns plain data:
#   list(value, verdict, detail, plot_data, reported, ...)
# plus tau_suggest (ph: the 75th centile of the event times, the horizon of the rmst sensitivity),
# rows (influence: the flagged rows as positions in sr_data) and var (linearity: the worst term).
#
# The checks fit the analysis model ONCE on the analysis data (the complete cases of the analysis
# columns, as the route does): a bootstrap layer is ignored and, under multiple imputation, the check
# runs on the complete cases; `detail` says so. A Firth model is diagnosed through its unpenalised
# fit (separation does not apply to it). plot_data is capped at 5000 points by a seeded sample that
# keeps the flagged rows, also stated in `detail`.
#
# sr_diag_run() runs the applicable checks of an analysis that has run successfully, through the
# same extract, prologue and fits cache as sr_run(), and records one diag.completed per check; the
# reducer keeps state$diag[[analysis]][[check]]. It never adds or changes an analysis: acting on a
# verdict (the `sensitivity` of the catalogue) is the job of the SAP rules.

# ---- the catalogue ------------------------------------------------------------------------------

# One row per check and route group; cmp is how value meets a threshold (ge: >=, gt: >, lt: <).
.sr_diag_table <- function() {
  data.frame(
    id = c("ph", "influence", "influence", "separation", "few_clusters", "inf_censoring", "linearity",
           "heteroscedasticity"),
    routes = c("cox", "lm,logistic,modpois", "cox", "logistic", "lm,logistic,modpois,cox", "cox,rmst",
               "lm,logistic,modpois,cox", "lm"),
    metric = c(
      "|log HR(early) - log HR(late)| of the exposure, split at the median event time (cox.zph reported only)",
      "max Cook's distance",
      "max |scaled dfbeta| of the exposure",
      "refit with maxit = 100, epsilon = 1e-12: some |beta| grows by > 1 and some fitted p is within 1e-8 of 0 or 1 (1 = yes)",
      "number of clusters",
      "max |SMD| of the baseline terms, censored before the median follow-up vs not",
      "max |linear - RCS(4) partial prediction| over the 5th-95th centile (outcome SD for lm, else the linear-predictor scale)",
      "robust (HC3) SE / model SE of the exposure"),
    warn = c(log(1.25), 0.5, 0.5, NA_real_, 50, 0.1, 0.1, 1.2),
    fail = c(log(1.5), 1, 1, 1, 30, 0.2, 0.25, 1.5),
    cmp = c("ge", "gt", "ge", "ge", "lt", "gt", "gt", "gt"),
    sensitivity = c(
      "rmst: tau = the pre-specified tau, else tau_suggest (the 75th centile of the event times)",
      "same route: exclude the rows with Cook's D > 1",
      "same route: exclude the rows with |scaled dfbeta| >= 1",
      "logistic: penalty = firth",
      "same route: layers$resampling = bootstrap, boot_type = cluster, boot = list(B = 2000, type = percentile)",
      "none: a limitation note (informative censoring; IPCW is P9)",
      "same route: the flagged term as rcs with 4 knots",
      "same route: se = robust"),
    stringsAsFactors = FALSE)
}

#' The diagnostic catalogue: one row per check (influence has one row for lm / logistic / modpois and
#' one for cox), with its routes, metric, warn and fail thresholds and the sensitivity analysis a
#' failing verdict suggests.
#' @return data.frame(id, routes, metric, warn, fail, sensitivity)
sr_diag_checks <- function() {
  .sr_diag_table()[c("id", "routes", "metric", "warn", "fail", "sensitivity")]
}

# The base route of a route id: a describe twin gets the checks of its route.
.sr_diag_base <- function(route) sub("_describe$", "", route)

.sr_diag_routes <- c("lm", "logistic", "modpois", "cox", "rmst")

# The catalogue row of a check for a base route.
.sr_diag_row <- function(check, base) {
  tab <- .sr_diag_table()
  hit <- tab$id == check & vapply(strsplit(tab$routes, ",", fixed = TRUE), function(r) base %in% r, logical(1))
  tab[which(hit)[1L], , drop = FALSE]
}

# The linear terms a linearity scan may test: the exposure or a term entered linearly and not part of
# an interaction. data.frame(id, var).
.sr_diag_lin_cands <- function(spec) {
  plan <- .sr_term_plan(spec)
  ia <- .sr_interaction_plan(spec, plan)
  in_ia <- unique(unlist(lapply(ia, function(p) c(p$a, p$b))))
  keep <- vapply(seq_along(plan), function(i) identical(plan[[i]]$type, "linear") && !(i %in% in_ia), logical(1))
  data.frame(id = vapply(plan[keep], function(i) i$id, character(1)),
             var = vapply(plan[keep], function(i) i$var, character(1)), stringsAsFactors = FALSE)
}

# The applicable checks of a route id and a (normalised) spec, in catalogue order.
.sr_diag_applies <- function(route, spec) {
  base <- .sr_diag_base(route)
  if (!(base %in% .sr_diag_routes)) return(character())
  model <- base %in% c("lm", "logistic", "modpois", "cox")
  out <- c(
    if (base == "cox") "ph",
    if (model) "influence",
    if (base == "logistic" && !.sr_glm_firth(spec)) "separation",
    if (model && length(.sr_spec_chr(spec[["cluster"]]))) "few_clusters",
    if (base %in% c("cox", "rmst")) "inf_censoring",
    if (model && nrow(.sr_diag_lin_cands(spec))) "linearity",
    if (base == "lm" && identical(.sr_lm_se(spec), "model")) "heteroscedasticity"
  )
  out
}

#' The ids of the diagnostics that apply to an analysis (list(route, route_version, spec)), in
#' catalogue order; character() for a route without diagnostics (table1).
sr_diag_applicable <- function(analysis) {
  if (!is.list(analysis) || !.sr_is_text1(analysis$route)) {
    stop("analysis must be a list with a route id", call. = FALSE)
  }
  .sr_diag_applies(analysis$route, .sr_spec_from_state(analysis$spec))
}

# ---- helpers inlined into the emitted code ------------------------------------------------------

#' ok / warn / fail of an effect-size value against the thresholds; cmp is "ge", "gt" or "lt". A value
#' that could not be computed (NA) is a warning.
.sr_h_diag_verdict <- function(value, warn, fail, cmp) {
  if (is.na(value)) return("warn")
  meets <- function(th) {
    if (is.na(th)) return(FALSE)
    switch(cmp, ge = value >= th, gt = value > th, lt = value < th)
  }
  if (meets(fail)) "fail" else if (meets(warn)) "warn" else "ok"
}

#' At most n rows of d: all of them, else the rows in `keep` plus a random sample of the others
#' (seeded by the run), in their original order. list(d, note).
.sr_h_diag_cap <- function(d, keep = integer(), n = 5000L) {
  rownames(d) <- NULL
  if (nrow(d) <= n) return(list(d = d, note = character()))
  keep <- unique(keep[keep >= 1L & keep <= nrow(d)])
  keep <- keep[seq_len(min(length(keep), n))]
  rest <- setdiff(seq_len(nrow(d)), keep)
  pick <- sort(c(keep, rest[sample.int(length(rest), n - length(keep))]))
  out <- d[pick, , drop = FALSE]
  rownames(out) <- NULL
  list(d = out, note = sprintf("plot data: a seeded sample of %d of %d points (flagged rows kept)", n, nrow(d)))
}

#' User labels of derived coefficient names, from the static coef map (coef, var, kind).
.sr_h_diag_label <- function(nm, coef, var, kind) {
  vapply(nm, function(s) {
    i <- match(s, coef)
    if (!is.na(i) && kind[i] != "factor") {
      if (kind[i] == "quad" && endsWith(s, "_q")) return(paste0(var[i], "^2"))
      if (kind[i] == "rcs" && grepl("_s[0-9]+$", s)) return(paste0(var[i], " (spline ", sub("^.*_s", "", s), ")"))
      return(var[i])
    }
    f <- which(kind == "factor" & startsWith(s, coef) & nchar(s) > nchar(coef))
    if (length(f) == 1L) return(paste0(var[f], ": ", substring(s, nchar(coef[f]) + 1L)))
    s
  }, character(1), USE.NAMES = FALSE)
}

#' The linear and the RCS(4) partial prediction of one linear term `col` (label `var`) over a grid of
#' 100 points from its 5th to its 95th centile, both centred, and their absolute difference divided
#' by `scale`. NULL when the term has fewer than 10 distinct values, its knots cannot be placed or
#' the spline refit fails. `dfit(d, rhs)` refits the model on d with the right-hand side rhs.
.sr_h_diag_lin <- function(d, fit, col, var, rhs, dfit, scale) {
  x <- d[[col]]
  if (length(unique(x)) < 10L) return(NULL)
  k <- tryCatch(Hmisc::rcspline.eval(x, nk = 4, knots.only = TRUE), error = function(e) NULL)
  if (length(k) != 4L) return(NULL)
  b <- Hmisc::rcspline.eval(x, knots = k, inclx = FALSE)
  d[[".lr1"]] <- as.numeric(b[, 1L])
  d[[".lr2"]] <- as.numeric(b[, 2L])
  f2 <- tryCatch(dfit(d, paste(rhs, "+ .lr1 + .lr2")), error = function(e) NULL)
  if (is.null(f2)) return(NULL)
  g <- seq(unname(stats::quantile(x, 0.05)), unname(stats::quantile(x, 0.95)), length.out = 100L)
  bg <- Hmisc::rcspline.eval(g, knots = k, inclx = FALSE)
  c1 <- stats::coef(fit)
  c2 <- stats::coef(f2)
  lin <- unname(c1[[col]]) * g
  rcs <- unname(c2[[col]]) * g + unname(c2[[".lr1"]]) * bg[, 1L] + unname(c2[[".lr2"]]) * bg[, 2L]
  lin <- lin - mean(lin)
  rcs <- rcs - mean(rcs)
  data.frame(var = var, x = g, linear = lin, rcs = as.numeric(rcs), dev = abs(lin - as.numeric(rcs)) / scale,
             stringsAsFactors = FALSE)
}

#' Standardised mean differences of each column of the named list `cols` between the rows where
#' `grp` is TRUE and the others (pooled SD; 0 when both groups are constant).
.sr_h_diag_smd <- function(cols, grp) {
  vapply(cols, function(v) {
    s <- sqrt((stats::var(v[grp]) + stats::var(v[!grp])) / 2)
    m <- mean(v[grp]) - mean(v[!grp])
    if (is.na(s) || s == 0) 0 else m / s
  }, numeric(1))
}

# ---- the code builders --------------------------------------------------------------------------

# The notes every detail carries for the layers the diagnostic does not follow.
.sr_diag_notes <- function(spec) {
  c(if (.sr_mi_on(spec)) "multiple imputation: the check runs once on the complete cases of the analysis columns",
    if (.sr_boot_on(spec)) "the bootstrap layer is not part of the diagnostic: the model is fitted once on the analysis data")
}

# The common start of a model check (lm, logistic, modpois, cox): the design, the complete cases of the
# analysis columns (.rows = their positions in sr_data), the refit function .h_dfit(d, rhs) and, when
# `fit` is TRUE, the fit on the analysis data. Also .exn (the exposure coefficient names), .exl (their
# labels) and .notes. Returns list(code, tc, rhs).
.sr_diag_model_prelude <- function(spec, base, fit = TRUE) {
  tc <- sr_terms_code(spec)
  plan <- .sr_term_plan(spec)
  rhs_vars <- all.vars(stats::as.formula(paste("~", tc$rhs)))
  notes <- .sr_diag_notes(spec)
  copy <- character()
  if (base == "cox") {
    ties <- .sr_cox_ties_of(spec)
    cl <- if (.sr_cox_clustered(spec)) spec[["cluster"]] else NULL
    st <- .sr_spec_chr(spec[["strata"]])
    st_names <- if (length(st)) paste0(".s", seq_along(st)) else character()
    used <- unique(c(".time", ".event", rhs_vars, st_names, if (!is.null(cl)) ".cl"))
    copy <- c(
      vapply(seq_along(st), function(i) sprintf("des[[%s]] <- des[[%s]]", .sr_lit(st_names[i]), .sr_lit(st[i])),
             character(1)),
      if (!is.null(cl)) sprintf("des[[\".cl\"]] <- des[[%s]]", .sr_lit(cl)))
    rhs <- paste(c(tc$rhs, if (length(st)) sprintf("strata(%s)", paste(st_names, collapse = ", "))),
                 collapse = " + ")
    outcome <- sr_outcome_code(spec, "surv")
    guard <- c("if (sum(des[[\".event\"]]) == 0L) {",
               "  stop(\"the outcome has no events among the \", nrow(des), \" rows analysed\")",
               "}")
    dfit <- paste0(".h_dfit <- function(d, rhs, lhs = \"survival::Surv(.time, .event)\") ",
                   "suppressWarnings(survival::coxph(stats::as.formula(paste(lhs, \"~\", rhs)), data = d, ties = ",
                   .sr_lit(ties), "))")
  } else {
    se <- if (base == "lm") .sr_lm_se(spec) else .sr_glm_se(spec, base)
    cl <- if (identical(se, "cluster")) spec[["cluster"]] else NULL
    used <- c(".y", rhs_vars, cl)
    rhs <- tc$rhs
    outcome <- sr_outcome_code(spec, if (base == "lm") "continuous" else "binary")
    guard <- if (base != "lm") c(
      "if (sum(des[[\".y\"]]) == 0L || all(des[[\".y\"]] == 1L)) {",
      "  stop(\"the outcome has no events or no non-events among the \", nrow(des), \" rows analysed\")",
      "}")
    dfit <- if (base == "lm") {
      ".h_dfit <- function(d, rhs) stats::lm(stats::as.formula(paste(\".y ~\", rhs)), data = d)"
    } else {
      family <- if (base == "modpois") "stats::poisson(link = \"log\")" else "stats::binomial()"
      paste0(".h_dfit <- function(d, rhs, control = stats::glm.control()) suppressWarnings(stats::glm(",
             "stats::as.formula(paste(\".y ~\", rhs)), data = d, family = ", family, ", control = control))")
    }
    if (base == "logistic" && .sr_glm_firth(spec)) {
      notes <- c(notes, "Firth model: diagnosed through its unpenalised maximum-likelihood fit")
    }
  }
  fac <- identical(plan[[1L]]$type, "factor")
  ex_var <- .sr_lit(plan[[1L]]$var)
  code <- c(
    tc$setup,
    tc$design,
    "des <- .h_design(sr_data)",
    outcome,
    copy,
    sprintf(".keep <- stats::complete.cases(des[%s])", .sr_lit(used)),
    "if (!any(.keep)) stop(\"no complete rows: every row has a missing value in the model\")",
    ".rows <- which(.keep)",
    "des <- des[.keep, , drop = FALSE]",
    guard,
    dfit,
    sprintf(".rhs <- %s", .sr_lit(rhs)),
    if (fit) "fit <- .h_dfit(des, .rhs)",
    if (fac) c(
      ".lvx <- .sr_fixed$levels[[\"t1\"]][-1L]",
      ".exn <- paste0(\"t1f\", .lvx)",
      sprintf(".exl <- paste0(%s, \": \", .lvx)", ex_var)
    ) else c(
      ".exn <- \"t1\"",
      sprintf(".exl <- %s", ex_var)
    ),
    sprintf(".notes <- %s", .sr_lit(notes))
  )
  list(code = code, tc = tc, rhs = rhs)
}

# The closing lines: the verdict, the detail and the result list. `main` is code giving the main
# sentence of the detail; `extra` named code strings for the extra fields.
.sr_diag_close <- function(row, main, extra = character(), reported = "NULL") {
  c(sprintf(".verdict <- .h_diag_verdict(.value, %s, %s, %s)", .sr_lit(row$warn), .sr_lit(row$fail),
            .sr_lit(row$cmp)),
    sprintf(".detail <- paste(c(%s, .notes), collapse = \"; \")", main),
    paste0("list(value = .value, verdict = .verdict, detail = .detail, plot_data = .pd, reported = ", reported,
           if (length(extra)) paste0(", ", paste(names(extra), "=", extra, collapse = ", ")), ")"))
}

# The value of a max over a vector that may hold NA: NA when nothing is finite.
.sr_diag_max <- function(x) sprintf("if (any(is.finite(%s))) max(%s[is.finite(%s)]) else NA_real_", x, x, x)

.sr_diag_code_ph <- function(spec, row) {
  pre <- .sr_diag_model_prelude(spec, "cox")
  cm <- pre$tc$coef_map
  c(pre$code,
    ".evt <- des[[\".time\"]][des[[\".event\"]] == 1L]",
    ".tm <- stats::median(.evt)",
    ".tau_s <- unname(stats::quantile(.evt, 0.75))",
    ".de <- des",
    ".de[[\".event\"]][.de[[\".time\"]] > .tm] <- 0L",
    ".de[[\".time\"]] <- pmin(.de[[\".time\"]], .tm)",
    ".dl <- des[des[[\".time\"]] > .tm, , drop = FALSE]",
    ".dl[[\".t0\"]] <- rep(.tm, nrow(.dl))",
    ".be <- tryCatch(stats::coef(.h_dfit(.de, .rhs))[.exn], error = function(e) rep(NA_real_, length(.exn)))",
    ".bl <- tryCatch(stats::coef(.h_dfit(.dl, .rhs, \"survival::Surv(.t0, .time, .event)\"))[.exn], error = function(e) rep(NA_real_, length(.exn)))",
    ".dv <- abs(unname(.be) - unname(.bl))",
    sprintf(".value <- %s", .sr_diag_max(".dv")),
    # cox.zph: reported only, never read by the verdict
    ".zrep <- tryCatch({",
    "  .zph <- survival::cox.zph(fit)",
    "  .zt <- .zph$table",
    sprintf("  data.frame(var = c(%s, \"GLOBAL\"), term = rownames(.zt), chisq = unname(.zt[, \"chisq\"]), df = unname(.zt[, \"df\"]), p = unname(.zt[, \"p\"]), stringsAsFactors = FALSE)",
            .sr_lit(cm$source_var)),
    "}, error = function(e) data.frame(var = character(), term = character(), chisq = numeric(), df = numeric(), p = numeric(), stringsAsFactors = FALSE))",
    ".sch <- as.matrix(stats::residuals(fit, type = \"scaledsch\"))",
    ".cfn <- names(stats::coef(fit))",
    "colnames(.sch) <- if (ncol(.sch) == length(.cfn)) .cfn else .cfn[!is.na(stats::coef(fit))]",
    ".stime <- if (is.null(rownames(.sch))) rep(NA_real_, nrow(.sch)) else suppressWarnings(as.numeric(rownames(.sch)))",
    ".pd <- do.call(rbind, lapply(seq_along(.exn), function(j) data.frame(term = .exl[j], time = .stime, resid = unname(.sch[, .exn[j]]), stringsAsFactors = FALSE)))",
    ".cap <- .h_diag_cap(.pd)",
    ".pd <- .cap$d",
    ".notes <- c(.notes, .cap$note, \"cox.zph is reported only and does not enter the verdict\")",
    sprintf(".main <- sprintf(\"max |log HR(early) - log HR(late)| of %%s = %%.3f, split at the median event time %%.4g; 75th centile of the event times %%.4g\", %s, .value, .tm, .tau_s)",
            .sr_lit(.sr_term_plan(spec)[[1L]]$var)),
    .sr_diag_close(row, ".main", c(tau_suggest = ".tau_s"), reported = ".zrep"))
}

.sr_diag_code_influence <- function(spec, base, row) {
  pre <- .sr_diag_model_prelude(spec, base)
  body <- if (base == "cox") c(
    ".dfb <- as.matrix(stats::residuals(fit, type = \"dfbetas\"))",
    ".cfn <- names(stats::coef(fit))",
    "colnames(.dfb) <- if (ncol(.dfb) == length(.cfn)) .cfn else .cfn[!is.na(stats::coef(fit))]",
    ".im <- apply(abs(.dfb[, .exn, drop = FALSE]), 1L, max)",
    sprintf(".value <- %s", .sr_diag_max(".im")),
    ".flag <- which(.im >= 1)",
    ".pd <- data.frame(row = .rows, dfbeta = unname(.im))",
    ".main <- sprintf(\"max |scaled dfbeta| of the exposure = %.3f; %d row(s) with |scaled dfbeta| >= 1\", .value, length(.flag))"
  ) else c(
    ".im <- unname(stats::cooks.distance(fit))",
    sprintf(".value <- %s", .sr_diag_max(".im")),
    ".flag <- which(.im > 1)",
    ".pd <- data.frame(row = .rows, leverage = unname(stats::hatvalues(fit)), cooks = .im)",
    ".main <- sprintf(\"max Cook's distance = %.3f; %d row(s) with D > 1\", .value, length(.flag))"
  )
  c(pre$code, body,
    ".flagged <- as.integer(.rows[.flag])",
    ".cap <- .h_diag_cap(.pd, .flag)",
    ".pd <- .cap$d",
    ".notes <- c(.notes, .cap$note)",
    .sr_diag_close(row, ".main", c(rows = ".flagged")))
}

.sr_diag_code_separation <- function(spec, row) {
  pre <- .sr_diag_model_prelude(spec, "logistic")
  cm <- pre$tc$coef_map
  c(pre$code,
    ".f2 <- .h_dfit(des, .rhs, stats::glm.control(epsilon = 1e-12, maxit = 100))",
    ".b1 <- stats::coef(fit)",
    ".b2 <- stats::coef(.f2)[names(.b1)]",
    ".gr <- abs(unname(.b2)) - abs(unname(.b1))",
    ".grow <- any(.gr > 1, na.rm = TRUE)",
    ".edge <- any(.f2$fitted.values < 1e-08 | .f2$fitted.values > 1 - 1e-08)",
    ".value <- as.numeric(.grow && .edge)",
    sprintf(".pd <- data.frame(term = .h_diag_label(names(.b1), %s, %s, %s), beta = unname(.b1), beta_refit = unname(.b2), growth = .gr, stringsAsFactors = FALSE)",
            .sr_lit(cm$coef), .sr_lit(cm$source_var), .sr_lit(cm$kind)),
    ".main <- sprintf(\"refit with maxit = 100, epsilon = 1e-12: largest |beta| growth %.3f; fitted probability within 1e-8 of 0 or 1: %s\", max(c(0, .gr), na.rm = TRUE), if (.edge) \"yes\" else \"no\")",
    .sr_diag_close(row, ".main"))
}

.sr_diag_code_few_clusters <- function(spec, base, row) {
  pre <- .sr_diag_model_prelude(spec, base, fit = FALSE)
  col <- if (base == "cox" && .sr_cox_clustered(spec)) ".cl" else spec[["cluster"]]
  c(pre$code,
    sprintf(".gcl <- des[[%s]]", .sr_lit(col)),
    ".gsz <- sort(as.numeric(table(.gcl[!is.na(.gcl)])))",
    ".value <- as.numeric(length(.gsz))",
    ".pd <- data.frame(cluster = seq_along(.gsz), size = .gsz)",
    ".main <- sprintf(\"%d cluster(s) among the %d rows analysed (sizes %s to %s)\", length(.gsz), nrow(des), if (length(.gsz)) min(.gsz) else NA, if (length(.gsz)) max(.gsz) else NA)",
    .sr_diag_close(row, ".main"))
}

# The baseline columns of the cox model for the SMD: each term's (first) numeric column, one 0/1
# indicator per non-reference level of a factor term.
.sr_diag_baseline_code <- function(spec) {
  out <- ".bcol <- list()"
  for (it in .sr_term_plan(spec)) {
    if (it$type == "factor") {
      out <- c(out, sprintf("for (.l in .sr_fixed$levels[[%s]][-1L]) .bcol[[paste0(%s, \": \", .l)]] <- as.numeric(des[[%s]] == .l)",
                            .sr_lit(it$id), .sr_lit(it$var), .sr_lit(paste0(it$id, "f"))))
    } else {
      out <- c(out, sprintf(".bcol[[%s]] <- as.numeric(des[[%s]])", .sr_lit(it$var), .sr_lit(it$id)))
    }
  }
  out
}

.sr_diag_code_inf_censoring <- function(spec, base, row) {
  if (base == "rmst") {
    cols <- c(spec[["time"]], spec[["status"]], spec[["exposure"]])
    ref <- spec[["exposure_ref"]]
    has_ref <- length(ref) == 1L && !is.na(ref) && nzchar(as.character(ref))
    pre <- c(
      sprintf(".miss <- setdiff(%s, names(sr_data))", .sr_lit(cols)),
      "if (length(.miss)) stop(\"column(s) not in the data: \", paste(.miss, collapse = \", \"))",
      .sr_inline(.h_levels, ".h_levels"),
      sprintf("des <- as.data.frame(sr_data)[%s]", .sr_lit(cols)),
      sr_outcome_code(spec, "surv"),
      sprintf("des[[\".ex\"]] <- des[[%s]]", .sr_lit(spec[["exposure"]])),
      ".keep <- stats::complete.cases(des[c(\".time\", \".event\", \".ex\")])",
      "if (!any(.keep)) stop(\"no complete rows: every row has a missing value in the model\")",
      ".rows <- which(.keep)",
      "des <- des[.keep, , drop = FALSE]",
      sprintf(".lv <- .h_levels(des[[\".ex\"]], %s)", if (has_ref) .sr_lit(as.character(ref)) else "NULL"),
      ".bcol <- list()",
      sprintf("for (.l in .lv[-1L]) .bcol[[paste0(%s, \": \", .l)]] <- as.numeric(as.character(des[[\".ex\"]]) == .l)",
              .sr_lit(spec[["exposure"]])),
      sprintf(".notes <- %s", .sr_lit(c(.sr_diag_notes(spec), "the exposure is the only baseline term of an RMST analysis"))))
  } else {
    pre <- c(.sr_diag_model_prelude(spec, "cox", fit = FALSE)$code, .sr_diag_baseline_code(spec))
  }
  c(pre,
    ".fu <- stats::median(des[[\".time\"]])",
    ".ec <- des[[\".event\"]] == 0L & des[[\".time\"]] < .fu",
    ".n1 <- sum(.ec)",
    ".n0 <- sum(!.ec)",
    ".small <- .n1 < 2L || .n0 < 2L",
    ".smd <- if (.small) rep(NA_real_, length(.bcol)) else unname(.h_diag_smd(.bcol, .ec))",
    ".value <- if (.small) 0 else if (any(is.finite(.smd))) max(abs(.smd[is.finite(.smd)])) else 0",
    ".pd <- data.frame(term = names(.bcol), smd = .smd, stringsAsFactors = FALSE)",
    paste0(".main <- if (.small) sprintf(\"too few rows to compare: %d censored before the median follow-up %.4g, %d not\", .n1, .fu, .n0) ",
           "else sprintf(\"max |SMD| of the baseline terms = %.3f, %d row(s) censored before the median follow-up %.4g vs %d not\", .value, .n1, .fu, .n0)"),
    .sr_diag_close(row, ".main"))
}

.sr_diag_code_linearity <- function(spec, base, row) {
  pre <- .sr_diag_model_prelude(spec, base)
  cand <- .sr_diag_lin_cands(spec)
  scale <- if (base == "lm") "stats::sd(des[[\".y\"]])" else "1"
  scale_txt <- if (base == "lm") "outcome SD" else "linear-predictor scale"
  c(pre$code,
    sprintf(".lid <- %s", .sr_lit(cand$id)),
    sprintf(".lvr <- %s", .sr_lit(cand$var)),
    sprintf(".lsc <- %s", scale),
    ".lin <- lapply(seq_along(.lid), function(j) .h_diag_lin(des, fit, .lid[[j]], .lvr[[j]], .rhs, .h_dfit, .lsc))",
    ".lin <- .lin[!vapply(.lin, is.null, logical(1))]",
    ".ldv <- vapply(.lin, function(z) if (any(is.finite(z$dev))) max(z$dev[is.finite(z$dev)]) else NA_real_, numeric(1))",
    ".none <- length(.lin) == 0L",
    sprintf(".value <- if (.none) 0 else %s", .sr_diag_max(".ldv")),
    ".lw <- if (.none || all(is.na(.ldv))) NA_character_ else .lin[[which.max(.ldv)]]$var[1L]",
    ".pd <- if (.none) data.frame(var = character(), x = numeric(), linear = numeric(), rcs = numeric()) else do.call(rbind, .lin)[c(\"var\", \"x\", \"linear\", \"rcs\")]",
    "rownames(.pd) <- NULL",
    sprintf(paste0(".main <- if (.none) \"no continuous term entered linearly (>= 10 distinct values) to scan\" ",
                   "else sprintf(\"max |linear - RCS(4)| over the 5th-95th centile = %%.3f (%s), worst term %%s; %%d term(s) scanned\", .value, .lw, length(.lin))"),
            scale_txt),
    .sr_diag_close(row, ".main", c(var = ".lw")))
}

.sr_diag_code_heteroscedasticity <- function(spec, row) {
  pre <- .sr_diag_model_prelude(spec, "lm")
  c(pre$code,
    ".vr <- diag(sandwich::vcovHC(fit, type = \"HC3\"))",
    ".vm <- diag(stats::vcov(fit))",
    ".hr <- sqrt(unname(.vr[.exn])) / sqrt(unname(.vm[.exn]))",
    sprintf(".value <- %s", .sr_diag_max(".hr")),
    ".pd <- data.frame(fitted = unname(stats::fitted(fit)), resid = unname(stats::residuals(fit)))",
    ".cap <- .h_diag_cap(.pd)",
    ".pd <- .cap$d",
    ".notes <- c(.notes, .cap$note)",
    ".main <- sprintf(\"robust (HC3) SE / model SE of the exposure = %.3f\", .value)",
    .sr_diag_close(row, ".main"))
}

#' The emitted code (kind "diagnostic") of one check for an analysis (list(route, route_version,
#' spec)). The code reads `sr_data` and returns list(value, verdict, detail, plot_data, reported, ...).
#' Errors for an unknown check, an invalid spec or a check that does not apply to the analysis.
sr_diag_compile <- function(check, analysis) {
  if (!.sr_is_text1(check)) stop("check must be one diagnostic id", call. = FALSE)
  if (!(check %in% SR_DIAG_IDS)) stop("unknown diagnostic ", .sr_sap_q(check), call. = FALSE)
  if (!is.list(analysis) || !.sr_is_text1(analysis$route)) {
    stop("analysis must be a list with a route id", call. = FALSE)
  }
  route <- sr_route_get(analysis$route, analysis$route_version)
  spec <- .sr_spec_from_state(analysis$spec)
  if (!(check %in% .sr_diag_applies(analysis$route, spec))) {
    stop("diagnostic ", .sr_sap_q(check), " does not apply to this analysis (route ",
         .sr_sap_q(analysis$route), ")", call. = FALSE)
  }
  v <- sr_spec_validate(spec, route)
  if (!v$ok) stop("the spec is not valid for route ", .sr_sap_q(analysis$route), ": ",
                  paste(v$errors, collapse = "; "), call. = FALSE)
  base <- .sr_diag_base(analysis$route)
  row <- .sr_diag_row(check, base)
  body <- switch(check,
    ph = .sr_diag_code_ph(spec, row),
    influence = .sr_diag_code_influence(spec, base, row),
    separation = .sr_diag_code_separation(spec, row),
    few_clusters = .sr_diag_code_few_clusters(spec, base, row),
    inf_censoring = .sr_diag_code_inf_censoring(spec, base, row),
    linearity = .sr_diag_code_linearity(spec, base, row),
    heteroscedasticity = .sr_diag_code_heteroscedasticity(spec, row))
  code <- paste(c(
    .sr_inline(.sr_h_diag_verdict, ".h_diag_verdict"),
    .sr_inline(.sr_h_diag_cap, ".h_diag_cap"),
    if (check == "separation") .sr_inline(.sr_h_diag_label, ".h_diag_label"),
    if (check == "linearity") .sr_inline(.sr_h_diag_lin, ".h_diag_lin"),
    if (check == "inf_censoring") .sr_inline(.sr_h_diag_smd, ".h_diag_smd"),
    body), collapse = "\n")
  .sr_emit_check(code, "diagnostic")
  code
}

# ---- the reducer --------------------------------------------------------------------------------

# One number from a re-read payload field (null / absent gives NA).
.sr_diag_num1 <- function(x) {
  if (is.null(x) || !length(x) || is.null(x[[1L]]) || is.na(x[[1L]])) NA_real_ else as.numeric(x[[1L]])
}

sr_reducer_register("diag.completed", function(state, event) {
  p <- event$payload
  id <- .sr_sap_chr1(p$analysis)
  ck <- .sr_sap_chr1(p$check)
  if (is.na(id) || is.na(ck)) stop("diag.completed needs an analysis id and a check id", call. = FALSE)
  dg <- state$diag %||% list()
  one <- dg[[id]] %||% list()
  one[[ck]] <- list(check = ck, verdict = .sr_sap_chr1(p$verdict), value = .sr_diag_num1(p$value),
                    warn = .sr_diag_num1(p$warn), fail = .sr_diag_num1(p$fail),
                    detail = .sr_sap_chr1(p$detail), key = .sr_sap_chr1(p$key),
                    tau_suggest = .sr_diag_num1(p$tau_suggest),
                    rows = if (is.null(p$rows)) integer() else as.integer(unlist(p$rows, use.names = FALSE)),
                    var = .sr_sap_chr1(p$var))
  dg[[id]] <- one
  state$diag <- dg
  state
})

# ---- the run ------------------------------------------------------------------------------------

#' Run the diagnostics of an analysis that has run successfully and record one diag.completed per
#' check: list(analysis, check, verdict, value, warn, fail, detail, key) plus tau_suggest, rows and
#' var where the check has them. Each check runs in the isolated worker on the analysis extract (the
#' prologue of sr_run), behind the fits cache keyed over list(check, route, spec). A check that fails
#' to compute is recorded with verdict "warn", value NA and the (path-scrubbed) reason. Refused
#' (nothing is written) for a read-only project, an unknown analysis, an unlocked SAP, a table that
#' changed after the lock, an analysis without an ok run, or a check that does not apply.
#' @param checks NULL (every applicable check) or a character vector of check ids
#' @return the project
sr_diag_run <- function(project, analysis_id, checks = NULL, timeout = 3600) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  .sr_sap_id(analysis_id, "analysis_id")
  a <- project$state$analyses[[analysis_id]]
  if (is.null(a)) stop("unknown analysis ", .sr_sap_q(analysis_id), call. = FALSE)
  if (sr_blind(project)) stop("the SAP is not locked", call. = FALSE)
  locked_hash <- project$state$sap$lock$tables[[a$table]]$store_hash
  if (is.null(locked_hash)) stop("table '", a$table, "' was not part of the SAP lock", call. = FALSE)
  if (!identical(sr_store_hash(sr_store_manifest(project, a$table)), locked_hash)) {
    stop("table '", a$table, "' changed after the SAP lock", call. = FALSE)
  }
  r <- project$state$runs[[analysis_id]]
  if (is.null(r) || is.null(r$ok_key) || is.na(r$ok_key)) {
    stop("analysis ", .sr_sap_q(analysis_id), " has not run successfully", call. = FALSE)
  }
  spec <- .sr_spec_from_state(a$spec)
  ap <- .sr_diag_applies(a$route, spec)
  if (is.null(checks)) {
    checks <- ap
  } else {
    if (!is.character(checks) || anyNA(checks)) stop("checks must be NULL or a character vector of check ids", call. = FALSE)
    unknown <- setdiff(checks, SR_DIAG_IDS)
    if (length(unknown)) stop("unknown diagnostic ", .sr_sap_q(unknown[1L]), call. = FALSE)
    off <- setdiff(checks, ap)
    if (length(off)) {
      stop("diagnostic ", .sr_sap_q(off[1L]), " does not apply to analysis ", .sr_sap_q(analysis_id), call. = FALSE)
    }
    checks <- unique(checks)
  }
  if (!length(checks)) return(project)
  bodies <- lapply(checks, function(ck) sr_diag_compile(ck, a))
  names(bodies) <- checks
  ex <- sr_extract(project, a$table, sr_spec_cols(spec))
  prologue <- .sr_run_prologue(ex)
  route_key <- .sr_route_key(a$route, a$route_version)
  base <- .sr_diag_base(a$route)
  for (ck in checks) {
    row <- .sr_diag_row(ck, base)
    key <- sr_content_hash(ex$sha, list(check = ck, route = route_key, spec = spec), spec$seed)
    res <- sr_exec_cached(project, key, paste(prologue, bodies[[ck]], sep = "\n"),
                          inputs = list(sr_project_dir = project$paths$dir), seed = spec$seed,
                          cache_inputs = list(extract = ex$sha), timeout = timeout)
    v <- if (isTRUE(res$ok)) res$value else NULL
    good <- is.list(v) && .sr_value_plain(v) && .sr_is_text1(v$verdict) &&
      v$verdict %in% c("ok", "warn", "fail") && is.numeric(v$value) && length(v$value) == 1L
    payload <- if (good) {
      c(list(analysis = analysis_id, check = ck, verdict = v$verdict, value = v$value, warn = row$warn,
             fail = row$fail, detail = .sr_run_scrub(v$detail, project$paths$dir), key = key),
        if (!is.null(v$tau_suggest)) list(tau_suggest = v$tau_suggest),
        if (!is.null(v$rows)) list(rows = as.integer(v$rows)),
        if (!is.null(v$var)) list(var = v$var))
    } else {
      why <- if (isTRUE(res$ok)) "the diagnostic did not return a verdict" else res$error
      if (isTRUE(res$ok)) unlink(file.path(project$paths$fits, paste0(key, ".rds")))
      list(analysis = analysis_id, check = ck, verdict = "warn", value = NA_real_, warn = row$warn,
           fail = row$fail, detail = paste0("could not be computed: ", .sr_run_scrub(why, project$paths$dir)),
           key = key)
    }
    project <- sr_project_append(project, "diag.completed", payload)
  }
  project
}
