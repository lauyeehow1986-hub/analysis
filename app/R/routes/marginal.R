# routes/marginal.R - marginal (standardised) effects and the effect-size helpers shared by the model
# routes (lm marginal mean difference here; logistic and modified Poisson in routes/glm.R).
#
# Three plain functions, inlined into the emitted code as .h_gcomp, .h_nnt and .h_wilson so that
# analysis.R runs without StatRouter. Each is a pure function of its arguments (ruling R4):
#   .sr_h_gcomp   g-computation. Set the exposure to each of two levels on the `over` rows, average
#                 the predicted means, and give the difference and the log ratio with delta-method
#                 standard errors g'Vg (gradient = column means of mu.eta(eta) * X at that level).
#   .sr_h_nnt     the number needed to treat from a risk difference and its CI (Altman 1998):
#                 NNTB / NNTH, and the "NNTB x to infinity to NNTH y" reading when the CI crosses 0.
#   .sr_h_wilson  the Wilson score interval of a proportion.
#
# The rest of this file writes the code and the result rows that lm and glm share: the code block
# that runs g-computation on the fitted model (.sr_marg_pre / .sr_marg_post) and the effects rows and
# table built from its value (.sr_marg_present). Marginal effects need a binary (0/1) or a
# categorical (factor) exposure: with the exposure at each non-reference level against the reference.

#' G-computation on a fitted lm / glm.
#' @param fit an lm or glm
#' @param data the table the model was fitted on (its exposure column is checked)
#' @param exposure_col name of the exposure column in `data` / `over`
#' @param levels c(exposed, reference): the two values the exposure is set to
#' @param V covariance matrix of the estimated coefficients (aliased ones dropped, or NA-filled)
#' @param over the rows to standardise over (default `data`)
#' @return list(p1, p0, rd, rd_se, log_rr, log_rr_se): the standardised means under the exposed and
#'   reference level, their difference and its SE, and the log ratio and its SE (NA when a mean is
#'   not positive)
.sr_h_gcomp <- function(fit, data, exposure_col, levels, V, over = data) {
  if (!is.character(exposure_col) || length(exposure_col) != 1L || is.na(exposure_col) ||
      !(exposure_col %in% names(data)) || !(exposure_col %in% names(over))) {
    stop("exposure must be the name of a column of the data")
  }
  if (length(levels) != 2L || anyNA(levels) || identical(levels[[1L]], levels[[2L]])) {
    stop("levels must be two different values: the exposed level, then the reference level")
  }
  if (!is.data.frame(over) || nrow(over) < 1L) stop("over must be a data frame with at least one row")
  if (!is.matrix(V) || nrow(V) != ncol(V)) stop("V must be a square covariance matrix")
  b <- stats::coef(fit)
  ok <- !is.na(b)
  bn <- names(b)[ok]
  if (nrow(V) == length(b) && !all(ok)) {
    V <- if (is.null(rownames(V))) V[ok, ok, drop = FALSE] else V[bn, bn, drop = FALSE]
  }
  if (nrow(V) != length(bn)) {
    stop("V must have one row and column for each estimated coefficient (", length(bn), ")")
  }
  if (!is.null(rownames(V)) && !is.null(colnames(V))) {
    if (!setequal(rownames(V), bn) || !setequal(colnames(V), bn)) {
      stop("V must be named like the estimated coefficients")
    }
    V <- V[bn, bn, drop = FALSE]
  }
  tt <- stats::delete.response(stats::terms(fit))
  fam <- if (inherits(fit, "glm")) fit$family else stats::gaussian()
  linkinv <- fam$linkinv
  mu_eta <- fam$mu.eta
  at <- function(value) {
    d <- over
    x <- over[[exposure_col]]
    d[[exposure_col]] <- if (is.factor(x)) {
      factor(rep(as.character(value), nrow(d)), levels = base::levels(x))
    } else {
      rep(value, nrow(d))
    }
    mf <- stats::model.frame(tt, d, na.action = stats::na.pass, xlev = fit$xlevels)
    if (anyNA(mf)) stop("the rows to standardise over have missing values in the model variables")
    X <- stats::model.matrix(tt, mf, contrasts.arg = fit$contrasts)[, bn, drop = FALSE]
    eta <- drop(X %*% b[ok])
    list(mu = mean(linkinv(eta)), grad = colMeans(X * mu_eta(eta)))
  }
  e1 <- at(levels[[1L]])
  e0 <- at(levels[[2L]])
  se <- function(g) {
    v <- drop(crossprod(g, V %*% g))
    if (is.finite(v)) sqrt(max(v, 0)) else NA_real_
  }
  pos <- e1$mu > 0 && e0$mu > 0
  list(p1 = e1$mu, p0 = e0$mu, rd = e1$mu - e0$mu, rd_se = se(e1$grad - e0$grad),
       log_rr = if (pos) log(e1$mu / e0$mu) else NA_real_,
       log_rr_se = if (pos) se(e1$grad / e1$mu - e0$grad / e0$mu) else NA_real_)
}

#' Number needed to treat from a risk difference `rd` and its interval (lo, hi) (Altman 1998).
#' A CI that excludes 0 gives NNTB (benefit, rd > 0) or NNTH (harm, rd < 0) with the bounds
#' 1/hi .. 1/lo; a CI that includes 0 has no finite interval, so its bounds are NA and the label
#' reads "NNTB a to infinity to NNTH b". rd = 0 has no NNT (NA).
#' @return list(nnt, lo, hi, label); nnt is signed (negative for harm)
.sr_h_nnt <- function(rd, lo, hi, level = 0.95) {
  fin <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
  if (!fin(rd) || !fin(lo) || !fin(hi)) stop("rd, lo and hi must each be one finite number")
  if (!fin(level) || level <= 0 || level >= 1) stop("level must be one number strictly between 0 and 1")
  if (lo > hi || rd < lo || rd > hi) stop("lo <= rd <= hi is required")
  num <- function(x) sub("[.]0$", "", sprintf("%.1f", x))
  ci <- sprintf("%d%% CI", as.integer(round(100 * level)))
  if (lo > 0 || hi < 0) {
    bnd <- sort(c(1 / lo, 1 / hi))
    kind <- if (rd > 0) "NNTB" else "NNTH"
    return(list(nnt = 1 / rd, lo = bnd[1L], hi = bnd[2L],
                label = sprintf("%s %s (%s %s to %s)", kind, num(abs(1 / rd)), ci,
                                num(min(abs(bnd))), num(max(abs(bnd))))))
  }
  ben <- if (hi > 0) sprintf("NNTB %s", num(1 / hi))
  harm <- if (lo < 0) sprintf("NNTH %s", num(1 / abs(lo)))
  seg <- if (rd >= 0) c(ben, harm) else c(harm, ben)
  rng <- if (length(seg)) {
    paste(seg[1L], "to infinity", if (length(seg) > 1L) paste("to", seg[2L]))
  } else {
    "infinity"
  }
  if (rd == 0) {
    return(list(nnt = NA_real_, lo = NA_real_, hi = NA_real_,
                label = sprintf("NNT undefined: the risk difference is 0 (%s: %s)", ci, rng)))
  }
  list(nnt = 1 / rd, lo = NA_real_, hi = NA_real_,
       label = sprintf("%s %s (%s %s)", if (rd > 0) "NNTB" else "NNTH", num(abs(1 / rd)), ci, rng))
}

#' Wilson score interval for `x` events in `n` trials, clamped to [0, 1].
#' @return c(est = x / n, lo, hi); all NA for n = 0
.sr_h_wilson <- function(x, n, level = 0.95) {
  whole <- function(v) is.numeric(v) && length(v) == 1L && is.finite(v) && v == round(v)
  if (!whole(x) || !whole(n) || x < 0 || n < 0 || x > n) {
    stop("x and n must be whole numbers with 0 <= x <= n")
  }
  if (!is.numeric(level) || length(level) != 1L || is.na(level) || level <= 0 || level >= 1) {
    stop("level must be one number strictly between 0 and 1")
  }
  if (n == 0) return(c(est = NA_real_, lo = NA_real_, hi = NA_real_))
  z <- stats::qnorm(1 - (1 - level) / 2)
  ph <- x / n
  den <- 1 + z^2 / n
  ctr <- (ph + z^2 / (2 * n)) / den
  half <- z * sqrt(ph * (1 - ph) / n + z^2 / (4 * n^2)) / den
  c(est = ph, lo = max(0, ctr - half), hi = min(1, ctr + half))
}

# ---- the code block shared by lm and glm --------------------------------------------------------

# The standardisation population: "analysis" (the default) is the rows the model was fitted on;
# "all" adds the rows lost only to a missing outcome (covariates, exposure complete).
.sr_marg_over <- function(spec) {
  v <- spec[["standardize_over"]]
  if (is.null(v)) "analysis" else v
}

#' The emitted code of a marginal block. `on` says whether to run it, `strict` whether a non-binary,
#' non-categorical exposure is an error (marginal asked for explicitly) or a recorded note.
#' It needs `fit`, `des` (the fitted rows), `.V` (covariance of the estimated coefficients),
#' `.h_gcomp` and, for standardize_over = "all", `.des_all` (the design before the complete-case
#' filter, which `pre` captures). Returns list(pre, post, value): lines to run before the
#' complete-case filter, lines to run after the fit, and the R expression for the `marginal` field.
.sr_marg_code <- function(spec, rhs_vars, on, strict) {
  if (!isTRUE(on)) return(list(pre = NULL, post = NULL, value = "NULL"))
  plan <- .sr_term_plan(spec)
  fac <- identical(plan[[1L]]$type, "factor")
  all_rows <- identical(.sr_marg_over(spec), "all")
  over <- if (all_rows) {
    sprintf(".over <- .des_all[stats::complete.cases(.des_all[%s]), , drop = FALSE]", .sr_lit(rhs_vars))
  } else {
    ".over <- des"
  }
  body <- if (fac) {
    c(".lv <- .sr_fixed$levels[[\"t1\"]]",
      "for (.l in .lv[-1L]) {",
      "  .g <- .h_gcomp(fit, des, \"t1f\", c(.l, .lv[1L]), .V, over = .over)",
      "  .mg <- rbind(.mg, data.frame(level = .l, as.data.frame(.g), stringsAsFactors = FALSE))",
      "}")
  } else {
    c(".ev <- sort(unique(des[[\"t1\"]]))",
      "if (length(.ev) == 2L && all(.ev == c(0, 1))) {",
      "  .g <- .h_gcomp(fit, des, \"t1\", c(1, 0), .V, over = .over)",
      "  .mg <- data.frame(level = NA_character_, as.data.frame(.g), stringsAsFactors = FALSE)",
      "} else {",
      if (isTRUE(strict)) {
        paste0("  stop(\"marginal effects need a binary (0/1) or categorical exposure; the exposure has \", ",
               "length(.ev), \" distinct values\")")
      } else {
        paste0("  .mg_note <- paste0(\"marginal effects were not computed: the exposure is not binary (0/1) ",
               "or categorical (\", length(.ev), \" distinct values)\")")
      },
      "}")
  }
  list(pre = if (all_rows) ".des_all <- des",
       post = c(".mg <- NULL", ".mg_note <- NULL", over, body),
       value = sprintf("list(rows = .mg, n_over = nrow(.over), over = %s, note = .mg_note)",
                       .sr_lit(.sr_marg_over(spec))))
}

# ---- the rows and the table of a marginal block ---------------------------------------------------

#' Effects rows and the marginal table from `value$marginal$rows` (one row per exposed level).
#' kind = "mean" (a continuous outcome): one MD row per contrast; kind = "risk" (a binary outcome): an
#' RD row, an RR row and a descriptive NNT row per contrast. Intervals are the delta-method ones on
#' the difference and on the log ratio, t on `value$df` or z when that is infinite. ci_base names the
#' interval family of the model (e.g. "t_robust_hc3"); it is prefixed with "delta_".
#' @return list(effects, table)
.sr_marg_present <- function(value, exposure, kind = c("mean", "risk"), ci_base) {
  kind <- match.arg(kind)
  mg <- value$marginal$rows
  r0 <- value$levels[["t1"]]
  ref <- if (length(r0)) as.character(r0[1L]) else NA_character_
  lab <- ifelse(is.na(mg$level), exposure, sprintf("%s: %s vs %s", exposure, mg$level, ref))
  lev <- if (is.null(value$level)) 0.95 else value$level
  df <- value$df
  a <- (1 - lev) / 2
  crit <- if (is.finite(df)) stats::qt(1 - a, df) else stats::qnorm(1 - a)
  pz <- function(z) {
    p <- if (is.finite(df)) 2 * stats::pt(-abs(z), df) else 2 * stats::pnorm(-abs(z))
    ifelse(is.nan(p), NA_real_, p)
  }
  ev <- if (is.null(value$events)) NA_real_ else value$events
  method <- paste0("delta_", ci_base)
  row <- function(term, measure, est, lo, hi, p, scale, level, role, method) {
    data.frame(term = term, measure = measure, est = est, lo = lo, hi = hi, p = p, scale = scale,
               level = level, mcse = NA_real_, role = role, conditional = FALSE, ci_method = method,
               n = value$n, events = ev, stringsAsFactors = FALSE)
  }
  role_of <- function(...) if (all(is.finite(c(...)))) "primary" else "descriptive"
  out <- list()
  tab <- list()
  for (i in seq_len(nrow(mg))) {
    g <- mg[i, , drop = FALSE]
    lo <- g$rd - crit * g$rd_se
    hi <- g$rd + crit * g$rd_se
    if (kind == "mean") {
      out[[length(out) + 1L]] <- row(paste0(lab[i], " (marginal)"), "MD", g$rd, lo, hi, pz(g$rd / g$rd_se),
                                     "difference", g$level, role_of(g$rd, lo, hi), method)
      tab[[length(tab) + 1L]] <- data.frame(
        Contrast = lab[i], `Mean, exposed` = sr_fmt_num(g$p1), `Mean, reference` = sr_fmt_num(g$p0),
        `Difference (95% CI)` = sr_fmt_ci(g$rd, lo, hi), check.names = FALSE, stringsAsFactors = FALSE)
      next
    }
    rlo <- exp(g$log_rr - crit * g$log_rr_se)
    rhi <- exp(g$log_rr + crit * g$log_rr_se)
    rr <- exp(g$log_rr)
    out[[length(out) + 1L]] <- row(paste0(lab[i], " (marginal RD)"), "RD", g$rd, lo, hi, pz(g$rd / g$rd_se),
                                   "difference", g$level, role_of(g$rd, lo, hi), method)
    out[[length(out) + 1L]] <- row(paste0(lab[i], " (marginal RR)"), "RR", rr, rlo, rhi,
                                   pz(g$log_rr / g$log_rr_se), "ratio", g$level, role_of(rr, rlo, rhi), method)
    nt <- if (all(is.finite(c(g$rd, lo, hi)))) {
      .sr_h_nnt(g$rd, lo, hi, level = lev)
    } else {
      list(nnt = NA_real_, lo = NA_real_, hi = NA_real_, label = "NNT not available")
    }
    out[[length(out) + 1L]] <- row(paste0(lab[i], " (marginal NNT)"), "NNT", nt$nnt, nt$lo, nt$hi, NA_real_,
                                   NA_character_, g$level, "descriptive", "altman_from_rd")
    tab[[length(tab) + 1L]] <- data.frame(
      Contrast = lab[i], `Risk, exposed` = sr_fmt_num(g$p1), `Risk, reference` = sr_fmt_num(g$p0),
      `RD (95% CI)` = sr_fmt_ci(g$rd, lo, hi), `RR (95% CI)` = sr_fmt_ci(rr, rlo, rhi),
      `NNT (95% CI)` = nt$label, check.names = FALSE, stringsAsFactors = FALSE)
  }
  eff <- if (length(out)) do.call(rbind, out) else sr_effects_empty()
  rownames(eff) <- NULL
  tb <- if (length(tab)) do.call(rbind, tab) else NULL
  if (!is.null(tb)) rownames(tb) <- NULL
  list(effects = eff, table = tb)
}
