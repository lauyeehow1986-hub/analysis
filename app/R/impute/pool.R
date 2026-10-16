# impute/pool.R - Rubin's rules for multiply imputed estimates and the two-stage choice of the number
# of imputations. Both helpers are plain functions of their arguments (no package beyond stats), so
# they are inlined into the emitted analysis code as .h_rubin and .h_auto_m (see impute/impute.R).
#
# .sr_h_rubin pools m estimates of the same k coefficients, each with its covariance matrix:
#   Qbar = mean of the estimates, Ubar = mean of the variances, B = between-imputation variance,
#   T = Ubar + (1 + 1/m) B;
#   riv = (1 + 1/m) B / Ubar (relative increase in variance), lambda = (1 + 1/m) B / T;
#   df_old = (m - 1) / lambda^2; with a finite complete-data df (dfcom) the Barnard-Rubin
#   df_obs = (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda) and df = df_old df_obs / (df_old + df_obs);
#   with dfcom = Inf, df = df_old;
#   fmi = (riv + 2 / (df + 3)) / (riv + 1) (the fraction of missing information, as mice::pool);
#   mcse = sqrt(B / m), the Monte Carlo standard error of Qbar from using m imputations.
# The interval and the two-sided p are t on df when dfcom is finite and normal when dfcom is Inf (ruling
# R25e: the Barnard-Rubin t for a linear model, normal otherwise).
#
# No between-imputation variance (B = 0: nothing was imputed, or every imputation gave the same
# estimate) is a legitimate result, not an error (ruling R25d): riv = lambda = mcse = 0 exactly, and
# df = df_obs (finite dfcom) or Inf, so no NaN reaches the table. mice floors lambda at 1e-4 before the
# df; this function does not (it follows the formula), which differs from mice only when
# 0 < lambda < 1e-4.

#' Pool per-imputation estimates with Rubin's rules (Barnard-Rubin degrees of freedom).
#' @param est_list list of m named numeric vectors (the same names, in the same order)
#' @param vcov_list list of m k x k covariance matrices
#' @param dfcom the complete-data degrees of freedom: one positive number, Inf for a large-sample model
#' @param level the confidence level
#' @return data.frame(term, est, se, lo, hi, p, df, riv, lambda, fmi, mcse)
.sr_h_rubin <- function(est_list, vcov_list, dfcom, level = 0.95) {
  m <- length(est_list)
  if (!is.list(est_list) || m < 2L) stop("pooling needs at least two imputations")
  if (!is.list(vcov_list) || length(vcov_list) != m) {
    stop("est_list and vcov_list must have the same length (one entry per imputation)")
  }
  if (!(is.numeric(dfcom) && length(dfcom) == 1L && !is.na(dfcom) && dfcom > 0)) {
    stop("dfcom must be one positive number (Inf for a large-sample model)")
  }
  nm <- names(est_list[[1L]])
  k <- length(est_list[[1L]])
  if (!is.numeric(est_list[[1L]]) || k < 1L || is.null(nm) || anyNA(nm) || !all(nzchar(nm)) ||
      anyDuplicated(nm)) {
    stop("the estimates must be named numeric vectors (one distinct name per coefficient)")
  }
  est <- matrix(NA_real_, m, k)
  u <- matrix(NA_real_, m, k)
  for (i in seq_len(m)) {
    e <- est_list[[i]]
    v <- vcov_list[[i]]
    same <- is.numeric(e) && length(e) == k && identical(names(e), nm) && is.matrix(v) &&
      is.numeric(v) && identical(dim(v), c(k, k)) &&
      (is.null(rownames(v)) || identical(rownames(v), nm)) &&
      (is.null(colnames(v)) || identical(colnames(v), nm))
    if (!same) {
      stop("design differs across imputations: imputation ", i, " does not have the coefficients of imputation 1 (",
           paste(nm, collapse = ", "), ")")
    }
    est[i, ] <- unname(e)
    u[i, ] <- unname(diag(v))
  }
  qbar <- colMeans(est)
  ubar <- colMeans(u)
  b <- apply(est, 2L, stats::var)
  for (j in seq_len(k)) {
    if (isTRUE(all(est[, j] == est[1L, j]))) {
      b[j] <- 0
      qbar[j] <- est[1L, j]
    }
    if (isTRUE(all(u[, j] == u[1L, j]))) ubar[j] <- u[1L, j]
  }
  zero <- !is.na(b) & b == 0
  tv <- ubar + (1 + 1 / m) * b
  riv <- (1 + 1 / m) * b / ubar
  lambda <- (1 + 1 / m) * b / tv
  riv[zero] <- 0
  lambda[zero] <- 0
  df_old <- (m - 1) / lambda^2
  if (is.finite(dfcom)) {
    df_obs <- (dfcom + 1) / (dfcom + 3) * dfcom * (1 - lambda)
    df <- ifelse(is.finite(df_old), df_old * df_obs / (df_old + df_obs), df_obs)
  } else {
    df <- df_old
  }
  fmi <- ifelse(is.na(riv), NA_real_, ifelse(is.finite(riv), (riv + 2 / (df + 3)) / (riv + 1), 1))
  se <- sqrt(tv)
  alpha <- (1 - level) / 2
  if (is.finite(dfcom)) {
    dfu <- ifelse(!is.na(df) & df > 0, df, NA_real_)
    crit <- stats::qt(1 - alpha, dfu)
    p <- 2 * stats::pt(-abs(qbar / se), dfu)
  } else {
    crit <- rep(stats::qnorm(1 - alpha), k)
    p <- 2 * stats::pnorm(-abs(qbar / se))
  }
  out <- data.frame(term = nm, est = qbar, se = se, lo = qbar - crit * se, hi = qbar + crit * se, p = p,
                    df = df, riv = riv, lambda = lambda, fmi = fmi, mcse = sqrt(b / m),
                    stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' The number of imputations from a pilot run (von Hippel 2020, two-stage rule, target SE CV 0.05).
#'
#' `fmi_up = plogis(qlogis(fmi_max) + 1.96 * sqrt(2 / m_pilot))` is the upper limit of the pilot's
#' fraction of missing information, and the number needed is `ceiling(1 + 0.5 * (fmi_up / 0.05)^2)`,
#' never fewer than the pilot and never more than `cap`.
#' @param fmi_max the largest fraction of missing information among the primary terms of the pilot
#' @param m_pilot the number of imputations of the pilot
#' @param cap the largest number of imputations allowed
#' @return list(m_pilot, fmi_max, m_final, m_capped)
.sr_h_auto_m <- function(fmi_max, m_pilot = 20L, cap = 200L) {
  if (!(is.numeric(fmi_max) && length(fmi_max) == 1L && !is.na(fmi_max) && fmi_max >= 0 && fmi_max <= 1)) {
    stop("fmi_max must be one number between 0 and 1 (the largest fraction of missing information of the pilot)")
  }
  up <- stats::plogis(stats::qlogis(fmi_max) + 1.96 * sqrt(2 / m_pilot))
  need <- ceiling(1 + 0.5 * (up / 0.05)^2)
  list(m_pilot = as.integer(m_pilot), fmi_max = fmi_max, m_final = as.integer(min(cap, max(m_pilot, need))),
       m_capped = need > cap)
}
