# routes/se.R - the standard-error helpers shared by the regression routes (lm here; logistic, modified
# Poisson and Cox reuse them). Both are plain functions of fitted objects and matrices, inlined into
# the emitted code as .h_vcov and .h_coef_table so that analysis.R runs without StatRouter.
#
#   se = "model"    stats::vcov(fit)
#   se = "robust"   sandwich::vcovHC: HC3 for an lm (the small-sample choice), HC0 for a glm (HC3 is
#                   not defined for the working weights of a glm)
#   se = "cluster"  sandwich::vcovCL(type = "HC1"), one cluster id per fitted row
#
# The lm and glm routes use the cluster column when and only when se = "cluster", and that is the
# one SE that layers$clustered = TRUE or spec$cluster allow (.sr_cluster_se_check, the rule Cox
# applies with cluster()), so the clustering the SAP declares and the clustering the fit uses agree.

# Cross-field rule of the lm and glm routes: se = "cluster" needs one cluster column and
# layers$clustered = TRUE; any other SE refuses both. `se` is the effective SE. Stops with the reason.
.sr_cluster_se_check <- function(spec, se) {
  cl <- spec[["cluster"]]
  clustered <- isTRUE(spec[["layers"]][["clustered"]])
  if (identical(se, "cluster")) {
    if (!(is.character(cl) && length(cl) == 1L && !is.na(cl) && nzchar(cl))) {
      stop("se = \"cluster\" needs a cluster column (spec$cluster)", call. = FALSE)
    }
    if (!clustered) stop("se = \"cluster\" needs layers$clustered = TRUE", call. = FALSE)
  } else if (clustered) {
    stop("layers$clustered = TRUE needs se = \"cluster\" (the cluster-robust SE); a ",
         if (identical(se, "model")) "model-based" else se, " SE would ignore the clustering", call. = FALSE)
  } else if (length(.sr_spec_chr(cl))) {
    stop("a cluster column needs se = \"cluster\" and layers$clustered = TRUE", call. = FALSE)
  }
  invisible(TRUE)
}

#' Covariance matrix of the coefficients of `fit`. `cluster` is the cluster id of every fitted row
#' (needed, and only used, for se = "cluster").
.sr_h_vcov <- function(fit, se = c("model", "robust", "cluster"), cluster = NULL) {
  se <- match.arg(se)
  if (se == "model") return(stats::vcov(fit))
  if (se == "robust") {
    return(sandwich::vcovHC(fit, type = if (inherits(fit, "glm")) "HC0" else "HC3"))
  }
  if (is.null(cluster)) stop("se = \"cluster\" needs a cluster vector")
  if (length(cluster) != stats::nobs(fit)) {
    stop("the cluster vector must have one value for each fitted row (cluster has ", length(cluster),
         ", the fit used ", stats::nobs(fit), ")")
  }
  if (anyNA(cluster)) stop("the cluster vector must not contain missing values")
  sandwich::vcovCL(fit, cluster = cluster, type = "HC1")
}

#' data.frame(term, est, se, lo, hi, p) from coefficients `est` (named) and their covariance `V`: a
#' t interval and two-sided p on `df` degrees of freedom, or a z interval for df = Inf.
.sr_h_coef_table <- function(est, V, df = Inf, level = 0.95) {
  if (!is.numeric(level) || length(level) != 1L || is.na(level) || level <= 0 || level >= 1) {
    stop("level must be one number strictly between 0 and 1")
  }
  if (!is.numeric(df) || length(df) != 1L || is.na(df) || df <= 0) {
    stop("df must be one positive number (Inf for a normal interval)")
  }
  if (!is.matrix(V) || nrow(V) != length(est) || ncol(V) != length(est)) {
    stop("V must be a square matrix with one row for each coefficient (length of est: ", length(est), ")")
  }
  v <- diag(V)
  v[!is.na(v) & v < 0] <- NA_real_
  se <- sqrt(v)
  a <- (1 - level) / 2
  crit <- if (is.finite(df)) stats::qt(1 - a, df) else stats::qnorm(1 - a)
  z <- as.numeric(est) / se
  p <- if (is.finite(df)) 2 * stats::pt(-abs(z), df) else 2 * stats::pnorm(-abs(z))
  data.frame(term = if (is.null(names(est))) as.character(seq_along(est)) else names(est),
             est = as.numeric(est), se = se, lo = as.numeric(est) - crit * se,
             hi = as.numeric(est) + crit * se, p = p, stringsAsFactors = FALSE)
}
