# P1e.2 shared survival test helpers (Task 3, reused by Task 4). Synthetic data only.
#
# cox_eval(beta, df, xcols, ties = "efron", chunk = 7L, strata = NULL)
#   Runs the streamed Cox step (.h_st_cox_new / .h_st_cox_step / .h_st_cox_close) over the data.frame df
#   in chunks of `chunk` rows and returns list(loglik, U, I, sumabs) at beta (centred, unscaled
#   coordinates). df has the columns time and status (0/1), the covariates named by xcols and, when
#   strata is a column name, that stratum column. The rows are sorted as the stream reads them (stratum,
#   then time DESC, then row), X is centred by its column means, and eta = X %*% beta.
#
# cox_ref(beta, df, xcols, ties = "efron", strata = NULL)
#   The reference: survival::coxph(..., init = beta, control = coxph.control(iter.max = 0)), giving
#   list(loglik = fit$loglik[1], V = vcov(fit)) at beta.
#
# .cs_data(n, seed, ntime): the Task 3 fixture (integer times with one heavy tie block, x1 normal, x2 0/1,
#   yr around 2000), shared by test-scale-surv-coxstep.R and test-scale-surv-coxfit.R.

.cs_data <- function(n = 120L, seed = 3L, ntime = 12L) {
  set.seed(seed)
  pt <- rep(1, ntime)
  pt[4L] <- 6                                  # one heavy tie block, many chunks long at chunk 7
  data.frame(time = sample(seq_len(ntime), n, replace = TRUE, prob = pt) * 1.0,
             status = rbinom(n, 1L, 0.7),
             x1 = rnorm(n),
             x2 = rbinom(n, 1L, 0.4),
             yr = 2000 + sample(-6:6, n, replace = TRUE) + rnorm(n, sd = 0.1))
}

.cox_sorted <- function(df, strata = NULL) {
  s <- if (is.null(strata)) rep(1L, nrow(df)) else as.integer(factor(df[[strata]]))
  o <- order(s, -df$time, seq_len(nrow(df)))
  list(df = df[o, , drop = FALSE], s = s[o])
}

cox_eval <- function(beta, df, xcols, ties = "efron", chunk = 7L, strata = NULL) {
  so <- .cox_sorted(df, strata)
  d <- so$df
  X <- as.matrix(d[, xcols, drop = FALSE])
  storage.mode(X) <- "double"
  X <- sweep(X, 2L, colMeans(X))
  eta <- drop(X %*% beta)
  st <- .h_st_cox_new(length(xcols), ties)
  for (i in split(seq_len(nrow(d)), ceiling(seq_len(nrow(d)) / chunk))) {
    st <- .h_st_cox_step(st, d$time[i], d$status[i], X[i, , drop = FALSE], eta[i], so$s[i])
  }
  out <- .h_st_cox_close(st)
  out$sumabs <- st$sumabs
  out
}

cox_ref <- function(beta, df, xcols, ties = "efron", strata = NULL) {
  rhs <- paste(xcols, collapse = " + ")
  if (!is.null(strata)) rhs <- paste0(rhs, " + strata(", strata, ")")
  f <- stats::as.formula(paste("Surv(time, status) ~", rhs),
                         env = new.env(parent = asNamespace("survival")))
  fit <- suppressWarnings(survival::coxph(f, data = df, ties = ties, init = beta,
                                          control = survival::coxph.control(iter.max = 0)))
  list(loglik = fit$loglik[1], V = unname(stats::vcov(fit)))
}
