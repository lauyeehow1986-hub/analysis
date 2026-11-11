# scale/stream_fit.R - the streaming fit of the linear model: TSQR over chunks, the aliasing rule and
# the lm finish (P1e.1, spec section 6.4, rulings R3, R10 and R12).
#
# Every .h_st_* function here is emitted into the analysis code with .sr_inline(), so it is
# self-contained: base and stats only (plus the other .h_ helpers), ASCII text, no StatRouter call.
#
# The fold: M <- .h_st_tsqr(M, cbind(X, y)) per chunk, starting from M = NULL. M is then the R factor
# of the whole [X | y] (columns in the design order), so crossprod(M) = crossprod(cbind(X, y)). The
# rank decision is taken once, on the final M, by .h_st_alias with the tolerance lm uses (1e-7): the
# LINPACK limited-pivoting QR compares each column's residual norm with its original norm, and both
# are invariant under the orthogonal transformation from [X | y] to M, so the kept and aliased
# columns are those of stats::lm.fit on the full data.

#' One TSQR merge (ruling R12): the R factor of rbind(M, B) by LINPACK qr(tol = 0), with its columns
#' put back in the original order (qr.R(qa)[, order(qa$pivot)]), so a column that is all zero in a chunk
#' (a factor level absent from it) cannot permute the result. With M = NULL it starts from B. A chunk
#' with fewer rows than columns gives a short (rows x columns) factor, which later chunks extend. An
#' empty B leaves M unchanged.
.h_st_tsqr <- function(M, B) {
  B <- as.matrix(B)
  storage.mode(B) <- "double"
  A <- if (is.null(M)) B else rbind(M, B)
  if (!nrow(A)) return(M)
  qa <- qr(A, tol = 0)
  R <- qr.R(qa)
  R[, order(qa$pivot), drop = FALSE]
}

#' The aliasing rule (rulings R10 and R12) on the final TSQR factor M: qa <- qr(M[, 1:p], tol = tol).
#' Returns list(R = the rank x rank upper triangle of the kept columns in pivot order, keep = the kept
#' column indices in the original order, rank, pivot, qr = qa). LINPACK's limited pivoting moves only
#' the deficient columns to the end and keeps the order of the others, so pivot order and original
#' order agree on the kept columns.
.h_st_alias <- function(M, p, tol) {
  p <- as.integer(p)
  qa <- qr(M[, seq_len(p), drop = FALSE], tol = tol)
  r <- qa$rank
  piv <- qa$pivot
  list(R = qr.R(qa)[seq_len(r), seq_len(r), drop = FALSE], keep = sort(piv[seq_len(r)]), rank = r,
       pivot = piv, qr = qa)
}

#' Chan's merge of the count, mean and centred sum of squares (M2) of y into st (NULL to start), for
#' the streamed total sum of squares of ruling R3 (st$m2 at the end).
.h_st_moments_add <- function(st, y) {
  if (is.null(st)) st <- list(n = 0, mean = 0, m2 = 0)
  y <- as.numeric(y)
  nb <- length(y)
  if (!nb) return(st)
  mb <- mean(y)
  m2b <- sum((y - mb)^2)
  n <- st$n + nb
  d <- mb - st$mean
  list(n = n, mean = st$mean + d * nb / n, m2 = st$m2 + m2b + d^2 * st$n * nb / n)
}

#' Finish the streamed lm from the TSQR factor M of [X | y] (p design columns named nms, n rows) and
#' the total sum of squares sst (centred, for a model with an intercept; sum(y^2) without one, as
#' summary.lm). Returns list(beta (named, NA for an aliased column), R, keep, rank, rss, df_resid,
#' sigma, r2, adj_r2, aliased, B, n): R the triangle of the kept columns, B = chol2inv(R) in keep order
#' named by the kept columns (the bread of every SE), sigma = sqrt(rss / (n - rank)) (ruling R3).
.h_st_lm_finish <- function(M, p, nms, n, sst) {
  if (is.null(M)) stop("no complete rows: every row has a missing value in the model")
  p <- as.integer(p)
  al <- .h_st_alias(M, p, 1e-7)
  r <- al$rank
  df_resid <- n - r
  if (df_resid < 1) {
    stop("no residual degrees of freedom: ", n, " complete rows for ", p, " coefficients")
  }
  z <- qr.qty(al$qr, M[, p + 1L])
  pk <- al$pivot[seq_len(r)]
  beta <- stats::setNames(rep(NA_real_, p), nms)
  beta[pk] <- backsolve(al$R, z[seq_len(r)])
  rss <- if (length(z) > r) sum(z[-seq_len(r)]^2) else 0
  o <- order(pk)
  B <- chol2inv(al$R)[o, o, drop = FALSE]
  dimnames(B) <- list(nms[al$keep], nms[al$keep])
  df_int <- if ("(Intercept)" %in% nms) 1 else 0
  r2 <- 1 - rss / sst
  list(beta = beta, R = al$R, keep = al$keep, rank = r, rss = rss, df_resid = df_resid,
       sigma = sqrt(rss / df_resid), r2 = r2, adj_r2 = 1 - (1 - r2) * ((n - df_int) / df_resid),
       aliased = nms[is.na(beta)], B = B, n = n)
}
