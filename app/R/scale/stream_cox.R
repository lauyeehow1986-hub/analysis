# scale/stream_cox.R - the streamed exact Cox partial likelihood of the streaming survival engine (P1e.2,
# plan Task 3): the cholesky2 / chsolve2 / chinv2 port of survival 3.8.3 and the per-chunk Cox step that
# accumulates the log-likelihood, score and information of survival:::coxph.fit (coxfit6) at a fixed beta.
#
# Every .h_st_* function here is emitted into the analysis code with .sr_inline(), so it is
# self-contained: base and stats only (it may call the other .h_st_* helpers of this file), ASCII text.
#
#   chol     .h_st_chol2(M, toler) / .h_st_chsolve2(ch, b) / .h_st_chinv2(ch): the generalised LDL' (S6)
#   weights  .h_st_cox_wts(S0, E0, d, ties): the per-block Efron / Breslow weights (S8, S9, S12)
#   step     .h_st_cox_new(p, ties), .h_st_cox_step(st, time, event, X, eta, strat), .h_st_cox_close(st)
#   guard    .h_st_time_guard(st, time, strat): the S10 time-order check, carried across chunks
#   fit      .h_st_coxfit(passfn, p, nms, scale, maxit, eps): coxfit6's Newton loop (S5, Task 4)
#   warn     .h_st_cox_warn(fit, eps): coxph.fit's warnings, verbatim (S7)
#
# Coordinates (S1): X is centred, not scaled; eta = X %*% beta. The rows arrive sorted by stratum, then
# time DESC, so the risk set of a tie block (every row of the stratum with time >= t) is the running total
# of the rows read so far, the block included. A tie block is the run of rows with the same stratum and
# the same (mapped) time.
#
# Information. With D_l = S0 - f E0 and a_l = S1 - f E1, f = l/d (Efron) or 0 (Breslow), l = 0..d-1:
#   I = sum_blocks sum_l (S2 - f E2) / D_l - a_l a_l' / D_l^2.
# The S2 part is never stored per block. Its block weight is w1 = sum_l 1/D_l = dLam, and a row read in
# the stratum is in the risk set of its own block and of every block read after it, so
#   sum_k w1_k S2(t_k) = sum_i r_i x_i x_i' (sum of w1 from row i's block to the stratum end).
# Per run of complete blocks: I += W S2c + X' (r wrow) X, then S2c += X' r X, where S2c is the carried
# sum of r x x' over the stratum's rows already completed, W the run's sum of w1 and wrow the reverse
# cumulative w1 from the row's block to the run's end. Every term is positive, so nothing cancels (a
# Lam A - B form loses I entirely once max|eta| is large).
#
# Chunk carries. The last block of a chunk may continue into the next one, so it is held back as its
# block sums (st$pend), not as rows: R0 = sum r, R1 = sum r x, E0, E1, d, the event sums of eta and x,
# E2 = sum over its events of r x x' and S2 = sum over its rows of r x x'. The stratum carries are S0,
# S1, S2c and Lam (the stratum's cumulative dLam).

#' The generalised Cholesky (LDL') of the symmetric matrix M, as survival's cholesky2: list(L, D, sing).
#' L is unit lower triangular, D the pivots. A pivot that is not finite or below toler * max(diag(M))
#' (toler itself when no diagonal is positive) is singular: D is 0 and its column is skipped, keeping the
#' values left below its diagonal, which chsolve2's forward pass reads as the C code does.
.h_st_chol2 <- function(M, toler) {
  M <- as.matrix(M)
  storage.mode(M) <- "double"
  n <- nrow(M)
  lw <- lower.tri(M)
  M[lw] <- t(M)[lw]
  dg <- diag(M)
  eps <- max(c(0, dg[!is.na(dg)]))
  eps <- if (eps == 0) toler else eps * toler
  sing <- logical(n)
  for (i in seq_len(n)) {
    piv <- M[i, i]
    if (!is.finite(piv) || piv < eps) {
      M[i, i] <- 0
      sing[i] <- TRUE
    } else if (i < n) {
      j <- (i + 1L):n
      orig <- M[j, i]
      tmp <- orig / piv
      M[j, i] <- tmp
      dj <- diag(M)[j] - tmp * tmp * piv
      M[j, j] <- M[j, j] - outer(orig, tmp)
      if (length(j) == 1L) M[j, j] <- dj else diag(M[j, j]) <- dj
    }
  }
  D <- diag(M)
  L <- M
  L[upper.tri(L)] <- 0
  diag(L) <- 1
  list(L = L, D = D, sing = sing)
}

#' The solution of M x = b from ch = .h_st_chol2(M, toler), as survival's chsolve2: 0 at singular
#' coordinates. b is a vector or a matrix of right-hand sides.
.h_st_chsolve2 <- function(ch, b) {
  vec <- is.null(dim(b))
  b <- as.matrix(b)
  storage.mode(b) <- "double"
  z <- forwardsolve(ch$L, b)
  L2 <- ch$L
  L2[, ch$sing] <- 0
  diag(L2) <- 1
  w <- z / ch$D
  w[ch$sing, ] <- 0
  x <- backsolve(t(L2), w)
  x[ch$sing, ] <- 0
  if (vec) as.numeric(x) else x
}

#' The inverse from ch = .h_st_chol2(M, toler), as survival's chinv2 followed by coxfit6's symmetric copy:
#' zero rows and columns at singular coordinates, the inverse of the non-singular block elsewhere.
.h_st_chinv2 <- function(ch) {
  n <- length(ch$D)
  V <- matrix(0, n, n)
  ns <- which(!ch$sing)
  if (length(ns)) {
    Li <- forwardsolve(ch$L[ns, ns, drop = FALSE], diag(length(ns)))
    Vn <- crossprod(Li, Li / ch$D[ns])
    lw <- lower.tri(Vn)
    Vn[lw] <- t(Vn)[lw]
    V[ns, ns] <- Vn
  }
  V
}

#' The per-block weights of the tie blocks with risk totals S0 (block included), event risk sums E0 and
#' event counts d, for ties "efron" or "breslow": a matrix with one row per block and the columns
#'   w1 = sum_l 1/D_l (the hazard increment dLam, also the carried S2 weight),
#'   w2 = sum_l f/D_l, q1 = sum_l 1/D_l^2, q2 = sum_l f/D_l^2, q3 = sum_l f^2/D_l^2, lg = sum_l log D_l,
#' with D_l = S0 - f E0, l = 0..d-1, f = l/d (Efron) or 0 (Breslow); all 0 when d = 0.
#' The S9 score-residual terms of a block with S1 (block included) and E1 follow from them:
#' dLam = w1, xbar_mean = (S1 w1 - E1 w2) / d, dB = S1 q1 - E1 q2, c1 = w2, c2 = S1 q2 - E1 q3.
.h_st_cox_wts <- function(S0, E0, d, ties) {
  nb <- length(S0)
  w <- matrix(0, nb, 6L, dimnames = list(NULL, c("w1", "w2", "q1", "q2", "q3", "lg")))
  e <- d > 0
  if (identical(ties, "breslow")) {
    w[e, "w1"] <- d[e] / S0[e]
    w[e, "q1"] <- d[e] / S0[e]^2
    w[e, "lg"] <- d[e] * log(S0[e])
    return(w)
  }
  w[e, "w1"] <- 1 / S0[e]
  w[e, "q1"] <- 1 / S0[e]^2
  w[e, "lg"] <- log(S0[e])
  # l = 1..d-1: blocks with a few events share one pass per l; a block with many is summed on its own
  small <- 16
  dm <- max(c(0, d[d <= small]))
  if (dm > 1) {
    for (l in seq_len(dm - 1)) {
      j <- which(d > l & d <= small)
      f <- l / d[j]
      D <- S0[j] - f * E0[j]
      w[j, "w1"] <- w[j, "w1"] + 1 / D
      w[j, "w2"] <- w[j, "w2"] + f / D
      w[j, "q1"] <- w[j, "q1"] + 1 / D^2
      w[j, "q2"] <- w[j, "q2"] + f / D^2
      w[j, "q3"] <- w[j, "q3"] + f^2 / D^2
      w[j, "lg"] <- w[j, "lg"] + log(D)
    }
  }
  for (j in which(d > small)) {
    f <- seq_len(d[j] - 1) / d[j]
    D <- S0[j] - f * E0[j]
    w[j, ] <- w[j, ] + c(sum(1 / D), sum(f / D), sum(1 / D^2), sum(f / D^2), sum(f^2 / D^2), sum(log(D)))
  }
  w
}

#' The per-pass state of the streamed Cox step for p columns and ties "efron" or "breslow": the sums
#' loglik, U and I; the stratum carries S0, S1, S2c (sum of r x x' over the stratum's completed rows) and
#' Lam; the current stratum cur; the pending block pend (NULL, or its block sums); and sumabs, the column
#' sums of |X| (S1).
.h_st_cox_new <- function(p, ties) {
  p <- as.integer(p)
  if (!ties %in% c("efron", "breslow")) stop("ties must be \"efron\" or \"breslow\"")
  list(p = p, ties = ties, loglik = 0, U = numeric(p), I = matrix(0, p, p), S0 = 0, S1 = numeric(p),
       S2c = matrix(0, p, p), Lam = 0, cur = NULL, pend = NULL, sumabs = numeric(p))
}

#' Internal to the step: process the tie blocks with sums ag (list(s, t, R0, R1, E0, E1, d, LE, XE), one
#' element or row per block, in reading order). Blocks 1..ncomp are complete; a block after them is the
#' pending one, whose rows are left to the caller. X, r, ev and blk are the chunk's rows of these blocks
#' (blk the block of each row, none for a replayed pending block). ex, when not NULL, is list(E2, S2) of
#' block 1's rows held from earlier chunks.
.h_st_cox_blocks <- function(st, ag, ncomp, X, r, ev, blk, ex = NULL) {
  nb <- length(ag$t)
  if (!nb) return(st)
  bend <- cumsum(tabulate(blk, nb))
  seg <- c(TRUE, ag$s[-1L] != ag$s[-nb])
  sid <- cumsum(seg)
  for (k in seq_len(sid[nb])) {
    kb <- which(sid == k)
    s <- ag$s[kb[1L]]
    if (is.null(st$cur) || s != st$cur) {
      st$S0 <- 0
      st$S1[] <- 0
      st$S2c[] <- 0
      st$Lam <- 0
      st$cur <- s
    }
    kc <- kb[kb <= ncomp]
    nc <- length(kc)
    if (!nc) next
    S0 <- cumsum(c(st$S0, ag$R0[kc]))[-1L]
    S1 <- apply(rbind(st$S1, ag$R1[kc, , drop = FALSE]), 2L, cumsum)
    S1 <- matrix(S1, nc + 1L, st$p)[-1L, , drop = FALSE]
    E1 <- ag$E1[kc, , drop = FALSE]
    w <- .h_st_cox_wts(S0, ag$E0[kc], ag$d[kc], st$ties)
    w1 <- w[, "w1"]
    rc <- rev(cumsum(rev(w1)))
    st$loglik <- st$loglik + sum(ag$LE[kc]) - sum(w[, "lg"])
    st$U <- st$U + colSums(ag$XE[kc, , drop = FALSE]) - colSums(S1 * w1) + colSums(E1 * w[, "w2"])
    st$I <- st$I - (crossprod(S1, w[, "q1"] * S1) - crossprod(S1, w[, "q2"] * E1) -
                      crossprod(E1, w[, "q2"] * S1) + crossprod(E1, w[, "q3"] * E1))
    st$I <- st$I + rc[1L] * st$S2c
    if (!is.null(ex) && kc[1L] == 1L) {
      st$I <- st$I + rc[1L] * ex$S2 - w[1L, "w2"] * ex$E2
      st$S2c <- st$S2c + ex$S2
    }
    lo <- if (kc[1L] > 1L) bend[kc[1L] - 1L] + 1L else 1L
    hi <- bend[kc[nc]]
    if (hi >= lo) {
      i <- lo:hi
      Xr <- X[i, , drop = FALSE]
      rr <- r[i]
      lb <- blk[i] - kc[1L] + 1L
      st$I <- st$I + crossprod(Xr, (rr * rc[lb]) * Xr)
      st$S2c <- st$S2c + crossprod(Xr, rr * Xr)
      e <- which(ev[i] == 1)
      if (length(e) && !identical(st$ties, "breslow")) {
        Xe <- Xr[e, , drop = FALSE]
        st$I <- st$I - crossprod(Xe, (rr[e] * w[lb[e], "w2"]) * Xe)
      }
    }
    st$S0 <- S0[nc]
    st$S1 <- S1[nc, ]
    st$Lam <- cumsum(c(st$Lam, w1))[nc + 1L]   # not used by the fit; kept for Task 6's baseline hazard
  }
  st
}

#' One chunk of the streamed Cox step. time is the mapped time (DESC within each stratum), event 0/1, X
#' the centred design rows, eta = X %*% beta and strat the stratum id. Every tie block that ends inside
#' the chunk is completed; the last block is held back in st$pend until the next chunk or the close.
.h_st_cox_step <- function(st, time, event, X, eta, strat) {
  n <- length(time)
  if (!n) return(st)
  X <- matrix(as.numeric(X), n, st$p)
  time <- as.numeric(time)
  ev <- as.numeric(event)
  eta <- as.numeric(eta)
  st$sumabs <- st$sumabs + colSums(abs(X))
  r <- exp(eta)
  brk <- c(TRUE, strat[-1L] != strat[-n] | time[-1L] != time[-n])
  blk <- cumsum(brk)
  nb <- blk[n]
  rx <- r * X
  rs <- function(v) rowsum(v, blk, reorder = FALSE)
  ag <- list(s = strat[brk], t = time[brk], R0 = rs(r)[, 1L], R1 = rs(rx), E0 = rs(ev * r)[, 1L],
             E1 = rs(ev * rx), d = rs(ev)[, 1L], LE = rs(ev * eta)[, 1L], XE = rs(ev * X))
  lr <- which(blk == nb)
  le <- lr[ev[lr] == 1]
  ex_last <- list(E2 = crossprod(X[le, , drop = FALSE], r[le] * X[le, , drop = FALSE]),
                  S2 = crossprod(X[lr, , drop = FALSE], r[lr] * X[lr, , drop = FALSE]))
  ex_first <- NULL
  pe <- st$pend
  st$pend <- NULL
  if (!is.null(pe)) {
    if (ag$s[1L] == pe$s && ag$t[1L] == pe$t) {
      for (v in c("R0", "E0", "d", "LE")) ag[[v]][1L] <- ag[[v]][1L] + pe[[v]]
      for (v in c("R1", "E1", "XE")) ag[[v]][1L, ] <- ag[[v]][1L, ] + pe[[v]][1L, ]
      if (nb == 1L) {
        ex_last$E2 <- ex_last$E2 + pe$E2
        ex_last$S2 <- ex_last$S2 + pe$S2
      } else {
        ex_first <- list(E2 = pe$E2, S2 = pe$S2)
      }
    } else {
      st <- .h_st_cox_blocks(st, pe, 1L, X[0L, , drop = FALSE], numeric(), numeric(), integer(),
                             list(E2 = pe$E2, S2 = pe$S2))
    }
  }
  st <- .h_st_cox_blocks(st, ag, nb - 1L, X, r, ev, blk, ex_first)
  st$pend <- list(s = ag$s[nb], t = ag$t[nb], R0 = ag$R0[nb], R1 = ag$R1[nb, , drop = FALSE],
                  E0 = ag$E0[nb], E1 = ag$E1[nb, , drop = FALSE], d = ag$d[nb], LE = ag$LE[nb],
                  XE = ag$XE[nb, , drop = FALSE], E2 = ex_last$E2, S2 = ex_last$S2)
  st
}

#' The time-order guard of a chunk (S10): within one stratum the analysed .time must not increase, the
#' last stratum and time carried across chunks in st (NULL at the start of a pass, then list(s, t)).
#' Returns the updated carry, or stops when a time rises above the one before it in the same stratum.
.h_st_time_guard <- function(st, time, strat) {
  n <- length(time)
  if (!n) return(st)
  s <- c(if (!is.null(st)) st$s, strat)
  t <- c(if (!is.null(st)) st$t, time)
  k <- length(t)
  if (k > 1L && any(s[-1L] == s[-k] & t[-1L] > t[-k])) {
    stop("the streamed Cox needs .time to keep the order of the time column")
  }
  list(s = strat[n], t = time[n])
}

#' Flush the pending block and return list(loglik, U, I) of the pass.
.h_st_cox_close <- function(st) {
  pe <- st$pend
  if (!is.null(pe)) {
    st <- .h_st_cox_blocks(st, pe, 1L, matrix(0, 0L, st$p), numeric(), numeric(), integer(),
                           list(E2 = pe$E2, S2 = pe$S2))
  }
  list(loglik = st$loglik, U = st$U, I = st$I)
}

#' The Newton-Raphson driver of the streamed Cox fit, as survival 3.8.3's coxfit6 (S4, S5, S6):
#' list(coef, V, U, loglik, iter, flag, sing, passes, max_eta, maxit).
#'   passfn(beta) runs one pass at beta and returns list(loglik, U, I, max_eta) in centred, unscaled
#'     coordinates; max_eta is max(lp), coxph's signed linear predictor, at beta.
#'   scale is the S1 vector, or a function of the first pass's result (beta = 0) returning it.
#' Only the Cholesky runs in scaled coordinates: U_s = U s, I_s = I outer(s, s), step = step_s s, and
#' V = chinv2(I_s) outer(s, s). Each iteration evaluates newbeta and runs cholesky2; the convergence
#' test (all finite, |1 - lbest / newlk| <= eps) runs before the reject test; a rejected (or non-finite)
#' step is pulled toward the best beta with newbeta = (newbeta + h beta) / (h + 1), an accepted one takes
#' the next Newton step. There is no break at the last iteration, so without convergence iter is
#' maxit + 1 and flag 1000; when maxit > 1 the best beta is then evaluated once more (loglik and U at it)
#' and coxfit6 runs chinv2 on that raw information without a new cholesky2, which leaves V diagonal:
#' 1 / I_jj where I_jj > 0. With maxit = 1 the iteration's own score and factored information are kept.
#' On convergence flag is the rank of the information, or -2 when step-halving ran since the last
#' accepted step. loglik is c(at the start, at the final beta); passes counts the passfn calls. When
#' flag < p, a coordinate with diag(V) = 0 is singular and gets an NA coefficient.
.h_st_coxfit <- function(passfn, p, nms, scale, maxit = 20L, eps = 1e-9) {
  p <- as.integer(p)
  maxit <- as.integer(maxit)
  toler <- .Machine$double.eps^0.75
  passes <- 0L
  run <- function(b) {
    passes <<- passes + 1L
    passfn(b)
  }
  scl <- function(r) matrix(r$I, p, p) * outer(s, s)
  beta <- numeric(p)
  res <- run(beta)
  s <- as.numeric(if (is.function(scale)) scale(res) else scale)
  loglik0 <- res$loglik
  ch <- .h_st_chol2(scl(res), toler)
  newbeta <- beta + .h_st_chsolve2(ch, res$U * s) * s
  lbest <- loglik0
  atbeta <- res                          # the pass at the current best beta (its max_eta)
  iter <- 0L
  conv <- maxit == 0L
  halving <- 0L
  Vs <- NULL
  if (maxit > 0L) {
    for (iter in seq_len(maxit)) {
      res <- run(newbeta)
      ch <- .h_st_chol2(scl(res), toler)
      newlk <- res$loglik
      notfinite <- !all(is.finite(c(res$U, res$I, newlk)))
      if (!notfinite && isTRUE(abs(1 - lbest / newlk) <= eps)) {
        beta <- newbeta
        lbest <- newlk
        atbeta <- res
        conv <- TRUE
        break
      }
      if (notfinite || isTRUE(newlk < lbest)) {   # a C comparison with NaN is false
        halving <- halving + 1L
        newbeta <- (newbeta + halving * beta) / (halving + 1)
      } else {
        halving <- 0L
        lbest <- newlk
        beta <- newbeta
        atbeta <- res
        newbeta <- beta + .h_st_chsolve2(ch, res$U * s) * s
      }
    }
    if (!conv) {
      iter <- maxit + 1L
      if (maxit > 1L) {
        res <- run(beta)
        lbest <- res$loglik
        atbeta <- res
        d <- diag(scl(res))
        v <- d
        pos <- !is.na(d) & d > 0
        v[pos] <- 1 / d[pos]
        Vs <- diag(v, p, p)
      }
    }
  }
  if (!is.finite(lbest)) stop("the streamed Cox fit gave a non-finite log-likelihood at every step-halving")
  if (is.null(Vs)) Vs <- .h_st_chinv2(ch)
  V <- Vs * outer(s, s)
  flag <- if (!conv) 1000L else if (halving > 0L) -2L else sum(!ch$sing)
  sing <- if (flag < p) diag(V) == 0 else rep(FALSE, p)
  coef <- beta
  if (maxit > 0L) coef[sing] <- NA
  U <- as.numeric(res$U)
  names(coef) <- names(U) <- names(sing) <- nms
  dimnames(V) <- list(nms, nms)
  list(coef = coef, V = V, U = U, loglik = c(loglik0, lbest), iter = as.integer(iter), flag = as.integer(flag),
       sing = sing, passes = passes, max_eta = atbeta$max_eta, maxit = maxit)
}

#' The warnings of survival:::coxph.fit for a fit from .h_st_coxfit, in order (S7), with control eps:
#' none when maxit <= 1. Flag 1000 gives "Ran out of iterations and did not converge", followed by "one or
#' more coefficients may be infinite" when max_eta > 500 or abs(U V) is not finite. Otherwise a column j
#' with a non-finite score, or abs(U V)_j > eps and > sqrt(eps) |coef_j|, is listed (design columns,
#' singular ones included) in coxph's paste() text, whose default sep puts spaces around the indices.
.h_st_cox_warn <- function(fit, eps = 1e-9) {
  if (fit$maxit <= 1L) return(character())
  cf <- as.numeric(fit$coef)
  cf[is.na(cf)] <- 0
  infs <- abs(as.numeric(fit$U) %*% fit$V)
  if (fit$flag == 1000L) {
    w <- "Ran out of iterations and did not converge"
    if (fit$max_eta > 500 || any(!is.finite(infs))) w <- c(w, "one or more coefficients may be infinite")
    return(w)
  }
  infs <- !is.finite(fit$U) | (infs > eps & infs > sqrt(eps) * abs(cf))
  if (!any(infs)) return(character())
  paste("Loglik converged before variable ", paste(which(infs), collapse = ","),
        "; coefficient may be infinite. ")
}
