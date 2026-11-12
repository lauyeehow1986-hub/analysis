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
#
# Coordinates (S1): X is centred, not scaled; eta = X %*% beta. The rows arrive sorted by stratum, then
# time DESC, so the risk set of a tie block (every row of the stratum with time >= t) is the running total
# of the rows read so far, the block included. A tie block is the run of rows with the same stratum and
# the same (mapped) time.
#
# Information. With D_l = S0 - f E0 and a_l = S1 - f E1, f = l/d (Efron) or 0 (Breslow), l = 0..d-1:
#   I = sum_blocks sum_l (S2 - f E2) / D_l - a_l a_l' / D_l^2.
# The S2 part is deferred (S12): its block weight is w1 = sum_l 1/D_l = dLam, so
#   sum_k w1_k S2(t_k) = sum_i r_i x_i x_i' (Lam_stratum - W_after(t_i)),
# with W_after(t_i) the Lam accumulated before row i's block. Per chunk A += X' r X and
# B += X' (r W_after) X; at the stratum end I += Lam_stratum A - B.
#
# Chunk carries. The last block of a chunk may continue into the next one, so it is held back as its
# block sums (st$pend), not as rows: R0 = sum r, R1 = sum r x, E0, E1, d, the event sums of eta and x, and
# E2 = sum over its events of r x x'. Its rows enter A and B at once, because their W_after (the Lam
# before the block) is already known. The stratum carries are S0, S1, Lam, A and B.

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
#'   w1 = sum_l 1/D_l (the hazard increment dLam, also the deferred S2 weight),
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
  dm <- max(c(0, d))
  if (dm > 1) {
    for (l in seq_len(dm - 1)) {
      j <- which(d > l)
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
  w
}

#' The per-pass state of the streamed Cox step for p columns and ties "efron" or "breslow": the sums
#' loglik, U and I; the stratum carries S0, S1, Lam, A and B; the current stratum cur; the pending block
#' pend (NULL, or its block sums); and sumabs, the column sums of |X| (S1).
.h_st_cox_new <- function(p, ties) {
  p <- as.integer(p)
  if (!ties %in% c("efron", "breslow")) stop("ties must be \"efron\" or \"breslow\"")
  list(p = p, ties = ties, loglik = 0, U = numeric(p), I = matrix(0, p, p), S0 = 0, S1 = numeric(p),
       Lam = 0, A = matrix(0, p, p), B = matrix(0, p, p), cur = NULL, pend = NULL, sumabs = numeric(p))
}

#' Internal to the step: process the tie blocks with sums ag (list(s, t, R0, R1, E0, E1, d, LE, XE), one
#' element or row per block, in reading order). Blocks 1..ncomp are complete; a block after them is the
#' pending one. X, r, ev and blk are the chunk's rows of these blocks (blk the block of each row, none
#' for a replayed pending block). E2x, when not NULL, is the E2 of block 1 held from earlier chunks.
.h_st_cox_blocks <- function(st, ag, ncomp, X, r, ev, blk, E2x = NULL) {
  nb <- length(ag$t)
  if (!nb) return(st)
  bend <- cumsum(tabulate(blk, nb))
  seg <- c(TRUE, ag$s[-1L] != ag$s[-nb])
  sid <- cumsum(seg)
  for (k in seq_len(sid[nb])) {
    kb <- which(sid == k)
    s <- ag$s[kb[1L]]
    if (is.null(st$cur) || s != st$cur) {
      st$I <- st$I + st$Lam * st$A - st$B
      st$S0 <- 0
      st$S1[] <- 0
      st$Lam <- 0
      st$A[] <- 0
      st$B[] <- 0
      st$cur <- s
    }
    kc <- kb[kb <= ncomp]
    nc <- length(kc)
    w <- NULL
    w1 <- numeric()
    if (nc) {
      S0 <- cumsum(c(st$S0, ag$R0[kc]))[-1L]
      S1 <- apply(rbind(st$S1, ag$R1[kc, , drop = FALSE]), 2L, cumsum)
      S1 <- matrix(S1, nc + 1L, st$p)[-1L, , drop = FALSE]
      E1 <- ag$E1[kc, , drop = FALSE]
      w <- .h_st_cox_wts(S0, ag$E0[kc], ag$d[kc], st$ties)
      w1 <- w[, "w1"]
      st$loglik <- st$loglik + sum(ag$LE[kc]) - sum(w[, "lg"])
      st$U <- st$U + colSums(ag$XE[kc, , drop = FALSE]) - colSums(S1 * w1) + colSums(E1 * w[, "w2"])
      st$I <- st$I - (crossprod(S1, w[, "q1"] * S1) - crossprod(S1, w[, "q2"] * E1) -
                        crossprod(E1, w[, "q2"] * S1) + crossprod(E1, w[, "q3"] * E1))
      if (!is.null(E2x) && kc[1L] == 1L) st$I <- st$I - w[1L, "w2"] * E2x
      st$S0 <- S0[nc]
      st$S1 <- S1[nc, ]
    }
    Lb <- cumsum(c(st$Lam, w1))
    lo <- if (kb[1L] > 1L) bend[kb[1L] - 1L] + 1L else 1L
    hi <- bend[kb[length(kb)]]
    if (hi >= lo) {
      i <- lo:hi
      Xr <- X[i, , drop = FALSE]
      rr <- r[i]
      wa <- Lb[blk[i] - kb[1L] + 1L]
      st$A <- st$A + crossprod(Xr, rr * Xr)
      st$B <- st$B + crossprod(Xr, (rr * wa) * Xr)
      e <- i[ev[i] == 1 & blk[i] <= ncomp]
      if (length(e) && !identical(st$ties, "breslow")) {
        we <- w[blk[e] - kb[1L] + 1L, "w2"]
        Xe <- X[e, , drop = FALSE]
        st$I <- st$I - crossprod(Xe, (r[e] * we) * Xe)
      }
    }
    st$Lam <- Lb[length(Lb)]
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
  last <- which(blk == nb & ev == 1)
  E2last <- crossprod(X[last, , drop = FALSE], r[last] * X[last, , drop = FALSE])
  E2first <- NULL
  pe <- st$pend
  st$pend <- NULL
  if (!is.null(pe)) {
    if (ag$s[1L] == pe$s && ag$t[1L] == pe$t) {
      for (v in c("R0", "E0", "d", "LE")) ag[[v]][1L] <- ag[[v]][1L] + pe[[v]]
      for (v in c("R1", "E1", "XE")) ag[[v]][1L, ] <- ag[[v]][1L, ] + pe[[v]][1L, ]
      if (nb == 1L) E2last <- E2last + pe$E2 else E2first <- pe$E2
    } else {
      st <- .h_st_cox_blocks(st, pe, 1L, X[0L, , drop = FALSE], numeric(), numeric(), integer(), pe$E2)
    }
  }
  st <- .h_st_cox_blocks(st, ag, nb - 1L, X, r, ev, blk, E2first)
  st$pend <- list(s = ag$s[nb], t = ag$t[nb], R0 = ag$R0[nb], R1 = ag$R1[nb, , drop = FALSE],
                  E0 = ag$E0[nb], E1 = ag$E1[nb, , drop = FALSE], d = ag$d[nb], LE = ag$LE[nb],
                  XE = ag$XE[nb, , drop = FALSE], E2 = E2last)
  st
}

#' Flush the pending block, close the stratum and return list(loglik, U, I) of the pass.
.h_st_cox_close <- function(st) {
  pe <- st$pend
  if (!is.null(pe)) {
    st <- .h_st_cox_blocks(st, pe, 1L, matrix(0, 0L, st$p), numeric(), numeric(), integer(), pe$E2)
    st$pend <- NULL
  }
  st$I <- st$I + st$Lam * st$A - st$B
  list(loglik = st$loglik, U = st$U, I = st$I)
}
