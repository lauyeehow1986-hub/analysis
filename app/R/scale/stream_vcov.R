# scale/stream_vcov.R - the streamed sandwich meats and the covariance finish of the streaming engine
# (P1e.1, spec section 6.5). They reproduce routes/se.R: sandwich::vcovHC (HC3 for lm, HC0 for glm)
# and sandwich::vcovCL(type = "HC1").
#
# Every .h_st_* function here is emitted into the analysis code with .sr_inline(), so it is
# self-contained: base and stats only, ASCII text, no StatRouter call.
#
#   robust   one pass, meat S = sum r_i^2 x_i x_i' by .h_st_meat_add: r = e / (1 - h) for the lm HC3
#            (h the leverage from the final R), r = e for HC0 (glm: the score weights of ruling R11)
#   cluster  one pass sorted by the cluster: U_g = sum of the cluster's scores, carried across chunk
#            boundaries by .h_st_cluster_step and closed once by .h_st_cluster_flush
#   V        .h_st_vcov_finish(se, B, meat, n, k, G) with the bread B = chol2inv(R)

#' Add the meat of one chunk: S + crossprod(X * r) (S = NULL starts it).
.h_st_meat_add <- function(S, X, r) {
  X <- as.matrix(X)
  A <- crossprod(X * as.numeric(r))
  if (is.null(S)) A else S + A
}

#' One chunk of the sorted cluster pass. st is NULL at the start, then list(S, U, g, G, seen): S the sum
#' of U_g U_g' over the closed clusters, U the score sum of the open cluster g (carried into the next
#' chunk), G the count of closed clusters and seen a hashed environment of the closed ids. X holds the
#' rows' design (kept columns), r the residual (or score weight) and g the cluster id of each row. A
#' closed id that appears again (within or across chunks) stops with "extract read out of order".
.h_st_cluster_step <- function(st, X, r, g) {
  X <- as.matrix(X)
  if (is.null(st)) {
    st <- list(S = matrix(0, ncol(X), ncol(X)), U = NULL, g = NULL, G = 0L,
               seen = new.env(hash = TRUE, parent = emptyenv()))
  }
  m <- length(g)
  if (!m) return(st)
  key <- paste0("k", if (is.double(g)) sprintf("%.17g", g) else as.character(g))
  brk <- c(TRUE, key[-1L] != key[-m])
  rk <- key[brk]
  if (anyDuplicated(rk)) stop("extract read out of order")
  U <- rowsum(X * as.numeric(r), cumsum(brk), reorder = FALSE)
  nr <- length(rk)
  first <- 1L
  if (!is.null(st$g) && identical(rk[1L], st$g)) {
    st$U <- st$U + U[1L, ]
    first <- 2L
  }
  if (first > nr) return(st)
  close <- function(st, ids) {
    list2env(stats::setNames(as.list(rep(TRUE, length(ids))), ids), envir = st$seen)
    st$G <- st$G + length(ids)
    st
  }
  if (!is.null(st$g)) {
    st$S <- st$S + tcrossprod(st$U)
    st <- close(st, st$g)
  }
  idx <- first:nr
  seen <- st$seen
  if (any(vapply(rk[idx], function(z) !is.null(seen[[z]]), logical(1)))) stop("extract read out of order")
  done <- idx[-length(idx)]
  if (length(done)) {
    st$S <- st$S + crossprod(U[done, , drop = FALSE])
    st <- close(st, rk[done])
  }
  st$g <- rk[nr]
  st$U <- U[nr, ]
  st
}

#' Close the last open cluster of the sorted pass. Returns the state with U = NULL, g = NULL and G the
#' number of clusters (each flushed exactly once); NULL (no rows) gives G = 0.
.h_st_cluster_flush <- function(st) {
  if (is.null(st)) return(list(S = NULL, U = NULL, g = NULL, G = 0L, seen = NULL))
  if (!is.null(st$g)) {
    st$S <- st$S + tcrossprod(st$U)
    st$seen[[st$g]] <- TRUE
    st$G <- st$G + 1L
  }
  st["U"] <- list(NULL)
  st["g"] <- list(NULL)
  st
}

#' The covariance of the kept coefficients from the bread B = chol2inv(R) (named, keep order).
#'   se = "model"    B * meat, meat the dispersion (sigma^2 for lm, 1 for glm)
#'   se = "robust"   B meat B (HC3 or HC0, decided by the weights the meat was built with)
#'   se = "cluster"  G / (G - 1) * (n - 1) / (n - k) * B meat B (HC1, vcovCL's adjustment); G >= 2
.h_st_vcov_finish <- function(se, B, meat, n, k, G) {
  if (identical(se, "model")) {
    V <- B * meat
  } else if (identical(se, "robust")) {
    V <- B %*% meat %*% B
  } else if (identical(se, "cluster")) {
    if (is.na(G) || G < 2) stop("se = cluster needs at least two clusters (found ", G, ")")
    V <- (G / (G - 1)) * ((n - 1) / (n - k)) * (B %*% meat %*% B)
  } else {
    stop("unknown se type '", se, "' (model, robust or cluster)")
  }
  dimnames(V) <- dimnames(B)
  V
}
