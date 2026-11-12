# P1e.2 Task 3: the Cholesky port (cholesky2 / chsolve2 / chinv2) and the streamed Cox chunk step with tie
# blocks and strata carried across chunk boundaries. The reference is survival::coxph at a fixed beta
# (init = beta, iter.max = 0). cox_eval and cox_ref live in helper-surv.R. Synthetic data only.

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

# step vs coxph at beta (1e-10), and chunks 1 and 7 vs 1e5 (1e-12)
.cs_check <- function(df, xcols, beta, ties, strata = NULL, info = "") {
  ref <- cox_ref(beta, df, xcols, ties, strata)
  big <- cox_eval(beta, df, xcols, ties, chunk = 1e5L, strata = strata)
  expect_equal(big$loglik, ref$loglik, tolerance = 1e-10, info = info)
  expect_equal(solve(big$I), ref$V, tolerance = 1e-10, info = info)
  for (ch in c(1L, 7L)) {
    got <- cox_eval(beta, df, xcols, ties, chunk = ch, strata = strata)
    expect_equal(got$loglik, big$loglik, tolerance = 1e-12, info = paste(info, ch))
    expect_equal(got$U, big$U, tolerance = 1e-12, info = paste(info, ch))
    expect_equal(got$I, big$I, tolerance = 1e-12, info = paste(info, ch))
    expect_equal(got$sumabs, big$sumabs, tolerance = 1e-12, info = paste(info, ch))
  }
  invisible(big)
}

test_that("chol2 matches solve on a full-rank matrix and flags a singular column", {
  set.seed(1)
  M <- crossprod(matrix(rnorm(40), 10, 4))
  b <- rnorm(4)
  ch <- .h_st_chol2(M, 1e-9^0.75)
  expect_identical(ch$sing, rep(FALSE, 4))
  expect_equal(.h_st_chsolve2(ch, b), solve(M, b), tolerance = 1e-12)
  expect_equal(.h_st_chinv2(ch), solve(M), tolerance = 1e-12)
  Z <- matrix(rnorm(40), 10, 4)
  Z[, 3] <- Z[, 1] + Z[, 2]
  S <- crossprod(Z)
  cs <- .h_st_chol2(S, 1e-9^0.75)
  expect_identical(cs$sing, c(FALSE, FALSE, TRUE, FALSE))
  V <- .h_st_chinv2(cs)
  expect_identical(V[3, ], rep(0, 4))
  expect_identical(V[, 3], rep(0, 4))
  expect_equal(V[-3, -3], solve(S[-3, -3]), tolerance = 1e-10)
  x <- .h_st_chsolve2(cs, b)
  expect_identical(x[3], 0)
  expect_equal(x[-3], solve(S[-3, -3], b[-3]), tolerance = 1e-8)
  # the solve equals survival's own cholesky2 + chsolve2 (coxph.wtest), singular column included
  expect_equal(x, survival:::coxph.wtest(S, b, 1e-9^0.75)$solve, tolerance = 1e-12)
  expect_equal(.h_st_chsolve2(ch, b), survival:::coxph.wtest(M, b, 1e-9^0.75)$solve, tolerance = 1e-12)
  # an all-zero column (a factor level absent from the rows) is singular
  S0 <- S[-3, -3]
  S0[2, ] <- 0
  S0[, 2] <- 0
  expect_identical(.h_st_chol2(S0, 1e-9^0.75)$sing, c(FALSE, TRUE, FALSE))
})

test_that("cox step loglik, score and information equal coxph at beta", {
  d <- .cs_data()
  xc <- c("x1", "x2", "yr")
  beta <- c(0.3, -0.5, 0.05)
  # efron and breslow, heavy integer ties, blocks straddling chunk 7, a covariate around 2000
  for (ties in c("efron", "breslow")) .cs_check(d, xc, beta, ties, info = ties)
  # two strata, the first ending exactly at a chunk boundary (42 = 6 * 7 rows)
  d$s <- c(rep("a", 42L), rep("b", nrow(d) - 42L))
  for (ties in c("efron", "breslow")) .cs_check(d, xc, beta, ties, strata = "s", info = paste("strata", ties))
  # an all-censored stratum in the middle
  d3 <- .cs_data(150L, 8L)
  d3$s <- rep(c("a", "b", "c"), each = 50L)
  d3$status[d3$s == "b"] <- 0L
  for (ties in c("efron", "breslow")) {
    .cs_check(d3, xc, beta, ties, strata = "s", info = paste("censored stratum", ties))
  }
  # a single event
  d1 <- .cs_data(60L, 9L)
  d1$status[] <- 0L
  d1$status[17L] <- 1L
  for (ties in c("efron", "breslow")) .cs_check(d1, c("x1", "yr"), c(0.4, -0.2), ties, info = paste("one", ties))
})

# A direct O(n^2) reference: at each distinct event time of each stratum, the risk set (time >= t) gives
# S0, S1 and S2 explicitly, and the Efron sum over l = 0..d-1 is taken term by term.
.cs_direct <- function(beta, df, xcols, ties, s) {
  X <- as.matrix(df[, xcols, drop = FALSE])
  X <- unname(sweep(X, 2L, colMeans(X)))
  r <- exp(drop(X %*% beta))
  p <- ncol(X)
  ll <- 0
  U <- numeric(p)
  I <- matrix(0, p, p)
  for (g in unique(s)) {
    for (t in unique(df$time[s == g & df$status == 1])) {
      rs <- s == g & df$time >= t
      ev <- rs & df$time == t & df$status == 1
      dk <- sum(ev)
      S0 <- sum(r[rs])
      S1 <- colSums(r[rs] * X[rs, , drop = FALSE])
      S2 <- crossprod(X[rs, , drop = FALSE], r[rs] * X[rs, , drop = FALSE])
      E0 <- sum(r[ev])
      E1 <- colSums(r[ev] * X[ev, , drop = FALSE])
      E2 <- crossprod(X[ev, , drop = FALSE], r[ev] * X[ev, , drop = FALSE])
      ll <- ll + sum(log(r[ev]))
      U <- U + colSums(X[ev, , drop = FALSE])
      for (l in 0:(dk - 1L)) {
        f <- if (ties == "efron") l / dk else 0
        D <- S0 - f * E0
        a <- S1 - f * E1
        ll <- ll - log(D)
        U <- U - a / D
        I <- I + (S2 - f * E2) / D - tcrossprod(a) / D^2
      }
    }
  }
  list(loglik = ll, U = U, I = I)
}

test_that("the deferred S2 identity holds", {
  d <- .cs_data(200L, 21L, ntime = 25L)
  d$s <- rep(c("a", "b"), c(91L, 109L))
  xc <- c("x1", "x2", "yr")
  beta <- c(-0.2, 0.7, 0.1)
  for (ties in c("efron", "breslow")) {
    so <- .cox_sorted(d, "s")
    ref <- .cs_direct(beta, so$df, xc, ties, so$s)
    for (ch in c(7L, 1e5L)) {
      got <- cox_eval(beta, d, xc, ties, chunk = ch, strata = "s")
      expect_equal(got$I, ref$I, tolerance = 1e-12, info = ties)
      expect_equal(got$U, ref$U, tolerance = 1e-12, info = ties)
      expect_equal(got$loglik, ref$loglik, tolerance = 1e-12, info = ties)
    }
  }
})

test_that("the information stays accurate when max|eta| is large", {
  set.seed(31)
  n <- 300L
  d <- data.frame(time = rexp(n), status = rbinom(n, 1L, 0.9), x1 = rnorm(n, sd = 2))
  d$time <- round(d$time * exp(-1.5 * d$x1), 2)   # some ties, so Efron differs from Breslow
  d$s <- sample(c("a", "b"), n, replace = TRUE)
  y <- data.frame(time = round(rexp(200L), 1), status = rbinom(200L, 1L, 0.8),
                  yr = 2000 + rnorm(200L, sd = 3), x1 = rnorm(200L))
  y$s <- rep(c("a", "b"), each = 100L)
  cases <- list(list(d, "x1", 4, NULL), list(d, "x1", 6, NULL), list(d, "x1", 6, "s"),
                list(y, c("yr", "x1"), c(2.5, 0.5), NULL), list(y, c("yr", "x1"), c(2.5, 0.5), "s"))
  for (cs in cases) {
    df <- cs[[1L]]
    xc <- cs[[2L]]
    beta <- cs[[3L]]
    stv <- cs[[4L]]
    X <- as.matrix(df[, xc, drop = FALSE])
    X <- sweep(X, 2L, colMeans(X))
    info <- paste(xc[1L], paste(beta, collapse = ","), if (is.null(stv)) "" else "strata")
    expect_gt(max(abs(X %*% beta)), 20)          # the fixture really has a large max|eta|
    for (ties in c("efron", "breslow")) {
      ref <- cox_ref(beta, df, xc, ties, stv)
      so <- .cox_sorted(df, stv)
      dr <- .cs_direct(beta, so$df, xc, ties, so$s)
      expect_true(all(is.finite(ref$V)))
      for (ch in c(7L, 1e5L)) {
        got <- cox_eval(beta, df, xc, ties, chunk = ch, strata = stv)
        expect_equal(got$loglik, ref$loglik, tolerance = 1e-10, info = paste(info, ties, ch))
        expect_equal(solve(got$I), ref$V, tolerance = 1e-10, info = paste(info, ties, ch))
        expect_equal(got$I, dr$I, tolerance = 1e-10, info = paste(info, ties, ch))
      }
    }
  }
})

test_that("the Efron weights of one huge tied block equal the term-by-term sum", {
  S0 <- c(5000, 40, 12, 7)
  E0 <- c(3000, 10, 3, 0)
  d <- c(2500, 1, 3, 0)
  w <- .h_st_cox_wts(S0, E0, d, "efron")
  for (k in 1:3) {
    f <- (0:(d[k] - 1)) / d[k]
    D <- S0[k] - f * E0[k]
    ref <- c(sum(1 / D), sum(f / D), sum(1 / D^2), sum(f / D^2), sum(f^2 / D^2), sum(log(D)))
    expect_equal(unname(w[k, ]), ref, tolerance = 1e-12)
  }
  expect_identical(unname(w[4L, ]), rep(0, 6))
})

test_that("the Cox step helpers are self-contained and inline cleanly", {
  fns <- c(".h_st_chol2", ".h_st_chsolve2", ".h_st_chinv2", ".h_st_cox_wts", ".h_st_cox_new",
           ".h_st_cox_blocks", ".h_st_cox_step", ".h_st_cox_close")
  for (f in fns) {
    fn <- get(f)
    code <- .sr_inline(fn, f)
    expect_false(grepl("[^\x01-\x7F]", code, perl = TRUE))
    g <- codetools::findGlobals(fn, merge = FALSE)$functions
    own <- g[!vapply(g, function(x) exists(x, envir = baseenv()) || exists(x, envir = asNamespace("stats")) ||
                       x %in% fns, NA)]
    expect_identical(own, character(), info = f)
  }
})
