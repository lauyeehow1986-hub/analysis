# Streaming lm core: TSQR fit, aliasing and streamed sandwich meats (P1e.1 Task 6). Synthetic data only.
#
# The reference values come from stats::lm / lm.fit and sandwich on the same rows. Tolerances are those
# of spec section 10: 1e-8 for estimates, model SEs and fit statistics, 1e-7 for robust and cluster SEs.

# The data and the design: yc ~ x1 + grp + x2, plus a duplicate of x1 (aliased) and, when asked, a
# constant column (aliased with the intercept).
.lmc_data <- function(n = 2000L, const = FALSE) {
  df <- .sc_frame(n)
  df$grp <- factor(df$grp, levels = sort(unique(df$grp)))
  df$dup <- df$x1
  if (const) df$k3 <- 3
  df
}
.lmc_fml <- function(const = FALSE) {
  stats::as.formula(paste("yc ~ x1 + grp + x2 + dup", if (const) "+ k3"))
}
.lmc_X <- function(df, const = FALSE) stats::model.matrix(.lmc_fml(const), df)

# Fold .h_st_tsqr over row blocks of A (chunk rows each); the moments of y are folded alongside.
.lmc_fold <- function(A, y, chunk) {
  M <- NULL
  mo <- NULL
  for (s in seq(1L, nrow(A), by = chunk)) {
    i <- s:min(nrow(A), s + chunk - 1L)
    M <- .h_st_tsqr(M, A[i, , drop = FALSE])
    mo <- .h_st_moments_add(mo, y[i])
  }
  list(M = M, mo = mo)
}

.lmc_fit <- function(X, y, chunk) {
  f <- .lmc_fold(cbind(X, y), y, chunk)
  .h_st_lm_finish(f$M, ncol(X), colnames(X), nrow(X), f$mo$m2)
}

# Row blocks of 1..n.
.lmc_blocks <- function(n, chunk) {
  s <- seq(1L, n, by = chunk)
  lapply(s, function(a) a:min(n, a + chunk - 1L))
}

test_that("TSQR lm equals lm for chunk sizes 7, 1000 and > n", {
  df <- .lmc_data()
  X <- .lmc_X(df)
  y <- df$yc
  ref <- stats::lm.fit(X, y)$coefficients
  fit <- stats::lm(.lmc_fml(), data = df)
  sm <- summary(fit)
  expect_true(anyNA(ref))
  for (chunk in c(3L, 7L, 1000L, nrow(X) + 5L)) {
    st <- .lmc_fit(X, y, chunk)
    expect_identical(names(st$beta), colnames(X), info = paste("chunk", chunk))
    expect_identical(is.na(st$beta), is.na(ref), info = paste("chunk", chunk))
    expect_equal(st$beta, ref, tolerance = 1e-8, info = paste("chunk", chunk))
    expect_identical(st$aliased, names(ref)[is.na(ref)], info = paste("chunk", chunk))
    expect_identical(st$keep, unname(which(!is.na(ref))), info = paste("chunk", chunk))
    expect_identical(st$rank, fit$rank, info = paste("chunk", chunk))
    expect_equal(st$df_resid, fit$df.residual, info = paste("chunk", chunk))
    expect_equal(st$rss, sum(stats::residuals(fit)^2), tolerance = 1e-8, info = paste("chunk", chunk))
    expect_equal(st$sigma, sm$sigma, tolerance = 1e-8, info = paste("chunk", chunk))
    expect_equal(st$r2, sm$r.squared, tolerance = 1e-8, info = paste("chunk", chunk))
    expect_equal(st$adj_r2, sm$adj.r.squared, tolerance = 1e-8, info = paste("chunk", chunk))
    # the model V: sigma^2 chol2inv(R), in keep order, equals vcov(lm)
    V <- .h_st_vcov_finish("model", st$B, st$sigma^2, st$n, st$rank, NA)
    nm <- names(ref)[!is.na(ref)]
    expect_identical(dimnames(V), list(nm, nm))
    expect_equal(V, stats::vcov(fit)[nm, nm], tolerance = 1e-8, info = paste("chunk", chunk))
  }
})

test_that("a constant column is aliased with the intercept as lm does", {
  df <- .lmc_data(const = TRUE)
  X <- .lmc_X(df, const = TRUE)
  ref <- stats::lm.fit(X, df$yc)$coefficients
  expect_true(is.na(ref[["k3"]]))
  for (chunk in c(7L, 500L)) {
    st <- .lmc_fit(X, df$yc, chunk)
    expect_identical(is.na(st$beta), is.na(ref), info = paste("chunk", chunk))
    expect_equal(st$beta, ref, tolerance = 1e-8, info = paste("chunk", chunk))
    expect_identical(st$aliased, c("dup", "k3"), info = paste("chunk", chunk))
  }
})

test_that("all-zero column in a chunk does not permute", {
  df <- .lmc_data()
  df$grp[1:50][df$grp[1:50] == "C"] <- "A"
  X <- .lmc_X(df)
  expect_true(all(X[1:50, "grpC"] == 0))
  y <- df$yc
  full <- .lmc_fit(X, y, nrow(X) + 1L)
  ref <- stats::lm.fit(X, y)$coefficients
  for (chunk in c(50L, 7L)) {
    st <- .lmc_fit(X, y, chunk)
    expect_identical(colnames(.lmc_fold(cbind(X, y), y, chunk)$M), c(colnames(X), "y"), info = paste("chunk", chunk))
    expect_identical(is.na(st$beta), is.na(full$beta), info = paste("chunk", chunk))
    expect_equal(st$beta, full$beta, tolerance = 1e-8, info = paste("chunk", chunk))
    expect_equal(st$beta, ref, tolerance = 1e-8, info = paste("chunk", chunk))
    expect_equal(st$sigma, full$sigma, tolerance = 1e-8, info = paste("chunk", chunk))
    # crossprod(M) is the invariant: X'X of the whole design
    M <- .lmc_fold(cbind(X, y), y, chunk)$M
    expect_equal(unname(crossprod(M)), unname(crossprod(cbind(X, y))), tolerance = 1e-8, info = paste("chunk", chunk))
  }
  # a first block whose zero column is in the middle merges into the same triangle
  B <- cbind(X, y)[1:50, ]
  M1 <- .h_st_tsqr(NULL, B)
  expect_identical(colnames(M1), colnames(B))
  expect_equal(unname(crossprod(M1)), unname(crossprod(B)), tolerance = 1e-10)
})

test_that("TSQR handles chunks smaller than the number of columns", {
  df <- .lmc_data(200L)
  X <- .lmc_X(df)
  M <- NULL
  for (i in .lmc_blocks(nrow(X), 2L)) M <- .h_st_tsqr(M, cbind(X, df$yc)[i, , drop = FALSE])
  expect_identical(dim(M), c(ncol(X) + 1L, ncol(X) + 1L))
  expect_equal(.h_st_lm_finish(M, ncol(X), colnames(X), nrow(X), sum((df$yc - mean(df$yc))^2))$beta,
               stats::lm.fit(X, df$yc)$coefficients, tolerance = 1e-8)
})

test_that("streamed moments equal the total sum of squares", {
  y <- .sc_frame(1003L)$yc
  for (chunk in c(1L, 7L, 1000L, 5000L)) {
    mo <- NULL
    for (i in .lmc_blocks(length(y), chunk)) mo <- .h_st_moments_add(mo, y[i])
    expect_equal(mo$n, length(y), info = paste("chunk", chunk))
    expect_equal(mo$mean, mean(y), tolerance = 1e-12, info = paste("chunk", chunk))
    expect_equal(mo$m2, sum((y - mean(y))^2), tolerance = 1e-10, info = paste("chunk", chunk))
  }
  expect_identical(.h_st_moments_add(NULL, numeric())$n, 0)
})

test_that("HC3 by leverage equals sandwich::vcovHC HC3", {
  df <- .lmc_data()
  X <- .lmc_X(df)
  y <- df$yc
  fit <- stats::lm(.lmc_fml(), data = df)
  ref <- sandwich::vcovHC(fit, type = "HC3")
  for (chunk in c(7L, 1000L, nrow(X) + 1L)) {
    st <- .lmc_fit(X, y, chunk)
    nm <- colnames(X)[st$keep]
    S <- NULL
    hs <- numeric()
    for (i in .lmc_blocks(nrow(X), chunk)) {
      Xk <- X[i, st$keep, drop = FALSE]
      h <- colSums(backsolve(st$R, t(Xk), transpose = TRUE)^2)
      e <- y[i] - drop(Xk %*% st$beta[st$keep])
      S <- .h_st_meat_add(S, Xk, e / (1 - h))
      hs <- c(hs, h)
    }
    expect_equal(unname(hs), unname(stats::hatvalues(fit)), tolerance = 1e-8, info = paste("chunk", chunk))
    V <- .h_st_vcov_finish("robust", st$B, S, st$n, st$rank, NA)
    expect_equal(V, ref[nm, nm], tolerance = 1e-7, info = paste("chunk", chunk))
  }
  # HC0: the meat of the plain residuals
  st <- .lmc_fit(X, y, 7L)
  Xk <- X[, st$keep]
  S0 <- .h_st_meat_add(NULL, Xk, y - drop(Xk %*% st$beta[st$keep]))
  nm <- colnames(X)[st$keep]
  expect_equal(.h_st_vcov_finish("robust", st$B, S0, st$n, st$rank, NA),
               sandwich::vcovHC(fit, type = "HC0")[nm, nm], tolerance = 1e-7)
})

# One sorted cluster pass: scores x_i e_i folded by .h_st_cluster_step over row blocks.
.lmc_cluster <- function(X, e, g, chunk) {
  st <- NULL
  for (i in .lmc_blocks(nrow(X), chunk)) st <- .h_st_cluster_step(st, X[i, , drop = FALSE], e[i], g[i])
  .h_st_cluster_flush(st)
}

test_that("cluster HC1 equals vcovCL, clusters crossing chunks and the last row", {
  df <- .lmc_data()
  df <- df[order(df$pid, df$id), ]
  # a cluster of at least 9 rows straddling the 7-row chunk boundary at 14/15, and a cluster ending on
  # the last row (the ids stay sorted and contiguous)
  df$pid[10:18] <- df$pid[10]
  df$pid[(nrow(df) - 3L):nrow(df)] <- max(df$pid) + 1
  expect_gte(sum(df$pid == df$pid[10]), 9L)
  expect_true(all(diff(df$pid) >= 0))
  rownames(df) <- NULL
  X <- .lmc_X(df)
  y <- df$yc
  fit <- stats::lm(.lmc_fml(), data = df)
  # the vector form of cluster = ~pid (the formula is evaluated where .lmc_fml made it)
  ref <- sandwich::vcovCL(fit, cluster = df$pid, type = "HC1")
  G <- length(unique(df$pid))
  for (chunk in c(7L, 1000L, nrow(X) + 1L)) {
    st <- .lmc_fit(X, y, chunk)
    nm <- colnames(X)[st$keep]
    Xk <- X[, st$keep]
    e <- y - drop(Xk %*% st$beta[st$keep])
    cl <- .lmc_cluster(Xk, e, df$pid, chunk)
    expect_identical(cl$G, G, info = paste("chunk", chunk))
    expect_null(cl$U)
    V <- .h_st_vcov_finish("cluster", st$B, cl$S, st$n, st$rank, cl$G)
    expect_equal(V, ref[nm, nm], tolerance = 1e-7, info = paste("chunk", chunk))
  }
  # text cluster ids work alike
  cl <- .lmc_cluster(X[, st$keep], y - drop(X[, st$keep] %*% st$beta[st$keep]),
                     paste0("c", df$pid), 7L)
  expect_identical(cl$G, G)
})

test_that("a closed cluster that reappears is read out of order", {
  X <- cbind(1, 1:6)
  e <- c(1, -1, 2, -2, 1, 1)
  # within one chunk
  expect_error(.h_st_cluster_step(NULL, X, e, c(1, 1, 2, 2, 1, 1)), "extract read out of order")
  # across chunks
  st <- .h_st_cluster_step(NULL, X[1:3, ], e[1:3], c(1, 1, 2))
  st <- .h_st_cluster_step(st, X[4, , drop = FALSE], e[4], 2)
  expect_error(.h_st_cluster_step(st, X[5:6, ], e[5:6], c(3, 1)), "extract read out of order")
  # the open cluster continuing into the next chunk is fine
  st <- .h_st_cluster_step(NULL, X[1:3, ], e[1:3], c(1, 2, 2))
  st <- .h_st_cluster_step(st, X[4:6, ], e[4:6], c(2, 2, 3))
  expect_identical(.h_st_cluster_flush(st)$G, 3L)
})

test_that("vcov finish refuses a single cluster with the in-memory text", {
  B <- diag(2)
  expect_error(.h_st_vcov_finish("cluster", B, diag(2), 10, 2, 1),
               "se = cluster needs at least two clusters (found 1)", fixed = TRUE)
  expect_error(.h_st_vcov_finish("bogus", B, diag(2), 10, 2, 1), "se")
})

test_that("streamed lm through DuckDB chunks equals lm, complete cases decided per chunk", {
  df <- .sc_frame(2000L)
  # missing values (R8: complete cases in R, per chunk), a whole 7-row chunk of them (an empty block)
  df$x1[c(3, 40, 41, 1999)] <- NA
  df$yc[c(5, 700)] <- NA
  df$x2[15:21] <- NA
  path <- .sc_parquet(df)
  con <- .sc_con(path)
  src <- sprintf("read_parquet('%s')", gsub("'", "''", path))
  lv <- sort(unique(df$grp))
  des <- function(d) {
    d$grp <- factor(d$grp, levels = lv)
    d <- d[stats::complete.cases(d[c("yc", "x1", "grp", "x2")]), , drop = FALSE]
    list(X = stats::model.matrix(~ x1 + grp + x2, d), y = d$yc)
  }
  fit <- stats::lm(yc ~ x1 + grp + x2, data = transform(df, grp = factor(grp, levels = lv)))
  sql <- sprintf("SELECT \"yc\", \"x1\", \"grp\", \"x2\", \".sr_row\" FROM %s", src)
  for (chunk in c(7L, 1000L)) {
    r <- .h_st_pass(con, sql, chunk, list(M = NULL, mo = NULL, n = 0), function(s, d) {
      b <- des(d)
      list(M = .h_st_tsqr(s$M, cbind(b$X, b$y)), mo = .h_st_moments_add(s$mo, b$y), n = s$n + length(b$y))
    })
    expect_equal(r$state$n, stats::nobs(fit))
    st <- .h_st_lm_finish(r$state$M, 6L, names(stats::coef(fit)), r$state$n, r$state$mo$m2)
    expect_equal(st$beta, stats::coef(fit), tolerance = 1e-8, info = paste("chunk", chunk))
    expect_equal(st$sigma, summary(fit)$sigma, tolerance = 1e-8, info = paste("chunk", chunk))
    expect_equal(st$r2, summary(fit)$r.squared, tolerance = 1e-8, info = paste("chunk", chunk))
  }
})

test_that("the lm core helpers inline as ASCII code the emit checker accepts and run standalone", {
  fns <- c(".h_st_tsqr", ".h_st_alias", ".h_st_lm_finish", ".h_st_moments_add", ".h_st_meat_add",
           ".h_st_cluster_step", ".h_st_cluster_flush", ".h_st_vcov_finish")
  code <- paste(vapply(fns, function(f) .sr_inline(match.fun(f), f), character(1)), collapse = "\n")
  expect_false(grepl("[^\\x01-\\x7F]", code, perl = TRUE))
  expect_silent(.sr_emit_check(code, "analysis", stream = TRUE))
  # evaluated where no StatRouter function is visible
  e <- new.env(parent = baseenv())
  eval(parse(text = code), e)
  df <- .lmc_data(300L)
  X <- .lmc_X(df)
  M <- NULL
  mo <- NULL
  for (i in .lmc_blocks(nrow(X), 7L)) {
    M <- e$.h_st_tsqr(M, cbind(X, df$yc)[i, , drop = FALSE])
    mo <- e$.h_st_moments_add(mo, df$yc[i])
  }
  st <- e$.h_st_lm_finish(M, ncol(X), colnames(X), nrow(X), mo$m2)
  expect_equal(st$beta, stats::lm.fit(X, df$yc)$coefficients, tolerance = 1e-8)
  cl <- e$.h_st_cluster_flush(e$.h_st_cluster_step(NULL, X[, st$keep], rep(1, nrow(X)), df$pid))
  expect_identical(cl$G, length(unique(df$pid)))
})
