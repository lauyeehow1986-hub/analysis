# P1e.2 Task 6: the streamed cluster-robust SE of exact Cox (rulings S9, S20 and S22), against the in-memory
# route (coxph with cluster(), the robust variance) on the same rows. Synthetic data only. The harness
# (.ssc_surv_frame, .ssc_surv_spec, .ssc_surv_expect, .ssc_surv_chunks, .ssc_memory, .ssc_stream) is in
# helper-scale.R.

.cl_spec <- function(...) .ssc_surv_spec("cox", cluster = "pid", layers = .ssc_layers(TRUE), ...)

test_that("stream cluster SE equals coxph robust", {
  df <- .ssc_surv_frame(400L, 21L)
  # pid holds clusters of 1-9 consecutive rows; st1 is drawn per row, so most clusters span both strata
  span <- tapply(df$st1, df$pid, function(z) length(unique(z)))
  expect_true(sum(span > 1L) > 20L)
  for (ties in c("efron", "breslow")) {
    spec <- .cl_spec(ties = ties, strata = "st1", terms = list(list(var = "x1"), list(var = "grp", type = "factor"),
                                                             list(var = "yr")))
    mem <- .ssc_memory(df, spec, "cox")
    expect_identical(mem$se_type, "cluster")
    out <- list()
    for (ch in c(7L, 100000L)) {
      st <- .ssc_stream(df, spec, "cox", ch)
      info <- paste(ties, ch)
      .ssc_surv_expect(st, mem, info)
      expect_identical(st$n_clusters, mem$n_clusters, info = info)
      expect_identical(st$se_type, "cluster", info = info)
      out[[as.character(ch)]] <- st
    }
    .ssc_surv_chunks(out[[2L]], out[[1L]], paste(ties, "chunks"))
    # the robust SE is not the model SE
    mod <- .ssc_memory(df, utils::modifyList(spec, list(cluster = NULL, layers = .ssc_layers(FALSE))), "cox")
    expect_false(isTRUE(all.equal(mod$coef$se, mem$coef$se, tolerance = 1e-4)), info = ties)
  }
  # two strata columns, a chunk size between, and the passes: the model passes plus two
  spec <- .cl_spec(strata = c("st1", "st2"))
  mem <- .ssc_memory(df, spec, "cox")
  st <- .ssc_stream(df, spec, "cox", 50L)
  .ssc_surv_expect(st, mem, "two strata 50")
  expect_identical(st$passes, st$iter + 4L)
})

test_that("rows lost to a missing cluster are dropped as in memory", {
  df <- .ssc_surv_frame(300L, 22L)
  df$pid[seq(4L, 300L, by = 13L)] <- NA
  spec <- .cl_spec()
  mem <- .ssc_memory(df, spec, "cox")
  st <- .ssc_stream(df, spec, "cox", 7L)
  .ssc_surv_expect(st, mem, "missing pid")
  expect_identical(st$n_clusters, mem$n_clusters)
  # with exclude_rows, which the S22 query filters through .sr_where
  spec <- .cl_spec(exclude_rows = c(3, 40, 41, 250))
  .ssc_surv_expect(.ssc_stream(df, spec, "cox", 7L), .ssc_memory(df, spec, "cox"), "exclude_rows")
})

test_that("aliased column with clusters", {
  df <- .ssc_surv_frame(300L, 27L)
  df$time[df$grp == "C"] <- NA
  spec <- .cl_spec(exposure = "grp", exposure_ref = "A", terms = list(list(var = "x1")))
  mem <- .ssc_memory(df, spec, "cox")
  expect_true("t1fC" %in% mem$aliased)
  for (ch in c(7L, 100000L)) .ssc_surv_expect(.ssc_stream(df, spec, "cox", ch), mem, paste("aliased", ch))
})

# A possibly-infinite coefficient (monotone likelihood, max|eta| about 16): coxph's own score residuals
# lose about 1e-4 there (its robust SE of z moves from 324 to 315 when the rows are only permuted), so the
# stream is checked against a brute-force residual, built from each risk set directly, instead of memory.
test_that("possibly-infinite coefficient with clusters: residuals against brute force", {
  df <- .ssc_surv_frame(300L, 27L)
  df$z <- as.numeric(df$time < stats::median(df$time) & df$status == 1)
  fit <- suppressWarnings(survival::coxph(survival::Surv(time, status) ~ x1 + z, data = df, ties = "breslow"))
  beta <- stats::coef(fit)
  X <- sweep(as.matrix(df[, c("x1", "z")]), 2L, colMeans(df[, c("x1", "z")]))
  eta <- drop(X %*% beta)
  expect_true(max(abs(eta)) > 10)
  r <- exp(eta)
  ev <- df$status
  mt <- .h_st_timefix_apply(df$time, .h_st_timefix_map(df$time))
  # brute force (Breslow): xbar and dLam at each event time from its whole risk set
  tk <- sort(unique(mt[ev == 1]))
  xb <- t(vapply(tk, function(t) colSums(r[mt >= t] * X[mt >= t, , drop = FALSE]) / sum(r[mt >= t]), numeric(2)))
  dl <- vapply(tk, function(t) sum(ev[mt == t]) / sum(r[mt >= t]), numeric(1))
  bf <- t(vapply(seq_len(nrow(df)), function(i) {
    k <- which(tk <= mt[i])
    own <- if (ev[i] == 1) X[i, ] - xb[match(mt[i], tk), ] else c(0, 0)
    own - r[i] * colSums(dl[k] * (matrix(X[i, ], length(k), 2L, byrow = TRUE) - xb[k, , drop = FALSE]))
  }, numeric(2)))
  # the streamed event-time table at chunk 7, joined in R
  o <- order(-df$time, seq_len(nrow(df)))
  st <- .h_st_cox_new(2L, "breslow")
  et <- NULL
  for (i in split(o, ceiling(seq_along(o) / 7L))) {
    st <- .h_st_et_step(st, mt[i], ev[i], X[i, , drop = FALSE], eta[i], rep(1, length(i)))
    et <- rbind(et, st$et)
  }
  et <- rbind(et, .h_st_et_close(st))
  et <- et[order(et$time), ]
  tab <- data.frame(lam = cumsum(et$dlam), c1 = et$c1, xbm_1 = et$xbm_1, xbm_2 = et$xbm_2, b_1 = cumsum(et$db_1),
                    b_2 = cumsum(et$db_2), c2_1 = et$c2_1, c2_2 = et$c2_2)
  j <- findInterval(df$time, et$time)
  j[j == 0L] <- NA
  res <- .h_st_cox_resid(X, r, ev, tab[j, , drop = FALSE])
  scl <- max(abs(bf))
  expect_lt(max(abs(res - bf)) / scl, 1e-10)
  # coxph's own residuals are farther from the brute force than the stream's
  expect_gt(max(abs(stats::residuals(fit, type = "score") - bf)), 100 * max(abs(res - bf)))
  # the full run: the estimates agree with memory; the SEs agree between chunk sizes
  spec <- .cl_spec(ties = "breslow", terms = list(list(var = "x1"), list(var = "z")))
  mem <- .ssc_memory(df, spec, "cox")
  expect_true(length(mem$warnings) > 0L)
  a <- .ssc_stream(df, spec, "cox", 7L)
  b <- .ssc_stream(df, spec, "cox", 100000L)
  expect_equal(a$coef$est, mem$coef$est, tolerance = 1e-6)
  expect_identical(a$warnings, mem$warnings)
  expect_identical(a$n_clusters, mem$n_clusters)
  # the flat likelihood moves est (and so the SE) at about 1e-7 between chunk sizes (.ssc_inf_terms)
  expect_equal(a$coef$se, b$coef$se, tolerance = 1e-6)
})

test_that("score residuals equal residuals.coxph score", {
  df <- .ssc_surv_frame(300L, 23L)
  df$time[1:2] <- 0.5                              # rows before the first event time of their stratum
  df$status[1:2] <- 0L
  m <- .h_st_timefix_map(df$time)
  expect_true(length(m$from) > 0L)                 # near ties that coxph merges
  for (ties in c("efron", "breslow")) {
    fit <- survival::coxph(survival::Surv(time, status) ~ x1 + yr + survival::strata(st1), data = df, ties = ties)
    beta <- stats::coef(fit)
    s <- as.integer(factor(df$st1))
    o <- order(s, -df$time, seq_len(nrow(df)))
    X <- as.matrix(df[, c("x1", "yr")])
    X <- sweep(X, 2L, colMeans(X))
    eta <- drop(X %*% beta)
    ev <- df$status
    mt <- .h_st_timefix_apply(df$time, m)
    # the event-time table from the streamed step at chunk 7
    st <- .h_st_cox_new(2L, ties)
    et <- NULL
    for (i in split(o, ceiling(seq_along(o) / 7L))) {
      st <- .h_st_et_step(st, mt[i], ev[i], X[i, , drop = FALSE], eta[i], s[i])
      et <- rbind(et, st$et)
    }
    et <- rbind(et, .h_st_et_close(st))
    expect_identical(names(et), c("stratum", "time", "dlam", "c1", "xbm_1", "xbm_2", "db_1", "db_2", "c2_1", "c2_2"))
    expect_identical(nrow(et), nrow(unique(data.frame(s, mt)[ev == 1, ])))
    # the cumulative view and the ASOF join, in R
    et <- et[order(et$stratum, et$time), ]
    cum <- function(v) ave(v, et$stratum, FUN = cumsum)
    tab <- data.frame(lam = cum(et$dlam), c1 = et$c1, xbm_1 = et$xbm_1, xbm_2 = et$xbm_2,
                      b_1 = cum(et$db_1), b_2 = cum(et$db_2), c2_1 = et$c2_1, c2_2 = et$c2_2)
    j <- vapply(seq_len(nrow(df)), function(k) {
      w <- which(et$stratum == s[k] & et$time <= df$time[k])
      if (length(w)) w[length(w)] else NA_integer_
    }, integer(1))
    expect_true(anyNA(j))                          # rows before the stratum's first event
    joined <- tab[j, , drop = FALSE]
    res <- .h_st_cox_resid(X, exp(eta), ev, joined)
    ref <- stats::residuals(fit, type = "score")
    expect_equal(unname(res), unname(ref), tolerance = 1e-10, info = ties)
    # the NA terms count as 0
    joined[is.na(joined)] <- 0
    expect_identical(.h_st_cox_resid(X, exp(eta), ev, joined), res)
  }
})

test_that("one cluster stops with the memory text", {
  df <- .ssc_surv_frame(120L, 24L)
  df$pid <- 5L
  spec <- .cl_spec()
  em <- .ssc_err(.ssc_memory(df, spec, "cox"))
  expect_identical(em, "layers$clustered needs at least two clusters (found 1)")
  for (ch in c(7L, 1000L)) expect_identical(.ssc_err(.ssc_stream(df, spec, "cox", ch)), em)
  # one cluster left after the complete-case filter
  d2 <- .ssc_surv_frame(120L, 25L)
  d2$pid[d2$pid != 3L] <- NA
  em <- .ssc_err(.ssc_memory(d2, spec, "cox"))
  expect_identical(.ssc_err(.ssc_stream(d2, spec, "cox", 7L)), em)
})

test_that("no file is written", {
  df <- .ssc_surv_frame(300L, 26L)
  spec <- .cl_spec(strata = "st2", ties = "breslow")
  td <- tempfile("sr-coxcl-tmp")
  dir.create(td)
  withr::defer(unlink(td, recursive = TRUE))
  before <- list.files(td, recursive = TRUE, all.files = TRUE)
  st <- .ssc_stream(df, spec, "cox", 7L, env_extra = list(sr_duck_temp = td))
  expect_identical(st$se_type, "cluster")
  expect_identical(list.files(td, recursive = TRUE, all.files = TRUE), before)
  body <- .ssc_body(spec, "cox", 1000L)
  expect_true(.sr_emit_check(body, "analysis", stream = TRUE))
  expect_false(grepl("[^\\x01-\\x7F]", body, perl = TRUE))
  expect_false(grepl("parquet", body, fixed = TRUE))
  expect_true(grepl("CREATE OR REPLACE TEMP TABLE sr_et AS SELECT * FROM sr_et_chunk", body, fixed = TRUE))
  expect_true(grepl("INSERT INTO sr_et SELECT * FROM sr_et_chunk", body, fixed = TRUE))
  expect_true(grepl("ASOF LEFT JOIN sr_et_cum", body, fixed = TRUE))
})

test_that("a clustered stream spec compiles (the Task 5 stop is gone)", {
  spec <- utils::modifyList(.cl_spec(), list(engine = "stream", stream_chunk = 100))
  body <- expect_silent(.sr_cox_compile_stream(spec))
  expect_false(grepl("not available yet", body, fixed = TRUE))
  expect_true(grepl("se_type = \"cluster\"", body, fixed = TRUE))
  expect_true(grepl("layers$clustered needs at least two clusters (found ", body, fixed = TRUE))
  # without the cluster layer the body has no event-time table
  b0 <- .sr_cox_compile_stream(utils::modifyList(.ssc_surv_spec("cox"), list(engine = "stream", stream_chunk = 100)))
  expect_false(grepl("sr_et", b0, fixed = TRUE))
})

test_that("a character cluster column gives the memory robust SE on the stream", {
  df <- .ssc_surv_frame(300L, 27L)
  df$pid <- sprintf("p%03d", as.integer(df$pid))
  spec <- .cl_spec()
  mem <- .ssc_memory(df, spec, "cox")
  expect_identical(mem$se_type, "cluster")
  for (ch in c(7L, 100000L)) {
    st <- .ssc_stream(df, spec, "cox", ch)
    .ssc_surv_expect(st, mem, as.character(ch))
    expect_identical(st$n_clusters, mem$n_clusters, info = as.character(ch))
  }
})
