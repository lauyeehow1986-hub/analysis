# P1e.2 Task 9: the streamed piecewise-exponential Cox (cox_method = "pem", rulings S15, S16, S18 and S19)
# against the in-memory PEM (survSplit + glm, Task 8) on the same rows, and the offset of the streamed IRLS
# against stats::glm. Synthetic data only. The harness (.ssc_surv_frame, .ssc_surv_spec, .ssc_surv_expect,
# .ssc_surv_chunks, .ssc_memory, .ssc_stream) is in helper-scale.R.

.pe_chunks <- c(7L, 50L, 100000L)

.pe_spec <- function(...) .ssc_surv_spec("cox", cox_method = "pem", ...)

# Two streamed PEM values at different chunk sizes: the counts exactly, the coefficient table at 1e-10. The
# TSQR fold of the IRLS sums in chunk order, and with the many baseline columns of strata the table moves
# by up to a few 1e-12 relative between chunk sizes (measured 5e-12; the exact Cox holds 1e-12, not here).
.pe_chunks_eq <- function(a, b, info = "") {
  for (f in c("est", "se", "lo", "hi", "p")) {
    expect_equal(a$coef[[f]], b$coef[[f]], tolerance = 1e-10, info = paste(info, f))
  }
  for (f in c("aliased", "n", "n_total", "events", "warnings", "iter", "passes", "cuts", "k")) {
    expect_identical(a[[f]], b[[f]], info = paste(info, f))
  }
}

# Stream at every chunk size against memory, the chunk sizes against each other, and the PEM fields.
.pe_sweep <- function(df, spec, info = "", chunks = .pe_chunks) {
  mem <- .ssc_memory(df, spec, "cox")
  expect_identical(mem$method, "pem", info = info)
  out <- list()
  for (ch in chunks) {
    st <- .ssc_stream(df, spec, "cox", ch)
    .ssc_surv_expect(st, mem, paste(info, ch))
    for (f in c("method", "cuts", "k", "ties")) expect_identical(st[[f]], mem[[f]], info = paste(info, ch, f))
    expect_identical(st$chunk, as.integer(ch), info = info)
    out[[as.character(ch)]] <- st
  }
  if (length(out) > 1L) {
    for (k in seq_along(out)[-1L]) {
      .pe_chunks_eq(out[[k]], out[[1L]], paste(info, names(out)[k], "vs", names(out)[1L]))
    }
  }
  invisible(list(mem = mem, st = out))
}

test_that("stream pem equals memory pem: quantile and explicit", {
  df <- .ssc_surv_frame(400L, 31L)
  terms <- list(list(var = "x1"), list(var = "grp", type = "factor"), list(var = "yr"))
  r <- .pe_sweep(df, .pe_spec(terms = terms), "default quantile")
  expect_identical(r$mem$k, length(r$mem$cuts) - 1L)
  expect_true(r$st[[1L]]$iter >= 2L)
  .pe_sweep(df, .pe_spec(pem_cuts = list(rule = "quantile", k = 4)), "quantile 4")
  # an explicit cut at or above the longest time is dropped
  r <- .pe_sweep(df, .pe_spec(pem_cuts = list(rule = "explicit", at = c(2.5, 6, 11, max(df$time), 1e6))),
                 "explicit")
  expect_identical(r$mem$cuts, c(0, 2.5, 6, 11, Inf))
  # no usable cut: one interval, the intercept model
  r <- .pe_sweep(df, .pe_spec(pem_cuts = list(rule = "explicit", at = max(df$time) + 1)), "one interval")
  expect_identical(r$mem$k, 1L)
  # a quantile cut equal to the longest time leaves an empty last interval (S16)
  d2 <- df
  d2$time <- pmin(d2$time, 4)
  d2$status[d2$time == 4] <- 1L
  r <- .pe_sweep(d2, .pe_spec(pem_cuts = list(rule = "quantile", k = 3)), "empty last interval")
  expect_identical(r$mem$cuts[length(r$mem$cuts) - 1L], max(d2$time))
  # an interaction and an rcs term
  .pe_sweep(df, .pe_spec(terms = list(list(var = "x1"), list(var = "x2", type = "rcs")),
                         interactions = list(c("sex", "x1"))), "interaction rcs", chunks = c(7L, 100000L))
  # a factor level absent from the analysed rows: the column is dropped, as glm does
  d3 <- df
  d3$time[d3$grp == "C"] <- NA
  r <- .pe_sweep(d3, .pe_spec(exposure = "grp", exposure_ref = "A", terms = list(list(var = "x1"))),
                 "absent level", chunks = c(7L, 100000L))
  expect_false(any(grepl("C", r$mem$coef$term, fixed = TRUE)))
  # exclude_rows
  .pe_sweep(df, .pe_spec(exclude_rows = c(2, 50, 51, 399)), "exclude_rows", chunks = c(7L, 100000L))
})

test_that("stream pem with strata and cluster", {
  df <- .ssc_surv_frame(400L, 32L)
  .pe_sweep(df, .pe_spec(strata = "st1"), "one stratum")
  .pe_sweep(df, .pe_spec(strata = c("st1", "st2"), pem_cuts = list(rule = "explicit", at = c(3, 8))), "two strata")
  # a stratum whose longest time ends before the last cut has fewer baseline intervals
  d2 <- df
  d2$time[d2$st1 == "a"] <- pmin(d2$time[d2$st1 == "a"], 3)
  .pe_sweep(d2, .pe_spec(strata = "st1", pem_cuts = list(rule = "explicit", at = c(2, 5, 9))), "short stratum")
  spec <- .pe_spec(strata = "st1", cluster = "pid", layers = .ssc_layers(TRUE))
  r <- .pe_sweep(df, spec, "cluster")
  expect_identical(r$mem$se_type, "cluster")
  expect_identical(r$st[[1L]]$n_clusters, r$mem$n_clusters)
  mod <- .ssc_memory(df, .pe_spec(strata = "st1"), "cox")
  expect_false(isTRUE(all.equal(mod$coef$se, r$mem$coef$se, tolerance = 1e-4)))
  # passes: pass 0, the IRLS passes and the cluster pass
  st <- r$st[[1L]]
  expect_identical(st$passes, st$iter + 3L)
  # a missing cluster drops the row, as in memory; one cluster stops with the memory text
  d3 <- df
  d3$pid[seq(4L, 400L, by = 13L)] <- NA
  .pe_sweep(d3, spec, "missing pid", chunks = c(7L, 100000L))
  d4 <- df
  d4$pid <- 1L
  em <- .ssc_err(.ssc_memory(d4, spec, "cox"))
  expect_false(is.na(em))
  expect_identical(.ssc_err(.ssc_stream(d4, spec, "cox", 7L)), em)
})

test_that("stream pem time <= 0 stops with the memory text", {
  df <- .ssc_surv_frame(200L, 33L)
  df$time[c(3, 9, 150)] <- c(0, -1, 0)
  spec <- .pe_spec()
  em <- .ssc_err(.ssc_memory(df, spec, "cox"))
  expect_identical(em, "the piecewise-exponential model needs every time above zero (found 3 rows with time <= 0)")
  for (ch in c(7L, 100000L)) expect_identical(.ssc_err(.ssc_stream(df, spec, "cox", ch)), em, info = as.character(ch))
  # the other stops keep the memory order and text
  d0 <- .ssc_surv_frame(120L, 34L)
  cases <- list(no_rows = within(d0, time <- NA_real_), no_events = within(d0, status <- 0L),
                missing_col = d0[setdiff(names(d0), "grp")])
  for (nm in names(cases)) {
    em <- .ssc_err(.ssc_memory(cases[[nm]], spec, "cox"))
    expect_false(is.na(em), info = nm)
    expect_identical(.ssc_err(.ssc_stream(cases[[nm]], spec, "cox", 7L)), em, info = nm)
  }
})

test_that("streamed IRLS with an offset equals glm with offset", {
  set.seed(91)
  n <- 700L
  x1 <- stats::rnorm(n)
  x2 <- stats::rbinom(n, 1L, 0.4)
  pt <- stats::runif(n, 0.05, 3)
  y <- stats::rpois(n, pt * exp(-0.7 + 0.5 * x1 - 0.3 * x2))
  X <- cbind("(Intercept)" = 1, x1 = x1, x2 = x2)
  ref <- stats::glm(y ~ x1 + x2 + offset(log(pt)), family = stats::poisson())
  for (ch in c(7L, 100L, 100000L)) {
    passfn <- function(beta, wls) {
      for (i in split(seq_len(n), ceiling(seq_len(n) / ch))) wls(X[i, , drop = FALSE], y[i], log(pt[i]))
      invisible(NULL)
    }
    fit <- .h_st_irls(passfn, "poisson", 3L, colnames(X))
    expect_equal(unname(fit$beta), unname(stats::coef(ref)), tolerance = 1e-8, info = as.character(ch))
    expect_equal(unname(fit$B), unname(stats::vcov(ref)), tolerance = 1e-8, info = as.character(ch))
    expect_equal(fit$deviance, ref$deviance, tolerance = 1e-8, info = as.character(ch))
    expect_identical(fit$iter, ref$iter, info = as.character(ch))
    expect_true(fit$converged, info = as.character(ch))
    # the glm scores with the offset in eta are sandwich::estfun.glm
    eta <- drop(X %*% fit$beta) + log(pt)
    eta_prev <- if (is.null(fit$beta_prev)) NULL else drop(X %*% fit$beta_prev) + log(pt)
    sc <- .h_st_glm_scores(X, y, eta, eta_prev, "poisson")
    expect_equal(unname(sc), unname(sandwich::estfun(ref)), tolerance = 1e-8, info = as.character(ch))
  }
  # no offset: the call without the third argument is unchanged
  pf0 <- function(beta, wls) {
    for (i in split(seq_len(n), ceiling(seq_len(n) / 50L))) wls(X[i, , drop = FALSE], y[i])
    invisible(NULL)
  }
  r0 <- stats::glm(y ~ x1 + x2, family = stats::poisson())
  expect_equal(unname(.h_st_irls(pf0, "poisson", 3L, colnames(X))$beta), unname(stats::coef(r0)), tolerance = 1e-8)
})

test_that("stream pem value names, progress and the emit check", {
  df <- .ssc_surv_frame(300L, 35L)
  spec <- .pe_spec()
  mem <- .ssc_memory(df, spec, "cox")
  pf <- tempfile(fileext = ".json")
  st <- .ssc_stream(df, spec, "cox", 50L, env_extra = list(sr_progress_file = pf))
  expect_identical(names(st), c("coef", "aliased", "n", "n_total", "events", "level", "se_type", "n_clusters",
                                "ties", "strata", "levels", "warnings", "km_by", "km", "km_risk", "method",
                                "cuts", "k", "engine", "chunk", "passes", "iter"))
  expect_identical(names(mem), names(st)[1:18])
  expect_identical(st$ties, NA_character_)
  expect_identical(st$passes, st$iter + 2L)       # pass 0, the start pass and one per iteration
  expect_true(st$passes <= 27L)
  got <- jsonlite::fromJSON(readLines(pf, warn = FALSE))
  expect_identical(as.integer(got$pass), st$passes)
  expect_identical(as.integer(got$max), st$passes)
  unlink(pf)
  for (s in list(spec, .pe_spec(strata = c("st1", "st2"), cluster = "pid", layers = .ssc_layers(TRUE),
                                pem_cuts = list(rule = "explicit", at = c(2, 5))))) {
    body <- .ssc_body(s, "cox", 1000L)
    expect_length(body, 1L)
    expect_true(.sr_emit_check(body, "analysis", stream = TRUE))
    expect_false(grepl("[^\\x01-\\x7F]", body, perl = TRUE))
    expect_false(grepl("survSplit", body, fixed = TRUE))
    expect_true(grepl("the piecewise-exponential model needs every time above zero", body, fixed = TRUE))
  }
})

test_that("a perfectly separating exposure stops the streamed PEM with the Poisson likelihood wording", {
  set.seed(3)
  d <- data.frame(time = stats::rexp(200, 1) + 0.1, sex = rep(0:1, each = 100))
  d$status <- ifelse(d$sex == 1, 1, 2)   # exposure 1 never has an event
  spec <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
               cox_method = "pem", layers = .ssc_layers(), seed = 7L, pem_cuts = list(rule = "explicit", at = c(0.5, 1)))
  # the stream value is a finite fit; the route's present() stops, as in memory
  v <- .ssc_stream(d, spec, "cox", 50L)
  e <- tryCatch({ sr_route_get("cox", 1L)$present(v, spec); NA_character_ },
                error = function(err) conditionMessage(err))
  expect_match(e, "Poisson likelihood", fixed = TRUE)
})
