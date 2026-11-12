# P1e.2 Task 2: the streamed Kaplan-Meier counts, the near-tie map (aeqSurv) and the RMST port must
# reproduce the memory references .sr_h_km and .sr_h_rmst. Synthetic data only.

.sk_data <- function(n = 2000L, seed = 11L) {
  set.seed(seed)
  t <- sample(1:60, n, replace = TRUE) * 1.0
  near <- sample(n, 80L)
  t[near] <- t[near] + 1e-10
  chain <- sample(n, 30L)
  t[chain] <- t[chain] + c(2e-10, 4e-10)[(seq_along(chain) %% 2L) + 1L]
  e <- rbinom(n, 1L, 0.6)
  e[t == max(t)] <- 0L                       # censoring at the last time
  list(t = t, e = e, g2 = sample(c("a", "b"), n, replace = TRUE))
}

# stream the rows in chunks of `ch` and finish like the engine does
.sk_stream <- function(t, e, g, ch, levels) {
  map <- .h_st_timefix_map(sort(unique(t)))
  st <- NULL
  idx <- split(seq_along(t), ceiling(seq_along(t) / ch))
  for (i in idx) st <- .h_st_km_add(st, t[i], e[i], g[i])
  cn <- .h_st_km_counts(st, map)
  .h_st_km_finish(cn, levels, range(t))
}

.sk_same <- function(got, ref) {
  for (nm in c("km", "risk")) {
    expect_identical(names(got[[nm]]), names(ref[[nm]]))
    expect_identical(got[[nm]]$strata, ref[[nm]]$strata)
    expect_identical(vapply(got[[nm]], typeof, ""), vapply(ref[[nm]], typeof, ""))
    expect_equal(got[[nm]]$time, ref[[nm]]$time, tolerance = 1e-12)
    expect_equal(got[[nm]]$n.risk, ref[[nm]]$n.risk, tolerance = 0)
  }
  for (v in c("n.event", "n.censor")) expect_equal(got$km[[v]], ref$km[[v]], tolerance = 0)
  for (v in c("surv", "lower", "upper")) {
    expect_equal(got$km[[v]], ref$km[[v]], tolerance = 1e-12)
    expect_identical(is.na(got$km[[v]]), is.na(ref$km[[v]]))
  }
}

test_that("timefix map equals aeqSurv", {
  set.seed(2)
  t <- c(5, 5 + 1e-10, 9, 9 + 1e-10, 9 + 2e-10, 12, 12, 30, 7, 7)
  n <- length(t)
  map <- .h_st_timefix_map(sort(unique(t)))
  ref <- as.numeric(survival::aeqSurv(survival::Surv(t, rep(1, n)))[, 1])
  expect_identical(.h_st_timefix_apply(t, map), ref)
  d <- .sk_data()
  map <- .h_st_timefix_map(sort(unique(d$t)))
  ref <- as.numeric(survival::aeqSurv(survival::Surv(d$t, rep(1, length(d$t))))[, 1])
  expect_identical(.h_st_timefix_apply(d$t, map), ref)
  none <- .h_st_timefix_map(c(1, 2, 3.5, 10))
  expect_length(none$from, 0L)
  expect_length(none$to, 0L)
  expect_identical(.h_st_timefix_apply(c(1, 2), none), c(1, 2))
})

test_that("km finish equals .sr_h_km", {
  d <- .sk_data()
  n <- length(d$t)
  for (ch in c(7L, 1e5L)) {
    # one group
    got <- .sk_stream(d$t, d$e, rep("All", n), ch, "All")
    .sk_same(got, .sr_h_km(d$t, d$e))
    # two groups
    got <- .sk_stream(d$t, d$e, d$g2, ch, c("a", "b"))
    .sk_same(got, .sr_h_km(d$t, d$e, d$g2))
    # a factor with an absent level (dropped, as .sr_h_km droplevels)
    f <- factor(d$g2, levels = c("a", "z", "b"))
    got <- .sk_stream(d$t, d$e, as.character(f), ch, levels(f))
    .sk_same(got, .sr_h_km(d$t, d$e, f))
  }
})

test_that("km finish handles a group whose last time has d = n", {
  t <- c(1, 2, 3, 3, 5, 6, 6, 7, 8, 8)
  e <- c(1, 0, 1, 1, 1, 1, 1, 0, 1, 1)
  g <- c("a", "a", "a", "a", "b", "b", "b", "b", "b", "b")
  got <- .sk_stream(t, e, g, 3L, c("a", "b"))
  ref <- .sr_h_km(t, e, g)
  .sk_same(got, ref)
  expect_true(any(is.na(ref$km$lower)))   # the fixture really hits surv = 0
})

test_that("km collapses after 16 chunks without changing counts", {
  d <- .sk_data(500L, 5L)
  g <- d$g2
  run <- function(ch) {
    st <- NULL
    for (i in split(seq_along(d$t), ceiling(seq_along(d$t) / ch))) {
      st <- .h_st_km_add(st, d$t[i], d$e[i], g[i])
    }
    .h_st_km_counts(st)
  }
  a <- run(7L)
  b <- run(1e5L)
  expect_identical(a, b)
  expect_identical(names(a), c("g", "t", "d", "c"))
  expect_equal(sum(a$d), sum(d$e))
  expect_equal(sum(a$c), sum(1 - d$e))
})

test_that("rmst port equals survmean", {
  d <- .sk_data()
  map <- .h_st_timefix_map(sort(unique(d$t)))
  st <- NULL
  for (i in split(seq_along(d$t), ceiling(seq_along(d$t) / 100))) {
    st <- .h_st_km_add(st, d$t[i], d$e[i], rep("All", length(i)))
  }
  cn <- .h_st_km_counts(st, map)
  ev <- sort(unique(d$t[d$e == 1]))
  for (tau in c(30.5, ev[10], max(d$t), 45)) {
    got <- .h_st_rmst(cn, tau)
    ref <- .sr_h_rmst(d$t, d$e, tau)
    expect_equal(unname(got), unname(ref), tolerance = 1e-10)
    expect_identical(names(got), c("rmst", "se"))
  }
})

test_that("the survival stream helpers are self-contained and inline cleanly", {
  fns <- c(".h_st_timefix_map", ".h_st_timefix_apply", ".h_st_km_add", ".h_st_km_counts",
           ".h_st_km_finish", ".h_st_rmst")
  for (f in fns) {
    fn <- get(f)
    code <- .sr_inline(fn, f)
    expect_false(grepl("[^\x01-\x7F]", code, perl = TRUE))
    g <- codetools::findGlobals(fn, merge = FALSE)$functions
    own <- g[!vapply(g, function(x) exists(x, envir = baseenv()) || exists(x, envir = asNamespace("stats")), NA)]
    expect_identical(own, character(), info = f)
  }
})
