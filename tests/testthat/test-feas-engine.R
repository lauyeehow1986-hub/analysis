# Tests for sap/feas_engine.R: the outcome-blind inputs of the study feasibility verdict. Synthetic
# data only; nothing is written outside withr::local_tempdir().

.fe_layers <- function(clustered = FALSE, missing = "cc") {
  list(missing = missing, weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = clustered)
}

# n = 500: binary x with P(x) = plogis(z1), covariates z1, z2, a 3-level site, z3 for an RCS term, a
# binary outcome y coded "yes"/"no"; 10% NA in z2 and 5% NA in y.
.fe_frame <- function(n = 500L) {
  withr::with_seed(1L, {
    z1 <- stats::rnorm(n)
    z2 <- stats::rnorm(n)
    z3 <- stats::rnorm(n)
    x <- stats::rbinom(n, 1L, stats::plogis(z1))
    site <- sample(c("a", "b", "c"), n, replace = TRUE)
    y <- ifelse(stats::rbinom(n, 1L, stats::plogis(-0.5 + 0.7 * x + 0.3 * z1)) == 1L, "yes", "no")
    z2[sample(n, n / 10)] <- NA
    y[sample(n, n / 20)] <- NA
    data.frame(y = y, x = x, z1 = z1, z2 = z2, z3 = z3, site = site, stringsAsFactors = FALSE)
  })
}

.fe_spec <- function(terms = list(list(var = "z1", type = "linear"), list(var = "z2", type = "linear")), ...) {
  utils::modifyList(list(purpose = "explain-causal", outcome = "y", event_value = "yes", exposure = "x",
                         terms = terms, layers = .fe_layers(), seed = 1L), list(...))
}

# The frame restricted to the spec's columns, as .sr_extract_frame hands it over.
.fe_df <- function(df, spec) df[, sr_spec_cols(spec), drop = FALSE]

# The outcome column(s) permuted among the rows where it is observed (NA positions kept).
.fe_permute <- function(df, cols, seed = 2L) {
  i <- which(stats::complete.cases(df[, cols, drop = FALSE]))
  j <- withr::with_seed(seed, sample(length(i)))
  df[i, cols] <- df[i[j], cols, drop = FALSE]
  df
}

.fe_shuffle <- function(df, seed = 3L) {
  out <- df[withr::with_seed(seed, sample(nrow(df))), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# A survival frame: binary x on z1, time / status, 5% NA in time.
.fe_surv_frame <- function(n = 400L) {
  withr::with_seed(4L, {
    z1 <- stats::rnorm(n)
    x <- stats::rbinom(n, 1L, stats::plogis(z1))
    t_ev <- stats::rexp(n, 0.1 * exp(0.4 * x))
    t_c <- stats::runif(n, 0, 15)
    time <- round(pmin(t_ev, t_c), 3)
    status <- as.integer(t_ev <= t_c)
    time[sample(n, n / 20)] <- NA
    data.frame(time = time, status = status, x = x, z1 = z1, g = sample(sprintf("g%02d", 1:20), n, replace = TRUE),
               stringsAsFactors = FALSE)
  })
}

.fe_surv_spec <- function(...) {
  utils::modifyList(list(purpose = "explain-causal", time = "time", status = "status", exposure = "x",
                         terms = list(list(var = "z1", type = "linear")), layers = .fe_layers(),
                         seed = 1L), list(...))
}

# ---- route kinds --------------------------------------------------------------------------------

test_that(".sr_feas_kind maps route ids to an outcome kind, vectorised", {
  expect_identical(.sr_feas_kind(c("lm", "lm_describe", "logistic", "logistic_describe", "modpois",
                                   "cox", "cox_describe", "rmst", "table1", "stub_run")),
                   c("continuous", "continuous", "binary", "binary", "binary", "surv", "surv", "surv",
                     "none", "none"))
  expect_identical(.sr_feas_kind(character()), character())
})

# ---- the binary example of the plan -------------------------------------------------------------

test_that("inputs of a logistic analysis: n, marginals, r2x, params", {
  spec <- .fe_spec()
  df <- .fe_df(.fe_frame(), spec)
  inp <- .sr_feas_inputs(df, spec, sr_route_get("logistic"))
  expect_named(inp, c("kind", "n_rows", "n_cc", "n_obs", "outcome", "exposure", "contrasts_in",
                      "clusters", "params", "blockers"))
  expect_identical(inp$kind, "binary")
  expect_identical(inp$n_rows, 500L)
  expect_identical(inp$n_cc, sum(stats::complete.cases(df)))
  expect_identical(inp$n_obs, sum(!is.na(df$y)))
  obs <- df$y[!is.na(df$y)]
  expect_identical(inp$outcome$risk, mean(obs == "yes"))
  expect_equal(inp$outcome$events_total, sum(obs == "yes"))
  expect_equal(inp$outcome$events_cc, inp$outcome$events_total * inp$n_cc / inp$n_obs)
  expect_true(is.na(inp$outcome$sd) && is.na(inp$outcome$followup_total) && is.na(inp$outcome$max_time))
  expect_null(inp$outcome$pairs)

  cc <- df[stats::complete.cases(df), ]
  expect_identical(inp$exposure$var, "x")
  expect_identical(inp$exposure$type, "binary")
  expect_identical(inp$exposure$ref, "0")
  expect_identical(inp$exposure$levels$level, c("0", "1"))
  expect_identical(inp$exposure$levels$n_all, c(sum(df$x == 0), sum(df$x == 1)))
  expect_identical(inp$exposure$levels$n_cc, c(sum(cc$x == 0), sum(cc$x == 1)))
  expect_equal(inp$exposure$variance, mean((cc$x - mean(cc$x))^2), tolerance = 1e-12)

  ci <- inp$contrasts_in
  expect_identical(nrow(ci), 1L)
  expect_identical(ci$level, "1")
  expect_identical(ci$n_k, sum(cc$x == 1))
  expect_identical(ci$n_ref, sum(cc$x == 0))
  expect_equal(ci$r2x, summary(stats::lm(x ~ z1 + z2, cc))$r.squared, tolerance = 1e-10)
  ps <- stats::fitted(suppressWarnings(stats::glm(x ~ z1 + z2, stats::binomial(), cc)))
  expect_equal(ci$ps_outside, mean(ps < 0.01 | ps > 0.99), tolerance = 1e-12)
  expect_false(ci$separated)
  expect_identical(inp$params, 4L)
  expect_null(inp$clusters)
  expect_identical(inp$blockers, character())
})

test_that("inputs are byte-identical when the outcome is permuted or the rows are shuffled", {
  spec <- .fe_spec(terms = list(list(var = "z1", type = "rcs", knots = 4), list(var = "z2"),
                                list(var = "site", type = "factor")))
  df <- .fe_df(.fe_frame(), spec)
  route <- sr_route_get("logistic")
  ref <- sr_canonical_json(.sr_feas_inputs(df, spec, route))
  perm <- .fe_permute(df, "y")
  expect_false(identical(perm$y, df$y))
  expect_identical(is.na(perm$y), is.na(df$y))
  expect_identical(sr_canonical_json(.sr_feas_inputs(perm, spec, route)), ref)
  expect_identical(sr_canonical_json(.sr_feas_inputs(.fe_shuffle(df), spec, route)), ref)
  expect_identical(sr_canonical_json(.sr_feas_inputs(.fe_shuffle(perm), spec, route)), ref)
})

test_that("a factor term adds its levels - 1 and an RCS(4) term adds 3 to params", {
  base <- .fe_spec()
  df <- .fe_frame()
  route <- sr_route_get("logistic")
  sp <- .fe_spec(terms = c(base$terms, list(list(var = "site", type = "factor"))))
  expect_identical(.sr_feas_inputs(.fe_df(df, sp), sp, route)$params, 6L)
  sp <- .fe_spec(terms = c(base$terms, list(list(var = "z3", type = "rcs", knots = 4))))
  expect_identical(.sr_feas_inputs(.fe_df(df, sp), sp, route)$params, 7L)
})

test_that("a logical outcome with a text event_value codes as no events, without an error", {
  df <- .fe_frame()
  df$y <- df$y == "yes"
  spec <- .fe_spec(event_value = "Yes")
  expect_no_error(inp <- .sr_feas_inputs(.fe_df(df, spec), spec, sr_route_get("logistic")))
  expect_identical(inp$outcome$risk, 0)
  expect_equal(inp$outcome$events_total, 0)
})

test_that("an outcome the route cannot code becomes an outcome blocker", {
  spec <- .fe_spec()
  spec$event_value <- NULL
  inp <- .sr_feas_inputs(.fe_df(.fe_frame(), spec), spec, sr_route_get("logistic"))
  expect_true(any(startsWith(inp$blockers, "outcome: ")))
  expect_true(is.na(inp$outcome$risk))
  expect_identical(inp$n_obs, sum(!is.na(.fe_frame()$y)))
  expect_identical(inp$params, 4L)                       # the design does not need the outcome
})

test_that("a hostile term name reaches the in-process term code only as data", {
  hostile <- "x`); system('calc'); ("
  df <- .fe_frame()
  names(df)[names(df) == "z2"] <- hostile
  spec <- .fe_spec(terms = list(list(var = "z1", type = "linear"), list(var = hostile, type = "linear")))
  seen <- new.env(parent = emptyenv())
  seen$code <- character()
  real <- get(".sr_emit_check", envir = globalenv())
  assign(".sr_emit_check", function(code, kind = c("analysis", "diagnostic"), prologue = FALSE) {
    seen$code <- c(seen$code, code)
    real(code, kind, prologue)
  }, envir = globalenv())
  withr::defer(assign(".sr_emit_check", real, envir = globalenv()))
  inp <- .sr_feas_inputs(.fe_df(df, spec), spec, sr_route_get("logistic"))
  expect_identical(inp$blockers, character())
  expect_identical(inp$params, 4L)
  expect_true(is.finite(inp$contrasts_in$r2x) && is.finite(inp$contrasts_in$ps_outside))
  expect_true(is.finite(inp$outcome$risk) && is.finite(inp$outcome$events_cc))
  expect_gte(length(seen$code), 2L)
  expect_true(all(grepl(.sr_lit(hostile), seen$code, fixed = TRUE)))
})

test_that(".sr_feas_blind_frame passes on the inputs", {
  spec <- .fe_spec(terms = list(list(var = "z1"), list(var = "site", type = "factor")))
  inp <- .sr_feas_inputs(.fe_df(.fe_frame(), spec), spec, sr_route_get("logistic"))
  expect_identical(.sr_feas_blind_frame(inp, spec), inp)
  expect_error(.sr_feas_blind_frame(inp), "spec")            # the spec is required
  sv <- .fe_surv_spec()
  si <- .sr_feas_inputs(.fe_df(.fe_surv_frame(), sv), sv, sr_route_get("cox"))
  expect_identical(.sr_feas_blind_frame(si, sv), si)
  bad <- inp
  bad$outcome$or <- 1
  expect_error(.sr_feas_blind_frame(bad, spec), "outcome-blind gate")
})

test_that("the engine's own frames never trip the gate when spec columns share their column names", {
  # survival: the time column is `time` and a covariate is named `event` (the pairs columns)
  sf <- .fe_surv_frame()
  names(sf)[names(sf) == "z1"] <- "event"
  sv <- .fe_surv_spec()
  sv$terms <- list(list(var = "event", type = "linear"))
  si <- .sr_feas_inputs(.fe_df(sf, sv), sv, sr_route_get("cox"))
  expect_identical(.sr_feas_blind_frame(si, sv), si)
  # binary: the outcome is `level` and a covariate is `n_cc` (the levels / contrasts_in columns)
  bf <- .fe_frame()
  names(bf)[match(c("y", "z1"), names(bf))] <- c("level", "n_cc")
  spec <- .fe_spec(outcome = "level", terms = list(list(var = "n_cc")))
  inp <- .sr_feas_inputs(.fe_df(bf, spec), spec, sr_route_get("logistic"))
  expect_identical(.sr_feas_blind_frame(inp, spec), inp)
  # a real crossing anywhere else is still refused
  bad <- si
  bad$extra <- data.frame(time = 1, event = 1)
  expect_error(.sr_feas_blind_frame(bad, sv), "outcome-blind gate")
})

test_that("an outcome statistic named as a column of an engine frame is still refused", {
  spec <- .fe_spec()
  inp <- .sr_feas_inputs(.fe_df(.fe_frame(), spec), spec, sr_route_get("logistic"))
  for (nm in c("or", "effect")) {
    bad <- inp
    bad$contrasts_in[[nm]] <- 1
    expect_error(.sr_feas_blind_frame(bad, spec), paste0("contrasts_in\\$", nm))
  }
  bad <- inp
  bad$exposure$levels$estimate <- 0L
  expect_error(.sr_feas_blind_frame(bad, spec), "exposure\\$levels\\$estimate")
  sv <- .fe_surv_spec()
  si <- .sr_feas_inputs(.fe_df(.fe_surv_frame(), sv), sv, sr_route_get("cox"))
  si$outcome$pairs$hr <- 1
  expect_error(.sr_feas_blind_frame(si, sv), "outcome\\$pairs\\$hr")
})

# ---- exposures ----------------------------------------------------------------------------------

test_that("a factor exposure: levels from the excluded frame with the reference first, counts from the complete cases", {
  df <- .fe_frame()
  spec <- .fe_spec(exposure = "site", exposure_ref = "b", terms = list(list(var = "z1"), list(var = "z2")))
  d <- .fe_df(df, spec)
  inp <- .sr_feas_inputs(d, spec, sr_route_get("logistic"))
  cc <- d[stats::complete.cases(d), ]
  expect_identical(inp$exposure$type, "factor")
  expect_identical(inp$exposure$ref, "b")
  expect_identical(inp$exposure$levels$level, c("b", "a", "c"))
  expect_identical(inp$exposure$levels$n_all, as.integer(table(d$site)[c("b", "a", "c")]))
  expect_identical(inp$exposure$levels$n_cc, as.integer(table(cc$site)[c("b", "a", "c")]))
  expect_true(is.na(inp$exposure$variance))
  ci <- inp$contrasts_in
  expect_identical(ci$level, c("a", "c"))
  expect_identical(ci$n_ref, rep(sum(cc$site == "b"), 2L))
  sub <- cc[cc$site %in% c("a", "b"), ]
  sub$k <- as.integer(sub$site == "a")
  expect_equal(ci$r2x[1L], summary(stats::lm(k ~ z1 + z2, sub))$r.squared, tolerance = 1e-10)
  expect_identical(inp$params, 5L)
})

test_that("an excluded or missing reference and a level without complete cases never raise", {
  df <- .fe_frame()
  df$z2[df$site == "c"] <- NA                            # level c: rows, but no complete case
  spec <- .fe_spec(exposure = "site", exposure_ref = "a", terms = list(list(var = "z1"), list(var = "z2")))
  expect_no_error(inp <- .sr_feas_inputs(.fe_df(df, spec), spec, sr_route_get("logistic")))
  lv <- inp$exposure$levels
  expect_identical(lv$level, c("a", "b", "c"))
  expect_true(lv$n_all[3L] > 0L && lv$n_cc[3L] == 0L)
  ci <- inp$contrasts_in
  expect_identical(ci$n_k[ci$level == "c"], 0L)
  expect_true(is.na(ci$r2x[ci$level == "c"]) && is.na(ci$separated[ci$level == "c"]))
  expect_false(is.na(ci$r2x[ci$level == "b"]))

  spec$exposure_ref <- "c"                               # the reference has rows but no complete case
  expect_no_error(inp <- .sr_feas_inputs(.fe_df(df, spec), spec, sr_route_get("logistic")))
  expect_identical(inp$exposure$levels$level, c("c", "a", "b"))
  expect_true(any(grepl("^design: reference level 'c'", inp$blockers)))
  expect_identical(inp$contrasts_in$n_ref, c(0L, 0L))
  expect_true(all(is.na(inp$contrasts_in$separated)))

  spec$exposure_ref <- "zz"                              # the reference is not in the data at all
  expect_no_error(inp <- .sr_feas_inputs(.fe_df(df, spec), spec, sr_route_get("logistic")))
  expect_true(any(grepl("^design: reference level 'zz'", inp$blockers)))
  expect_identical(inp$exposure$levels$level, c("a", "b", "c"))
  expect_true(is.na(inp$params))
  expect_true(all(is.na(inp$contrasts_in$r2x)))

  df$site[df$site != "a"] <- "a"                         # one level only: the term code refuses
  spec$exposure_ref <- "a"
  expect_no_error(inp <- .sr_feas_inputs(.fe_df(df, spec), spec, sr_route_get("logistic")))
  expect_true(any(startsWith(inp$blockers, "design: ")))
  expect_identical(nrow(inp$contrasts_in), 0L)
})

test_that("a covariate that perfectly predicts the exposure is separation", {
  df <- .fe_frame()
  df$x <- as.integer(df$z1 > 0)
  spec <- .fe_spec()
  inp <- .sr_feas_inputs(.fe_df(df, spec), spec, sr_route_get("logistic"))
  expect_true(inp$contrasts_in$separated)
  expect_identical(inp$blockers, character())
})

test_that("a continuous exposure: linear type, no propensity, separation from r2x", {
  df <- .fe_frame()
  spec <- .fe_spec(exposure = "z3", terms = list(list(var = "z1"), list(var = "z2")))
  d <- .fe_df(df, spec)
  inp <- .sr_feas_inputs(d, spec, sr_route_get("logistic"))
  cc <- d[stats::complete.cases(d), ]
  expect_identical(inp$exposure$type, "linear")
  expect_identical(nrow(inp$exposure$levels), 0L)
  expect_equal(inp$exposure$variance, mean((cc$z3 - mean(cc$z3))^2), tolerance = 1e-12)
  ci <- inp$contrasts_in
  expect_true(is.na(ci$level) && is.na(ci$n_ref) && is.na(ci$ps_outside))
  expect_identical(ci$n_k, nrow(cc))
  expect_equal(ci$r2x, summary(stats::lm(z3 ~ z1 + z2, cc))$r.squared, tolerance = 1e-10)
  expect_false(ci$separated)
  d$z3 <- 2 * d$z1 + 1
  inp <- .sr_feas_inputs(d, spec, sr_route_get("logistic"))
  expect_true(inp$contrasts_in$separated)
})

test_that("no covariates gives r2x 0", {
  spec <- .fe_spec(terms = NULL)
  inp <- .sr_feas_inputs(.fe_df(.fe_frame(), spec), spec, sr_route_get("logistic"))
  expect_identical(inp$contrasts_in$r2x, 0)
  expect_identical(inp$params, 2L)
  expect_false(inp$contrasts_in$separated)
})

# ---- other kinds --------------------------------------------------------------------------------

test_that("a continuous outcome: the marginal SD over every observed outcome", {
  df <- .fe_frame()
  df$w <- df$z3 * 2 + 1
  df$w[1:30] <- NA
  spec <- .fe_spec(outcome = "w")
  spec$event_value <- NULL
  d <- .fe_df(df, spec)
  inp <- .sr_feas_inputs(d, spec, sr_route_get("lm"))
  expect_identical(inp$kind, "continuous")
  expect_equal(inp$outcome$sd, stats::sd(d$w, na.rm = TRUE), tolerance = 1e-12)
  expect_true(is.na(inp$outcome$risk) && is.na(inp$outcome$events_total))
  expect_identical(inp$n_obs, sum(!is.na(d$w)))
  expect_identical(inp$params, 4L)
  ref <- sr_canonical_json(inp)
  expect_identical(sr_canonical_json(.sr_feas_inputs(.fe_shuffle(.fe_permute(d, "w")), spec, sr_route_get("lm"))), ref)
  d$w <- as.character(d$w)
  inp <- .sr_feas_inputs(d, spec, sr_route_get("lm"))
  expect_true(any(startsWith(inp$blockers, "outcome: ")))
})

test_that("a survival outcome: sorted pairs, events, follow-up, no intercept, clusters", {
  spec <- .fe_surv_spec()
  d <- .fe_df(.fe_surv_frame(), spec)
  inp <- .sr_feas_inputs(d, spec, sr_route_get("cox"))
  ok <- !is.na(d$time) & !is.na(d$status)
  expect_identical(inp$kind, "surv")
  expect_identical(inp$n_obs, sum(ok))
  expect_equal(inp$outcome$events_total, sum(d$status[ok]))
  expect_equal(inp$outcome$events_cc, inp$outcome$events_total * inp$n_cc / inp$n_obs)
  expect_equal(inp$outcome$followup_total, sum(d$time[ok]), tolerance = 1e-12)
  expect_identical(inp$outcome$max_time, max(d$time[ok]))
  expect_true(is.na(inp$outcome$risk) && is.na(inp$outcome$sd))
  pr <- inp$outcome$pairs
  expect_identical(names(pr), c("time", "event"))
  expect_identical(nrow(pr), sum(ok))
  expect_false(is.unsorted(pr$time))
  expect_identical(inp$params, 2L)                       # x + z1, no intercept
  ref <- sr_canonical_json(inp)
  perm <- .fe_permute(d, c("time", "status"))
  expect_identical(sr_canonical_json(.sr_feas_inputs(perm, spec, sr_route_get("cox"))), ref)
  expect_identical(sr_canonical_json(.sr_feas_inputs(.fe_shuffle(perm), spec, sr_route_get("cox"))), ref)

  sp <- .fe_surv_spec(status_event = 1, cluster = "g", layers = .fe_layers(clustered = TRUE))
  d2 <- .fe_df(.fe_surv_frame(), sp)
  inp <- .sr_feas_inputs(d2, sp, sr_route_get("cox"))
  m <- as.numeric(table(d2$g[stats::complete.cases(d2)]))
  expect_identical(inp$clusters$G, length(m))
  expect_equal(inp$clusters$m_bar, mean(m), tolerance = 1e-12)
  expect_equal(inp$clusters$cv, sqrt(mean((m - mean(m))^2)) / mean(m), tolerance = 1e-12)
  expect_identical(inp$params, 2L)

  ss <- .fe_surv_spec(strata = "g")                      # strata adjust the exposure model, not params
  inp <- .sr_feas_inputs(.fe_df(.fe_surv_frame(), ss), ss, sr_route_get("cox"))
  expect_identical(inp$params, 2L)
  expect_true(is.finite(inp$contrasts_in$r2x))
})

test_that("rmst: the exposure as groups and params = the number of levels", {
  spec <- list(purpose = "explain-causal", time = "time", status = "status", exposure = "x", tau = 10,
               layers = .fe_layers(), seed = 1L)
  d <- .fe_df(.fe_surv_frame(), spec)
  inp <- .sr_feas_inputs(d, spec, sr_route_get("rmst"))
  expect_identical(inp$exposure$type, "factor")
  expect_identical(inp$exposure$levels$level, c("0", "1"))
  expect_identical(inp$params, 2L)
  expect_identical(inp$contrasts_in$r2x, 0)
  expect_identical(inp$blockers, character())
})

test_that("exclude_rows drops rows by position before anything else", {
  spec <- .fe_spec()
  df <- .fe_df(.fe_frame(), spec)
  sp <- .fe_spec(exclude_rows = c(1, 5, 9))
  inp <- .sr_feas_inputs(df, sp, sr_route_get("logistic"))
  expect_identical(sr_canonical_json(inp), sr_canonical_json(.sr_feas_inputs(df[-c(1, 5, 9), ], spec, sr_route_get("logistic"))))
  expect_identical(inp$n_rows, 497L)
  sp$exclude_rows <- 900
  expect_no_error(inp <- .sr_feas_inputs(df, sp, sr_route_get("logistic")))
  expect_true(any(grepl("^design: exclude_rows names row 900", inp$blockers)))
})

test_that("kind none (table1, unknown routes) gives only the counts and never errors", {
  df <- .fe_frame()
  t1 <- list(purpose = "describe", by = "x", vars = c("z1", "z2", "site"), layers = .fe_layers())
  inp <- .sr_feas_inputs(.fe_df(df, t1), t1, sr_route_get("table1"))
  d <- .fe_df(df, t1)
  expect_identical(inp$kind, "none")
  expect_identical(inp$n_rows, 500L)
  expect_identical(inp$n_cc, sum(stats::complete.cases(d)))
  expect_true(is.na(inp$n_obs))
  expect_null(inp$outcome)
  expect_null(inp$exposure)
  expect_true(is.na(inp$params))
  expect_identical(inp$blockers, character())
  odd <- list(outcome = "y", terms = list(list(var = "z1", type = "weird")), layers = list(clustered = "maybe"))
  expect_no_error(inp <- .sr_feas_inputs(.fe_df(df, odd), odd, "stub_route"))
  expect_identical(inp$n_obs, sum(!is.na(df$y)))
})

# ---- the frame the engine reads -----------------------------------------------------------------

test_that(".sr_extract_frame is the frame sr_extract serialises", {
  df <- .fe_frame(60L)
  df$id <- seq_len(nrow(df))
  p <- .fx_ready_project(df, "t", c(id = "id", y = "outcome", x = "exposure"))
  cols <- c("y", "x", "z1")
  fr <- .sr_extract_frame(p, "t", cols)
  ex <- sr_extract(p, "t", cols)
  f <- file.path(p$paths$dir, ex$rel)
  expect_identical(unserialize(readBin(f, "raw", n = file.size(f))), fr)
  expect_identical(names(fr), cols)
  expect_error(.sr_extract_frame(p, "t", c("y", "nope")), "nope")
})
