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

# ---- the computation: SEs, design effect, MDE, blockers, tiers ----------------------------------

# Hand-made inputs of the documented .sr_feas_inputs() structure. `lv_n` are the complete-case counts
# per exposure level, the reference first (binary / factor); a linear exposure has no levels.
.fe_hand <- function(kind = "binary", type = "binary", n_cc = 400L, variance = 0.25, r2x = 0.2,
                     risk = 0.3, sd = 2, events_cc = 120, n_obs = n_cc, params = 3L,
                     lv_n = c(`0` = 200L, `1` = 200L), ps_outside = 0, separated = FALSE,
                     clusters = NULL, max_time = 20, pairs = NULL, blockers = character()) {
  lv <- if (type == "linear") character() else names(lv_n)
  levels_df <- data.frame(level = lv, n_all = as.integer(lv_n[lv]), n_cc = as.integer(lv_n[lv]),
                          stringsAsFactors = FALSE)
  ci <- if (type == "linear") {
    data.frame(level = NA_character_, n_k = as.integer(n_cc), n_ref = NA_integer_, r2x = r2x,
               ps_outside = NA_real_, separated = separated, stringsAsFactors = FALSE)
  } else {
    k <- lv[-1L]
    data.frame(level = k, n_k = as.integer(lv_n[k]), n_ref = rep(as.integer(lv_n[1L]), length(k)),
               r2x = rep(r2x, length(k)), ps_outside = rep(ps_outside, length(k)),
               separated = rep(separated, length(k)), stringsAsFactors = FALSE)
  }
  outcome <- list(sd = if (kind == "continuous") sd else NA_real_, risk = if (kind == "binary") risk else NA_real_,
                  events_total = if (kind == "continuous") NA_real_ else events_cc * n_obs / n_cc,
                  events_cc = if (kind == "continuous") NA_real_ else events_cc,
                  followup_total = NA_real_, max_time = if (kind == "surv") max_time else NA_real_,
                  pairs = pairs)
  list(kind = kind, n_rows = as.integer(n_cc), n_cc = as.integer(n_cc), n_obs = as.integer(n_obs),
       outcome = outcome,
       exposure = list(var = "x", type = type, ref = if (type == "linear") NA_character_ else lv[1L],
                       levels = levels_df, variance = if (type == "factor") NA_real_ else variance),
       contrasts_in = ci, clusters = clusters, params = as.integer(params), blockers = blockers)
}

.fe_row <- function(res, measure, level = NULL) {
  r <- res$contrasts[res$contrasts$measure == measure, , drop = FALSE]
  if (!is.null(level)) r <- r[r$level == level, , drop = FALSE]
  stopifnot(nrow(r) == 1L)
  r
}

test_that(".sr_feas_measures follows the route and the marginal block", {
  sp <- .fe_spec()
  expect_identical(.sr_feas_measures("lm", sp, "binary"), "MD")
  expect_identical(.sr_feas_measures("lm_describe", sp, "linear"), "MD")
  expect_identical(.sr_feas_measures("logistic", sp, "binary"), c("OR", "RD", "RR"))
  expect_identical(.sr_feas_measures("logistic", sp, "factor"), c("OR", "RD", "RR"))
  expect_identical(.sr_feas_measures("logistic", sp, "linear"), "OR")
  expect_identical(.sr_feas_measures("logistic", sp, NULL), "OR")
  expect_identical(.sr_feas_measures("logistic", .fe_spec(marginal = FALSE), "binary"), "OR")
  expect_identical(.sr_feas_measures("logistic", .fe_spec(penalty = "firth"), "binary"), "OR")
  expect_identical(.sr_feas_measures("modpois", sp, "binary"), c("RR", "RD"))
  expect_identical(.sr_feas_measures("modpois", sp, "linear"), "RR")
  expect_identical(.sr_feas_measures("cox", sp, "binary"), "HR")
  expect_identical(.sr_feas_measures("cox_describe", sp, "linear"), "HR")
  expect_identical(.sr_feas_measures("rmst", sp, "factor"), "RMST_diff")
  expect_identical(.sr_feas_measures("table1", sp, "binary"), character())
})

test_that(".sr_feas_check_args refuses every malformed delta and assumption", {
  ck <- function(delta = NULL, assumptions = list(), kind = "surv", measures = c("HR"), clustered = FALSE) {
    .sr_feas_check_args(delta, assumptions, kind, measures, clustered)
  }
  d <- function(...) utils::modifyList(list(value = 0.8, measure = "HR", source = "the trial protocol"), list(...))
  j <- function(value) list(value = value, justification = "a published estimate")
  expect_true(ck())
  expect_true(ck(d()))
  expect_error(ck(d(measure = "OR")), "measure")                    # not a measure of this analysis
  expect_error(ck(d(measure = "ZZ")), "measure")
  expect_error(ck(d(value = 1)), "ratio")
  expect_error(ck(d(value = 0)), "ratio")
  expect_error(ck(d(value = -0.5)), "ratio")
  expect_error(ck(d(source = NULL)), "delta must be")
  expect_error(ck(d(source = "")), "source")
  expect_error(ck(d(source = "  ")), "source")
  expect_error(ck(d(source = NA_character_)), "source")
  expect_error(ck(d(value = "a")), "value")
  expect_error(ck(d(value = NA_real_)), "value")
  expect_error(ck(d(extra = 1)), "delta must be")
  expect_error(ck("HR 0.8"), "delta must be")
  expect_true(ck(d(measure = "MD", value = 5), measures = "MD", kind = "continuous"))
  expect_error(ck(d(measure = "MD", value = 0), measures = "MD", kind = "continuous"), "difference")
  expect_error(ck(d(measure = "MD", value = -2), measures = "MD", kind = "continuous"), "difference")
  expect_true(ck(d(measure = "RD", value = 0.02), measures = c("OR", "RD", "RR"), kind = "binary"))
  # assumptions
  expect_error(ck(assumptions = list(r2_y = j(0.3)), kind = "binary"), "r2_y applies to a continuous outcome only")
  expect_error(ck(assumptions = list(r2_y = j(0.3))), "r2_y applies to a continuous outcome only")
  expect_true(ck(assumptions = list(r2_y = j(0.3)), kind = "continuous"))
  expect_error(ck(assumptions = list(r2_y = j(1)), kind = "continuous"), "r2_y")
  expect_error(ck(assumptions = list(r2_y = j(-0.1)), kind = "continuous"), "r2_y")
  expect_error(ck(assumptions = list(r2_y = list(value = 0.3, justification = "")), kind = "continuous"),
               "justification")
  expect_error(ck(assumptions = list(r2_y = list(value = 0.3)), kind = "continuous"), "r2_y")
  expect_error(ck(assumptions = list(power = j(0.8))), "r2_y and icc")
  expect_error(ck(assumptions = list(j(0.3))), "r2_y and icc")
  expect_error(ck(clustered = TRUE), "a clustered analysis needs assumptions\\$icc \\(value and justification\\)")
  expect_error(ck(assumptions = list(r2_y = j(0.1)), clustered = TRUE, kind = "continuous"),
               "a clustered analysis needs assumptions\\$icc")
  expect_error(ck(assumptions = list(icc = j(0.05))), "icc applies to a clustered analysis only")
  expect_true(ck(assumptions = list(icc = j(0.05)), clustered = TRUE))
  expect_error(ck(assumptions = list(icc = j(1)), clustered = TRUE), "icc")
  expect_error(ck(assumptions = list(icc = j(-0.01)), clustered = TRUE), "icc")
  expect_error(ck(assumptions = list(icc = list(value = 0.05, justification = NA_character_)), clustered = TRUE),
               "justification")
})

test_that("each standard error matches its hand-computed formula (binary exposure)", {
  sp <- .fe_spec()
  v <- 400 * 0.25 * 0.8
  res <- .sr_feas_compute(.fe_hand(), sp, sr_route_get("logistic"))
  expect_named(res, c("contrasts", "blockers", "warnings", "tier", "advice"))
  expect_identical(names(res$contrasts), c("contrast", "level", "measure", "scale", "se", "h", "mde",
                                           "se_upper", "n_k", "n_ref", "r2x", "deff", "tier"))
  expect_identical(res$contrasts$measure, c("OR", "RD", "RR"))
  expect_equal(.fe_row(res, "OR")$se, 1 / sqrt(400 * 0.25 * 0.8 * 0.3 * 0.7), tolerance = 1e-12)
  expect_equal(.fe_row(res, "RD")$se, sqrt(0.3 * 0.7 / v), tolerance = 1e-12)
  expect_equal(.fe_row(res, "RR")$se, sqrt(0.7 / (0.3 * v)), tolerance = 1e-12)
  or <- .fe_row(res, "OR")
  expect_equal(or$h, exp(1.96 * or$se), tolerance = 1e-12)
  expect_equal(or$mde, exp(2.80 * or$se), tolerance = 1e-12)
  expect_identical(or$scale, "log")
  rd <- .fe_row(res, "RD")
  expect_equal(rd$h, 1.96 * rd$se, tolerance = 1e-12)
  expect_equal(rd$mde, 2.80 * rd$se, tolerance = 1e-12)
  expect_identical(rd$scale, "identity")
  expect_identical(c(or$n_k, or$n_ref), c(200L, 200L))
  expect_equal(c(or$r2x, or$deff), c(0.2, 1))
  expect_true(is.na(or$se_upper))
  expect_identical(or$level, "1")

  lm_res <- .sr_feas_compute(.fe_hand(kind = "continuous"), sp, sr_route_get("lm"))
  expect_equal(.fe_row(lm_res, "MD")$se, 2 / sqrt(v), tolerance = 1e-12)
  expect_equal(.fe_row(lm_res, "MD")$h, 1.96 * 2 / sqrt(v), tolerance = 1e-12)
  as <- list(r2_y = list(value = 0.5, justification = "published R2 of the covariates"))
  lm_r2 <- .sr_feas_compute(.fe_hand(kind = "continuous"), sp, sr_route_get("lm"), assumptions = as)
  expect_equal(.fe_row(lm_r2, "MD")$se, 2 * sqrt(0.5) / sqrt(v), tolerance = 1e-12)

  hr <- .sr_feas_compute(.fe_hand(kind = "surv"), sp, sr_route_get("cox"))
  expect_equal(.fe_row(hr, "HR")$se, 1 / sqrt(120 * 0.25 * 0.8), tolerance = 1e-12)
  expect_equal(.fe_row(hr, "HR")$mde, exp(2.80 / sqrt(120 * 0.25 * 0.8)), tolerance = 1e-12)

  mp <- .sr_feas_compute(.fe_hand(), sp, sr_route_get("modpois"))
  expect_identical(mp$contrasts$measure, c("RR", "RD"))
  expect_equal(.fe_row(mp, "RR")$se, sqrt(0.7 / (0.3 * v)), tolerance = 1e-12)
})

test_that("a linear exposure uses n_cc * Var(x) * (1 - r2x) and a factor contrast its two group sizes", {
  sp <- .fe_spec()
  lin <- .sr_feas_compute(.fe_hand(kind = "binary", type = "linear", variance = 1.7), sp, sr_route_get("logistic"))
  expect_identical(lin$contrasts$measure, "OR")
  expect_equal(lin$contrasts$se, 1 / sqrt(400 * 1.7 * 0.8 * 0.3 * 0.7), tolerance = 1e-12)
  expect_true(is.na(lin$contrasts$level) && is.na(lin$contrasts$n_ref))
  expect_match(lin$contrasts$contrast, "x", fixed = TRUE)
  linhr <- .sr_feas_compute(.fe_hand(kind = "surv", type = "linear", variance = 1.7), sp, sr_route_get("cox"))
  expect_equal(linhr$contrasts$se, 1 / sqrt(120 * 1.7 * 0.8), tolerance = 1e-12)

  lv_n <- c(a = 100L, b = 150L, c = 150L)
  fh <- .fe_hand(kind = "binary", type = "factor", lv_n = lv_n, n_cc = 400L)
  fac <- .sr_feas_compute(fh, sp, sr_route_get("logistic"))
  expect_identical(fac$contrasts$level[fac$contrasts$measure == "OR"], c("b", "c"))
  vb <- 0.8 / (1 / 150 + 1 / 100)
  expect_equal(.fe_row(fac, "OR", "b")$se, 1 / sqrt(vb * 0.21), tolerance = 1e-12)
  expect_equal(.fe_row(fac, "RD", "c")$se, sqrt(0.21 / vb), tolerance = 1e-12)
  expect_identical(.fe_row(fac, "OR", "b")$n_k, 150L)
  expect_identical(.fe_row(fac, "OR", "b")$n_ref, 100L)
  fhr <- .sr_feas_compute(.fe_hand(kind = "surv", type = "factor", lv_n = lv_n, n_cc = 400L), sp, sr_route_get("cox"))
  p <- 150 / 250
  expect_equal(.fe_row(fhr, "HR", "b")$se, 1 / sqrt(120 * 250 / 400 * p * (1 - p) * 0.8), tolerance = 1e-12)
})

test_that("clustering multiplies the SE by sqrt of the design effect", {
  sp <- .fe_spec(cluster = "site", layers = .fe_layers(clustered = TRUE))
  cl <- list(G = 20L, m_bar = 10, cv = 0.5)
  as <- list(icc = list(value = 0.05, justification = "similar trials report 0.05"))
  base <- .sr_feas_compute(.fe_hand(), .fe_spec(), sr_route_get("logistic"))
  res <- .sr_feas_compute(.fe_hand(clusters = cl), sp, sr_route_get("logistic"), assumptions = as)
  deff <- 1 + (1.25 * 10 - 1) * 0.05
  expect_identical(.fe_row(res, "OR")$deff, deff)
  expect_equal(.fe_row(res, "OR")$se, .fe_row(base, "OR")$se * sqrt(deff), tolerance = 1e-12)
  expect_equal(.fe_row(res, "RD")$se, .fe_row(base, "RD")$se * sqrt(deff), tolerance = 1e-12)
  expect_true(any(grepl("fewer than 30 clusters", res$warnings)))
  big <- .sr_feas_compute(.fe_hand(clusters = list(G = 40L, m_bar = 10, cv = 0.5)), sp, sr_route_get("logistic"),
                          assumptions = as)
  expect_false(any(grepl("cluster", big$warnings)))
  expect_error(.sr_feas_compute(.fe_hand(clusters = cl), sp, sr_route_get("logistic")), "assumptions\\$icc")
  expect_error(.sr_feas_compute(.fe_hand(), .fe_spec(), sr_route_get("logistic"), assumptions = as),
               "icc applies to a clustered analysis only")
})

test_that("multiple imputation: se_upper uses n_obs, the tier uses the conservative se", {
  sp <- .fe_spec(layers = .fe_layers(missing = "mi"))
  res <- .sr_feas_compute(.fe_hand(n_obs = 500L), sp, sr_route_get("logistic"))
  for (m in c("OR", "RD", "RR")) {
    r <- .fe_row(res, m)
    expect_lt(r$se_upper, r$se)
    expect_equal(r$se_upper, r$se * sqrt(400 / 500), tolerance = 1e-12)
  }
  hr <- .sr_feas_compute(.fe_hand(kind = "surv", n_obs = 500L), sp, sr_route_get("cox"))
  expect_equal(.fe_row(hr, "HR")$se_upper, 1 / sqrt(120 * 500 / 400 * 0.25 * 0.8), tolerance = 1e-12)
  fac <- .sr_feas_compute(.fe_hand(type = "factor", lv_n = c(a = 100L, b = 300L), n_obs = 500L), sp,
                          sr_route_get("logistic"))
  expect_equal(.fe_row(fac, "OR")$se_upper, .fe_row(fac, "OR")$se * sqrt(0.8), tolerance = 1e-12)
  d <- list(value = 1.5, measure = "OR", source = "a guideline")
  tt <- .sr_feas_compute(.fe_hand(n_obs = 500L), sp, sr_route_get("logistic"), delta = d)
  s <- .fe_row(tt, "OR")$se
  expect_identical(tt$tier, if (2.8 * s <= log(1.5)) "adequate" else if (1.96 * s > log(1.5)) "inadequate" else "limited")
  # a complete-case analysis has no upper end, even when n_obs > n_cc
  cc <- .sr_feas_compute(.fe_hand(n_obs = 500L), .fe_spec(), sr_route_get("logistic"))
  expect_true(all(is.na(cc$contrasts$se_upper)))
})

test_that("tiers follow MDE and CI half-width against delta (HR 0.80)", {
  sp <- .fe_spec()
  d <- list(value = 0.8, measure = "HR", source = "the trial protocol")
  dd <- abs(log(0.8))
  tier_for <- function(se, delta = d) {
    D <- 1 / (se^2 * 0.25 * 0.8)
    .sr_feas_compute(.fe_hand(kind = "surv", events_cc = D), sp, sr_route_get("cox"), delta = delta)
  }
  ad <- tier_for(0.9 * dd / 2.80)
  expect_equal(.fe_row(ad, "HR")$se, 0.9 * dd / 2.80, tolerance = 1e-12)
  expect_identical(ad$tier, "adequate")
  expect_identical(ad$contrasts$tier, "adequate")
  expect_identical(ad$advice, "proceed")
  lim <- tier_for(dd / 2.4)
  expect_true(1.96 * .fe_row(lim, "HR")$se < dd && dd < 2.80 * .fe_row(lim, "HR")$se)
  expect_identical(lim$tier, "limited")
  expect_identical(lim$advice, "proceed")
  inad <- tier_for(dd / 1.5)
  expect_true(1.96 * .fe_row(inad, "HR")$se > dd)
  expect_identical(inad$tier, "inadequate")
  expect_identical(inad$advice, c("revise", "stop", "exploratory"))
  # well inside each band (the boundaries themselves are float-fragile)
  expect_identical(tier_for(dd / 2.80 * 0.999)$tier, "adequate")
  expect_identical(tier_for(dd / 1.96 * 0.999)$tier, "limited")
  expect_identical(tier_for(dd / 1.96 * 1.001)$tier, "inadequate")
  po <- tier_for(0.05, delta = NULL)
  expect_identical(po$tier, "precision_only")
  expect_identical(po$contrasts$tier, "precision_only")
  expect_identical(po$advice, "proceed")
  # a difference measure compares with the absolute delta
  rd <- .sr_feas_compute(.fe_hand(), sp, sr_route_get("logistic"),
                         delta = list(value = 0.5, measure = "RD", source = "convention"))
  expect_identical(rd$tier, "adequate")
})

test_that("with a delta, rows of another measure are precision_only and the overall tier is the delta-measure worst", {
  sp <- .fe_spec()
  lv_n <- c(a = 100L, b = 250L, c = 50L)
  inp <- .fe_hand(type = "factor", lv_n = lv_n, n_cc = 400L)
  d <- list(value = 1.5, measure = "OR", source = "a guideline")
  res <- .sr_feas_compute(inp, sp, sr_route_get("logistic"), delta = d)
  cn <- res$contrasts
  expect_true(all(cn$tier[cn$measure != "OR"] == "precision_only"))
  expect_true(all(cn$tier[cn$measure == "OR"] %in% c("adequate", "limited", "inadequate")))
  rank <- c(adequate = 1, limited = 2, inadequate = 3)
  expect_identical(res$tier, names(rank)[max(rank[cn$tier[cn$measure == "OR"]])])
  # the two contrasts differ in precision, so a delta between their MDEs gives two different row tiers
  se_b <- .fe_row(res, "OR", "b")$se
  se_c <- .fe_row(res, "OR", "c")$se
  expect_gt(se_c, se_b)
  d2 <- list(value = exp(2.80 * se_b * 1.05), measure = "OR", source = "x")
  r2 <- .sr_feas_compute(inp, sp, sr_route_get("logistic"), delta = d2)
  expect_identical(.fe_row(r2, "OR", "b")$tier, "adequate")
  expect_false(.fe_row(r2, "OR", "c")$tier == "adequate")
  expect_identical(r2$tier, .fe_row(r2, "OR", "c")$tier)
})

test_that("a route with no effect measure is not_applicable, and proceeds", {
  sp <- .fe_spec()
  res <- .sr_feas_compute(.fe_hand(), sp, "table1")
  expect_identical(res$tier, "not_applicable")
  expect_identical(nrow(res$contrasts), 0L)
  expect_identical(res$advice, "proceed")
  expect_error(.sr_feas_compute(.fe_hand(), sp, "table1", delta = list(value = 2, measure = "OR", source = "x")),
               "measure")
  t1 <- list(purpose = "describe", by = "x", vars = c("z1", "z2"), layers = .fe_layers())
  inp <- .sr_feas_inputs(.fe_df(.fe_frame(), t1), t1, sr_route_get("table1"))
  r2 <- .sr_feas_compute(inp, t1, sr_route_get("table1"))
  expect_identical(r2$tier, "not_applicable")
  expect_identical(r2$blockers, character())
})

# a planted frame -> the verdict
.fe_verdict <- function(df, spec, route, ...) {
  .sr_feas_compute(.sr_feas_inputs(.fe_df(df, spec), spec, sr_route_get(route)), spec, sr_route_get(route), ...)
}

test_that("each blocker fires on a planted frame: tier cannot_run, advice revise / stop", {
  expect_blocked <- function(res, pattern) {
    expect_identical(res$tier, "cannot_run")
    expect_true(any(grepl(pattern, res$blockers)), info = paste(res$blockers, collapse = " | "))
    expect_true(all(res$contrasts$tier == "cannot_run"))
    expect_identical(res$advice, c("revise", "stop"))
  }
  # the healthy frame is not blocked
  ok <- .fe_verdict(.fe_frame(), .fe_spec(), "logistic")
  expect_identical(ok$blockers, character())
  expect_identical(ok$tier, "precision_only")
  # delta does not lift a blocker
  bl <- .fe_verdict(transform(.fe_frame(), x = 1L), .fe_spec(), "logistic",
                    delta = list(value = 2, measure = "OR", source = "x"))
  expect_identical(bl$tier, "cannot_run")

  df <- .fe_frame()
  df$x <- 1L                                                    # a single-level exposure
  expect_blocked(.fe_verdict(df, .fe_spec(), "logistic"), "fewer than 2 exposure levels")

  df <- .fe_frame()                                             # a factor level with only NA covariates
  df$z2[df$site == "c"] <- NA
  sp <- .fe_spec(exposure = "site", exposure_ref = "a", terms = list(list(var = "z1"), list(var = "z2")))
  r <- .fe_verdict(df, sp, "logistic")
  expect_blocked(r, "exposure level 'c' has no complete-case row")
  expect_identical(r$blockers[grepl("level 'c'", r$blockers)], "exposure level 'c' has no complete-case row")

  df <- .fe_frame()                                             # an all-"no" outcome
  df$y[!is.na(df$y)] <- "no"
  expect_blocked(.fe_verdict(df, .fe_spec(), "logistic"), "no events")
  df$y[!is.na(df$y)] <- "yes"                                   # and an all-"yes" one
  expect_blocked(.fe_verdict(df, .fe_spec(), "logistic"), "constant")

  df <- .fe_frame()                                             # a constant continuous outcome
  df$w <- 5
  spw <- .fe_spec(outcome = "w")
  spw$event_value <- NULL
  expect_blocked(.fe_verdict(df, spw, "lm"), "SD is 0")

  df <- .fe_frame()                                             # 8 terms with 5 events
  set <- withr::with_seed(9L, as.data.frame(matrix(stats::rnorm(500 * 6), 500, 6,
                                                   dimnames = list(NULL, paste0("q", 1:6)))))
  df <- cbind(df, set)
  df$y[!is.na(df$y)] <- "no"
  df$y[which(!is.na(df$y))[1:5]] <- "yes"
  tm <- lapply(c("z1", "z2", "z3", paste0("q", 1:5)), function(v) list(var = v, type = "linear"))
  r <- .fe_verdict(df, .fe_spec(terms = tm), "logistic")
  expect_blocked(r, "model parameters")
  # survival: more parameters than events
  sf <- .fe_surv_frame(60L)
  sf$status <- 0L
  sf$status[1:2] <- 1L
  sf$z2 <- withr::with_seed(2L, stats::rnorm(60))
  sf$z3 <- withr::with_seed(3L, stats::rnorm(60))
  tms <- lapply(c("z1", "z2", "z3"), function(v) list(var = v, type = "linear"))
  ssp <- .fe_surv_spec()
  ssp$terms <- tms
  r <- .fe_verdict(sf, ssp, "cox")
  expect_blocked(r, "model parameters")
  # a continuous outcome: parameters >= n_cc
  small <- .fe_frame(5L)
  small$w <- seq_len(5L) * 1.5
  small$z2 <- c(0.3, -1.2, 0.8, 0.1, -0.4)
  small$x <- c(0, 1, 0, 1, 1)
  spw <- .fe_spec(outcome = "w", terms = lapply(c("z1", "z2", "z3"), function(v) list(var = v, type = "linear")))
  spw$event_value <- NULL
  expect_blocked(.fe_verdict(small, spw, "lm"), "model parameters")

  df <- .fe_frame()                                             # x equal to z1 > 0 (positivity)
  df$x <- as.integer(df$z1 > 0)
  expect_blocked(.fe_verdict(df, .fe_spec(), "logistic"),
                 "the exposure is perfectly predicted by the covariates \\(positivity\\)")

  df <- .fe_frame()                                             # a continuous exposure that does not vary
  df$x <- 3.5
  r <- .fe_verdict(df, .fe_spec(), "logistic")
  expect_blocked(r, "the exposure does not vary")
  expect_false(any(grepl("fewer than 2", r$blockers)))

  spec <- list(purpose = "explain-causal", time = "time", status = "status", exposure = "x", tau = 500,
               layers = .fe_layers(), seed = 1L)               # rmst tau beyond the follow-up
  withr::local_options(sr.feas_rmst_sims = 5)
  expect_blocked(.fe_verdict(.fe_surv_frame(), spec, "rmst"), "tau")

  # the Task 1 coding and design blockers pass through
  spc <- .fe_spec()
  spc$event_value <- NULL
  expect_blocked(.fe_verdict(.fe_frame(), spc, "logistic"), "^outcome: ")
  expect_blocked(.fe_verdict(.fe_frame(), .fe_spec(exclude_rows = 900), "logistic"), "^design: exclude_rows")
})

test_that("warnings: near-positivity, events per parameter (Firth advice), few clusters", {
  sp <- .fe_spec()
  r <- .sr_feas_compute(.fe_hand(ps_outside = 0.2), sp, sr_route_get("logistic"))
  expect_true(any(grepl("near-positivity", r$warnings)))
  expect_false(any(grepl("near-positivity",
                         .sr_feas_compute(.fe_hand(ps_outside = 0.05), sp, sr_route_get("logistic"))$warnings)))
  low <- .fe_hand(kind = "binary", risk = 0.05, params = 4L, n_cc = 400L)
  low$outcome$events_cc <- 20                                   # 20 events / 4 parameters = 5 < 10
  r <- .sr_feas_compute(low, sp, sr_route_get("logistic"))
  expect_true(any(grepl("events per parameter", r$warnings)))
  expect_true(any(grepl("consider pre-specifying penalty = \"firth\"", r$warnings, fixed = TRUE)))
  rf <- .sr_feas_compute(low, .fe_spec(penalty = "firth"), sr_route_get("logistic"))
  expect_true(any(grepl("events per parameter", rf$warnings)))
  expect_false(any(grepl("firth", rf$warnings)))
  lowc <- .fe_hand(kind = "surv", params = 4L)
  lowc$outcome$events_cc <- 20
  rc <- .sr_feas_compute(lowc, sp, sr_route_get("cox"))
  expect_true(any(grepl("events per parameter", rc$warnings)))
  expect_false(any(grepl("firth", rc$warnings)))
  ok <- .sr_feas_compute(.fe_hand(), sp, sr_route_get("logistic"))
  expect_identical(ok$warnings, character())
})

test_that("RMST SE: deterministic, order-free, symmetric, RNG-neutral", {
  withr::local_options(sr.feas_rmst_sims = 40)
  sp <- list(purpose = "explain-causal", time = "time", status = "status", exposure = "x", tau = 8,
             layers = .fe_layers(), seed = 7L)
  inp <- .sr_feas_inputs(.fe_df(.fe_surv_frame(), sp), sp, sr_route_get("rmst"))
  pr <- inp$outcome$pairs
  se <- .sr_feas_rmst_se(pr, 150L, 200L, 8, 40L, 7L)
  expect_true(is.finite(se) && se > 0)
  expect_identical(.sr_feas_rmst_se(pr, 150L, 200L, 8, 40L, 7L), se)
  expect_false(identical(.sr_feas_rmst_se(pr, 150L, 200L, 8, 40L, 8L), se))
  perm <- pr[withr::with_seed(3L, sample(nrow(pr))), ]
  expect_identical(.sr_feas_rmst_se(perm, 150L, 200L, 8, 40L, 7L), se)
  expect_identical(.sr_feas_rmst_se(pr, 200L, 150L, 8, 40L, 7L), se)
  # larger groups are more precise
  expect_lt(.sr_feas_rmst_se(pr, 600L, 800L, 8, 40L, 7L), se)
  # the caller's RNG kind and .Random.seed are restored
  set.seed(11L)
  before <- get(".Random.seed", envir = globalenv())
  kind <- RNGkind()
  .sr_feas_rmst_se(pr, 100L, 100L, 8, 10L, 1L)
  expect_identical(get(".Random.seed", envir = globalenv()), before)
  expect_identical(RNGkind(), kind)
  # through the compute: the seed is spec$seed, else the route default, else 1
  res <- .sr_feas_compute(inp, sp, sr_route_get("rmst"))
  expect_identical(res$contrasts$measure, "RMST_diff")
  expect_identical(res$contrasts$scale, "identity")
  expect_identical(res$contrasts$se, .sr_feas_rmst_se(pr, inp$contrasts_in$n_k, inp$contrasts_in$n_ref, 8, 40L, 7L))
  no_seed <- sp
  no_seed$seed <- NULL
  r1 <- .sr_feas_compute(inp, no_seed, sr_route_get("rmst"))
  expect_identical(r1$contrasts$se, .sr_feas_rmst_se(pr, inp$contrasts_in$n_k, inp$contrasts_in$n_ref, 8, 40L, 1L))
  expect_equal(res$contrasts$h, 1.96 * res$contrasts$se, tolerance = 1e-12)
  expect_identical(res$tier, "precision_only")
})

test_that("compute on real inputs: precision only, blind, and byte-identical under outcome permutation", {
  withr::local_options(sr.feas_rmst_sims = 20)
  spec <- .fe_spec(terms = list(list(var = "z1", type = "rcs", knots = 4), list(var = "z2"),
                                list(var = "site", type = "factor")))
  df <- .fe_df(.fe_frame(), spec)
  route <- sr_route_get("logistic")
  one <- function(d) .sr_feas_compute(.sr_feas_inputs(d, spec, route), spec, route)
  res <- one(df)
  expect_identical(res$blockers, character())
  expect_identical(res$tier, "precision_only")
  expect_true(all(is.finite(res$contrasts$se)))
  expect_identical(.sr_feas_blind_frame(res, spec), res)
  ref <- sr_canonical_json(res)
  expect_identical(sr_canonical_json(one(.fe_permute(df, "y"))), ref)
  expect_identical(sr_canonical_json(one(.fe_shuffle(.fe_permute(df, "y")))), ref)

  sv <- .fe_surv_spec()
  sd0 <- .fe_df(.fe_surv_frame(), sv)
  cox <- sr_route_get("cox")
  one_c <- function(d) .sr_feas_compute(.sr_feas_inputs(d, sv, cox), sv, cox,
                                        delta = list(value = 0.7, measure = "HR", source = "a protocol"))
  rc <- one_c(sd0)
  expect_identical(rc$contrasts$measure, "HR")
  expect_true(rc$tier %in% c("adequate", "limited", "inadequate"))
  expect_identical(.sr_feas_blind_frame(rc, sv), rc)
  expect_identical(sr_canonical_json(one_c(.fe_shuffle(.fe_permute(sd0, c("time", "status"))))), sr_canonical_json(rc))

  rs <- list(purpose = "explain-causal", time = "time", status = "status", exposure = "x", tau = 8,
             layers = .fe_layers(), seed = 5L)
  rd0 <- .fe_df(.fe_surv_frame(), rs)
  rr <- sr_route_get("rmst")
  one_r <- function(d) .sr_feas_compute(.sr_feas_inputs(d, rs, rr), rs, rr)
  rres <- one_r(rd0)
  expect_identical(rres$blockers, character())
  expect_identical(sr_canonical_json(one_r(.fe_shuffle(.fe_permute(rd0, c("time", "status"))))), sr_canonical_json(rres))
  expect_identical(.sr_feas_blind_frame(rres, rs), rres)
})

test_that("the verdict's contrasts frame never trips the gate when spec columns share its column names", {
  bf <- .fe_frame()
  names(bf)[match(c("y", "z1"), names(bf))] <- c("scale", "level")
  spec <- .fe_spec(outcome = "scale", terms = list(list(var = "level")))
  res <- .sr_feas_compute(.sr_feas_inputs(.fe_df(bf, spec), spec, sr_route_get("logistic")), spec,
                          sr_route_get("logistic"))
  expect_identical(res$blockers, character())
  expect_identical(.sr_feas_blind_frame(res, spec), res)
  bad <- res
  bad$contrasts$hr <- 1
  expect_error(.sr_feas_blind_frame(bad, spec), "contrasts\\$hr")
})

test_that("an uncodable survival outcome gives cannot_run without simulating, and a non-whole seed falls back to 1", {
  withr::local_options(sr.feas_rmst_sims = 10)
  expect_true(is.na(.sr_feas_rmst_se(NULL, 100L, 100L, 8, 10L, 1L)))
  expect_true(is.na(.sr_feas_rmst_se(data.frame(t = 1:5), 100L, 100L, 8, 10L, 1L)))
  sf <- .fe_surv_frame()
  sf$status <- ifelse(sf$status == 1L, "dead", "alive")
  spec <- list(purpose = "explain-causal", time = "time", status = "status", exposure = "x", tau = 8,
               layers = .fe_layers(), seed = 1L)
  expect_no_error(r <- .fe_verdict(sf, spec, "rmst"))
  expect_identical(r$tier, "cannot_run")
  expect_true(any(grepl("^outcome: ", r$blockers)))
  expect_true(all(r$contrasts$tier == "cannot_run"))
  cx <- .fe_surv_spec()
  expect_no_error(r2 <- .fe_verdict(sf, cx, "cox"))
  expect_identical(r2$tier, "cannot_run")
  # a seed that is not a whole number is not used: the result is that of the default seed 1
  rs <- list(purpose = "explain-causal", time = "time", status = "status", exposure = "x", tau = 8,
             layers = .fe_layers(), seed = 1L)
  d <- .fe_df(.fe_surv_frame(), rs)
  inp <- .sr_feas_inputs(d, rs, sr_route_get("rmst"))
  base <- .sr_feas_compute(inp, rs, sr_route_get("rmst"))
  for (bad in list(1.5, NA_real_, Inf, "a", 3e10)) {
    rs$seed <- bad
    expect_no_error(r <- .sr_feas_compute(inp, rs, sr_route_get("rmst")))
    expect_identical(r$contrasts$se, base$contrasts$se)
  }
})
