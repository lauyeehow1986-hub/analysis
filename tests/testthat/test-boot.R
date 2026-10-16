# Tests for resample/boot.R: the unit and cluster bootstrap behind layers$resampling = "bootstrap"
# (percentile, BCa and bias-corrected normal intervals, the endpoint Monte Carlo error, the jackknife
# of the BCa acceleration), on every model route and combined with multiple imputation. Package data
# (survival::lung) and seeded synthetic data only; projects live under withr::local_tempdir(). The
# boot package is the reference implementation of the intervals and is used here only (never by the
# emitted code).

.bt_layers <- function(...) {
  utils::modifyList(list(missing = "cc", weights = "none", matching = FALSE, resampling = "bootstrap",
                         framework = "frequentist", clustered = FALSE), list(...))
}

# The routes, registered again when an earlier test file emptied the registry.
.bt_route <- function(id) {
  make <- list(lm = .sr_lm_route, lm_describe = .sr_lm_describe_route, logistic = .sr_logistic_route,
               logistic_describe = .sr_logistic_describe_route, modpois = .sr_modpois_route,
               cox = .sr_cox_route, cox_describe = .sr_cox_describe_route, rmst = .sr_rmst_route)
  for (k in names(make)) {
    if (!(.sr_route_key(k, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make[[k]]())
  }
  sr_route_get(id, 1L)
}

# Evaluate `expr` after the run preamble of `seed` (L'Ecuyer-CMRG, set.seed), restoring the caller's
# RNG kind and seed afterwards.
.bt_rng <- function(seed, expr) {
  withr::local_preserve_seed()
  old <- RNGkind()
  withr::defer(RNGkind(old[1L], old[2L], old[3L]))
  eval(parse(text = sr_rng_preamble(seed)))
  force(expr)
}

# Compile, check and run the emitted code on a data frame under the run preamble of the spec.
.bt_value <- function(df, spec, id) {
  route <- .bt_route(id)
  code <- sr_compile(route, spec)
  expect_silent(.sr_emit_check(code, "analysis"))
  expect_false(grepl("boot::", code, fixed = TRUE))
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  .bt_rng(spec$seed, eval(parse(text = code), envir = env))
}

.bt_present <- function(value, spec, id) {
  r <- .bt_route(id)
  sr_result_check(r$present(value, spec), r)
}

# A boot object for the mean of `x` that carries the replicates `t` (so boot.ci sees exactly the
# replicate matrix of .sr_h_boot).
.bt_bootobj <- function(x, t) {
  bo <- withr::with_seed(1, boot::boot(data.frame(x = x), function(d, i) mean(d$x[i]), R = 2L))
  bo$t <- matrix(t, ncol = 1L)
  bo$R <- length(t)
  bo
}

# id, cl (25 clusters of 3 to 8 rows), x (0/1), g (3 levels), z, y (continuous), yb (0/1)
.bt_df <- function(n_cl = 25L, seed = 3L) {
  withr::with_seed(seed, {
    cl <- rep(seq_len(n_cl), times = sample(3:8, n_cl, replace = TRUE))
    n <- length(cl)
    u <- stats::rnorm(n_cl)[cl]
    x <- stats::rbinom(n, 1L, 0.5)
    z <- stats::rnorm(n)
    g <- sample(c("a", "b", "c"), n, replace = TRUE)
    y <- 1 + 0.5 * x + 0.3 * z + u + stats::rnorm(n)
    yb <- stats::rbinom(n, 1L, stats::plogis(-0.3 + 0.8 * x + 0.4 * z + 0.3 * (g == "c")))
    data.frame(id = seq_len(n), cl = cl, x = x, g = g, z = z, y = y, yb = yb, stringsAsFactors = FALSE)
  })
}

.bt_lm_spec <- function(...) {
  s <- list(purpose = "explain-causal", outcome = "y", exposure = "x", terms = list(list(var = "z")),
            layers = .bt_layers(), boot = list(B = 60, type = "percentile"), seed = 7L)
  mod <- list(...)
  for (nm in names(mod)) s[nm] <- list(mod[[nm]])
  s
}

.bt_lung <- function() {
  d <- survival::lung
  data.frame(time = d$time, status = d$status, sex = d$sex, age = d$age, ph = d$ph.ecog)
}

# the primary rows of a presented result
.bt_prim <- function(res) res$effects[res$effects$role == "primary", , drop = FALSE]

# ---- the intervals ------------------------------------------------------------------------------

test_that("percentile, normal and BCa intervals equal boot::boot.ci on the same replicates", {
  skip_if_not_installed("boot")
  x <- withr::with_seed(42, stats::rexp(50))
  d <- data.frame(x = x)
  stat <- function(dd) c(m = mean(dd$x))
  tm <- .bt_rng(7, .sr_h_boot(d, stat, 2000L))
  expect_true(is.matrix(tm))
  expect_identical(dim(tm), c(2000L, 1L))
  expect_identical(colnames(tm), "m")
  t0 <- mean(x)
  jk <- .sr_h_boot_jack(d, stat)
  expect_identical(jk$units, 50L)
  expect_identical(jk$groups, 50L)
  expect_false(jk$grouped)
  bo <- .bt_bootobj(x, tm[, 1L])
  ref <- boot::boot.ci(bo, conf = 0.95, type = c("norm", "perc", "bca"), L = boot::empinf(bo, type = "jack"))
  got <- lapply(c(percentile = "percentile", normal = "normal", bca = "bca"), function(ty) {
    .sr_h_boot_ci(t0, tm[, 1L], ty, jack = jk$values[, 1L])
  })
  expect_equal(c(got$percentile$lo, got$percentile$hi), unname(ref$percent[1L, 4:5]), tolerance = 1e-8)
  expect_equal(c(got$normal$lo, got$normal$hi), unname(ref$normal[1L, 2:3]), tolerance = 1e-8)
  expect_equal(c(got$bca$lo, got$bca$hi), unname(ref$bca[1L, 4:5]), tolerance = 1e-8)
  for (ty in names(got)) {
    expect_identical(got[[ty]]$type_used, ty)
    expect_true(is.na(got[[ty]]$fallback_reason))
  }
})

test_that("BCa falls back to percentile, with the reason, for a constant statistic and for B = 20", {
  d <- data.frame(x = 1:30)
  stat <- function(dd) c(k = 1)
  tm <- .bt_rng(3, .sr_h_boot(d, stat, 200L))
  jk <- .sr_h_boot_jack(d, stat)
  ci <- .sr_h_boot_ci(1, tm[, 1L], "bca", jack = jk$values[, 1L])
  expect_identical(ci$type_used, "percentile")
  expect_match(ci$fallback_reason, "bias")
  expect_equal(c(ci$lo, ci$hi), c(1, 1))

  x <- withr::with_seed(5, stats::rexp(40)^3)
  d <- data.frame(x = x)
  stat <- function(dd) c(v = stats::var(dd$x))
  tm <- .bt_rng(9, .sr_h_boot(d, stat, 20L))
  jk <- .sr_h_boot_jack(d, stat)
  ci <- .sr_h_boot_ci(stats::var(x), tm[, 1L], "bca", jack = jk$values[, 1L])
  expect_identical(ci$type_used, "percentile")
  expect_true(is.character(ci$fallback_reason) && nzchar(ci$fallback_reason))
  pc <- .sr_h_boot_ci(stats::var(x), tm[, 1L], "percentile")
  expect_identical(c(ci$lo, ci$hi), c(pc$lo, pc$hi))
})

test_that("the endpoint Monte Carlo error is positive and shrinks like 1 / sqrt(B)", {
  x <- withr::with_seed(11, stats::rexp(60))
  d <- data.frame(x = x)
  stat <- function(dd) c(m = mean(dd$x))
  mc <- function(seed, B) {
    tm <- .bt_rng(seed, .sr_h_boot(d, stat, B))
    ci <- .sr_h_boot_ci(mean(x), tm[, 1L], "percentile")
    c(ci$mcse_lo, ci$mcse_hi)
  }
  m1 <- vapply(1:5, function(s) mc(s, 1000L), numeric(2))
  m4 <- vapply(1:5, function(s) mc(100L + s, 4000L), numeric(2))
  expect_true(all(is.finite(c(m1, m4))) && all(c(m1, m4) > 0))
  r <- mean(m4) / mean(m1)
  expect_gt(r, 0.3)
  expect_lt(r, 0.8)
})

# ---- the replicates -----------------------------------------------------------------------------

test_that("the replicate matrix depends only on the entry stream and the chunk size", {
  d <- data.frame(id = 1:40)
  stat <- function(dd) c(s = sum(dd$id * seq_along(dd$id)))
  a <- .bt_rng(5, .sr_h_boot(d, stat, 250L, chunk = 100L))
  b <- .bt_rng(5, .sr_h_boot(d, stat, 250L, chunk = 100L))
  expect_identical(a, b)
  expect_false(identical(a, .bt_rng(6, .sr_h_boot(d, stat, 250L, chunk = 100L))))
  # chunk j runs under the j-th nextRNGStream of the entry stream, and the caller's stream is untouched
  .bt_rng(5, {
    s0 <- get(".Random.seed", envir = globalenv())
    tm <- .sr_h_boot(d, stat, 250L, chunk = 100L)
    expect_identical(get(".Random.seed", envir = globalenv()), s0)
    s2 <- parallel::nextRNGStream(parallel::nextRNGStream(s0))
    genv <- globalenv()
    genv[[".Random.seed"]] <- s2
    idx <- sample.int(40L, 40L, replace = TRUE)
    expect_identical(unname(tm[101L, 1L]), as.numeric(sum(idx * seq_along(idx))))
  })
})

test_that("a failed or non-finite replicate is NA and too few finite replicates stop", {
  d <- data.frame(x = 1:20)
  stat <- function(dd) {
    if (dd$x[1L] > 15) stop("deliberate failure")
    c(m = if (dd$x[2L] > 18) NA_real_ else mean(dd$x))
  }
  tm <- .bt_rng(2, .sr_h_boot(d, stat, 100L))
  expect_true(anyNA(tm[, 1L]))
  expect_true(sum(is.finite(tm[, 1L])) >= 2L)
  expect_match(attr(tm, "first_error"), "deliberate")
  expect_error(.bt_rng(2, .sr_h_boot(d, function(dd) stop("never"), 30L)), "finite")
})

test_that("the cluster bootstrap resamples whole clusters and the jackknife leaves out clusters", {
  sizes <- c(2, 3, 4, 5, 6, 7, 2, 3, 4, 5, 6, 7)
  d <- data.frame(cl = rep(seq_along(sizes), times = sizes), v = 1)
  names(sizes) <- as.character(seq_along(sizes))
  stat <- function(dd) {
    tb <- table(dd$cl)
    k <- as.numeric(tb) / sizes[names(tb)]
    c(whole = as.numeric(all(k == round(k))), drawn = sum(k))
  }
  tm <- .bt_rng(4, .sr_h_boot(d, stat, 80L, cluster = d$cl))
  expect_true(all(tm[, "whole"] == 1))
  expect_true(all(tm[, "drawn"] == length(sizes)))
  jk <- .sr_h_boot_jack(d, function(dd) c(n = nrow(dd)), cluster = d$cl)
  expect_identical(jk$groups, 12L)
  expect_identical(jk$over, "clusters")
  expect_equal(sort(jk$values[, 1L]), sort(nrow(d) - unname(sizes)))
})

test_that("the jackknife of more than 1000 units leaves out 1000 seeded groups", {
  d <- data.frame(x = withr::with_seed(1, stats::rnorm(1500)))
  stat <- function(dd) c(m = mean(dd$x))
  a <- .bt_rng(2, .sr_h_boot_jack(d, stat))
  b <- .bt_rng(2, .sr_h_boot_jack(d, stat))
  expect_identical(a, b)
  expect_true(a$grouped)
  expect_identical(a$units, 1500L)
  expect_identical(a$groups, 1000L)
  expect_identical(nrow(a$values), 1000L)
})

# ---- the layer at compile -----------------------------------------------------------------------

test_that("the bootstrap settings are validated and the unsupported combinations are refused, naming them", {
  r <- .bt_route("lm")
  expect_silent(sr_compile(r, .bt_lm_spec()))
  expect_silent(sr_compile(r, .bt_lm_spec(boot = NULL)))
  expect_error(sr_compile(r, .bt_lm_spec(boot = list(B = 10))), "B")
  expect_error(sr_compile(r, .bt_lm_spec(boot = list(B = 100, type = "student"))), "type")
  cl <- .bt_lm_spec(layers = .bt_layers(clustered = TRUE), cluster = "cl")
  expect_error(sr_compile(r, cl), "boot_type")
  for (bt in c("pair", "replicate_weights")) {
    s <- cl
    s$boot_type <- bt
    expect_error(sr_compile(r, s), bt, fixed = TRUE)
  }
  s <- cl
  s$boot_type <- "cluster"
  expect_silent(sr_compile(r, s))
  s <- .bt_lm_spec(boot_type = "cluster")
  expect_error(sr_compile(r, s), "clustered")
  mi <- .bt_lm_spec(layers = .bt_layers(missing = "mi"), mi = list(m = 3))
  expect_error(sr_compile(r, mi), "mi_x_boot")
  mi$choices <- list(mi_x_boot = "boot_then_impute")
  expect_silent(sr_compile(r, mi))
  mi$boot <- list(B = 50, type = "bca")
  expect_error(sr_compile(r, mi), "percentile")
  mi$choices <- list(mi_x_boot = "impute_then_boot")
  expect_silent(sr_compile(r, mi))
  mi$choices <- list(mi_x_boot = "neither")
  expect_error(sr_compile(r, mi), "mi_x_boot")
  # the clustered RMST stays refused, for its own reason
  rs <- list(purpose = "explain-causal", time = "time", status = "status", exposure = "sex", tau = 500,
             cluster = "inst", boot_type = "cluster", layers = .bt_layers(clustered = TRUE), seed = 1L)
  expect_error(sr_compile(.bt_route("rmst"), rs), "clustered RMST")
})

test_that("the bootstrap code embeds the complete-case code of the route verbatim", {
  r <- .bt_route("lm")
  s <- .bt_lm_spec(marginal = TRUE)
  s0 <- s
  s0$layers$resampling <- "none"
  nb <- sr_compile(r, s0)
  bc <- sr_compile(r, s)
  setup <- sr_terms_code(s0)$setup
  expect_true(startsWith(nb, setup))
  expect_true(grepl(substring(nb, nchar(setup) + 2L), bc, fixed = TRUE))
  expect_true(startsWith(bc, setup))
})

# ---- the routes ---------------------------------------------------------------------------------

test_that("lm: cluster bootstrap with BCa on the coefficient and the marginal difference", {
  df <- .bt_df()
  s <- .bt_lm_spec(layers = .bt_layers(clustered = TRUE), cluster = "cl", boot_type = "cluster",
                   marginal = TRUE, boot = list(B = 150, type = "bca"))
  v <- .bt_value(df, s, "lm")
  expect_identical(v$boot$boot_type, "cluster")
  expect_identical(v$boot$jack$over, "clusters")
  expect_identical(v$boot$jack$groups, 25L)
  expect_identical(v$boot$n_units, 25L)
  expect_true(all(c("t1", "mrd1") %in% v$boot$ci$key))
  res <- .bt_present(v, s, "lm")
  pr <- .bt_prim(res)
  expect_identical(nrow(pr), 2L)
  expect_true(all(pr$ci_method %in% c("boot_bca", "boot_percentile")))
  expect_true(all(is.finite(pr$mcse)) && all(pr$mcse > 0))
  expect_true(all(is.na(pr$p)))
  ck <- v$boot$ci[v$boot$ci$key == "t1", ]
  expect_equal(pr$lo[1L], ck$lo)
  expect_equal(pr$hi[1L], ck$hi)
  expect_equal(pr$est[1L], unname(v$coef$est[v$coef$term == "t1"]))
  expect_equal(pr$mcse[1L], max(ck$mcse_lo, ck$mcse_hi))
  # the covariates keep their model-based intervals
  cv <- res$effects[res$effects$role == "covariate", , drop = FALSE]
  expect_true(all(cv$ci_method == "t_model"))
  expect_true(!is.null(res$fragments$boot_note))
})

test_that("logistic: bootstrap of the log OR and of the marginal RD / log RR, the NNT from the RD bounds", {
  df <- .bt_df()
  s <- list(purpose = "explain-causal", outcome = "yb", exposure = "g", exposure_ref = "a",
            terms = list(list(var = "z")), layers = .bt_layers(), boot = list(B = 60, type = "normal"),
            seed = 3L)
  v <- .bt_value(df, s, "logistic")
  res <- .bt_present(v, s, "logistic")
  pr <- .bt_prim(res)
  expect_identical(sort(unique(pr$measure)), c("OR", "RD", "RR"))
  expect_true(all(pr$ci_method == "boot_normal"))
  ci <- v$boot$ci
  orb <- pr[pr$measure == "OR" & pr$level == "b", ]
  ck <- ci[ci$key == "t1fb", ]
  expect_equal(c(orb$lo, orb$hi), exp(c(ck$lo, ck$hi)))
  expect_equal(orb$mcse, max(ck$mcse_lo, ck$mcse_hi))
  for (lv in c("b", "c")) {
    rd <- res$effects[res$effects$measure == "RD" & res$effects$level == lv, ]
    nn <- res$effects[res$effects$measure == "NNT" & res$effects$level == lv, ]
    ref <- .sr_h_nnt(rd$est, rd$lo, rd$hi)
    expect_equal(nn$est, ref$nnt)
    expect_identical(c(nn$lo, nn$hi), c(ref$lo, ref$hi))
  }
})

test_that("logistic with the Firth penalty and modified Poisson take the bootstrap", {
  df <- .bt_df()
  s <- list(purpose = "explain-causal", outcome = "yb", exposure = "x", terms = list(list(var = "z")),
            penalty = "firth", layers = .bt_layers(), boot = list(B = 30), seed = 4L)
  res <- .bt_present(.bt_value(df, s, "logistic"), s, "logistic")
  pr <- .bt_prim(res)
  expect_identical(pr$ci_method, "boot_percentile")
  s <- list(purpose = "explain-causal", outcome = "yb", exposure = "x", terms = list(list(var = "z")),
            layers = .bt_layers(), boot = list(B = 30), seed = 4L)
  res <- .bt_present(.bt_value(df, s, "modpois"), s, "modpois")
  pr <- .bt_prim(res)
  expect_true(all(pr$ci_method == "boot_percentile"))
  expect_true(all(c("RR", "RD") %in% pr$measure))
})

test_that("cox and rmst take the bootstrap", {
  d <- .bt_lung()
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
            terms = list(list(var = "age")), layers = .bt_layers(), boot = list(B = 40, type = "bca"), seed = 5L)
  v <- .bt_value(d, s, "cox")
  res <- .bt_present(v, s, "cox")
  pr <- .bt_prim(res)
  expect_identical(nrow(pr), 1L)
  expect_true(pr$ci_method %in% c("boot_bca", "boot_percentile"))
  ck <- v$boot$ci[v$boot$ci$key == "t1", ]
  expect_equal(c(pr$lo, pr$hi), exp(c(ck$lo, ck$hi)))

  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
            tau = 500, layers = .bt_layers(), boot = list(B = 40), seed = 6L)
  v <- .bt_value(d, s, "rmst")
  res <- .bt_present(v, s, "rmst")
  pr <- .bt_prim(res)
  expect_identical(nrow(pr), 1L)
  expect_identical(pr$ci_method, "boot_percentile")
  ck <- v$boot$ci
  expect_equal(c(pr$lo, pr$hi), c(ck$lo, ck$hi))
  expect_true(is.finite(pr$mcse) && pr$mcse > 0)
  expect_identical(res$tables$rmst_difference$P, "NA")
})

# ---- through sr_run -----------------------------------------------------------------------------

.bt_run_df <- function(n = 120L, seed = 8L) {
  withr::with_seed(seed, {
    age <- stats::rnorm(n, 50, 10)
    bmi <- stats::rnorm(n, 27, 4)
    y <- 10 + 0.1 * age + 0.4 * bmi + stats::rnorm(n, 0, 2)
    bmi[sample.int(n, round(0.15 * n))] <- NA
    data.frame(id = seq_len(n), age = round(age, 1), bmi = round(bmi, 1), y = round(y, 2))
  })
}

.bt_run <- function(spec, df = .bt_run_df(), env = parent.frame()) {
  p <- .fx_ready_project(df, "t", c(id = "id", y = "outcome"), env = env)
  .bt_route("lm")
  p <- sr_sap_add(p, "m1", "lm", spec, "t", label = "primary")
  p <- sr_sap_lock(p)
  p <- sr_run(p, "m1")
  run <- p$state$runs$m1
  expect_true(run$ok, info = run$error)
  list(value = readRDS(file.path(p$paths$fits, paste0(run$ok_key, ".rds")))$value, result = sr_result(p, "m1"))
}

test_that("lm with a BCa bootstrap through sr_run", {
  s <- list(purpose = "explain-causal", outcome = "y", exposure = "bmi", terms = list(list(var = "age")),
            layers = .bt_layers(), boot = list(B = 400, type = "bca"), seed = 12L)
  out <- .bt_run(s)
  expect_identical(out$value$boot$type, "bca")
  expect_identical(out$value$boot$B, 400L)
  pr <- .bt_prim(out$result)
  expect_identical(nrow(pr), 1L)
  expect_true(all(is.finite(c(pr$est, pr$lo, pr$hi, pr$mcse))))
  expect_true(pr$ci_method %in% c("boot_bca", "boot_percentile"))
})

test_that("multiple imputation with the bootstrap through sr_run, under each choice", {
  base <- list(purpose = "explain-causal", outcome = "y", exposure = "bmi", terms = list(list(var = "age")),
               layers = .bt_layers(missing = "mi"), mi = list(m = 3, maxit = 5), boot = list(B = 30),
               seed = 13L)
  s <- base
  s$choices <- list(mi_x_boot = "boot_then_impute")
  out <- .bt_run(s)
  expect_identical(out$value$boot$choice, "boot_then_impute")
  expect_identical(out$value$boot$m_inner, 3L)
  pr <- .bt_prim(out$result)
  expect_true(all(is.finite(c(pr$est, pr$lo, pr$hi))))
  expect_identical(pr$ci_method, "boot_percentile")
  ck <- out$value$boot$ci[out$value$boot$ci$key == "t1", ]
  expect_equal(pr$est, ck$mean)
  expect_true(!is.null(out$result$fragments$boot_then_impute))

  s <- base
  s$choices <- list(mi_x_boot = "impute_then_boot")
  out <- .bt_run(s)
  expect_identical(out$value$boot$choice, "impute_then_boot")
  pr <- .bt_prim(out$result)
  expect_true(all(is.finite(c(pr$est, pr$lo, pr$hi))))
  expect_identical(pr$ci_method, "rubin_bootvar")
  expect_true(!is.null(out$result$fragments$impute_then_boot))
})
