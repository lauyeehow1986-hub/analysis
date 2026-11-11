# Streamed g-computation marginal effects and observed risk (P1e.1 Task 8, spec section 6.6, rulings R8 and
# R9). Synthetic data only.
#
# The reference is the in-memory route itself: the compiled logistic / modpois body evaluated on the whole
# table. The stream side replays what the stream body does per chunk (ruling R8): the route's own
# .h_design and outcome snippets on each chunk, the complete-case filter of the chunk, TSQR-IRLS for the
# fit, then one marginal pass (and the observed-risk counts on the fitted rows). Every streaming helper is
# evaluated from its inlined text in an environment whose parent is the base environment, so the test also
# proves that the emitted helpers run without StatRouter. Tolerance: 1e-8 for marginal effects (spec 10).

.smg_layers <- function() {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = FALSE)
}

# .sc_frame with rows lost only to a missing outcome (they join the "all" standardisation set), rows lost to
# a missing covariate, a numeric 0/1 exposure a01 and an aliased covariate dup (= 2 x1).
.smg_data <- function(n = 5000L) {
  df <- .sc_frame(n)
  df$y[seq(5L, n, by = 37L)] <- NA
  df$x1[seq(11L, n, by = 53L)] <- NA
  df$a01 <- as.integer(df$sex == "M")
  df$dup <- 2 * df$x1
  df
}

.smg_spec <- function(exposure = "sex", ref = "F", terms = NULL, over = "analysis", se = NULL) {
  if (is.null(terms)) {
    terms <- list(list(var = "x1"), list(var = "grp", type = "factor"), list(var = "x2", type = "quad"))
  }
  s <- list(purpose = "explain-causal", outcome = "y", exposure = exposure, terms = terms,
            layers = .smg_layers(), seed = 42L, standardize_over = over)
  if (!is.null(ref)) s$exposure_ref <- ref
  if (!is.null(se)) s$se <- se
  s
}

.smg_route <- function(id) {
  reg <- function(key, make) {
    if (!(.sr_route_key(key, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make())
  }
  reg("logistic", .sr_logistic_route)
  reg("modpois", .sr_modpois_route)
  reg("lm", .sr_lm_route)
  sr_route_get(id, 1L)
}

# The memory specs whose emitted code is pinned (Review Focus 1, ruling R6): a logistic marginal block,
# lm with marginal = TRUE and a logistic marginal block under multiple imputation.
.smg_pin_specs <- function() {
  cc <- .smg_layers()
  mi <- cc
  mi$missing <- "mi"
  list(
    logistic = list(id = "logistic", spec = .smg_spec()),
    lm = list(id = "lm", spec = list(purpose = "explain-causal", outcome = "yc", exposure = "sex",
                                     exposure_ref = "F", terms = list(list(var = "x1")), se = "model",
                                     layers = cc, seed = 42L, marginal = TRUE)),
    mi = list(id = "logistic", spec = list(purpose = "explain-causal", outcome = "y", exposure = "a01",
                                           terms = list(list(var = "x1")), layers = mi,
                                           mi = list(m = 5), seed = 42L))
  )
}

# sha256 of the emitted memory text at the base commit 1ea4d44 (before P1e.1 Task 8), produced by a scratch
# script that sourced `git archive 1ea4d44` and ran .sr_sha256 on .sr_inline(.sr_h_gcomp, ".h_gcomp") and on
# sr_compile() of each .smg_pin_specs() entry. deparse() output is R-version-sensitive: these hashes were
# made under R 4.5.2 and must be regenerated (from 1ea4d44) if the pinned R version changes.
.smg_pin_sha <- c(
  h_gcomp = "8172ae7b92b45d05a290e75295e74acf988acae5b4a6bf6ebf763e4bc055df93",
  logistic = "de5f2e850aad2696640bdc4618d98efbbd4ec609eb7cc656d97cc1d5de1baf8e",
  lm = "ca116caac2ef2d629038e67162eb453d700bac40c1c7b22c9f602c0d77a4148f",
  mi = "e0404647a79efc3834b58b9bda1fe0273c8e716489cb95d380977e45be1d865b"
)

# The in-memory value of the route on the whole table.
.smg_memory <- function(df, spec, id) {
  code <- sr_compile(.smg_route(id), spec)
  env <- new.env(parent = globalenv())
  env$sr_data <- df
  eval(parse(text = code), envir = env)
}

# The streaming helpers, evaluated from their inlined text in a base-parented environment.
.smg_helpers <- function() {
  fns <- c(".h_st_tsqr", ".h_st_alias", ".h_st_fam", ".h_st_mustart", ".h_st_linkfun", ".h_st_glm_eval",
           ".h_st_irls", ".h_st_glm_scores", ".h_st_meat_add", ".h_st_vcov_finish",
           ".h_st_marg_rows", ".h_st_marg_add", ".h_st_marg_finish", ".h_gcomp_finish",
           ".h_st_obs_add", ".h_st_obs_finish")
  code <- c(vapply(fns, function(f) .sr_inline(match.fun(f), f), character(1)),
            .sr_inline(.sr_h_wilson, ".h_wilson"))
  code <- paste(code, collapse = "\n")
  e <- new.env(parent = baseenv())
  eval(parse(text = code), e)
  list(env = e, code = code)
}

# Row blocks of 1..n.
.smg_blocks <- function(n, chunk) {
  s <- seq(1L, n, by = chunk)
  lapply(s, function(a) a:min(n, a + chunk - 1L))
}

# Replay of the stream body over chunks of `chunk` rows. Returns list(rows, obs, n_over): the marginal rows
# as the in-memory block builds them and the observed-risk table.
.smg_stream <- function(df, spec, id, chunk, H) {
  E <- H$env
  tc <- sr_terms_code(spec)
  fam <- if (identical(id, "modpois")) "poisson" else "binomial"
  se <- .sr_glm_se(spec, id)
  rhs_vars <- all.vars(stats::as.formula(paste("~", tc$rhs)))
  used <- c(".y", rhs_vars)
  # the setup fixes the levels and knots on the whole post-exclusion column (ruling R1)
  S <- new.env(parent = globalenv())
  S$sr_data <- df
  eval(parse(text = tc$setup), envir = S)
  eval(parse(text = tc$design), envir = S)
  oc <- parse(text = sr_outcome_code(spec, "binary"))
  fac <- identical(.sr_term_plan(spec)[[1L]]$type, "factor")
  exc <- if (fac) "t1f" else "t1"
  all_rows <- identical(spec$standardize_over, "all")
  # per chunk (R8): the design and outcome snippets, the standardisation rows and the complete cases
  ch <- lapply(.smg_blocks(nrow(df), chunk), function(i) {
    C <- new.env(parent = S)
    C$des <- S$.h_design(df[i, , drop = FALSE])
    eval(oc, envir = C)
    des <- C$des
    over_all <- des[stats::complete.cases(des[rhs_vars]), , drop = FALSE]
    des <- des[stats::complete.cases(des[used]), , drop = FALSE]
    list(des = des, over = if (all_rows) over_all else des,
         X = stats::model.matrix(stats::as.formula(paste("~", tc$rhs)), des))
  })
  nms <- colnames(ch[[1L]]$X)
  passfn <- function(beta, wls) for (k in ch) wls(k$X, k$des[[".y"]])
  st <- E$.h_st_irls(passfn, fam, length(nms), nms)
  # V: model (dispersion 1) or HC0 from the R11 scores
  V <- if (identical(se, "model")) {
    E$.h_st_vcov_finish("model", st$B, 1, st$n, st$rank, NA)
  } else {
    S0 <- NULL
    for (k in ch) {
      Xk <- k$X[, st$keep, drop = FALSE]
      eta <- drop(Xk %*% st$beta[st$keep])
      ep <- if (is.null(st$beta_prev)) NULL else drop(k$X %*% st$beta_prev)
      S0 <- E$.h_st_meat_add(S0, E$.h_st_glm_scores(Xk, k$des[[".y"]], eta, ep, fam), 1)
    }
    E$.h_st_vcov_finish("robust", st$B, S0, st$n, st$rank, NA)
  }
  lv <- if (fac) S$.sr_fixed$levels[["t1"]] else c(0, 1)
  vals <- if (fac) lv else c(1, 0)
  ms <- NULL
  os <- NULL
  for (k in ch) {
    ms <- E$.h_st_marg_add(ms, E$.h_st_marg_rows(k$over, tc$rhs, exc, vals), st$beta, st$keep, fam)
    os <- E$.h_st_obs_add(os, k$des[[exc]], k$des[[".y"]])
  }
  rows <- NULL
  if (fac) {
    for (l in lv[-1L]) {
      g <- E$.h_st_marg_finish(ms, V, c(l, lv[1L]))
      rows <- rbind(rows, data.frame(level = l, as.data.frame(g), stringsAsFactors = FALSE))
    }
  } else {
    g <- E$.h_st_marg_finish(ms, V, c(1, 0))
    rows <- data.frame(level = NA_character_, as.data.frame(g), stringsAsFactors = FALSE)
  }
  list(rows = rows, obs = E$.h_st_obs_finish(os, if (fac) lv else NULL), n_over = ms$n, st = st)
}

.smg_nnt <- function(r) {
  z <- stats::qnorm(0.975)
  .sr_h_nnt(r$rd, r$rd - z * r$rd_se, r$rd + z * r$rd_se)
}

.smg_expect <- function(sm, mem, info) {
  mg <- mem$marginal
  expect_false(is.null(mg$rows), info = info)
  expect_identical(names(sm$rows), names(mg$rows), info = info)
  expect_identical(sm$rows$level, mg$rows$level, info = info)
  for (f in c("p1", "p0", "rd", "rd_se", "log_rr", "log_rr_se")) {
    expect_equal(sm$rows[[f]], mg$rows[[f]], tolerance = 1e-8, info = paste(info, f))
  }
  expect_identical(sm$n_over, as.numeric(mg$n_over), info = info)
  for (i in seq_len(nrow(mg$rows))) {
    a <- .smg_nnt(sm$rows[i, ])
    b <- .smg_nnt(mg$rows[i, ])
    expect_equal(a[c("nnt", "lo", "hi")], b[c("nnt", "lo", "hi")], tolerance = 1e-8, info = info)
    expect_identical(a$label, b$label, info = info)
  }
}

test_that("the emitted memory code is byte-identical to the base commit", {
  # Review Focus 1 / R6: a memory spec compiles exactly as before P1e, so cached fits and exported
  # analysis.R stay valid. .h_gcomp_finish belongs to the stream path only.
  expect_identical(.sr_sha256(.sr_inline(.sr_h_gcomp, ".h_gcomp")), .smg_pin_sha[["h_gcomp"]])
  sp <- .smg_pin_specs()
  for (nm in names(sp)) {
    code <- sr_compile(.smg_route(sp[[nm]]$id), sp[[nm]]$spec)
    expect_false(grepl(".h_gcomp_finish", code, fixed = TRUE), info = nm)
    expect_identical(.sr_sha256(code), .smg_pin_sha[[nm]], info = nm)
  }
})

test_that(".h_gcomp_finish equals .sr_h_gcomp", {
  d <- .smg_data(600L)
  d <- d[stats::complete.cases(d[c("y", "x1")]), ]
  fit <- stats::glm(y ~ a01 + x1, data = d, family = stats::binomial())
  g <- .sr_h_gcomp(fit, d, "a01", c(1, 0), stats::vcov(fit))
  expect_identical(names(g), c("p1", "p0", "rd", "rd_se", "log_rr", "log_rr_se"))
  # the finish on the same means and gradients
  X <- stats::model.matrix(fit)
  at <- function(v) {
    Xa <- X
    Xa[, "a01"] <- v
    eta <- drop(Xa %*% stats::coef(fit))
    list(mu = mean(stats::plogis(eta)), grad = colMeans(Xa * stats::dlogis(eta)))
  }
  e1 <- at(1)
  e0 <- at(0)
  f <- .h_gcomp_finish(c(`1` = e1$mu, `0` = e0$mu), list(`1` = e1$grad, `0` = e0$grad), stats::vcov(fit), c(1, 0))
  expect_equal(f, g, tolerance = 1e-12)
})

test_that("streamed g-computation equals .sr_h_gcomp", {
  df <- .smg_data(5000L)
  H <- .smg_helpers()
  for (id in c("logistic", "modpois")) {
    for (over in c("analysis", "all")) {
      spec <- .smg_spec(over = over)
      mem <- .smg_memory(df, spec, id)
      sm <- .smg_stream(df, spec, id, 7L, H)
      .smg_expect(sm, mem, paste(id, over, "chunk 7"))
      if (over == "all") expect_gt(sm$n_over, mem$n)
      if (over == "analysis") expect_identical(sm$n_over, as.numeric(mem$n))
    }
  }
})

test_that("streamed g-computation: other chunk sizes, >2 levels, aliasing, robust V and a 0/1 exposure", {
  df <- .smg_data(3000L)
  H <- .smg_helpers()
  cases <- list(
    # a 4-level text exposure (mixed case, non-ASCII) with an aliased covariate and HC0
    list(id = "logistic", spec = .smg_spec("grp", "A", list(list(var = "x1"), list(var = "dup"),
                                                            list(var = "sex", type = "factor")),
                                           over = "all", se = "robust")),
    # a numeric 0/1 exposure, a factor whose level z sits only in the last rows (absent from most chunks)
    list(id = "modpois", spec = .smg_spec("a01", NULL, list(list(var = "x1"), list(var = "late", type = "factor")),
                                          over = "analysis")),
    list(id = "logistic", spec = .smg_spec("a01", NULL, list(list(var = "x1"), list(var = "late", type = "factor")),
                                           over = "all", se = "robust"))
  )
  for (cs in cases) {
    mem <- .smg_memory(df, cs$spec, cs$id)
    for (chunk in c(3L, 7L, 1000L, nrow(df) + 5L)) {
      info <- paste(cs$id, cs$spec$exposure, "chunk", chunk)
      sm <- .smg_stream(df, cs$spec, cs$id, chunk, H)
      .smg_expect(sm, mem, info)
      if (identical(cs$spec$exposure, "grp")) {
        expect_identical(mem$aliased, "t3")
        expect_identical(sm$st$aliased, "t3")
        expect_identical(nrow(sm$rows), 3L)
      }
    }
  }
})

test_that("streamed observed risk equals the in-memory table", {
  df <- .smg_data(3000L)
  H <- .smg_helpers()
  num <- function(o) {
    k <- vapply(o, is.numeric, logical(1))
    o[k] <- lapply(o[k], round, 12)
    o
  }
  specs <- list(list("logistic", .smg_spec()),
                list("modpois", .smg_spec("grp", "A", list(list(var = "x1")))),
                list("logistic", .smg_spec("a01", NULL, list(list(var = "x1")))))
  for (s in specs) {
    mem <- .smg_memory(df, s[[2L]], s[[1L]])
    expect_gt(nrow(mem$obs), 0L)
    for (chunk in c(7L, 1000L)) {
      sm <- .smg_stream(df, s[[2L]], s[[1L]], chunk, H)
      expect_identical(num(sm$obs), num(mem$obs), info = paste(s[[1L]], s[[2L]]$exposure, chunk))
    }
  }
  # a numeric exposure that is not 0/1 gives no table (NULL, the value then holds data.frame())
  st <- H$env$.h_st_obs_add(NULL, c(0, 1, 1), c(1L, 0L, 1L))
  st <- H$env$.h_st_obs_add(st, c(2, 0), c(0L, 1L))
  expect_null(H$env$.h_st_obs_finish(st, NULL))
  # an empty chunk changes nothing; a level never seen has n = 0 and NA Wilson bounds
  st <- H$env$.h_st_obs_add(NULL, factor(c("a", "b"), levels = c("a", "b", "c")), c(1L, 0L))
  st2 <- H$env$.h_st_obs_add(st, factor(character(), levels = c("a", "b", "c")), integer())
  expect_identical(st2, st)
  ob <- H$env$.h_st_obs_finish(st, c("a", "b", "c"))
  expect_identical(ob$n, c(1L, 1L, 0L))
  expect_identical(ob$events, c(1L, 0L, 0L))
  expect_true(is.na(ob$est[3L]) && is.na(ob$lo[3L]) && is.na(ob$hi[3L]))
})

test_that("the marginal helpers: lm (identity), empty chunks, refusals, inlining", {
  H <- .smg_helpers()
  E <- H$env
  expect_false(grepl("[^\\x01-\\x7F]", H$code, perl = TRUE))
  expect_silent(.sr_emit_check(H$code, "analysis"))
  d <- .smg_data(800L)
  d <- d[stats::complete.cases(d[c("yc", "x1")]), ]
  d$sexf <- factor(d$sex, levels = c("F", "M"))
  fit <- stats::lm(yc ~ sexf + x1, data = d)
  ref <- .sr_h_gcomp(fit, d, "sexf", c("M", "F"), stats::vcov(fit))
  b <- stats::coef(fit)
  ms <- NULL
  for (i in .smg_blocks(nrow(d), 7L)) {
    ms <- E$.h_st_marg_add(ms, E$.h_st_marg_rows(d[i, , drop = FALSE], "sexf + x1", "sexf", c("F", "M")),
                           b, seq_along(b), "gaussian")
  }
  # an empty chunk leaves the sums unchanged
  ms2 <- E$.h_st_marg_add(ms, E$.h_st_marg_rows(d[0L, , drop = FALSE], "sexf + x1", "sexf", c("F", "M")),
                          b, seq_along(b), "gaussian")
  expect_identical(ms2, ms)
  expect_equal(E$.h_st_marg_finish(ms, stats::vcov(fit), c("M", "F")), ref, tolerance = 1e-8)
  expect_error(E$.h_st_marg_finish(NULL, stats::vcov(fit), c("M", "F")),
               "over must be a data frame with at least one row", fixed = TRUE)
  dd <- d[1:5, ]
  dd$x1[2L] <- NA
  expect_error(E$.h_st_marg_rows(dd, "sexf + x1", "sexf", c("F", "M")),
               "the rows to standardise over have missing values in the model variables", fixed = TRUE)
  expect_error(E$.h_gcomp_finish(c(a = 1), list(a = 1), diag(1), c("a", "b")),
               "the means and gradients must hold the exposed and the reference level", fixed = TRUE)
})
