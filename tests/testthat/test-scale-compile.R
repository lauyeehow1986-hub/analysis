# The stream compile branches of lm, logistic and modpois (P1e.1 Task 9, rulings R3, R8, R11 and R15).
# Synthetic data only.
#
# The reference is the in-memory route: its prologue and body evaluated on the rds of the same rows. The
# stream side is the stream prologue and the stream body evaluated on the Parquet extract, as a run does.
# Tolerances (spec section 10): 1e-8 for estimates, model SE, intervals and marginal effects; 1e-7 for the
# robust and cluster SE.

# The harness (.ssc_spec, .ssc_data, .ssc_memory, .ssc_stream, .ssc_expect, ...) is in helper-scale.R.


test_that("stream body is lint-clean, ASCII and URL-free", {
  for (id in c("lm", "logistic", "modpois")) {
    for (se in c("model", "robust", "cluster")) {
      info <- paste(id, se)
      spec <- .ssc_spec(id, se = se)
      if (identical(id, "modpois") && identical(se, "model")) {
        expect_error(.ssc_body(spec, id, 1000L), "robust or cluster SE", info = info)
        next
      }
      for (over in c("analysis", "all")) {
        spec$standardize_over <- over
        body <- .ssc_body(spec, id, 1000L)
        expect_length(body, 1L)
        expect_silent(parse(text = body))
        expect_true(.sr_emit_check(body, "analysis", stream = TRUE), info = info)
        expect_false(grepl("[^\\x01-\\x7F]", body, perl = TRUE), info = info)
        expect_false(grepl("http", body, fixed = TRUE), info = info)
        expect_false(grepl("p.value", body, fixed = TRUE), info = info)
        expect_false(grepl("p_value", body, fixed = TRUE), info = info)
        expect_true(grepl(".sr_chunk <- 1000L", body, fixed = TRUE), info = info)
        expect_true(grepl("if (exists(\"sr_progress_file\"))", body, fixed = TRUE), info = info)
        expect_false(grepl("sr_data <- unserialize", body, fixed = TRUE), info = info)
        # every in-memory stop text is held by the stream body
        expect_true(grepl("no complete rows: every row has a missing value in the model", body, fixed = TRUE), info = info)
        if (!identical(id, "lm")) {
          expect_true(grepl("the outcome has no events or no non-events among the ", body, fixed = TRUE), info = info)
        } else {
          expect_true(grepl("no residual degrees of freedom: ", body, fixed = TRUE), info = info)
        }
        if (identical(se, "cluster")) {
          expect_true(grepl("se = cluster needs at least two clusters (found ", body, fixed = TRUE), info = info)
          expect_true(grepl("the cluster vector must not contain missing values", body, fixed = TRUE), info = info)
          expect_true(grepl("ORDER BY", body, fixed = TRUE), info = info)
        }
      }
    }
  }
  # a stream spec without a chunk size is a compile error
  for (id in c("lm", "logistic", "modpois")) {
    s <- utils::modifyList(.ssc_spec(id, se = "robust"), list(engine = "stream"))
    expect_error(sr_compile(.ssc_route(id), s), "a stream spec needs stream_chunk (set by the run)", fixed = TRUE)
  }
})

test_that("memory compile is byte-identical with and without engine", {
  for (id in c("lm", "logistic", "modpois")) {
    for (se in c("model", "robust", "cluster")) {
      if (identical(id, "modpois") && identical(se, "model")) next
      spec <- .ssc_spec(id, se = se)
      r <- .ssc_route(id)
      base <- sr_compile(r, spec)
      for (eng in c("memory", "auto")) {
        expect_identical(sr_compile(r, utils::modifyList(spec, list(engine = eng))), base, info = paste(id, se, eng))
        expect_identical(sr_compile(r, utils::modifyList(spec, list(engine = eng, stream_chunk = 500))), base,
                         info = paste(id, se, eng, "chunk"))
      }
    }
  }
})

test_that("stream body evaluates to the in-memory value", {
  # the brief's case: logistic with a factor, an RCS term and the cluster SE, chunks of 1000 rows
  df <- .ssc_data(3000L)
  spec <- .ssc_spec("logistic", se = "cluster")
  mem <- .ssc_memory(df, spec, "logistic")
  st <- .ssc_stream(df, spec, "logistic", 1000L)
  expect_equal(st$coef$est, mem$coef$est, tolerance = 1e-7)
  for (f in c("n", "n_total", "events", "aliased", "levels", "n_clusters")) expect_identical(st[[f]], mem[[f]], info = f)
  .ssc_expect(st, mem, "logistic", 1e-7, "logistic cluster")
  expect_identical(st$chunk, 1000L)
  expect_true(st$converged)
  # the IRLS passes (iter + 1), the sorted cluster pass and the marginal pass
  expect_identical(st$passes, st$iter + 1L + 1L + 1L)
})

test_that("streamed lm, logistic and modpois equal memory for every SE", {
  df <- .ssc_data(2500L)
  for (id in c("lm", "logistic", "modpois")) {
    for (se in c("model", "robust", "cluster")) {
      if (identical(id, "modpois") && identical(se, "model")) next
      for (over in c("analysis", "all")) {
        info <- paste(id, se, over)
        spec <- .ssc_spec(id, se = se, over = over)
        mem <- .ssc_memory(df, spec, id)
        st <- .ssc_stream(df, spec, id, 700L)
        .ssc_expect(st, mem, id, .ssc_tol(se), info)
      }
    }
  }
  # a numeric 0/1 exposure, marginal off for lm
  for (id in c("lm", "logistic")) {
    spec <- .ssc_spec(id, se = "robust", exposure = "a01", ref = NULL, marginal = if (identical(id, "lm")) FALSE)
    .ssc_expect(.ssc_stream(df, spec, id, 900L), .ssc_memory(df, spec, id), id, 1e-7, paste(id, "a01"))
  }
})

test_that("progress follows ruling R15 and the passes are counted", {
  df <- .ssc_data(1500L)
  for (id in c("lm", "logistic")) {
    for (se in c("model", "robust", "cluster")) {
      pf <- tempfile(fileext = ".json")
      spec <- .ssc_spec(id, se = se)
      st <- .ssc_stream(df, spec, id, 600L, env_extra = list(sr_progress_file = pf))
      fitp <- if (identical(id, "lm")) 1L else 26L
      m <- fitp + (se != "model") + (se != "robust")
      got <- jsonlite::fromJSON(readLines(pf, warn = FALSE))
      expect_identical(as.integer(got$pass), st$passes, info = paste(id, se))
      expect_identical(as.integer(got$max), as.integer(m), info = paste(id, se))
      fit_done <- if (identical(id, "lm")) 1L else st$iter + 1L
      expect_identical(st$passes, fit_done + (se != "model") + (se != "robust"), info = paste(id, se))
      unlink(pf)
    }
  }
  # without sr_progress_file the body runs (the exported script)
  expect_silent(.ssc_stream(df, .ssc_spec("lm", se = "model"), "lm", 600L))
})

test_that("the merged marginal pass equals the separate one", {
  df <- .ssc_data(2000L)
  for (id in c("lm", "logistic", "modpois")) {
    for (over in c("analysis", "all")) {
      spec <- .ssc_spec(id, se = "robust", over = over)
      a <- .ssc_stream(df, spec, id, 800L, merge = TRUE)
      b <- .ssc_stream(df, spec, id, 800L, merge = FALSE)
      expect_identical(b$passes, a$passes + 1L, info = paste(id, over))
      a$passes <- b$passes <- NULL
      expect_equal(a, b, tolerance = 1e-12, info = paste(id, over))
    }
  }
})

test_that("absent levels follow the in-memory fit", {
  df <- .ssc_data(2000L)
  z <- df$late == "z"
  # an exposure level present only among outcome-missing rows: the coefficient is absent, the marginal stops
  d1 <- df
  d1$y[z] <- NA
  d1$yc[z] <- NA
  for (id in c("lm", "logistic")) {
    spec <- .ssc_spec(id, se = "robust", exposure = "late", ref = "a", marginal = FALSE)
    mem <- .ssc_memory(d1, spec, id)
    st <- .ssc_stream(d1, spec, id, 500L)
    expect_false(any(grepl("t1fz", mem$coef$term, fixed = TRUE)))
    .ssc_expect(st, mem, id, 1e-7, paste(id, "late exposure"))
    sm <- utils::modifyList(spec, list(marginal = TRUE))
    em <- .ssc_err(.ssc_memory(d1, sm, id))
    expect_identical(em, "factor t1f has new level z")
    expect_identical(.ssc_err(.ssc_stream(d1, sm, id, 500L)), em, info = id)
  }
  # a covariate level only among outcome-missing rows: same coefficients and aliasing as memory; under
  # "all" the standardisation rows hold the level, which stops g-computation with the memory wording
  terms <- list(list(var = "x1"), list(var = "late", type = "factor"))
  for (id in c("lm", "logistic", "modpois")) {
    for (over in c("analysis", "all")) {
      spec <- .ssc_spec(id, se = "robust", terms = terms, over = over)
      em <- .ssc_err(mem <- .ssc_memory(d1, spec, id))
      es <- .ssc_err(st <- .ssc_stream(d1, spec, id, 500L))
      expect_identical(es, em, info = paste(id, over))
      if (identical(over, "all")) {
        expect_identical(em, "factor t3f has new levels z")
      } else {
        expect_true(is.na(em))
        .ssc_expect(st, mem, id, 1e-7, paste(id, over, "late covariate"))
      }
    }
  }
  # an aliased covariate: the same aliased names and coefficient names as memory
  d2 <- df
  d2$dup <- 2 * d2$x1
  terms2 <- list(list(var = "x1"), list(var = "dup"), list(var = "grp", type = "factor"))
  for (id in c("lm", "logistic")) {
    spec <- .ssc_spec(id, se = "model", terms = terms2)
    mem <- .ssc_memory(d2, spec, id)
    st <- .ssc_stream(d2, spec, id, 500L)
    expect_identical(mem$aliased, "t3")
    .ssc_expect(st, mem, id, 1e-8, paste(id, "aliased"))
  }
  # a factor left with one level among the fitted rows stops as lm / glm do
  d3 <- df
  d3$y[d3$grp != "A"] <- NA
  d3$yc[d3$grp != "A"] <- NA
  for (id in c("lm", "logistic")) {
    spec <- .ssc_spec(id, se = "model")
    em <- .ssc_err(.ssc_memory(d3, spec, id))
    expect_match(em, "contrasts can be applied only to factors with 2 or more levels", fixed = TRUE)
    expect_identical(.ssc_err(.ssc_stream(d3, spec, id, 500L)), em, info = id)
  }
})

test_that("a non-binary numeric exposure follows the in-memory marginal rule (R8)", {
  df <- .ssc_data(2000L)
  # many distinct values (the heavily tied xt, read in small chunks): strict stops, non-strict notes
  for (ex in c("xt", "x2", "a02")) {
    info <- ex
    terms <- list(list(var = "x1"))
    lg <- .ssc_spec("logistic", se = "robust", exposure = ex, ref = NULL, terms = terms)
    mem <- .ssc_memory(df, lg, "logistic")
    expect_match(mem$marginal$note, "marginal effects were not computed", fixed = TRUE)
    st <- .ssc_stream(df, lg, "logistic", 300L)
    expect_identical(st$marginal, mem$marginal, info = info)
    .ssc_expect(st, mem, "logistic", 1e-7, info)
    for (id in c("lm", "logistic")) {
      s <- .ssc_spec(id, se = "model", exposure = ex, ref = NULL, terms = terms, marginal = TRUE)
      em <- .ssc_err(.ssc_memory(df, s, id))
      expect_match(em, "marginal effects need a binary (0/1) or categorical exposure; the exposure has ", fixed = TRUE)
      expect_identical(.ssc_err(.ssc_stream(df, s, id, 300L)), em, info = paste(id, ex))
    }
  }
})

test_that("the stream body keeps the in-memory stop texts", {
  df <- .ssc_data(600L)
  run_both <- function(d, spec, id, chunk = 200L) {
    em <- .ssc_err(.ssc_memory(d, spec, id))
    expect_false(is.na(em), info = id)
    expect_identical(.ssc_err(.ssc_stream(d, spec, id, chunk)), em, info = id)
  }
  # no complete rows
  d <- df
  d$y <- NA_integer_
  d$yc <- NA_real_
  for (id in c("lm", "logistic")) run_both(d, .ssc_spec(id, se = "model"), id)
  # no events
  d <- df
  d$y <- 0L
  run_both(d, .ssc_spec("logistic", se = "model"), "logistic")
  run_both(d, .ssc_spec("modpois", se = "robust"), "modpois")
  # no residual degrees of freedom
  d <- df[1:6, ]
  d$sex <- rep(c("M", "F"), 3L)
  d$grp <- rep(c("A", "b", "C"), 2L)
  run_both(d, .ssc_spec("lm", se = "model", terms = list(list(var = "x1"), list(var = "grp", type = "factor"),
                                                         list(var = "x2"))), "lm", 2L)
  # one cluster only
  d <- df
  d$pid <- 1L
  for (id in c("lm", "logistic")) run_both(d, .ssc_spec(id, se = "cluster"), id)
})

test_that("the lm finish takes sum(y^2) without an intercept", {
  set.seed(3)
  x <- stats::rnorm(200)
  y <- 1 + x + stats::rnorm(200)
  mom <- .h_st_moments_add(NULL, y)
  expect_identical(.h_st_sst(mom, sum(y^2), TRUE), mom$m2)
  expect_identical(.h_st_sst(mom, sum(y^2), FALSE), sum(y^2))
  X <- cbind(x = x)
  M <- .h_st_tsqr(NULL, cbind(X, y))
  f <- .h_st_lm_finish(M, 1L, "x", 200L, .h_st_sst(mom, sum(y^2), "(Intercept)" %in% "x"))
  sm <- summary(stats::lm(y ~ 0 + x))
  expect_equal(f$r2, sm$r.squared, tolerance = 1e-10)
  expect_equal(f$adj_r2, sm$adj.r.squared, tolerance = 1e-10)
})

test_that(".h_st_irls with select fits the selected columns only", {
  set.seed(4)
  n <- 400L
  X <- cbind(`(Intercept)` = 1, a = stats::rnorm(n), b = 0, c = stats::rnorm(n))
  y <- stats::rbinom(n, 1L, stats::plogis(0.3 * X[, "a"]))
  passfn <- function(beta, wls) {
    for (i in split(seq_len(n), rep(1:4, each = 100L))) wls(X[i, , drop = FALSE], y[i])
  }
  st <- .h_st_irls(passfn, "binomial", NA, NULL,
                   select = function() list(p = 4L, nms = colnames(X), sel = c(1L, 2L, 4L)))
  fit <- stats::glm(y ~ X[, "a"] + X[, "c"], family = stats::binomial())
  expect_equal(unname(st$beta), unname(stats::coef(fit)), tolerance = 1e-8)
  expect_identical(names(st$beta), c("(Intercept)", "a", "c"))
  expect_identical(st$sel, c(1L, 2L, 4L))
  expect_length(st$beta_prev, 4L)
  expect_identical(st$aliased, character())
})
