# Shared helpers for the streaming-engine tests (P1e.1). Synthetic data only.

# A synthetic analysis table: clusters of 1-9 rows (so they cross chunk boundaries), a 0/1 outcome `y`, a
# continuous outcome `yc`, a tie-free `x2`, a heavily tied `xt`, text `grp` with a mixed-case and a
# non-ASCII level, `late` whose level "z" appears only in the last 5 rows, and the exposure `sex`.
.sc_frame <- function(n = 20000L, seed = 1L) {
  set.seed(seed)
  n <- as.integer(n)
  sizes <- sample.int(9L, n, replace = TRUE)
  pid <- rep(seq_along(sizes), sizes)[seq_len(n)]
  x1 <- stats::rnorm(n)
  x2 <- stats::rgamma(n, shape = 2, rate = 1)
  sex <- sample(c("M", "F"), n, replace = TRUE)
  y <- stats::rbinom(n, 1L, stats::plogis(-0.5 + 0.4 * x1 + 0.3 * (sex == "M")))
  yc <- 1 + 0.5 * x1 + 0.3 * (sex == "M") + stats::rnorm(n)
  late <- sample(c("a", "b"), n, replace = TRUE)
  late[max(1L, n - 4L):n] <- "z"
  data.frame(id = seq_len(n), pid = pid, y = y, yc = yc, x1 = x1, x2 = x2,
             xt = round(stats::runif(n) * 5),
             grp = sample(c("A", "b", "C", "\u00e9"), n, replace = TRUE),
             late = late, sex = sex, stringsAsFactors = FALSE)
}

# A locked project with one primary analysis a1 on route `route` over .sc_frame(). `spec` is the route
# spec (a list); the roles are id, y (outcome) and sex (exposure). Mirrors .run_locked of helper-run.R.
.sc_locked <- function(spec, route = "logistic", env = parent.frame(), df = .sc_frame(2000L)) {
  p <- .fx_ready_project(df, "t", c(id = "id", y = "outcome", sex = "exposure"), env = env)
  p <- sr_sap_add(p, "a1", route, spec, "t", label = "primary")
  sr_sap_lock(.fx_feasible(p))
}

# Write `df` plus a .sr_row column (1..n, unless df has one) to a temp parquet; returns the path.
.sc_parquet <- function(df, env = parent.frame()) {
  path <- tempfile(fileext = ".parquet")
  withr::defer(unlink(path), envir = env)
  if (is.null(df$.sr_row)) df$.sr_row <- seq_len(nrow(df))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "sc_df", df)
  DBI::dbExecute(con, sprintf("COPY sc_df TO '%s' (FORMAT PARQUET)", gsub("'", "''", path)))
  path
}

# A locked-down connection from the inlined streaming helpers (.h_st_open, Task 5), closed on defer.
.sc_con <- function(path, env = parent.frame()) {
  con <- .h_st_open(path, "2GB", 2L, tempdir())
  withr::defer(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), envir = env)
  con
}

# ---- the compile-level memory vs stream harness (moved from test-scale-compile.R for Task 15) ---------
#
# The reference is the in-memory route: its prologue and body evaluated on the rds of the same rows. The
# stream side is the stream prologue and the stream body evaluated on the Parquet extract, as a run does.
# Tolerances (spec section 10): 1e-8 for estimates, model SE, intervals and marginal effects; 1e-7 for the
# robust and cluster SE.

.ssc_layers <- function(clustered = FALSE) {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = clustered)
}

.ssc_route <- function(id) {
  reg <- function(key, make) {
    if (!(.sr_route_key(key, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make())
  }
  reg("logistic", .sr_logistic_route)
  reg("modpois", .sr_modpois_route)
  reg("lm", .sr_lm_route)
  reg("cox", .sr_cox_route)
  reg("cox_describe", .sr_cox_describe_route)
  reg("rmst", .sr_rmst_route)
  sr_route_get(id, 1L)
}

# A spec of route `id`: exposure sex (ref F), terms x1, grp (factor) and x2 (rcs, 4 knots) unless given.
.ssc_spec <- function(id, se = NULL, exposure = "sex", ref = "F", terms = NULL, over = "analysis",
                      marginal = NULL) {
  if (is.null(terms)) {
    terms <- list(list(var = "x1"), list(var = "grp", type = "factor"), list(var = "x2", type = "rcs"))
  }
  s <- list(purpose = "explain-causal", outcome = if (identical(id, "lm")) "yc" else "y",
            exposure = exposure, terms = terms, layers = .ssc_layers(identical(se, "cluster")),
            seed = 42L, standardize_over = over)
  if (!is.null(ref)) s$exposure_ref <- ref
  if (!is.null(se)) s$se <- se
  if (identical(se, "cluster")) s$cluster <- "pid"
  if (!is.null(marginal)) s$marginal <- marginal
  if (identical(id, "lm") && is.null(marginal)) s$marginal <- TRUE
  s
}

# .sc_frame with rows lost only to a missing outcome, rows lost to a missing covariate, a numeric 0/1
# exposure a01 and a two-valued non-binary exposure a02.
.ssc_data <- function(n = 3000L) {
  df <- .sc_frame(n)
  df$y[seq(5L, n, by = 37L)] <- NA
  df$yc[seq(5L, n, by = 37L)] <- NA
  df$x1[seq(11L, n, by = 53L)] <- NA
  df$a01 <- as.integer(df$sex == "M")
  df$a02 <- 2L * df$a01
  df
}

# A project folder holding one extract file; returns list(proj, ex).
.ssc_proj <- function(env = parent.frame()) {
  proj <- tempfile("ssc-proj")
  dir.create(file.path(proj, "data", "extracts"), recursive = TRUE)
  withr::defer(unlink(proj, recursive = TRUE), envir = env)
  proj
}

.ssc_env <- function(proj) {
  e <- new.env(parent = globalenv())
  e$sr_project_dir <- proj
  e$sr_duck_memory <- "2GB"
  e$sr_duck_threads <- 2L
  e$sr_duck_temp <- tempdir()
  e
}

# The in-memory value: the memory prologue (the rds extract of sr_extract) and the compiled body.
.ssc_memory <- function(df, spec, id) {
  proj <- .ssc_proj()
  bytes <- serialize(df, NULL, version = 3)
  sha <- .sr_sha256(bytes)
  rel <- paste0("data/extracts/", sha, ".rds")
  writeBin(bytes, file.path(proj, rel))
  code <- paste(.sr_run_prologue(list(rel = rel, sha = sha)), sr_compile(.ssc_route(id), spec), sep = "\n")
  eval(parse(text = code), .ssc_env(proj))
}

# The stream body of `spec` (engine = "stream", stream_chunk = chunk); merge = FALSE forces the separate
# marginal pass through the internal compile functions.
.ssc_body <- function(spec, id, chunk, merge = TRUE) {
  s <- utils::modifyList(spec, list(engine = "stream", stream_chunk = chunk))
  if (isTRUE(merge) || id %in% c("cox", "cox_describe", "rmst")) return(sr_compile(.ssc_route(id), s))
  if (identical(id, "lm")) .sr_lm_compile_stream(s, merge = FALSE) else .sr_glm_compile_stream(s, id, merge = FALSE)
}

# The streamed value: the stream prologue on the Parquet extract, then the stream body. `env_extra` is a
# list of variables bound in the evaluation environment (sr_progress_file).
.ssc_stream <- function(df, spec, id, chunk = 1000L, merge = TRUE, env_extra = list()) {
  proj <- .ssc_proj()
  path <- .sc_parquet(df)
  sha <- paste(as.character(unclass(openssl::sha256(file(path)))), collapse = "")
  rel <- paste0("data/extracts/", sha, ".parquet")
  file.copy(path, file.path(proj, rel))
  code <- paste(.sr_run_prologue_stream(list(rel = rel, sha = sha)), .ssc_body(spec, id, chunk, merge),
                sep = "\n")
  e <- .ssc_env(proj)
  for (nm in names(env_extra)) e[[nm]] <- env_extra[[nm]]
  eval(parse(text = code), e)
}

.ssc_err <- function(expr) tryCatch({
  expr
  NA_character_
}, error = conditionMessage)

# Compare a streamed value with the in-memory one. se_tol is the tolerance of the SEs (1e-8 for the model
# SE, 1e-7 for robust and cluster).
.ssc_expect <- function(st, mem, id, se_tol, info = "") {
  extra <- c("engine", "chunk", "passes", "iter")
  expect_identical(names(st), c(names(mem), extra), info = info)
  expect_identical(st$engine, "stream", info = info)
  expect_identical(st$coef$term, mem$coef$term, info = info)
  expect_equal(st$coef$est, mem$coef$est, tolerance = 1e-8, info = info)
  for (f in c("se", "lo", "hi")) expect_equal(st$coef[[f]], mem$coef[[f]], tolerance = se_tol, info = paste(info, f))
  for (f in c("aliased", "n", "n_total", "levels", "n_clusters", "se_type", "level")) {
    expect_identical(st[[f]], mem[[f]], info = paste(info, f))
  }
  if (identical(id, "lm")) {
    expect_null(st$fig, info = info)
    expect_identical(st$df, mem$df, info = info)
    expect_identical(st$df_resid, mem$df_resid, info = info)
    expect_identical(st$iter, NA_integer_, info = info)
    for (f in c("sigma", "r2", "adj_r2")) expect_equal(st[[f]], mem[[f]], tolerance = 1e-8, info = paste(info, f))
  } else {
    for (f in c("events", "df", "penalty", "family", "converged", "near_separation", "risk_above_one")) {
      expect_identical(st[[f]], mem[[f]], info = paste(info, f))
    }
    if (!nrow(mem$obs)) {
      # no observed-risk table (a non-binary numeric exposure): both are the empty data.frame()
      expect_identical(st$obs, mem$obs, info = info)
    } else {
      expect_identical(st$obs[c("level", "n", "events")], mem$obs[c("level", "n", "events")], info = info)
      expect_equal(st$obs[c("est", "lo", "hi")], mem$obs[c("est", "lo", "hi")], tolerance = 1e-12, info = info)
    }
  }
  mg <- mem$marginal
  sm <- st$marginal
  if (is.null(mg)) {
    expect_null(sm, info = info)
  } else {
    expect_identical(sm[c("n_over", "over", "note")], mg[c("n_over", "over", "note")], info = info)
    expect_identical(names(sm$rows), names(mg$rows), info = info)
    if (!is.null(mg$rows)) {
      expect_identical(sm$rows$level, mg$rows$level, info = info)
      for (f in c("p1", "p0", "rd", "log_rr")) {
        expect_equal(sm$rows[[f]], mg$rows[[f]], tolerance = 1e-8, info = paste(info, f))
      }
      for (f in c("rd_se", "log_rr_se")) {
        expect_equal(sm$rows[[f]], mg$rows[[f]], tolerance = se_tol, info = paste(info, f))
      }
    }
  }
}

.ssc_tol <- function(se) if (identical(se, "model")) 1e-8 else 1e-7

# ---- the survival harness (P1e.2) ---------------------------------------------------------------------
#
# .ssc_surv_frame(n, seed): .sc_frame plus `time` (integer times with heavy ties, and every 41st row moved
# 1e-10 up, a near tie that aeqSurv merges), `status` (0/1, about 30% censored), the strata `st1` (2 values)
# and `st2` (3 values) and `yr` (about 2000, for the centring). The hazard rises with x1 and sex M.
.ssc_surv_frame <- function(n = 600L, seed = 1L) {
  df <- .sc_frame(n, seed)
  set.seed(seed + 1000L)
  n <- nrow(df)
  haz <- exp(0.5 * df$x1 + 0.4 * (df$sex == "M"))
  tm <- ceiling(stats::rexp(n, 0.15 * haz)) * 1.0
  nt <- seq(3L, n, by = 41L)
  tm[nt] <- tm[nt] + 1e-10
  df$time <- tm
  df$status <- stats::rbinom(n, 1L, 0.7)
  df$st1 <- sample(c("a", "b"), n, replace = TRUE)
  df$st2 <- sample(c("u", "v", "w"), n, replace = TRUE)
  df$yr <- 2000 + sample(-6:6, n, replace = TRUE) + stats::rnorm(n, sd = 0.1)
  df
}

# A spec of the survival route `id` (cox, cox_describe or rmst): time, status (0/1, no status_event),
# exposure sex (ref F), terms x1 and grp (factor) for cox; the named arguments are set over these (NULL
# removes a field).
.ssc_surv_spec <- function(id, ...) {
  s <- list(purpose = if (identical(id, "cox_describe")) "describe" else "explain-causal",
            time = "time", status = "status", exposure = "sex", exposure_ref = "F",
            layers = .ssc_layers(), seed = 42L)
  if (identical(id, "rmst")) {
    s$tau <- 10
  } else {
    s$terms <- list(list(var = "x1"), list(var = "grp", type = "factor"))
  }
  m <- list(...)
  for (nm in names(m)) s[nm] <- list(m[[nm]])
  s[!vapply(s, is.null, logical(1))]
}

# The coefficient rows coxph flags as possibly infinite ("Loglik converged before variable i,j ; ..."), as a
# logical over value$coef$term. The index counts all design columns, so it maps onto coef$term only with
# no aliased column (the flagged cases here have none). Such an estimate sits on a flat likelihood with a
# huge SE, and the summation order moves it at about 1e-7 relative, so these rows are compared at 1e-6
# (plan correction); every other row keeps the table's tolerance.
.ssc_inf_terms <- function(v) {
  inf <- logical(length(v$coef$term))
  w <- grep("Loglik converged before variable", v$warnings, fixed = TRUE, value = TRUE)
  if (!length(w) || length(v$aliased)) return(inf)
  idx <- as.integer(unlist(strsplit(trimws(sub(".*variable +([0-9, ]+);.*", "\\1", w)), ",")))
  inf[idx[!is.na(idx) & idx <= length(inf)]] <- TRUE
  inf
}

# Compare a streamed survival value with the in-memory one by the plan's tolerance table: the S18 names,
# the coefficients at 1e-8, the counts, aliased, levels and warnings identical, KM surv / lower / upper at
# 1e-12 and the KM counts and at-risk table identical.
.ssc_surv_expect <- function(st, mem, info = "") {
  expect_identical(names(st), c(names(mem), "engine", "chunk", "passes", "iter"), info = info)
  expect_identical(st$engine, "stream", info = info)
  expect_identical(st$coef$term, mem$coef$term, info = info)
  inf <- .ssc_inf_terms(mem)
  for (f in c("est", "se", "lo", "hi", "p")) {
    expect_equal(st$coef[[f]][!inf], mem$coef[[f]][!inf], tolerance = 1e-8, info = paste(info, f))
    expect_equal(st$coef[[f]][inf], mem$coef[[f]][inf], tolerance = 1e-6, info = paste(info, f, "infinite"))
  }
  for (f in c("aliased", "n", "n_total", "events", "level", "se_type", "n_clusters", "ties", "strata", "levels",
              "warnings", "km_by")) {
    expect_identical(st[[f]], mem[[f]], info = paste(info, f))
  }
  for (k in c("km", "km_risk")) {
    expect_identical(names(st[[k]]), names(mem[[k]]), info = paste(info, k))
    expect_identical(nrow(st[[k]]), nrow(mem[[k]]), info = paste(info, k))
  }
  for (f in c("strata", "time", "n.risk", "n.event", "n.censor")) {
    expect_identical(st$km[[f]], mem$km[[f]], info = paste(info, "km", f))
  }
  for (f in c("surv", "lower", "upper")) {
    expect_equal(st$km[[f]], mem$km[[f]], tolerance = 1e-12, info = paste(info, "km", f))
  }
  for (f in names(mem$km_risk)) expect_identical(st$km_risk[[f]], mem$km_risk[[f]], info = paste(info, "km_risk", f))
}

# Two streamed values of the same spec at different chunk sizes agree to 1e-12 (and their counts exactly).
.ssc_surv_chunks <- function(a, b, info = "") {
  inf <- .ssc_inf_terms(b)
  for (f in c("est", "se", "lo", "hi", "p")) {
    expect_equal(a$coef[[f]][!inf], b$coef[[f]][!inf], tolerance = 1e-12, info = paste(info, f))
    expect_equal(a$coef[[f]][inf], b$coef[[f]][inf], tolerance = 1e-6, info = paste(info, f, "infinite"))
  }
  for (f in c("aliased", "n", "n_total", "events", "warnings", "iter", "passes")) {
    expect_identical(a[[f]], b[[f]], info = paste(info, f))
  }
}
