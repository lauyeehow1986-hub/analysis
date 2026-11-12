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
  if (isTRUE(merge)) return(sr_compile(.ssc_route(id), s))
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
