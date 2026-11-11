# sr_run on the effective engine: run spec, dispatch, run metadata, progress, the engine Methods fragment
# and the diagnostics refusal (P1e.1 Task 11, rulings R4 to R7 and R15 to R17). Synthetic data only.

.srr_layers <- function() {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = FALSE)
}

# A logistic spec with a factor term and robust SEs; extra fields from `...` (engine, ...).
.srr_spec <- function(...) {
  s <- list(purpose = "explain-causal", outcome = "y", exposure = "sex", exposure_ref = "F",
            terms = list(list(var = "x1"), list(var = "grp", type = "factor")),
            layers = .srr_layers(), seed = 42L, se = "robust")
  x <- list(...)
  for (nm in names(x)) s[[nm]] <- x[[nm]]
  s
}

.srr_events <- function(p) sr_events_read(p$paths$events)
.srr_last <- function(p, type) {
  ev <- Filter(function(e) identical(e$type, type), .srr_events(p))
  ev[[length(ev)]]
}

test_that("stream run equals memory run through sr_run", {
  withr::local_options(sr.stream_chunk_rows = 1000)
  df <- .sc_frame(3000L)
  pm <- .sc_locked(.srr_spec(engine = "memory"), env = environment(), df = df)
  ps <- .sc_locked(.srr_spec(engine = "stream"), env = environment(), df = df)
  pm <- sr_run(pm, "a1")
  ps <- sr_run(ps, "a1")
  expect_true(isTRUE(pm$state$runs$a1$ok), info = pm$state$runs$a1$error)
  expect_true(isTRUE(ps$state$runs$a1$ok), info = ps$state$runs$a1$error)
  em <- sr_result(pm, "a1")$effects
  es <- sr_result(ps, "a1")$effects
  expect_identical(es$term, em$term)
  expect_equal(es[, c("est", "lo", "hi")], em[, c("est", "lo", "hi")], tolerance = 1e-7)
  expect_identical(ps$state$runs$a1$ok_engine, "stream")
  expect_identical(ps$state$runs$a1$ok_extract_kind, "parquet")
  expect_equal(ps$state$runs$a1$ok_chunk, 1000)
  expect_identical(pm$state$runs$a1$ok_engine, "memory")
  expect_identical(pm$state$runs$a1$ok_extract_kind, "rds")
  expect_null(pm$state$runs$a1$ok_chunk)
  rc <- .srr_last(ps, "run.completed")$payload
  expect_identical(rc$engine, "stream")
  expect_identical(rc$extract_kind, "parquet")
  expect_equal(rc$chunk, 1000)
  expect_true(file.exists(file.path(ps$paths$dir, "data", "extracts", paste0(rc$extract_sha, ".parquet"))))
})

test_that("second stream run is a cache hit with an identical value", {
  withr::local_options(sr.stream_chunk_rows = 1000)
  p <- .sc_locked(.srr_spec(engine = "stream"), env = environment())
  p <- sr_run(p, "a1")
  expect_true(isTRUE(p$state$runs$a1$ok), info = p$state$runs$a1$error)
  k1 <- p$state$runs$a1$ok_key
  v1 <- readRDS(file.path(p$paths$fits, paste0(k1, ".rds")))$value
  p <- sr_run(p, "a1")
  expect_true(isTRUE(p$state$runs$a1$cached))
  expect_identical(p$state$runs$a1$ok_key, k1)
  v2 <- readRDS(file.path(p$paths$fits, paste0(p$state$runs$a1$ok_key, ".rds")))$value
  expect_true(identical(serialize(v1, NULL), serialize(v2, NULL)))
  expect_identical(v2$engine, "stream")
  expect_equal(v2$chunk, 1000)
})

test_that("old spec without engine keeps its run key", {
  p <- .sc_locked(.srr_spec(), env = environment())
  a <- p$state$analyses$a1
  expect_false("engine" %in% names(a$spec))
  expect_false("stream_chunk" %in% names(a$spec))
  p <- sr_run(p, "a1")
  expect_true(isTRUE(p$state$runs$a1$ok), info = p$state$runs$a1$error)
  st <- .srr_last(p, "run.started")$payload
  ex <- sr_extract(p, a$table, sr_spec_cols(.sr_spec_from_state(a$spec)))
  expect_identical(st$extract_sha, ex$sha)
  key <- sr_content_hash(ex$sha, list(route = .sr_route_key(a$route, a$route_version),
                                      spec = .sr_spec_from_state(a$spec)),
                         .sr_spec_from_state(a$spec)$seed, sr_libsnap_hash(), sr_hw_fingerprint())
  expect_identical(st$key, key)
  # the memory run spec of an analysis whose spec says "auto" or "memory" hashes like the spec without it
  for (e in c("auto", "memory")) {
    q <- p
    q$state$analyses$a1$spec$engine <- e
    q$state$analyses$a1$spec$stream_chunk <- 500L
    expect_identical(.sr_run_spec(q, "a1"), .sr_spec_from_state(a$spec))
  }
})

test_that("a project locked before P1e has the same run spec and key (fixture)", {
  ev <- testthat::test_path("fixtures", "pre-p1e-lock", "events.jsonl")
  state <- sr_state_replay(sr_events_read(ev))
  p <- list(state = state)
  ids <- names(state$analyses)
  expect_true(length(ids) >= 1L)
  for (id in ids) {
    a <- state$analyses[[id]]
    expect_null(a$spec$engine)
    expect_identical(.sr_engine_effective(p, id), "memory")
    s <- .sr_run_spec(p, id)
    expect_identical(s, .sr_spec_from_state(a$spec))
    sha <- strrep("ab", 32L)
    rk <- .sr_route_key(a$route, a$route_version)
    expect_identical(sr_content_hash(sha, list(route = rk, spec = s), s$seed, "lib", "hw"),
                     sr_content_hash(sha, list(route = rk, spec = .sr_spec_from_state(a$spec)), s$seed,
                                     "lib", "hw"))
  }
})

test_that(".sr_run_spec sets engine and chunk for stream", {
  withr::local_options(sr.stream_chunk_rows = 1234)
  p <- .sc_locked(.srr_spec(engine = "stream"), env = environment())
  s <- .sr_run_spec(p, "a1")
  expect_identical(s$engine, "stream")
  expect_identical(s$stream_chunk, 1234L)
  expect_identical(.sr_run_spec(p, "a1", chunk = 77)$stream_chunk, 77L)
  q <- p
  q$state$analyses$a1$spec$stream_chunk <- 300L
  expect_identical(.sr_run_spec(q, "a1")$stream_chunk, 300L)
  m <- .sr_run_spec(p, "a1", engine = "memory")
  expect_null(m$engine)
  expect_null(m$stream_chunk)
})

test_that("memory lock refuses after the budget shrinks, with the switch hint", {
  p <- .sc_locked(.srr_spec(), env = environment())
  n0 <- length(.srr_events(p))
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  expect_error(sr_run(p, "a1"),
               "analysis 'a1' does not fit in memory: .*; it can be switched to the streaming engine with sr_engine_switch\\(\\)")
  expect_identical(length(.srr_events(p)), n0)
  expect_false("run.started" %in% vapply(.srr_events(p), function(e) e$type, ""))
})

test_that("progress file reports passes", {
  withr::local_options(sr.stream_chunk_rows = 1000)
  p <- .sc_locked(.srr_spec(engine = "stream"), env = environment())
  pf <- .sr_progress_path(p, "jprog1")
  expect_identical(pf, file.path(p$paths$dir, "data", "tmp", "progress-jprog1.json"))
  p <- sr_run(p, "a1", job = "jprog1")
  expect_true(isTRUE(p$state$runs$a1$ok), info = p$state$runs$a1$error)
  expect_true(file.exists(pf))
  pg <- jsonlite::fromJSON(paste(readLines(pf, warn = FALSE), collapse = ""))
  expect_true(pg$pass >= 1L)
  expect_true(pg$pass <= pg$max)
})

test_that("stream engine adds the Methods fragment", {
  withr::local_options(sr.stream_chunk_rows = 1000)
  p <- .sc_locked(.srr_spec(engine = "stream"), env = environment())
  p <- sr_run(p, "a1")
  res <- sr_result(p, "a1")
  expect_match(res$fragments$engine, "fitted by streaming iteratively reweighted least squares", fixed = TRUE)
  expect_match(res$fragments$engine, "over fixed chunks of 1000 rows", fixed = TRUE)
  # the memory result has no engine fragment
  pm <- sr_run(.sc_locked(.srr_spec(), env = environment()), "a1")
  expect_null(sr_result(pm, "a1")$fragments$engine)
})

test_that("the engine fragment texts", {
  glm <- .sr_engine_fragment(list(n = 1234567L, chunk = 250000L), "glm")
  expect_identical(glm, paste(
    "The analysis dataset (1,234,567 rows) exceeded the in-memory budget, so the models were fitted by",
    "streaming iteratively reweighted least squares over fixed chunks of 250000 rows. This gives the same",
    "maximum-likelihood estimates and standard errors as standard software within numerical precision."))
  lm <- .sr_engine_fragment(list(n = 5000L, chunk = 1000L), "lm")
  expect_match(lm, "fitted by streaming least squares over fixed chunks of 1000 rows.", fixed = TRUE)
  expect_match(lm, "^The analysis dataset \\(5,000 rows\\)")
  t1 <- .sr_engine_fragment(list(n = c(10, 20, 30), n_excluded = 2L), "table1")
  expect_identical(t1, "Descriptive statistics were computed exactly by SQL aggregation over all 32 rows.")
  # sr_present appends it only for a stream value or a stream run spec
  est <- sr_estimand(population = "adults", variables = "y", intercurrent = "none", summary = "stub",
                     level = "marginal")
  r <- sr_route(id = "lm", version = 99L, family = "regression", purpose = "explain-causal", estimand = est,
                spec_schema = list(), compile = function(spec) "1",
                present = function(value, spec) list(tables = list(), figures = list(),
                                                     fragments = list(a = "x"), effects = sr_effects_empty()))
  expect_null(sr_present(r, list(n = 5L), list())$fragments$engine)
  expect_match(sr_present(r, list(n = 5L, engine = "stream", chunk = 2L), list())$fragments$engine,
               "least squares")
})

test_that("diagnostics refuse a stream analysis", {
  p <- .sc_locked(.srr_spec(engine = "stream"), env = environment())
  expect_error(sr_diag_run(p, "a1"), "diagnostics for a streaming analysis arrive in P1e.3", fixed = TRUE)
})

test_that("a stream Table 1 runs through sr_run on one thread with identical strings", {
  withr::local_options(sr.stream_chunk_rows = 1000)
  df <- .sc_frame(1500L)
  spec <- function(...) c(list(purpose = "describe", by = "sex", vars = c("x1", "x2", "grp"), nonnormal = "x2",
                               layers = .srr_layers(), seed = 42L), list(...))
  pm <- sr_run(.sc_locked(spec(), route = "table1", env = environment(), df = df), "a1")
  ps <- sr_run(.sc_locked(spec(engine = "stream"), route = "table1", env = environment(), df = df), "a1")
  expect_true(isTRUE(pm$state$runs$a1$ok), info = pm$state$runs$a1$error)
  expect_true(isTRUE(ps$state$runs$a1$ok), info = ps$state$runs$a1$error)
  expect_identical(ps$state$runs$a1$ok_engine, "stream")
  rm_ <- sr_result(pm, "a1")
  rs <- sr_result(ps, "a1")
  expect_identical(rs$tables$table1, rm_$tables$table1)
  expect_identical(rs$fragments$engine,
                   "Descriptive statistics were computed exactly by SQL aggregation over all 1,500 rows.")
  expect_null(rm_$fragments$engine)
})
