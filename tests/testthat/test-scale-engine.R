# Engine field, stream refusals and engine resolution (P1e.1 Task 1). Synthetic data only.

.sce_layers <- function(...) {
  L <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
            framework = "frequentist", clustered = FALSE)
  x <- list(...)
  for (nm in names(x)) L[[nm]] <- x[[nm]]
  L
}

.sce_spec <- function(route = "lm", ...) {
  base <- if (identical(route, "lm")) {
    .run_spec("yc", exposure = "sex", exposure_ref = "F")
  } else if (route %in% c("cox", "rmst")) {
    s <- .run_spec("died", exposure = "sex")
    s$outcome <- NULL
    s$time <- "time"
    s$status <- "died"
    if (identical(route, "rmst")) s$tau <- 100
    s
  } else if (identical(route, "table1")) {
    list(purpose = "describe", by = "sex", vars = c("x1", "grp"), seed = 1L)
  } else {
    .run_spec("y", exposure = "sex", exposure_ref = "F")
  }
  extra <- list(...)
  for (nm in names(extra)) base[[nm]] <- extra[[nm]]
  base
}

.sce_val <- function(spec, route = "lm") sr_spec_validate(spec, sr_route_get(route, 1L))
.sce_err <- function(v) paste(v$errors, collapse = " ")

test_that("engine field validates against SR_ENGINES", {
  expect_identical(SR_ENGINES, c("auto", "memory", "stream"))
  bad <- .sce_val(.sce_spec("lm", engine = "fast"))
  expect_false(bad$ok)
  expect_match(.sce_err(bad), "engine")
  for (e in SR_ENGINES) expect_true(.sce_val(.sce_spec("lm", engine = e))$ok, info = e)
  for (r in c("logistic", "logistic_describe", "modpois", "table1")) {
    s <- .sce_spec(if (r == "table1") "table1" else "glm", engine = "stream")
    if (r == "logistic_describe") s$purpose <- "describe"
    if (r == "modpois") s$se <- "robust"
    expect_true(.sce_val(s, r)$ok, info = r)
    s$engine <- "fast"
    v <- .sce_val(s, r)
    expect_false(v$ok, info = r)
    expect_match(.sce_err(v), "engine", info = r)
  }
})

test_that("cox and rmst accept the stream engine at validation (P1e.2)", {
  for (r in c("cox", "rmst")) {
    v <- .sce_val(.sce_spec(r, engine = "stream"), r)
    expect_true(v$ok, info = paste(r, .sce_err(v)))
    expect_false(grepl("arrives in P1e.2", .sce_err(v), fixed = TRUE), info = r)
    expect_true(.sce_val(.sce_spec(r, engine = "memory"), r)$ok, info = r)
    expect_true(.sce_val(.sce_spec(r, engine = "auto"), r)$ok, info = r)
  }
  expect_null(.sr_engine_refusal("cox", list(engine = "stream")))
  expect_null(.sr_engine_refusal("rmst", list()))
  expect_null(.sr_engine_refusal("lm", list(engine = "stream", layers = .sce_layers())))
})

test_that("stream refusals name their phase", {
  mi <- .sce_spec("lm", engine = "stream", layers = .sce_layers(missing = "mi"), mi = list(m = 5L))
  bt <- .sce_spec("lm", engine = "stream", layers = .sce_layers(resampling = "bootstrap"), boot = list(B = 200L))
  fi <- .sce_spec("glm", engine = "stream", penalty = "firth", marginal = FALSE)
  m_mi <- "multiple imputation at scale arrives in P1e.3"
  m_bt <- "the bag of little bootstraps arrives in P1e.3"
  m_fi <- "the Firth penalty at scale arrives in P1e.3"
  expect_match(.sce_err(.sce_val(mi, "lm")), m_mi, fixed = TRUE)
  expect_match(.sce_err(.sce_val(bt, "lm")), m_bt, fixed = TRUE)
  expect_match(.sce_err(.sce_val(fi, "logistic")), m_fi, fixed = TRUE)
  expect_identical(.sr_engine_refusal("lm", mi), m_mi)
  expect_identical(.sr_engine_refusal("lm", bt), m_bt)
  expect_identical(.sr_engine_refusal("logistic", fi), m_fi)
  # with engine = "auto" the spec stays valid (the refusal is applied when auto resolves to stream)
  mi$engine <- bt$engine <- fi$engine <- "auto"
  expect_true(.sce_val(mi, "lm")$ok)
  expect_true(.sce_val(bt, "lm")$ok)
  expect_true(.sce_val(fi, "logistic")$ok)
})

test_that("stream_chunk must be a whole number >= 1", {
  for (bad in list(0, 2.5, -1)) {
    v <- .sce_val(.sce_spec("lm", stream_chunk = bad), "lm")
    expect_false(v$ok, info = bad)
    expect_match(.sce_err(v), "stream_chunk must be a whole number >= 1", fixed = TRUE)
  }
  expect_true(.sce_val(.sce_spec("lm", stream_chunk = 7), "lm")$ok)
  expect_true(.sce_val(.sce_spec("table1", stream_chunk = 7L), "table1")$ok)
  expect_false(.sce_val(.sce_spec("table1", stream_chunk = 0), "table1")$ok)
})

test_that("sr_mem_estimate table1 counts data bytes only", {
  p <- .fx_ready_project(.sc_frame(2000L), "t", c(id = "id", y = "outcome", sex = "exposure"))
  cols <- c("id", "x1")
  e <- sr_mem_estimate(p, "t", cols, "table1", override_rows = 1e6)
  ty <- sr_profile_table(p, "t")$columns
  by <- vapply(cols, function(cn) .sr_mem_bytes(ty$type[ty$column == cn], 16), numeric(1))
  expect_equal(e$need_gb * 1024^3, 1e6 * sum(by), tolerance = 1e-9)
  expect_equal(e$need_gb, e$rows * sum(by) / 1024^3, tolerance = 1e-9)
  expect_identical(e$route, "in_memory")
  big <- sr_mem_estimate(p, "t", cols, "table1", override_rows = 1e10)
  expect_false(big$fits)
  expect_identical(big$route, "stream")
  expect_identical(sr_mem_estimate(p, "t", cols, "lm", override_rows = 1e10)$route, "stream")
  expect_identical(sr_mem_estimate(p, "t", cols, "glm", override_rows = 1e10)$route, "stream")
  expect_identical(.sr_mem_method("table1"), "table1")
})

test_that("an invalid RAM budget override warns once and falls back to the profile budget", {
  p <- .fx_ready_project(.sc_frame(200L), "t", c(id = "id", y = "outcome", sex = "exposure"))
  prof <- sr_profile_caps(p$meta$profile)$ram_budget_gb
  est <- function() sr_mem_estimate(p, "t", c("id", "x1"), "glm")
  for (bad in c("abc", "0", "-2", "NA")) {
    rm(list = ls(.sr_budget_warned, all.names = TRUE), envir = .sr_budget_warned)
    withr::local_envvar(SR_RAM_BUDGET_GB = bad)
    expect_warning(e <- est(), sprintf("ignoring SR_RAM_BUDGET_GB = \"%s\"", bad), fixed = TRUE)
    expect_equal(e$budget_gb, prof, info = bad)
    expect_no_warning(e2 <- est())   # once per value
    expect_equal(e2$budget_gb, prof, info = bad)
  }
  rm(list = ls(.sr_budget_warned, all.names = TRUE), envir = .sr_budget_warned)
  withr::local_envvar(SR_RAM_BUDGET_GB = "")
  withr::local_options(sr.ram_budget_gb = -1)
  expect_warning(e <- est(), "ignoring option sr.ram_budget_gb = \"-1\"", fixed = TRUE)
  expect_equal(e$budget_gb, prof)
  withr::local_options(sr.ram_budget_gb = 3)
  expect_no_warning(e <- est())
  expect_equal(e$budget_gb, 3)
})

test_that("auto resolves to memory when it fits and to stream when it does not", {
  p <- .sc_locked(.sce_spec("glm"))
  r <- sr_engine_resolve(p, "a1")
  expect_named(r, c("engine", "requested", "reason", "estimate", "block"))
  expect_identical(r$engine, "memory")
  expect_identical(r$requested, "auto")
  expect_null(r$block)
  expect_match(r$reason, "^auto -> memory: fits in memory")
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  r2 <- sr_engine_resolve(p, "a1")
  expect_identical(r2$engine, "stream")
  expect_null(r2$block)
  expect_match(r2$reason, "^auto -> stream: does not fit in memory")
})

test_that("explicit stream on data that fits resolves to stream", {
  p <- .sc_locked(.sce_spec("glm", engine = "stream"))
  r <- sr_engine_resolve(p, "a1")
  expect_identical(r$engine, "stream")
  expect_identical(r$requested, "stream")
  expect_true(r$estimate$fits)
  expect_match(r$reason, "^stream -> stream: fits in memory")
  # a spec argument overrides the state spec
  r2 <- sr_engine_resolve(p, "a1", spec = .sce_spec("glm", engine = "memory"))
  expect_identical(r2$engine, "memory")
})

test_that("memory that does not fit is a block, not a switch", {
  p <- .sc_locked(.sce_spec("glm", engine = "memory"))
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  r <- sr_engine_resolve(p, "a1")
  expect_true(is.na(r$engine))
  expect_match(r$block, "does not fit in memory", fixed = TRUE)
  expect_match(r$block, "^memory engine requested but the analysis does not fit in memory: ")
})

test_that("auto that must stream hits a refusal as a block", {
  s <- .sce_spec("lm", layers = .sce_layers(missing = "mi"), mi = list(m = 5L))
  p <- .sc_locked(s, route = "lm")
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  r <- sr_engine_resolve(p, "a1")
  expect_true(is.na(r$engine))
  expect_identical(r$block, "multiple imputation at scale arrives in P1e.3")
})
