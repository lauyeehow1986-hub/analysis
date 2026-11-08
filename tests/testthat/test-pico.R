# Tests for intake/pico.R: the event-sourced research question. Synthetic data only.

.pico_project <- function(env = parent.frame()) {
  lung <- survival::lung
  df <- data.frame(id = seq_len(nrow(lung)), time = lung$time, died = as.integer(lung$status == 2L),
                   sex = ifelse(lung$sex == 1L, "M", "F"), stringsAsFactors = FALSE)
  .fx_ready_project(df, "t", c(id = "id", time = "time", died = "outcome", sex = "exposure"), env = env)
}

# A locked project: a stub route, one primary analysis, feasibility decided, then the lock.
.pico_locked <- function(env = parent.frame()) {
  est <- sr_estimand(population = "adults", variables = "died", intercurrent = "none",
                     summary = "stub", level = "marginal")
  r <- sr_route(id = "stub_pico", version = 1L, family = "stub", purpose = "explain-causal", estimand = est,
                spec_schema = list(outcome = list(type = "chr", required = TRUE),
                                   exposure = list(type = "chr", required = FALSE)),
                compile = function(spec) "1", present = function(result, spec) list(text = "stub"))
  sr_route_register(r)
  withr::defer(rm(list = .sr_route_key("stub_pico", 1L), envir = .sr_routes), envir = env)
  p <- .pico_project(env = env)
  layers <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                 framework = "frequentist", clustered = FALSE)
  spec <- list(purpose = "explain-causal", outcome = "died", layers = layers)
  p <- sr_sap_add(p, "a1", "stub_pico", spec, "t", label = "primary")
  sr_sap_lock(.fx_feasible(p))
}

test_that("sr_pico_set records the question and sr_pico reads it back", {
  p <- .fx_project()
  expect_null(sr_pico(p))
  p <- sr_pico_set(p, population = "adults with heart failure", exposure = "SGLT2 inhibitor",
                   outcome = "hospitalisation")
  expect_true(is.na(sr_pico(p)$comparator))
  expect_identical(sr_pico(p)$exposure, "SGLT2 inhibitor")
  expect_true(is.character(sr_pico(p)$at) && nzchar(sr_pico(p)$at))
  ev <- sr_events_read(p$paths$events)
  expect_identical(ev[[length(ev)]]$type, "pico.set")
  dir <- p$paths$dir
  sr_project_close(p)
  p2 <- sr_project_open(dir)
  withr::defer(sr_project_close(p2))
  expect_identical(sr_pico(p2)$outcome, "hospitalisation")
  expect_true(is.na(sr_pico(p2)$comparator))
})

test_that("sr_pico_set validates its fields", {
  p <- .fx_project()
  expect_error(sr_pico_set(p, population = "a", outcome = "b", foo = "x"), "population, exposure")
  expect_error(sr_pico_set(p, population = "a"), "outcome")
  expect_error(sr_pico_set(p, population = "a", outcome = "b", aim = "prove"), "describe")
  expect_error(sr_pico_set(p, population = strrep("x", 2001L), outcome = "b"), "2000")
  p <- sr_pico_set(p, population = "  x  ", outcome = "y", aim = "predict", setting = "   ")
  expect_identical(sr_pico(p)$population, "x")
  expect_true(is.na(sr_pico(p)$setting))
  expect_identical(sr_pico(p)$aim, "predict")
  p <- sr_pico_set(p, .list = list(population = "a", outcome = "b"))
  expect_identical(sr_pico(p)$outcome, "b")
})

test_that("sr_pico_set replaces the whole question", {
  p <- .fx_project()
  p <- sr_pico_set(p, population = "a", exposure = "e", outcome = "o")
  p <- sr_pico_set(p, population = "a2", outcome = "o2")
  expect_true(is.na(sr_pico(p)$exposure))
  expect_identical(sr_pico(p)$population, "a2")
})

test_that("the question is frozen by the lock", {
  p <- .pico_locked()
  expect_error(sr_pico_set(p, population = "a", outcome = "o"), "frozen")
})

test_that(".sr_pico_text renders key: value lines", {
  p <- .fx_project()
  expect_identical(.sr_pico_text(p), character(0))
  p <- sr_pico_set(p, outcome = "death", population = "adults")
  expect_identical(.sr_pico_text(p), c("population: adults", "outcome: death"))
})
