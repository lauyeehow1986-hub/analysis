# sr_engine_switch: the engine change as a computational deviation (P1e.1 Task 3). Synthetic data only.

.scw_spec <- function(...) {
  s <- .run_spec("y", exposure = "sex", exposure_ref = "F")
  x <- list(...)
  for (nm in names(x)) s[[nm]] <- x[[nm]]
  s
}

test_that("switch writes the event, lists a computational deviation, keeps the hash", {
  p <- .sc_locked(.scw_spec(), env = environment())
  h <- sr_sap_hash(p)
  expect_identical(.sr_engine_effective(p, "a1"), "memory")
  p2 <- sr_engine_switch(p, "a1", "stream", "moved to locked16")
  evs <- sr_events_read(p2$paths$events)
  expect_identical(evs[[length(evs)]]$type, "engine.switched")
  expect_identical(.sr_engine_effective(p2, "a1"), "stream")
  d <- sr_deviations(p2)
  r <- d[d$kind == "computational", , drop = FALSE]
  expect_identical(nrow(r), 1L)
  expect_identical(r$reason, "engine changed from memory to stream: moved to locked16; estimator unchanged")
  expect_identical(r$analysis, "a1")
  expect_true(is.na(r$trigger))
  expect_identical(sr_sap_hash(p2), h)
})

test_that("switch refusals", {
  p <- .sc_locked(.scw_spec(), env = environment())
  ro <- p
  ro$readonly <- TRUE
  expect_error(sr_engine_switch(ro, "a1", "stream", "r"), "read-only")
  expect_error(sr_engine_switch(p, "nope", "stream", "r"), "unknown analysis")
  expect_error(sr_engine_switch(p, "a1", "auto", "r"), "engine must be \"memory\" or \"stream\"")
  expect_error(sr_engine_switch(p, "a1", "memory", "r"), "already uses the")
  expect_error(sr_engine_switch(p, "a1", "stream", ""), "a reason is required")
  expect_error(sr_engine_switch(p, "a1", "stream", NA_character_), "a reason is required")
  ps <- sr_engine_switch(p, "a1", "stream", "r")
  pm <- .sc_locked(.scw_spec(layers = list(missing = "mi", weights = "none", matching = FALSE,
                                           resampling = "none", framework = "frequentist", clustered = FALSE),
                             mi = list(m = 5L)), route = "lm", env = environment())
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  expect_error(sr_engine_switch(ps, "a1", "memory", "r"), "does not fit in memory")
  expect_error(sr_engine_switch(pm, "a1", "stream", "r"), "P1e.3")
})
