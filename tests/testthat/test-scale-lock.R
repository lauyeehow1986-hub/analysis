# Lock checklist engine rows, the lock record and the SAP hash part (P1e.1 Task 2). Synthetic data only.

.scl_spec <- function(route = "logistic", ...) {
  s <- .run_spec(if (identical(route, "lm")) "yc" else "y", exposure = "sex", exposure_ref = "F")
  x <- list(...)
  for (nm in names(x)) s[[nm]] <- x[[nm]]
  s
}

# A drafted (unlocked) project with a1 on `route`; `feasible` decides the primary analyses first.
.scl_draft <- function(spec = .scl_spec(), route = "logistic", feasible = TRUE, env = parent.frame()) {
  p <- .fx_ready_project(.sc_frame(2000L), "t", c(id = "id", y = "outcome", sex = "exposure"), env = env)
  p <- sr_sap_add(p, "a1", route, spec, "t", label = "primary")
  if (feasible) p <- .fx_feasible(p)
  p
}

.scl_row <- function(chk, pattern) chk[grepl(pattern, chk$detail), , drop = FALSE]

test_that("checklist shows an engine row per analysis", {
  p <- .scl_draft(feasible = FALSE)
  chk <- sr_sap_check(p)
  r <- chk[chk$item == "engine", , drop = FALSE]
  expect_identical(nrow(r), 1L)
  expect_identical(r$status, "ok")
  expect_match(r$detail, "^a1: in memory [(]auto -> memory: ")
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  r2 <- sr_sap_check(p)
  r2 <- r2[r2$item == "engine", , drop = FALSE]
  expect_identical(r2$status, "ok")
  expect_match(r2$detail, "^a1: stream engine [(]auto -> stream: ")
  expect_false(any(grepl("big-data engines arrive in P1e", c(chk$detail, sr_sap_check(p)$detail), fixed = TRUE)))
  expect_false("fits in memory" %in% chk$item)
})

test_that("memory request that does not fit blocks the lock", {
  p <- .scl_draft(.scl_spec(engine = "memory"))
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  chk <- sr_sap_check(p)
  r <- chk[chk$item == "engine", , drop = FALSE]
  expect_identical(r$status, "block")
  expect_match(r$detail, "^a1: memory engine requested but the analysis does not fit in memory: ")
  expect_error(sr_sap_lock(p), "memory engine requested")
})

test_that("auto resolving to stream with mi blocks, naming P1e.3", {
  s <- .scl_spec("lm", layers = list(missing = "mi", weights = "none", matching = FALSE, resampling = "none",
                                     framework = "frequentist", clustered = FALSE), mi = list(m = 5L))
  p <- .scl_draft(s, route = "lm")
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  chk <- sr_sap_check(p)
  r <- chk[chk$item == "engine", , drop = FALSE]
  expect_identical(r$status, "block")
  expect_match(r$detail, "^a1: multiple imputation at scale arrives in P1e.3")
  expect_match(r$detail, "P1e.3", fixed = TRUE)
  expect_error(sr_sap_lock(p), "P1e.3")
})

test_that("lock records engines and the hash part only for stream", {
  ps <- sr_sap_lock(.scl_draft(.scl_spec(engine = "stream")))
  pm <- sr_sap_lock(.scl_draft(.scl_spec(engine = "memory")))
  expect_identical(ps$state$sap$lock$engines$a1$engine, "stream")
  expect_identical(ps$state$sap$lock$engines$a1$requested, "stream")
  expect_match(ps$state$sap$lock$engines$a1$reason, "^stream -> stream: ")
  expect_identical(pm$state$sap$lock$engines$a1$engine, "memory")
  expect_false(identical(ps$state$sap$lock$sap_hash, pm$state$sap$lock$sap_hash))
  expect_identical(sr_sap_hash(ps), ps$state$sap$lock$sap_hash)
  expect_identical(sr_sap_hash(pm), pm$state$sap$lock$sap_hash)
  # the engines part exists for stream only: .sr_sap_engines freezes at the lock
  expect_identical(.sr_sap_engines(ps)$a1$engine, "stream")
  expect_identical(.sr_sap_engines(pm)$a1$engine, "memory")
  # a lock record without engines (a SAP locked before P1e) has no engines: nothing is resolved afresh
  ps2 <- ps
  ps2$state$sap$lock$engines <- NULL
  expect_length(.sr_sap_engines(ps2), 0L)
})

test_that("a project locked before P1e keeps its SAP hash", {
  ev <- testthat::test_path("fixtures", "pre-p1e-lock", "events.jsonl")
  state <- sr_state_replay(sr_events_read(ev))
  p <- list(state = state)
  expect_true(isTRUE(state$sap$locked))
  expect_null(state$sap$lock$engines)
  expect_identical(sr_sap_hash(p), state$sap$lock$sap_hash)
  expect_identical(state$sap$lock$sap_hash,
                   "bae149a153f485dcf72145e9140f5fb353563977c860a2d92e9fb99359d4b1d7")
})

test_that("analysis added after the lock records its engine and leaves the hash", {
  p <- sr_sap_lock(.scl_draft())
  h <- sr_sap_hash(p)
  p <- sr_analysis_add(p, "a9", "logistic", .scl_spec(), "t", label = "exploratory", reason = "check")
  e <- p$state$analyses$a9$engine_resolved
  expect_identical(e$engine, "memory")
  expect_identical(e$requested, "auto")
  expect_match(e$reason, "^auto -> memory: ")
  expect_identical(sr_sap_hash(p), h)
  # a stream request is recorded as such
  p <- sr_analysis_add(p, "a8", "logistic", .scl_spec(engine = "stream"), "t", label = "exploratory",
                       reason = "check")
  expect_identical(p$state$analyses$a8$engine_resolved$engine, "stream")
  expect_identical(sr_sap_hash(p), h)
  # a blocked resolution refuses the add with the block text
  withr::local_envvar(SR_RAM_BUDGET_GB = "0.0001")
  expect_error(sr_analysis_add(p, "a7", "logistic", .scl_spec(engine = "memory"), "t", label = "exploratory",
                               reason = "check"), "memory engine requested")
})
