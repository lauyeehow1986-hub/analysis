# P1e.2 Task 10: streamed survival end to end: lock with engine = "stream", sr_run, the writer, the archive
# replay on the Parquet extract, the deviations and the pre-P1e.2 reopen. Public data only (survival::lung).

.sv_layers <- function() {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = FALSE)
}

.sv_spec <- function(route = "cox", ...) {
  s <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
            layers = .sv_layers(), seed = 7L)
  if (identical(route, "rmst")) s$tau <- 500 else s$terms <- list(list(var = "age"))
  x <- list(...)
  for (nm in names(x)) s[nm] <- list(x[[nm]])
  s[!vapply(s, is.null, logical(1))]
}

.sv_frame <- function(times = 1L) {
  d <- survival::lung[rep(seq_len(nrow(survival::lung)), times), ]
  d$id <- seq_len(nrow(d))
  d
}

# A locked project with analysis a1 of `route` over lung.
.sv_locked <- function(spec, route = "cox", env = parent.frame(), times = 1L) {
  .ssc_route(route)
  p <- .fx_ready_project(.sv_frame(times), "t", c(id = "id", status = "outcome"), env = env)
  p <- sr_sap_add(p, "a1", route, spec, "t", label = "primary")
  sr_sap_lock(.fx_feasible(p))
}

.sv_run <- function(spec, route = "cox", env = parent.frame()) {
  withr::local_options(sr.stream_chunk_rows = 60, .local_envir = env)
  p <- .sv_locked(spec, route, env)
  p <- sr_run(p, "a1")
  expect_true(isTRUE(p$state$runs$a1$ok), info = p$state$runs$a1$error)
  p
}

.sv_methods_tpl <- function(m, id = "a1") {
  b <- Filter(function(b) identical(b$type, "para") && identical(b$id, paste0("methods_", id)), m$main)
  b[[1L]]$text
}

# The Methods text of `id` through placeholders: no raw digits, resolved text, every placeholder verified and
# the claim guard clean. Returns the resolved text.
.sv_methods <- function(p, id = "a1") {
  m <- sr_doc_model(p)
  tpl <- .sv_methods_tpl(m, id)
  expect_identical(sr_ph_digits(tpl), character(0))
  txt <- sr_ph_resolve(tpl, m$reg, cite = sr_cite_fn(m$main[[length(m$main)]]$keys))
  v <- sr_ph_verify(m$reg, p)
  expect_true(all(v$ok), info = paste(v$source[!v$ok], v$got[!v$ok], collapse = "; "))
  g <- sr_claim_guard(m)
  expect_identical(nrow(g), 0L, info = paste(g$block, g$rule, g$detail, collapse = "\n"))
  expect_true(any(grepl(paste0("^fragment:", id, ":engine"), sr_ph_table(m$reg)$source)))
  txt
}

# Source the exported analysis.R in a fresh R process; returns sr_results.
.sv_rerun <- function(script, dir) {
  f <- tempfile("analysis-", fileext = ".R")
  on.exit(unlink(f), add = TRUE)
  writeLines(script, f, useBytes = TRUE)
  callr::r(function(f, d) {
    Sys.setenv(SR_PROJECT_DIR = d)
    e <- new.env()
    sys.source(f, envir = e)
    e$sr_results
  }, args = list(f = f, d = dir), libpath = .libPaths(), user_profile = FALSE, wd = tempdir())
}

.sv_stored <- function(p, id) {
  readRDS(file.path(p$paths$fits, paste0(p$state$runs[[id]]$ok_key, ".rds")))$value
}

# The archive replay: analysis.R reads the Parquet extract and reproduces the stored value.
.sv_replay <- function(p, id = "a1") {
  expect_identical(p$state$runs[[id]]$ok_extract_kind, "parquet")
  s <- sr_analysis_script(p, id)
  expect_error(parse(text = s), NA)
  expect_false(any(charToRaw(s) > as.raw(127L)))
  expect_true(grepl(paste0("data/extracts/", p$state$runs[[id]]$ok_extract_sha, ".parquet"), s, fixed = TRUE))
  expect_false(grepl(".rds", s, fixed = TRUE))
  res <- .sv_rerun(s, p$paths$dir)
  st <- .sv_stored(p, id)
  expect_identical(res[[id]]$engine, "stream")
  expect_equal(res[[id]]$coef, st$coef, tolerance = 1e-12)
  invisible(res)
}

test_that("streamed cox runs end to end", {
  p <- .sv_run(.sv_spec(engine = "stream"), env = environment())
  expect_identical(p$state$runs$a1$ok_engine, "stream")
  expect_identical(p$state$runs$a1$ok_extract_kind, "parquet")
  frag <- sr_result(p, "a1")$fragments$engine
  expect_match(frag, "Newton-Raphson iteration on the partial likelihood", fixed = TRUE)
  # it agrees with the in-memory run
  pm <- .sv_run(.sv_spec(engine = "memory"), env = environment())
  es <- sr_result(p, "a1")$effects
  em <- sr_result(pm, "a1")$effects
  expect_identical(es$term, em$term)
  expect_equal(es[, c("est", "lo", "hi")], em[, c("est", "lo", "hi")], tolerance = 1e-8)
  expect_null(sr_result(pm, "a1")$fragments$engine)
  # the Methods text carries the S17 sentence lifted to placeholders; the claim guard passes
  txt <- .sv_methods(p)
  expect_match(txt, "fitted by streaming Newton-Raphson iteration on the partial likelihood over all 228 rows",
               fixed = TRUE)
  expect_match(txt, "in chunks of 60 rows", fixed = TRUE)
  .sv_replay(p)
})

test_that("streamed cox with pem runs end to end", {
  p <- .sv_run(.sv_spec(engine = "stream", cox_method = "pem", pem_cuts = list(rule = "quantile", k = 5)),
               env = environment())
  res <- sr_result(p, "a1")
  expect_match(res$fragments$engine, "streaming iteratively reweighted least squares", fixed = TRUE)
  expect_match(res$fragments$pem_method, "piecewise-exponential (Poisson) model", fixed = TRUE)
  txt <- .sv_methods(p)
  expect_match(txt, "fitted by streaming iteratively reweighted least squares", fixed = TRUE)
  expect_match(txt, "piecewise-exponential (Poisson) model", fixed = TRUE)
  expect_true(any(grepl("^fragment:a1:pem_method", sr_ph_table(sr_doc_model(p)$reg)$source)))
  pm <- .sv_run(.sv_spec(engine = "memory", cox_method = "pem", pem_cuts = list(rule = "quantile", k = 5)),
                env = environment())
  es <- res$effects
  em <- sr_result(pm, "a1")$effects
  expect_equal(es[, c("est", "lo", "hi")], em[, c("est", "lo", "hi")], tolerance = 1e-6)
  .sv_replay(p)
})

test_that("streamed rmst runs end to end", {
  p <- .sv_run(.sv_spec("rmst", engine = "stream"), route = "rmst", env = environment())
  res <- sr_result(p, "a1")
  expect_identical(p$state$runs$a1$ok_engine, "stream")
  expect_match(res$fragments$engine, "restricted mean survival times were computed exactly from Kaplan-Meier counts",
               fixed = TRUE)
  txt <- .sv_methods(p)
  expect_match(txt, "accumulated over all 228 rows in chunks of 60 rows", fixed = TRUE)
  pm <- .sv_run(.sv_spec("rmst", engine = "memory"), route = "rmst", env = environment())
  es <- res$effects
  em <- sr_result(pm, "a1")$effects
  expect_equal(es[, c("est", "lo", "hi")], em[, c("est", "lo", "hi")], tolerance = 1e-8)
  .sv_replay(p)
})

test_that("auto resolves survival routes by size", {
  for (rt in c("cox", "rmst")) {
    sp <- .sv_spec(rt)
    p0 <- .sv_locked(sp, rt, env = environment(), times = 8L)
    expect_identical(p0$state$sap$lock$engines$a1$engine, "memory", info = rt)
    withr::with_envvar(c(SR_RAM_BUDGET_GB = "0.0001"), {
      p1 <- .sv_locked(sp, rt, env = environment(), times = 8L)
    })
    expect_identical(p1$state$sap$lock$engines$a1$requested, "auto", info = rt)
    expect_identical(p1$state$sap$lock$engines$a1$engine, "stream", info = rt)
  }
  # and the auto-resolved stream run says why in the Methods fragment
  withr::local_options(sr.stream_chunk_rows = 60)
  withr::with_envvar(c(SR_RAM_BUDGET_GB = "0.0001"), {
    p <- .sv_locked(.sv_spec(), "cox", env = environment(), times = 8L)
    p <- sr_run(p, "a1")
  })
  expect_true(isTRUE(p$state$runs$a1$ok), info = p$state$runs$a1$error)
  expect_match(sr_result(p, "a1")$fragments$engine, "exceeded the in-memory budget", fixed = TRUE)
})

test_that("engine switch on cox is computational, cox_method amendment is methodological", {
  p <- .sv_locked(.sv_spec(), "cox", env = environment())
  h <- sr_sap_hash(p)
  p2 <- sr_engine_switch(p, "a1", "stream", "moved to locked16")
  expect_identical(sr_sap_hash(p2), h)
  d <- sr_deviations(p2)
  r <- d[d$kind == "computational", , drop = FALSE]
  expect_identical(nrow(r), 1L)
  expect_identical(r$reason, "engine changed from memory to stream: moved to locked16; estimator unchanged")
  # switching to PEM after the lock is a different estimator: it enters the SAP hash, so it cannot pass as a
  # computational change; the way to report it is a new analysis, which is a post-hoc or sensitivity deviation
  q <- p
  q$state$analyses$a1$spec$cox_method <- "pem"
  expect_false(identical(sr_sap_hash(q), h))
  p3 <- sr_analysis_add(p2, "s1", "cox", .sv_spec(cox_method = "pem"), "t", label = "sensitivity",
                        reason = "piecewise-exponential check of the Cox model")
  expect_identical(sr_sap_hash(p3), h)
  d3 <- sr_deviations(p3)
  expect_identical(d3$kind[d3$analysis == "s1"], "unplanned_sensitivity")
  expect_identical(sum(d3$kind == "computational"), 1L)
  expect_identical(p3$state$analyses$s1$spec$cox_method, "pem")
  # the locked analysis itself cannot be edited into PEM: sr_sap_add refuses after the lock
  expect_error(sr_sap_add(p, "a1", "cox", .sv_spec(cox_method = "pem"), "t", label = "primary"),
               "the SAP is locked; use sr_analysis_add", fixed = TRUE)
})

.sv_docx_text <- function(path) {
  s <- officer::docx_summary(officer::read_docx(path), remove_fields = TRUE)
  s$text[!is.na(s$text)]
}

test_that("the SAP text names the cox method only when the spec stores one", {
  pem <- .sv_locked(.sv_spec(cox_method = "pem", pem_cuts = list(rule = "quantile", k = 10)), "cox",
                    env = environment())
  b <- .sr_sd_analysis_blocks(pem$state$analyses$a1)
  tb <- b[[2L]]$data %||% b[[2L]]$df
  expect_true(is.data.frame(tb))
  expect_identical(tb$Value[tb$Field == "Method"],
                   "Hazard ratios were estimated by a piecewise-exponential Poisson model with 10 intervals (rule quantile).")
  pem <- sr_sap_docx(pem)
  txt <- .sv_docx_text(file.path(pem$paths$out, "sap", "SAP_v1.docx"))
  expect_true(any(grepl("piecewise-exponential Poisson model with 10 intervals (rule quantile)", txt, fixed = TRUE)))
  # no cox_method stored (the form never stores the default): no Method row, the table is as before
  ex <- .sv_locked(.sv_spec(), "cox", env = environment())
  b0 <- .sr_sd_analysis_blocks(ex$state$analyses$a1)
  tb0 <- b0[[2L]]$data %||% b0[[2L]]$df
  expect_false("Method" %in% tb0$Field)
  ex <- sr_sap_docx(ex)
  txt0 <- .sv_docx_text(file.path(ex$paths$out, "sap", "SAP_v1.docx"))
  expect_false("Method" %in% txt0)
  expect_false(any(grepl("Hazard ratios were estimated", txt0, fixed = TRUE)))
})

test_that("old cox project reopens unchanged", {
  # tests/testthat/fixtures/make_pre_p1e2_cox_lock.R, run on main before P1e.2: a locked project whose cox spec
  # has no cox_method. Its SAP hash and run key were printed on main and are pinned here.
  ev <- testthat::test_path("fixtures", "pre-p1e2-cox-lock", "events.jsonl")
  state <- sr_state_replay(sr_events_read(ev))
  p <- list(state = state)
  a <- state$analyses$a1
  expect_identical(a$route, "cox")
  expect_null(a$spec$cox_method)
  expect_null(a$spec$pem_cuts)
  expect_identical(state$sap$lock$sap_hash, "7ec92bf39ed1f44a90018e01d8d350024905bddf4fc282261a6c451c2d226ebb")
  expect_identical(sr_sap_hash(p), state$sap$lock$sap_hash)
  expect_identical(.sr_engine_effective(p, "a1"), "memory")
  s <- .sr_run_spec(p, "a1")
  expect_identical(s, .sr_spec_from_state(a$spec))
  expect_identical(.sr_cox_method_of(s), "exact")
  key <- sr_content_hash(strrep("ab", 32L), list(route = .sr_route_key(a$route, a$route_version), spec = s),
                         s$seed, "lib", "hw")
  expect_identical(key, "a060bcb3827132ceba8b925d59d9ec7c401ba29003e5d6540227cb6081d8cbc8")
})

test_that("diagnostics refuse streamed cox with the P1e.3 text", {
  p <- .sv_locked(.sv_spec(engine = "stream"), "cox", env = environment())
  expect_error(sr_diag_run(p, "a1"), "diagnostics for a streaming analysis arrive in P1e.3", fixed = TRUE)
  pr <- .sv_locked(.sv_spec("rmst", engine = "stream"), "rmst", env = environment())
  expect_error(sr_diag_run(pr, "a1"), "diagnostics for a streaming analysis arrive in P1e.3", fixed = TRUE)
})
