# The P1e.1 UI (Task 14): the engine field of the spec form, the Streaming badge, the progress line and the
# engine switch dialog. Synthetic data only.

# A minimal lm form fill (the layers are left out: the collect keeps what is given) plus extra values.
.sui_fill <- function(r, ...) c(list(outcome = "age", exposure = "grp", exposure_ref = "A", seed = "42"), list(...))

test_that("Engine select appears for the five routes and is limited for cox/rmst", {
  p <- .fx_ui_project()
  all3 <- c("Auto" = "auto", "In memory" = "memory", "Streaming" = "stream")
  for (r in c("lm", "logistic", "logistic_describe", "modpois", "table1")) {
    f <- sr_spec_form(r, "t", p)
    e <- f[["engine"]]
    expect_false(is.null(e), info = r)
    expect_identical(e$kind, "engine", info = r)
    expect_identical(e$choices, all3, info = r)
    expect_identical(e$value, "auto", info = r)
    expect_false("stream_chunk" %in% names(f), info = r)
  }
  for (r in c("cox", "rmst")) {
    e <- sr_spec_form(r, "t", p)[["engine"]]
    expect_identical(e$choices, c("Auto" = "auto", "In memory" = "memory"), info = r)
  }
  # the renderer offers exactly the choices, with the labels
  html <- paste(.sr_ui_spec_inputs(sr_spec_form("lm", "t", p)["engine"], shiny::NS("x")))
  expect_match(html, "value=\"stream\"", fixed = TRUE)
  expect_match(html, "Streaming", fixed = TRUE)
  html2 <- paste(.sr_ui_spec_inputs(sr_spec_form("cox", "t", p)["engine"], shiny::NS("x")))
  expect_false(grepl("stream", html2, fixed = TRUE))
  # collect: auto is the default and stays out of the spec (hash-stable); the others are kept
  expect_false("engine" %in% names(sr_spec_collect("lm", .sui_fill("lm", engine = "auto"))))
  expect_identical(sr_spec_collect("lm", .sui_fill("lm", engine = "stream"))$engine, "stream")
  expect_identical(sr_spec_collect("lm", .sui_fill("lm", engine = "memory"))$engine, "memory")
})

test_that("the SAP checklist engine rows render", {
  p <- .sc_locked(.run_spec("y", exposure = "sex", exposure_ref = "F"), env = environment())
  v <- .sr_sap_check_view(p)
  expect_true("engine" %in% v$item)
  expect_true(all(c("item", "status", "detail") %in% names(v)))
})

test_that("badge reads ok_engine", {
  mk <- function(oe) list(state = list(runs = list(a1 = list(ok_engine = oe), a2 = list(ok = TRUE))))
  expect_identical(.sr_an_engine_badge(mk("stream"), "a1"), "Streaming")
  expect_identical(.sr_an_engine_badge(mk("memory"), "a1"), "")
  expect_identical(.sr_an_engine_badge(mk(NA_character_), "a1"), "")
  expect_identical(.sr_an_engine_badge(mk("stream"), "a2"), "")
  expect_identical(.sr_an_engine_badge(mk("stream"), "nope"), "")
  expect_identical(.sr_an_engine_badge(list(state = list()), "a1"), "")
})

test_that("progress line reads progress.json", {
  f <- withr::local_tempfile(fileext = ".json")
  writeLines("{\"pass\": 4, \"max\": 29}", f)
  expect_identical(.sr_an_progress_text(f), "pass 4 of at most 29")
  # missing, empty, partial and invalid files give "" and never an error
  expect_identical(.sr_an_progress_text(file.path(dirname(f), "nope.json")), "")
  writeLines(character(), f)
  expect_identical(.sr_an_progress_text(f), "")
  writeLines("{\"pass\": 4, \"ma", f)
  expect_identical(.sr_an_progress_text(f), "")
  writeLines("not json at all", f)
  expect_identical(.sr_an_progress_text(f), "")
  writeLines("{\"pass\": \"x\", \"max\": null}", f)
  expect_identical(.sr_an_progress_text(f), "")
  writeLines("[1, 2]", f)
  expect_identical(.sr_an_progress_text(f), "")
  expect_identical(.sr_an_progress_text(NULL), "")
  expect_identical(.sr_an_progress_text(NA_character_), "")
  expect_identical(.sr_an_progress_text(character()), "")
})

# a locked project whose analysis a1 has a failed run that names sr_engine_switch
.sui_refused_project <- function(env = parent.frame()) {
  p <- .sc_locked(.run_spec("y", exposure = "sex", exposure_ref = "F"), env = env)
  p$state$runs[["a1"]] <- list(
    status = "failed", ok = FALSE, job = "j1",
    error = paste0("memory engine requested but the analysis does not fit in memory; ",
                   "it can be switched to the streaming engine with sr_engine_switch()"))
  p
}

test_that("switch action shown after a memory refusal", {
  rv <- shiny::reactiveVal(.sui_refused_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    expect_match(paste(output$result_head$html), "switch_engine", fixed = TRUE)
  })
  # a failed run with another error shows no switch action
  p <- shiny::isolate(rv())
  p$state$runs[["a1"]]$error <- "something else failed"
  rv2 <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv2, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    expect_false(grepl("switch_engine", paste(output$result_head$html), fixed = TRUE))
  })
})

test_that("switch dialog requires a reason and calls sr_engine_switch", {
  p0 <- .sui_refused_project()
  rv <- shiny::reactiveVal(p0)
  n_events <- function() length(sr_events_read(shiny::isolate(rv())$paths$events))
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    session$setInputs(switch_engine = 1)
    n0 <- n_events()
    # an empty (or blank) reason: the error is shown and no event is appended
    session$setInputs(sw_engine = "stream", sw_reason = "   ", sw_ok = 1)
    expect_match(output$sw_err, "reason", ignore.case = TRUE)
    expect_identical(n_events(), n0)
    expect_null(rv()$state$engines[["a1"]])
    # a reason: engine.switched is appended through sr_engine_switch
    session$setInputs(sw_reason = "moved to the locked16 profile", sw_ok = 2)
    evs <- sr_events_read(rv()$paths$events)
    expect_identical(evs[[length(evs)]]$type, "engine.switched")
    expect_identical(n_events(), n0 + 1L)
    expect_identical(rv()$state$engines[["a1"]]$to, "stream")
    expect_identical(rv()$state$engines[["a1"]]$reason, "moved to the locked16 profile")
  })
})
