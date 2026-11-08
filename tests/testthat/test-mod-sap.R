# Tests for the SAP panel module (ui/mod_sap.R), driven through shiny::testServer. Synthetic data only.

.ms_n_events <- function(p) length(sr_events_read(p$paths$events))
.ms_html <- function(x) paste(as.character(x$html %||% x), collapse = " ")

# Record the UI of every shiny::showModal() call (html text) for the calling test.
.ms_spy_modal <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$html <- character()
  testthat::local_mocked_bindings(
    showModal = function(ui, session = NULL) {
      seen$html <- c(seen$html, paste(as.character(ui), collapse = " "))
      invisible(NULL)
    }, .package = "shiny", .env = env)
  seen
}

.ms_with_a1 <- function(env = parent.frame()) {
  .fx_writer_routes()
  sr_sap_add(.fx_ui_project(env), "a1", "logistic", .ui_bin_spec(), "t", label = "primary")
}

# Open the dialog in add mode with a table / route chosen.
.ms_open_add <- function(session, id = "prim", route = "logistic") {
  session$setInputs(add = 1)
  session$setInputs(an_id = id, an_label = "primary", an_table = "t", an_route = route)
}

test_that("add builds a prefilled form and saves a valid analysis", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .fx_writer_routes()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    n0 <- .ms_n_events(rv())
    .ms_open_add(session)
    html <- .ms_html(output$an_form)
    expect_match(html, "from roles")
    expect_match(html, "f_layers_weights")
    session$setInputs(f_event_value = "yes")
    session$setInputs(an_save = 1)
    expect_equal(output$an_err, "")
    expect_true("prim" %in% names(.sr_sap_analyses(rv())))
    expect_equal(.sr_sap_analyses(rv())$prim$spec$outcome, "y")
    expect_equal(.ms_n_events(rv()), n0 + 1L)
  })
})

test_that("the route is preselected from the router's decision", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  modal <- .ms_spy_modal()
  .fx_writer_routes()
  p <- .fx_routed(sr_sap_add(.fx_ui_project(), "a1", "logistic", .ui_bin_spec(), "t", label = "primary"))
  rt <- sr_spec_form_route(p, "t")
  expect_false(is.na(rt))
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(add = 1)
    expect_match(modal$html[1L], sprintf("<option value=\"%s\" selected", rt))
  })
})

test_that("a refused add shows the core message and keeps inputs", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .fx_writer_routes()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    .ms_open_add(session)
    session$setInputs(f_event_value = "yes", f_outcome = "")
    n0 <- .ms_n_events(rv())
    f <- form_r()
    spec <- sr_spec_collect("logistic", .sr_sap_form_values(input, f$form))
    expect_null(spec$outcome)
    core <- tryCatch(sr_sap_add(rv(), "prim", "logistic", spec, "t", "primary"),
                     error = function(e) conditionMessage(e))
    expect_type(core, "character")
    expect_match(core, "outcome")
    session$setInputs(an_save = 1)
    expect_equal(output$an_err, core)
    expect_equal(input$an_id, "prim")
    expect_equal(input$f_event_value, "yes")
    expect_equal(.ms_n_events(rv()), n0)
    expect_false("prim" %in% names(.sr_sap_analyses(rv())))
    expect_false(is.null(mode()))   # the dialog stays open
  })
})

test_that("a bad id is refused before the core", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .fx_writer_routes()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    .ms_open_add(session, id = "Prim")
    session$setInputs(f_event_value = "yes")
    n0 <- .ms_n_events(rv())
    session$setInputs(an_save = 1)
    expect_equal(output$an_err, "id must match ^[a-z][a-z0-9_]{0,39}$")
    expect_equal(.ms_n_events(rv()), n0)
    # the same inputs with a valid id are accepted, so the id was the only problem
    session$setInputs(an_id = "prim", an_save = 2)
    expect_equal(output$an_err, "")
    expect_true("prim" %in% names(.sr_sap_analyses(rv())))
  })
})

test_that("remove asks, then removes", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  modal <- .ms_spy_modal()
  rv <- shiny::reactiveVal(.ms_with_a1())
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    n0 <- .ms_n_events(rv())
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(remove = 1)
    expect_match(modal$html[length(modal$html)], "Remove the pre-specified analysis 'a1'")
    expect_equal(.ms_n_events(rv()), n0)
    expect_true("a1" %in% names(.sr_sap_analyses(rv())))
    session$setInputs(remove_ok = 1)
    expect_false("a1" %in% names(.sr_sap_analyses(rv())))
    expect_equal(.ms_n_events(rv()), n0 + 1L)
  })
})

test_that("a refused remove shows the core message", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .ms_spy_modal()
  p <- .ms_with_a1()
  p <- sr_sap_add(p, "s1", "logistic", .ui_bin_spec(), "t", label = "sensitivity", parent = "a1")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    n0 <- .ms_n_events(rv())
    session$setInputs(analyses_rows_selected = match("a1", .sr_sap_table_view(rv())$id))
    session$setInputs(remove = 1, remove_ok = 1)
    expect_match(output$an_err, "name it as the parent")
    expect_equal(.ms_n_events(rv()), n0)
  })
})

test_that("read-only: every edit refused", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  modal <- .ms_spy_modal()
  p <- .ms_with_a1()
  p$readonly <- TRUE
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    n0 <- .ms_n_events(rv())
    # nothing selected: the refusal is the read-only one, not "select a row"
    session$setInputs(add = 1)
    session$setInputs(edit = 1)
    session$setInputs(remove = 1)
    expect_equal(sum(grepl("read-only", seen$msg)), 3L)
    expect_false(any(grepl("select an analysis", seen$msg)))
    expect_length(modal$html, 0L)
    # forced past the buttons: save and remove_ok are refused by the server too
    mode(list(kind = "add", id = "", rec = NULL, stamp = 99L))
    session$setInputs(an_id = "prim", an_label = "primary", an_table = "t", an_route = "logistic",
                      f_event_value = "yes")
    session$setInputs(an_save = 1)
    expect_match(output$an_err, "read-only")
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(remove_ok = 1)
    expect_match(output$an_err, "read-only")
    expect_equal(.ms_n_events(rv()), n0)
    expect_true("a1" %in% names(.sr_sap_analyses(rv())))
    expect_false("prim" %in% names(.sr_sap_analyses(rv())))
    expect_match(.ms_html(output$btns), "<button[^>]*disabled[^>]*id=\"[^\"]*add\"")
    expect_match(.ms_html(output$an_form), "<select id=\"[^\"]*f_layers_missing\"[^>]*disabled")
    expect_match(.ms_html(output$an_form), "<input id=\"[^\"]*f_seed\"[^>]*disabled")
  })
})

test_that("busy: add refused", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  modal <- .ms_spy_modal()
  rv <- shiny::reactiveVal(.ms_with_a1())
  busy <- shiny::reactiveVal(TRUE)
  shiny::testServer(mod_sap_server, args = list(project = rv, busy = busy), {
    n0 <- .ms_n_events(rv())
    session$setInputs(add = 1)
    expect_length(modal$html, 0L)
    expect_true(any(grepl("wait for the import", seen$msg)))
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(remove = 1, remove_ok = 1)
    expect_equal(.ms_n_events(rv()), n0)
    expect_true("a1" %in% names(.sr_sap_analyses(rv())))
  })
})

test_that("a locked SAP refuses a save with the SAP message and disables the form", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .fx_writer_routes()
  p0 <- sr_sap_add(.fx_ui_project(), "a1", "lm",
                   list(purpose = "explain-causal", outcome = "age", exposure = "grp", exposure_ref = "A",
                        layers = .fx_writer_layers(), seed = 42L), "t", label = "primary")
  rv <- shiny::reactiveVal(p0)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    .ms_open_add(session)
    session$setInputs(f_event_value = "yes")
    expect_false(grepl("<input id=\"[^\"]*f_seed\"[^>]*disabled", .ms_html(output$an_form)))
    locked <- sr_sap_lock(.fx_feasible(p0))
    expect_true(isTRUE(locked$state$sap$locked))
    rv(locked)
    session$setInputs(dummy = 1)
    n0 <- .ms_n_events(rv())
    session$setInputs(an_save = 1)
    expect_match(output$an_err, "SAP is locked")
    expect_equal(.ms_n_events(rv()), n0)
    expect_false("prim" %in% names(.sr_sap_analyses(rv())))
    expect_match(.ms_html(output$an_form), "<input id=\"[^\"]*f_seed\"[^>]*disabled")
    expect_match(.ms_html(output$btns), "<button[^>]*disabled[^>]*id=\"[^\"]*add\"")
    # a locked SAP with nothing selected still gives the SAP message, not "select a row"
    session$setInputs(analyses_rows_selected = NULL)
    session$setInputs(remove_ok = 1)
    expect_match(output$an_err, "SAP is locked")
  })
})

test_that("typed values survive project changes (the form is not re-rendered)", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .fx_writer_routes()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    .ms_open_add(session)
    .ms_html(output$an_form)
    session$setInputs(f_event_value = "yes", f_seed = 7)
    c0 <- .sr_sap_renders$an_form
    rv(sr_pico_set(rv(), .list = list(population = "adults", outcome = "y")))
    session$setInputs(dummy = 1)
    .ms_html(output$an_form)
    expect_equal(.sr_sap_renders$an_form, c0)
    expect_equal(input$f_event_value, "yes")
    expect_equal(input$f_seed, 7)
    # a layer change does rebuild it, and the typed values are laid over the new form
    session$setInputs(f_layers_missing = "mi")
    html <- .ms_html(output$an_form)
    expect_gt(.sr_sap_renders$an_form, c0)
    expect_match(html, "Number of imputations")
    expect_match(html, "value=\"yes\"")
    expect_match(html, "value=\"7\"")
  })
})

test_that("layers: P1-unrunnable values are disabled with the reason, flags are logical both ways", {
  p <- .fx_ui_project()
  f <- sr_spec_form("logistic", "t", p)
  html <- paste(as.character(.sr_ui_spec_inputs(f, shiny::NS("x"))), collapse = " ")
  expect_match(html, "<option value=\"ipw\" disabled>ipw \\(not runnable in P1\\)")
  expect_match(html, "<option value=\"TRUE\" disabled>yes \\(not runnable in P1\\)")
  expect_match(html, "<option value=\"FALSE\" selected>no</option>")
  expect_match(html, "id=\"x-f_layers_matching\"")
  dis <- paste(as.character(.sr_ui_spec_inputs(f, shiny::NS("x"), disabled = TRUE)), collapse = " ")
  expect_match(dis, "<select id=\"x-f_layers_missing\" class=\"form-select\" disabled")
  # the values go back as logicals
  inp <- shiny::reactiveValues(f_layers_matching = "FALSE", f_layers_clustered = "TRUE")
  v <- .sr_sap_form_values(inp, f)
  expect_identical(v$layers_matching, FALSE)
  expect_identical(v$layers_clustered, TRUE)
  expect_identical(sr_spec_collect("logistic", v)$layers$clustered, TRUE)
})

test_that("the terms and interactions editors use the fixed ids", {
  p <- .fx_ui_project()
  f <- sr_spec_form("logistic", "t", p)
  html <- paste(as.character(.sr_ui_spec_inputs(f, shiny::NS("x"))), collapse = " ")
  for (id in c("f_terms_n", "f_terms_var_1", "f_terms_type_1", "f_terms_knots_30", "f_int_n", "f_int_a_1",
               "f_int_b_30")) expect_match(html, paste0("id=\"x-", id, "\""), info = id)
  expect_match(html, "id=\"x-f_terms_n\"[^>]*min=\"0\"[^>]*max=\"30\"")
  # form values -> terms
  inp <- shiny::reactiveValues(f_terms_n = 2, f_terms_var_1 = "age", f_terms_type_1 = "rcs",
                               f_terms_knots_1 = "3", f_terms_var_2 = "z1", f_terms_type_2 = "linear",
                               f_terms_knots_2 = "", f_int_n = 1, f_int_a_1 = "grp", f_int_b_1 = "age")
  v <- .sr_sap_form_values(inp, f)
  expect_equal(v$terms[[1L]]$type, "rcs")
  expect_equal(length(v$terms), 2L)
  expect_equal(v$interactions[[1L]], c("grp", "age"))
  spec <- sr_spec_collect("logistic", v)
  expect_equal(spec$terms[[1L]]$knots, 3)
  expect_equal(spec$interactions, list(c("grp", "age")))
})

test_that("a route with a field the form cannot build shows the form's refusal", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .fx_writer_routes()
  rv <- shiny::reactiveVal(.fx_ui_project())
  env <- environment(.sr_form_route)
  old <- get("SR_FORM_KINDS", envir = env)
  assign("SR_FORM_KINDS", old[names(old) != "event_value"], envir = env)
  withr::defer(assign("SR_FORM_KINDS", old, envir = env))
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    .ms_open_add(session)
    expect_match(.ms_html(output$an_form), "the form cannot build field 'event_value'")
    n0 <- .ms_n_events(rv())
    session$setInputs(an_save = 1)
    expect_match(output$an_err, "the form cannot build field 'event_value'")
    expect_equal(.ms_n_events(rv()), n0)
  })
})

test_that("the dialog's id box and the free-text fields carry maxlength on the input", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  modal <- .ms_spy_modal()
  rv <- shiny::reactiveVal(.ms_with_a1())
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(add = 1)
    expect_match(modal$html[1L], "<input id=\"[^\"]*an_id\"[^>]*maxlength=\"40\"")
    expect_false(grepl("<div[^>]*maxlength", modal$html[1L]))
    .ms_open_add(session)
    expect_match(.ms_html(output$an_form), "<input id=\"[^\"]*f_event_value\"[^>]*maxlength=\"200\"")
  })
})

test_that("edit prefills from the stored spec, keeps the id and replaces the analysis", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  modal <- .ms_spy_modal()
  rv <- shiny::reactiveVal(.ms_with_a1())
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    n0 <- .ms_n_events(rv())
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(edit = 1)
    expect_match(modal$html[1L], "<input id=\"[^\"]*an_id\"[^>]*readonly")
    session$setInputs(an_id = "a1", an_label = "primary", an_table = "t", an_route = "logistic")
    html <- .ms_html(output$an_form)
    expect_match(html, "saved")
    expect_false(grepl("from roles", html))
    expect_match(html, "value=\"yes\"")
    session$setInputs(f_seed = 7)
    session$setInputs(an_id = "other")   # the id of an edit is the selected one, whatever the box says
    session$setInputs(an_save = 1)
    expect_equal(output$an_err, "")
    expect_equal(names(.sr_sap_analyses(rv())), "a1")
    expect_equal(.sr_sap_analyses(rv())$a1$spec$seed, 7)
    expect_equal(.sr_sap_analyses(rv())$a1$spec$event_value, "yes")
    expect_equal(.ms_n_events(rv()), n0 + 1L)
  })
})

test_that("the selection is held by analysis id and an empty selection event is ignored", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .ms_spy_modal()
  p <- sr_sap_add(.ms_with_a1(), "b2", "logistic", .ui_bin_spec(), "t", label = "secondary")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(analyses_rows_selected = 2L)
    expect_equal(sel(), "b2")
    rv(sr_pico_set(rv(), .list = list(population = "adults", outcome = "y")))   # the DT re-renders, selection arrives empty
    session$setInputs(analyses_rows_selected = integer())
    expect_equal(sel(), "b2")
    n0 <- .ms_n_events(rv())
    session$setInputs(remove = 1, remove_ok = 1)
    expect_false("b2" %in% names(.sr_sap_analyses(rv())))
    expect_true("a1" %in% names(.sr_sap_analyses(rv())))
    expect_equal(.ms_n_events(rv()), n0 + 1L)
  })
})

test_that("the analyses table shows the feasibility decision, the tier or not assessed", {
  .fx_writer_routes()
  p <- sr_sap_add(.fx_ui_project(), "a1", "lm",
                  list(purpose = "explain-causal", outcome = "age", exposure = "grp", exposure_ref = "A",
                       layers = .fx_writer_layers(), seed = 42L), "t", label = "primary")
  v <- .sr_sap_table_view(p)
  expect_equal(names(v), c("id", "label", "route", "table", "purpose", "feasibility"))
  expect_equal(v$feasibility, "not assessed")
  p <- .fx_feasible(p)
  expect_equal(.sr_sap_table_view(p)$feasibility, "proceed")
  expect_equal(nrow(.sr_sap_table_view(NULL)), 0L)
})

test_that("a value left over from an earlier dialog never reaches an Edit or an Add", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .ms_spy_modal()
  .fx_writer_routes()
  sp <- .ui_bin_spec()
  sp$layers$missing <- "mi"
  sp$mi <- list(m = 20L)
  p <- sr_sap_add(.fx_ui_project(), "c1", "logistic", sp, "t", label = "primary")
  expect_equal(.sr_sap_analyses(p)$c1$spec$mi$m, 20L)
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    # dialog A: multiple imputation with m = 5, cancelled
    session$setInputs(add = 1)
    session$setInputs(an_id = "a", an_label = "primary", an_table = "t", an_route = "logistic")
    session$setInputs(f_layers_missing = "mi")
    expect_true("mi_m" %in% names(form_r()$form))
    session$setInputs(f_mi_m = "5")
    # dialog B: back to complete cases, cancelled
    session$setInputs(add = 2)
    expect_false("mi_m" %in% names(form_r()$form))   # the first form of a new Add ignores dialog A's layers
    session$setInputs(f_layers_missing = "cc")
    # edit c1 (saved: mi, m = 20)
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(edit = 1)
    f <- form_r()
    expect_equal(f$form$layers_missing$value, "mi")
    expect_equal(f$form$mi_m$value, 20L)
    # the browser binds the rendered layer select and reports it; it does not re-send f_mi_m (equal to
    # what the input held before)
    session$setInputs(f_layers_missing = "mi")
    f <- form_r()
    expect_equal(f$form$mi_m$value, 20L)
    expect_equal(f$form$mi_m$source, "saved")
    expect_match(.ms_html(output$an_form), "saved")
    session$setInputs(an_save = 1)
    expect_equal(output$an_err, "")
    expect_equal(.sr_sap_analyses(rv())$c1$spec$mi$m, 20L)
    # a value typed in THIS dialog does count, even when it equals an old one
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(edit = 2)
    session$setInputs(f_layers_missing = "mi")
    session$setInputs(f_mi_m = "7")
    session$setInputs(f_mi_m = "5")
    session$setInputs(an_save = 2)
    expect_equal(.sr_sap_analyses(rv())$c1$spec$mi$m, 5)
  })
})

test_that("the renderer refuses a field kind it cannot render", {
  p <- .fx_ui_project()
  f <- sr_spec_form("logistic", "t", p)
  f$outcome$kind <- "weird"
  expect_error(.sr_ui_spec_inputs(f, shiny::NS("x")), "cannot render kind 'weird' (field 'outcome')", fixed = TRUE)
})
