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

test_that("a second Edit keeps a terms row change even though the row count is not re-sent", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .ms_spy_modal()
  .fx_writer_routes()
  sp <- .ui_bin_spec()
  sp$terms <- list(list(var = "age", type = "linear"))
  p <- sr_sap_add(.fx_ui_project(), "c1", "logistic", sp, "t", label = "primary")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    # first Edit: the browser reports the rendered rows, then the dialog is saved unchanged
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(edit = 1)
    session$setInputs(an_table = "t", an_route = "logistic", an_label = "primary")
    form_r()
    session$setInputs(f_terms_n = 1, f_terms_var_1 = "age", f_terms_type_1 = "linear", f_terms_knots_1 = "")
    session$setInputs(an_save = 1)
    expect_equal(output$an_err, "")
    # second Edit of the same analysis: the count is unchanged so it is not re-sent; the user changes row 1
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(edit = 2)
    form_r()
    session$setInputs(f_terms_type_1 = "rcs", f_terms_knots_1 = "4")
    session$setInputs(an_save = 2)
    expect_equal(output$an_err, "")
    t1 <- .sr_sap_analyses(rv())$c1$spec$terms[[1]]
    expect_equal(t1$var, "age")
    expect_equal(t1$type, "rcs")
    expect_equal(as.numeric(t1$knots), 4)
  })
})

test_that("a second Edit keeps an interaction row change even though the row count is not re-sent", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .ms_spy_modal()
  .fx_writer_routes()
  sp <- .ui_bin_spec()
  sp$interactions <- list(c("grp", "age"))
  p <- sr_sap_add(.fx_ui_project(), "c1", "logistic", sp, "t", label = "primary")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(edit = 1)
    session$setInputs(an_table = "t", an_route = "logistic", an_label = "primary")
    form_r()
    session$setInputs(f_int_n = 1, f_int_a_1 = "grp", f_int_b_1 = "age")
    session$setInputs(an_save = 1)
    expect_equal(output$an_err, "")
    session$setInputs(analyses_rows_selected = 1L)
    session$setInputs(edit = 2)
    form_r()
    session$setInputs(f_int_b_1 = "z1")
    session$setInputs(an_save = 2)
    expect_equal(output$an_err, "")
    expect_equal(as.character(.sr_sap_analyses(rv())$c1$spec$interactions[[1]]), c("grp", "z1"))
  })
})

test_that("the renderer refuses a field kind it cannot render", {
  p <- .fx_ui_project()
  f <- sr_spec_form("logistic", "t", p)
  f$outcome$kind <- "weird"
  expect_error(.sr_ui_spec_inputs(f, shiny::NS("x")), "cannot render kind 'weird' (field 'outcome')", fixed = TRUE)
})

# ---- Task 7: feasibility, families and rules ---------------------------------------------------------

.ms_opts <- function(html) {
  m <- regmatches(html, gregexpr("<option value=\"[^\"]*\"", html))[[1]]
  sub("<option value=\"([^\"]*)\"", "\\1", m)
}

# A project with the logistic primary prim (and the secondary sec and sensitivity sens when `more`).
.ms_feas_project <- function(more = FALSE, env = parent.frame()) {
  .fx_writer_routes()
  p <- sr_sap_add(.fx_ui_project(env), "prim", "logistic", .ui_bin_spec(), "t", label = "primary")
  if (more) {
    p <- sr_sap_add(p, "sec", "logistic", .ui_bin_spec(), "t", label = "secondary")
    p <- sr_sap_add(p, "sens", "logistic", .ui_bin_spec(), "t", label = "sensitivity", parent = "prim")
  }
  p
}

test_that("assess offers only the analysis measures and saves", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .ms_spy_modal()
  rv <- shiny::reactiveVal(.ms_feas_project())
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(feas_rows_selected = 1L)
    session$setInputs(feas_assess = 1)
    html <- .ms_html(output$fa_form)
    expect_setequal(.ms_opts(html), c("OR", "RD", "RR"))
    expect_false(grepl("fa_r2|fa_icc", html))
    session$setInputs(fa_value = 0.05, fa_measure = "RD", fa_source = "pilot")
    session$setInputs(fa_ok = 1)
    expect_equal(output$fa_err, "")
    r <- sr_feasibility_report(rv())
    expect_true(nrow(r) > 0L)
    expect_true(all(is.na(r$stale)))
    expect_false(any(is.na(r$tier)))
    expect_equal(unique(r$delta), 0.05)
    expect_equal(unique(r$delta_measure), "RD")
    expect_equal(unique(r$delta_source), "pilot")
  })
})

test_that("assess refusal shows in dialog and keeps the inputs", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  .ms_spy_modal()
  .fx_writer_routes()
  sp <- .ui_bin_spec()
  sp$layers$clustered <- TRUE
  sp$se <- "cluster"
  sp$cluster <- "stage"
  p <- tryCatch(sr_sap_add(.fx_ui_project(), "cl1", "logistic", sp, "t", label = "primary"),
                error = function(e) e)
  if (inherits(p, "error")) fail(paste("clustered fixture refused:", conditionMessage(p)))
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(feas_rows_selected = 1L)
    session$setInputs(feas_assess = 1)
    expect_match(.ms_html(output$fa_form), "fa_icc")
    n0 <- .ms_n_events(rv())
    core <- tryCatch(sr_feasibility_assess(rv(), "cl1"), error = function(e) conditionMessage(e))
    expect_match(core, "a clustered analysis needs assumptions$icc", fixed = TRUE)
    session$setInputs(fa_value = 0.05, fa_measure = "RD", fa_source = "pilot")
    session$setInputs(fa_ok = 1)
    expect_equal(output$fa_err, core)
    expect_equal(.ms_n_events(rv()), n0)
    expect_equal(input$fa_source, "pilot")
    session$setInputs(fa_icc = 0.05, fa_icc_why = "pilot data")
    session$setInputs(fa_ok = 2)
    expect_equal(output$fa_err, "")
    expect_equal(.ms_n_events(rv()), n0 + 1L)
    expect_equal(sr_feasibility_report(rv())$analysis[1L], "cl1")
  })
})

test_that("assessment fields: r2 and icc blocks, disabled and maxlength", {
  f <- .sr_sap_assess_fields(shiny::NS("x"), c("OR"), show_r2 = TRUE, show_icc = FALSE, disabled = FALSE)
  expect_match(as.character(f), "x-fa_r2_why")
  expect_false(grepl("fa_icc", as.character(f)))
  f2 <- .sr_sap_assess_fields(shiny::NS("x"), c("OR"), show_r2 = FALSE, show_icc = TRUE, disabled = TRUE)
  expect_match(as.character(f2), "x-fa_icc_why")
  expect_match(as.character(f2), "<textarea[^>]*disabled")
  expect_match(as.character(f2), "<textarea[^>]*maxlength")
  expect_false(grepl("fa_r2", as.character(f2)))
})

test_that("stop needs a second confirmation", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  modal <- .ms_spy_modal()
  p <- sr_feasibility_assess(.ms_feas_project(), "prim")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(feas_rows_selected = 1L)
    session$setInputs(feas_decide = 1)
    n0 <- .ms_n_events(rv())
    session$setInputs(fd_decision = "stop", fd_reason = "not powered")
    session$setInputs(fd_ok = 1)
    expect_equal(.ms_n_events(rv()), n0)
    expect_null(rv()$state$feasibility$prim$decision)
    expect_match(modal$html[length(modal$html)], "stop blocks every run of this analysis")
    session$setInputs(fd_stop_ok = 1)
    expect_equal(rv()$state$feasibility$prim$decision$decision, "stop")
    expect_equal(rv()$state$feasibility$prim$decision$reason, "not powered")
    expect_equal(.ms_n_events(rv()), n0 + 1L)
  })
})

test_that("a refused decision shows the core message; a refused stop returns to the decision dialog", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  modal <- .ms_spy_modal()
  rv <- shiny::reactiveVal(.ms_feas_project())   # not assessed: the core refuses any decision
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(feas_rows_selected = 1L)
    session$setInputs(feas_decide = 1)
    session$setInputs(fd_decision = "stop", fd_reason = "why")
    session$setInputs(fd_ok = 1)
    n_modal <- length(modal$html)
    session$setInputs(fd_stop_ok = 1)
    expect_match(output$fd_err, "has no feasibility assessment")
    expect_length(modal$html, n_modal + 1L)
    expect_match(modal$html[n_modal + 1L], "why")
    session$setInputs(fd_decision = "revise", fd_reason = "r")
    session$setInputs(fd_ok = 2)
    expect_match(output$fd_err, "has no feasibility assessment")
  })
})

test_that("family and rule save; the rule alternative is limited to the analysis sensitivities", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.ms_feas_project(more = TRUE))
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    expect_equal(.ms_opts(.ms_html(output$fam_form))[1:2], c("prim", "sec"))
    expect_false("sens" %in% .ms_opts(.ms_html(output$fam_form)))
    session$setInputs(fam_id = "f1", fam_members = c("prim", "sec"), fam_method = "holm")
    session$setInputs(fam_save = 1)
    expect_equal(output$fam_err, "")
    expect_equal(unlist(rv()$state$sap$families$f1$members), c("prim", "sec"))
    expect_equal(rv()$state$sap$families$f1$method, "holm")
    session$setInputs(rule_analysis = "prim")
    expect_equal(.ms_opts(.ms_html(output$rule_alt_ui)), "sens")
    session$setInputs(rule_analysis = "sec")
    html <- .ms_html(output$rule_alt_ui)
    expect_length(.ms_opts(html), 0L)
    expect_match(html, "add a sensitivity analysis with this parent first")
    session$setInputs(rule_analysis = "prim", rule_alt = "sens", rule_id = "r1", rule_check = "separation",
                      rule_action = "add_sensitivity")
    session$setInputs(rule_save = 1)
    expect_equal(output$rule_err, "")
    r <- rv()$state$sap$rules$r1
    expect_equal(c(r$analysis, r$check, r$action, r$alternative), c("prim", "separation", "add_sensitivity", "sens"))
    expect_equal(nrow(.sr_sap_rules_view(rv())), 1L)
    n0 <- .ms_n_events(rv())
    session$setInputs(rule_id = "r2", rule_check = "ph")
    session$setInputs(rule_analysis = "sec")
    session$setInputs(rule_save = 2)
    expect_match(output$rule_err, "alternative")
    expect_equal(.ms_n_events(rv()), n0)
    expect_equal(input$rule_id, "r2")
  })
})

test_that("a refused family keeps its inputs and shows the core message", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.ms_feas_project(more = TRUE))
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    n0 <- .ms_n_events(rv())
    session$setInputs(fam_id = "f1", fam_members = character(), fam_method = "holm")
    session$setInputs(fam_save = 1)
    expect_equal(output$fam_err, "members must be a non-empty character vector of analysis ids")
    expect_equal(input$fam_id, "f1")
    expect_equal(.ms_n_events(rv()), n0)
  })
})

test_that("typed family text survives project changes, and the form re-renders only on new choices", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.ms_feas_project(more = TRUE))
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(fam_id = "typed")
    h0 <- .ms_html(output$fam_form)
    rv(sr_sap_family(rv(), "other", "prim", "holm"))   # an event that changes no analysis
    session$setInputs(x = 1)
    expect_identical(h0, .ms_html(output$fam_form))
    rv(sr_sap_add(rv(), "extra", "logistic", .ui_bin_spec(), "t", label = "secondary"))
    session$setInputs(x = 2)
    h2 <- .ms_html(output$fam_form)
    expect_match(h2, "extra")
    expect_match(h2, "value=\"typed\"")
  })
})

test_that("read-only: feasibility, family and rule edits refused", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  modal <- .ms_spy_modal()
  p <- .ms_feas_project(more = TRUE)
  p$readonly <- TRUE
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    n0 <- .ms_n_events(rv())
    session$setInputs(feas_assess = 1)
    session$setInputs(feas_decide = 1)
    session$setInputs(feas_doc = 1)
    expect_equal(sum(grepl("read-only", seen$msg)), 3L)
    expect_false(any(grepl("select an analysis", seen$msg)))
    expect_length(modal$html, 0L)
    session$setInputs(fam_id = "f1", fam_members = "prim", fam_method = "holm", fam_save = 1)
    expect_match(output$fam_err, "read-only")
    session$setInputs(rule_id = "r1", rule_analysis = "prim", rule_check = "separation",
                      rule_action = "add_sensitivity", rule_alt = "sens", rule_save = 1)
    expect_match(output$rule_err, "read-only")
    fa_mode(list(id = "prim", measures = "OR", show_r2 = FALSE, show_icc = FALSE))
    session$setInputs(fa_ok = 1)
    expect_match(output$fa_err, "read-only")
    fd_id("prim")
    session$setInputs(fd_decision = "revise", fd_reason = "x", fd_ok = 1)
    expect_match(output$fd_err, "read-only")
    expect_equal(.ms_n_events(rv()), n0)
    expect_match(.ms_html(output$fam_form), "<input id=\"[^\"]*fam_id\"[^>]*disabled")
    expect_match(.ms_html(output$fam_form), "<select[^>]*disabled[^>]*id=\"[^\"]*fam_members\"")
    expect_match(.ms_html(output$fam_form), "<button[^>]*disabled[^>]*id=\"[^\"]*fam_save\"")
    expect_match(.ms_html(output$rule_form), "<input id=\"[^\"]*rule_id\"[^>]*disabled")
    expect_match(.ms_html(output$rule_alt_ui), "<button[^>]*disabled[^>]*id=\"[^\"]*rule_save\"")
    expect_match(.ms_html(output$feas_btns), "<button[^>]*disabled[^>]*id=\"[^\"]*feas_assess\"")
    expect_match(.ms_html(output$feas_btns), "<button[^>]*disabled[^>]*id=\"[^\"]*feas_doc\"")
  })
})

test_that("a locked SAP refuses feasibility, family and rule edits from every control", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  modal <- .ms_spy_modal()
  .fx_writer_routes()
  lm <- function(sp = list()) utils::modifyList(list(purpose = "explain-causal", outcome = "age", exposure = "grp",
                                                       exposure_ref = "A", layers = .fx_writer_layers(), seed = 42L), sp)
  p0 <- sr_sap_add(.fx_ui_project(), "prim", "lm", lm(), "t", label = "primary")
  p0 <- sr_sap_add(p0, "sec", "lm", lm(), "t", label = "secondary")
  p0 <- sr_sap_add(p0, "sens", "lm", lm(), "t", label = "sensitivity", parent = "prim")
  p0 <- sr_sap_family(p0, "f1", c("prim", "sec"), "holm")
  locked <- sr_sap_lock(.fx_feasible(p0))
  rv <- shiny::reactiveVal(locked)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    n0 <- .ms_n_events(rv())
    session$setInputs(feas_assess = 1, feas_decide = 1)
    expect_equal(sum(grepl("SAP is locked", seen$msg)), 2L)
    expect_length(modal$html, 0L)
    session$setInputs(fam_id = "f2", fam_members = "prim", fam_method = "holm", fam_save = 1)
    expect_match(output$fam_err, "SAP is locked")
    session$setInputs(rule_id = "r1", rule_analysis = "prim", rule_check = "separation",
                      rule_action = "add_sensitivity", rule_alt = "sens", rule_save = 1)
    expect_match(output$rule_err, "SAP is locked")
    expect_equal(.ms_n_events(rv()), n0)
    expect_match(.ms_html(output$rule_alt_ui), "<button[^>]*disabled[^>]*id=\"[^\"]*rule_save\"")
    expect_match(.ms_html(output$feas_btns), "<button[^>]*disabled[^>]*id=\"[^\"]*feas_decide\"")
    expect_false(grepl("<button[^>]*disabled[^>]*id=\"[^\"]*feas_doc\"", .ms_html(output$feas_btns)))
  })
})

test_that("the feasibility report is written through act and listed as an export", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  p <- sr_feasibility_assess(.ms_feas_project(), "prim")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(feas_doc = 1)
    ex <- .sr_ui_exports(rv(), "feasibility")
    expect_equal(nrow(ex), 1L)
    expect_true(file.exists(ex$path))
  })
})

test_that("the feasibility table lists unassessed analyses and keeps the selection across re-renders", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  p <- .ms_feas_project(more = TRUE)
  v <- .sr_sap_feas_view(p)
  expect_equal(v$analysis, c("prim", "sec", "sens"))
  expect_true(all(v$tier == "not assessed"))
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_sap_server, args = list(project = rv), {
    session$setInputs(feas_rows_selected = 2L)
    expect_equal(fsel(), "sec")
    session$setInputs(feas_rows_selected = integer())   # a re-render empties it: ignored
    expect_equal(fsel(), "sec")
    rv(sr_feasibility_assess(rv(), "prim"))
    session$setInputs(x = 1)
    expect_equal(fsel(), "sec")
    v2 <- .sr_sap_feas_view(rv())
    expect_false(any(v2$tier[v2$analysis == "prim"] == "not assessed"))
  })
})
