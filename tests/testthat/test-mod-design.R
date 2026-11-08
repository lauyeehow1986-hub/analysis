# Tests for the Design panel module (ui/mod_design.R), driven through shiny::testServer. Synthetic data only.

.md_n_events <- function(p) length(sr_events_read(p$paths$events))

# The 1-based row of the decision table with this node (and column, when given).
.md_row <- function(p, node, column = NULL) {
  tb <- sr_router_table(p)
  which(tb$node == node & (is.null(column) | tb$column %in% column))[1L]
}

# The rendered text of a renderUI output.
.md_html <- function(x) paste(as.character(x$html %||% x), collapse = " ")

test_that("PICO save writes one event", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    n0 <- .md_n_events(rv())
    session$setInputs(pico_population = "adults", pico_outcome = "y", pico_aim = "explain-causal", pico_save = 1)
    expect_equal(sr_pico(rv())$population, "adults")
    expect_equal(sr_pico(rv())$aim, "explain-causal")
    expect_equal(.md_n_events(rv()), n0 + 1L)
  })
})

test_that("run rules decides what rules can, with no backend", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    session$setInputs(pico_population = "adults", pico_outcome = "y", pico_aim = "explain-causal", pico_save = 1)
    session$setInputs(run_rules = 1)
    tb <- sr_router_table(rv())
    expect_true(any(tb$status == "decided" & tb$resolver == "rules"))
  })
})

test_that("an override without a reason is refused in place", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    session$setInputs(pico_population = "adults", pico_outcome = "y", pico_aim = "explain-causal", pico_save = 1)
    session$setInputs(run_rules = 1)
    i <- .md_row(rv(), "purpose")
    row <- sr_router_table(rv())[i, ]
    expect_false(is.na(row$suggested))
    other <- setdiff(sr_router_editor(rv(), "purpose")$options, row$suggested)[1L]
    n0 <- .md_n_events(rv())
    session$setInputs(table_rows_selected = i)
    session$setInputs(dec_choice = other, dec_reason = "", dec_save = 1)
    expect_match(output$dec_err, "needs a reason|override")
    expect_equal(input$dec_choice, other)
    expect_equal(.md_n_events(rv()), n0)
    session$setInputs(dec_reason = "my own reasons", dec_save = 2)
    expect_equal(output$dec_err, "")
    expect_equal(.md_n_events(rv()), n0 + 1L)
    expect_equal(sr_router_table(rv())$choice[i], other)
  })
})

test_that("a stale row says decide again", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    session$setInputs(pico_population = "adults", pico_outcome = "y", pico_aim = "explain-causal", pico_save = 1)
    session$setInputs(run_rules = 1)
    tb <- sr_router_table(rv())
    i <- .md_row(rv(), "outcome_col")
    expect_false(is.na(i))
    session$setInputs(table_rows_selected = i)
    ch <- sr_router_editor(rv(), "outcome_col", tb$table[i], tb$column[i])$options[1L]
    session$setInputs(dec_choice = ch, dec_reason = "because", dec_save = 1)
    expect_equal(sr_router_table(rv())$status[i], "decided")
    # change the purpose: outcome_col depends on it
    pi <- .md_row(rv(), "purpose")
    cur <- sr_router_table(rv())$choice[pi]
    other <- setdiff(sr_router_editor(rv(), "purpose")$options, cur)[1L]
    rv(sr_router_decide(rv(), "purpose", other, reason = "change"))
    expect_equal(sr_router_table(rv())$status[i], "stale")
    session$setInputs(table_rows_selected = i)
    expect_match(.md_html(output$editor), "decide again")
  })
})

test_that("read-only: every edit refused", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  p <- .fx_ui_project()
  p$readonly <- TRUE
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_design_server, args = list(project = rv), {
    n0 <- .md_n_events(rv())
    session$setInputs(table_rows_selected = 1L)
    session$setInputs(pico_population = "a", pico_outcome = "b", pico_save = 1, run_rules = 1, dec_save = 1)
    expect_equal(.md_n_events(rv()), n0)
    expect_equal(sum(grepl("read-only", seen$msg)), 3L)
  })
})

test_that("busy: edits refused", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv, busy = function() TRUE), {
    n0 <- .md_n_events(rv())
    session$setInputs(pico_population = "a", pico_outcome = "b", pico_save = 1)
    expect_equal(.md_n_events(rv()), n0)
    expect_true(any(grepl("wait for the import to finish", seen$msg)))
  })
})

test_that("a veto is caught, kept and shown in place; override_required offers the override", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  old <- sr_node_get("design")
  sr_node_register(sr_node("design", old$label, old$options, question = old$question, rule = old$rule,
                           applies = old$applies, facts = old$facts,
                           veto = function(ctx, choice) if (identical(choice, "rct")) "no RCT here" else NULL))
  withr::defer(sr_node_register(old))
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    i <- .md_row(rv(), "design")
    session$setInputs(table_rows_selected = i)
    n0 <- .md_n_events(rv())
    for (k in 1:3) session$setInputs(dec_choice = "rct", dec_reason = "x", dec_save = k)
    expect_match(output$dec_err, "no RCT here")
    expect_equal(.md_n_events(rv()), n0 + 3L)
    expect_equal(sr_router_table(rv())$status[i], "override_required")
    expect_match(.md_html(output$editor), "dec_override")
    session$setInputs(dec_override = TRUE, dec_reason = "reviewed", dec_save = 4)
    expect_equal(sr_router_table(rv())$choice[i], "rct")
  })
})

test_that("shadow export writes a jsonl under out/router and shows the path", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    session$setInputs(shadow_export = 1)
    expect_match(output$shadow_path, "router[/\\\\]shadow-[0-9]{8}-[0-9]{6}[.]jsonl$")
    expect_true(file.exists(output$shadow_path))
  })
})

test_that("after the lock the table is read-only", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  p <- sr_sap_add(.fx_ui_project(), "a1", "lm",
                 list(purpose = "explain-causal", outcome = "age", exposure = "grp", exposure_ref = "A",
                      layers = .fx_writer_layers(), seed = 42L), "t", label = "primary")
  p <- sr_sap_lock(.fx_feasible(p))
  expect_true(isTRUE(p$state$sap$locked))
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_design_server, args = list(project = rv), {
    n0 <- .md_n_events(rv())
    expect_false(any(sr_router_table(rv())$status == "stale"))
    expect_match(.md_html(output$pico_form), "<dl")
    expect_match(.md_html(output$dec_save_btn), "disabled")
    expect_match(.md_html(output$run_rules_btn), "disabled")
    session$setInputs(table_rows_selected = 1L)
    opt <- sr_router_editor(rv(), sr_router_table(rv())$node[1L])$options[1L]
    session$setInputs(dec_choice = opt, dec_reason = "y", dec_save = 1, pico_population = "a",
                      pico_outcome = "b", pico_save = 1, run_rules = 1)
    expect_equal(.md_n_events(rv()), n0)
    expect_match(output$dec_err, "SAP is locked")
    expect_match(.md_html(output$dec_reason_ui), "disabled")
  })
})

test_that("PICO fields and the reason box carry maxlength on the control itself", {
  withr::local_options(sr.mode = "airgap")
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    html <- .md_html(output$pico_form)
    expect_match(html, "<input[^>]*maxlength=\"2000\"")
    expect_false(grepl("form-group[^>]*maxlength", html))
    expect_match(.md_html(output$dec_reason_ui), "<textarea[^>]*maxlength=\"2000\"")
  })
})

test_that("a veto keeps the row selected and its message across the table refresh", {
  withr::local_options(sr.mode = "airgap")
  .ui_spy_notify()
  old <- sr_node_get("design")
  sr_node_register(sr_node("design", old$label, old$options, question = old$question, rule = old$rule,
                           applies = old$applies, facts = old$facts,
                           veto = function(ctx, choice) if (identical(choice, "rct")) "no RCT here" else NULL))
  withr::defer(sr_node_register(old))
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    i <- .md_row(rv(), "design")
    session$setInputs(table_rows_selected = i)
    session$setInputs(dec_choice = "rct", dec_reason = "x", dec_save = 1)
    expect_match(output$dec_err, "no RCT here")
    # DT re-renders after the project changed and reports an empty selection
    session$setInputs(table_rows_selected = integer(0))
    expect_match(output$dec_err, "no RCT here")
    expect_equal(sel(), "design")
    expect_match(.md_html(output$editor), "Design|design")
    for (k in 2:3) session$setInputs(dec_choice = "rct", dec_reason = "x", dec_save = k)
    session$setInputs(table_rows_selected = integer(0))
    expect_match(.md_html(output$editor), "dec_override")
    expect_match(output$dec_err, "no RCT here")
    # a different row clears the message
    j <- setdiff(seq_len(nrow(sr_router_table(rv()))), i)[1L]
    session$setInputs(table_rows_selected = j)
    expect_equal(output$dec_err, "")
  })
})

test_that("read-only with no row selected gives the read-only refusal, not 'Select a row'", {
  withr::local_options(sr.mode = "airgap")
  seen <- .ui_spy_notify()
  p <- .fx_ui_project()
  p$readonly <- TRUE
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_design_server, args = list(project = rv), {
    session$setInputs(dec_save = 1)
    expect_true(any(grepl("read-only", seen$msg)))
    expect_equal(output$dec_err, "")
    expect_match(.md_html(output$dec_reason_ui), "disabled")
  })
})

# ---- Ask Jev -------------------------------------------------------------------------------------

# Ack and consent through the dialogs, then fire the preview.
.md_jev_ready <- function(session) {
  session$setInputs(jev_ack = 1, jev_ack_reason = "DPA and IT sign-off documented, ref SYN-1", jev_ack_ok = 1)
  session$setInputs(jev_consent = 1, jev_consent_ok = 1)
  session$setInputs(jev_ask = 1)
}

test_that("air-gap: no Jev card, transport never called", {
  withr::local_options(sr.mode = "airgap", sr.jev_key = "sk-test-SENTINEL-0000",
                       sr.jev_transport = function(...) stop("transport called in air-gap"))
  .ui_spy_notify()
  spy <- local_fetch_spy("https://api.typesafe.ai/v1/systemone")
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    session$setInputs(pico_population = "adults", pico_outcome = "y", pico_aim = "explain-causal", pico_save = 1)
    session$setInputs(run_rules = 1)
    session$setInputs(jev_ask = 1)
    session$setInputs(jev_pick = "abc", jev_send = 1)
    expect_error(output$jev)
  })
  expect_length(spy$calls, 0L)
})

test_that("only ticked SHAs are sent", {
  env <- environment()
  .fx_connected(env)
  withr::local_options(sr.jev_transport = .fx_fake_transport(env))
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    .md_jev_ready(session)
    pl <- .sr_design_payloads(rv(), FALSE)
    expect_gte(length(pl), 2L)
    expect_match(.md_html(output$jev_payloads), pl[[1]]$sha, fixed = TRUE)
    session$setInputs(jev_pick = pl[[1]]$sha, jev_send = 1)
    expect_length(env$.calls, 1L)
    expect_identical(env$.calls[[1]]$body, pl[[1]]$body)
  })
})

test_that("masking changes the SHAs and the send uses the masked body", {
  env <- environment()
  .fx_connected(env)
  withr::local_options(sr.jev_transport = .fx_fake_transport(env))
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    session$setInputs(pico_population = "adults", pico_outcome = "y", pico_aim = "explain-causal", pico_save = 1)
    session$setInputs(run_rules = 1)
    .md_jev_ready(session)
    plain <- .sr_design_payloads(rv(), FALSE)
    session$setInputs(jev_mask = TRUE)
    masked <- .sr_design_payloads(rv(), TRUE)
    expect_gte(length(masked), 1L)
    plain_shas <- vapply(plain, function(x) x$sha, "")
    new <- Filter(function(x) !x$sha %in% plain_shas, masked)
    expect_gte(length(new), 1L)
    session$setInputs(jev_pick = new[[1]]$sha, jev_send = 1)
    expect_length(env$.calls, 1L)
    expect_identical(env$.calls[[1]]$body, new[[1]]$body)
  })
})

test_that("send with nothing ticked is refused", {
  env <- environment()
  .fx_connected(env)
  withr::local_options(sr.jev_transport = .fx_fake_transport(env))
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    .md_jev_ready(session)
    session$setInputs(jev_send = 1)
    expect_equal(output$jev_err, "tick at least one payload to send")
    expect_length(env$.calls, 0L)
  })
})

test_that("gates show each closed reason, never the key", {
  env <- environment()
  .fx_connected(env)
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    expect_match(.md_html(output$jev_gates), "no DPA / IT acknowledgement for this project", fixed = TRUE)
    session$setInputs(jev_ack = 1, jev_ack_reason = "", jev_ack_ok = 1)
    expect_match(output$jev_err, "needs a reason")
    session$setInputs(jev_ack_reason = "DPA and IT sign-off documented", jev_ack_ok = 2)
    expect_equal(output$jev_err, "")
    expect_match(.md_html(output$jev_gates), "no consent in this session", fixed = TRUE)
    expect_match(.md_html(output$jev_gates), "key set", fixed = TRUE)
    expect_false(grepl("SENTINEL", .md_html(output$jev_gates)))
    session$setInputs(jev_consent = 1, jev_consent_ok = 1)
    expect_false(grepl("no consent", .md_html(output$jev_gates)))
    session$setInputs(jev_withdraw = 1)
    expect_match(.md_html(output$jev_gates), "no consent in this session", fixed = TRUE)
  })
})

test_that("Jev controls are refused on a locked or read-only project", {
  env <- environment()
  .fx_connected(env)
  withr::local_options(sr.jev_transport = .fx_fake_transport(env))
  .ui_spy_notify()
  p <- .fx_ui_project()
  p$readonly <- TRUE
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_design_server, args = list(project = rv), {
    n0 <- .md_n_events(rv())
    session$setInputs(jev_ack = 1, jev_ack_reason = "DPA and IT sign-off documented", jev_ack_ok = 1)
    session$setInputs(jev_consent = 1, jev_consent_ok = 1)
    session$setInputs(jev_pick = "x", jev_send = 1)
    expect_equal(.md_n_events(rv()), n0)
    expect_length(env$.calls, 0L)
    expect_match(.md_html(output$jev), "disabled", fixed = TRUE)
  })
})

test_that("the mask choice survives project changes (the card does not re-render)", {
  env <- environment()
  .fx_connected(env)
  withr::local_options(sr.jev_transport = .fx_fake_transport(env))
  .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_ui_project())
  shiny::testServer(mod_design_server, args = list(project = rv), {
    session$setInputs(pico_population = "adults", pico_outcome = "y", pico_aim = "explain-causal", pico_save = 1)
    session$setInputs(run_rules = 1)
    session$setInputs(jev_mask = TRUE)
    before <- .md_html(output$jev)
    .md_jev_ready(session)
    session$setInputs(jev_withdraw = 1)
    session$setInputs(jev_consent = 2, jev_consent_ok = 2)
    expect_true(isTRUE(input$jev_mask))
    expect_identical(.md_html(output$jev), before)
    masked <- .sr_design_payloads(rv(), TRUE)
    session$setInputs(jev_pick = masked[[1]]$sha, jev_send = 1)
    expect_length(env$.calls, 1L)
    expect_identical(env$.calls[[1]]$body, masked[[1]]$body)
  })
})

test_that("a locked SAP appends no Jev events and disables the mask and pick inputs", {
  env <- environment()
  .fx_connected(env)
  withr::local_options(sr.jev_transport = .fx_fake_transport(env))
  .ui_spy_notify()
  p <- sr_sap_add(.fx_ui_project(), "a1", "lm",
                 list(purpose = "explain-causal", outcome = "age", exposure = "grp", exposure_ref = "A",
                      layers = .fx_writer_layers(), seed = 42L), "t", label = "primary")
  p <- sr_sap_lock(.fx_feasible(p))
  expect_true(isTRUE(p$state$sap$locked))
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_design_server, args = list(project = rv), {
    n0 <- .md_n_events(rv())
    session$setInputs(jev_ack = 1, jev_ack_reason = "DPA and IT sign-off documented", jev_ack_ok = 1)
    expect_match(output$jev_err, "locked")
    session$setInputs(jev_consent = 1, jev_consent_ok = 1)
    session$setInputs(jev_withdraw = 1)
    expect_equal(.md_n_events(rv()), n0)
    html <- .md_html(output$jev)
    tag <- regmatches(html, regexpr("<input[^>]*jev_mask[^>]*>", html))
    expect_length(tag, 1L)
    expect_match(tag, "disabled")
    # the pick group has no choices before a preview (and a locked plan cannot preview), so no input to disable
    expect_false(grepl("<input[^>]*jev_pick", html))
  })
})

test_that("a read-only project shows the refusal in the card and disables the inputs", {
  env <- environment()
  .fx_connected(env)
  .ui_spy_notify()
  p <- .fx_ui_project()
  p$readonly <- TRUE
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_design_server, args = list(project = rv), {
    session$setInputs(jev_consent = 1, jev_consent_ok = 1)
    expect_match(output$jev_err, "read-only")
    expect_match(regmatches(.md_html(output$jev), regexpr("<input[^>]*jev_mask[^>]*>", .md_html(output$jev))), "disabled")
  })
})
