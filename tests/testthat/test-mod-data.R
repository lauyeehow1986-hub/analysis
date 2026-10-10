# Tests for the Data tab module (ui/mod_data.R), driven through shiny::testServer. The import runs
# inline (option sr.ingest_sync = TRUE); one test runs the real mirai worker. Synthetic data only.

.md_roles <- c(patient_id = "id", surgery_date = "time_zero", statin_start_date = "exposure_date",
               statin = "exposure", death_date = "event_date", mortality_30d = "outcome")

# The synthetic cardiac cohort as a CSV next to the project folder. Returns its path.
.md_csv <- function(p, n = 600L, name = "cardiac.csv") {
  csv <- file.path(dirname(p$paths$dir), name)
  .fx_write_csv(sr_sample_cardiac(n = n)$data, csv)
  csv
}

# A reactiveVal holding an open project that already has the cardiac table, with the roles of
# .md_roles set when `roles` is TRUE.
.md_rv <- function(roles = FALSE, env = parent.frame()) {
  p <- .fx_cardiac_project(n = 600L, env = env)
  if (roles) p <- sr_roles_set(p, "cardiac", .md_roles)
  shiny::reactiveVal(p)
}

# The role inputs (role_1, role_2, ...) for the columns of `table`, from .md_roles (others covariate).
.md_role_inputs <- function(p, table = "cardiac") {
  cols <- vapply(sr_store_manifest(p, table)$schema, function(s) s$name, "")
  vals <- ifelse(cols %in% names(.md_roles), .md_roles[cols], "covariate")
  stats::setNames(as.list(unname(vals)), paste0("role_", seq_along(cols)))
}

.md_types <- function(p) vapply(sr_events_read(p$paths$events), function(e) e$type, "")

# How many readiness badges have status `s`.
.md_badges <- function(html, s) lengths(regmatches(html, gregexpr(sprintf("data-status=\"%s\"", s), html, fixed = TRUE)))

# Record every showNotification call: returns an environment with `type` and `msg`.
.md_spy_notify <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$type <- character()
  seen$msg <- character()
  testthat::local_mocked_bindings(
    showNotification = function(ui, ..., type = "message") {
      seen$type <- c(seen$type, type)
      seen$msg <- c(seen$msg, paste(as.character(ui), collapse = " "))
      invisible(NULL)
    }, .package = "shiny", .env = env)
  seen
}

# Inputs that decide finding `id` with `action` and `why`.
.md_decide <- function(id, action, why, nonce = 1) {
  inp <- list(decide = list(id = id, nonce = nonce))
  inp[[paste0("act_", id)]] <- action
  inp[[paste0("why_", id)]] <- why
  inp
}

test_that("a picked path is sniffed, previewed and pre-flighted", {
  p <- .fx_project()
  csv <- .md_csv(p)
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(path = csv)
    expect_match(output$sniff, "delim", fixed = TRUE)
    expect_match(output$sniff, "encoding", fixed = TRUE)
    expect_match(output$preview, "patient_id", fixed = TRUE)
    expect_match(as.character(output$preflight$html), "Free space", fixed = TRUE)
  })
})

test_that("the delimiter override re-reads the preview", {
  p <- .fx_project()
  csv <- file.path(dirname(p$paths$dir), "semi.csv")
  writeLines(c("a;b;c", "1;2;3", "4;5;6"), csv)
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(path = csv)
    expect_match(output$preview, "<th>b<", fixed = TRUE)
    session$setInputs(ov_delim = ",", ov_encoding = "auto", ov_decimal = "auto", ov_header = "auto",
                      ov_date_order = "auto")
    expect_match(output$sniff, "delim: ,", fixed = TRUE)
    expect_false(grepl("<th>b<", output$preview, fixed = TRUE))   # one column now: "a;b;c"
    expect_match(output$preview, "a;b;c", fixed = TRUE)
  })
})

test_that("Import (inline) ingests the file as the table named after it", {
  withr::local_options(sr.ingest_sync = TRUE)
  p <- .fx_project()
  csv <- .md_csv(p)
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(path = csv, import = 1)
    expect_false(is.null(rv()$state$tables$cardiac))
    expect_equal(rv()$state$tables$cardiac$store$rows, 600)
    expect_true("data.imported" %in% .md_types(rv()))
    expect_match(paste(output$tables_list$html), "cardiac", fixed = TRUE)
  })
})

test_that("errors from the core are notified and never crash the session", {
  withr::local_options(sr.ingest_sync = TRUE)
  seen <- .md_spy_notify()
  p <- .fx_project()
  rv <- shiny::reactiveVal(p)
  missing <- file.path(dirname(p$paths$dir), "nope.csv")
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(path = missing, import = 1)
    expect_true("error" %in% seen$type)
    expect_null(rv()$state$tables$nope)
  })
})

test_that(".sr_data_try keeps a warned result and notifies it as a warning", {
  seen <- .md_spy_notify()
  expect_identical(.sr_data_try("w", function() { warning("negative time", call. = FALSE); 42L }), 42L)
  expect_identical(seen$type, "warning")
  expect_match(seen$msg, "negative time", fixed = TRUE)
  expect_null(.sr_data_try("e", function() stop("boom", call. = FALSE)))
  expect_identical(utils::tail(seen$type, 1L), "error")
  expect_match(utils::tail(seen$msg, 1L), "boom", fixed = TRUE)
})

test_that("setting the roles and confirming appends roles.set", {
  rv <- .md_rv()
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac")
    expect_match(as.character(output$roles_ui$html), "role_1", fixed = TRUE)
    n0 <- length(sr_events_read(rv()$paths$events))
    do.call(session$setInputs, c(.md_role_inputs(rv()), list(roles_confirm = 1)))
    expect_equal(utils::tail(.md_types(rv()), 1L), "roles.set")
    expect_equal(length(sr_events_read(rv()$paths$events)), n0 + 1L)
    expect_equal(unname(sr_roles(rv(), "cardiac")["mortality_30d"]), "outcome")
  })
})

test_that("two outcomes are refused with a notification and nothing is recorded", {
  seen <- .md_spy_notify()
  rv <- .md_rv()
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac")
    inp <- .md_role_inputs(rv())
    cols <- vapply(sr_store_manifest(rv(), "cardiac")$schema, function(s) s$name, "")
    inp[[paste0("role_", match("statin", cols))]] <- "outcome"
    do.call(session$setInputs, c(inp, list(roles_confirm = 1)))
    expect_true("error" %in% seen$type)
    expect_false("roles.set" %in% .md_types(rv()))
  })
})

test_that("Run DQ renders the findings table with severity badges", {
  rv <- .md_rv(roles = TRUE)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", dq_run = 1)
    expect_true("dq.run" %in% .md_types(rv()))
    expect_false(is.null(output$findings))
    expect_match(output$findings, "badge", fixed = TRUE)
    expect_match(output$findings, "range", fixed = TRUE)
  })
})

test_that("deciding a range finding as recheck appends action.decided", {
  rv <- .md_rv(roles = TRUE)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", dq_run = 1)
    s <- sr_suggest(rv(), "cardiac")
    id <- s$finding_id[s$check == "range"][1L]
    expect_false(is.na(id))
    expect_match(as.character(output$suggestions_ui$html), id, fixed = TRUE)
    do.call(session$setInputs, .md_decide(id, "recheck", "ask the site to check the source record"))
    expect_equal(utils::tail(.md_types(rv()), 1L), "action.decided")
    expect_equal(rv()$state$actions[[id]]$action, "recheck")
  })
})

test_that("an override without a reason is refused and nothing is recorded", {
  seen <- .md_spy_notify()
  rv <- .md_rv(roles = TRUE)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", dq_run = 1)
    s <- sr_suggest(rv(), "cardiac")
    i <- which(s$action != "keep_sensitivity")[1L]
    do.call(session$setInputs, .md_decide(s$finding_id[i], "keep_sensitivity", ""))
    expect_true("error" %in% seen$type)
    expect_false("action.decided" %in% .md_types(rv()))
  })
})

test_that("Export query sheet records the sheet for rechecked findings", {
  rv <- .md_rv(roles = TRUE)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", dq_run = 1)
    s <- sr_suggest(rv(), "cardiac")
    id <- s$finding_id[s$check == "range"][1L]
    do.call(session$setInputs, .md_decide(id, "recheck", "check the source record"))
    session$setInputs(export_query = 1)
    expect_equal(utils::tail(.md_types(rv()), 1L), "query.exported")
  })
})

test_that("readiness blocks before roles and clears once every block finding is decided", {
  rv <- .md_rv()
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac")
    html <- as.character(output$readiness$html)
    expect_gt(.md_badges(html, "block"), 0L)
    do.call(session$setInputs, c(.md_role_inputs(rv()), list(roles_confirm = 1)))
    session$setInputs(dq_run = 1)
    f <- sr_dq_findings(rv(), "cardiac")
    ids <- f$id[f$severity == "block"]
    expect_gt(.md_badges(as.character(output$readiness$html), "block"), 0L)   # block findings undecided
    for (k in seq_along(ids)) {
      do.call(session$setInputs, .md_decide(ids[k], "recheck", "to be checked against the source", k))
    }
    html <- as.character(output$readiness$html)
    expect_equal(.md_badges(html, "block"), 0L)
    expect_gt(.md_badges(html, "ok"), 0L)
  })
})

test_that("the viewer pages the table and hides outcome columns until the SAP is locked", {
  rv <- .md_rv(roles = TRUE)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", view_page = 1, view_size = 25)
    expect_match(output$viewer, "patient_id", fixed = TRUE)
    expect_false(grepl("mortality_30d", output$viewer, fixed = TRUE))
    expect_match(paste(output$view_note$html), "outcome columns hidden until SAP lock", fixed = TRUE)
    p <- rv()
    p$state$sap <- list(locked = TRUE)
    rv(p)
    session$flushReact()
    expect_match(output$viewer, "mortality_30d", fixed = TRUE)
    expect_false(any(grepl("outcome columns hidden", paste(output$view_note$html), fixed = TRUE)))
  })
})

test_that("the viewer cannot be sorted by an outcome column while blind", {
  rv <- .md_rv(roles = TRUE)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", view_page = 1, view_size = 25, view_order = "mortality_30d")
    expect_false(grepl("mortality_30d", output$viewer, fixed = TRUE))
    expect_match(output$viewer, "patient_id", fixed = TRUE)
  })
})

test_that("the tables list hides the row count of a table whose flow has a hidden row", {
  rv <- .md_rv(roles = TRUE)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    expect_match(paste(output$tables_list$html), ">600<", fixed = TRUE)
    p <- sr_recipe_add(rv(), "cardiac", list(op = "drop_cases", reason = "deliberate",
      args = list(where = list(col = "mortality_30d", op = "==", value = 1),
                  confirm_outcome_dependent = TRUE)))
    rv(p)
    session$flushReact()
    expect_true(any(is.na(sr_flow(p, "cardiac")$n_after)))
    html <- paste(output$tables_list$html)
    expect_match(html, "hidden until SAP lock", fixed = TRUE)
    expect_false(grepl(">600<", html, fixed = TRUE))
    p$state$sap <- list(locked = TRUE)
    rv(p)
    session$flushReact()
    expect_false(grepl("hidden until SAP lock", paste(output$tables_list$html), fixed = TRUE))
  })
})

test_that("a recipe can be undone, branched, checked out and applied from the tab", {
  rv <- .md_rv(roles = TRUE)
  step <- list(op = "drop_cases", reason = "implausible age",
               args = list(where = list(col = "age", op = ">", value = 200)))
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac")
    rv(sr_recipe_add(rv(), "cardiac", step))
    session$flushReact()
    expect_match(output$recipe_steps, "drop_cases", fixed = TRUE)
    session$setInputs(branch_name = "alt", recipe_branch = 1)
    expect_true("alt" %in% names(rv()$state$recipe$branches))
    session$setInputs(branch_pick = "alt", recipe_checkout = 1)
    expect_equal(rv()$state$recipe$current, "alt")
    session$setInputs(branch_pick = "main", recipe_checkout = 2)
    session$setInputs(recipe_undo = 1)
    expect_equal(length(sr_recipe_steps(rv())), 0L)
    session$setInputs(recipe_undo = 2)                    # nothing left: notified, not fatal
    rv(sr_recipe_add(rv(), "cardiac", step))
    session$setInputs(recipe_apply = 1)
    expect_false(is.null(rv()$state$tables$cardiac_clean))
  })
})

test_that("the flow diagram renders as a PNG image", {
  rv <- .md_rv(roles = TRUE)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac")
    expect_match(output$flow_png$src, "^data:image/png")
  })
})

test_that("a dictionary is attached from a CSV, REDCap CSV or ODM file", {
  rv <- .md_rv()
  csv <- file.path(dirname(shiny::isolate(rv())$paths$dir), "dict.csv")
  .fx_write_csv(sr_sample_cardiac(n = 600L)$dict, csv)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", dict_kind = "csv", dict_path = csv, dict_attach = 1)
    expect_equal(utils::tail(.md_types(rv()), 1L), "dict.set")
    expect_equal(rv()$state$dict$cardiac$source, "csv")
  })
  rv2 <- .md_rv()
  rd <- normalizePath(testthat::test_path("fixtures/redcap/dd.csv"), winslash = "/")
  shiny::testServer(mod_data_server, args = list(project = rv2), {
    session$setInputs(table = "cardiac", dict_kind = "redcap", dict_path = rd, dict_attach = 1)
    expect_equal(rv2()$state$dict$cardiac$source, "redcap")
  })
  rv3 <- .md_rv()
  od <- normalizePath(testthat::test_path("fixtures/redcap/odm.xml"), winslash = "/")
  shiny::testServer(mod_data_server, args = list(project = rv3), {
    session$setInputs(table = "cardiac", dict_kind = "odm", dict_path = od, dict_attach = 1)
    expect_equal(rv3()$state$dict$cardiac$source, "odm")
  })
})

# ---- the pure helpers -------------------------------------------------------------------------------

test_that("a refused pre-flight shows the reason and blocks Import", {
  bad <- list(ok = FALSE, size_bytes = 1e9, uncompressed_bytes = 1e9, need_bytes = 5e9, free_bytes = 1e9,
              est_seconds = 10, network_share = FALSE, external_suggested = FALSE, warnings = "w1",
              refuse_reason = "insufficient space: need 5 GB, have 1 GB")
  good <- bad
  good$ok <- TRUE
  good$refuse_reason <- NULL
  html <- as.character(.sr_data_preflight_ui(bad))
  expect_match(html, "insufficient space", fixed = TRUE)
  expect_match(html, "w1", fixed = TRUE)
  expect_false(.sr_data_can_import(bad))
  expect_true(.sr_data_can_import(good))
  expect_false(.sr_data_can_import(NULL))
})

test_that("the progress file is read back and tolerates a missing or torn file", {
  f <- withr::local_tempfile()
  expect_null(.sr_data_progress_read(f))
  writeLines("500 1000", f)
  expect_equal(.sr_data_progress_read(f), list(done = 500, total = 1000, fraction = 0.5))
  writeLines("50", f)
  expect_null(.sr_data_progress_read(f))
})

test_that("the ingest job reports progress and stops on the cancel file", {
  p <- .fx_project()
  csv <- .md_csv(p, n = 400L)
  prog <- withr::local_tempfile()
  cancel <- withr::local_tempfile()
  ok <- .sr_data_ingest_job(project = p, path = csv, table = "cardiac", progress_file = prog, cancel_file = cancel)
  expect_true(ok$ok)
  expect_false(ok$cancelled)
  expect_equal(ok$project$state$tables$cardiac$store$rows, 400)
  expect_equal(.sr_data_progress_read(prog)$fraction, 1)
  p2 <- .fx_project()
  file.create(cancel)
  res <- .sr_data_ingest_job(project = p2, path = csv, table = "cardiac", progress_file = prog, cancel_file = cancel)
  expect_false(res$ok)
  expect_true(res$cancelled)
  expect_null(res$project$state$tables$cardiac)
})

test_that("the worker sources the app, takes the project lock itself and ingests", {
  skip_if_not_installed("mirai")
  p <- .fx_project()
  csv <- .md_csv(p, n = 300L)
  prog <- withr::local_tempfile()
  cancel <- withr::local_tempfile()
  dir <- p$paths$dir
  sr_project_close(p)                                     # the UI releases the lock before the worker opens
  m <- .sr_data_spawn(app_dir = sr_root, project_dir = dir, path = csv, table = "cardiac",
                      progress_file = prog, cancel_file = cancel)
  res <- mirai::call_mirai(m)$data
  expect_false(mirai::is_error_value(res), info = paste(format(res), collapse = " "))
  expect_true(res$ok)
  q <- sr_project_open(dir)
  withr::defer(sr_project_close(q))
  expect_false(q$readonly)                                # the worker released the lock
  expect_equal(q$state$tables$cardiac$store$rows, 300)
})

test_that("the Data panel is in the app shell", {
  src <- readLines(file.path(sr_root, "app.R"), warn = FALSE)
  expect_true(any(grepl("mod_data_ui", src, fixed = TRUE)))
  expect_true(any(grepl("mod_data_server", src, fixed = TRUE)))
})

test_that("Import runs in the background, releases the lock to the worker and reopens the project", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  withr::local_options(sr.ingest_sync = FALSE)
  p <- .fx_project()
  csv <- .md_csv(p, n = 300L)
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(path = csv, import = 1)
    expect_true(job$busy)
    expect_null(shiny::isolate(rv())$state$tables$cardiac)
    t0 <- Sys.time()
    while (isTRUE(job$busy) && as.numeric(difftime(Sys.time(), t0, units = "secs")) < 90) {
      later::run_now(0.2)
      session$flushReact()
    }
    expect_false(job$busy)
    q <- shiny::isolate(rv())
    expect_false(q$readonly)
    expect_equal(q$state$tables$cardiac$store$rows, 300)
  })
})

test_that("Cancel with no import running only says so", {
  seen <- .md_spy_notify()
  rv <- .md_rv()
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(cancel = 1)
    expect_identical(seen$type, "message")
    expect_match(seen$msg, "No import", fixed = TRUE)
  })
})

# ---- fix round 1 -------------------------------------------------------------------------------------

# Replace the global function `name` for the rest of the test; returns the original.
.md_stub_global <- function(name, fn, env = parent.frame()) {
  old <- get(name, envir = globalenv())
  assign(name, fn, envir = globalenv())
  withr::defer(assign(name, old, envir = globalenv()), envir = env)
  invisible(old)
}

# A recipe step that drops outcome-dependent cases: the flow then has a hidden count while blind.
.md_outcome_step <- function() {
  list(op = "drop_cases", reason = "deliberate",
       args = list(where = list(col = "mortality_30d", op = "==", value = 1), confirm_outcome_dependent = TRUE))
}

# Run the event loop until `done()` is TRUE or `secs` pass.
.md_wait <- function(done, session, secs = 90) {
  t0 <- Sys.time()
  while (!done() && as.numeric(difftime(Sys.time(), t0, units = "secs")) < secs) {
    later::run_now(0.2)
    session$flushReact()
  }
  invisible(done())
}

test_that("the raw-file preview hides outcome columns of any table while blind", {
  rv0 <- .md_rv()                                          # no roles yet: nothing is known to hide
  csv0 <- .md_csv(shiny::isolate(rv0()), n = 100L, name = "again.csv")
  shiny::testServer(mod_data_server, args = list(project = rv0), {
    session$setInputs(path = csv0)
    expect_match(output$preview, "mortality_30d", fixed = TRUE)
  })
  rv <- .md_rv(roles = TRUE)
  csv <- .md_csv(shiny::isolate(rv()), n = 100L, name = "again2.csv")
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(path = csv)
    expect_match(output$preview, "patient_id", fixed = TRUE)
    expect_false(grepl("mortality_30d", output$preview, fixed = TRUE))
    expect_match(paste(output$preview_note$html), "outcome columns hidden until SAP lock", fixed = TRUE)
    p <- rv()
    p$state$sap <- list(locked = TRUE)
    rv(p)
    session$flushReact()
    expect_match(output$preview, "mortality_30d", fixed = TRUE)
    expect_false(any(grepl("outcome columns hidden", paste(output$preview_note$html), fixed = TRUE)))
  })
})

test_that("suggestions are ordered block first and paged, so a block finding past 200 can be decided", {
  n <- 250L
  ids <- sprintf("f%03d", seq_len(n))
  sev <- c(rep("warn", 20L), rep("block", n - 20L))        # the blocks come late in finding order
  cols <- sprintf("c%03d", seq_len(n))
  fake_f <- data.frame(id = ids, check = "range", domain = "plausibility", column = cols, severity = sev,
                       n_affected = 1, message = "m", stringsAsFactors = FALSE)
  fake_s <- data.frame(finding_id = ids, check = "range", column = cols, action = "recheck", reason = "r",
                       stringsAsFactors = FALSE)
  fake_s$proposed_step <- vector("list", n)
  .md_stub_global("sr_dq_findings", function(project, table) fake_f)
  .md_stub_global("sr_suggest", function(project, table) fake_s)
  decided <- new.env()
  decided$id <- character()
  .md_stub_global("sr_action_decide", function(project, finding_id, action, reason = NULL, step = NULL,
                                               table = NULL) {
    decided$id <- c(decided$id, finding_id)
    project
  })
  rv <- .md_rv()
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", sug_page = 1)
    html <- paste(output$suggestions_ui$html)
    expect_match(html, "showing 1-50 of 250", fixed = TRUE)
    expect_match(html, "f021", fixed = TRUE)               # the first block
    expect_false(grepl("f001", html, fixed = TRUE))        # warns sort after the blocks
    session$setInputs(sug_page = 5)
    html <- paste(output$suggestions_ui$html)
    expect_match(html, "showing 201-250 of 250", fixed = TRUE)
    expect_match(html, "f250", fixed = TRUE)               # block number 230 of 230
    do.call(session$setInputs, .md_decide("f250", "recheck", "to be checked"))
    expect_identical(decided$id, "f250")
  })
})

test_that("a finished import leaves a different open project alone and says where it finished", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  withr::local_options(sr.ingest_sync = FALSE)
  seen <- .md_spy_notify()
  a <- .fx_project()
  csv <- .md_csv(a, n = 200L)
  b <- .fx_project()
  rv <- shiny::reactiveVal(a)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(path = csv, import = 1)
    expect_true(job$busy)
    ro <- shiny::isolate(rv())
    expect_true(ro$readonly)                                # no lock is held while the worker runs
    expect_error(sr_project_append(ro, "roles.set", list()), "read-only", fixed = TRUE)
    rv(b)                                                   # the user opens another project meanwhile
    session$flushReact()
    .md_wait(function() !isTRUE(job$busy), session)
    expect_false(job$busy)
    cur <- shiny::isolate(rv())
    expect_identical(cur$paths$dir, b$paths$dir)
    expect_false(cur$readonly)
    expect_true(file.exists(b$paths$lock))                  # the lock of the open project is untouched
    expect_true(any(grepl("import finished in fixture", seen$msg, ignore.case = TRUE)))
    q <- sr_project_open(a$paths$dir)
    withr::defer(sr_project_close(q))
    expect_false(q$readonly)                                # the worker released the first project
    expect_equal(q$state$tables$cardiac$store$rows, 200)
  })
})

test_that("while row counts are hidden the viewer is one page and paging is refused", {
  seen <- .md_spy_notify()
  rv <- .md_rv(roles = TRUE)
  rv(sr_recipe_add(shiny::isolate(rv()), "cardiac", .md_outcome_step()))
  rec <- new.env()
  rec$offset <- numeric()
  orig <- get("sr_table_page", envir = globalenv())
  .md_stub_global("sr_table_page", function(project, table, offset, limit, ...) {
    rec$offset <- c(rec$offset, offset)
    orig(project, table, offset, limit, ...)
  })
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", view_size = 25, view_page = 3)
    expect_match(output$viewer, "patient_id", fixed = TRUE)
    expect_true(length(rec$offset) >= 1L)
    expect_true(all(rec$offset == 0))                       # page 3 was clamped to page 1
    expect_match(paste(output$view_nav$html), "disabled", fixed = TRUE)
    session$setInputs(view_next = 1)
    expect_true(any(grepl("paging", seen$msg, ignore.case = TRUE)))
  })
})

test_that("an error in a blind check hides the columns and the count instead of showing them", {
  rv <- .md_rv(roles = TRUE)
  csv <- .md_csv(shiny::isolate(rv()), n = 100L, name = "closed.csv")
  .md_stub_global("sr_role_cols", function(...) stop("boom", call. = FALSE))
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac", view_size = 25, view_page = 1, path = csv)
    expect_false(grepl("patient_id", output$viewer, fixed = TRUE))
    expect_false(grepl("mortality_30d", output$viewer, fixed = TRUE))
    expect_match(paste(output$view_note$html), "outcome columns hidden until SAP lock", fixed = TRUE)
    expect_false(grepl("patient_id", output$preview, fixed = TRUE))
  })
  rv2 <- .md_rv(roles = TRUE)
  .md_stub_global("sr_flow", function(...) stop("boom", call. = FALSE))
  shiny::testServer(mod_data_server, args = list(project = rv2), {
    expect_match(paste(output$tables_list$html), "hidden until SAP lock", fixed = TRUE)
    expect_false(grepl(">600<", paste(output$tables_list$html), fixed = TRUE))
  })
  expect_true(.sr_data_rows_hidden(shiny::isolate(rv2()), "cardiac"))
})

test_that("ending the session during an import cancels it and removes the progress file", {
  skip_if_not_installed("mirai")
  skip_if_not_installed("later")
  withr::local_options(sr.ingest_sync = FALSE)
  a <- .fx_project()
  csv <- .md_csv(a, n = 200L)
  rv <- shiny::reactiveVal(a)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(path = csv, import = 1)
    prog <- job$progress
    canc <- job$cancel
    expect_true(is.character(prog))
    session$close()
    expect_true(file.exists(canc))
    expect_false(file.exists(prog))
    # the worker removes the cancel file itself when it is done, after it has released the project
    t0 <- Sys.time()
    while (file.exists(canc) && as.numeric(difftime(Sys.time(), t0, units = "secs")) < 90) later::run_now(0.2)
    expect_false(file.exists(canc))
    expect_false(file.exists(a$paths$lock))
  })
})

# ---- fix round 2 -------------------------------------------------------------------------------------

test_that("the Data tab is a plain stack of cards that size to their content", {
  html <- as.character(htmltools::renderTags(mod_data_ui("x"))$html)
  doc <- xml2::read_html(paste0("<html><body>", html, "</body></html>"))
  has <- function(cls) paste0("contains(concat(' ', normalize-space(@class), ' '), ' ", cls, ' ', "')")
  cards <- xml2::xml_find_all(doc, paste0("//*[", has("card"), "]"))
  expect_equal(length(cards), 10L)
  # no fill item anywhere (card, grid or widget): a fill item is squeezed into the viewport height
  expect_false(grepl("html-fill-item", html, fixed = TRUE))
  # no grid wraps the cards (a grid row would share the page height between them)
  inside_grid <- xml2::xml_find_all(doc, paste0("//*[", has("bslib-grid"), "]//*[", has("card"), "]"))
  expect_equal(length(inside_grid), 0L)
})

test_that("the flow image scales to its card instead of showing at its pixel size", {
  rv <- .md_rv(roles = TRUE)
  shiny::testServer(mod_data_server, args = list(project = rv), {
    session$setInputs(table = "cardiac")
    img <- output$flow_png
    expect_match(img$src, "^data:image/png")
    expect_match(img$style %||% "", "max-width:[[:space:]]*100%")
    expect_match(img$style %||% "", "height:[[:space:]]*auto")
  })
})
