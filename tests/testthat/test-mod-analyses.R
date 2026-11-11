# Tests for the Analyses panel module (ui/mod_analyses.R), driven through shiny::testServer. The queue runs
# inline (option sr.run_sync = TRUE) unless a test says otherwise: the stub routes of these tests exist only
# in this session, so a real worker could not run them. The background tests replace the spawner
# (.sr_an_spawn) with a promise or an already-resolved mirai, except the last one, which runs real lm
# analyses through mirai workers.

# Replace the global function `name` for the rest of the calling test; returns the original.
.ma_stub <- function(name, fn, env = parent.frame()) {
  old <- get(name, envir = globalenv())
  assign(name, fn, envir = globalenv())
  withr::defer(assign(name, old, envir = globalenv()), envir = env)
  invisible(old)
}

# Run the event loop until `done()` is TRUE or `secs` pass.
.ma_wait <- function(done, session, secs = 90) {
  t0 <- Sys.time()
  while (!isTRUE(done()) && as.numeric(difftime(Sys.time(), t0, units = "secs")) < secs) {
    later::run_now(0.2)
    session$flushReact()
  }
  invisible(done())
}

# A spawner stub whose job stays pending until the test calls ctl$resolve(value) (no worker process); it
# records each call in ctl$calls.
.ma_pending_spawner <- function(ctl) {
  ctl$calls <- list()
  function(app_dir, project_dir, id, job, progress_file, job_file, cancel_file) {
    ctl$calls[[length(ctl$calls) + 1L]] <- list(id = id, job = job, progress_file = progress_file,
                                                job_file = job_file, cancel_file = cancel_file)
    promises::promise(function(resolve, reject) {
      ctl$resolve <- resolve
      ctl$reject <- reject
    })
  }
}

# The button tag of `id` in the rendered Runs buttons, as one string.
.ma_btn <- function(html, id) {
  m <- regmatches(html, regexpr(sprintf("<button[^>]*id=\"[^\"]*%s\"[^>]*>", id), html))
  if (length(m)) m else ""
}

test_that("Run all runs every analysis not ok, in SAP order", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  order <- character()
  old <- .sr_an_job
  .ma_stub(".sr_an_job", function(p, id, job, ...) {
    order <<- c(order, id)
    old(p, id, job, ...)
  })
  rv <- shiny::reactiveVal(.fx_an_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    expect_identical(order, c("a1", "s1", "a2"))
    st <- vapply(c("a1", "s1", "a2"), function(i) rv()$state$runs[[i]]$status, "")
    expect_true(all(st == "ok"))
    expect_false(running())
    # nothing left to run: Run all says so and runs nothing
    session$setInputs(run_all = 2)
    expect_identical(order, c("a1", "s1", "a2"))
    expect_true(any(seen$msg == "Run all: every analysis has run successfully"))
  })
})

test_that("Run all on a partly run project runs only the analyses not ok", {
  withr::local_options(sr.run_sync = TRUE)
  order <- character()
  old <- .sr_an_job
  .ma_stub(".sr_an_job", function(p, id, job, ...) {
    order <<- c(order, id)
    old(p, id, job, ...)
  })
  rv <- shiny::reactiveVal(sr_run(.fx_an_project(), "a2", job = "j0"))
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    expect_identical(order, c("a1", "s1"))
    expect_identical(rv()$state$runs$a2$job, "j0")         # the ok one was not re-run
    expect_true(all(vapply(c("a1", "s1", "a2"), function(i) rv()$state$runs[[i]]$status, "") == "ok"))
  })
})

test_that("deselecting the row makes Run selected refuse", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_an_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    expect_identical(sel(), "a1")
    session$setInputs(runs_rows_selected = NULL)
    expect_null(sel())
    n0 <- n_events(rv())
    session$setInputs(run_sel = 1)
    expect_true(any(seen$msg == "Run: select an analysis first"))
    expect_identical(n_events(rv()), n0)
    expect_null(rv()$state$runs$a1)
  })
})

test_that("Run selected re-runs an ok analysis and records new events", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  p <- sr_run(.fx_an_project(), "a2", job = "j0")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_sel = 1)
    expect_true(any(seen$msg == "Run: select an analysis first"))
    n0 <- n_events(rv())
    i <- match("a2", .sr_an_order(rv())$id)
    session$setInputs(runs_rows_selected = i)
    expect_identical(sel(), "a2")
    session$setInputs(run_sel = 2)
    q <- rv()
    expect_true(n_events(q) > n0)
    expect_identical(q$state$runs$a2$status, "ok")
    expect_false(identical(q$state$runs$a2$job, "j0"))
    expect_length(q$state$runs$a2$history, 2L)
    expect_null(q$state$runs$a1)                           # only the selection ran
    expect_identical(sel(), "a2")                          # the selection survives the re-render
    expect_false(running())
  })
})

test_that("a failed fit is reported and the queue goes on", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  rv <- shiny::reactiveVal(.fx_an_project(body_fn = function(spec) "stop('boom')"))
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    st <- vapply(c("a1", "s1", "a2"), function(i) rv()$state$runs[[i]]$status, "")
    expect_true(all(st == "failed"))
    w <- seen$msg[seen$type == "warning"]
    expect_length(w, 3L)
    expect_true(all(startsWith(w, c("a1 failed: ", "s1 failed: ", "a2 failed: "))))
    expect_false(running())
  })
})

test_that("a refusal stops Run all and is shown verbatim", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  old <- .sr_an_job
  assign(".sr_an_job", function(p, id, job, ...) {
    if (id == "s1") return(list(status = "refused", message = "the SAP is not locked", project = p))
    old(p, id, job, ...)
  }, envir = globalenv())
  withr::defer(assign(".sr_an_job", old, envir = globalenv()))
  rv <- shiny::reactiveVal(.fx_an_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    expect_true(any(seen$msg == "Run stopped at s1: the SAP is not locked"))
    expect_identical(rv()$state$runs$a1$status, "ok")
    expect_null(rv()$state$runs$a2)
    expect_false(running())
  })
})

test_that("Run is refused while busy, read-only, unlocked or stopped, and the buttons are disabled", {
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  check <- function(p, busy, reason) {
    rv <- shiny::reactiveVal(p)
    n0 <- n_events(p)
    shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(busy)), {
      html <- paste(output$run_btns$html)
      expect_match(.ma_btn(html, "run_all"), "disabled", info = reason)
      expect_match(.ma_btn(html, "run_sel"), "disabled", info = reason)
      expect_match(.ma_btn(html, "cancel"), "disabled", info = reason)
      session$setInputs(runs_rows_selected = 1)
      session$setInputs(run_all = 1)
      session$setInputs(run_sel = 1)
      expect_identical(sum(seen$msg == paste0("Run: ", reason)), 2L, info = reason)
      expect_identical(n_events(p), n0)
      expect_null(rv()$state$runs$a1)
    })
  }
  p <- .fx_an_project()
  check(p, TRUE, "wait for the running import or analysis to finish")
  ro <- p
  ro$readonly <- TRUE
  check(ro, FALSE, "the project is read-only (locked by another session)")
  # the same analysis on a project whose SAP is not locked (the stub route is registered already)
  un <- sr_sap_add(.fx_ready_project(.run_frame(), "t", .run_roles), "a1", "stub_run", .run_spec(), "t",
                   label = "primary")
  check(un, FALSE, "lock the SAP first")
  st <- p
  st$state$feasibility$a1$decision <- list(decision = "stop", reason = "too few events")
  check(st, FALSE, "the project is stopped: too few events")
  # a runnable project: Run is on, Cancel off
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    html <- paste(output$run_btns$html)
    expect_false(grepl("disabled", .ma_btn(html, "run_all"), fixed = TRUE))
    expect_false(grepl("disabled", .ma_btn(html, "run_sel"), fixed = TRUE))
    expect_match(.ma_btn(html, "cancel"), "disabled")
  })
})

test_that("a project reopened after a crash shows interrupted and Run selected completes it", {
  withr::local_options(sr.run_sync = TRUE)
  p <- .fx_an_project()
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k", extract_sha = "s", job = "jdead"))
  q <- .run_reopen(p)                                      # the session that ran jdead died
  rv <- shiny::reactiveVal(q)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    expect_match(output$runs, "interrupted", fixed = TRUE)
    session$setInputs(runs_rows_selected = 1)
    expect_identical(sel(), "a1")
    session$setInputs(run_sel = 1)
    r <- rv()$state$runs$a1
    expect_identical(r$status, "ok")
    expect_identical(r$history[[1L]]$outcome, "interrupted")
    expect_identical(r$history[[1L]]$job, "jdead")
    expect_false(grepl("interrupted", output$runs, fixed = TRUE))
  })
})

test_that("the badge shows while a queue runs and Cancel when idle says nothing is running", {
  skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ma_stub(".sr_an_spawn", .ma_pending_spawner(ctl))
  p <- .fx_an_project()
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    expect_null(output$badge)
    session$setInputs(cancel = 1)
    expect_true(any(seen$msg == "Cancel: nothing is running"))
    session$setInputs(run_all = 1)
    expect_true(running())
    expect_match(paste(output$badge$html), "running", fixed = TRUE)
    expect_match(paste(output$progress$html), "Running 1 of 3: a1, starting", fixed = TRUE)
    expect_true(isTRUE(rv()$readonly))                     # the lock is released to the worker
    html <- paste(output$run_btns$html)
    expect_match(.ma_btn(html, "run_all"), "disabled")
    expect_false(grepl("disabled", .ma_btn(html, "cancel"), fixed = TRUE))
    # a second Run while the queue runs names the real reason, not the read-only copy
    session$setInputs(run_all = 2)
    expect_true(any(seen$msg == "Run: wait for the running import or analysis to finish"))
    expect_false(any(grepl("read-only", seen$msg, fixed = TRUE)))
    expect_length(ctl$calls, 1L)
    writeLines("fitting", ctl$calls[[1L]]$progress_file)
    session$elapse(1000)
    expect_match(paste(output$progress$html), "Running 1 of 3: a1, fitting", fixed = TRUE)
    for (k in 1:3) {
      n <- length(ctl$calls)
      ctl$resolve(list(status = "ok", message = NULL))
      .ma_wait(function() length(ctl$calls) > n || !running(), session, secs = 10)
    }
    expect_identical(vapply(ctl$calls, function(x) x$id, ""), c("a1", "s1", "a2"))
    expect_false(running())
    expect_null(output$badge)
    expect_false(isTRUE(rv()$readonly))                    # the project is ours again
    f <- unlist(lapply(ctl$calls, function(x) c(x$progress_file, x$job_file, x$cancel_file)))
    expect_false(any(file.exists(f)))
  })
})

test_that("Cancel kills the worker by its job file, drops the queue and cleans up", {
  skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ma_stub(".sr_an_spawn", .ma_pending_spawner(ctl))
  killed <- character()
  .ma_stub(".sr_an_kill", function(job_file, wait = 10) {
    killed <<- c(killed, job_file)
    ctl$reject(simpleError("19 | Connection reset"))      # what a killed worker's mirai gives
    TRUE
  })
  rv <- shiny::reactiveVal(.fx_an_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    jf <- ctl$calls[[1L]]$job_file
    file.create(paste0(jf, ".part"))
    session$setInputs(cancel = 1)
    expect_identical(killed, jf)
    expect_true(any(seen$msg == "Cancelling a1..."))
    expect_length(run$queue, 0L)
    .ma_wait(function() !running(), session, secs = 10)
    expect_false(running())
    expect_length(ctl$calls, 1L)                           # s1 and a2 were dropped
    expect_true(any(seen$msg == "Cancelled before a1 started."))
    expect_false(any(seen$type == "error"))
    expect_false(isTRUE(rv()$readonly))
    cl <- ctl$calls[[1L]]
    expect_false(any(file.exists(c(cl$progress_file, cl$job_file, cl$cancel_file, paste0(jf, ".part")))))
  })
})

test_that("a kill that cannot stop the worker is shown and the project is not re-taken meanwhile", {
  skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ma_stub(".sr_an_spawn", .ma_pending_spawner(ctl))
  .ma_stub(".sr_an_kill", function(job_file, wait = 10) {
    stop("could not stop the analysis worker (process 4242 still running)", call. = FALSE)
  })
  p <- .fx_an_project()
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    session$setInputs(cancel = 1)
    expect_true(any(seen$msg == "Cancel: could not stop the analysis worker (process 4242 still running)"))
    later::run_now(0.2)
    session$flushReact()
    expect_true(running())                                 # the worker may still be alive
    expect_true(isTRUE(rv()$readonly))                     # so the lock is not taken back
    expect_false(file.exists(p$paths$lock))
    ctl$resolve(list(status = "ok", message = NULL))        # the worker ends after all
    .ma_wait(function() !running(), session, secs = 10)
    expect_false(isTRUE(rv()$readonly))
    expect_length(ctl$calls, 1L)
  })
})

test_that("a kill that finds no worker says so and the result is settled when it ends", {
  skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ma_stub(".sr_an_spawn", .ma_pending_spawner(ctl))
  .ma_stub(".sr_an_kill", function(job_file, wait = 10) FALSE)
  rv <- shiny::reactiveVal(.fx_an_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    session$setInputs(cancel = 1)
    expect_true(any(seen$msg == "Cancelling a1..."))
    expect_true(any(seen$msg == "Cancel: no running worker was found for a1; it will be settled when it ends"))
    expect_true(running())
    ctl$resolve(list(status = "skipped", message = NULL))
    .ma_wait(function() !running(), session, secs = 10)
    expect_false(running())
    expect_false(isTRUE(rv()$readonly))
    expect_length(ctl$calls, 1L)
  })
})

# A spawner stub for the "another project is open" tests: it records the job in the run's folder (only
# run.started when `full` is FALSE, the whole run when TRUE), then stays pending like .ma_pending_spawner.
.ma_recording_spawner <- function(ctl, full) {
  pend <- .ma_pending_spawner(ctl)
  function(app_dir, project_dir, id, job, progress_file, job_file, cancel_file) {
    q <- sr_project_open(project_dir)
    q <- if (full) sr_run(q, id, job = job) else
      sr_project_append(q, "run.started", list(analysis = id, key = "k", extract_sha = "s", job = job))
    sr_duck_close(q)
    sr_project_close(q)
    pend(app_dir, project_dir, id, job, progress_file, job_file, cancel_file)
  }
}

# Run all on project A, open project B meanwhile, then end the job by `finish(ctl)`; returns what the
# checks need. B must stay untouched; the job settles on A.
.ma_other_project <- function(full, finish, env = parent.frame()) {
  seen <- .ui_spy_notify(env = env)
  ctl <- new.env()
  .ma_stub(".sr_an_spawn", .ma_recording_spawner(ctl, full), env = env)
  .ma_stub(".sr_an_kill", function(job_file, wait = 10) {
    ctl$reject(simpleError("19 | Connection reset"))
    TRUE
  }, env = env)
  a <- .fx_an_project(env = env)
  # a second project (the stub route is registered already)
  b <- sr_sap_add(.fx_ready_project(.run_frame(), "t", .run_roles), "a1", "stub_run", .run_spec(), "t",
                  label = "primary")
  withr::defer({
    try(sr_duck_close(b), silent = TRUE)
    try(sr_project_close(b), silent = TRUE)
  }, envir = env)
  rv <- shiny::reactiveVal(a)
  r <- list(a = a, b = b, seen = seen, name = as.character(a$meta$name %||% basename(a$paths$dir))[1L])
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    rv(b)                                                    # the user opens another project
    session$flushReact()
    expect_length(live(), 0L)                                # the live row belongs to A only
    n0 <- n_events(b)
    finish(ctl, session)
    .ma_wait(function() !running(), session, secs = 10)
    r$running <<- running()
    r$queue <<- run$queue
    r$calls <<- length(ctl$calls)
    r$same_b <<- identical(rv(), b)
    r$n_b <<- n_events(rv()) == n0
  })
  r$lock_gone <- !file.exists(a$paths$lock)
  q <- sr_project_open(a$paths$dir)
  r$a_types <- types_of(q)
  r$a_runs <- q$state$runs
  sr_duck_close(q)
  sr_project_close(q)
  r
}

test_that("Cancel while another project is open settles on the run's folder and leaves the open one alone", {
  skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  r <- .ma_other_project(FALSE, function(ctl, session) session$setInputs(cancel = 1))
  expect_false(r$running)
  expect_length(r$queue, 0L)
  expect_identical(r$calls, 1L)                              # s1 and a2 were not started
  expect_true(r$same_b)
  expect_true(r$n_b)
  expect_true(r$lock_gone)
  expect_true("run.cancelled" %in% r$a_types)
  expect_true(any(r$seen$msg == paste0("Cancelled a1. (in ", r$name, ")")))
})

test_that("a run that ends ok while another project is open is recorded in its own folder and stops the queue", {
  skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  r <- .ma_other_project(TRUE, function(ctl, session) ctl$resolve(list(status = "ok", message = NULL)))
  expect_false(r$running)
  expect_length(r$queue, 0L)
  expect_identical(r$calls, 1L)                              # the queue stopped after a1
  expect_true(r$same_b)
  expect_true(r$n_b)
  expect_true(r$lock_gone)
  expect_false("run.cancelled" %in% r$a_types)
  expect_identical(r$a_runs$a1$status, "ok")
  expect_true(any(r$seen$msg == paste0("a1 finished (in ", r$name, ")")))
})

test_that("Cancel racing completion keeps the result, writes no run.cancelled and says so", {
  skip_on_cran(); skip_if_not_installed("mirai"); skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  seen <- .ui_spy_notify()
  # the run is recorded in this session (the stub route exists only here) and the mirai has resolved
  .ma_stub(".sr_an_spawn", function(app_dir, project_dir, id, job, progress_file, job_file, cancel_file) {
    q <- sr_project_open(project_dir)
    q <- sr_run(q, id, job = job)
    sr_duck_close(q)
    sr_project_close(q)
    m <- mirai::mirai(list(status = "ok", message = NULL))
    mirai::call_mirai(m)
    m
  })
  killed <- 0L
  .ma_stub(".sr_an_kill", function(job_file, wait = 10) {
    killed <<- killed + 1L
    TRUE
  })
  rv <- shiny::reactiveVal(.fx_an_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1, run_sel = 1)
    session$setInputs(cancel = 1)
    expect_identical(killed, 0L)                           # a finished worker is not waited for
    .ma_wait(function() !running(), session, secs = 30)
    q <- rv()
    expect_false(isTRUE(q$readonly))
    expect_identical(q$state$runs$a1$status, "ok")
    expect_false("run.cancelled" %in% types_of(q))
    i <- which(grepl("finished before it could be cancelled", seen$msg, fixed = TRUE))
    expect_length(i, 1L)
    expect_identical(seen$type[i], "warning")
    expect_false(any(seen$type == "error"))
  })
})

test_that("Run all in the background reopens the project after each analysis", {
  skip_on_cran(); skip_if_not_installed("mirai"); skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  seen <- .ui_spy_notify()
  p <- .dv_project()
  p <- sr_sap_add(p, "l1", "lm", .dv_lm(outcome = "time"), "t", label = "primary")
  p <- sr_sap_add(p, "l2", "lm", .dv_lm(outcome = "time", seed = 12L), "t", label = "secondary")
  p <- sr_sap_lock(.fx_feasible(p))
  rv <- shiny::reactiveVal(p)
  withr::defer({
    q <- shiny::isolate(rv())
    try(sr_duck_close(q), silent = TRUE)
    try(sr_project_close(q), silent = TRUE)
  })
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(run_all = 1)
    expect_true(running())
    .ma_wait(function() !running(), session, secs = 90)
    expect_false(running())
    q <- rv()
    expect_false(isTRUE(q$readonly))
    expect_identical(q$state$runs$l1$status, "ok", info = paste(seen$msg, collapse = " | "))
    expect_identical(q$state$runs$l2$status, "ok", info = paste(seen$msg, collapse = " | "))
    expect_true(.sr_lock_is_mine(.sr_lock_read(q$paths$lock)))
  })
})

test_that("the Analyses tab is a stack of five cards, Runs first", {
  html <- as.character(htmltools::renderTags(mod_analyses_ui("x"))$html)
  heads <- regmatches(html, gregexpr("card-header[^>]*>[^<]*", html))[[1L]]
  heads <- trimws(sub("^card-header[^>]*>", "", heads))
  expect_identical(heads, c("Runs", "Result", "Primary vs sensitivity", "Multiplicity",
                            "Deviations and run history"))
  for (id in c("x-runs", "x-run_btns", "x-progress")) expect_match(html, sprintf("id=\"%s\"", id), fixed = TRUE)
})

# ---- card 2: the result, its diagnostics and Apply -----------------------------------------------------

# A locked project with the cox analysis m1 (it fails the proportional-hazards check on this data).
.ma_cox_project <- function(env = parent.frame()) {
  p <- .dv_project(env = env)
  p <- sr_sap_add(p, "m1", "cox", .dv_cox(terms = list(list(var = "z"))), "t", label = "primary")
  sr_sap_lock(.fx_feasible(p))
}

test_that("the result card shows effects, tables, figures, fragments and diagnostics of a cox run", {
  skip_on_cran()
  withr::local_options(sr.run_sync = TRUE)
  rv <- shiny::reactiveVal(.ma_cox_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    expect_match(paste(output$result_head$html), "Select an analysis in the Runs table.", fixed = TRUE)
    session$setInputs(runs_rows_selected = 1)
    expect_match(paste(output$result_head$html), "No successful run yet.", fixed = TRUE)
    expect_null(output$apply_preview)
    session$setInputs(run_sel = 1)
    expect_identical(rv()$state$runs$m1$status, "ok", info = rv()$state$runs$m1$error)
    head <- paste(output$result_head$html)
    expect_match(head, "ok", fixed = TRUE)
    expect_match(head, "fresh", fixed = TRUE)
    expect_match(output$effects, "HR", fixed = TRUE)
    expect_match(paste(output$figs$html), "fig_km", fixed = TRUE)
    expect_match(output$diag, "ph", fixed = TRUE)
    expect_match(output$diag, "fail", fixed = TRUE)
    pres <- sr_result(rv(), "m1")
    expect_gt(length(pres$tables), 0L)                       # the fixture really has tables and fragments
    expect_gt(length(pres$fragments), 0L)
    expect_match(paste(output$tables$html), names(pres$tables)[1L], fixed = TRUE)
    expect_match(paste(output$fragments$html), "<pre", fixed = TRUE)
    html <- paste(output$apply_preview$html)
    expect_match(html, "`m1_ph`", fixed = TRUE)
    expect_false(grepl("disabled", .ma_btn(html, "apply"), fixed = TRUE))
  })
})

test_that("a failed run shows its error verbatim and the last good result labelled", {
  withr::local_options(sr.run_sync = TRUE)
  p <- sr_run(.fx_an_project(), "a1", job = "j0")
  p <- sr_project_append(p, "run.completed", list(analysis = "a1", key = "k2", ok = FALSE, job = "jx",
                                                  error = "boom <b>& more", elapsed = 2))
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    head <- paste(output$result_head$html)
    expect_match(head, "<pre>boom &lt;b&gt;&amp; more</pre>", fixed = TRUE)
    expect_match(head, "failed", fixed = TRUE)
    expect_match(head, "Showing the last good run (", fixed = TRUE)
    expect_match(output$effects, "descriptive", fixed = TRUE)
  })
})

test_that("a failed run with no earlier ok run shows the error and nothing below", {
  withr::local_options(sr.run_sync = TRUE)
  q <- sr_project_append(.fx_an_project(), "run.completed",
                         list(analysis = "a1", key = "k3", ok = FALSE, job = "jy", error = "no fit", elapsed = 1))
  rv2 <- shiny::reactiveVal(q)
  shiny::testServer(mod_analyses_server, args = list(project = rv2, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    head <- paste(output$result_head$html)
    expect_match(head, "no fit", fixed = TRUE)
    expect_match(head, "No successful run yet.", fixed = TRUE)
    expect_false(grepl("Showing the last good run", head, fixed = TRUE))
    expect_null(output$apply_preview)
  })
})

test_that("a missing stored result is shown instead of the result, verbatim", {
  withr::local_options(sr.run_sync = TRUE)
  p <- sr_run(.fx_an_project(), "a1", job = "j0")
  unlink(file.path(p$paths$fits, paste0(p$state$runs$a1$ok_key, ".rds")))
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    expect_match(paste(output$result_head$html), "is missing; run it again", fixed = TRUE)
  })
})

test_that("Apply adds m1_ph, the Runs table lists it as not run, and Run all runs it", {
  skip_on_cran()
  withr::local_options(sr.run_sync = TRUE)
  rv <- shiny::reactiveVal(.ma_cox_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1, run_sel = 1)
    expect_null(rv()$state$analyses$m1_ph)
    session$setInputs(apply = 1)
    expect_false(is.null(rv()$state$analyses$m1_ph))
    expect_match(output$runs, "m1_ph", fixed = TRUE)
    expect_match(output$runs, "not run", fixed = TRUE)
    session$setInputs(run_all = 1)
    expect_identical(rv()$state$runs$m1_ph$status, "ok", info = rv()$state$runs$m1_ph$error)
    expect_match(paste(output$apply_preview$html), "Nothing to apply.", fixed = TRUE)
    expect_match(.ma_btn(paste(output$apply_preview$html), "apply"), "disabled")
  })
})

test_that("Apply is refused on a read-only project and writes nothing", {
  skip_on_cran()
  withr::local_options(sr.run_sync = TRUE)
  seen <- .ui_spy_notify()
  p <- sr_run(.ma_cox_project(), "m1")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    ro <- rv()
    ro$readonly <- TRUE
    rv(ro)
    session$flushReact()
    expect_match(.ma_btn(paste(output$apply_preview$html), "apply"), "disabled")
    n0 <- n_events(ro)
    session$setInputs(apply = 1)
    expect_true(any(seen$msg == "Apply diagnostics: the project is read-only (locked by another session)"))
    expect_identical(n_events(ro), n0)
    expect_null(rv()$state$analyses$m1_ph)
  })
})

test_that("Apply is refused while a queue runs", {
  skip_if_not_installed("later")
  withr::local_options(sr.run_sync = FALSE)
  seen <- .ui_spy_notify()
  ctl <- new.env()
  .ma_stub(".sr_an_spawn", .ma_pending_spawner(ctl))
  rv <- shiny::reactiveVal(sr_run(.fx_an_project(), "a1", job = "j0"))
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    session$setInputs(run_all = 1)
    session$setInputs(apply = 1)
    expect_true(any(seen$msg == "Apply diagnostics: wait for the running import or analysis to finish"))
    ctl$resolve(list(status = "ok", message = NULL))
  })
})

# ---- cards 3-5: comparison, multiplicity, deviations and run history -----------------------------------

# A locked project with real lm analyses: l1 (primary) and l2 (secondary) in the family f1 (holm), and
# ls, a pre-specified sensitivity analysis of l1.
.ma_lm_project <- function(env = parent.frame()) {
  p <- .dv_project(env = env)
  p <- sr_sap_add(p, "l1", "lm", .dv_lm(outcome = "time"), "t", label = "primary")
  p <- sr_sap_add(p, "l2", "lm", .dv_lm(outcome = "time", seed = 12L), "t", label = "secondary")
  p <- sr_sap_add(p, "ls", "lm", .dv_lm(outcome = "time", seed = 13L), "t", label = "sensitivity", parent = "l1")
  p <- sr_sap_family(p, "f1", c("l1", "l2"), "holm")
  sr_sap_lock(.fx_feasible(p))
}

test_that("compare shows each sensitivity analysis under its parent, never blank", {
  skip_on_cran()
  withr::local_options(sr.run_sync = TRUE)
  p <- sr_run(sr_run(.ma_lm_project(), "l1", job = "j1"), "l2", job = "j2")
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$flushReact()
    cp <- .sr_an_compare(rv())
    expect_identical(cp$analysis[cp$block == "l1"], c("l1", "ls"))
    expect_identical(cp$status[cp$analysis == "ls"], "not run")           # never blank
    expect_identical(cp$status[cp$analysis == "l1"], "ok")
    expect_identical(cmp_v(), cp)
    html <- output$compare
    expect_match(html, "not run", fixed = TRUE)
    expect_match(html, "ls", fixed = TRUE)
    expect_match(html, as.character(cp$est[cp$analysis == "l1"][1L]), fixed = TRUE)
    # running the sensitivity analysis fills its row
    session$setInputs(runs_rows_selected = match("ls", .sr_an_order(rv())$id), run_sel = 1)
    expect_identical(rv()$state$runs$ls$status, "ok")
    expect_identical(.sr_an_compare(rv())$status[cp$analysis == "ls"], "ok")
    expect_identical(cmp_v(), .sr_an_compare(rv()))
  })
})

test_that("compare says so when there is no primary or secondary analysis, and with no project", {
  rv <- shiny::reactiveVal(NULL)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    expect_match(output$compare, "open a project first", fixed = TRUE)
    expect_match(paste(output$families$html), "open a project first", fixed = TRUE)
    expect_match(output$deviations, "open a project first", fixed = TRUE)
    expect_match(output$history, "open a project first", fixed = TRUE)
  })
  rv2 <- shiny::reactiveVal(.fx_ready_project(.run_frame(), "t", .run_roles))
  shiny::testServer(mod_analyses_server, args = list(project = rv2, busy = shiny::reactive(FALSE)), {
    expect_match(output$compare, "No primary or secondary analysis.", fixed = TRUE)
    expect_match(paste(output$families$html), "No multiplicity family in the SAP.", fixed = TRUE)
  })
})

test_that("a family waits for its unrun members, then matches sr_family_adjust", {
  skip_on_cran()
  withr::local_options(sr.run_sync = TRUE)
  rv <- shiny::reactiveVal(sr_run(.ma_lm_project(), "l1", job = "j1"))
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    html <- paste(output$families$html)
    expect_match(html, "f1 (holm)", fixed = TRUE)
    expect_match(html, "Waiting for: l2", fixed = TRUE)
    expect_null(fam_v()[[1L]]$table)
    session$setInputs(runs_rows_selected = match("l2", .sr_an_order(rv())$id), run_sel = 1)
    expect_identical(rv()$state$runs$l2$status, "ok")
    html <- paste(output$families$html)
    expect_false(grepl("Waiting for", html, fixed = TRUE))
    expect_match(html, "f1 (holm)", fixed = TRUE)
    fa <- sr_family_adjust(rv(), "f1")
    tb <- fam_v()[[1L]]$table
    expect_equal(tb$p_adj, signif(fa$p_adj, 4))
    expect_identical(tb$analysis, fa$analysis)
    expect_match(html, "l2", fixed = TRUE)
  })
})

test_that("deviations match sr_deviations and history lists a cancelled attempt", {
  skip_on_cran()
  withr::local_options(sr.run_sync = TRUE)
  p <- .ma_lm_project()
  expect_identical(nrow(sr_deviations(p)), 0L)
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    expect_match(output$deviations, "No deviation from the locked SAP.", fixed = TRUE)
    expect_match(output$history, "No cancelled or interrupted run.", fixed = TRUE)
    q <- sr_analysis_add(rv(), "e1", "lm", .dv_lm(outcome = "time", seed = 14L), "t", "exploratory",
                         reason = "look at it <b>")
    q <- sr_project_append(q, "run.started", list(analysis = "l1", key = "k", extract_sha = "s", job = "jc"))
    q <- sr_run_cancel(q, "l1", "jc")
    rv(q)
    session$flushReact()
    d <- sr_deviations(q)
    expect_identical(dev_v(), d)
    expect_match(output$deviations, "exploratory", fixed = TRUE)
    expect_match(output$deviations, "e1", fixed = TRUE)
    expect_match(output$deviations, "look at it &lt;b&gt;", fixed = TRUE)
    h <- .sr_an_history(q)
    expect_identical(h$job, "jc")
    expect_identical(hist_v(), h)
    expect_match(output$history, "jc", fixed = TRUE)
    expect_match(output$history, "cancelled", fixed = TRUE)
  })
})

test_that("an unrelated project change does not redraw the cards", {
  withr::local_options(sr.run_sync = TRUE)
  rv <- shiny::reactiveVal(.fx_an_project())
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$flushReact()
    c0 <- cmp_v(); d0 <- dev_v()
    expect_identical(c0$analysis, c("a1", "s1", "a2"))
    ro <- rv()
    ro$meta$note <- "changed"                                  # a new project value, same cards
    rv(ro)
    session$flushReact()
    expect_identical(cmp_v(), c0)
    expect_identical(dev_v(), d0)
  })
})

test_that("an ok analysis with no diagnostics shows a muted note, not an error, and Apply stays off", {
  skip_on_cran()
  withr::local_options(sr.run_sync = TRUE)
  chk <- function(p, note) {
    rv <- shiny::reactiveVal(p)
    shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
      session$setInputs(runs_rows_selected = 1)
      html <- paste(output$apply_preview$html)
      expect_match(html, note, fixed = TRUE)
      expect_false(grepl("text-danger", html, fixed = TRUE))
      expect_false(grepl("sr_diag_run", html, fixed = TRUE))
      expect_match(.ma_btn(html, "apply"), "disabled")
    })
  }
  # a route with no applicable check records nothing
  chk(sr_run(.fx_an_project(), "a1", job = "j0"), "No diagnostics apply to this route.")
  # a Cox fit whose diagnostics were never recorded (a cancelled run, say)
  chk(sr_run(.ma_cox_project(), "m1", job = "j0"), "No diagnostics recorded yet - Run selected runs them.")
})

test_that("a run that is not finished shows no stale elapsed, cached or library warning", {
  withr::local_options(sr.run_sync = TRUE)
  p <- sr_run(.fx_an_project(), "a1", job = "j0")
  p$state$runs$a1$status <- "cancelled"
  p$state$runs$a1$elapsed <- 987
  p$state$runs$a1$cached <- TRUE
  p$state$runs$a1$lib_changed <- TRUE
  rv <- shiny::reactiveVal(p)
  shiny::testServer(mod_analyses_server, args = list(project = rv, busy = shiny::reactive(FALSE)), {
    session$setInputs(runs_rows_selected = 1)
    head <- paste(output$result_head$html)
    expect_match(head, "cancelled", fixed = TRUE)
    expect_false(grepl("cached", head, fixed = TRUE))
    expect_false(grepl("fresh", head, fixed = TRUE))
    expect_false(grepl("library snapshot", head, fixed = TRUE))
    expect_false(grepl("987", head, fixed = TRUE))
  })
})

test_that("the read-only banner is not shown while the app is busy", {
  p <- .fx_an_project()
  p$readonly <- TRUE
  shiny::testServer(mod_analyses_server, args = list(project = shiny::reactiveVal(p), busy = shiny::reactive(FALSE)), {
    expect_match(paste(output$run_btns$html), "read-only", fixed = TRUE)
  })
  shiny::testServer(mod_analyses_server, args = list(project = shiny::reactiveVal(p), busy = shiny::reactive(TRUE)), {
    html <- paste(output$run_btns$html)
    expect_false(grepl("alert-warning", html, fixed = TRUE))
    expect_match(html, "wait for the running import or analysis to finish", fixed = TRUE)
  })
})

test_that("a multiplicity error is shown instead of the generic note", {
  skip_on_cran()
  withr::local_options(sr.run_sync = TRUE)
  p <- sr_run(sr_run(.ma_lm_project(), "l1", job = "j1"), "l2", job = "j2")
  .ma_stub("sr_family_adjust", function(project, id) stop("members share no term", call. = FALSE))
  shiny::testServer(mod_analyses_server, args = list(project = shiny::reactiveVal(p), busy = shiny::reactive(FALSE)), {
    session$flushReact()
    html <- paste(output$families$html)
    expect_match(html, "members share no term", fixed = TRUE)
    expect_false(grepl("No adjusted table", html, fixed = TRUE))
  })
})
