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
