# The Analyses panel worker layer: .sr_an_job, .sr_an_spawn, .sr_an_kill and .sr_an_settle.

test_that("job: a fast analysis runs, records the job and reports ok", {
  p <- .fx_an_project()
  pf <- withr::local_tempfile(); jf <- withr::local_tempfile()
  res <- .sr_an_job(p, "a1", "j1", progress_file = pf, job_file = jf)
  expect_identical(res$status, "ok")
  expect_identical(res$project$state$runs$a1$job, "j1")
  expect_identical(readLines(pf), "diagnostics")
  expect_match(readLines(jf), sprintf("^%d [0-9.]+$", Sys.getpid()))
})

test_that("job: a failed fit is 'failed' with the run's error; a refusal is 'refused' verbatim", {
  p <- .fx_an_project(body_fn = function(spec) "stop('boom')")
  res <- .sr_an_job(p, "a1", "j1")
  expect_identical(res$status, "failed")
  expect_match(res$message, "boom")
  res <- .sr_an_job(p, "nope", "j2")
  expect_identical(res$status, "refused")
  expect_identical(res$message, "unknown analysis 'nope'")
})

test_that("job: a cancel file present before the fit skips the run", {
  p <- .fx_an_project()
  cf <- withr::local_tempfile(); file.create(cf)
  res <- .sr_an_job(p, "a1", "j1", cancel_file = cf)
  expect_identical(res$status, "skipped")
  expect_null(p$state$runs$a1)
})

test_that("settle: ok continues; failed warns and continues; refused stops with the text verbatim", {
  p <- .fx_an_project()
  n0 <- n_events(p)
  out <- .sr_an_settle(p, "a1", "j1", list(status = "ok"), cancelling = FALSE)
  expect_null(out$notice)
  expect_false(out$stop)
  expect_identical(out$project, p)
  out <- .sr_an_settle(p, "a1", "j1", list(status = "failed", message = "boom"), cancelling = FALSE)
  expect_identical(out$notice, "a1 failed: boom")
  expect_identical(out$type, "warning")
  expect_false(out$stop)
  msg <- .sr_an_job(p, "nope", "j1")$message
  out <- .sr_an_settle(p, "nope", "j1", list(status = "refused", message = msg), cancelling = FALSE)
  expect_true(out$notice == "Run stopped at nope: unknown analysis 'nope'")
  expect_identical(out$type, "error")
  expect_true(out$stop)
  out <- .sr_an_settle(p, "a1", "j1", list(status = "skipped"), cancelling = FALSE)
  expect_null(out$notice)
  expect_true(out$stop)
  expect_identical(n_events(p), n0)                       # settle writes nothing unless cancelling
})

test_that("settle: cancel of a running job records run.cancelled and stops", {
  p <- .fx_an_project()
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k", extract_sha = "s", job = "j2"))
  killed <- structure(19L, class = "errorValue")          # the mirai value of a killed worker
  out <- .sr_an_settle(p, "a1", "j2", killed, cancelling = TRUE)
  expect_identical(tail(types_of(out$project), 1L), "run.cancelled")
  expect_identical(out$project$state$runs$a1$status, "cancelled")
  expect_identical(out$notice, "Cancelled a1.")
  expect_identical(out$type, "message")
  expect_true(out$stop)
})

test_that("settle: a refusal reported while cancelling is kept in the notice", {
  p <- .fx_an_project()
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k", extract_sha = "s", job = "j2"))
  out <- .sr_an_settle(p, "a1", "j2", list(status = "refused", message = "diagnostics: boom"),
                       cancelling = TRUE)
  expect_identical(out$notice, "Cancelled a1. The worker reported: diagnostics: boom")
  expect_identical(out$type, "message")
  expect_true(out$stop)
})

test_that("settle: a killed worker that never started the job is 'Cancelled before'", {
  p <- .fx_an_project()
  n0 <- n_events(p)
  out <- .sr_an_settle(p, "a1", "j9", structure(19L, class = "errorValue"), cancelling = TRUE)
  expect_identical(out$notice, "Cancelled before a1 started.")
  expect_identical(out$type, "message")
  expect_true(out$stop)
  expect_identical(n_events(out$project), n0)
})

test_that("settle: a worker that died without a status stops the queue with a readable message", {
  p <- .fx_an_project()
  out <- .sr_an_settle(p, "a1", "j1", structure(19L, class = "errorValue"), cancelling = FALSE)
  expect_identical(out$notice, "Run stopped at a1: worker stopped unexpectedly (mirai error 19)")
  expect_identical(out$type, "error")
  expect_true(out$stop)
})

test_that("settle: a cancel before the start says so; a refused cancel is an error; both stop", {
  p <- .fx_an_project()
  n0 <- n_events(p)
  out <- .sr_an_settle(p, "a1", "j1", list(status = "skipped"), cancelling = TRUE)
  expect_identical(out$notice, "Cancelled before a1 started.")
  expect_identical(out$type, "message")
  expect_true(out$stop)
  expect_identical(n_events(out$project), n0)
  ro <- p; ro$readonly <- TRUE
  out <- .sr_an_settle(ro, "a1", "j1", list(status = "ok"), cancelling = TRUE)
  expect_identical(out$notice, "Cancel a1: project is read-only")
  expect_identical(out$type, "error")
  expect_true(out$stop)
  expect_identical(n_events(p), n0)
})

test_that("settle: cancel that lost the race keeps the result and says so", {
  p <- sr_run(.fx_an_project(), "a1", job = "j1")
  n0 <- length(sr_events_read(p$paths$events))
  out <- .sr_an_settle(p, "a1", "j1", list(status = "ok"), cancelling = TRUE)
  expect_identical(length(sr_events_read(out$project$paths$events)), n0)
  expect_identical(out$project$state$runs$a1$status, "ok")
  expect_match(out$notice, "finished before it could be cancelled")
  expect_identical(out$type, "warning")
  expect_true(out$stop)
})

test_that("spawn: the worker opens the project itself, runs an lm analysis and releases the lock", {
  skip_on_cran(); skip_if_not_installed("mirai")
  # a locked project with a real lm analysis (.dv_project data, .dv_lm spec with outcome = "time")
  p <- .dv_project()
  p <- sr_sap_add(p, "l1", "lm", .dv_lm(outcome = "time"), "t", label = "primary")
  p <- sr_sap_lock(.fx_feasible(p))
  dir <- p$paths$dir
  pf <- withr::local_tempfile(); jf <- withr::local_tempfile(); cf <- withr::local_tempfile()
  sr_duck_close(p)
  sr_project_close(p)                                     # the panel releases the lock before the worker opens
  m <- .sr_an_spawn(sr_root, dir, "l1", "jw1", pf, jf, cf)
  withr::defer(try(mirai::stop_mirai(m), silent = TRUE))
  res <- mirai::call_mirai(m)$data
  expect_false(mirai::is_error_value(res), info = paste(format(res), collapse = " "))
  expect_identical(res$status, "ok")
  expect_null(res$project)
  q <- sr_project_open(dir); withr::defer(sr_project_close(q)); withr::defer(sr_duck_close(q))
  expect_false(q$readonly)
  expect_identical(q$state$runs$l1$job, "jw1")
  expect_identical(q$state$runs$l1$status, "ok")
  expect_false(any(file.exists(c(pf, jf, cf))))
})

test_that("spawn: a project locked by another session is refused", {
  skip_on_cran(); skip_if_not_installed("mirai")
  p <- .fx_an_project()                                   # this session keeps the lock
  m <- .sr_an_spawn(sr_root, p$paths$dir, "a1", "jw2", NULL, NULL, NULL)
  withr::defer(try(mirai::stop_mirai(m), silent = TRUE))
  res <- mirai::call_mirai(m)$data
  expect_false(mirai::is_error_value(res), info = paste(format(res), collapse = " "))
  expect_identical(res$status, "refused")
  expect_identical(res$message, "the project is locked by another session")
  expect_null(p$state$runs$a1)
})

test_that("kill: the worker and its callr child are both gone", {
  skip_on_cran(); skip_if_not_installed("mirai")
  jf <- withr::local_tempfile()
  m <- mirai::mirai({
    h <- ps::ps_handle()
    writeLines(paste(Sys.getpid(), format(as.numeric(ps::ps_create_time(h)), digits = 15)), jf)
    callr::r(function() Sys.sleep(120))
  }, jf = jf, .timeout = 180000)
  w <- list(handles = list())
  withr::defer(.fx_an_cleanup(m, jf, w$handles))         # never leave a process behind, whatever happens
  # wait (up to 60 s) for the job file and one child process of the worker
  w <- .fx_an_wait_child(jf, function() TRUE)
  expect_false(is.na(w$worker))
  expect_true(length(w$child) >= 1L)
  pids <- c(w$worker, w$child)
  expect_true(.sr_an_kill(jf))
  expect_false(any(vapply(pids, .sr_pid_running, NA)))
  expect_true(mirai::is_error_value(mirai::call_mirai(m)$data))
})

test_that("kill: a cancelled long fit records no run.completed, leaves no process and settles as cancelled", {
  skip_on_cran(); skip_if_not_installed("mirai")
  slow <- function(spec) "Sys.sleep(120)\nlist(n = nrow(sr_data), m = 1)"
  environment(slow) <- globalenv()
  p <- .fx_an_project(body_fn = slow)
  # the stub route exists only in this session: hand the worker a copy with no test environment attached
  r <- sr_route_get("stub_run", 1L)
  environment(r$compile) <- globalenv()
  environment(r$present) <- globalenv()
  dir <- p$paths$dir
  pf <- withr::local_tempfile(); jf <- withr::local_tempfile()
  sr_duck_close(p)
  sr_project_close(p)
  m <- mirai::mirai({
    source(file.path(app_dir, "global.R"), local = FALSE, encoding = "UTF-8")
    sr_route_register(r)
    .sr_an_job(sr_project_open(dir), "a1", "jk", pf, jf, NULL)[c("status", "message")]
  }, app_dir = sr_root, r = r, dir = dir, pf = pf, jf = jf, .timeout = 180000)
  w <- list(handles = list())
  withr::defer(.fx_an_cleanup(m, jf, w$handles))
  # wait (up to 60 s) until the fit runs in its callr child
  w <- .fx_an_wait_child(jf, function() identical(suppressWarnings(tryCatch(readLines(pf), error = function(e) "")), "fitting"))
  expect_true(length(w$child) >= 1L)
  pids <- c(w$worker, w$child)
  expect_true(.sr_an_kill(jf))
  expect_false(any(vapply(pids, .sr_pid_running, NA)))
  expect_true(mirai::is_error_value(res <- mirai::call_mirai(m)$data))
  q <- sr_project_open(dir); withr::defer(sr_project_close(q)); withr::defer(sr_duck_close(q))
  expect_false(q$readonly)                                # the dead worker's lock is taken over
  expect_false("run.completed" %in% types_of(q))
  expect_identical(q$state$runs$a1$status, "running")
  out <- .sr_an_settle(q, "a1", "jk", res, cancelling = TRUE)
  expect_identical(out$notice, "Cancelled a1.")
  expect_identical(out$project$state$runs$a1$status, "cancelled")
  expect_identical(tail(types_of(out$project), 1L), "run.cancelled")
})

test_that("kill: a recorded PID whose start time differs is not killed", {
  jf <- withr::local_tempfile()
  writeLines(paste(Sys.getpid(), "1"), jf)        # this very process, with a wrong start time
  expect_false(.sr_an_kill(jf, wait = 1))
  expect_true(.sr_pid_running(Sys.getpid()))
})

test_that("kill: a kill that fails resumes the suspended worker and raises", {
  skip_on_cran()
  bg <- callr::r_bg(function() Sys.sleep(120))
  withr::defer(try(bg$kill_tree(), silent = TRUE))        # processx, not the mocked ps_kill
  h <- ps::ps_handle(bg$get_pid())
  jf <- withr::local_tempfile()
  writeLines(sprintf("%d %.6f", bg$get_pid(), as.numeric(ps::ps_create_time(h))), jf)
  file.create(paste0(jf, ".part")); withr::defer(unlink(paste0(jf, ".part")))
  resumed <- integer()
  real_resume <- ps::ps_resume
  testthat::local_mocked_bindings(
    ps_kill = function(p, ...) stop("access denied"),
    ps_resume = function(p) {
      resumed <<- c(resumed, as.integer(ps::ps_pid(p)))
      real_resume(p)
    },
    .package = "ps")
  expect_error(.sr_an_kill(jf, wait = 1), "could not stop the analysis worker.*access denied")
  expect_true(bg$get_pid() %in% resumed)                  # the worker (and any child of it) resumed
  expect_true(bg$is_alive())
  expect_false(file.exists(paste0(jf, ".part")))
})

test_that("kill: no job file, or an unreadable one, kills nothing", {
  jf <- withr::local_tempfile()
  expect_false(.sr_an_kill(jf, wait = 0.2))
  writeLines("not a pid", jf)
  expect_false(.sr_an_kill(jf, wait = 0.2))
  expect_true(.sr_pid_running(Sys.getpid()))
})
