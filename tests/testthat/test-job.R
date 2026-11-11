# The shared job layer (ui/job.R): job file, kill, result message and the options snapshot.

test_that("job: record writes pid and start time; read returns them", {
  jf <- withr::local_tempfile()
  expect_null(.sr_job_record(jf))
  expect_false(file.exists(paste0(jf, ".part")))
  r <- .sr_job_read(jf)
  expect_identical(r$pid, as.integer(Sys.getpid()))
  expect_true(is.finite(r$time))
  expect_null(.sr_job_record(NULL))
})

test_that("job: read of a missing, empty or malformed file is NULL", {
  jf <- withr::local_tempfile()
  expect_null(.sr_job_read(jf))
  expect_null(.sr_job_read(NULL))
  writeLines(character(), jf)
  expect_null(.sr_job_read(jf))
  writeLines("not a pid", jf)
  expect_null(.sr_job_read(jf))
})

test_that("job: kill of a dead PID is a no-op", {
  jf <- withr::local_tempfile()
  expect_false(.sr_job_kill(jf, wait = 0.2))
  bg <- callr::r_bg(function() 1)
  bg$wait()
  writeLines(sprintf("%d 1.000000", bg$get_pid()), jf)
  expect_false(.sr_job_kill(jf, wait = 0.5))
  expect_true(.sr_pid_running(Sys.getpid()))
})

test_that("job: a recorded PID whose start time differs is not killed", {
  jf <- withr::local_tempfile()
  writeLines(paste(Sys.getpid(), "1"), jf)
  expect_false(.sr_job_kill(jf, wait = 1))
  expect_true(.sr_pid_running(Sys.getpid()))
})

test_that("opts snapshot holds set sr.* settings only", {
  withr::local_options(sr.user = "actor-x", sr.digits = 3L, sr.run_sync = TRUE)
  o <- .sr_job_opts()
  expect_identical(o$sr.user, "actor-x")
  expect_identical(o$sr.digits, 3L)
  expect_false("sr.run_sync" %in% names(o))
  expect_false(any(c("sr.ingest_sync", "sr.jev_transport") %in% names(o)))
})

test_that("opts snapshot never holds jev_key", {
  withr::local_options(sr.jev_key = "SENTINEL-KEY-123", sr.user = "u")
  o <- .sr_job_opts()
  expect_false("sr.jev_key" %in% names(o))
  expect_false(grepl("SENTINEL-KEY-123", paste(deparse(o), collapse = ""), fixed = TRUE))
})

test_that("real worker: the snapshot reaches the worker", {
  skip_on_cran(); skip_if_not_installed("mirai")
  m <- .sr_job_spawn(sr_root, "sr_opt", list("user"), opts = list(sr.user = "actor-x"))
  withr::defer(try(mirai::stop_mirai(m), silent = TRUE))
  expect_identical(mirai::call_mirai(m)$data, "actor-x")
  withr::local_options(sr.jev_key = "SENTINEL-KEY-123")
  m2 <- .sr_job_spawn(sr_root, "sr_opt", list("jev_key", ""))
  withr::defer(try(mirai::stop_mirai(m2), silent = TRUE))
  expect_identical(mirai::call_mirai(m2)$data, "")
})
