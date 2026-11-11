test_that("freeze gates: an unlocked project fails SAP locked; a locked, run project passes all", {
  g <- .sr_ar_freeze_gates(.fx_ui_project())
  expect_identical(names(g), c("gate", "ok", "reason"))
  expect_identical(nrow(g), 6L)
  expect_false(g$ok[g$gate == "SAP locked"])
  expect_match(g$reason[g$gate == "SAP locked"], "the SAP is not locked; lock it before freezing", fixed = TRUE)
  expect_true(all(g$reason[g$ok] == ""))
  g2 <- .sr_ar_freeze_gates(.fx_writer_project())
  expect_true(all(g2$ok))
  expect_true(all(g2$reason == ""))
})

test_that("label problem: bad pattern and used label", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  expect_identical(.sr_ar_label_problem(p, "Bad Label"), "the label must match ^[a-z][a-z0-9-]{0,39}$")
  expect_identical(.sr_ar_label_problem(p, NA_character_), "the label must match ^[a-z][a-z0-9-]{0,39}$")
  expect_identical(.sr_ar_label_problem(p, "v1"), "freeze label 'v1' is already used")
  expect_null(.sr_ar_label_problem(p, "v2"))
})

test_that("freezes table and verify rows", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  f <- .sr_ar_freezes(p)
  expect_identical(names(f), c("label", "created", "events_seq", "libsnap"))
  expect_identical(f$label, "v1")
  expect_identical(f$libsnap, substr(sr_freezes(p)$libsnap_hash, 1L, 12L))
  expect_lte(nchar(f$libsnap), 12L)
  r <- .sr_ar_verify_rows(sr_freeze_verify(p, "v1", data = FALSE))
  expect_identical(names(r), c("ok", "problems", "absent"))
  expect_true(r$ok); expect_length(r$problems, 0L)
  r2 <- .sr_ar_verify_rows(list(ok = FALSE, problems = "x", absent = c("a", "b")))
  expect_false(r2$ok); expect_identical(r2$absent, c("a", "b")); expect_identical(r2$problems, "x")
})

test_that("diff tables: an unchanged project gives empty sections; results diff rounds only", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  d <- .sr_ar_diff_tables(sr_freeze_diff(p, "v1"))
  expect_type(d, "list")
  expect_false("sap" %in% names(d))
  expect_false("library" %in% names(d))
  expect_true(all(vapply(d, is.data.frame, NA)))
  r <- .sr_ar_results_diff(data.frame(analysis = "a", status = "changed", n_values = 3L,
                                      max_abs_diff = 0.123456789, max_rel_diff = 1234.5678,
                                      stringsAsFactors = FALSE))
  expect_identical(r$max_abs_diff, 0.1235)
  expect_identical(r$max_rel_diff, 1235)
  expect_identical(r$n_values, 3L)
  rr <- .sr_ar_results_diff(sr_freeze_results_diff(p, "v1"))
  expect_true(all(rr$status == "same_run"))
})

test_that("srproj plan matches what sr_srproj_export picks", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  out <- file.path(withr::local_tempdir(), "a.srproj")
  plan <- .sr_ar_srproj_plan(p, "v1", "auto")
  expect_identical(names(plan), c("mode", "bytes", "absent"))
  expect_gt(plan$bytes, 0)
  p <- sr_srproj_export(p, out, label = "v1", data = "auto")
  expect_identical(p$state$srproj_exports[[1]]$data_mode, plan$mode)
  withr::local_options(sr.freeze_full_max_bytes = 0)
  plan2 <- .sr_ar_srproj_plan(p, "v1", "auto")
  expect_identical(plan2$mode, "lean")
  p <- sr_srproj_export(p, file.path(dirname(out), "b.srproj"), label = "v1", data = "auto")
  expect_identical(p$state$srproj_exports[[2]]$data_mode, "lean")
  expect_identical(.sr_ar_srproj_plan(p, "v1", "full")$mode, "full")
})

test_that("path problem: extension, existing and missing parent", {
  d <- withr::local_tempdir()
  expect_identical(.sr_ar_path_problem(file.path(d, "a.zip")), "the path must end in .srproj")
  ok <- file.path(d, "a.srproj")
  expect_null(.sr_ar_path_problem(ok))
  writeLines("x", ok)
  expect_identical(.sr_ar_path_problem(ok), paste0(ok, " already exists"))
  miss <- file.path(d, "nope", "b.srproj")
  expect_identical(.sr_ar_path_problem(miss), paste0("the folder ", dirname(miss), " does not exist"))
})

test_that("actor warning only when sr.user is unset", {
  withr::local_options(sr.user = NULL)
  withr::local_envvar(SR_USER = NA)
  w <- .sr_ar_actor_warning()
  expect_identical(w, SR_SRPROJ_ACTOR_WARNING)
  expect_match(w, "OS user name", fixed = TRUE)
  withr::local_options(sr.user = "analyst-1")
  expect_null(.sr_ar_actor_warning())
})

test_that("export cleanup: removes the archive, its .partial and the staging folder when no srproj.exported was written", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  d <- withr::local_tempdir()
  path <- file.path(d, "x.srproj")
  stg <- file.path(p$paths$out, ".srproj-v1.partial")
  writeLines("x", path); writeLines("y", paste0(path, ".partial"))
  dir.create(file.path(stg, "sub"), recursive = TRUE); writeLines("z", file.path(stg, "sub", "f"))
  n <- .sr_ar_n_events(p$paths$dir)
  expect_identical(.sr_ar_export_cleanup(p$paths$dir, path, "v1", n), "removed")
  expect_false(file.exists(path)); expect_false(file.exists(paste0(path, ".partial")))
  expect_false(dir.exists(stg))
})

test_that("export cleanup: the archive is not hashed when no srproj.exported event exists", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  path <- file.path(withr::local_tempdir(), "x.srproj")
  writeLines("x", path)
  n <- .sr_ar_n_events(p$paths$dir)
  hashed <- 0L
  env <- environment(.sr_ar_export_cleanup)
  real <- get("sr_sha256_file", envir = env)
  withr::defer(assign("sr_sha256_file", real, envir = env))
  assign("sr_sha256_file", function(...) { hashed <<- hashed + 1L; "x" }, envir = env)
  expect_identical(.sr_ar_export_cleanup(p$paths$dir, path, "v1", n), "removed")
  expect_identical(hashed, 0L)
})

test_that("export cleanup: a finished export is kept and reported", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  path <- file.path(withr::local_tempdir(), "x.srproj")
  n <- .sr_ar_n_events(p$paths$dir)
  p <- sr_srproj_export(p, path, label = "v1", data = "lean")
  expect_identical(.sr_ar_last_type(p$paths$dir), "srproj.exported")
  expect_identical(.sr_ar_export_cleanup(p$paths$dir, path, "v1", n), "finished")
  expect_true(file.exists(path))
})

test_that("export cleanup: a path that existed before the job (keep) is never removed", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  d <- withr::local_tempdir()
  path <- file.path(d, "x.srproj")
  writeLines("mine", path); writeLines("part", paste0(path, ".partial"))
  stg <- file.path(p$paths$out, ".srproj-v1.partial")
  dir.create(stg, recursive = TRUE)
  n <- .sr_ar_n_events(p$paths$dir)
  expect_identical(.sr_ar_export_cleanup(p$paths$dir, path, "v1", n, keep = c(path)), "removed")
  expect_true(file.exists(path))
  expect_false(file.exists(paste0(path, ".partial")))
  expect_false(dir.exists(stg))
  expect_identical(readLines(path), "mine")
})

test_that("export cleanup: a finished export followed by another event is still finished", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  path <- file.path(withr::local_tempdir(), "x.srproj")
  n <- .sr_ar_n_events(p$paths$dir)
  p <- sr_srproj_export(p, path, label = "v1", data = "lean")
  p <- sr_project_append(p, "note", list(text = "after"))
  expect_identical(.sr_ar_last_type(p$paths$dir), "note")
  expect_identical(.sr_ar_export_cleanup(p$paths$dir, path, "v1", n), "finished")
  expect_true(file.exists(path))
  expect_identical(.sr_ar_export_cleanup(p$paths$dir, path, "v1", .sr_ar_n_events(p$paths$dir)), "removed")
})

# A torn last line before the job is truncated away by the export's append: counted as a line, it would hide
# the srproj.exported event that replaces it, and the cleanup would delete a finished archive.
test_that("export cleanup: a torn last event line before the job does not hide the finished export", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  ev <- p$paths$events
  con <- file(ev, "ab")
  writeBin(charToRaw("{\"seq\":99,\"type\":\"tor"), con)            # an append cut short (no LF)
  close(con)
  n <- .sr_ar_n_events(p$paths$dir)
  expect_warning(evs <- sr_events_read(ev), "torn event line")
  expect_identical(n, length(evs))                                 # the torn fragment is not counted
  path <- file.path(withr::local_tempdir(), "x.srproj")
  p <- suppressWarnings(sr_srproj_export(p, path, label = "v1", data = "lean"))
  expect_identical(.sr_ar_n_events(p$paths$dir), n + 1L)
  expect_identical(.sr_ar_export_cleanup(p$paths$dir, path, "v1", n), "finished")
  expect_true(file.exists(path))
})

test_that("export cleanup: an srproj.exported event for another archive does not keep this one", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  path <- file.path(withr::local_tempdir(), "x.srproj")
  n <- .sr_ar_n_events(p$paths$dir)
  p <- sr_project_append(p, "srproj.exported", list(label = "v1", file = "other.srproj",
                                                     archive_sha256 = strrep("0", 64)))
  writeLines("half", path)
  expect_identical(.sr_ar_export_cleanup(p$paths$dir, path, "v1", n), "removed")
  expect_false(file.exists(path))
  # an event naming this file but another archive (other bytes) does not keep it either
  writeLines("half", path)
  p <- sr_project_append(p, "srproj.exported", list(label = "v1", file = "x.srproj",
                                                     archive_sha256 = strrep("0", 64)))
  expect_identical(.sr_ar_export_cleanup(p$paths$dir, path, "v1", n), "removed")
  expect_false(file.exists(path))
})

test_that("verify worker on a project the session holds runs read-only and returns the verify", {
  p <- sr_freeze(.fx_writer_project(), "v1")
  # the worker is another process in the app; in-process, make the lock look like that of another live process
  lk <- .sr_lock_me()
  lk$pid <- as.integer(ps::ps_ppid())
  .sr_write_json_file(p$paths$lock, lk)
  expect_true(sr_project_open(p$paths$dir)$readonly)
  v <- .sr_ar_verify_worker(p$paths$dir, "v1", NULL)
  expect_true(v$ok)
  expect_identical(.sr_lock_read(p$paths$lock)$pid, lk$pid)   # the lock of the session is untouched
  e <- .sr_ar_export_worker(p$paths$dir, file.path(withr::local_tempdir(), "x.srproj"), "v1", "lean", NULL)
  expect_false(e$ok)
  expect_identical(e$error, "the project is locked by another session")
})
