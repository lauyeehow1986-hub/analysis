test_that("full export: the archive holds the index and the frozen project, nothing else", {
  p <- sr_freeze(.fx_writer_project())
  out <- file.path(withr::local_tempdir(), "study.srproj")
  p <- sr_srproj_export(p, out, data = "full")
  expect_true(file.exists(out)); expect_false(file.exists(paste0(out, ".partial")))
  e <- .sr_tar_entries(out); files <- e$name[e$type == "0"]
  expect_true(all(c("srproj.json", "project.json", "events.jsonl", "audit.log",
                    "out/freezes/submission-v1/manifest.json") %in% files))
  m <- sr_freeze_manifest(p, "submission-v1")
  expect_true(all(vapply(m$files, `[[`, "", "path") %in% files))
  expect_false(any(grepl("(^|/)[.]lock$|[.]partial|[.]tmp$", files)))
  x <- p$state$srproj_exports[[1]]
  expect_identical(x$file, "study.srproj"); expect_identical(x$data_mode, "full")
  expect_identical(x$archive_sha256, sr_sha256_file(out))
})

test_that("lean export omits parts and sources; auto follows the size threshold", {
  p <- sr_freeze(.fx_writer_project())
  d <- withr::local_tempdir()
  p <- sr_srproj_export(p, file.path(d, "lean.srproj"), data = "lean")
  files <- .sr_tar_entries(file.path(d, "lean.srproj"))$name
  expect_false(any(grepl("/part-[0-9]+[.]parquet$", files)))
  expect_true(any(grepl("^data/store/t/manifest[.]json$", files)))
  expect_true(any(grepl("^data/extracts/", files)) && any(grepl("^fits/", files)))
  expect_identical(p$state$srproj_exports[[1]]$parts_omitted, list("t"))
  withr::local_options(sr.freeze_full_max_bytes = 0)
  p <- sr_srproj_export(p, file.path(d, "auto.srproj"))
  expect_identical(p$state$srproj_exports[[2]]$data_mode, "lean")
})

test_that("work after the freeze is not in the archive (events, audit, runs, exports, later freezes)", {
  p <- sr_freeze(.fx_writer_project())
  seq_fz <- p$state$freezes[["submission-v1"]]$events_seq
  p <- sr_project_append(p, "note", list(text = "revision work"))
  p <- sr_export(p)                                   # a post-freeze manuscript export
  p <- sr_freeze(p)                                   # revision-r1
  out <- file.path(withr::local_tempdir(), "v1.srproj")
  p <- sr_srproj_export(p, out, label = "submission-v1", data = "lean")
  x <- withr::local_tempdir(); utils::untar(out, exdir = x, tar = "internal")
  ev <- .sr_events_load(file.path(x, "events.jsonl"))$events
  expect_identical(ev[[length(ev)]]$type, "freeze.created")
  expect_identical(ev[[length(ev)]]$seq, seq_fz + 1L)
  expect_true(sr_audit_verify(file.path(x, "audit.log"))$ok)
  expect_false(dir.exists(file.path(x, "out", "freezes", "revision-r1")))
  names_in <- .sr_tar_entries(out)$name
  expect_false(any(grepl("^out/freezes/revision-r1/", names_in)))
  m1 <- sr_freeze_manifest(p, "submission-v1")
  ex1 <- vapply(Filter(function(f) f$kind == "export", m1$files), `[[`, "", "path")
  expect_setequal(names_in[grepl("^out/exports/", names_in)], ex1)   # only what existed at the freeze
  live <- list.files(file.path(p$paths$out, "exports"), recursive = TRUE)
  expect_true(length(live) > length(ex1))                           # the live project holds more than the archive
})

test_that("refusals and a failure halfway leave nothing behind", {
  p <- sr_freeze(.fx_writer_project())
  d <- withr::local_tempdir()
  expect_error(sr_srproj_export(p, file.path(d, "x.zip")), "[.]srproj")
  file.create(file.path(d, "exists.srproj"))
  expect_error(sr_srproj_export(p, file.path(d, "exists.srproj")), "already exists")
  expect_error(sr_srproj_export(p, file.path(d, "nope", "a.srproj")), "folder")
  expect_error(sr_srproj_export(p, file.path(d, "a.srproj"), label = "revision-r9"), "unknown freeze")
  expect_error(sr_srproj_export(.fx_writer_project(), file.path(d, "a.srproj")), "no freeze")
  n_ev <- length(.sr_events_load(p$paths$events)$events)
  real_tar <- .sr_srproj_tar
  assign(".sr_srproj_tar", function(...) stop("disk full"), envir = globalenv())
  withr::defer(assign(".sr_srproj_tar", real_tar, envir = globalenv()))
  expect_error(sr_srproj_export(p, file.path(d, "b.srproj")), "disk full")
  expect_identical(list.files(d, all.files = TRUE, no.. = TRUE), "exists.srproj")
  expect_false(any(grepl("srproj", list.files(p$paths$out, all.files = TRUE, recursive = TRUE, include.dirs = TRUE))))
  expect_identical(length(.sr_events_load(p$paths$events)$events), n_ev)
})

test_that("a tampered frozen file blocks the export", {
  p <- sr_freeze(.fx_writer_project())
  fit <- Filter(function(f) f$kind == "fit", sr_freeze_manifest(p, "submission-v1")$files)[[1]]$path
  cat("x", file = file.path(p$paths$dir, fit), append = TRUE)
  expect_error(sr_srproj_export(p, file.path(withr::local_tempdir(), "a.srproj")), "no longer verifies")
})

test_that("no member header carries an owner or group name, and the archive still extracts", {
  p <- sr_freeze(.fx_writer_project())
  out <- file.path(withr::local_tempdir(), "o.srproj")
  p <- sr_srproj_export(p, out, data = "lean")
  con <- gzfile(out, "rb")
  n <- 0L
  repeat {
    h <- readBin(con, "raw", 512L)
    if (!length(h) || all(h == as.raw(0L))) break
    expect_true(all(h[266:329] == as.raw(0L)))
    .sr_tar_skip(con, ceiling(.sr_tar_size(h[125:136]) / 512) * 512)
    n <- n + 1L
  }
  close(con)
  expect_true(n > 5L)
  x <- withr::local_tempdir()
  utils::untar(out, exdir = x, tar = "internal")
  expect_true(file.exists(file.path(x, "srproj.json")))
  idx <- .sr_read_json_file(file.path(x, "srproj.json"))
  for (f in idx$files) expect_identical(sr_sha256_file(file.path(x, f$path)), f$sha256)
  expect_false(file.exists(paste0(out, ".raw")))
})
