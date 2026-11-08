test_that("sr_freeze writes the manifest and the freeze.created event", {
  p <- .fx_writer_project()
  p <- sr_freeze(p)
  fz <- sr_freezes(p)
  expect_identical(fz$label, "submission-v1")
  m <- sr_freeze_manifest(p, "submission-v1")
  expect_identical(m$format, "statrouter-freeze")
  expect_identical(m$sap_hash, p$state$sap$lock$sap_hash)
  expect_identical(m$project_id, p$meta$id)
  expect_setequal(names(m$runs), c("t1", "a_lr", "a_cox", "a_rmst", "a_mp", "x_age"))
  kinds <- vapply(m$files, `[[`, "", "kind")
  expect_true(all(c("store_manifest", "part", "extract", "fit", "code") %in% kinds))
  paths <- vapply(m$files, `[[`, "", "path")
  expect_true(all(c("out/freezes/submission-v1/analysis.R", "out/freezes/submission-v1/renv.lock") %in% paths))
  expect_false(any(grepl("^[A-Za-z]:|^/", paths)))            # relative only
  mf <- file.path(.sr_freeze_dir(p, "submission-v1"), "manifest.json")
  expect_identical(fz$manifest_sha, .sr_sha256(.sr_read_bytes(mf)))
  ev <- .sr_events_load(p$paths$events)$events
  last <- ev[[length(ev)]]
  expect_identical(last$type, "freeze.created")
  expect_identical(m$events$seq, last$seq - 1L)
  expect_identical(m$events$sha256, .sr_events_prefix_sha(p$paths$events, last$seq - 1L))
  expect_identical(p$state$lib_baseline$hash, m$libsnap_hash)
})

test_that("labels: next label, pattern, reuse", {
  p <- .fx_writer_project()
  expect_identical(sr_freeze_next_label(p), "submission-v1")
  p <- sr_freeze(p)
  expect_identical(sr_freeze_next_label(p), "revision-r1")
  expect_error(sr_freeze(p, "submission-v1"), "already")
  expect_error(sr_freeze(p, "Bad Label"), "label")
  p <- sr_freeze(p)
  expect_identical(sr_freezes(p)$label, c("submission-v1", "revision-r1"))
  expect_identical(sr_freeze_next_label(p), "revision-r2")
})

test_that("the freeze gate refuses an unlocked SAP, an unhashed source, a primary without an ok run, a missing fit", {
  p0 <- .fx_project()
  expect_error(sr_freeze(p0), "not locked")
  p <- .fx_writer_project()
  t <- p$state$tables$t; t$source$sha256 <- NA_character_; p$state$tables$t <- t
  expect_error(sr_freeze(p), "SHA-256 of the source.*'t'")
  p <- .fx_writer_project()
  p$state$runs$a_lr$ok_key <- NA_character_
  expect_error(sr_freeze(p), "primary analysis 'a_lr' has no successful run")
  p <- .fx_writer_project()
  file.remove(file.path(p$paths$fits, paste0(p$state$runs$a_cox$ok_key, ".rds")))
  expect_error(sr_freeze(p), "stored result of 'a_cox' is missing")
  expect_false(dir.exists(file.path(p$paths$out, "freezes", "submission-v1")))
})

test_that("sr_freeze_verify: clean, tampered, absent data", {
  p <- .fx_writer_project()
  p <- sr_freeze(p)
  v <- sr_freeze_verify(p, "submission-v1")
  expect_true(v$ok); expect_gt(v$checked, 5L); expect_length(v$absent, 0L)
  m <- sr_freeze_manifest(p, "submission-v1")
  ex <- Filter(function(f) f$kind == "extract", m$files)[[1]]$path
  b <- .sr_read_bytes(file.path(p$paths$dir, ex)); b[length(b)] <- as.raw(bitwXor(as.integer(b[length(b)]), 1L))
  writeBin(b, file.path(p$paths$dir, ex))
  v <- sr_freeze_verify(p, "submission-v1")
  expect_false(v$ok); expect_match(v$problems, ex, fixed = TRUE, all = FALSE)
  p <- .fx_writer_project(); p <- sr_freeze(p)
  part <- Filter(function(f) f$kind == "part", sr_freeze_manifest(p, "submission-v1")$files)[[1]]$path
  file.remove(file.path(p$paths$dir, part))
  v <- sr_freeze_verify(p, "submission-v1")
  expect_true(v$ok); expect_identical(v$absent, part)
  v <- sr_freeze_verify(p, "submission-v1", data = FALSE)
  expect_true(v$ok); expect_length(v$absent, 0L)
})

test_that("events after the freeze do not change its verification; a read-only project is refused", {
  p <- .fx_writer_project()
  p <- sr_freeze(p)
  p <- sr_project_append(p, "note", list(text = "after the freeze"))
  expect_true(sr_freeze_verify(p, "submission-v1")$ok)
  ro <- p; ro$readonly <- TRUE
  expect_error(sr_freeze(ro, "revision-r1"), "read-only")
})

test_that("only a primary analysis without an ok run blocks the freeze", {
  p <- .fx_writer_project()
  p$state$runs$a_mp$ok_key <- NA_character_            # a_mp is a pre-specified sensitivity analysis
  expect_identical(p$state$analyses$a_mp$label, "sensitivity")
  p <- sr_freeze(p)
  m <- sr_freeze_manifest(p, "submission-v1")
  expect_false("a_mp" %in% names(m$runs))
  expect_true("a_lr" %in% names(m$runs))
  p2 <- .fx_writer_project()
  p2$state$runs$a_lr$ok_key <- NA_character_
  expect_error(sr_freeze(p2), "primary analysis 'a_lr' has no successful run")
})

test_that("the gate refuses a missing extract and a project that does not verify", {
  p <- .fx_writer_project()
  sha <- p$state$runs$a_lr$ok_extract_sha
  file.remove(file.path(p$paths$dir, "data", "extracts", paste0(sha, ".rds")))
  expect_error(sr_freeze(p), "the extract of .* is missing")
  p <- .fx_writer_project()
  b <- .sr_read_bytes(p$paths$audit); b[length(b) - 5L] <- as.raw(bitwXor(as.integer(b[length(b) - 5L]), 1L))
  writeBin(b, p$paths$audit)
  expect_error(sr_freeze(p), "the project does not verify")
  expect_false(dir.exists(file.path(p$paths$out, "freezes", "submission-v1")))
})

test_that("sr_freeze_verify and sr_freeze_manifest refuse a non-scalar label", {
  p <- .fx_writer_project()
  expect_error(sr_freeze_verify(p, c("a", "b")), "unknown freeze label")
  expect_error(sr_freeze_manifest(p, NA_character_), "unknown freeze label")
})
