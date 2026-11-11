# Tests for ui/write_view.R: the pure views behind the Write panel. Synthetic data only.

test_that("presets: every preset is marked not verified", {
  d <- .sr_wp_presets()
  expect_identical(names(d), c("name", "label"))
  all <- .sr_journal_all()
  expect_identical(d$name, vapply(all, function(x) x$name, ""))
  ver <- vapply(all, function(x) isTRUE(x$verified), NA)
  expect_identical(d$label[!ver], paste0(d$name[!ver], " (not verified)"))
  expect_identical(d$label[ver], d$name[ver])
  expect_true(any(grepl("not verified", d$label)))
})

test_that("opts: a valid set returns opts; suppress_k 3 returns the core message and no opts", {
  ok <- .sr_wp_opts("default", 5, TRUE, 0.05)
  expect_null(ok$error)
  expect_identical(ok$opts, sr_writer_opts("default", 5, "label", 0.05))
  off <- .sr_wp_opts("default", 5, FALSE, 0.05)
  expect_identical(off$opts$table2, "suppress")
  bad <- .sr_wp_opts("default", 3, TRUE, 0.05)
  expect_null(bad$opts)
  expect_match(bad$error, "suppress_k")
})

test_that("gates: a missing project gives one row", {
  g <- .sr_wp_gates(NULL, FALSE)
  expect_identical(nrow(g), 1L)
  expect_identical(g$gate, "Project open")
  expect_identical(.sr_wp_gate_reason(g), "open a project first")
})

test_that("gates: an unlocked project fails SAP locked and names the reason", {
  p <- .fx_ui_project()
  g <- .sr_wp_gates(p, FALSE)
  expect_identical(g$gate, c("Project writable", "SAP locked", "Pre-specified analyses run", "App not busy"))
  expect_false(g$ok[g$gate == "SAP locked"])
  expect_identical(g$reason[g$gate == "SAP locked"], "lock the SAP first")
  expect_identical(.sr_wp_gate_reason(g), "lock the SAP first")
})

test_that("gates: a locked and run project passes all four; busy fails only App not busy", {
  p <- .fx_writer_project()
  g <- .sr_wp_gates(p, FALSE)
  expect_true(all(g$ok))
  expect_true(all(g$reason == ""))
  expect_null(.sr_wp_gate_reason(g))
  b <- .sr_wp_gates(p, TRUE)
  expect_identical(b$gate[!b$ok], "App not busy")
  expect_identical(b$reason[!b$ok], .sr_ui_busy_text)
})

test_that("gates: a pre-specified analysis without an ok run is named", {
  p <- .fx_writer_build(env = environment(), unrun = "a_rmst")
  g <- .sr_wp_gates(p, FALSE)
  r <- g$reason[g$gate == "Pre-specified analyses run"]
  expect_false(g$ok[g$gate == "Pre-specified analyses run"])
  expect_match(r, "a_rmst")
  expect_match(r, "^no ok run: ")
})

test_that("gates: a read-only project fails Project writable", {
  p <- .fx_writer_project()
  p$readonly <- TRUE
  g <- .sr_wp_gates(p, FALSE)
  expect_false(g$ok[g$gate == "Project writable"])
  expect_match(g$reason[g$gate == "Project writable"], "read-only")
})

test_that("guard rows: block findings come first", {
  f <- data.frame(block = c("b2", "b1", "b3", "b0"), analysis = NA_character_,
                  rule = c("r1", "r2", "r3", "r4"), severity = c("warn", "block", "block", "warn"),
                  detail = "d", stringsAsFactors = FALSE)
  o <- .sr_wp_guard_rows(f)
  expect_identical(names(o), c("severity", "rule", "block", "analysis", "detail"))
  expect_identical(o$severity, c("block", "block", "warn", "warn"))
  expect_identical(o$block, c("b1", "b3", "b0", "b2"))
})

test_that("preview: a suppressed count renders as <5 and no {{ token remains", {
  p <- .fx_writer_project()
  m <- sr_doc_model(p)
  tab <- sr_ph_table(m$reg)
  if (!any(tab$suppressed)) {
    i <- which(tab$kind == "count" & !is.na(tab$value))[1L]
    expect_false(is.na(i))
    m$reg$rows$suppressed[m$reg$rows$id == tab$id[i]] <- TRUE
    m$reg$rows$text[m$reg$rows$id == tab$id[i]] <- "<5"
    id <- tab$id[i]
    m$main <- c(m$main, list(list(type = "para", id = "zz", text = paste0("n = {{", id, "}}"),
                                  analysis = NA_character_, section = "results")))
  }
  html <- as.character(.sr_wp_preview(m, withr::local_tempdir(), sr_journal_preset()))
  expect_false(grepl("{{", html, fixed = TRUE))
  expect_match(html, "&lt;5")
  expect_match(html, "<h3", fixed = TRUE)
  expect_match(html, "Supplement")
})

test_that("history: one row per export with ok status", {
  p <- .fx_writer_project()
  p <- sr_export(p)
  h <- .sr_wp_history(p)
  expect_identical(names(h), c("kind", "id", "created", "preset", "n_files", "on_disk", "status"))
  expect_identical(nrow(h), nrow(sr_exports(p)))
  expect_true(all(h$on_disk))
  expect_identical(h$status, rep("ok", nrow(h)))
  expect_false(any(is.na(h$created)))
})

test_that("history: a deleted export folder is missing on disk", {
  p <- .fx_writer_project()
  p <- sr_export(p)
  id <- sr_exports(p)$id[1L]
  unlink(file.path(p$paths$out, "exports", id), recursive = TRUE)
  h <- .sr_wp_history(p)
  expect_false(h$on_disk[h$id == id])
  expect_identical(h$status[h$id == id], "missing on disk")
})

test_that("history: no exports gives an empty frame", {
  p <- .fx_writer_project()
  h <- .sr_wp_history(p)
  expect_identical(nrow(h), 0L)
  expect_identical(names(h), c("kind", "id", "created", "preset", "n_files", "on_disk", "status"))
})

test_that("verify lines: problems per bad file; numbers only when they differ", {
  v <- list(files = data.frame(path = c("out/a", "out/b", "out/c"), status = c("ok", "changed", "missing"),
                               stringsAsFactors = FALSE),
            numbers = NULL, numbers_ok = TRUE)
  o <- .sr_wp_verify_lines(v)
  expect_identical(o$problems, c("changed: out/b", "missing: out/c"))
  expect_null(o$numbers)
  expect_null(o$note)
  expect_identical(.sr_wp_verify_lines(c(v, list(numbers_note = "gone")))$note, "gone")
  v$numbers <- data.frame(id = c("E1", "E2"), ok = c(TRUE, FALSE), want = c(1, 2.123456), got = c(1, 2.5),
                          stringsAsFactors = FALSE)
  v$numbers_ok <- FALSE
  o2 <- .sr_wp_verify_lines(v)
  expect_identical(nrow(o2$numbers), 1L)
  expect_identical(o2$numbers$id, "E2")
  expect_equal(o2$numbers$want, 2.123)
  v$files$status <- "ok"
  expect_identical(.sr_wp_verify_lines(v)$problems, character())
})

test_that("checklist: the STROBE frame has item, section, label, status, detail", {
  p <- .fx_writer_project()
  ck <- .sr_wp_checklist(sr_doc_model(p))
  expect_null(ck$note)
  expect_identical(names(ck$df), c("item", "section", "label", "status", "detail"))
  expect_gt(nrow(ck$df), 5L)
})

test_that("checklist: a CONSORT model gives the note instead", {
  m <- list(supplement = list(list(type = "para", id = "checklist_note", text = "The CONSORT checklist is not yet produced.",
                                   analysis = NA_character_, section = "methods")))
  ck <- .sr_wp_checklist(m)
  expect_null(ck$df)
  expect_match(ck$note, "CONSORT")
})
