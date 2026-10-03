# Tests for the project create/open module (ui/mod_project.R), driven through shiny::testServer.

new_dir <- function() file.path(withr::local_tempdir(.local_envir = parent.frame()), "proj")

# A closed project on disk, ready to be opened from the module.
make_closed <- function(dir, name = "demo") {
  p <- sr_project_create(dir, name, profile = "laptop")
  sr_project_close(p)
  p
}

# A lock held by another machine (fixed fake host: nothing machine-specific is ever written).
write_foreign_lock <- function(paths) {
  .sr_write_json_file(paths$lock, list(host = "other-host", pid = 4242L, user = "someone",
                                       ts = "2026-01-01T00:00:00.000Z"))
}

test_that("create makes a project, reports it and lists its events", {
  d <- new_dir()
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = d, name = "demo", create = 1)
    p <- session$returned()
    expect_equal(p$meta$name, "demo")
    expect_false(p$readonly)
    expect_true(file.exists(file.path(d, "project.json")))
    expect_match(output$status, "demo", fixed = TRUE)
    expect_match(output$events, "project.created", fixed = TRUE)
    sr_project_close(p)
  })
})

test_that("open takes the lock and lists the events of a writable project", {
  d <- new_dir()
  make_closed(d, "reopen")
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = d, name = "", open = 1)
    p <- session$returned()
    expect_equal(p$meta$name, "reopen")
    expect_false(p$readonly)
    expect_match(output$status, "reopen", fixed = TRUE)
    expect_false(grepl("read-only", output$status, fixed = TRUE))
    expect_match(output$events, "project.created", fixed = TRUE)
    sr_project_close(p)
  })
})

test_that("a project locked by another host opens read-only and names the host", {
  d <- new_dir()
  p0 <- make_closed(d)
  write_foreign_lock(p0$paths)
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = d, name = "", open = 1)
    p <- session$returned()
    expect_true(p$readonly)
    expect_match(output$status, "read-only (locked by other-host)", fixed = TRUE)
    expect_match(output$events, "project.created", fixed = TRUE)
  })
  expect_true(file.exists(p0$paths$lock))  # a read-only open never releases the foreign lock
})

test_that("the lock host label falls back to unknown host", {
  d <- new_dir()
  p0 <- make_closed(d)
  expect_identical(.sr_lock_host_label(p0$paths), "unknown host")           # no lock file
  write_foreign_lock(p0$paths)
  expect_identical(.sr_lock_host_label(p0$paths), "other-host")
  writeLines("not json", p0$paths$lock)
  expect_identical(.sr_lock_host_label(p0$paths), "unknown host")           # damaged lock
})

test_that("a pasted path is cleaned: surrounding quotes and spaces are removed", {
  d <- new_dir()
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = paste0("\"", d, "\""), name = "quoted", create = 1)  # "Copy as path"
    p <- session$returned()
    expect_equal(p$meta$name, "quoted")
    expect_equal(p$paths$dir, normalizePath(d, winslash = "/", mustWork = FALSE))
    expect_true(file.exists(file.path(d, "project.json")))
    expect_match(output$status, p$paths$dir, fixed = TRUE)
    sr_project_close(p)
    session$setInputs(dir = paste0("  \"", d, "\"  "), open = 1)
    expect_equal(session$returned()$meta$name, "quoted")
    expect_false(session$returned()$readonly)
    sr_project_close(session$returned())
  })
  d2 <- new_dir()
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = paste0("   ", d2, "   "), name = "spaced", create = 1)
    expect_equal(session$returned()$meta$name, "spaced")
    expect_true(file.exists(file.path(d2, "project.json")))
    sr_project_close(session$returned())
    session$setInputs(dir = paste0("\t", d2, " "), open = 1)
    expect_equal(session$returned()$meta$name, "spaced")
    sr_project_close(session$returned())
  })
})

test_that("a relative path is resolved against the working directory and shown in full", {
  base <- withr::local_tempdir()
  withr::local_dir(base)
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = "rel_proj", name = "rel", create = 1)
    p <- session$returned()
    expect_true(file.exists(file.path(base, "rel_proj", "project.json")))
    expect_equal(p$paths$dir, normalizePath(file.path(base, "rel_proj"), winslash = "/"))
    expect_match(output$status, p$paths$dir, fixed = TRUE)
    sr_project_close(p)
  })
})

test_that("errors are shown in the status and do not crash or open anything", {
  d <- new_dir()
  junk <- file.path(d, "junk")
  dir.create(junk, recursive = TRUE)
  writeLines("x", file.path(junk, "a.txt"))
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = junk, name = "demo", create = 1)   # not empty
    expect_null(session$returned())
    expect_match(output$status, "^Error: ")
    session$setInputs(dir = junk, open = 1)                    # no project.json
    expect_null(session$returned())
    expect_match(output$status, "^Error: ")
    session$setInputs(dir = "", create = 2)                    # no path
    expect_match(output$status, "^Error: ")
    session$setInputs(dir = d, name = "", create = 3)          # no name
    expect_match(output$status, "^Error: ")
    expect_null(session$returned())
  })
})

test_that("a second project closes the first one", {
  a <- new_dir()
  b <- new_dir()
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = a, name = "first", create = 1)
    pa <- session$returned()
    expect_true(file.exists(pa$paths$lock))
    session$setInputs(dir = b, name = "second", create = 2)
    pb <- session$returned()
    expect_equal(pb$meta$name, "second")
    expect_false(file.exists(pa$paths$lock))
    expect_true(file.exists(pb$paths$lock))
    session$setInputs(dir = a, open = 1)                       # reopen the first: the second closes
    expect_false(file.exists(pb$paths$lock))
    expect_equal(session$returned()$meta$name, "first")
    expect_true(file.exists(pa$paths$lock))
    sr_project_close(session$returned())
  })
})

test_that("ending the session releases a project this session opened", {
  d <- new_dir()
  lock <- NULL
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = d, name = "demo", create = 1)
    lock <<- session$returned()$paths$lock
    expect_true(file.exists(lock))
    session$close()
    expect_false(file.exists(lock))
  })
})

test_that("ending the session with no project, or a read-only one, is harmless", {
  expect_no_error(shiny::testServer(mod_project_server, { session$close() }))
  d <- new_dir()
  p0 <- make_closed(d)
  write_foreign_lock(p0$paths)
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = d, open = 1)
    expect_true(session$returned()$readonly)
    session$close()
  })
  expect_true(file.exists(p0$paths$lock))
})

test_that("a project failing the integrity check still opens and the status says why (I3)", {
  d <- new_dir()
  p0 <- make_closed(d)
  b <- readBin(p0$paths$events, "raw", n = file.size(p0$paths$events))
  txt <- sub("\"name\":\"demo\"", "\"name\":\"dem0\"", rawToChar(b), fixed = TRUE)
  writeBin(charToRaw(txt), p0$paths$events)
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = d, name = "", open = 1)
    p <- session$returned()
    expect_false(is.null(p))
    expect_false(p$readonly)
    expect_match(output$status, "Opened project 'demo'", fixed = TRUE)
    expect_match(output$status, "Integrity check failed: ", fixed = TRUE)
    expect_match(output$status, "seq 1", fixed = TRUE)
    sr_project_close(p)
  })
})

test_that("a clean project opens with no integrity warning (I3)", {
  d <- new_dir()
  make_closed(d)
  shiny::testServer(mod_project_server, {
    session$setInputs(dir = d, name = "", open = 1)
    expect_false(grepl("Integrity", output$status, fixed = TRUE))
    sr_project_close(session$returned())
  })
})
