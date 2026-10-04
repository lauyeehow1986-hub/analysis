# Fixture helpers for P1b tests: the synthetic cardiac cohort and throwaway projects.
# The generator lives in samples/ (a deliverable in its own right); source it from the repo root.
local({
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  source(file.path(root, "samples", "gen_cardiac.R"), local = FALSE)
})

# A fresh, open, writable project in a temp dir that is removed (and the project closed) when the
# calling test (or `env`) finishes.
.fx_project <- function(env = parent.frame()) {
  dir <- file.path(withr::local_tempdir(.local_envir = env), "proj")
  p <- sr_project_create(dir, "fixture", profile = "laptop")
  withr::defer(sr_project_close(p), envir = env)
  p
}

# Write a CSV as UTF-8 without row names; `...` goes to utils::write.csv (e.g. na = "").
.fx_write_csv <- function(df, path, ...) {
  utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8", ...)
  invisible(path)
}

# A project with the synthetic cardiac cohort ingested as table `cardiac`. Ingest exists from P1b
# Task 5 on; until then the project is returned with nothing ingested.
.fx_cardiac_project <- function(n = 2000L, defects = TRUE, env = parent.frame()) {
  p <- .fx_project(env = env)
  s <- sr_sample_cardiac(n = n, defects = defects)
  csv <- file.path(dirname(p$paths$dir), "cardiac.csv")
  .fx_write_csv(s$data, csv)
  if (exists("sr_ingest")) p <- sr_ingest(p, csv, table = "cardiac")
  p
}
