# Tests for io/duck.R: the locked-down DuckDB connection and its SQL guard.

.duck_setting <- function(con, name) {
  DBI::dbGetQuery(con, sprintf("SELECT current_setting('%s') AS v", name))$v
}

.duck_project <- function(profile = "laptop", env = parent.frame()) {
  dir <- file.path(withr::local_tempdir(.local_envir = env), "proj")
  p <- sr_project_create(dir, "duck", profile = profile)
  withr::defer(sr_project_close(p), envir = env)
  withr::defer(sr_duck_close(p), envir = env)   # runs first: closes DuckDB before the folder goes
  p
}

.refused <- function(sql) {
  tryCatch({ .sr_duck_guard(sql); FALSE }, error = function(e) grepl("refused", conditionMessage(e)))
}

test_that("the connection is locked down: extensions off, configuration locked", {
  p <- .duck_project()
  con <- sr_duck(p)
  expect_true(DBI::dbIsValid(con))
  expect_false(as.logical(.duck_setting(con, "autoinstall_known_extensions")))
  expect_false(as.logical(.duck_setting(con, "autoload_known_extensions")))
  expect_false(as.logical(.duck_setting(con, "allow_community_extensions")))
  expect_true(as.logical(.duck_setting(con, "lock_configuration")))
  expect_false(as.logical(.duck_setting(con, "preserve_insertion_order")))
})

test_that("the extension directory is project-owned and empty; temp lives under the data dir", {
  p <- .duck_project()
  con <- sr_duck(p)
  data <- sr_project_paths(p$paths$dir)$data
  norm <- function(x) normalizePath(gsub(intToUtf8(92L), "/", x, fixed = TRUE), winslash = "/", mustWork = FALSE)
  ext <- norm(.duck_setting(con, "extension_directory"))
  expect_identical(ext, norm(file.path(data, "tmp", "duckdb_ext")))
  expect_true(dir.exists(ext))
  expect_identical(norm(.duck_setting(con, "temp_directory")), norm(file.path(data, "tmp")))
  expect_length(list.files(ext, all.files = TRUE, recursive = TRUE, no.. = TRUE), 0L)
})

test_that("memory_limit and threads follow the profile caps", {
  for (prof in c("laptop", "locked16")) {
    p <- .duck_project(prof)
    con <- sr_duck(p)
    caps <- sr_profile_caps(prof)
    ref <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
    DBI::dbExecute(ref, sprintf("SET memory_limit='%dGB'", as.integer(caps$duckdb_memory_gb)))
    expect_identical(.duck_setting(con, "memory_limit"), .duck_setting(ref, "memory_limit"))
    DBI::dbDisconnect(ref, shutdown = TRUE)
    expect_identical(as.integer(.duck_setting(con, "threads")), as.integer(caps$duckdb_threads))
  }
})

test_that("sr_duck is cached per project dir and sr_duck_close closes and forgets it", {
  p <- .duck_project()
  con <- sr_duck(p)
  expect_identical(sr_duck(p), con)
  sr_duck_close(p)
  expect_false(DBI::dbIsValid(con))
  con2 <- sr_duck(p)
  expect_true(DBI::dbIsValid(con2))
  expect_false(identical(con2, con))
  expect_silent(sr_duck_close(p))
  expect_silent(sr_duck_close(p))   # closing twice is fine
})

test_that("guarded exec and query run ordinary SQL", {
  p <- .duck_project()
  con <- sr_duck(p)
  expect_identical(sr_duck_query(con, "SELECT 1 AS x")$x, 1L)
  sr_duck_exec(con, "CREATE TABLE t (a INTEGER)")
  sr_duck_exec(con, "INSERT INTO t VALUES (1), (2)")
  sr_duck_exec(con, "UPDATE t SET a = a + 1")                     # SET inside a statement is fine
  expect_identical(sr_duck_query(con, "SELECT sum(a) AS s FROM t")$s, 5)
  expect_identical(sr_duck_query(con, "SELECT 'LOAD' AS w, 'a set' AS v")$w, "LOAD")
  expect_identical(sr_duck_query(con, "SELECT 1 AS x -- trailing comment")$x, 1L)
  expect_identical(sr_duck_query(con, "SELECT 'x--y' AS w")$w, "x--y")
  expect_identical(sr_duck_query(con, "SELECT 'C:/data/a.csv' AS w")$w, "C:/data/a.csv")
  expect_identical(sr_duck_query(con, "SELECT 'a' /* 'http://x' */ AS w")$w, "a")
  expect_identical(sr_duck_query(con, "-- 'ftp://x'\nSELECT 2 AS x")$x, 2L)
})

test_that("the guard refuses INSTALL, LOAD, SET, RESET, PRAGMA, ATTACH in every form", {
  p <- .duck_project()
  con <- sr_duck(p)
  expect_error(sr_duck_exec(con, "LOAD httpfs"), "refused")
  expect_error(sr_duck_exec(con, "INSTALL httpfs"), "refused")
  expect_error(sr_duck_exec(con, "FORCE INSTALL httpfs"), "refused")
  expect_error(sr_duck_exec(con, "SET threads=1"), "refused")
  expect_error(sr_duck_query(con, "PRAGMA database_list"), "refused")
  expect_error(sr_duck_exec(con, "LOAD 'C:/x/httpfs.duckdb_extension'"), "refused")
  expect_error(sr_duck_exec(con, "ATTACH 'x.duckdb' AS x"), "refused")
  expect_error(sr_duck_exec(con, "RESET threads"), "refused")
  # case, whitespace, comments and statement separators do not hide a statement
  for (s in c("load httpfs", "  \n\t Install httpfs", "force   install httpfs",
              "/* c */ LOAD httpfs", "-- c\nLOAD httpfs", "/* a /* nested */ still */ LOAD httpfs",
              "SELECT 1; LOAD httpfs", "SELECT 1;\n-- x\nSET threads=1", "SELECT 1; /* x */ ATTACH 'a' AS a",
              "SELECT '--'; LOAD httpfs", "SELECT $$'$$; LOAD httpfs", "SELECT 1; PRAGMA threads=1")) {
    expect_true(.refused(s), info = s)
  }
  # a refused statement never reaches DuckDB: no table appeared
  expect_error(sr_duck_exec(con, "CREATE TABLE ok (a INT); LOAD httpfs"), "refused")
  expect_false("ok" %in% DBI::dbListTables(con))
})

test_that("the guard refuses scheme:// literals before DuckDB sees them", {
  p <- .duck_project()
  con <- sr_duck(p)
  expect_error(sr_duck_query(con, "SELECT * FROM read_csv('https://example.invalid/a.csv')"), "refused")
  for (s in c("SELECT * FROM read_parquet('s3://b/k.parquet')", "COPY t FROM 'ftp://h/f.csv'",
              "SELECT * FROM read_csv('file:///x.csv')", "SELECT * FROM read_csv('HTTP://X/a.csv')",
              "SELECT * FROM read_csv('git+ssh://h/r')", "SELECT 'a''b' , 'http://x'",
              "SELECT 1 /* c */ , 'https://x'")) {
    expect_true(.refused(s), info = s)
  }
  expect_false(.refused("SELECT 'not a url: //x' AS w"))
  expect_false(.refused("SELECT \"http://x\" FROM t"))   # an identifier, not a literal
})

test_that("a raw DBI call cannot load or install an extension either", {
  p <- .duck_project()
  con <- sr_duck(p)
  expect_error(DBI::dbExecute(con, "LOAD httpfs"))
  expect_error(DBI::dbExecute(con, "LOAD 'C:/x/httpfs.duckdb_extension'"))
  # INSTALL must not reach the network nor drop a file into the project-owned extension dir
  expect_error(DBI::dbExecute(con, "INSTALL httpfs"))
  ext <- file.path(sr_project_paths(p$paths$dir)$data, "tmp", "duckdb_ext")
  expect_length(list.files(ext, all.files = TRUE, recursive = TRUE, no.. = TRUE), 0L)
  expect_error(DBI::dbExecute(con, "SET threads=1"), "locked")
  expect_error(DBI::dbExecute(con, "SET autoinstall_known_extensions=true"), "locked")
  expect_error(DBI::dbGetQuery(con, "SELECT * FROM read_csv('https://example.invalid/a.csv')"))
})

test_that("the guard validates its input", {
  expect_error(.sr_duck_guard(NA_character_), "single string")
  expect_error(.sr_duck_guard(c("SELECT 1", "SELECT 2")), "single string")
  expect_error(.sr_duck_guard("SELECT 'open"), "refused")        # unterminated quote: not guessable
  expect_error(.sr_duck_guard("SELECT 1 /* open"), "refused")    # unterminated comment
})
