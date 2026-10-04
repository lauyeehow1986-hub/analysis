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

test_that("a raw DBI call cannot load or install an extension either (settings only; no network)", {
  p <- .duck_project()
  con <- sr_duck(p)
  dd <- sr_project_paths(p$paths$dir)$data
  norm <- function(x) normalizePath(x, winslash = "/", mustWork = FALSE)
  ext <- norm(file.path(dd, "tmp", "duckdb_ext"))
  repo <- norm(file.path(dd, "tmp", "duckdb_ext_repo"))
  expect_identical(norm(.duck_setting(con, "extension_directory")), ext)
  expect_identical(norm(.duck_setting(con, "custom_extension_repository")), repo)
  expect_identical(norm(.duck_setting(con, "autoinstall_extension_repository")), repo)
  expect_false(as.logical(.duck_setting(con, "autoinstall_known_extensions")))
  expect_false(as.logical(.duck_setting(con, "autoload_known_extensions")))
  expect_length(list.files(ext, all.files = TRUE, recursive = TRUE, no.. = TRUE), 0L)
  expect_length(list.files(repo, all.files = TRUE, recursive = TRUE, no.. = TRUE), 0L)
  # behavioural: a made-up name without FROM can only resolve against the local empty repo
  expect_error(DBI::dbExecute(con, "INSTALL zz_sr_fake_ext"))
  expect_length(list.files(ext, all.files = TRUE, recursive = TRUE, no.. = TRUE), 0L)
  expect_error(DBI::dbExecute(con, "SET threads=1"), "locked")
  expect_error(DBI::dbExecute(con, "SET autoinstall_known_extensions=true"), "locked")
  expect_error(DBI::dbExecute(con, "RESET threads"), "locked")
  expect_error(DBI::dbExecute(con, "PRAGMA threads=1"), "locked")
})

test_that("the guard peels EXPLAIN / ANALYZE / (options) and checks what is wrapped", {
  p <- .duck_project()
  con <- sr_duck(p)
  # fake names and a fake local path only: were the guard to miss one, DuckDB fails locally
  forms <- c(
    "EXPLAIN ANALYZE INSTALL zz_sr_fake_ext FROM 'C:/zz_sr_nonexistent_dir'",
    "EXPLAIN ANALYZE FORCE INSTALL zz_sr_fake_ext",
    "EXPLAIN INSTALL zz_sr_fake_ext",
    "EXPLAIN LOAD zz_sr_fake_ext",
    "EXPLAIN (ANALYZE) INSTALL zz_sr_fake_ext",
    "explain   analyze   load zz_sr_fake_ext",
    "EXPLAIN (FORMAT JSON, ANALYZE) LOAD zz_sr_fake_ext",
    "EXPLAIN (FORMAT 'a)b') LOAD zz_sr_fake_ext",
    "EXPLAIN /* c */ ANALYZE -- c\n INSTALL zz_sr_fake_ext",
    "EXPLAIN EXPLAIN LOAD zz_sr_fake_ext",
    "EXPLAIN ANALYSE ANALYZE (ANALYZE) LOAD zz_sr_fake_ext",
    "EXPLAIN ANALYZE SET threads=1",
    "EXPLAIN PRAGMA database_list",
    "EXPLAIN ATTACH 'C:/zz_sr_nonexistent.duckdb' AS zz",
    "EXPLAIN RESET threads",
    "SELECT 1; EXPLAIN LOAD zz_sr_fake_ext",
    "EXPLAIN (ANALYZE LOAD zz_sr_fake_ext"       # unbalanced option list: not guessable
  )
  for (s in forms) {
    expect_true(.refused(s), info = s)
    expect_error(sr_duck_exec(con, s), "refused", info = s)
  }
  expect_error(sr_duck_query(con, "EXPLAIN ANALYZE LOAD 'C:/zz/zz_sr_fake.duckdb_extension'"), "refused")
  # plain EXPLAIN of a query still works
  expect_false(.refused("EXPLAIN SELECT 1"))
  expect_false(.refused("EXPLAIN ANALYZE SELECT 1"))
  expect_false(.refused("EXPLAIN (ANALYZE) SELECT 1"))
  expect_identical(nrow(sr_duck_query(con, "EXPLAIN SELECT 1 AS x")) >= 1L, TRUE)
})

test_that("the guard refuses an INSTALL carrying a FROM clause in any position", {
  for (s in c("SELECT 1; INSTALL zz_sr_fake_ext FROM 'C:/zz_sr_nonexistent_dir'",
              "INSTALL zz_sr_fake_ext\nFROM 'C:/zz_sr_nonexistent_dir'",
              "INSTALL zz_sr_fake_ext /* c */ FROM 'C:/zz_sr_nonexistent_dir'",
              "WITH a AS (SELECT 1) INSTALL zz_sr_fake_ext FROM 'C:/zz_sr_nonexistent_dir'",
              "CREATE TABLE t AS INSTALL zz_sr_fake_ext FROM 'C:/zz_sr_nonexistent_dir'")) {
    expect_true(.refused(s), info = s)
  }
  # look-alikes stay allowed: words inside literals and quoted identifiers, longer words
  expect_false(.refused("SELECT 'install x from y' AS w"))
  expect_false(.refused("SELECT \"install\" FROM t"))
  expect_false(.refused("SELECT a FROM installs"))
  expect_false(.refused("SELECT uninstall FROM t"))
})

test_that("the scheme check covers dollar-quoted bodies", {
  for (s in c("SELECT * FROM read_csv($$http://example.invalid/a.csv$$)",
              "SELECT * FROM read_csv($t$https://example.invalid/a.csv$t$)",
              "SELECT $$s3://b/k$$",
              "SELECT $tag_1$ftp://h/f$tag_1$")) {
    expect_true(.refused(s), info = s)
  }
  expect_false(.refused("SELECT $$not a url: //x$$ AS w"))
  expect_false(.refused("SELECT $$C:/data/a.csv$$ AS w"))
})

test_that("the tokenizing scanner alone is enough: no match-limit failures, no free-text false positives", {
  # ~30+ consecutive block comments used to exhaust the PCRE match limit and fail open
  cm <- strrep("/* c */ ", 40)
  expect_no_warning(r1 <- .refused(paste0(cm, "LOAD zz_sr_fake_ext")))
  expect_true(r1)
  expect_no_warning(r2 <- .refused(paste0("SELECT 1; ", cm, "-- x\n", cm, "SET threads=1")))
  expect_true(r2)
  expect_no_warning(r3 <- .refused(paste0(strrep("-- c\n", 60), "INSTALL zz_sr_fake_ext")))
  expect_true(r3)
  # free text that merely contains ; and a keyword is data, not a statement
  p <- .duck_project()
  con <- sr_duck(p)
  expect_false(.refused("SELECT 'Discharged; set for review' AS w"))
  expect_identical(sr_duck_query(con, "SELECT 'Discharged; set for review' AS w")$w, "Discharged; set for review")
  expect_false(.refused("INSERT INTO t VALUES ('a; load b', 'x;\nattach y')"))
  expect_false(.refused("SELECT $$a; pragma b$$"))
  expect_false(.refused("SELECT \"a; set b\" FROM t"))
})

test_that("E'...' strings (backslash escapes) cannot hide a statement from the scanner", {
  bs <- intToUtf8(92L)
  # DuckDB reads E'<bs>'' as one string holding a quote, so the LOAD after it is a real statement
  s <- paste0("SELECT E'", bs, "''; LOAD zz_sr_fake_ext; --'")
  expect_true(.refused(s))
  expect_true(.refused(paste0("SELECT e'", bs, "''; SET threads=1; --'")))
  p <- .duck_project()
  con <- sr_duck(p)
  expect_identical(sr_duck_query(con, paste0("SELECT E'", bs, "'' AS w"))$w, "'")
  expect_identical(sr_duck_query(con, paste0("SELECT E'a", bs, "'; set x' AS w"))$w, "a'; set x")
  # an identifier that merely ends in e is not an E-string prefix
  expect_identical(as.character(sr_duck_query(con, "SELECT DATE'2020-01-01' AS d")$d), "2020-01-01")
  expect_false(.refused("SELECT DATE'2020-01-01' AS d"))
})

test_that("the guard validates its input", {
  expect_error(.sr_duck_guard(NA_character_), "single string")
  expect_error(.sr_duck_guard(c("SELECT 1", "SELECT 2")), "single string")
  expect_error(.sr_duck_guard("SELECT 'open"), "refused")        # unterminated quote: not guessable
  expect_error(.sr_duck_guard("SELECT 1 /* open"), "refused")    # unterminated comment
})
