# Stream IO, frozen terms and the stream prologue (P1e.1 Task 5). Synthetic data only.

.sci_src <- function(path) sprintf("read_parquet('%s')", gsub("'", "''", path))

test_that("stream connection is locked down", {
  df <- .sc_frame(200L)
  con <- .sc_con(.sc_parquet(df))
  get1 <- function(s) DBI::dbGetQuery(con, sprintf("SELECT current_setting('%s') AS v", s))$v
  expect_false(as.logical(get1("autoload_known_extensions")))
  expect_false(as.logical(get1("autoinstall_known_extensions")))
  expect_false(as.logical(get1("allow_community_extensions")))
  expect_true(as.logical(get1("lock_configuration")))
  expect_true(as.logical(get1("preserve_insertion_order")))
  ext <- get1("extension_directory")
  expect_true(dir.exists(ext))
  expect_length(list.files(ext, all.files = TRUE, no.. = TRUE), 0L)
  expect_error(DBI::dbExecute(con, "SET threads = 1"))
})

test_that("pass reads in file order and detects disorder", {
  n <- 100L
  df <- .sc_frame(n)
  path <- .sc_parquet(df)
  con <- .sc_con(path)
  src <- .sci_src(path)
  r <- .h_st_pass(con, sprintf("SELECT \"id\", \".sr_row\" FROM %s", src), 7L, 0,
                  function(s, d) s + sum(d[[".sr_row"]]))
  expect_equal(r$state, n * (n + 1) / 2)
  expect_equal(r$rows, n)
  # a shuffled .sr_row is refused
  sh <- df
  sh$.sr_row <- sample.int(n)
  path2 <- .sc_parquet(sh)
  con2 <- .sc_con(path2)
  expect_error(.h_st_pass(con2, sprintf("SELECT \".sr_row\" FROM %s", .sci_src(path2)), 7L, 0,
                          function(s, d) s),
               "extract read out of order")
  # order = FALSE does not look at .sr_row
  r2 <- .h_st_pass(con2, sprintf("SELECT \"id\" FROM %s", .sci_src(path2)), 7L, 0,
                   function(s, d) s + nrow(d), order = FALSE)
  expect_equal(r2$rows, n)
  expect_equal(r2$state, n)
})

test_that("stream predicate registers the excluded rows", {
  df <- .sc_frame(50L)
  path <- .sc_parquet(df)
  con <- .sc_con(path)
  expect_identical(.h_st_excl(con, integer()), "TRUE")
  w <- .h_st_excl(con, c(3L, 10L, 50L))
  expect_identical(w, "\".sr_row\" NOT IN (SELECT r FROM sr_excl)")
  got <- DBI::dbGetQuery(con, sprintf("SELECT count(*) AS n FROM %s WHERE %s", .sci_src(path), w))$n
  expect_equal(got, 47)
})

test_that("stream progress writes json and ignores a failed write", {
  f <- tempfile(fileext = ".json")
  withr::defer(unlink(f))
  .h_st_progress(f, 3L, 26L)
  expect_identical(trimws(paste(readLines(f), collapse = "")), "{\"pass\": 3, \"max\": 26}")
  expect_silent(.h_st_progress(file.path(tempdir(), "no-such-dir", "p.json"), 1L, 2L))
})

test_that("stream levels equal .h_levels, including non-ASCII and mixed case", {
  df <- .sc_frame(300L)
  path <- .sc_parquet(df)
  con <- .sc_con(path)
  src <- .sci_src(path)
  for (col in c("grp", "late")) {
    ref <- df[[col]][1L]
    got <- .h_levels(.h_st_distinct(con, src, col, "TRUE"), ref)
    expect_identical(got, .h_levels(df[[col]], ref), info = col)
  }
  expect_true("\u00e9" %in% .h_st_distinct(con, src, "grp", "TRUE"))
  # NA is not a level, and the exclusion predicate applies
  df2 <- df
  df2$grp[1:3] <- NA
  p2 <- .sc_parquet(df2)
  c2 <- .sc_con(p2)
  expect_false(anyNA(.h_st_distinct(c2, .sci_src(p2), "grp", "TRUE")))
  w <- .h_st_excl(c2, which(df2$late == "z"))
  expect_false("z" %in% .h_st_distinct(c2, .sci_src(p2), "late", w))
})

test_that("stream knots equal Hmisc on the full column", {
  ref <- function(x, nk) suppressWarnings(Hmisc::rcspline.eval(x[!is.na(x)], nk = nk, knots.only = TRUE))
  df <- .sc_frame(2000L)
  path <- .sc_parquet(df)
  con <- .sc_con(path)
  src <- .sci_src(path)
  for (col in c("x2", "xt")) {
    for (nk in 3:5) {
      want <- ref(df[[col]], nk)
      if (length(want) == nk) {
        got <- suppressWarnings(.h_st_knots(con, src, col, nk, "TRUE", 5e7))
        expect_equal(as.numeric(got), as.numeric(want), tolerance = 1e-12, info = paste(col, nk))
      } else {
        # Hmisc cannot place nk distinct knots (xt has 6 values): memory and stream refuse alike
        expect_error(suppressWarnings(.h_st_knots(con, src, col, nk, "TRUE", 5e7)), "distinct knots")
      }
    }
  }
  # fewer than 100 rows: Hmisc's small-sample rule
  d60 <- .sc_frame(60L)
  p60 <- .sc_parquet(d60)
  c60 <- .sc_con(p60)
  for (nk in 3:5) {
    got <- .h_st_knots(c60, .sci_src(p60), "x2", nk, "TRUE", 5e7)
    expect_equal(as.numeric(got), as.numeric(ref(d60$x2, nk)), tolerance = 1e-12, info = nk)
  }
  # a heavy mass at the minimum (Hmisc's fractied rule) takes the exact path
  d <- .sc_frame(2000L)
  d$x2[1:300] <- 0
  pd <- .sc_parquet(d)
  cd <- .sc_con(pd)
  expect_equal(as.numeric(.h_st_knots(cd, .sci_src(pd), "x2", 4, "TRUE", 5e7)),
               as.numeric(ref(d$x2, 4)), tolerance = 1e-12)
  # a text column is refused as in memory
  expect_error(.h_st_knots(con, src, "grp", 4, "TRUE", 5e7), "not numeric")
})

test_that("knots refused above stream_knot_rows when tied", {
  df <- .sc_frame(2000L)
  path <- .sc_parquet(df)
  con <- .sc_con(path)
  expect_error(.h_st_knots(con, .sci_src(path), "xt", 4, "TRUE", 10),
               "too many tied values to place knots for xt at scale")
})

# Copy a parquet into a fake project folder under its streaming sha; returns list(proj, ex).
.sci_ex <- function(path, env = parent.frame()) {
  proj <- tempfile("sci-proj")
  dir.create(file.path(proj, "data", "extracts"), recursive = TRUE)
  withr::defer(unlink(proj, recursive = TRUE), envir = env)
  sha <- paste(as.character(unclass(openssl::sha256(file(path)))), collapse = "")
  rel <- paste0("data/extracts/", sha, ".parquet")
  file.copy(path, file.path(proj, rel))
  list(proj = proj, ex = list(rel = rel, sha = sha))
}

.sci_env <- function(proj) {
  e <- new.env(parent = globalenv())
  e$sr_project_dir <- proj
  e$sr_duck_memory <- "1GB"
  e$sr_duck_threads <- 2L
  e$sr_duck_temp <- tempfile("sci-tmp")
  e
}

test_that("stream prologue checks the extract", {
  path <- .sc_parquet(.sc_frame(100L))
  h <- .sci_ex(path)
  code <- .sr_run_prologue_stream(h$ex)
  expect_length(code, 1L)
  expect_silent(parse(text = code))
  expect_false(grepl("[^\\x01-\\x7F]", code, perl = TRUE))
  expect_false(grepl("http", code, fixed = TRUE))
  expect_false(grepl(h$proj, code, fixed = TRUE))
  # intact: the connection opens, sr_src reads the extract, and it closes when the code ends
  e <- .sci_env(h$proj)
  eval(parse(text = paste(code, "n <- DBI::dbGetQuery(.sr_con, paste('SELECT count(*) AS n FROM', sr_src))$n",
                          "ok <- DBI::dbIsValid(.sr_con)", sep = "\n")), e)
  expect_equal(e$n, 100)
  expect_true(e$ok)
  expect_false(DBI::dbIsValid(e$.sr_con))
  # one flipped byte
  f <- file.path(h$proj, h$ex$rel)
  bytes <- readBin(f, "raw", n = file.size(f))
  k <- length(bytes) %/% 2L
  bytes[k] <- as.raw(bitwXor(as.integer(bytes[k]), 1L))
  writeBin(bytes, f)
  expect_error(eval(parse(text = code), .sci_env(h$proj)), "extract changed")
  # a rel that is not data/extracts/<sha>.parquet
  bad <- h$ex
  bad$rel <- sub("\\.parquet$", ".rds", bad$rel)
  expect_error(.sr_run_prologue_stream(bad), "data/extracts/<sha>.parquet", fixed = TRUE)
  bad2 <- h$ex
  bad2$sha <- "xyz"
  expect_error(.sr_run_prologue_stream(bad2), "64-character")
})

test_that("stream terms setup keeps design identical", {
  spec <- list(exposure = "sex", exposure_ref = "F", outcome = "y",
               terms = list(list(var = "grp", type = "factor", ref = "A"),
                            list(var = "x2", type = "rcs", knots = 4),
                            list(var = "xt", type = "rcs", knots = c(1, 2, 3)),
                            list(var = "x1", type = "quad")),
               interactions = list(c("sex", "grp")), exclude_rows = c(5, 9))
  mem <- sr_terms_code(spec)
  st <- sr_terms_code(spec, "stream")
  expect_identical(st$design, mem$design)
  expect_identical(st$rhs, mem$rhs)
  expect_identical(st$coef_map, mem$coef_map)
  expect_false(identical(st$setup, mem$setup))
  expect_silent(parse(text = st$setup))
  expect_false(grepl("[^\\x01-\\x7F]", st$setup, perl = TRUE))
  expect_true(grepl(".sr_excl <- c(5, 9)", st$setup, fixed = TRUE) ||
              grepl(".sr_excl <- c(5L, 9L)", st$setup, fixed = TRUE))
  expect_true(grepl(".sr_where <- .h_st_excl(.sr_con, .sr_excl)", st$setup, fixed = TRUE))
  expect_true(grepl(".h_st_knots(.sr_con, sr_src,", st$setup, fixed = TRUE))
  expect_false(grepl("sr_data", st$setup, fixed = TRUE))
  expect_error(sr_terms_code(spec, "gpu"))
  # the emitted setup passes the checker
  expect_true(.sr_emit_check(paste(st$setup, st$design, sep = "\n"), "analysis"))
})

test_that("stream setup freezes the same levels and knots as memory", {
  df <- .sc_frame(1500L)
  path <- .sc_parquet(df)
  spec <- list(exposure = "sex", exposure_ref = "F", outcome = "y",
               terms = list(list(var = "grp", type = "factor", ref = "A"),
                            list(var = "x2", type = "rcs", knots = 4)),
               exclude_rows = c(2, 7))
  em <- new.env(parent = globalenv())
  em$sr_data <- df
  eval(parse(text = sr_terms_code(spec)$setup), em)
  ex <- .sci_ex(path)
  es <- .sci_env(ex$proj)
  code <- paste(.sr_run_prologue_stream(ex$ex), sr_terms_code(spec, "stream")$setup, sep = "\n")
  eval(parse(text = code), es)
  expect_identical(es$.sr_fixed$levels, em$.sr_fixed$levels)
  expect_equal(es$.sr_fixed$knots, em$.sr_fixed$knots, tolerance = 1e-12)
})
