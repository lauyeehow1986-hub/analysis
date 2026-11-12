# SQL Table 1 (P1e.1 Task 10, spec section 6.7, ruling R14). Synthetic data only.
# The reference is the in-memory .h_t1_main on the same data frame; the SQL side runs on a Parquet
# extract through a locked-down connection (.sc_con, helper-scale.R).

.st1_frame <- function(n = 3000L) {
  set.seed(20261111)
  df <- .sc_frame(n)
  df$arm <- sample(c("ctl", "trt", "alt"), n, replace = TRUE)
  df$arm[seq(7L, n, by = 41L)] <- NA
  df$x1[seq(3L, n, by = 29L)] <- NA
  df$x2[seq(5L, n, by = 31L)] <- NA
  df$grp[seq(2L, n, by = 47L)] <- NA
  df$bin <- sample(c(0, 1), n, replace = TRUE)
  df$bin[seq(4L, n, by = 53L)] <- NA
  df$two <- sample(c(2.5, 7), n, replace = TRUE)
  df$allna <- NA_real_
  df$cnt <- sample(1:6, n, replace = TRUE)
  df$flag <- sample(c(TRUE, FALSE), n, replace = TRUE)
  df
}

.st1_expect_equal <- function(a, b, info = "") {
  expect_identical(names(a), names(b), info = info)
  expect_identical(a$by, b$by)
  expect_identical(a$groups, b$groups)
  expect_identical(a$n, b$n)
  expect_identical(a$n_excluded, b$n_excluded)
  expect_identical(length(a$vars), length(b$vars))
  for (i in seq_along(a$vars)) {
    u <- a$vars[[i]]
    v <- b$vars[[i]]
    nm <- paste(info, v$var)
    expect_identical(names(u), names(v), info = nm)
    for (f in names(v)) {
      if (is.numeric(v[[f]])) {
        expect_identical(typeof(u[[f]]), typeof(v[[f]]), info = paste(nm, f))
        expect_equal(u[[f]], v[[f]], tolerance = 1e-10, info = paste(nm, f))
        expect_identical(is.na(u[[f]]), is.na(v[[f]]), info = paste(nm, f))
      } else {
        expect_identical(u[[f]], v[[f]], info = paste(nm, f))
      }
    }
  }
}

.st1_sql <- function(df, by, vars, nonnormal) {
  path <- .sc_parquet(df)
  con <- .sc_con(path)
  src <- paste0("read_parquet('", gsub("'", "''", normalizePath(path, winslash = "/"), fixed = TRUE), "')")
  .h_t1_sql_main(con, src, by, vars, nonnormal)
}

test_that("SQL Table 1 equals .h_t1_main", {
  df <- .st1_frame()
  vars <- c("x1", "x2", "sex", "grp", "bin", "two", "allna", "cnt", "flag")
  nn <- "x2"
  mem <- .h_t1_main(df, "arm", vars, nn)
  sql <- .st1_sql(df, "arm", vars, nn)
  .st1_expect_equal(sql, mem)
  expect_true(mem$n_excluded > 0L)
  kinds <- vapply(sql$vars, function(v) v$kind, "")
  expect_identical(kinds, vapply(mem$vars, function(v) v$kind, ""))
  expect_identical(unname(kinds[c(5L, 6L, 7L)]), c("categorical", "categorical", "continuous"))
  # identical strings
  spec <- list(by = "arm", vars = vars, nonnormal = nn)
  pm <- .sr_t1_present(mem, spec)
  ps <- .sr_t1_present(sql, spec)
  expect_identical(ps$tables, pm$tables)
  expect_identical(ps$fragments, pm$fragments)
  expect_equal(ps$effects, pm$effects, tolerance = 1e-10)
})

test_that("SQL Table 1 handles two groups, NaN and integer columns", {
  df <- .st1_frame()
  df$arm <- ifelse(df$arm == "alt", "ctl", df$arm)
  df$x1[seq(9L, nrow(df), by = 61L)] <- NaN
  vars <- c("x1", "x2", "sex", "late", "cnt")
  mem <- .h_t1_main(df, "arm", vars, c("x1", "cnt"))
  sql <- .st1_sql(df, "arm", vars, c("x1", "cnt"))
  .st1_expect_equal(sql, mem)
  spec <- list(by = "arm", vars = vars, nonnormal = c("x1", "cnt"))
  expect_identical(.sr_t1_present(sql, spec)$tables, .sr_t1_present(mem, spec)$tables)
})

test_that("SQL Table 1 refusals match in-memory texts", {
  df <- .st1_frame()
  df$many <- paste0("id", seq_len(nrow(df)))
  df$when <- as.Date("2020-01-01") + seq_len(nrow(df))
  df$ts <- as.POSIXct("2020-01-01", tz = "UTC") + seq_len(nrow(df))
  df$one <- "a"
  msg <- function(expr) tryCatch({
    expr
    NA_character_
  }, error = conditionMessage)
  for (case in list(list("arm", "many"), list("arm", c("x1", "when")), list("arm", "ts"),
                    list("one", "x1"))) {
    m <- msg(.h_t1_main(df, case[[1L]], case[[2L]], character()))
    s <- msg(.st1_sql(df, case[[1L]], case[[2L]], character()))
    expect_false(is.na(m))
    expect_identical(s, m, info = paste(case[[2L]], collapse = ","))
  }
  expect_match(msg(.st1_sql(df, "arm", "many", character())), "at most 50 levels")
  expect_match(msg(.st1_sql(df, "arm", "when", character())), "date or time")
  expect_match(msg(.st1_sql(df, "one", "x1", character())), "at least two observed groups")
})

# ---- the compiled stream body ------------------------------------------------------------------

.st1_layers <- function() {
  list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
       framework = "frequentist", clustered = FALSE)
}

.st1_proj <- function(env = parent.frame()) {
  proj <- tempfile("st1-proj")
  dir.create(file.path(proj, "data", "extracts"), recursive = TRUE)
  withr::defer(unlink(proj, recursive = TRUE), envir = env)
  proj
}

.st1_env <- function(proj) {
  e <- new.env(parent = globalenv())
  e$sr_project_dir <- proj
  e$sr_duck_memory <- "2GB"
  e$sr_duck_threads <- 4L
  e$sr_duck_temp <- tempdir()
  e
}

.st1_run <- function(df, spec) {
  proj <- .st1_proj()
  path <- .sc_parquet(df)
  sha <- paste(as.character(unclass(openssl::sha256(file(path)))), collapse = "")
  rel <- paste0("data/extracts/", sha, ".parquet")
  file.copy(path, file.path(proj, rel))
  if (!(.sr_route_key("table1", 1L) %in% ls(.sr_routes, all.names = TRUE))) {
    sr_route_register(.sr_table1_route())
  }
  s <- c(spec, list(engine = "stream", stream_chunk = 1000L))
  body <- sr_compile(sr_route_get("table1", 1L), s)
  prologue <- .sr_run_prologue_stream(list(rel = rel, sha = sha), threads = .sr_stream_threads("table1"))
  code <- paste(prologue, body, sep = "\n")
  list(code = code, value = eval(parse(text = code), .st1_env(proj)), body = body, prologue = prologue)
}

test_that("the stream Table 1 body matches the in-memory value and opens with one thread", {
  df <- .st1_frame()
  vars <- c("x1", "x2", "sex", "grp", "bin")
  spec <- list(purpose = "describe", by = "arm", vars = vars, nonnormal = "x2", layers = .st1_layers(),
               seed = 42L)
  r <- .st1_run(df, spec)
  .st1_expect_equal(r$value, .h_t1_main(df, "arm", vars, "x2"))
  expect_false(any(utf8ToInt(r$body) > 127L))
  # one connection, opened by the prologue with the literal 1L threads (R14); the body opens none
  expect_false(grepl(".h_st_open(", r$body, fixed = TRUE))
  expect_false(grepl("dbDisconnect", r$body, fixed = TRUE))
  expect_match(r$prologue, ".h_st_open(.sr_xf, sr_duck_memory, 1L, sr_duck_temp)", fixed = TRUE)
  expect_false(grepl("sr_duck_threads", r$prologue, fixed = TRUE))
  ex0 <- list(rel = paste0("data/extracts/", strrep("a", 64L), ".parquet"), sha = strrep("a", 64L))
  expect_match(.sr_run_prologue_stream(ex0), ".h_st_open(.sr_xf, sr_duck_memory, sr_duck_threads, sr_duck_temp)",
               fixed = TRUE)
  expect_error(.sr_run_prologue_stream(ex0, threads = 0), "whole number")
  expect_error(.sr_emit_check(r$body, "analysis", stream = TRUE), NA)
})

# ---- the mean is R's mean() to the bit (fix round 1) --------------------------------------------

# The SQL value on a locked-down connection with ONE thread, as the real run opens it (R14).
.st1_sql1 <- function(df, by, vars, nonnormal, env = parent.frame()) {
  path <- .sc_parquet(df, env = env)
  con <- .h_st_open(path, "2GB", 1L, tempdir())
  withr::defer(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), envir = env)
  src <- paste0("read_parquet('", gsub("'", "''", normalizePath(path, winslash = "/"), fixed = TRUE), "')")
  .h_t1_sql_main(con, src, by, vars, nonnormal)
}

test_that("the SQL mean equals mean() bitwise on .xx5 tie grids and the strings are identical", {
  bad <- 0L
  total <- 0L
  for (seed in 1:6) {
    for (grid in c(0.01, 0.1)) {
      set.seed(1000L * seed + round(grid * 100))
      ng <- 40L
      sz <- 2L * sample(2:40, ng, replace = TRUE)           # even n, 4 to 80
      g <- rep(sprintf("g%02d", seq_len(ng)), sz)
      x <- round(sample(0:600, length(g), replace = TRUE) * grid + if (grid == 0.1) 100 else 0, 2)
      df <- data.frame(g = g, x = x, stringsAsFactors = FALSE)
      mem <- .h_t1_main(df, "g", "x", character())
      sql <- .st1_sql1(df, "g", "x", character())
      mm <- mem$vars[[1L]]$mean
      sm <- sql$vars[[1L]]$mean
      bad <- bad + sum(!mapply(identical, as.list(mm), as.list(sm)))
      total <- total + length(mm)
      spec <- list(by = "g", vars = "x", nonnormal = character())
      pm <- .sr_t1_present(mem, spec)$tables$table1
      ps <- .sr_t1_present(sql, spec)$tables$table1
      expect_identical(ps, pm, info = paste(seed, grid))
      expect_equal(sql$vars[[1L]]$sd, mem$vars[[1L]]$sd, tolerance = 1e-12)
    }
  }
  expect_gt(total, 400L)
  expect_identical(bad, 0L)
})

test_that("an infinite value gives mean Inf and SD NaN as in memory", {
  set.seed(5)
  df <- data.frame(g = rep(c("a", "b"), 30), x = c(Inf, rnorm(59)), y = c(rnorm(58), -Inf, Inf),
                   stringsAsFactors = FALSE)
  mem <- .h_t1_main(df, "g", c("x", "y"), character())
  sql <- .st1_sql1(df, "g", c("x", "y"), character())
  for (i in 1:2) {
    expect_identical(sql$vars[[i]]$mean, mem$vars[[i]]$mean)
    expect_identical(is.nan(sql$vars[[i]]$sd), is.nan(mem$vars[[i]]$sd))
    expect_identical(is.na(sql$vars[[i]]$sd), is.na(mem$vars[[i]]$sd))
  }
  expect_identical(sql$vars[[1L]]$mean[c(1L, 3L)], c(Inf, Inf))
})
