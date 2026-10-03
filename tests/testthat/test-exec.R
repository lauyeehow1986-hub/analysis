# Tests for seeded isolated execution, RNG streams and the result cache (core/exec.R).

new_proj <- function() {
  env <- parent.frame()
  d <- file.path(withr::local_tempdir(.local_envir = env), "proj")
  p <- sr_project_create(d, "demo", profile = "laptop")
  withr::defer(sr_project_close(p), envir = env)
  p
}
key <- function(s) .sr_sha256(s)
rds_files <- function(p) list.files(p$paths$fits, pattern = "[.]rds$")

test_that("the RNG preamble is exactly two lines with an integer seed literal", {
  expect_identical(sr_rng_preamble(42),
                   "RNGkind(\"L'Ecuyer-CMRG\", \"Inversion\", \"Rejection\")\nset.seed(42L)")
  expect_match(sr_rng_preamble(-5), "set.seed(-5L)", fixed = TRUE)
  expect_error(sr_rng_preamble(1.5), "seed")
})

test_that("seeded runs are reproducible and the seed matters", {
  a <- sr_exec("runif(3)", seed = 42)
  b <- sr_exec("runif(3)", seed = 42)
  c43 <- sr_exec("runif(3)", seed = 43)
  expect_true(a$ok && b$ok && c43$ok)
  expect_identical(a$value, b$value)
  expect_false(identical(a$value, c43$value))
  expect_length(a$value, 3)
  expect_true(startsWith(a$code_run, "RNGkind(\"L'Ecuyer-CMRG\""))
  expect_identical(a$code_run, paste(sr_rng_preamble(42), "runif(3)", sep = "\n"))
  expect_true(is.numeric(a$elapsed) && length(a$elapsed) == 1 && a$elapsed >= 0)
})

test_that("inputs are visible to the code", {
  r <- sr_exec("sum(x)", inputs = list(x = 1:3), seed = 1)
  expect_true(r$ok)
  expect_equal(r$value, 6L)
})

test_that("errors, crashes and timeouts give ok = FALSE and never throw", {
  e <- sr_exec("stop('x')", seed = 1)
  expect_false(e$ok)
  expect_match(e$error, "x", fixed = TRUE)
  expect_null(e$value)
  expect_true(startsWith(e$code_run, "RNGkind("))

  q <- sr_exec("quit(status = 3)", seed = 1)
  expect_false(q$ok)
  expect_true(is.character(q$error) && length(q$error) == 1 && nzchar(q$error))
  expect_equal(sr_exec("1 + 1", seed = 1)$value, 2)  # the parent is alive

  t0 <- Sys.time()
  tm <- sr_exec("Sys.sleep(10)", seed = 1, timeout = 2)
  expect_false(tm$ok)
  expect_match(tm$error, "timeout", ignore.case = TRUE)
  expect_lt(as.numeric(difftime(Sys.time(), t0, units = "secs")), 6)
})

test_that("invalid arguments throw a clear error", {
  expect_error(sr_exec(1, seed = 1), "code")
  expect_error(sr_exec(NA_character_, seed = 1), "code")
  expect_error(sr_exec(c("a", "b"), seed = 1), "code")
  expect_error(sr_exec("1", seed = NA), "seed")
  expect_error(sr_exec("1", seed = 1.5), "seed")
  expect_error(sr_exec("1", seed = Inf), "seed")
  expect_error(sr_exec("1", seed = c(1, 2)), "seed")
  expect_error(sr_exec("1", seed = "1"), "seed")
  expect_error(sr_exec("1", seed = 2^31), "seed")
  expect_error(sr_exec("1", inputs = 1:3, seed = 1), "inputs")
  expect_error(sr_exec("1", inputs = list(1), seed = 1), "inputs")
  expect_error(sr_exec("1", inputs = list(a = 1, a = 2), seed = 1), "inputs")
  expect_error(sr_exec("1", inputs = list(a = 1, 2), seed = 1), "inputs")
  expect_error(sr_exec("1", seed = 1, timeout = 0), "timeout")
  expect_error(sr_exec("1", seed = 1, timeout = c(1, 2)), "timeout")
  expect_error(sr_exec("1", seed = 1, timeout = "5"), "timeout")
  expect_error(sr_exec("1", seed = 1, timeout = NA_real_), "timeout")
  expect_error(sr_streams(1, 0), "n")
  expect_error(sr_streams(1, 1.5), "n")
  expect_error(sr_streams(1, c(1, 2)), "n")
  expect_error(sr_streams(NA, 2), "seed")
})

test_that("an empty inputs list is accepted", {
  expect_true(sr_exec("1", inputs = list(), seed = 1)$ok)
})

test_that("streams are deterministic, distinct and do not depend on n", {
  s8 <- sr_streams(7, 8)
  expect_length(s8, 8)
  expect_true(all(vapply(s8, is.integer, logical(1))))
  expect_identical(sr_streams(7, 8), s8)
  expect_identical(sr_streams(7, 3), s8[1:3])
  expect_false(identical(s8[[1]], s8[[2]]))
  expect_false(identical(sr_streams(8, 1)[[1]], s8[[1]]))
  # stream 1 is nextRNGStream(s0), not s0 itself
  withr::with_preserve_seed({
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    set.seed(7)
    s0 <- get(".Random.seed", envir = globalenv())
  })
  expect_identical(s8[[1]], parallel::nextRNGStream(s0))
  expect_identical(s8[[2]], parallel::nextRNGStream(parallel::nextRNGStream(s0)))
  expect_false(identical(s8[[1]], s0))
})

test_that("sr_streams restores the caller's RNG kind and seed, even if there was none", {
  old_kind <- RNGkind()
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    get(".Random.seed", envir = globalenv()) else NULL
  withr::defer({
    suppressWarnings(RNGkind(old_kind[1], old_kind[2], old_kind[3]))
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", old_seed, envir = globalenv())
  })

  # a non-default kind with a seed in place
  suppressWarnings(RNGkind("Wichmann-Hill", "Box-Muller", "Rounding"))
  set.seed(99)
  before_seed <- get(".Random.seed", envir = globalenv())
  before_kind <- RNGkind()
  invisible(sr_streams(7, 4))
  expect_identical(RNGkind(), before_kind)
  expect_identical(get(".Random.seed", envir = globalenv()), before_seed)

  # no .Random.seed at all
  rm(".Random.seed", envir = globalenv())
  invisible(sr_streams(7, 4))
  expect_false(exists(".Random.seed", envir = globalenv(), inherits = FALSE))
  expect_identical(RNGkind(), before_kind)
})

test_that("bootstrap means from streams are identical sequentially and on 2 or 4 workers", {
  x <- c(2.5, 3.1, 4.7, 5.2, 6.8, 7.4, 8.9, 9.3, 10.6, 11.2)
  streams <- sr_streams(7, 8)
  rep_fun <- function(s, dat) {
    RNGkind("L'Ecuyer-CMRG", "Inversion", "Rejection")
    assign(".Random.seed", s, envir = globalenv())
    mean(sample(dat, replace = TRUE))
  }
  environment(rep_fun) <- globalenv()
  # the sequential run touches this process's RNG: put it back afterwards
  old_kind <- RNGkind()
  old_seed <- if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
    get(".Random.seed", envir = globalenv()) else NULL
  withr::defer({
    suppressWarnings(RNGkind(old_kind[1], old_kind[2], old_kind[3]))
    if (is.null(old_seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
        rm(".Random.seed", envir = globalenv())
    } else assign(".Random.seed", old_seed, envir = globalenv())
  })
  seq_res <- unlist(lapply(streams, rep_fun, dat = x))
  expect_length(seq_res, 8)
  expect_gt(length(unique(seq_res)), 1)
  for (k in c(2, 4)) {
    cl <- parallel::makeCluster(k)
    res <- tryCatch(unlist(parallel::parLapply(cl, streams, rep_fun, dat = x)),
                    finally = parallel::stopCluster(cl))
    expect_identical(res, seq_res)
  }
})

test_that("sr_exec_cached validates the key hash (no path traversal)", {
  p <- new_proj()
  expect_error(sr_exec_cached(p, "../x", "1", list(), 1), "key_hash")
  expect_error(sr_exec_cached(p, "ABC", "1", list(), 1), "key_hash")
  expect_error(sr_exec_cached(p, paste0(key("a"), "/../a"), "1", list(), 1), "key_hash")
  expect_error(sr_exec_cached(p, NA_character_, "1", list(), 1), "key_hash")
  expect_error(sr_exec_cached(p, c(key("a"), key("b")), "1", list(), 1), "key_hash")
  expect_identical(rds_files(p), character(0))
})

test_that("sr_exec_cached hits on the same key, misses on another, never saves failures", {
  p <- new_proj()
  k1 <- key("one")
  r1 <- sr_exec_cached(p, k1, "runif(3)", list(), 42)
  expect_true(r1$ok)
  expect_false(r1$cached)
  expect_identical(rds_files(p), paste0(k1, ".rds"))
  r2 <- sr_exec_cached(p, k1, "runif(3)", list(), 42)
  expect_true(r2$cached)
  expect_identical(r2$value, r1$value)
  expect_identical(r2$code_run, r1$code_run)

  r3 <- sr_exec_cached(p, key("two"), "runif(3)", list(), 42)
  expect_false(r3$cached)
  expect_identical(r3$value, r1$value)  # same code and seed, different key: re-run
  expect_length(rds_files(p), 2)

  kf <- key("fail")
  f <- sr_exec_cached(p, kf, "stop('boom')", list(), 1)
  expect_false(f$ok)
  expect_false(f$cached)
  expect_match(f$error, "boom", fixed = TRUE)
  expect_false(file.exists(file.path(p$paths$fits, paste0(kf, ".rds"))))
  expect_length(rds_files(p), 2)
  expect_identical(list.files(p$paths$fits, pattern = "[.]tmp$"), character(0))
})

test_that("sr_exec_cached passes ... to sr_exec and inputs to the code", {
  p <- new_proj()
  r <- sr_exec_cached(p, key("t"), "Sys.sleep(10)", list(), 1, timeout = 1.5)
  expect_false(r$ok)
  expect_match(r$error, "timeout", ignore.case = TRUE)
  s <- sr_exec_cached(p, key("s"), "sum(x)", list(x = 1:3), 1)
  expect_equal(s$value, 6L)
})

test_that("a corrupt cache file is a miss and is overwritten by an ok run", {
  p <- new_proj()
  k <- key("corrupt")
  f <- file.path(p$paths$fits, paste0(k, ".rds"))
  writeBin(as.raw(c(1, 2, 3, 4, 5)), f)
  r <- sr_exec_cached(p, k, "6 * 7", list(), 1)
  expect_false(r$cached)
  expect_equal(r$value, 42)
  expect_equal(readRDS(f)$value, 42)
  expect_true(sr_exec_cached(p, k, "6 * 7", list(), 1)$cached)

  # a readable rds that is not a result list is also a miss
  saveRDS("not a result", f)
  r2 <- sr_exec_cached(p, k, "6 * 7", list(), 1)
  expect_false(r2$cached)
  expect_equal(r2$value, 42)
})

test_that("a stored code_run that differs is a miss; only an ok re-run overwrites", {
  p <- new_proj()
  k <- key("mismatch")
  r1 <- sr_exec_cached(p, k, "1 + 1", list(), 1)
  expect_equal(r1$value, 2)
  r2 <- sr_exec_cached(p, k, "1 + 2", list(), 1)
  expect_false(r2$cached)
  expect_equal(r2$value, 3)
  r3 <- sr_exec_cached(p, k, "1 + 2", list(), 1)
  expect_true(r3$cached)
  expect_equal(r3$value, 3)
  # a different seed changes code_run too
  expect_false(sr_exec_cached(p, k, "1 + 2", list(), 2)$cached)
  # a failing re-run leaves the stored good result alone
  bad <- sr_exec_cached(p, k, "stop('no')", list(), 3)
  expect_false(bad$ok)
  expect_equal(readRDS(file.path(p$paths$fits, paste0(k, ".rds")))$value, 3)
})

test_that("a read-only project reads hits but never writes to fits/", {
  p <- new_proj()
  k <- key("ro")
  ro <- p
  ro$readonly <- TRUE
  miss <- sr_exec_cached(ro, k, "10 + 1", list(), 1)
  expect_true(miss$ok)
  expect_false(miss$cached)
  expect_equal(miss$value, 11)
  expect_identical(rds_files(p), character(0))

  sr_exec_cached(p, k, "10 + 1", list(), 1)  # a writable project saves it
  expect_length(rds_files(p), 1)
  hit <- sr_exec_cached(ro, k, "10 + 1", list(), 1)
  expect_true(hit$cached)
  expect_equal(hit$value, 11)
  # a mismatch on a read-only project re-runs and does not overwrite
  other <- sr_exec_cached(ro, k, "10 + 2", list(), 1)
  expect_false(other$cached)
  expect_equal(readRDS(file.path(p$paths$fits, paste0(k, ".rds")))$value, 11)
})

test_that("input names cannot hijack execution (code, inputs, eval, parse, c)", {
  base <- sr_exec("c(sum(x), runif(1))", inputs = list(x = 1:3), seed = 5)
  expect_true(base$ok)
  expect_equal(base$value[1], 6)
  r <- sr_exec("sum(x)", inputs = list(code = "1+1", x = 1:3), seed = 1)
  expect_true(r$ok)
  expect_equal(r$value, 6L)
  expect_identical(r$code_run, paste(sr_rng_preamble(1), "sum(x)", sep = "\n"))
  # the seeded draw is unchanged by an input named code
  r2 <- sr_exec("c(sum(x), runif(1))", inputs = list(code = "1+1", x = 1:3), seed = 5)
  expect_identical(r2$value, base$value)
  r3 <- sr_exec("length(inputs)", inputs = list(inputs = 1:7), seed = 1)
  expect_equal(r3$value, 7L)
  r4 <- sr_exec("sum(x)", inputs = list(eval = function(...) 99, parse = 1, c = 2, x = 1:3),
                seed = 1)
  expect_true(r4$ok)
  expect_equal(r4$value, 6L)
  expect_error(sr_exec("1", inputs = list(.sr_x = 1), seed = 1), "inputs")
  expect_error(sr_exec("1", inputs = list(.sr_code = 1), seed = 1), "inputs")
})

test_that("a zero-status quit is a failure, never cached; a NULL value is a success", {
  for (code in c("quit(status = 0)", "q('no')")) {
    r <- sr_exec(code, seed = 1)
    expect_false(r$ok)
    expect_identical(r$error, "worker exited before completing")
    expect_null(r$value)
  }
  p <- new_proj()
  c1 <- sr_exec_cached(p, key("q"), "quit(status = 0)", list(), 1)
  expect_false(c1$ok)
  expect_identical(rds_files(p), character(0))
  n <- sr_exec("NULL", seed = 1)
  expect_true(n$ok)
  expect_null(n$value)
  expect_true("value" %in% names(n))
  n2 <- sr_exec_cached(p, key("null"), "invisible(NULL)", list(), 1)
  expect_true(n2$ok)
  expect_true(sr_exec_cached(p, key("null"), "invisible(NULL)", list(), 1)$cached)
})

test_that("the worker runs in the session temp dir, not the caller's working directory", {
  r <- sr_exec("getwd()", seed = 1)
  expect_true(r$ok)
  expect_false(identical(normalizePath(r$value, winslash = "/"),
                         normalizePath(getwd(), winslash = "/")))
})

test_that("the cache also keys on the inputs", {
  p <- new_proj()
  k <- key("inputs")
  a <- sr_exec_cached(p, k, "sum(x)", list(x = 1:3), 1)
  expect_equal(a$value, 6L)
  expect_true(sr_exec_cached(p, k, "sum(x)", list(x = 1:3), 1)$cached)
  b <- sr_exec_cached(p, k, "sum(x)", list(x = 1:4), 1)
  expect_false(b$cached)
  expect_equal(b$value, 10L)
  expect_equal(readRDS(file.path(p$paths$fits, paste0(k, ".rds")))$value, 10L)
  expect_true(sr_exec_cached(p, k, "sum(x)", list(x = 1:4), 1)$cached)
  # the stored entry from before the inputs hash existed is a miss
  old <- b[c("ok", "value", "error", "elapsed", "code_run")]
  saveRDS(old, file.path(p$paths$fits, paste0(k, ".rds")))
  expect_false(sr_exec_cached(p, k, "sum(x)", list(x = 1:4), 1)$cached)
  # the hit has the same fields as a fresh run (plus cached)
  expect_identical(sort(names(sr_exec_cached(p, k, "sum(x)", list(x = 1:4), 1))),
                   sort(c(names(old), "cached")))
})

test_that("a failed cache write leaves no temp file and no warning, even under warn = 2", {
  p <- new_proj()
  k <- key("w")
  res <- sr_exec("1", seed = 1)
  # the target name is an existing directory, so the rename fails
  dir.create(file.path(p$paths$fits, paste0(k, ".rds")))
  old <- options(warn = 2)
  on.exit(options(old), add = TRUE)
  expect_no_warning(.sr_cache_write(p$paths$fits, k, res))
  # the folder cannot be created (its parent is a file): also silent
  blocker <- file.path(p$paths$fits, "blocker")
  writeLines("x", blocker)
  expect_no_warning(.sr_cache_write(file.path(blocker, "sub"), k, res))
  options(old)
  expect_identical(list.files(p$paths$fits, pattern = "[.]tmp$"), character(0))
})
