# Tests for routes/run.R: the hashed column extract and the locked, cached run engine. Synthetic data
# only (survival::lung reshaped); everything lives under withr::local_tempdir().

extracts <- function(p) list.files(file.path(p$paths$dir, "data", "extracts"))
fit_files <- function(p) list.files(p$paths$fits, pattern = "[.]rds$")

# ---- the state normaliser (ruling R14) ----------------------------------------------------------------

test_that(".sr_spec_from_state turns all-scalar lists of one type back into vectors", {
  f <- .sr_spec_from_state
  expect_identical(f(list(1L, 2L, 3L)), 1:3)
  expect_identical(f(list(40L, 55.5, 70L)), c(40, 55.5, 70))
  expect_identical(f(list("a", "b")), c("a", "b"))
  expect_identical(f(list(TRUE, FALSE)), c(TRUE, FALSE))
  expect_identical(f(list("a", 1L)), list("a", 1L))                     # mixed types stay a list
  expect_identical(f(list()), list())
  expect_null(f(NULL))
  expect_identical(f("x"), "x")
  x <- list(a = list("p", "q"), terms = list(list(var = "age", knots = list(40L, 55L, 70L)),
                                             list(var = "ecog")),
            interactions = list(list("sex", "ecog")), layers = list(missing = "cc"))
  y <- f(x)
  expect_identical(y$a, c("p", "q"))
  expect_identical(y$terms[[1L]]$knots, c(40L, 55L, 70L))
  expect_identical(y$terms[[2L]], list(var = "ecog"))
  expect_identical(y$interactions, list(c("sex", "ecog")))
  expect_identical(y$layers, list(missing = "cc"))
  expect_identical(names(y), names(x))
})

test_that("a spec compiles identically before and after an event round trip", {
  est <- sr_estimand(population = "adults", variables = "died", intercurrent = "none",
                     summary = "stub", level = "marginal")
  r <- sr_route(id = "stub_rt", version = 1L, family = "stub", purpose = "explain-causal",
                estimand = est,
                spec_schema = list(outcome = list(type = "chr", required = TRUE),
                                   terms = list(type = "list", required = FALSE),
                                   interactions = list(type = "list", required = FALSE),
                                   mi = list(type = "list", required = FALSE)),
                compile = function(spec) {
                  tc <- sr_terms_code(spec)
                  paste(c(tc$setup, tc$design, tc$rhs, .sr_lit(sr_spec_cols(spec)),
                          .sr_lit(spec$mi$aux)), collapse = "\n")
                },
                present = function(value, spec) list())
  spec <- list(purpose = "explain-causal", outcome = "died", exposure = "sex", seed = 42L,
               terms = list(list(var = "age", type = "rcs", knots = c(40, 55, 70)),
                            list(var = "ecog", type = "linear")),
               interactions = list(c("sex", "ecog")),
               mi = list(aux = c("wt_loss", "time", "id")))
  rt <- .sr_parse_json(.sr_json(spec))
  expect_false(identical(rt$terms[[1L]]$knots, spec$terms[[1L]]$knots))   # the round trip changes it
  expect_identical(sr_compile(r, .sr_spec_from_state(rt)), sr_compile(r, spec))
  expect_identical(sr_spec_cols(.sr_spec_from_state(rt)), sr_spec_cols(spec))
})

# ---- sr_extract -----------------------------------------------------------------------------------

test_that("sr_extract writes a byte-stable, hashed extract under data/extracts", {
  p <- .run_drafted()
  ex <- sr_extract(p, "t", c("died", "age", "sex"))
  expect_named(ex, c("rel", "sha", "n", "cols"))
  expect_identical(ex$cols, c("died", "age", "sex"))
  expect_identical(ex$n, nrow(.run_frame()))
  expect_identical(ex$rel, paste0("data/extracts/", ex$sha, ".rds"))
  f <- file.path(p$paths$dir, ex$rel)
  bytes <- readBin(f, "raw", n = file.size(f))
  expect_identical(.sr_sha256(bytes), ex$sha)
  d <- unserialize(bytes)
  expect_s3_class(d, "data.frame")
  expect_identical(names(d), c("died", "age", "sex"))
  expect_identical(nrow(d), ex$n)
  expect_identical(sort(d$age), sort(.run_frame()$age))
  ex2 <- sr_extract(p, "t", c("died", "age", "sex"))
  expect_identical(ex2, ex)
  expect_identical(extracts(p), paste0(ex$sha, ".rds"))
  # a reopened project makes the same bytes
  q <- .run_reopen(p)
  expect_identical(sr_extract(q, "t", c("died", "age", "sex")), ex)
  expect_length(extracts(q), 1L)
  # other columns, another extract
  ex3 <- sr_extract(q, "t", "died")
  expect_false(identical(ex3$sha, ex$sha))
})

test_that("sr_extract refuses unknown columns and tables", {
  p <- .run_drafted()
  expect_error(sr_extract(p, "t", c("died", "nope")), "nope")
  expect_error(sr_extract(p, "no_table", "died"), "no_table")
  expect_error(sr_extract(p, "t", character()), "cols")
  expect_error(sr_extract(p, "t", NA_character_), "cols")
  expect_length(extracts(p), 0L)
})

test_that("the prologue passes its own check and verifies the extract hash", {
  ex <- list(rel = paste0("data/extracts/", strrep("a", 64), ".rds"), sha = strrep("a", 64))
  pro <- .sr_run_prologue(ex)
  expect_true(is.character(pro) && length(pro) == 1L)
  expect_silent(.sr_emit_check(pro, "analysis", prologue = TRUE))
  expect_error(.sr_emit_check(pro, "analysis"))
  expect_match(pro, "extract changed", fixed = TRUE)
  expect_match(pro, ex$sha, fixed = TRUE)
  expect_match(pro, "sr_project_dir", fixed = TRUE)
  expect_false(grepl(":/", pro, fixed = TRUE))                # no absolute path in the text
})

# ---- sr_run ----------------------------------------------------------------------------------------

test_that("sr_run refuses an unknown analysis and an unlocked SAP, and writes nothing", {
  p <- .run_drafted()
  n0 <- n_events(p)
  expect_error(sr_run(p, "a1"), "the SAP is not locked")
  expect_error(sr_run(p, "nope"), "nope")
  expect_length(extracts(p), 0L)
  expect_identical(fit_files(p), character(0))
  expect_identical(n_events(p), n0)
  expect_null(p$state$runs)
})

test_that("sr_run runs a locked analysis, caches it and records run.completed", {
  p <- .run_locked()
  p <- sr_run(p, "a1")
  r <- p$state$runs$a1
  expect_true(r$ok)
  expect_false(r$cached)
  expect_true(is.na(r$error))
  expect_false(r$lib_changed)
  expect_match(r$key, "^[0-9a-f]{64}$")
  expect_match(r$extract_sha, "^[0-9a-f]{64}$")
  expect_identical(r$analysis, "a1")
  expect_true(is.numeric(r$elapsed))
  expect_identical(extracts(p), paste0(r$extract_sha, ".rds"))
  expect_identical(fit_files(p), paste0(r$key, ".rds"))
  expect_identical(utils::tail(types_of(p), 1L), "run.completed")
  res <- sr_result(p, "a1")
  expect_equal(res$effects$est, mean(.run_frame()$died))
  expect_equal(res$effects$n, nrow(.run_frame()))
  expect_true("flags" %in% names(res))
  # the second run is a cache hit with the same key
  p2 <- sr_run(p, "a1")
  expect_true(p2$state$runs$a1$cached)
  expect_identical(p2$state$runs$a1$key, r$key)
  expect_identical(sr_result(p2, "a1"), res)
  # events carry no absolute path
  txt <- paste(readLines(p$paths$events, warn = FALSE), collapse = "\n")
  expect_false(grepl(p$paths$dir, txt, fixed = TRUE))
})

test_that("the run state replays identically and the key is the same for a reopened project", {
  p <- sr_run(.run_locked(), "a1")
  q <- .run_reopen(p)
  expect_identical(q$state$runs, p$state$runs)
  expect_identical(q$state$analyses, p$state$analyses)
  q <- sr_run(q, "a1")
  expect_identical(q$state$runs$a1$key, p$state$runs$a1$key)
  expect_true(q$state$runs$a1$cached)
  expect_identical(sr_result(q, "a1"), sr_result(p, "a1"))
})

test_that("a table changed after the lock is refused and appends nothing", {
  p <- .run_locked()
  n0 <- n_events(p)
  df <- .run_frame()
  df$age <- df$age + 1
  csv <- file.path(dirname(p$paths$dir), "t2.csv")
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = "t", options = list(drive_info = .run_drive))
  n1 <- n_events(p)
  expect_gt(n1, n0)                                            # the ingest itself is logged
  expect_error(sr_run(p, "a1"), "table 't' changed after the SAP lock")
  expect_identical(n_events(p), n1)
  expect_false("run.completed" %in% types_of(p))
  expect_length(extracts(p), 0L)
})

test_that("a project copied to another folder reruns from the cache; a changed extract fails the run", {
  p <- sr_run(.run_locked(), "a1")
  r0 <- p$state$runs$a1
  res0 <- sr_result(p, "a1")
  sr_duck_close(p)
  sr_project_close(p)
  dest <- file.path(withr::local_tempdir(), "copy")
  dir.create(dest)
  file.copy(list.files(p$paths$dir, all.files = TRUE, no.. = TRUE, full.names = TRUE), dest,
            recursive = TRUE)
  unlink(file.path(dest, ".lock"))
  q <- sr_project_open(dest)
  withr::defer(sr_duck_close(q))
  withr::defer(sr_project_close(q))
  expect_false(isTRUE(q$readonly))
  q <- sr_run(q, "a1")
  expect_true(q$state$runs$a1$ok)
  expect_true(q$state$runs$a1$cached)
  expect_identical(q$state$runs$a1$key, r0$key)
  expect_identical(sr_result(q, "a1"), res0)
  # one flipped byte in the extract, and no cache: the run refuses
  f <- file.path(dest, "data", "extracts", paste0(r0$extract_sha, ".rds"))
  b <- readBin(f, "raw", n = file.size(f))
  b[length(b)] <- as.raw(bitwXor(as.integer(b[length(b)]), 1L))
  writeBin(b, f)
  unlink(Sys.glob(file.path(dest, "fits", "*.rds")))
  q <- sr_run(q, "a1")
  expect_false(q$state$runs$a1$ok)
  expect_match(q$state$runs$a1$error, "extract changed", fixed = TRUE)
  expect_false(grepl(dest, q$state$runs$a1$error, fixed = TRUE))
  expect_identical(fit_files(q), character(0))
  # the earlier ok run is still the result of record, but its cached value is gone
  expect_error(sr_result(q, "a1"), "run it again")
})

test_that("a body that errors is recorded as a failed run and the project is returned", {
  p <- .run_locked("stub_err", function(spec) "stop(\"boom\")")
  p <- sr_run(p, "a1")
  expect_type(p$state$runs$a1, "list")
  expect_false(p$state$runs$a1$ok)
  expect_match(p$state$runs$a1$error, "boom")
  expect_identical(utils::tail(types_of(p), 1L), "run.completed")
  expect_identical(fit_files(p), character(0))
  expect_error(sr_result(p, "a1"), "analysis 'a1' has not run successfully")
})

test_that("a body that returns a function is not plain data", {
  p <- .run_locked("stub_fn", function(spec) "function(x) x")
  p <- sr_run(p, "a1")
  expect_false(p$state$runs$a1$ok)
  expect_match(p$state$runs$a1$error, "not plain data")
  expect_identical(fit_files(p), character(0))          # the bad value is not left in the cache
  expect_error(sr_result(p, "a1"), "has not run successfully")
})

test_that("a presented result that breaks the contract is a failed run", {
  p <- .run_locked("stub_bad", function(spec) "list(n = 1)")
  p <- sr_run(p, "a1")                                  # present() reads value$m, which is absent
  expect_false(p$state$runs$a1$ok)
  expect_true(is.character(p$state$runs$a1$error) && nzchar(p$state$runs$a1$error))
  expect_identical(fit_files(p), character(0))
})

test_that("an analysis that does not fit in memory is refused before any extract", {
  p <- .run_locked()
  old <- get("sr_mem_estimate", envir = globalenv())
  assign("sr_mem_estimate", function(...) list(fits = FALSE, reason = "too big"), envir = globalenv())
  withr::defer(assign("sr_mem_estimate", old, envir = globalenv()))
  expect_error(sr_run(p, "a1"), "too big")
  expect_length(extracts(p), 0L)
  expect_false("run.completed" %in% types_of(p))
})

test_that("a read-only project cannot run and sr_result needs a run", {
  p <- .run_locked()
  expect_error(sr_result(p, "a1"), "analysis 'a1' has not run successfully")
  expect_error(sr_result(p, "nope"), "nope")
  p$readonly <- TRUE
  expect_error(sr_run(p, "a1"), "read-only")
})

test_that("the error scrub replaces the project, temporary and home folders, in both slash styles", {
  home <- path.expand("~")
  tmp <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  proj <- file.path(tmp, "some_project")
  under_home <- paste0(home, "/notes/file.txt")
  msg <- paste("cannot open", file.path(proj, "data", "x.parquet"), "|",
               gsub("/", "\\", file.path(tmp, "spill.tmp"), fixed = TRUE), "|",
               file.path(tmp, "spill2.tmp"), "|", under_home, "|", gsub("/", "\\", under_home, fixed = TRUE))
  out <- .sr_run_scrub(msg, proj)
  expect_match(out, "<project>/data/x.parquet", fixed = TRUE)
  bs <- intToUtf8(92L)                                                  # one backslash
  expect_match(out, paste0("<tempdir>", bs, "spill.tmp"), fixed = TRUE)
  expect_match(out, "<tempdir>/spill2.tmp", fixed = TRUE)
  expect_match(out, "<home>/notes/file.txt", fixed = TRUE)
  expect_match(out, paste0("<home>", bs, "notes", bs, "file.txt"), fixed = TRUE)
  for (p in unique(c(tmp, home, gsub("/", "\\", tmp, fixed = TRUE), gsub("/", "\\", home, fixed = TRUE)))) {
    expect_false(grepl(p, out, fixed = TRUE), info = "a path survived the scrub")
  }
  prof <- Sys.getenv("USERPROFILE")
  if (nzchar(prof)) {
    m2 <- paste0("at ", prof, bs, "AppData", bs, "x and ", gsub(bs, "/", prof, fixed = TRUE), "/y")
    o2 <- .sr_run_scrub(m2, proj)
    expect_identical(o2, paste0("at <home>", bs, "AppData", bs, "x and <home>/y"))
  }
  if (identical(.Platform$OS.type, "windows")) {
    o3 <- .sr_run_scrub(paste0("in ", toupper(tmp), "/z"), proj)
    expect_identical(o3, "in <tempdir>/z")
  }
  # regular-expression characters in a path are matched literally
  odd <- file.path(tmp, "a+b (1)[x]")
  expect_identical(.sr_run_scrub(paste0(odd, "/f"), odd), "<project>/f")
  expect_identical(.sr_run_scrub("no paths here", proj), "no paths here")
})

# ---- run.started, run.cancelled, status and history ---------------------------------------------------

test_that("sr_run writes run.started with the job just before run.completed, and only after every refusal", {
  p <- .run_locked()
  n0 <- n_events(p)
  expect_error(sr_run(p, "nope", job = "j1"), "unknown analysis")
  expect_error(sr_run(p, "a1", job = "bad job!"), "job must be one string")
  expect_identical(n_events(p), n0)
  p <- sr_run(p, "a1", job = "j1")
  ty <- types_of(p)
  expect_identical(tail(ty, 2L), c("run.started", "run.completed"))
  ev <- tail(sr_events_read(p$paths$events), 2L)
  expect_identical(ev[[1]]$payload$job, "j1")
  expect_identical(ev[[2]]$payload$job, "j1")
  expect_identical(ev[[1]]$payload$key, ev[[2]]$payload$key)
  r <- p$state$runs$a1
  expect_identical(r$status, "ok")
  expect_identical(r$job, "j1")
  expect_length(r$history, 1L)
  expect_identical(r$history[[1]]$outcome, "ok")
  expect_false(is.na(r$history[[1]]$started))
})

test_that("a NULL job gets a generated id; the generator leaves the RNG alone", {
  set.seed(1); before <- .Random.seed
  j <- .sr_run_job_id()
  expect_identical(.Random.seed, before)
  expect_match(j, "^j[0-9]{14}[0-9a-f]+$")
  p <- sr_run(.run_locked(), "a1")
  expect_match(p$state$runs$a1$job, "^j[0-9]{14}[0-9a-f]+$")
})

test_that("a dangling run.started replays as running; a new run closes it as interrupted", {
  p <- .run_locked()
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k0", extract_sha = "s0", job = "jdead"))
  q <- .run_reopen(p)
  expect_identical(q$state$runs$a1$status, "running")
  expect_identical(q$state$runs$a1$job, "jdead")
  q <- sr_run(q, "a1", job = "j2")
  h <- q$state$runs$a1$history
  expect_identical(vapply(h, `[[`, "", "outcome"), c("interrupted", "ok"))
  expect_identical(q$state$runs$a1$status, "ok")
})

test_that("sr_run_cancel records run.cancelled and keeps the earlier good result", {
  p <- sr_run(.run_locked(), "a1", job = "j1")
  good <- p$state$runs$a1$ok_key
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k2", extract_sha = "s2", job = "j2"))
  p <- sr_run_cancel(p, "a1", "j2")
  r <- p$state$runs$a1
  expect_identical(r$status, "cancelled")
  expect_identical(r$ok_key, good)
  expect_identical(tail(r$history, 1L)[[1]]$outcome, "cancelled")
  expect_identical(tail(r$history, 1L)[[1]]$reason, "cancelled by the user")
  expect_s3_class(sr_result(p, "a1")$effects, "data.frame")
  expect_identical(.run_reopen(p)$state$runs, p$state$runs)
})

test_that("sr_run_cancel refuses a read-only project, an unknown analysis and a stale job, writing nothing", {
  p <- sr_run(.run_locked(), "a1", job = "j1")
  n0 <- n_events(p)
  expect_error(sr_run_cancel(p, "nope", "j1"), "unknown analysis")
  ro <- p; ro$readonly <- TRUE
  expect_error(sr_run_cancel(ro, "a1", "j1"), "read-only")
  expect_identical(n_events(p), n0)
})

test_that("sr_run_cancel refuses a completed run with class sr_run_stale", {
  p <- sr_run(.run_locked(), "a1", job = "j1")
  e <- tryCatch(sr_run_cancel(p, "a1", "j1"), error = function(e) e)
  expect_s3_class(e, "sr_run_stale")
  expect_match(conditionMessage(e), "no running job 'j1'")
  p <- sr_project_append(p, "run.started", list(analysis = "a1", key = "k", extract_sha = "s", job = "j2"))
  expect_s3_class(tryCatch(sr_run_cancel(p, "a1", "j1"), error = function(e) e), "sr_run_stale")
})

test_that("a project with run.completed only (no job, no run.started) replays to ok / failed", {
  p <- .run_locked()
  p <- sr_project_append(p, "run.completed", list(analysis = "a1", key = "k", extract_sha = "s", ok = FALSE,
                                                  cached = FALSE, error = "boom", elapsed = 1))
  r <- .run_reopen(p)$state$runs$a1
  expect_identical(r$status, "failed")
  expect_true(is.na(r$job))
  expect_identical(r$history[[1]]$outcome, "failed")
  expect_true(is.na(r$history[[1]]$started))
})
