# Tests for routes/run.R: the hashed column extract and the locked, cached run engine. Synthetic data
# only (survival::lung reshaped); everything lives under withr::local_tempdir().

.run_frame <- function() {
  lung <- survival::lung
  data.frame(id = seq_len(nrow(lung)), time = lung$time, died = as.integer(lung$status == 2L),
             age = lung$age, sex = ifelse(lung$sex == 1L, "M", "F"), ecog = lung$ph.ecog,
             wt_loss = lung$wt.loss, stringsAsFactors = FALSE)
}

.run_roles <- c(id = "id", time = "time", died = "outcome", sex = "exposure")

.run_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

# Register a stub route whose compile returns `body_fn(spec)`; removed when the calling test ends.
.run_route <- function(id = "stub_run", body_fn = NULL, env = parent.frame()) {
  if (is.null(body_fn)) {
    body_fn <- function(spec) {
      sprintf("list(n = nrow(sr_data), m = mean(sr_data[[%s]]))", .sr_lit(spec$outcome))
    }
  }
  est <- sr_estimand(population = "adults", variables = "died", intercurrent = "none",
                     summary = "stub", level = "marginal")
  present <- function(value, spec) {
    eff <- data.frame(term = "m", measure = "MD", est = value$m, lo = NA_real_, hi = NA_real_,
                      p = NA_real_, scale = NA_character_, level = NA_character_, mcse = NA_real_,
                      role = "descriptive", conditional = FALSE, ci_method = NA_character_,
                      n = value$n, events = NA_real_, stringsAsFactors = FALSE)
    list(tables = list(), figures = list(), fragments = list(), effects = eff)
  }
  r <- sr_route(id = id, version = 1L, family = "stub", purpose = "explain-causal", estimand = est,
                spec_schema = list(outcome = list(type = "chr", required = TRUE),
                                   seed = list(type = "num", required = FALSE)),
                compile = body_fn, present = present)
  sr_route_register(r)
  withr::defer(rm(list = .sr_route_key(id, 1L), envir = .sr_routes), envir = env)
  invisible(r)
}

.run_spec <- function(outcome = "died", ...) {
  layers <- list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                 framework = "frequentist", clustered = FALSE)
  c(list(purpose = "explain-causal", outcome = outcome, layers = layers, seed = 42L), list(...))
}

# A ready project with analysis a1 (route `id`) drafted, SAP not yet locked.
.run_drafted <- function(id = "stub_run", body_fn = NULL, env = parent.frame()) {
  .run_route(id, body_fn, env = env)
  p <- .fx_ready_project(.run_frame(), "t", .run_roles, env = env)
  sr_sap_add(p, "a1", id, .run_spec(), "t", label = "primary")
}

.run_locked <- function(id = "stub_run", body_fn = NULL, env = parent.frame()) {
  sr_sap_lock(.fx_feasible(.run_drafted(id, body_fn, env = env)))
}

# Re-open the project in `dir` (the old handle is closed first).
.run_reopen <- function(p, env = parent.frame()) {
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  q <- sr_project_open(dir)
  withr::defer(sr_duck_close(q), envir = env)
  withr::defer(sr_project_close(q), envir = env)
  q
}

extracts <- function(p) list.files(file.path(p$paths$dir, "data", "extracts"))
fit_files <- function(p) list.files(p$paths$fits, pattern = "[.]rds$")
n_events <- function(p) length(sr_events_read(p$paths$events))
types_of <- function(p) vapply(sr_events_read(p$paths$events), function(e) e$type, "")

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
