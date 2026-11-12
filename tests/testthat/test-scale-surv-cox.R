# P1e.2 Task 5: the stream compile of exact Cox (cox@1, cox_describe@1) with the model SE, against the
# in-memory route on the same rows (memory prologue and body on the rds; stream prologue and body on the
# Parquet extract). Tolerances are the plan's table. Synthetic data only. The harness (.ssc_surv_frame,
# .ssc_surv_spec, .ssc_surv_expect, .ssc_memory, .ssc_stream) is in helper-scale.R.

.sx_chunks <- c(7L, 50L, 100000L)

# Stream at every chunk size against memory, and the chunk sizes against each other; returns the streams.
.sx_sweep <- function(df, spec, id = "cox", info = "", chunks = .sx_chunks) {
  mem <- .ssc_memory(df, spec, id)
  out <- list()
  for (ch in chunks) {
    st <- .ssc_stream(df, spec, id, ch)
    .ssc_surv_expect(st, mem, paste(info, ch))
    expect_identical(st$chunk, as.integer(ch), info = info)
    out[[as.character(ch)]] <- st
  }
  if (length(out) > 1L) {
    for (k in seq_along(out)[-1L]) .ssc_surv_chunks(out[[k]], out[[1L]], paste(info, names(out)[k], "vs", names(out)[1L]))
  }
  invisible(list(mem = mem, st = out))
}

test_that("stream cox equals memory: efron and breslow", {
  df <- .ssc_surv_frame(400L, 2L)
  for (id in c("cox", "cox_describe")) {
    for (ties in c("efron", "breslow")) {
      spec <- .ssc_surv_spec(id, ties = ties, terms = list(list(var = "x1"), list(var = "grp", type = "factor"),
                                                           list(var = "yr")))
      r <- .sx_sweep(df, spec, id, paste(id, ties))
      expect_true(r$st[[1L]]$iter >= 2L)
      expect_identical(r$mem$ties, ties)
    }
  }
})

test_that("strata: one, two, all-censored stratum, stratum ending on a chunk boundary", {
  df <- .ssc_surv_frame(300L, 3L)
  .sx_sweep(df, .ssc_surv_spec("cox", strata = "st1"), info = "one stratum")
  .sx_sweep(df, .ssc_surv_spec("cox", strata = c("st1", "st2"), ties = "breslow"), info = "two strata")
  d2 <- df
  d2$status[d2$st2 == "v"] <- 0L
  .sx_sweep(d2, .ssc_surv_spec("cox", strata = "st2"), info = "censored stratum")
  # stratum "a" holds exactly 49 rows, so it ends on a boundary of chunk 7
  d3 <- df
  d3$st1 <- c(rep("a", 49L), rep("b", nrow(d3) - 49L))
  .sx_sweep(d3, .ssc_surv_spec("cox", strata = "st1"), info = "boundary")
})

test_that("terms: interaction with the exposure, rcs, factor exposure with an absent level", {
  df <- .ssc_surv_frame(400L, 4L)
  spec <- .ssc_surv_spec("cox", terms = list(list(var = "x1"), list(var = "x2", type = "rcs")),
                         interactions = list(c("sex", "x1")))
  .sx_sweep(df, spec, info = "interaction rcs")
  # level "C" of grp is in the data but on no analysed row (their time is missing)
  d2 <- df
  d2$time[d2$grp == "C"] <- NA
  spec <- .ssc_surv_spec("cox", exposure = "grp", exposure_ref = "A", terms = list(list(var = "x1")))
  r <- .sx_sweep(d2, spec, info = "absent level")
  expect_true("t1fC" %in% r$mem$aliased)
  expect_identical(r$st[[1L]]$aliased, r$mem$aliased)
})

test_that("rows: small and large exclude_rows", {
  df <- .ssc_surv_frame(300L, 5L)
  .sx_sweep(df, .ssc_surv_spec("cox", exclude_rows = c(2, 17, 250)), info = "small")
  big <- .ssc_surv_frame(2400L, 6L)
  ex <- seq(1, 2400, by = 2)                     # 1200 rows: the hash anti-join
  expect_true(length(ex) > 1000L)
  .sx_sweep(big, .ssc_surv_spec("cox", exclude_rows = ex), info = "large", chunks = c(50L, 100000L))
  # 1010 rows excluded, 290 analysed, at chunk 7
  .sx_sweep(big[1:1300, ], .ssc_surv_spec("cox", exclude_rows = as.numeric(291:1300)), info = "large chunk 7",
            chunks = 7L)
})

test_that("near ties, single event, large-mean covariate", {
  df <- .ssc_surv_frame(300L, 7L)
  # the near ties matter: memory merges them, a fit on the unmerged times would differ
  m <- .h_st_timefix_map(df$time)
  expect_true(length(m$from) > 0L)
  spec <- .ssc_surv_spec("cox", terms = list(list(var = "yr"), list(var = "x1")))
  .sx_sweep(df, spec, info = "near ties yr")
  # one event
  d1 <- df
  d1$status <- 0L
  d1$status[5L] <- 1L
  r <- .sx_sweep(d1, .ssc_surv_spec("cox", terms = list(list(var = "x1"))), info = "single event")
  expect_identical(r$mem$events, 1L)
})

test_that("monotone likelihood warnings are identical", {
  set.seed(11)
  df <- .ssc_surv_frame(200L, 8L)
  med <- stats::median(df$time)
  df$z <- as.numeric(df$time < med & df$status == 1)    # z = 1 only on early events
  df$w <- df$z * 3 + 2
  for (v in c("z", "w")) {
    spec <- .ssc_surv_spec("cox", terms = list(list(var = "x1"), list(var = v)))
    r <- .sx_sweep(df, spec, info = v)
    expect_true(length(r$mem$warnings) > 0L, info = v)
    expect_match(r$mem$warnings[1L], "Loglik converged before variable", fixed = TRUE)
    expect_true(.ssc_inf_terms(r$mem)[3L], info = v)    # the third coefficient (z or w) is the flagged one
  }
})

test_that("km and km_risk equal memory, factor and >8-valued numeric exposure", {
  df <- .ssc_surv_frame(300L, 9L)
  r <- .sx_sweep(df, .ssc_surv_spec("cox", exposure = "grp", exposure_ref = "A", terms = NULL), info = "factor")
  expect_identical(r$mem$km_by, "exposure")
  r <- .sx_sweep(df, .ssc_surv_spec("cox", exposure = "x1", exposure_ref = NULL, terms = NULL), info = "numeric")
  expect_identical(r$mem$km_by, "overall")
  expect_identical(unique(r$st[[1L]]$km$strata), "All")
  # six distinct values: one curve per value
  r <- .sx_sweep(df, .ssc_surv_spec("cox", exposure = "xt", exposure_ref = NULL, terms = NULL), info = "xt")
  expect_identical(r$mem$km_by, "exposure")
  expect_identical(length(unique(r$st[[1L]]$km$strata)), 6L)
})

test_that("time order guard", {
  msg <- "the streamed Cox needs .time to keep the order of the time column"
  g <- .h_st_time_guard(NULL, c(5, 3, 3, 1), c(1, 1, 1, 1))
  expect_identical(g$t, 1)
  expect_error(.h_st_time_guard(NULL, c(5, 3, 4), c(1, 1, 1)), msg, fixed = TRUE)
  # a new stratum may start high; the next chunk may not start above the last time of the same stratum
  g <- .h_st_time_guard(NULL, c(5, 3, 9, 2), c(1, 1, 2, 2))
  expect_identical(g, list(s = 2, t = 2))
  expect_error(.h_st_time_guard(g, c(4, 1), c(2, 2)), msg, fixed = TRUE)
  expect_identical(.h_st_time_guard(g, c(8, 1), c(3, 3)), list(s = 3, t = 1))
  expect_identical(.h_st_time_guard(g, numeric(), numeric()), g)
})

test_that("errors match memory", {
  df <- .ssc_surv_frame(120L, 10L)
  spec <- .ssc_surv_spec("cox")
  cases <- list(
    no_rows = within(df, time <- NA_real_),
    no_events = within(df, status <- 0L),
    missing_col = df[setdiff(names(df), "grp")])
  for (nm in names(cases)) {
    em <- .ssc_err(.ssc_memory(cases[[nm]], spec, "cox"))
    es <- .ssc_err(.ssc_stream(cases[[nm]], spec, "cox", 7L))
    expect_false(is.na(em), info = nm)
    expect_identical(es, em, info = nm)
  }
  expect_error(sr_compile(.ssc_route("cox"), utils::modifyList(spec, list(engine = "stream"))),
               "a stream spec needs stream_chunk (set by the run)", fixed = TRUE)
})

test_that("value names and progress", {
  df <- .ssc_surv_frame(300L, 12L)
  spec <- .ssc_surv_spec("cox")
  mem <- .ssc_memory(df, spec, "cox")
  pf <- tempfile(fileext = ".json")
  st <- .ssc_stream(df, spec, "cox", 50L, env_extra = list(sr_progress_file = pf))
  expect_identical(names(st), c("coef", "aliased", "n", "n_total", "events", "level", "se_type", "n_clusters",
                                "ties", "strata", "levels", "warnings", "km_by", "km", "km_risk", "engine",
                                "chunk", "passes", "iter"))
  expect_identical(names(mem), names(st)[1:15])
  expect_true(st$passes <= 23L)
  expect_identical(st$passes, st$iter + 2L)       # pass 0, the start pass and one per iteration
  got <- jsonlite::fromJSON(readLines(pf, warn = FALSE))
  expect_identical(as.integer(got$max), st$passes)
  expect_identical(as.integer(got$pass), st$passes)
  unlink(pf)
  expect_silent(.ssc_stream(df, spec, "cox", 50L))
})

test_that("emitted stream code passes the emit check and is ASCII", {
  specs <- list(
    .ssc_surv_spec("cox"),
    .ssc_surv_spec("cox_describe", ties = "breslow", strata = c("st1", "st2")),
    .ssc_surv_spec("cox", exposure = "x1", exposure_ref = NULL,
                   terms = list(list(var = "x2", type = "rcs")), exclude_rows = c(1, 2)))
  for (i in seq_along(specs)) {
    id <- if (identical(specs[[i]]$purpose, "describe")) "cox_describe" else "cox"
    body <- .ssc_body(specs[[i]], id, 1000L)
    expect_length(body, 1L)
    expect_silent(parse(text = body))
    expect_true(.sr_emit_check(body, "analysis", stream = TRUE), info = i)
    expect_false(grepl("[^\\x01-\\x7F]", body, perl = TRUE), info = i)
    expect_false(grepl("http", body, fixed = TRUE), info = i)
    expect_false(grepl("sr_data <- unserialize", body, fixed = TRUE), info = i)
    expect_true(grepl(".sr_chunk <- 1000L", body, fixed = TRUE), info = i)
    expect_true(grepl("if (exists(\"sr_progress_file\"))", body, fixed = TRUE), info = i)
    for (txt in c("no complete rows: every row has a missing value in the model",
                  "the outcome has no events among the ",
                  "the streamed Cox needs .time to keep the order of the time column")) {
      expect_true(grepl(txt, body, fixed = TRUE), info = paste(i, txt))
    }
  }
  # PEM on the stream (Task 9) compiles through the same entry and passes the emit check
  pb <- .sr_cox_compile_stream(utils::modifyList(.ssc_surv_spec("cox", cox_method = "pem"),
                                                 list(engine = "stream", stream_chunk = 100)))
  expect_true(.sr_emit_check(pb, "analysis", stream = TRUE))
  expect_false(grepl("[^\\x01-\\x7F]", pb, perl = TRUE))
})
