# P1e.2 Task 7: the streamed RMST (.sr_rmst_compile_stream), against the in-memory route (survfit and the
# rmean branch of survival:::survmean) on the same rows. Synthetic data only. The harness (.ssc_surv_frame,
# .ssc_surv_spec, .ssc_memory, .ssc_stream) is in helper-scale.R.

.rs_spec <- function(...) .ssc_surv_spec("rmst", ...)

# groups: rmst and se at 1e-10; n, events, at_risk and max_time identical; KM at 1e-12, its counts identical
.rs_expect <- function(st, mem, info = "") {
  expect_identical(names(st), c(names(mem), "engine", "chunk", "passes"), info = info)
  expect_identical(st$engine, "stream", info = info)
  expect_identical(names(st$groups), names(mem$groups), info = info)
  expect_identical(st$groups$level, mem$groups$level, info = info)
  for (f in c("rmst", "se")) expect_equal(st$groups[[f]], mem$groups[[f]], tolerance = 1e-10, info = paste(info, f))
  for (f in c("n", "events", "at_risk", "max_time")) {
    expect_identical(st$groups[[f]], mem$groups[[f]], info = paste(info, f))
  }
  for (f in c("tau", "n", "n_total", "events", "level")) expect_identical(st[[f]], mem[[f]], info = paste(info, f))
  for (k in c("km", "km_risk")) {
    expect_identical(names(st[[k]]), names(mem[[k]]), info = paste(info, k))
    expect_identical(nrow(st[[k]]), nrow(mem[[k]]), info = paste(info, k))
  }
  for (f in c("strata", "time", "n.risk", "n.event", "n.censor")) {
    expect_identical(st$km[[f]], mem$km[[f]], info = paste(info, "km", f))
  }
  for (f in c("surv", "lower", "upper")) {
    expect_equal(st$km[[f]], mem$km[[f]], tolerance = 1e-12, info = paste(info, "km", f))
  }
  for (f in names(mem$km_risk)) expect_identical(st$km_risk[[f]], mem$km_risk[[f]], info = paste(info, "risk", f))
}

.rs_present <- function(value, spec) {
  r <- .ssc_route("rmst")
  r$present(value, spec)
}

test_that("stream rmst equals memory", {
  df <- .ssc_surv_frame(500L, 31L)
  df$time[c(10L, 11L)] <- c(6, 6 + 1e-10)             # a near tie inside one group
  df$sex[c(10L, 11L)] <- "M"
  spec <- .rs_spec()
  mem <- .ssc_memory(df, spec, "rmst")
  expect_identical(names(mem), c("groups", "tau", "n", "n_total", "events", "level", "km", "km_risk"))
  for (ch in c(7L, 100000L)) {
    st <- .ssc_stream(df, spec, "rmst", ch)
    .rs_expect(st, mem, paste("chunk", ch))
    expect_identical(st$chunk, ch)
  }
  # rows lost to a missing time, status or exposure
  df$time[seq(5L, 500L, by = 23L)] <- NA
  df$status[seq(8L, 500L, by = 31L)] <- NA
  df$sex[seq(9L, 500L, by = 37L)] <- NA
  mem <- .ssc_memory(df, spec, "rmst")
  .rs_expect(.ssc_stream(df, spec, "rmst", 7L), mem, "missing")
})

test_that("the near-tie map is built per group (S13)", {
  # group A has times 5 and 5 + 2e-7, group B has 5 + 1e-7 between them: over all rows the chain 5, 5 + 1e-7,
  # 5 + 2e-7 is one tie block, but within A the two times are 2e-7 apart (above the tolerance), so
  # survfit on A alone keeps them apart (A's first time is a censoring, so the risk sets differ) and the RMST
  # of A differs from the globally mapped one
  set.seed(5)
  ta <- c(5, 5 + 2e-7, 5 + 2e-7, 3, 8, 8, 9, 12, 12, 2, 4, 6, 7, 11, 13)
  tb <- c(5 + 1e-7, 5 + 1e-7, 4, 6, 6, 9, 10, 12, 14, 3, 7, 8, 11, 13, 15)
  tc <- c(5, 5 + 1e-7, 5 + 1e-7, 4, 6, 7, 9, 10, 12, 14, 3, 8, 11, 13, 16)
  df <- data.frame(time = c(ta, tb, tc), grp = rep(c("A", "B", "C"), each = 15L),
                   status = c(0, 1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1,
                              1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 0, 1,
                              1, 1, 0, 1, 1, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1))
  df$sex <- df$grp
  spec <- .rs_spec(exposure_ref = "A", tau = 9)
  mem <- .ssc_memory(df, spec, "rmst")
  # the memory route maps within each group: A's two times stay apart, as a survfit on A alone
  fitA <- survival::survfit(survival::Surv(time, status) ~ 1, data = df[df$grp == "A", ])
  expect_equal(mem$groups$se[1L], unname(summary(fitA, rmean = 9)$table[["se(rmean)"]]), tolerance = 1e-12)
  # the same group with the map of all rows would differ (the test discriminates)
  allmap <- .h_st_timefix_map(df$time)
  tA <- .h_st_timefix_apply(df$time[df$grp == "A"], allmap)
  fitG <- survival::survfit(survival::Surv(tA, status) ~ 1, data = data.frame(tA = tA, status = df$status[df$grp == "A"]))
  expect_false(isTRUE(all.equal(unname(summary(fitG, rmean = 9)$table[["rmean"]]), mem$groups$rmst[1L],
                                tolerance = 1e-6)))
  for (ch in c(4L, 7L, 100000L)) .rs_expect(.ssc_stream(df, spec, "rmst", ch), mem, paste("chunk", ch))
  # a larger frame with three groups, every group with its own near ties
  d2 <- .ssc_surv_frame(900L, 33L)
  d2$grp <- ifelse(d2$grp == "é", "C", d2$grp)
  d2$sex <- d2$grp
  d2$time[seq(2L, 900L, by = 17L)] <- d2$time[seq(2L, 900L, by = 17L)] + 3e-9
  d2$time[seq(3L, 900L, by = 19L)] <- d2$time[seq(3L, 900L, by = 19L)] - 5e-9
  spec2 <- .rs_spec(exposure_ref = "A", tau = 8)
  mem2 <- .ssc_memory(d2, spec2, "rmst")
  expect_identical(mem2$groups$level, c("A", "b", "C"))
  .rs_expect(.ssc_stream(d2, spec2, "rmst", 7L), mem2, "three groups")
})

test_that("rmst with exposure_ref and a text exposure", {
  df <- .ssc_surv_frame(400L, 34L)
  spec <- .rs_spec(exposure = "grp", exposure_ref = "C", tau = 8)
  mem <- .ssc_memory(df, spec, "rmst")
  expect_identical(mem$groups$level[1L], "C")
  for (ch in c(7L, 100000L)) .rs_expect(.ssc_stream(df, spec, "rmst", ch), mem, paste("grp", ch))
  # a numeric exposure with few values: the groups keep the numeric order
  df$a <- c(0.5, 2, 10)[sample(3L, nrow(df), replace = TRUE)]
  spec <- .rs_spec(exposure = "a", exposure_ref = NULL)
  mem <- .ssc_memory(df, spec, "rmst")
  expect_identical(mem$groups$level, c("0.5", "2", "10"))
  .rs_expect(.ssc_stream(df, spec, "rmst", 7L), mem, "numeric exposure")
  spec <- .rs_spec(exposure = "a", exposure_ref = "10")
  .rs_expect(.ssc_stream(df, spec, "rmst", 7L), .ssc_memory(df, spec, "rmst"), "numeric exposure, ref 10")
})

test_that("tau above a group's follow-up stops with the memory text", {
  df <- .ssc_surv_frame(300L, 35L)
  spec <- .rs_spec(tau = 1e6)
  em <- .ssc_err(.ssc_memory(df, spec, "rmst"))
  expect_true(grepl("exceeds the follow-up of group", em, fixed = TRUE))
  expect_identical(.ssc_err(.ssc_stream(df, spec, "rmst", 7L)), em)
  # only the second group is short
  df$time[df$sex == "M"] <- pmin(df$time[df$sex == "M"], 9)
  spec <- .rs_spec(tau = 12, exposure_ref = "M")
  em <- .ssc_err(.ssc_memory(df, spec, "rmst"))
  expect_false(is.na(em))
  expect_identical(.ssc_err(.ssc_stream(df, spec, "rmst", 7L)), em)
})

test_that("fewer than 10 at risk gives the memory note", {
  df <- .ssc_surv_frame(400L, 36L)
  tau <- min(tapply(df$time, df$sex, max))
  spec <- .rs_spec(tau = tau)
  mem <- .ssc_memory(df, spec, "rmst")
  expect_true(any(mem$groups$at_risk < 10L))
  st <- .ssc_stream(df, spec, "rmst", 7L)
  .rs_expect(st, mem, "thin")
  pm <- .rs_present(mem, spec)
  ps <- .rs_present(st, spec)
  expect_true("rmst_few_at_risk" %in% names(pm$fragments))
  expect_identical(ps$fragments, pm$fragments)
  expect_equal(ps$effects$est, pm$effects$est, tolerance = 1e-10)
  expect_identical(ps$tables, pm$tables)
})

test_that("more than 8 exposure values stops with the memory text", {
  df <- .ssc_surv_frame(300L, 37L)
  df$a <- rep(1:11, length.out = nrow(df)) * 1.0
  spec <- .rs_spec(exposure = "a", exposure_ref = NULL)
  em <- .ssc_err(.ssc_memory(df, spec, "rmst"))
  expect_true(grepl("has 11 distinct values", em, fixed = TRUE))
  expect_identical(.ssc_err(.ssc_stream(df, spec, "rmst", 7L)), em)
  # exactly 8 is allowed
  df$a <- rep(1:8, length.out = nrow(df)) * 1.0
  .rs_expect(.ssc_stream(df, spec, "rmst", 7L), .ssc_memory(df, spec, "rmst"), "eight values")
})

test_that("the memory stops are the stream stops", {
  df <- .ssc_surv_frame(200L, 38L)
  spec <- .rs_spec()
  cases <- list(
    noevents = transform(df, status = 0),
    allmissing = transform(df, time = NA_real_),
    oneval = transform(df, sex = "M"),
    badref = df)
  specs <- list(noevents = spec, allmissing = spec, oneval = spec, badref = .rs_spec(exposure_ref = "Q"))
  for (nm in names(cases)) {
    em <- .ssc_err(.ssc_memory(cases[[nm]], specs[[nm]], "rmst"))
    expect_false(is.na(em), info = nm)
    expect_identical(.ssc_err(.ssc_stream(cases[[nm]], specs[[nm]], "rmst", 7L)), em, info = nm)
  }
  expect_error(sr_compile(.ssc_route("rmst"), utils::modifyList(spec, list(engine = "stream"))),
               "a stream spec needs stream_chunk (set by the run)", fixed = TRUE)
})

test_that("rmst value names", {
  df <- .ssc_surv_frame(300L, 39L)
  spec <- .rs_spec()
  mem <- .ssc_memory(df, spec, "rmst")
  pf <- tempfile(fileext = ".json")
  st <- .ssc_stream(df, spec, "rmst", 50L, env_extra = list(sr_progress_file = pf))
  expect_identical(names(st), c("groups", "tau", "n", "n_total", "events", "level", "km", "km_risk", "engine",
                                "chunk", "passes"))
  expect_identical(names(mem), names(st)[1:8])
  expect_identical(st$engine, "stream")
  expect_identical(st$chunk, 50L)
  expect_identical(st$passes, 1L)
  got <- jsonlite::fromJSON(readLines(pf, warn = FALSE))
  expect_identical(as.integer(got$max), 1L)
  expect_identical(as.integer(got$pass), 1L)
  unlink(pf)
  expect_silent(.ssc_stream(df, spec, "rmst", 50L))
  # the engine sentence of the value
  expect_match(.sr_engine_fragment(st, "rmst"), "computed exactly from Kaplan-Meier counts", fixed = TRUE)
})

test_that("emitted rmst stream code passes the emit check and is ASCII", {
  for (s in list(.rs_spec(), .rs_spec(exposure = "grp", exposure_ref = "A"))) {
    body <- .ssc_body(s, "rmst", 50L)
    expect_silent(.sr_emit_check(body, "analysis", stream = TRUE))
    expect_false(any(utf8ToInt(body) > 127L))
  }
})
