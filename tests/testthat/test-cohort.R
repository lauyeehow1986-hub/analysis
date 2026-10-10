# Tests for io/cohort.R: the cohort builder (criteria, windows, features, time-to-event) and its flow.
# Synthetic data only; everything lives under withr::local_tempdir(). The expected values are computed in
# base R from the same data frames, never from the SQL under test.

.ch_project <- function(env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  p
}

.ch_ing <- function(p, df, table) {
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  sr_ingest(p, csv, table = table,
            options = list(drive_info = function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())))
}

.ch_t0 <- as.Date("2020-06-01")

# patients (320 ids, 20 of them without any visit), visits (1-5 per patient, clinic or phone), labs
# (creatinine / urea, with same-day ties and values above 1000), deaths (about 40%, some before the index,
# some after the horizon, a few with a second row). Fixed seed.
.ch_data <- function() {
  set.seed(21)
  ids <- sprintf("P%03d", 1:320)
  patients <- data.frame(id = ids, site = rep(c("A", "B"), 160),
                         last_fu_date = .ch_t0 + sample(300:1500, 320), stringsAsFactors = FALSE)
  patients$last_fu_date[sample(320, 12)] <- NA
  nv <- sample(1:5, 300, replace = TRUE)
  visits <- data.frame(id = rep(ids[1:300], nv), visit_date = .ch_t0 + sample(0:500, sum(nv), replace = TRUE),
                       visit_type = sample(c("clinic", "phone"), sum(nv), replace = TRUE, prob = c(0.7, 0.3)),
                       stringsAsFactors = FALSE)
  nl <- sample(0:6, 300, replace = TRUE)
  labs <- data.frame(id = rep(ids[1:300], nl), lab_date = .ch_t0 + sample(-100:600, sum(nl), replace = TRUE),
                     test = sample(c("creatinine", "urea"), sum(nl), replace = TRUE, prob = c(0.7, 0.3)),
                     stringsAsFactors = FALSE)
  labs$value <- ifelse(runif(nrow(labs)) < 0.04, 1000 + sample(1:500, nrow(labs), replace = TRUE),
                       round(runif(nrow(labs), 40, 400)))
  tie <- labs[sample(nrow(labs), 60), ]            # same id, same date, same test, another value
  tie$value <- tie$value + sample(1:9, nrow(tie), replace = TRUE)
  labs <- rbind(labs, tie)
  dead <- sample(300, 120)
  deaths <- data.frame(id = ids[dead], death_date = .ch_t0 + sample(-50:900, 120, replace = TRUE), died = 1L,
                       stringsAsFactors = FALSE)
  deaths <- rbind(deaths, data.frame(id = ids[dead[1:10]], death_date = deaths$death_date[1:10] + 20L, died = 1L,
                                     stringsAsFactors = FALSE))
  list(patients = patients, visits = visits, labs = labs, deaths = deaths)
}

.ch_fixture <- function(env = parent.frame()) {
  d <- .ch_data()
  p <- .ch_project(env)
  for (t in names(d)) p <- .ch_ing(p, d[[t]], t)
  list(p = p, d = d)
}

.ch_creat <- list(col = "test", op = "==", value = "creatinine")
.ch_urea <- list(col = "test", op = "==", value = "urea")

.ch_spec <- function(name = "cohort", outcome = TRUE, ...) {
  sr_cohort_spec(
    base = "patients", id = "id",
    index = list(table = "visits", date_col = "visit_date", where = list(col = "visit_type", op = "==", value = "clinic"),
                 pick = "first"),
    criteria = list(
      list(label = "Creatinine in the year before index", type = "include", table = "labs", date_col = "lab_date",
           where = .ch_creat, window = c(-365, 0)),
      list(label = "No creatinine above 1000 in the month before index", type = "exclude", table = "labs",
           date_col = "lab_date", where = list(all = list(.ch_creat, list(col = "value", op = ">", value = 1000))),
           window = c(-30, 0))),
    features = list(
      list(name = "creat_last", table = "labs", col = "value", agg = "last", window = c(-365, 0), where = .ch_creat,
           date_col = "lab_date"),
      list(name = "creat_first90", table = "labs", col = "value", agg = "first", window = c(-90, 0), where = .ch_creat,
           date_col = "lab_date"),
      list(name = "creat_max90", table = "labs", col = "value", agg = "max", window = c(-90, 0), where = .ch_creat,
           date_col = "lab_date"),
      list(name = "creat_mean90", table = "labs", col = "value", agg = "mean", window = c(-90, 0), where = .ch_creat,
           date_col = "lab_date"),
      list(name = "urea_any", table = "labs", col = "value", agg = "any", window = c(-365, 0), where = .ch_urea,
           date_col = "lab_date"),
      list(name = "visits_prior", table = "visits", col = "visit_date", agg = "count", window = c(-365, -1),
           date_col = "visit_date")),
    outcome = if (outcome) list(event_table = "deaths", event_date_col = "death_date", censor_date_col = "last_fu_date",
                                horizon_days = 365) else NULL,
    name = name, ...)
}

# ---- the expected values, in base R ------------------------------------------------------------------

.ch_expected <- function(d) {
  v1 <- d$visits[d$visits$visit_type == "clinic", ]
  idx_num <- tapply(as.numeric(v1$visit_date), v1$id, min)
  idx <- stats::setNames(as.Date(as.numeric(idx_num), origin = "1970-01-01"), names(idx_num))
  base_ids <- unique(d$patients$id)
  with_idx <- sort(base_ids[base_ids %in% names(idx)])
  days <- function(df, col) as.numeric(df[[col]] - idx[df$id])          # NA when the id has no index
  inwin <- function(x, lo, hi) !is.na(x) & x >= lo & x <= hi
  labs <- d$labs
  labs$day <- days(labs, "lab_date")
  creat <- labs[labs$test == "creatinine", ]
  inc <- unique(creat$id[inwin(creat$day, -365, 0)])
  exc <- unique(creat$id[creat$value > 1000 & inwin(creat$day, -30, 0)])
  after_inc <- sort(with_idx[with_idx %in% inc])
  ids <- sort(after_inc[!(after_inc %in% exc)])

  pick <- function(df, id, lo, hi, which) {                              # first / last with the tie rule
    s <- df[df$id == id & inwin(df$day, lo, hi), ]
    if (!nrow(s)) return(NA_real_)
    s <- s[order(s$lab_date, s$value), ]
    if (which == "first") s$value[1L] else s$value[nrow(s)]
  }
  agg <- function(df, id, lo, hi, f) {
    s <- df$value[df$id == id & inwin(df$day, lo, hi)]
    if (length(s)) f(s) else NA_real_
  }
  vis <- d$visits
  vis$day <- days(vis, "visit_date")
  urea <- labs[labs$test == "urea", ]
  out <- data.frame(
    id = ids, index_date = unname(idx[ids]),
    creat_last = vapply(ids, function(i) pick(creat, i, -365, 0, "last"), 0, USE.NAMES = FALSE),
    creat_first90 = vapply(ids, function(i) pick(creat, i, -90, 0, "first"), 0, USE.NAMES = FALSE),
    creat_max90 = vapply(ids, function(i) agg(creat, i, -90, 0, max), 0, USE.NAMES = FALSE),
    creat_mean90 = vapply(ids, function(i) agg(creat, i, -90, 0, mean), 0, USE.NAMES = FALSE),
    urea_any = vapply(ids, function(i) any(urea$id == i & inwin(urea$day, -365, 0)), TRUE, USE.NAMES = FALSE),
    visits_prior = vapply(ids, function(i) sum(vis$id == i & inwin(vis$day, -365, -1)), 0, USE.NAMES = FALSE),
    stringsAsFactors = FALSE)

  dth <- d$deaths
  dth$day <- days(dth, "death_date")
  ev <- vapply(ids, function(i) {
    s <- dth$day[dth$id == i & inwin(dth$day, 0, 365)]
    if (length(s)) min(s) else NA_real_
  }, 0, USE.NAMES = FALSE)
  fu <- stats::setNames(d$patients$last_fu_date, d$patients$id)[ids]
  cens <- as.numeric(fu - idx[ids])
  out$time <- ifelse(!is.na(ev), ev, ifelse(is.na(cens), 365, pmin(cens, 365)))
  out$event <- as.integer(!is.na(ev))
  list(table = out, n = c(length(base_ids), length(with_idx), length(after_inc), length(ids)))
}

.ch_read <- function(p, table) {
  sr_store_view(p, table)
  sr_duck_query(sr_duck(p), sprintf("SELECT * FROM %s ORDER BY id", DBI::dbQuoteIdentifier(sr_duck(p), table)))
}

.ch_n_events <- function(p) length(sr_events_read(p$paths$events))

# ---- tests ------------------------------------------------------------------------------------------

test_that("the flow counts, features, time and event match a base-R computation", {
  fx <- .ch_fixture()
  e <- .ch_expected(fx$d)
  expect_gt(length(e$table$id), 50L)                        # a meaningful cohort
  expect_true(any(e$table$event == 1L) && any(e$table$event == 0L))
  expect_true(anyNA(e$table$creat_first90))                 # empty windows are exercised
  expect_true(any(e$table$visits_prior == 0) && any(!e$table$urea_any))
  expect_true(anyNA(fx$d$patients$last_fu_date))

  p <- sr_cohort_build(fx$p, .ch_spec())
  fl <- p$state$cohorts$cohort$flow
  expect_identical(fl$step, c("index", "c1", "c2"))
  expect_equal(fl$n_before, c(e$n[1:3]))
  expect_equal(fl$n_after, c(e$n[2:4]))
  expect_identical(fl$outcome_dependent, c(FALSE, FALSE, FALSE))

  got <- .ch_read(p, "cohort")
  expect_identical(names(got), names(e$table))
  expect_identical(got$id, e$table$id)
  expect_identical(got$index_date, e$table$index_date)
  for (col in setdiff(names(e$table), c("id", "index_date"))) expect_equal(as.numeric(got[[col]]), as.numeric(e$table[[col]]), info = col)
  expect_type(got$urea_any, "logical")
  expect_false(anyNA(got$visits_prior))
  expect_false(anyNA(got$time))
})

test_that("the payload SQL re-run gives the identical table, ordered by id", {
  fx <- .ch_fixture()
  p <- sr_cohort_build(fx$p, .ch_spec())
  ev <- Filter(function(x) identical(x$type, "cohort.built"), sr_events_read(p$paths$events))
  expect_length(ev, 1L)
  pl <- ev[[1L]]$payload
  expect_identical(pl$name, "cohort")
  expect_identical(pl$store_hash, sr_store_hash(sr_store_manifest(p, "cohort")))
  expect_identical(p$state$cohorts$cohort$store_hash, pl$store_hash)
  again <- sr_duck_query(sr_duck(p), pl$sql$final)
  got <- .ch_read(p, "cohort")
  expect_identical(again, got)
  expect_false(is.unsorted(got$id))
  # sr_cohort_sql gives the same SQL, one CTE per flow step, and runs nothing
  n0 <- .ch_n_events(p)
  s <- sr_cohort_sql(p, .ch_spec(name = "other"))
  expect_identical(s$final, pl$sql$final)
  expect_identical(names(s$steps), c("sr_x_base", "sr_x_index", "sr_x_c1", "sr_x_c2"))
  expect_identical(.ch_n_events(p), n0)
  expect_false("other" %in% sr_store_tables(p))
  # the same build twice gives the same store hash
  q <- sr_cohort_build(p, .ch_spec(name = "cohort2"))
  expect_identical(q$state$cohorts$cohort2$store_hash, p$state$cohorts$cohort$store_hash)
})

test_that("the new table is a derived table with the id, outcome and time roles", {
  fx <- .ch_fixture()
  p <- sr_cohort_build(fx$p, .ch_spec())
  tb <- p$state$tables$cohort
  expect_identical(tb$format, "cohort")
  expect_null(tb$source)
  expect_setequal(unlist(tb$derived_from), c("patients", "visits", "labs", "deaths"))
  expect_identical(sr_source_status(p, "cohort")$status, "derived")
  expect_true("cohort" %in% sr_store_tables(p))
  expect_identical(sr_roles(p, "cohort"), c(id = "id", event = "outcome", time = "time"))
  expect_identical(sr_role_cols(p, "cohort", "outcome"), "event")
  # without an outcome only the id is a role
  q <- sr_cohort_build(fx$p, .ch_spec(name = "plain", outcome = FALSE))
  expect_identical(sr_roles(q, "plain"), c(id = "id"))
  expect_false(any(c("time", "event") %in% names(.ch_read(q, "plain"))))
  # a reopened project replays the cohort
  dir <- p$paths$dir
  sr_duck_close(p)
  sr_project_close(p)
  r <- sr_project_open(dir)
  withr::defer(sr_duck_close(r))
  withr::defer(sr_project_close(r))
  expect_identical(r$state$tables$cohort$format, "cohort")
  expect_identical(r$state$cohorts$cohort$store_hash, sr_store_hash(sr_store_manifest(r, "cohort")))
  expect_identical(sr_roles(r, "cohort"), c(id = "id", event = "outcome", time = "time"))
  expect_equal(r$state$cohorts$cohort$flow$n_after, .ch_expected(fx$d)$n[2:4])
  expect_identical(r$state$cohorts$cohort$flow$step, c("index", "c1", "c2"))
})

test_that("a missing column errors before any SQL: no event, no table", {
  fx <- .ch_fixture()
  p <- fx$p
  n0 <- .ch_n_events(p)
  tabs0 <- sr_store_tables(p)
  bad <- function(f) {
    s <- .ch_spec()
    s <- f(s)
    expect_error(sr_cohort_build(p, s), "nope")
    expect_error(sr_cohort_sql(p, s), "nope")
  }
  bad(function(s) {
    s$criteria[[1L]]$where <- list(col = "nope", op = "==", value = 1)
    s
  })
  bad(function(s) {
    s$criteria[[2L]]$where <- list(all = list(.ch_creat, list(col = "nope", op = ">", value = 1)))
    s
  })
  bad(function(s) {
    s$features[[2L]]$col <- "nope"
    s
  })
  bad(function(s) {
    s$features[[1L]]$date_col <- "nope"
    s
  })
  bad(function(s) {
    s$features[[3L]]$where <- list(col = "nope", op = "==", value = 1)
    s
  })
  bad(function(s) {
    s$outcome$event_date_col <- "nope"
    s
  })
  bad(function(s) {
    s$outcome$censor_date_col <- "nope"
    s
  })
  bad(function(s) {
    s$index$date_col <- "nope"
    s
  })
  expect_identical(.ch_n_events(p), n0)
  expect_identical(sr_store_tables(p), tabs0)
  expect_false("cohort" %in% sr_store_tables(p))
  expect_false(dir.exists(file.path(sr_project_paths(p$paths$dir)$data, "ingest", "cohort")))
})

test_that("the other validations also run before any work", {
  fx <- .ch_fixture()
  p <- fx$p
  n0 <- .ch_n_events(p)
  chk <- function(f, msg) expect_error(sr_cohort_build(p, f(.ch_spec())), msg)
  chk(function(s) {
    s$criteria[[1L]]$table <- "missing_table"
    s
  }, "not in the store")
  chk(function(s) {
    s$criteria[[1L]]$window <- c(0, -365)
    s
  }, "wrong way round")
  chk(function(s) {
    s$features[[1L]]$window <- c(5, 1)
    s
  }, "wrong way round")
  chk(function(s) {
    s$features[[1L]]$agg <- "median"
    s
  }, "agg must be one of")
  chk(function(s) {
    s$index$pick <- "middle"
    s
  }, "pick must be one of")
  chk(function(s) {
    s$criteria[[1L]]$type <- "maybe"
    s
  }, "type must be one of")
  chk(function(s) {
    s$criteria[[1L]]$date_col <- NULL
    s
  }, "needs a date_col")
  chk(function(s) {
    s$features[[4L]]$col <- "test"
    s
  }, "mean needs a numeric column")
  chk(function(s) {
    s$criteria[[1L]]$date_col <- "test"
    s
  }, "not a date")
  chk(function(s) {
    s$features[[2L]]$name <- "creat_last"
    s
  }, "unique")
  chk(function(s) {
    s$features[[2L]]$name <- "event"
    s
  }, "clash")
  chk(function(s) {
    s$outcome$horizon_days <- -1
    s
  }, "horizon_days")
  chk(function(s) {
    s$outcome$censor_table <- "patients"
    s$outcome$censor_date_col <- NULL
    s
  }, "censor_table needs")
  chk(function(s) {
    s$criteria[[1L]]$where <- list(col = "value", op = ">", value = "abc")
    s
  }, "")
  expect_error(sr_cohort_build(p, .ch_spec(name = "Bad Name")), "invalid table name")
  expect_error(sr_cohort_spec("patients", "id", list(table = "visits"), name = "x"), "date_col")
  expect_identical(.ch_n_events(p), n0)
  expect_false("cohort" %in% sr_store_tables(p))
})

test_that("a name that collides with an existing table errors and changes nothing", {
  fx <- .ch_fixture()
  p <- fx$p
  n0 <- .ch_n_events(p)
  expect_error(sr_cohort_build(p, .ch_spec(name = "labs")), "already exists")
  p <- sr_cohort_build(p, .ch_spec())
  n1 <- .ch_n_events(p)
  expect_error(sr_cohort_build(p, .ch_spec()), "already exists")
  expect_identical(.ch_n_events(p), n1)
  expect_gt(n1, n0)
  h <- sr_store_hash(sr_store_manifest(p, "cohort"))
  expect_identical(h, p$state$cohorts$cohort$store_hash)
})

test_that("index pick, an unwindowed criterion, a censor table and no censoring follow the rules", {
  p <- .ch_project()
  pat <- data.frame(id = c("a", "b", "c", "d", "e"), stringsAsFactors = FALSE)
  vis <- data.frame(id = c("a", "a", "b", "b", "c", "d"),
                    vd = as.Date(c("2021-01-10", "2021-03-01", "2021-02-01", "2021-02-01", "2021-05-05", "2021-06-01")),
                    stringsAsFactors = FALSE)
  # events: a dies 10 days after the last visit (pick last) but 70 after the first; b: before the index
  ev <- data.frame(id = c("a", "b", "c", "c"), ed = as.Date(c("2021-03-11", "2021-01-01", "2021-05-05", "2022-12-01")),
                   stringsAsFactors = FALSE)
  fu <- data.frame(id = c("a", "a", "b", "c"), fd = as.Date(c("2021-04-01", "2021-03-20", "2021-02-11", "2021-06-01")),
                   stringsAsFactors = FALSE)
  for (nm in c("pat", "vis", "ev", "fu")) p <- .ch_ing(p, get(nm), nm)
  mk <- function(pick, name, outcome) sr_cohort_spec(
    base = "pat", id = "id", index = list(table = "vis", date_col = "vd", pick = pick),
    criteria = list(list(label = "has a visit", table = "vis")), outcome = outcome, name = name)
  oc <- list(event_table = "ev", event_date_col = "ed", horizon_days = 100, censor_table = "fu", censor_date_col = "fd")
  p <- sr_cohort_build(p, mk("last", "last_idx", oc))
  g <- .ch_read(p, "last_idx")
  expect_identical(g$id, c("a", "b", "c", "d"))                       # e has no visit
  expect_identical(g$index_date, as.Date(c("2021-03-01", "2021-02-01", "2021-05-05", "2021-06-01")))
  # a: event 10 days after its (last) index; b: the death is before the index -> ignored, censored on the
  # latest follow-up (10 days); c: event on day 0 counts; d: no event, no censor row -> the horizon
  expect_equal(g$event, c(1, 0, 1, 0))
  expect_equal(g$time, c(10, 10, 0, 100))
  fl <- p$state$cohorts$last_idx$flow
  expect_equal(fl$n_before, c(5, 4))
  expect_equal(fl$n_after, c(4, 4))
  # first pick: a's index is 2021-01-10, the event is 60 days later; the latest follow-up caps the time
  p <- sr_cohort_build(p, mk("first", "first_idx", oc))
  g <- .ch_read(p, "first_idx")
  expect_equal(g$time, c(60, 10, 0, 100))
  # horizon 30: a's event (60 days) is outside it, so a is censored at min(latest fu = 81 days, 30)
  oc30 <- oc
  oc30$horizon_days <- 30
  p <- sr_cohort_build(p, mk("first", "h30", oc30))
  g <- .ch_read(p, "h30")
  expect_equal(g$event, c(0, 0, 1, 0))
  expect_equal(g$time, c(30, 10, 0, 30))
  # no censor columns at all: censored at the horizon
  oc0 <- list(event_table = "ev", event_date_col = "ed", horizon_days = 100)
  p <- sr_cohort_build(p, mk("first", "nocens", oc0))
  g <- .ch_read(p, "nocens")
  expect_equal(g$time, c(60, 100, 0, 100))
})

test_that("while blind, an outcome-dependent criterion and every later flow row hide their counts", {
  fx <- .ch_fixture()
  p <- sr_roles_set(fx$p, "deaths", c(id = "id", died = "outcome"))
  e <- .ch_expected(fx$d)
  spec <- .ch_spec(outcome = FALSE)
  spec$criteria <- list(
    spec$criteria[[1L]],
    list(label = "No death before the index", type = "exclude", table = "deaths", date_col = "death_date",
         where = list(col = "died", op = "==", value = 1), window = c(-3650, -1)),
    spec$criteria[[2L]])
  p <- sr_cohort_build(p, spec)
  pl <- p$state$cohorts$cohort$flow
  expect_identical(pl$outcome_dependent, c(FALSE, FALSE, TRUE, FALSE))
  expect_false(anyNA(pl$n_after))                                   # the payload keeps true counts
  expect_lt(pl$n_after[3L], pl$n_before[3L])                        # some deaths were excluded
  # a recipe step on the cohort table comes after the cohort rows
  p <- sr_recipe_add(p, "cohort", list(op = "drop_cases", reason = "implausible creatinine",
                                       args = list(where = list(col = "creat_last", op = ">", value = 1e6))))
  expect_true(sr_blind(p))
  fl <- sr_flow(p, "cohort")
  expect_identical(fl$step, c("index", "c1", "c2", "c3", "s1"))
  suffix <- " (count hidden until SAP lock)"
  for (col in c("n_before", "n_after", "n_excluded")) {
    expect_false(anyNA(fl[[col]][1:2]), info = col)
    expect_true(all(is.na(fl[[col]][3:5])), info = col)
  }
  expect_false(any(endsWith(fl$label[1:2], suffix)))
  expect_true(all(endsWith(fl$label[3:5], suffix)))
  expect_false("outcome_dependent" %in% names(fl))
  # the box at the top is the starting population, not the table, whose size would give the count away
  expect_equal(attr(fl, "n_total"), e$n[1L])
  # after the lock every count is shown and chains
  p$state$sap <- list(locked = TRUE)
  fl2 <- sr_flow(p, "cohort")
  expect_false(anyNA(fl2$n_after))
  expect_equal(fl2$n_after[1:4], pl$n_after)
  expect_equal(fl2$n_before[1:4], pl$n_before)
  expect_equal(fl2$n_before[5L], pl$n_after[4L])
  expect_false(any(grepl("hidden", fl2$label)))
  # a cohort whose exclusions are not outcome-dependent shows everything while blind
  q <- sr_cohort_build(fx$p, .ch_spec(name = "open_cohort", outcome = FALSE))
  expect_true(sr_blind(q))
  expect_false(anyNA(sr_flow(q, "open_cohort")$n_after))
})
