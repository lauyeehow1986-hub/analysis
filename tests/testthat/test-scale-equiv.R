# Memory vs stream equivalence (P1e.1 Task 15): the route matrix through sr_run and sr_result, the edge
# cases, and the carried-in pins (NaN, Inf, tiny chunks, the emit checker scope, a tampered extract and the
# SAP_v1 of a pre-P1e lock). Synthetic data only.
#
# Tolerances (spec section 10): 1e-8 for estimates, model SEs, intervals and marginal effects; 1e-7 for the
# robust and cluster SEs; identical strings for Table 1. The compile-level harness (.ssc_*) is in
# helper-scale.R.

# ---- the matrix: one project per outcome kind, built once ----------------------------------------------

.seq_terms <- list(
  linear = list(terms = list(list(var = "x1"), list(var = "x2"))),
  factor = list(terms = list(list(var = "x1"), list(var = "grp", type = "factor"))),
  rcs = list(terms = list(list(var = "x1"), list(var = "x2", type = "rcs"))),
  quad = list(terms = list(list(var = "x1"), list(var = "x2", type = "quad"))),
  `factor:linear` = list(terms = list(list(var = "x1"), list(var = "grp", type = "factor")),
                         interactions = list(c("grp", "x1")))
)

# The cells: route x se x terms at chunk 1000; chunk 7 on the linear and factor terms with cluster SEs;
# chunk 25000 (one chunk for 20000 rows) on the linear terms with cluster SEs; exclude_rows on one case per
# route; marginal over "all" on logistic and modpois. modpois has no model SE (refused, pinned below).
.seq_cells <- function() {
  out <- list()
  add <- function(route, se, terms, chunk, extra = list(), tag = "") {
    out[[length(out) + 1L]] <<- list(route = route, se = se, terms = terms, chunk = chunk, extra = extra,
                                      tag = tag)
  }
  for (route in c("lm", "logistic", "modpois")) {
    for (se in c("model", "robust", "cluster")) {
      if (identical(route, "modpois") && identical(se, "model")) next
      for (tm in names(.seq_terms)) add(route, se, tm, 1000L)
    }
    for (tm in c("linear", "factor")) add(route, "cluster", tm, 7L)
    add(route, "cluster", "linear", 25000L)
    add(route, "robust", "factor", 1000L, list(exclude_rows = c(3, 500, 19999)), " exclude_rows")
    if (!identical(route, "lm")) {
      add(route, "robust", "factor", 1000L, list(standardize_over = "all"), " over all")
    }
  }
  for (i in seq_along(out)) out[[i]]$id <- sprintf("c%02d", i)
  out
}

.seq_spec <- function(cell, engine) {
  t <- .seq_terms[[cell$terms]]
  s <- .ssc_spec(cell$route, se = cell$se, terms = t$terms)
  if (!is.null(t$interactions)) s$interactions <- t$interactions
  for (nm in names(cell$extra)) s[[nm]] <- cell$extra[[nm]]
  s$engine <- engine
  if (identical(engine, "stream")) s$stream_chunk <- cell$chunk
  s
}

.seq_t1_spec <- function(engine) {
  list(purpose = "describe", layers = .ssc_layers(), by = "sex",
       vars = c("x1", "x2", "xt", "grp", "late", "yc"), nonnormal = "x2", seed = 42L, engine = engine)
}

# The projects live for the whole file: kind "bin" (outcome y, logistic and modpois) and "lm" (outcome yc,
# lm and Table 1). Every cell has a memory analysis m<id> and a stream analysis s<id>.
.seq_cache <- new.env(parent = emptyenv())
.seq_project <- function(kind) {
  if (!is.null(.seq_cache[[kind]])) return(.seq_cache[[kind]])
  df <- .ssc_data(20000L)
  out <- if (identical(kind, "lm")) "yc" else "y"
  roles <- c(id = "id", sex = "exposure")
  roles[[out]] <- "outcome"
  p <- .fx_ready_project(df, "t", roles, env = testthat::teardown_env())
  cells <- Filter(function(c) identical(c$route == "lm", kind == "lm"), .seq_cells())
  first <- TRUE
  for (c in cells) {
    p <- sr_sap_add(p, paste0("m", c$id), c$route, .seq_spec(c, "memory"), "t",
                    label = if (first) "primary" else "secondary")
    first <- FALSE
    p <- sr_sap_add(p, paste0("s", c$id), c$route, .seq_spec(c, "stream"), "t", label = "secondary")
  }
  if (identical(kind, "lm")) {
    p <- sr_sap_add(p, "mt1", "table1", .seq_t1_spec("memory"), "t", label = "secondary")
    p <- sr_sap_add(p, "st1", "table1", .seq_t1_spec("stream"), "t", label = "secondary")
  }
  p <- sr_sap_lock(.fx_feasible(p))
  .seq_cache[[kind]] <- p
  p
}

# Run analysis `id` of project `kind` (state carried in the cache); returns list(p, value).
.seq_run <- function(kind, id) {
  p <- sr_run(.seq_project(kind), id)
  .seq_cache[[kind]] <- p
  run <- p$state$runs[[id]]
  value <- if (isTRUE(run$ok)) readRDS(file.path(p$paths$fits, paste0(run$ok_key, ".rds")))$value else NULL
  list(p = p, run = run, value = value)
}

# The presented results: stream = memory plus the engine fragment; effects within the value tolerances.
.seq_expect_present <- function(ps, pm, id_s, id_m, se_tol, info) {
  rs <- sr_result(ps, id_s)
  rm <- sr_result(pm, id_m)
  expect_identical(names(rs$fragments), c(names(rm$fragments), "engine"), info = info)
  expect_identical(rs$fragments[names(rm$fragments)], rm$fragments, info = info)
  expect_identical(names(rs$tables), names(rm$tables), info = info)
  for (tb in names(rm$tables)) expect_identical(dim(rs$tables[[tb]]), dim(rm$tables[[tb]]), info = paste(info, tb))
  es <- rs$effects
  em <- rm$effects
  expect_identical(names(es), names(em), info = info)
  expect_identical(nrow(es), nrow(em), info = info)
  for (f in names(em)) {
    if (is.numeric(em[[f]])) {
      tol <- if (grepl("se", f, fixed = TRUE)) se_tol else 1e-8
      expect_equal(es[[f]], em[[f]], tolerance = max(tol, se_tol), info = paste(info, f))
    } else {
      expect_identical(es[[f]], em[[f]], info = paste(info, f))
    }
  }
}

for (cell in .seq_cells()) {
  local({
    c <- cell
    nm <- sprintf("equiv %s %s %s chunk %d%s", c$route, c$se, c$terms, c$chunk, c$tag)
    test_that(nm, {
      kind <- if (identical(c$route, "lm")) "lm" else "bin"
      m <- .seq_run(kind, paste0("m", c$id))
      s <- .seq_run(kind, paste0("s", c$id))
      expect_true(isTRUE(m$run$ok), info = m$run$error)
      expect_true(isTRUE(s$run$ok), info = s$run$error)
      expect_identical(m$run$ok_engine, "memory")
      expect_identical(s$run$ok_engine, "stream")
      expect_equal(s$value$chunk, c$chunk)
      .ssc_expect(s$value, m$value, c$route, .ssc_tol(c$se), nm)
      if (!identical(c$route, "lm") && !is.null(m$value$marginal$rows)) {
        expect_identical(s$value$marginal$over, c$extra$standardize_over %||% "analysis", info = nm)
      }
      .seq_expect_present(s$p, m$p, paste0("s", c$id), paste0("m", c$id), .ssc_tol(c$se), nm)
    })
  })
}

test_that("equiv table1 chunk default", {
  m <- .seq_run("lm", "mt1")
  s <- .seq_run("lm", "st1")
  expect_true(isTRUE(m$run$ok), info = m$run$error)
  expect_true(isTRUE(s$run$ok), info = s$run$error)
  rm <- sr_result(m$p, "mt1")
  rs <- sr_result(s$p, "st1")
  expect_identical(rs$tables, rm$tables)
  expect_identical(rs$fragments[names(rm$fragments)], rm$fragments)
  expect_match(rs$fragments$engine, "Descriptive statistics were computed exactly by SQL aggregation", fixed = TRUE)
  expect_equal(rs$effects, rm$effects, tolerance = 1e-10)
})

test_that("modpois with model SEs is refused under stream", {
  s <- utils::modifyList(.ssc_spec("modpois", se = "model"), list(engine = "stream", stream_chunk = 1000L))
  expect_error(sr_compile(.ssc_route("modpois"), s), "robust or cluster SE")
})

# ---- edge cases (Review Focus 3 and 5) -------------------------------------------------------------

# Memory and stream on the same rows: the same error text, or values equal within the tolerances.
.seq_both <- function(df, spec, id, chunk = 300L, info = "") {
  m <- tryCatch(.ssc_memory(df, spec, id), error = function(e) structure(conditionMessage(e), class = "seq_err"))
  s <- tryCatch(.ssc_stream(df, spec, id, chunk), error = function(e) structure(conditionMessage(e), class = "seq_err"))
  if (inherits(m, "seq_err")) {
    expect_true(inherits(s, "seq_err"), info = paste(info, "stream did not stop; memory:", m))
    expect_identical(unclass(s), unclass(m), info = info)
  } else {
    expect_false(inherits(s, "seq_err"), info = paste(info, unclass(s)))
    if (!inherits(s, "seq_err")) .ssc_expect(s, m, id, .ssc_tol(spec$se %||% "model"), info)
  }
  invisible(list(m = m, s = s))
}

test_that("edge: no complete case", {
  df <- .ssc_data(1200L)
  df$x1[seq(1L, nrow(df), 2L)] <- NA
  df$y[seq(2L, nrow(df), 2L)] <- NA
  df$yc[seq(2L, nrow(df), 2L)] <- NA
  for (id in c("lm", "logistic", "modpois")) {
    r <- .seq_both(df, .ssc_spec(id, se = "robust"), id, info = id)
    expect_match(unclass(r$m), "no complete rows", info = id)
  }
})

test_that("edge: a single cluster", {
  df <- .ssc_data(1200L)
  df$pid <- 1L
  for (id in c("lm", "logistic", "modpois")) {
    r <- .seq_both(df, .ssc_spec(id, se = "cluster"), id, info = id)
    expect_match(unclass(r$m), "se = cluster needs at least two clusters", fixed = TRUE, info = id)
  }
})

test_that("edge: an all-0 and an all-1 outcome", {
  df <- .ssc_data(1200L)
  for (v in c(0L, 1L)) {
    d <- df
    d$y[!is.na(d$y)] <- v
    for (id in c("logistic", "modpois")) {
      r <- .seq_both(d, .ssc_spec(id, se = "robust"), id, info = paste(id, v))
      expect_match(unclass(r$m), "no events or no non-events", fixed = TRUE, info = paste(id, v))
    }
  }
})

test_that("edge: an aliased column", {
  df <- .ssc_data(1500L)
  df$x3 <- 2 * df$x1
  terms <- list(list(var = "x1"), list(var = "x3"), list(var = "grp", type = "factor"))
  for (id in c("lm", "logistic", "modpois")) {
    for (se in c("model", "robust", "cluster")) {
      if (identical(id, "modpois") && identical(se, "model")) next
      r <- .seq_both(df, .ssc_spec(id, se = se, terms = terms), id, info = paste(id, se))
      expect_identical(r$m$aliased, "t3", info = paste(id, se))
    }
  }
})

test_that("edge: maxit non-convergence (complete separation) sets converged = FALSE in both", {
  df <- .ssc_data(1500L)
  df$y <- as.integer(df$x1 > 0)
  df$y[is.na(df$x1)] <- 0L
  r <- .seq_both(df, .ssc_spec("logistic", se = "model", terms = list(list(var = "x1"))), "logistic",
                 info = "complete separation")
  expect_false(r$m$converged)
  expect_false(r$s$converged)
  expect_identical(r$s$iter, 25L)
  expect_true(r$s$near_separation)
})

test_that("edge: quasi-separation sets near_separation in both", {
  set.seed(7)
  df <- .ssc_data(1500L)
  df$xs <- as.integer(stats::runif(nrow(df)) < 0.1)
  df$y[df$xs == 1L] <- 1L
  r <- .seq_both(df, .ssc_spec("logistic", se = "model", terms = list(list(var = "x1"), list(var = "xs"))),
                 "logistic", info = "quasi separation")
  expect_true(r$m$near_separation)
  expect_true(r$s$near_separation)
  expect_true(r$s$converged)
})

test_that("edge: an NA cluster id", {
  df <- .ssc_data(1200L)
  df$pid[c(40L, 700L)] <- NA
  for (id in c("lm", "logistic", "modpois")) {
    r <- .seq_both(df, .ssc_spec(id, se = "cluster"), id, info = id)
    # neither engine stops: .seq_both compared the two values (n, n_clusters, coefficients and SEs)
    expect_false(inherits(r$m, "seq_err"), info = id)
  }
})

test_that("edge: exclude_rows naming every row", {
  df <- .ssc_data(400L)
  for (id in c("lm", "logistic", "modpois")) {
    s <- utils::modifyList(.ssc_spec(id, se = "robust"), list(exclude_rows = as.numeric(seq_len(nrow(df)))))
    r <- .seq_both(df, s, id, info = id)
    expect_true(inherits(r$m, "seq_err"), info = id)
  }
})

test_that("edge: an all-NA column", {
  df <- .ssc_data(1200L)
  df$allna <- NA_real_
  terms <- list(list(var = "x1"), list(var = "allna"))
  for (id in c("lm", "logistic", "modpois")) {
    r <- .seq_both(df, .ssc_spec(id, se = "robust", terms = terms), id, info = id)
    expect_true(inherits(r$m, "seq_err"), info = id)
  }
})

test_that("edge: a text outcome with event_value", {
  df <- .ssc_data(1500L)
  df$y <- ifelse(is.na(df$y), NA, ifelse(df$y == 1L, "yes", "no"))
  for (id in c("logistic", "modpois")) {
    s <- .ssc_spec(id, se = "robust")
    s$event_value <- "yes"
    r <- .seq_both(df, s, id, info = id)
    expect_false(inherits(r$s, "seq_err"), info = id)
    expect_identical(r$s$events, sum(df$y == "yes" & !is.na(df$x1), na.rm = TRUE), info = id)
  }
  # without event_value both refuse with the same text
  r <- .seq_both(df, .ssc_spec("logistic", se = "robust"), "logistic", info = "no event_value")
  expect_match(unclass(r$m), "set event_value", fixed = TRUE)
})

test_that("edge: the non-ASCII factor grp as the exposure", {
  df <- .ssc_data(2000L)
  terms <- list(list(var = "x1"), list(var = "x2"))
  for (id in c("lm", "logistic", "modpois")) {
    r <- .seq_both(df, .ssc_spec(id, se = "robust", exposure = "grp", ref = "A", terms = terms), id, info = id)
    expect_true("\u00e9" %in% r$s$levels[[1L]] || any(grepl("\u00e9", unlist(r$s$levels), fixed = TRUE)), info = id)
    expect_identical(r$s$marginal$rows$level, r$m$marginal$rows$level, info = id)
  }
})

# ---- carried-in pins ---------------------------------------------------------------------------------

test_that("NaN is missing in stream as in memory (levels, knots and complete cases)", {
  df <- .ssc_data(1500L)
  # under 5% NaN, so the end-value rule does not route the SQL knots to the exact table path
  df$x2[seq(3L, nrow(df), 41L)] <- NaN
  df$x1[seq(4L, nrow(df), 19L)] <- NaN
  df$xn <- round(df$x2)
  for (id in c("lm", "logistic")) {
    # x2 rcs: the SQL knots skip NaN as .h_knots does
    r <- .seq_both(df, .ssc_spec(id, se = "robust"), id, info = paste(id, "rcs"))
    expect_identical(r$s$n, r$m$n)
    # a numeric factor with NaN: NaN is not a level
    terms <- list(list(var = "x1"), list(var = "xn", type = "factor"))
    .seq_both(df, .ssc_spec(id, se = "model", terms = terms), id, info = paste(id, "factor"))
  }
  # the knots directly, through the many-values (SQL quantile) path
  path <- .sc_parquet(df)
  con <- .sc_con(path)
  src <- paste0("read_parquet('", gsub("'", "''", normalizePath(path, winslash = "/"), fixed = TRUE), "')")
  expect_equal(.h_st_knots(con, src, "x2", 4L, "TRUE", 1e7), .h_knots(df$x2, 4L), tolerance = 1e-12)
})

test_that("the exclude_rows predicate keeps .sr_row order over many row groups and threads", {
  # found by tools/scale_equiv.R at 1e6 rows: NOT IN (SELECT ...) was a parallel join that reordered rows
  path <- tempfile(fileext = ".parquet")
  withr::defer(unlink(path))
  c0 <- DBI::dbConnect(duckdb::duckdb())
  DBI::dbExecute(c0, sprintf(paste0("COPY (SELECT i AS \".sr_row\", i * 0.5 AS x FROM range(1, 600001) t(i)) ",
                                    "TO '%s' (FORMAT PARQUET, ROW_GROUP_SIZE 20000)"), gsub("'", "''", path)))
  DBI::dbDisconnect(c0, shutdown = TRUE)
  con <- .h_st_open(path, "2GB", 4L, tempdir())
  withr::defer(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE))
  src <- paste0("read_parquet('", gsub("'", "''", normalizePath(path, winslash = "/"), fixed = TRUE), "')")
  ex <- c(3, 500, seq(1000, 590000, by = 997), 599999)
  w <- .h_st_excl(con, ex)
  r <- .h_st_pass(con, paste0("SELECT \".sr_row\" FROM ", src, " WHERE (", w, ")"), 50000L, 0,
                  function(st, df) st + nrow(df))
  expect_identical(r$rows, 600000 - length(ex))
})

test_that("an infinite value in a chunk: the memory and stream error texts (pinned, they differ)", {
  df <- .ssc_data(1200L)
  df$x1[7L] <- Inf
  # memory: lm.fit / glm.fit refuse the design; stream: the chunk QR (lm) or the IRLS working-value check
  # (glm) refuses it. Both stop; only the texts differ.
  for (id in c("lm", "logistic", "modpois")) {
    spec <- .ssc_spec(id, se = if (identical(id, "modpois")) "robust" else "model")
    m <- .ssc_err(.ssc_memory(df, spec, id))
    s <- .ssc_err(.ssc_stream(df, spec, id, 300L))
    expect_identical(m, "NA/NaN/Inf in 'x'", info = id)
    if (identical(id, "lm")) {
      expect_identical(s, "NA/NaN/Inf in foreign function call (arg 1)", info = id)
    } else {
      expect_identical(s, "the streaming fit met a non-finite working value; refit in memory", info = id)
    }
  }
})

test_that("tiny chunks (1 and 7) with an aliased term, an interaction and exclude_rows", {
  df <- .ssc_data(240L)
  df$x3 <- 3 * df$x1
  terms <- list(list(var = "x1"), list(var = "x3"), list(var = "grp", type = "factor"))
  for (id in c("lm", "logistic")) {
    for (k in c(1L, 7L)) {
      s <- .ssc_spec(id, se = "cluster", terms = terms)
      s$interactions <- list(c("grp", "x1"))
      s$exclude_rows <- c(2, 50, 239)
      info <- paste(id, "chunk", k)
      r <- .seq_both(df, s, id, chunk = k, info = info)
      expect_identical(r$m$aliased, "t3", info = info)
      expect_identical(r$s$n_total, nrow(df) - 3L, info = info)
      expect_true(any(grepl(":", r$s$coef$term, fixed = TRUE)), info = info)
    }
  }
})

test_that("the emit checker allows DBI and duckdb in streaming code only, and dbConnect in .h_st_open only", {
  ex <- list(rel = paste0("data/extracts/", strrep("a", 64L), ".parquet"), sha = strrep("a", 64L))
  pro <- .sr_run_prologue_stream(ex)
  expect_silent(.sr_emit_check(pro, "analysis", prologue = TRUE, stream = TRUE))
  expect_error(.sr_emit_check(pro, "analysis", prologue = TRUE), "namespace 'DBI' is not allowed")
  body <- sr_compile(.ssc_route("logistic"),
                     utils::modifyList(.ssc_spec("logistic", se = "robust"), list(engine = "stream", stream_chunk = 100L)))
  expect_silent(.sr_emit_check(body, "analysis", stream = TRUE))
  expect_error(.sr_emit_check(body, "analysis"), "namespace '(DBI|duckdb)' is not allowed")
  # every memory body passes without the stream allowance
  for (id in c("lm", "logistic", "modpois")) {
    expect_silent(.sr_emit_check(sr_compile(.ssc_route(id), .ssc_spec(id, se = "robust")), "analysis"))
  }
  # a connection opened outside .h_st_open is refused, bare, namespaced or as a value
  for (code in c("con <- DBI::dbConnect(duckdb::duckdb())", "con <- duckdb::duckdb()",
                 "f <- DBI::dbConnect", "g <- function() DBI::dbConnect(x)", "dbConnect(x)",
                 ".h_st_other <- function() DBI::dbConnect(duckdb::duckdb())",
                 ".h_st_open <- 1; y <- DBI::dbConnect(z)")) {
    expect_error(.sr_emit_check(code, "analysis", stream = TRUE), "allowed inside .h_st_open only", info = code)
  }
  expect_silent(.sr_emit_check(".h_st_open <- function(p) DBI::dbConnect(duckdb::duckdb(), dbdir = p)",
                               "analysis", stream = TRUE))
  # the only DuckDB connection in the emitted stream code is the one in .h_st_open
  expect_identical(lengths(regmatches(paste(pro, body), gregexpr("dbConnect(", paste(pro, body), fixed = TRUE))),
                   1L)
})

test_that("a tampered Parquet extract fails sr_freeze_verify", {
  withr::local_options(sr.stream_chunk_rows = 700)
  spec <- .ssc_spec("logistic", se = "robust", terms = list(list(var = "x1")))
  spec$engine <- "stream"
  p <- .sc_locked(spec, env = environment())
  p <- sr_run(p, "a1")
  expect_true(isTRUE(p$state$runs$a1$ok), info = p$state$runs$a1$error)
  p <- sr_freeze(p)
  lab <- names(p$state$freezes)[1L]
  expect_true(isTRUE(sr_freeze_verify(p, lab)$ok))
  rel <- paste0("data/extracts/", p$state$runs$a1$ok_extract_sha, ".parquet")
  pq <- file.path(p$paths$dir, rel)
  expect_true(file.exists(pq))
  b <- readBin(pq, "raw", n = file.size(pq))
  i <- length(b) %/% 2L
  b[i] <- as.raw(bitwXor(as.integer(b[i]), 0xFFL))
  writeBin(b, pq)
  v <- sr_freeze_verify(p, lab)
  expect_false(v$ok)
  expect_true(paste0(rel, ": SHA-256 differs from the manifest") %in% v$problems)
})

# The text (headings, paragraphs, table captions and cells) of the SAP_v1 blocks of a replayed state.
.seq_sapdoc_text <- function(state) {
  # the fixture is events only (no table store), so the lock checklist is stubbed out; the engine table does
  # not depend on it
  .ui_stub("sr_sap_check", function(project, ...) data.frame(item = character(), status = character(),
                                                             detail = character()))
  b <- .sr_sd_sap_blocks(list(state = state), locked = TRUE)
  paste(as.character(unlist(b, use.names = FALSE)),
        collapse = "\n")
}

test_that("the SAP_v1 of a pre-P1e lock (fixture) has no engine table", {
  ev <- testthat::test_path("fixtures", "pre-p1e-lock", "events.jsonl")
  state <- sr_state_replay(sr_events_read(ev))
  expect_true(isTRUE(state$sap$locked) || !is.null(state$sap$lock))
  expect_null(state$sap$lock$engines)
  expect_null(.sr_sd_engine_table(list(state = state)))
  doc <- .seq_sapdoc_text(state)
  expect_false(grepl("Computational engine", doc, fixed = TRUE))
  # the same state with lock engines does have it
  st2 <- state
  st2$sap$lock$engines <- stats::setNames(lapply(names(state$analyses), function(i) list(engine = "stream", reason = "test")),
                                         names(state$analyses))
  expect_true(grepl("Computational engine", .seq_sapdoc_text(st2), fixed = TRUE))
})
