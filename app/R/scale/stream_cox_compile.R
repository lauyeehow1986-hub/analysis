# scale/stream_cox_compile.R - the stream compile of exact Cox, cox@1 and cox_describe@1 (P1e.2, plan Task 5,
# rulings S1, S2, S3, S10, S11, S13, S15 and S18). .sr_cox_compile() returns .sr_cox_compile_stream(spec) when
# spec$engine is "stream"; the memory code is untouched.
#
# The emitted body runs after .sr_run_prologue_stream() (.sr_con, sr_src) and the stream terms setup
# (.sr_where, .sr_fixed). Every pass reads the S10 query (.sr_cox_sql): the needed columns, the stratum id
# .sr_stratum and .sr_row, sorted by the strata columns, the time column DESC and .sr_row, in chunks of
# stream_chunk rows (.h_st_pass, order = FALSE). Each chunk goes through the in-memory snippets (R8):
# .h_design, sr_outcome_code(spec, "surv"), the strata copies and the complete-case filter; then the time
# guard (.h_st_time_guard). The passes:
#   0     n, n_total, events, the column sums and {-1,0,1} flags of the design X (S1), the analysed time
#         range and the Kaplan-Meier counts by KM group (S13). The near-tie map (S3) is then built from the
#         distinct raw times of those counts (S11, S21).
#   fit   .h_st_coxfit (coxfit6's Newton loop, Task 4): each pass maps .time, centres X by the pass-0 means,
#         and runs .h_st_cox_step at the pass's beta. It returns list(loglik, U, I, sumabs, max_eta) in
#         centred, unscaled coordinates; max_eta is max(lp) with the {-1,0,1} columns uncentred, as coxph.
# The design is model.matrix of the right-hand side with the full .sr_fixed levels and without the
# intercept column (S2), so a factor level absent from the analysed rows is an all-zero column that the
# cholesky declares singular (NA coefficient, `aliased`), as coxph. The warnings are .h_st_cox_warn(fit),
# the texts coxph.fit gives. The value has the memory names, then engine, chunk, passes and iter (S18).
# Cluster-robust SE (layers$clustered, Task 6, S9, S20, S22). The cluster column is copied to .cl and is
# part of the complete-case filter, as in memory; pass 0 also notes whether two clusters exist, so one
# cluster stops where memory stops. After the fit two passes run:
#   et       a pass at the final beta (.h_st_et_step) that writes the event-time table of each chunk into
#            the TEMP table sr_et of the run connection (no file); the view sr_et_cum adds the cumulative
#            lam and b_j per stratum (columns prefixed .sr_et_, so they cannot clash with a data column)
#   cluster  the S22 query: the S10 rows ASOF LEFT JOINed to sr_et_cum on stratum and raw time, sorted by
#            the cluster; each chunk re-runs the R8 snippets and eta, and .h_st_cox_resid(...) %*% V goes
#            to .h_st_cluster_step. V_robust = D'D, with no small-sample factor, as coxph.
# Progress {"pass": k, "max": m}: m = 1 + 22 (S15), + 2 when clustered, lowered to the passes run once the
# fit ends.
#
# PEM (cox_method = "pem") is .sr_pem_compile_stream, Task 9.

#' The S10 query of the streamed Cox as R code: a paste0() expression that evaluates, in the emitted body,
#' to `SELECT <cols>, <rank> AS ".sr_stratum", ".sr_row" FROM <sr_src> WHERE (<.sr_where>)` plus, with
#' order = TRUE, `ORDER BY <strata cols>, <time col> DESC, ".sr_row"`. The identifiers are quoted (ANSI)
#' at compile time; <rank> is DENSE_RANK() OVER (ORDER BY <strata cols>), or 1 with no strata.
.sr_cox_sql <- function(spec, cols, order = TRUE) {
  q <- function(x) as.character(DBI::dbQuoteIdentifier(DBI::ANSI(), x))
  st <- .sr_spec_chr(spec[["strata"]])
  rank <- if (length(st)) paste0("DENSE_RANK() OVER (ORDER BY ", paste(q(st), collapse = ", "), ")") else "1"
  head <- paste0("SELECT ", paste(c(q(cols), paste(rank, "AS", q(".sr_stratum")), q(".sr_row")), collapse = ", "),
                 " FROM ")
  tail <- if (isTRUE(order)) {
    paste0(" ORDER BY ", paste(c(q(st), paste(q(spec[["time"]]), "DESC"), q(".sr_row")), collapse = ", "))
  } else ""
  sprintf("paste0(%s, sr_src, \" WHERE (\", .sr_where, %s)", .sr_lit(head), .sr_lit(paste0(")", tail)))
}

#' The stream body of the piecewise-exponential Cox (cox_method = "pem"): Task 9.
.sr_pem_compile_stream <- function(spec) {
  stop("the streamed PEM fit is not available yet", call. = FALSE)
}

#' The stream body of exact Cox (cox@1, cox_describe@1): one code string.
.sr_cox_compile_stream <- function(spec) {
  if (identical(.sr_cox_method_of(spec), "pem")) return(.sr_pem_compile_stream(spec))
  ch <- spec[["stream_chunk"]]
  if (is.null(ch)) stop("a stream spec needs stream_chunk (set by the run)", call. = FALSE)
  cl <- if (.sr_cox_clustered(spec)) spec[["cluster"]] else NULL
  tc <- sr_terms_code(spec, "stream")
  plan <- .sr_term_plan(spec)
  ties <- .sr_cox_ties_of(spec)
  st <- .sr_spec_chr(spec[["strata"]])
  st_names <- if (length(st)) paste0(".s", seq_along(st)) else character()
  rhs_vars <- all.vars(stats::as.formula(paste("~", tc$rhs)))
  used <- unique(c(".time", ".event", rhs_vars, st_names, if (!is.null(cl)) ".cl"))
  fac <- identical(plan[[1L]]$type, "factor")
  # the columns read; the cluster column is among them (only clustered specs have one, .sr_cox_check)
  needed <- unique(c(spec[["time"]], spec[["status"]], .sr_spec_chr(spec[["cluster"]]), st,
                     vapply(plan, function(i) i$var, character(1))))
  copy <- c(vapply(seq_along(st), function(i) {
    sprintf("  des[[%s]] <- des[[%s]]", .sr_lit(st_names[i]), .sr_lit(st[i]))
  }, character(1)), if (!is.null(cl)) sprintf("  des[[\".cl\"]] <- des[[%s]]", .sr_lit(cl)))
  m_max <- 1L + 22L + if (is.null(cl)) 0L else 2L

  helpers <- c(
    .sr_inline(.sr_h_coef_table, ".h_coef_table"),
    .sr_inline(.h_st_pass, ".h_st_pass"),
    .sr_inline(.h_st_progress, ".h_st_progress"),
    .sr_inline(.h_st_count, ".h_st_count"),
    .sr_inline(.h_st_time_guard, ".h_st_time_guard"),
    .sr_inline(.h_st_timefix_map, ".h_st_timefix_map"),
    .sr_inline(.h_st_timefix_apply, ".h_st_timefix_apply"),
    .sr_inline(.h_st_km_add, ".h_st_km_add"),
    .sr_inline(.h_st_km_counts, ".h_st_km_counts"),
    .sr_inline(.h_st_km_finish, ".h_st_km_finish"),
    .sr_inline(.h_st_chol2, ".h_st_chol2"),
    .sr_inline(.h_st_chsolve2, ".h_st_chsolve2"),
    .sr_inline(.h_st_chinv2, ".h_st_chinv2"),
    .sr_inline(.h_st_cox_wts, ".h_st_cox_wts"),
    .sr_inline(.h_st_cox_new, ".h_st_cox_new"),
    .sr_inline(.h_st_cox_blocks, ".h_st_cox_blocks"),
    .sr_inline(.h_st_cox_step, ".h_st_cox_step"),
    .sr_inline(.h_st_cox_close, ".h_st_cox_close"),
    .sr_inline(.h_st_coxfit, ".h_st_coxfit"),
    .sr_inline(.h_st_cox_warn, ".h_st_cox_warn"),
    if (!is.null(cl)) c(
      .sr_inline(.h_st_et_rows, ".h_st_et_rows"),
      .sr_inline(.h_st_et_step, ".h_st_et_step"),
      .sr_inline(.h_st_et_close, ".h_st_et_close"),
      .sr_inline(.h_st_cox_resid, ".h_st_cox_resid"),
      .sr_inline(.h_st_cluster_step, ".h_st_cluster_step"),
      .sr_inline(.h_st_cluster_flush, ".h_st_cluster_flush"))
  )

  head <- c(
    sprintf(".sr_chunk <- %s", .sr_lit(as.integer(ch))),
    sprintf(".sr_sql <- %s", .sr_cox_sql(spec, needed)),
    sprintf(".tt <- stats::delete.response(stats::terms(~ %s))", tc$rhs),
    # coxph's design: model.matrix with the intercept column removed (S2)
    ".X <- function(d) {",
    "  m <- stats::model.matrix(.tt, stats::model.frame(.tt, d, na.action = stats::na.pass))",
    "  m[, colnames(m) != \"(Intercept)\", drop = FALSE]",
    "}",
    # the in-memory snippets on one chunk (R8): design, outcome, strata copies, complete cases
    ".prep <- function(df) {",
    "  sr_data <- df",
    "  des <- .h_design(sr_data)",
    sr_outcome_code(spec, "surv"),
    copy,
    sprintf("  .keep <- stats::complete.cases(des[%s])", .sr_lit(used)),
    "  list(des = des[.keep, , drop = FALSE], s = df[[\".sr_stratum\"]][.keep], n = nrow(df), keep = .keep)",
    "}",
    # progress (R15, S15)
    ".pg <- new.env(parent = emptyenv())",
    ".pg$k <- 0L",
    sprintf(".pg$m <- %s", .sr_lit(m_max)),
    ".pg_tick <- function() {",
    "  .pg$k <- .pg$k + 1L",
    "  if (.pg$k > .pg$m) .pg$m <- .pg$k",
    "  if (exists(\"sr_progress_file\")) .h_st_progress(sr_progress_file, .pg$k, .pg$m)",
    "  invisible(NULL)",
    "}"
  )

  km_add <- if (fac) {
    "  .A$km <- .h_st_km_add(.A$km, d[[\".time\"]], d[[\".event\"]], as.character(d[[\"t1f\"]]))"
  } else {
    c("  if (!.A$km_many) {",
      "    .A$kmv <- sort(unique(c(.A$kmv, d[[\"t1\"]])))",
      "    if (length(.A$kmv) > 8L) {",
      "      .A$km_many <- TRUE",
      "      if (!is.null(.A$km)) .A$km$parts <- lapply(.A$km$parts, function(pt) {",
      "        pt$g <- rep(\"All\", nrow(pt))",
      "        pt",
      "      })",
      "    }",
      "  }",
      "  .A$km <- .h_st_km_add(.A$km, d[[\".time\"]], d[[\".event\"]], if (.A$km_many) rep(\"All\", nrow(d)) else d[[\"t1\"]])")
  }

  pass0 <- c(
    ".A <- new.env(parent = emptyenv())",
    ".A$n_total <- 0",
    ".A$n <- 0",
    ".A$events <- 0",
    ".A$cols <- NULL",
    ".A$sx <- NULL",
    ".A$z1 <- NULL",
    ".A$tlo <- Inf",
    ".A$thi <- -Inf",
    ".A$km <- NULL",
    ".A$kmv <- numeric()",
    ".A$km_many <- FALSE",
    if (!is.null(cl)) c(".A$cl1 <- NULL", ".A$cl2 <- FALSE"),
    ".h_st_pass(.sr_con, .sr_sql, .sr_chunk, NULL, function(g, df) {",
    "  pr <- .prep(df)",
    "  .A$n_total <- .A$n_total + pr$n",
    "  d <- pr$des",
    "  if (!nrow(d)) return(g)",
    "  g <- .h_st_time_guard(g, d[[\".time\"]], pr$s)",
    "  X <- .X(d)",
    "  if (is.null(.A$cols)) {",
    "    .A$cols <- colnames(X)",
    "    .A$sx <- numeric(ncol(X))",
    "    .A$z1 <- rep(TRUE, ncol(X))",
    "  }",
    "  .A$n <- .A$n + nrow(d)",
    "  .A$events <- .A$events + sum(d[[\".event\"]])",
    "  .A$sx <- .A$sx + colSums(X)",
    "  .A$z1 <- .A$z1 & apply(X, 2L, function(z) all(z %in% c(-1, 0, 1)))",
    "  .A$tlo <- min(.A$tlo, d[[\".time\"]])",
    "  .A$thi <- max(.A$thi, d[[\".time\"]])",
    km_add,
    if (!is.null(cl)) c(
      "  if (!.A$cl2) {",
      "    if (is.null(.A$cl1)) .A$cl1 <- d[[\".cl\"]][1L]",
      "    .A$cl2 <- any(d[[\".cl\"]] != .A$cl1)",
      "  }"),
    "  g",
    "}, order = FALSE)",
    ".pg_tick()",
    "if (.A$n < 1) stop(\"no complete rows: every row has a missing value in the model\")",
    ".n <- .h_st_count(.A$n)",
    "if (.A$events == 0) {",
    "  stop(\"the outcome has no events among the \", .n, \" rows analysed\")",
    "}",
    if (!is.null(cl)) "if (!.A$cl2) stop(\"layers$clustered needs at least two clusters (found 1)\")",
    ".p <- length(.A$cols)",
    ".mu <- .A$sx / .A$n",
    ".z1 <- .A$z1",
    # the near-tie map from the distinct raw times (S3, S11)
    ".map <- .h_st_timefix_map(.h_st_km_counts(.A$km, NULL)$t)"
  )

  fit <- c(
    ".passfn <- function(beta) {",
    "  .A$me <- -Inf",
    sprintf("  r <- .h_st_pass(.sr_con, .sr_sql, .sr_chunk, list(g = NULL, cx = .h_st_cox_new(.p, %s)), function(st, df) {",
            .sr_lit(ties)),
    "    pr <- .prep(df)",
    "    d <- pr$des",
    "    if (!nrow(d)) return(st)",
    "    st$g <- .h_st_time_guard(st$g, d[[\".time\"]], pr$s)",
    "    X <- .X(d)",
    "    X <- X - rep(.mu, each = nrow(X))",
    "    eta <- drop(X %*% beta)",
    "    .A$me <- max(.A$me, eta)",
    "    st$cx <- .h_st_cox_step(st$cx, .h_st_timefix_apply(d[[\".time\"]], .map), d[[\".event\"]], X, eta, pr$s)",
    "    st",
    "  }, order = FALSE)$state",
    "  .pg_tick()",
    "  out <- .h_st_cox_close(r$cx)",
    "  out$sumabs <- r$cx$sumabs",
    # coxph's lp: a {-1,0,1} column is not centred (S5a)
    "  out$max_eta <- .A$me + sum((.mu * beta)[.z1])",
    "  out",
    "}",
    ".scale <- function(r) ifelse(.z1 | r$sumabs == 0, 1, .A$n / r$sumabs)",
    ".fit <- .h_st_coxfit(.passfn, .p, .A$cols, .scale)",
    ".wrn <- .h_st_cox_warn(.fit)",
    sprintf(".pg$m <- .pg$k%s", if (is.null(cl)) "" else " + 2L"),
    "if (exists(\"sr_progress_file\")) .h_st_progress(sr_progress_file, .pg$k, .pg$m)",
    ".cf <- .fit$coef",
    ".ok <- !is.na(.cf)",
    ".nm <- names(.cf)[.ok]",
    ".V <- .fit$V[.nm, .nm, drop = FALSE]",
    ".ncl <- NA_real_"
  )

  robust <- if (!is.null(cl)) {
    q <- function(x) as.character(DBI::dbQuoteIdentifier(DBI::ANSI(), x))
    c(
      # the event-time table at the final beta (S20): a TEMP table of the run connection, no file
      ".beta <- .fit$coef",
      ".beta[is.na(.beta)] <- 0",
      ".A$et_n <- 0",
      r"{.et_put <- function(rows) {}",
      r"{  if (is.null(rows) || !nrow(rows)) return(invisible(NULL))}",
      r"{  duckdb::duckdb_register(.sr_con, "sr_et_chunk", rows)}",
      r"{  if (.A$et_n == 0) {}",
      r"{    DBI::dbExecute(.sr_con, "CREATE TEMP TABLE sr_et AS SELECT * FROM sr_et_chunk")}",
      r"{  } else {}",
      r"{    DBI::dbExecute(.sr_con, "INSERT INTO sr_et SELECT * FROM sr_et_chunk")}",
      r"{  }}",
      r"{  duckdb::duckdb_unregister(.sr_con, "sr_et_chunk")}",
      r"{  .A$et_n <- .A$et_n + nrow(rows)}",
      r"{  invisible(NULL)}",
      r"{}}",
      sprintf(".r <- .h_st_pass(.sr_con, .sr_sql, .sr_chunk, list(g = NULL, cx = .h_st_cox_new(.p, %s)), function(st, df) {",
              .sr_lit(ties)),
      r"{  pr <- .prep(df)}",
      r"{  d <- pr$des}",
      r"{  if (!nrow(d)) return(st)}",
      r"{  st$g <- .h_st_time_guard(st$g, d[[".time"]], pr$s)}",
      r"{  X <- .X(d)}",
      r"{  X <- X - rep(.mu, each = nrow(X))}",
      r"{  st$cx <- .h_st_et_step(st$cx, .h_st_timefix_apply(d[[".time"]], .map), d[[".event"]], X,}",
      r"{                         drop(X %*% .beta), pr$s)}",
      r"{  .et_put(st$cx$et)}",
      r"{  st$cx$et <- NULL}",
      r"{  st}",
      r"{}, order = FALSE)$state}",
      ".et_put(.h_st_et_close(.r$cx))",
      ".pg_tick()",
      # the view of the cumulative lam and b_j (S20), its columns prefixed .sr_et_
      ".j <- seq_len(.p)",
      r"{.vsql <- paste0("CREATE TEMP VIEW sr_et_cum AS SELECT stratum, time, ",}",
      r"{  paste(c("SUM(dlam) OVER w AS \".sr_et_lam\"", "c1 AS \".sr_et_c1\"",}",
      r"{          sprintf("xbm_%d AS \".sr_et_xbm_%d\"", .j, .j),}",
      r"{          sprintf("SUM(db_%d) OVER w AS \".sr_et_b_%d\"", .j, .j),}",
      r"{          sprintf("c2_%d AS \".sr_et_c2_%d\"", .j, .j)), collapse = ", "),}",
      r"{  " FROM sr_et WINDOW w AS (PARTITION BY stratum ORDER BY time ROWS UNBOUNDED PRECEDING)")}",
      ".nv <- DBI::dbExecute(.sr_con, .vsql)",
      r"{.etc <- c(".sr_et_lam", ".sr_et_c1", paste0(".sr_et_", rep(c("xbm_", "b_", "c2_"), each = .p), .j))}",
      # the cluster pass (S22): the S10 rows with their event-time terms, sorted by the cluster
      sprintf(".sr_csql <- paste0(%s, %s, %s)",
              .sr_lit(paste0("SELECT ", paste(c(paste0("s.", q(needed)), "s.\".sr_stratum\"", "s.\".sr_row\"",
                                                 "et.* EXCLUDE (stratum, time)"), collapse = ", "), " FROM (")),
              .sr_cox_sql(spec, needed, order = FALSE),
              .sr_lit(paste0(") s ASOF LEFT JOIN sr_et_cum et ON s.\".sr_stratum\" = et.stratum AND s.",
                             q(spec[["time"]]), " >= et.time ORDER BY s.", q(cl), ", s.\".sr_row\""))),
      ".S <- new.env(parent = emptyenv())",
      ".S$cl <- NULL",
      ".S$cn <- 0",
      r"{.h_st_pass(.sr_con, .sr_csql, .sr_chunk, NULL, function(st, df) {}",
      r"{  pr <- .prep(df)}",
      r"{  d <- pr$des}",
      r"{  if (!nrow(d)) return(st)}",
      r"{  X <- .X(d)}",
      r"{  X <- X - rep(.mu, each = nrow(X))}",
      r"{  et <- df[pr$keep, .etc, drop = FALSE]}",
      r"{  names(et) <- sub("^[.]sr_et_", "", names(et))}",
      r"{  res <- .h_st_cox_resid(X, exp(drop(X %*% .beta)), d[[".event"]], et)}",
      r"{  .S$cl <- .h_st_cluster_step(.S$cl, res %*% .fit$V, 1, d[[".cl"]])}",
      r"{  .S$cn <- .S$cn + nrow(d)}",
      r"{  st}",
      r"{}, order = FALSE)}",
      ".pg_tick()",
      ".S$cl <- .h_st_cluster_flush(.S$cl)",
      r"{if (.S$cn != .A$n) stop("the cluster pass read ", .h_st_count(.S$cn), " rows, the fit used ", .n)}",
      ".ncl <- .S$cl$G",
      r"{if (.ncl < 2L) stop("layers$clustered needs at least two clusters (found ", .ncl, ")")}",
      ".Vr <- .S$cl$S",
      "dimnames(.Vr) <- dimnames(.fit$V)",
      ".V <- .Vr[.nm, .nm, drop = FALSE]"
    )
  }
  tabc <- "tab <- .h_coef_table(.cf[.ok], .V, df = Inf)"

  km <- c(
    if (fac) ".kml <- .sr_fixed$levels[[\"t1\"]]" else ".kml <- if (.A$km_many) \"All\" else levels(factor(.A$kmv))",
    ".kmo <- .h_st_km_finish(.h_st_km_counts(.A$km, .map), .kml, c(.A$tlo, .A$thi))",
    sprintf(".kmb <- %s", if (fac) "\"exposure\"" else "if (.A$km_many) \"overall\" else \"exposure\"")
  )

  value <- paste0(
    "list(coef = tab, aliased = names(.cf)[!.ok], n = .n, n_total = .h_st_count(.A$n_total), ",
    "events = .h_st_count(.A$events), level = 0.95, se_type = ", .sr_lit(if (is.null(cl)) "model" else "cluster"),
    ", n_clusters = .ncl, ties = ",
    .sr_lit(ties), ", strata = ", .sr_lit(st), ", levels = .sr_fixed$levels, warnings = .wrn, ",
    "km_by = .kmb, km = .kmo$km, km_risk = .kmo$risk, engine = \"stream\", chunk = .sr_chunk, ",
    "passes = .pg$k, iter = .fit$iter)")

  paste(c(tc$setup, helpers, tc$design, head, pass0, fit, robust, tabc, km, value), collapse = "\n")
}

# ---- RMST (P1e.2 Task 7, rulings S13, S14, S15, S17 and S18) --------------------------------------------------
#
# .sr_rmst_compile() returns .sr_rmst_compile_stream(spec) when spec$engine is "stream". One counting pass
# (progress max 1) reads the time, status and exposure columns in chunks of stream_chunk rows, runs the R8
# snippets on each (.h_num / .h_bin of sr_outcome_code, the complete cases of time, status and exposure) and
# keeps: n, n_total, events, the raw time range, the exact distinct exposure values (S14, so the stop text
# of more than 8 values is the memory one) and the Kaplan-Meier counts keyed by (exposure as text, raw time)
# (S13; not kept once a ninth value shows, because the run then stops). Afterwards the stops of the memory
# code run in its order, each group's RMST uses the near-tie map of that group's own raw times (survfit on
# one group), and the KM figure data use the map of all times. n, events, at_risk and max_time are raw.

#' The stream body of rmst@1: one code string.
.sr_rmst_compile_stream <- function(spec) {
  ch <- spec[["stream_chunk"]]
  if (is.null(ch)) stop("a stream spec needs stream_chunk (set by the run)", call. = FALSE)
  q <- function(x) as.character(DBI::dbQuoteIdentifier(DBI::ANSI(), x))
  cols <- unique(c(spec[["time"]], spec[["status"]], spec[["exposure"]]))
  ref <- spec[["exposure_ref"]]
  has_ref <- length(ref) == 1L && !is.na(ref) && nzchar(as.character(ref))
  sql <- sprintf("paste0(%s, sr_src)", .sr_lit(paste0("SELECT ", paste(q(cols), collapse = ", "), " FROM ")))
  helpers <- c(
    .sr_inline(.h_levels, ".h_levels"),
    .sr_inline(.h_st_pass, ".h_st_pass"),
    .sr_inline(.h_st_progress, ".h_st_progress"),
    .sr_inline(.h_st_count, ".h_st_count"),
    .sr_inline(.h_st_timefix_map, ".h_st_timefix_map"),
    .sr_inline(.h_st_timefix_apply, ".h_st_timefix_apply"),
    .sr_inline(.h_st_km_add, ".h_st_km_add"),
    .sr_inline(.h_st_km_counts, ".h_st_km_counts"),
    .sr_inline(.h_st_km_finish, ".h_st_km_finish"),
    .sr_inline(.h_st_rmst, ".h_st_rmst"))
  code <- c(
    sprintf(".miss <- setdiff(%s, names(DBI::dbGetQuery(.sr_con, paste(\"SELECT * FROM\", sr_src, \"LIMIT 0\"))))",
            .sr_lit(cols)),
    "if (length(.miss)) stop(\"column(s) not in the data: \", paste(.miss, collapse = \", \"))",
    helpers,
    sprintf(".sr_chunk <- %s", .sr_lit(as.integer(ch))),
    sprintf(".sr_sql <- %s", sql),
    sprintf(".tau <- %s", .sr_lit(spec[["tau"]])),
    ".prep <- function(df) {",
    "  sr_data <- df",
    sprintf("  des <- as.data.frame(sr_data)[%s]", .sr_lit(cols)),
    sr_outcome_code(spec, "surv"),
    sprintf("  des[[\".ex\"]] <- des[[%s]]", .sr_lit(spec[["exposure"]])),
    "  .keep <- stats::complete.cases(des[c(\".time\", \".event\", \".ex\")])",
    "  list(des = des[.keep, , drop = FALSE], n = nrow(df))",
    "}",
    ".pg <- new.env(parent = emptyenv())",
    ".pg$k <- 0L",
    ".pg$m <- 1L",
    ".pg_tick <- function() {",
    "  .pg$k <- .pg$k + 1L",
    "  if (exists(\"sr_progress_file\")) .h_st_progress(sr_progress_file, .pg$k, .pg$m)",
    "  invisible(NULL)",
    "}",
    ".A <- new.env(parent = emptyenv())",
    ".A$n_total <- 0",
    ".A$n <- 0",
    ".A$events <- 0",
    ".A$tlo <- Inf",
    ".A$thi <- -Inf",
    ".A$km <- NULL",
    ".A$ex <- NULL",
    ".A$many <- FALSE",
    ".h_st_pass(.sr_con, .sr_sql, .sr_chunk, NULL, function(g, df) {",
    "  pr <- .prep(df)",
    "  .A$n_total <- .A$n_total + pr$n",
    "  d <- pr$des",
    "  if (!nrow(d)) return(g)",
    "  .A$n <- .A$n + nrow(d)",
    "  .A$events <- .A$events + sum(d[[\".event\"]])",
    "  .A$tlo <- min(.A$tlo, d[[\".time\"]])",
    "  .A$thi <- max(.A$thi, d[[\".time\"]])",
    "  .A$ex <- unique(c(.A$ex, unique(d[[\".ex\"]])))",
    "  if (!.A$many) {",
    "    if (length(.A$ex) > 8L) {",
    "      .A$many <- TRUE",
    "      .A$km <- NULL",
    "    } else {",
    "      .A$km <- .h_st_km_add(.A$km, d[[\".time\"]], d[[\".event\"]], as.character(d[[\".ex\"]]))",
    "    }",
    "  }",
    "  g",
    "}, order = FALSE)",
    ".pg_tick()",
    "if (.A$n < 1) stop(\"no complete rows: every row has a missing value in the model\")",
    ".n <- .h_st_count(.A$n)",
    "if (.A$events == 0) {",
    "  stop(\"the outcome has no events among the \", .n, \" rows analysed\")",
    "}",
    sprintf(".exn <- %s", .sr_lit(spec[["exposure"]])),
    ".nlv <- length(.A$ex)",
    "if (.nlv > 8L) {",
    "  stop(\"RMST compares groups: exposure '\", .exn, \"' has \", .nlv, \" distinct values; categorise it first\")",
    "}",
    sprintf(".lv <- .h_levels(.A$ex, %s)", if (has_ref) .sr_lit(as.character(ref)) else "NULL"),
    ".raw <- .h_st_km_counts(.A$km, NULL)",
    ".mx <- vapply(.lv, function(l) max(.raw$t[.raw$g == l]), numeric(1))",
    ".far <- which(.mx < .tau)",
    "if (length(.far)) {",
    paste0("  stop(\"tau = \", .tau, \" exceeds the follow-up of group '\", .lv[.far[1L]], \"' (max time \", ",
           ".mx[.far[1L]], \")\")"),
    "}",
    ".rows <- lapply(.lv, function(l) {",
    "  cg <- .raw[.raw$g == l, , drop = FALSE]",
    "  m <- cg$d + cg$c",
    # the group's own near-tie map (S13), as survfit on this group alone
    "  cm <- .h_st_km_counts(list(parts = list(cg), n = 1L), .h_st_timefix_map(cg$t))",
    "  r <- .h_st_rmst(cm, .tau)",
    paste0("  data.frame(level = l, rmst = r[[\"rmst\"]], se = r[[\"se\"]], n = .h_st_count(sum(m)), ",
           "events = .h_st_count(sum(cg$d)), at_risk = .h_st_count(sum(m[cg$t >= .tau])), ",
           "max_time = max(cg$t), stringsAsFactors = FALSE)"),
    "})",
    "groups <- do.call(rbind, .rows)",
    ".map <- .h_st_timefix_map(.raw$t)",
    ".kmo <- .h_st_km_finish(.h_st_km_counts(.A$km, .map), .lv, c(.A$tlo, .A$thi))",
    paste0("list(groups = groups, tau = .tau, n = .n, n_total = .h_st_count(.A$n_total), ",
           "events = .h_st_count(.A$events), level = 0.95, km = .kmo$km, km_risk = .kmo$risk, ",
           "engine = \"stream\", chunk = .sr_chunk, passes = .pg$k)"))
  paste(code, collapse = "\n")
}
