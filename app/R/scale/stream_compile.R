# scale/stream_compile.R - the stream compile branches of lm@1, logistic@1 and modpois@1 (P1e.1, spec
# sections 6.4 to 6.6, rulings R3, R8, R11 and R15).
#
# .sr_lm_compile() and .sr_glm_compile_fn() return .sr_lm_compile_stream(spec) /
# .sr_glm_compile_stream(spec, kind) when spec$engine is "stream"; the memory code is untouched (its text
# stays byte-identical). The emitted body runs after .sr_run_prologue_stream() (which defines .sr_con and
# sr_src) and returns a value list with exactly the names of the in-memory value (lm: fig = NULL, ruling
# R3), plus engine = "stream", chunk, passes and iter (NA for lm).
#
# The body reads `SELECT <cols>, ".sr_row" FROM <sr_src> WHERE (<.sr_where>)` (plus ORDER BY ".sr_row" when
# .sr_where is the hash anti-join of a large exclude_rows, .h_st_order_by) in chunks of stream_chunk
# rows (.h_st_pass, order = TRUE). Each chunk is bound as `sr_data` inside the step function and goes
# through the in-memory snippets (R8): .h_design, the outcome coding and the complete-case filter, so a
# row is fitted on the stream exactly when it is fitted in memory. The passes:
#   1  (lm) the TSQR of [X | y] with the moments of y; (glm) the first IRLS pass at mustart. Both count
#      the rows, the events (glm), the observed risk (glm, R9), the fitted rows of every factor level
#      and, for a numeric exposure, its first distinct values.
#   2+ (glm) the other IRLS passes (.h_st_irls), at most 25.
#   SE (robust) one pass in file order: the HC3 (lm) or HC0 (glm, the scores of R11) meat; the marginal
#      sums are merged into it. (cluster) one pass sorted by cluster then .sr_row (order = FALSE).
#   marginal one pass when not merged (model or cluster SE).
# Progress {"pass": k, "max": m} goes to sr_progress_file when it exists (R15); m is the fit passes (lm 1,
# glm 26) plus the SE pass and the separate marginal pass, raised when an extra pass runs.
#
# The columns of a factor level absent from the fitted rows are dropped before the fit, as lm and glm do
# (model.frame drop.unused.levels): .h_st_mm_cols gives the in-memory column names from one fitted row
# and the levels seen. The marginal block stops with model.frame's own "factor <col> has new level(s)"
# text where the in-memory .h_gcomp would (.h_st_newlev), and a non-binary numeric exposure gives the
# in-memory "N distinct values" stop or note.

#' A count as an integer (as nrow() gives it in memory) while it fits, else the double.
.h_st_count <- function(x) {
  if (is.finite(x) && x <= .Machine$integer.max) as.integer(x) else x
}

#' The total sum of squares of the streamed lm (ruling R3): the centred sum (the moments' m2) for a model
#' with an intercept, sum(y^2) (ss) without one, as summary.lm.
.h_st_sst <- function(mom, ss, intercept) {
  if (isTRUE(intercept)) mom$m2 else ss
}

#' The design column names the in-memory fit has: model.matrix of the right-hand side `rhs` on one fitted
#' row `proto`, with each factor column of `present` (a list column -> levels seen among the fitted rows)
#' reduced to those levels. A factor left with one level stops with model.matrix's contrasts error, as lm.
.h_st_mm_cols <- function(rhs, proto, present) {
  tt <- stats::delete.response(stats::terms(stats::as.formula(paste("~", rhs))))
  d <- proto[1L, , drop = FALSE]
  for (nm in names(present)) {
    lv <- present[[nm]]
    d[[nm]] <- factor(lv[1L], levels = lv)
  }
  colnames(stats::model.matrix(tt, stats::model.frame(tt, d)))
}

#' model.frame's check of new factor levels (the xlev check .h_gcomp meets in memory): nxl the levels of
#' column nm in the rows predicted on, xl the levels of the fit. Stops with the same text.
.h_st_newlev <- function(nm, nxl, xl) {
  m <- is.na(match(nxl, xl))
  if (any(m)) {
    stop(sprintf(if (length(m) == 1L) "factor %s has new level %s" else "factor %s has new levels %s",
                 nm, paste(nxl[m], collapse = ", ")))
  }
  invisible(NULL)
}

#' The stream body of lm@1. merge = FALSE puts the marginal sums in their own pass (a test checks that
#' the merged pass gives the same value).
.sr_lm_compile_stream <- function(spec, merge = TRUE) .sr_stream_compile(spec, "lm", merge)

#' The stream body of logistic@1 or modpois@1 (kind).
.sr_glm_compile_stream <- function(spec, kind, merge = TRUE) .sr_stream_compile(spec, kind, merge)

# The shared generator. kind is "lm", "logistic" or "modpois".
.sr_stream_compile <- function(spec, kind, merge = TRUE) {
  ch <- spec[["stream_chunk"]]
  if (is.null(ch)) stop("a stream spec needs stream_chunk (set by the run)", call. = FALSE)
  is_lm <- identical(kind, "lm")
  se <- if (is_lm) .sr_lm_se(spec) else .sr_glm_se(spec, kind)
  fam <- if (is_lm) "gaussian" else if (identical(kind, "modpois")) "poisson" else "binomial"
  tc <- sr_terms_code(spec, "stream")
  plan <- .sr_term_plan(spec)
  cl <- if (identical(se, "cluster")) spec[["cluster"]] else NULL
  rhs_vars <- all.vars(stats::as.formula(paste("~", tc$rhs)))
  used <- c(".y", rhs_vars, cl)
  fac <- identical(plan[[1L]]$type, "factor")
  exc <- if (fac) "t1f" else "t1"
  ids <- vapply(plan, function(i) if (identical(i$type, "factor")) i$id else NA_character_, character(1))
  ids <- ids[!is.na(ids)]
  fcols <- if (length(ids)) paste0(ids, "f") else character()
  covs <- setdiff(fcols, "t1f")
  marg_on <- if (is_lm) isTRUE(spec[["marginal"]]) else .sr_glm_marg_on(spec)
  strict <- isTRUE(spec[["marginal"]])
  all_rows <- marg_on && identical(.sr_marg_over(spec), "all")
  merged <- marg_on && identical(se, "robust") && isTRUE(merge)
  needed <- unique(c(.sr_spec_chr(spec[["outcome"]]), .sr_spec_chr(spec[["cluster"]]),
                     vapply(plan, function(i) i$var, character(1))))
  rhs <- tc$rhs
  m_max <- (if (is_lm) 1L else 26L) + as.integer(se != "model") + as.integer(marg_on && !merged)

  # the design rows of a chunk and the score / residual of its kept columns
  resid <- if (is_lm) {
    "y - drop(Xk %*% .b)"
  } else {
    sprintf(".h_st_glm_scores(Xk, y, drop(Xk %%*%% .b), if (is.null(.fit$beta_prev)) NULL else drop(X %%*%% .fit$beta_prev), %s)",
            .sr_lit(fam))
  }

  helpers <- c(
    .sr_inline(.sr_h_coef_table, ".h_coef_table"),
    .sr_inline(.h_st_pass, ".h_st_pass"),
    .sr_inline(.h_st_progress, ".h_st_progress"),
    .sr_inline(.h_st_count, ".h_st_count"),
    .sr_inline(.h_st_mm_cols, ".h_st_mm_cols"),
    .sr_inline(.h_st_tsqr, ".h_st_tsqr"),
    .sr_inline(.h_st_alias, ".h_st_alias"),
    if (is_lm) c(.sr_inline(.h_st_moments_add, ".h_st_moments_add"), .sr_inline(.h_st_sst, ".h_st_sst"),
                 .sr_inline(.h_st_lm_finish, ".h_st_lm_finish")),
    if (!is_lm) c(.sr_inline(.h_st_fam, ".h_st_fam"), .sr_inline(.h_st_mustart, ".h_st_mustart"),
                  .sr_inline(.h_st_linkfun, ".h_st_linkfun"), .sr_inline(.h_st_glm_eval, ".h_st_glm_eval"),
                  .sr_inline(.h_st_irls, ".h_st_irls"), .sr_inline(.h_st_glm_scores, ".h_st_glm_scores"),
                  .sr_inline(.sr_h_wilson, ".h_wilson"), .sr_inline(.h_st_obs_add, ".h_st_obs_add"),
                  .sr_inline(.h_st_obs_finish, ".h_st_obs_finish")),
    if (!identical(se, "model")) .sr_inline(.h_st_meat_add, ".h_st_meat_add"),
    if (identical(se, "cluster")) c(.sr_inline(.h_st_cluster_step, ".h_st_cluster_step"),
                                    .sr_inline(.h_st_cluster_flush, ".h_st_cluster_flush")),
    .sr_inline(.h_st_vcov_finish, ".h_st_vcov_finish"),
    if (marg_on) c(.sr_inline(.h_gcomp_finish, ".h_gcomp_finish"), .sr_inline(.h_st_marg_rows, ".h_st_marg_rows"),
                   .sr_inline(.h_st_marg_add, ".h_st_marg_add"), .sr_inline(.h_st_marg_finish, ".h_st_marg_finish"),
                   .sr_inline(.h_st_newlev, ".h_st_newlev"))
  )

  head <- c(
    sprintf(".sr_chunk <- %s", .sr_lit(as.integer(ch))),
    sprintf(".sr_cols <- %s", .sr_lit(needed)),
    sprintf(".sr_fcols <- %s", .sr_lit(fcols)),
    paste0(".sr_base <- paste0(\"SELECT \", paste(c(as.character(DBI::dbQuoteIdentifier(.sr_con, .sr_cols)), ",
           "\"\\\".sr_row\\\"\"), collapse = \", \"), \" FROM \", sr_src, \" WHERE (\", .sr_where, \")\")"),
    ".sr_sql <- paste0(.sr_base, .h_st_order_by(.sr_where))",
    sprintf(".tt <- stats::delete.response(stats::terms(~ %s))", rhs),
    ".X <- function(d) stats::model.matrix(.tt, stats::model.frame(.tt, d, na.action = stats::na.pass))",
    # the in-memory snippets on one chunk (R8): design, outcome, complete cases (and the rows to
    # standardise over for standardize_over = "all")
    ".prep <- function(df) {",
    "  sr_data <- df",
    "  des <- .h_design(sr_data)",
    sr_outcome_code(spec, if (is_lm) "continuous" else "binary"),
    sprintf("  .keep <- stats::complete.cases(des[%s])", .sr_lit(used)),
    sprintf("  list(des = des[.keep, , drop = FALSE], over = %s, n = nrow(df))",
            if (all_rows) sprintf("des[stats::complete.cases(des[%s]), , drop = FALSE]", .sr_lit(rhs_vars)) else "NULL"),
    "}",
    # progress (R15)
    ".pg <- new.env(parent = emptyenv())",
    ".pg$k <- 0L",
    sprintf(".pg$m <- %s", .sr_lit(m_max)),
    ".pg_tick <- function() {",
    "  .pg$k <- .pg$k + 1L",
    "  if (.pg$k > .pg$m) .pg$m <- .pg$k",
    "  if (exists(\"sr_progress_file\")) .h_st_progress(sr_progress_file, .pg$k, .pg$m)",
    "  invisible(NULL)",
    "}",
    # the first-pass accumulator
    ".A <- new.env(parent = emptyenv())",
    ".A$n_total <- 0",
    ".A$n <- 0",
    ".A$n_over <- 0",
    ".A$events <- 0",
    ".A$cols <- NULL",
    ".A$proto <- NULL",
    ".A$cnt <- list()",
    ".A$ovc <- list()",
    ".A$obs <- NULL",
    ".A$M <- NULL",
    ".A$mom <- NULL",
    ".A$ss <- 0",
    ".A$ev <- numeric()",
    ".A$ev_many <- FALSE",
    "for (.fc in .sr_fcols) {",
    "  .A$cnt[[.fc]] <- 0",
    "  .A$ovc[[.fc]] <- 0",
    "}",
    ".acc <- function(pr) {",
    "  .A$n_total <- .A$n_total + pr$n",
    "  if (!is.null(pr$over)) {",
    "    .A$n_over <- .A$n_over + nrow(pr$over)",
    "    for (.fc in .sr_fcols) .A$ovc[[.fc]] <- .A$ovc[[.fc]] + tabulate(as.integer(pr$over[[.fc]]), nlevels(pr$over[[.fc]]))",
    "  }",
    "  d <- pr$des",
    "  if (!nrow(d)) return(NULL)",
    "  X <- .X(d)",
    "  y <- d[[\".y\"]]",
    "  if (is.null(.A$cols)) {",
    "    .A$cols <- colnames(X)",
    "    .A$proto <- d[1L, , drop = FALSE]",
    "  }",
    "  .A$n <- .A$n + nrow(d)",
    "  for (.fc in .sr_fcols) .A$cnt[[.fc]] <- .A$cnt[[.fc]] + tabulate(as.integer(d[[.fc]]), nlevels(d[[.fc]]))",
    if (!is_lm) c(
      "  .A$events <- .A$events + sum(y)",
      sprintf("  .A$obs <- .h_st_obs_add(.A$obs, d[[%s]], y)", .sr_lit(exc))),
    if (marg_on && !fac) c(
      "  if (!.A$ev_many) {",
      "    .A$ev <- sort(unique(c(.A$ev, d[[\"t1\"]])))",
      "    if (length(.A$ev) > 2L) .A$ev_many <- TRUE",
      "  }"),
    if (is_lm) c(
      "  .A$M <- .h_st_tsqr(.A$M, cbind(X, y))",
      "  .A$mom <- .h_st_moments_add(.A$mom, y)",
      "  .A$ss <- .A$ss + sum(y^2)"),
    "  X",
    "}",
    # after the first pass: the in-memory stops on the counts, then the columns the in-memory fit has
    ".sel_fn <- function() {",
    "  if (.A$n < 1) stop(\"no complete rows: every row has a missing value in the model\")",
    if (!is_lm) c(
      "  if (.A$events == 0 || .A$events == .A$n) {",
      "    stop(\"the outcome has no events or no non-events among the \", .h_st_count(.A$n), \" rows analysed\")",
      "  }"),
    "  .A$pres <- list()",
    "  for (.fc in .sr_fcols) .A$pres[[.fc]] <- levels(.A$proto[[.fc]])[.A$cnt[[.fc]] > 0]",
    sprintf("  .mc <- .h_st_mm_cols(%s, .A$proto, .A$pres)", .sr_lit(rhs)),
    "  .s <- match(.mc, .A$cols)",
    "  if (anyNA(.s)) stop(\"the streamed design does not hold the in-memory columns\")",
    "  .s",
    "}"
  )

  fit <- if (is_lm) {
    c(".h_st_pass(.sr_con, .sr_sql, .sr_chunk, NULL, function(st, df) {",
      "  .acc(.prep(df))",
      "  st",
      "})",
      ".pg_tick()",
      ".sel <- .sel_fn()",
      ".p <- length(.A$cols)",
      ".nms <- .A$cols[.sel]",
      paste0(".fit <- .h_st_lm_finish(.A$M[, c(.sel, .p + 1L), drop = FALSE], length(.sel), .nms, ",
             ".h_st_count(.A$n), .h_st_sst(.A$mom, .A$ss, \"(Intercept)\" %in% .nms))"))
  } else {
    c(".passfn <- function(beta, wls) {",
      "  .h_st_pass(.sr_con, .sr_sql, .sr_chunk, NULL, function(st, df) {",
      "    pr <- .prep(df)",
      "    X <- if (is.null(beta)) .acc(pr) else if (nrow(pr$des)) .X(pr$des)",
      "    if (!is.null(X)) wls(X, pr$des[[\".y\"]])",
      "    st",
      "  })",
      "  .pg_tick()",
      "  invisible(NULL)",
      "}",
      sprintf(".fit <- .h_st_irls(.passfn, %s, NA, NULL, select = function() list(nms = .A$cols, sel = .sel_fn()))",
              .sr_lit(fam)),
      ".sel <- .fit$sel",
      ".p <- length(.A$cols)",
      ".nms <- .A$cols[.sel]")
  }

  # the marginal sums of one chunk (the fitted rows, or the rows to standardise over for "all")
  marg_fns <- if (marg_on) c(
    ".S <- new.env(parent = emptyenv())",
    ".S$mg <- NULL",
    if (fac) ".mg_vals <- .sr_fixed$levels[[\"t1\"]]" else ".mg_vals <- c(1, 0)",
    if (fac) ".mg_go <- TRUE" else ".mg_go <- !.A$ev_many && length(.A$ev) == 2L && all(.A$ev == c(0, 1))",
    ".mg_add <- function(pr) {",
    "  ov <- if (is.null(pr$over)) pr$des else pr$over",
    "  if (!nrow(ov)) return(invisible(NULL))",
    sprintf("  Xa <- .h_st_marg_rows(ov, %s, %s, .mg_vals)", .sr_lit(rhs), .sr_lit(exc)),
    "  Xa <- lapply(Xa, function(x) x[, .sel, drop = FALSE])",
    sprintf("  .S$mg <- .h_st_marg_add(.S$mg, Xa, .fit$beta, .fit$keep, %s)", .sr_lit(fam)),
    "  invisible(NULL)",
    "}"
  ) else c(".S <- new.env(parent = emptyenv())", ".mg_go <- FALSE")

  common <- c(
    ".b <- .fit$beta[.fit$keep]",
    ".kc <- .sel[.fit$keep]",
    ".n <- .h_st_count(.A$n)",
    ".ncl <- NA_real_",
    if (is_lm) ".df <- .fit$df_resid",
    marg_fns
  )

  # the standard errors. The meat argument of the model SE carries the dispersion: sigma^2 for lm, 1 for
  # the binomial and poisson glm (stats::vcov).
  se_code <- switch(se,
    model = sprintf(".V <- .h_st_vcov_finish(\"model\", .fit$B, %s, .n, .fit$rank, NA)",
                    if (is_lm) ".fit$sigma^2" else "1"),
    robust = c(
      ".S$meat <- NULL",
      ".h_st_pass(.sr_con, .sr_sql, .sr_chunk, NULL, function(st, df) {",
      "  pr <- .prep(df)",
      "  d <- pr$des",
      "  if (nrow(d)) {",
      "    X <- .X(d)",
      "    Xk <- X[, .kc, drop = FALSE]",
      "    y <- d[[\".y\"]]",
      if (is_lm) c(
        # HC3: r = e / (1 - h), h the leverage x' (X'X)^-1 x of the kept columns
        "    h <- rowSums((Xk %*% .fit$B) * Xk)",
        sprintf("    .S$meat <- .h_st_meat_add(.S$meat, Xk, (%s) / (1 - h))", resid))
      else sprintf("    .S$meat <- .h_st_meat_add(.S$meat, %s, 1)", resid),
      "  }",
      if (merged) "  if (.mg_go) .mg_add(pr)",
      "  st",
      "})",
      ".pg_tick()",
      ".V <- .h_st_vcov_finish(\"robust\", .fit$B, .S$meat, .n, .fit$rank, NA)"),
    cluster = c(
      ".S$cl <- NULL",
      ".S$cn <- 0",
      sprintf(".sr_csql <- paste0(.sr_base, \" ORDER BY \", DBI::dbQuoteIdentifier(.sr_con, %s), \", \\\".sr_row\\\"\")",
              .sr_lit(cl)),
      ".h_st_pass(.sr_con, .sr_csql, .sr_chunk, NULL, function(st, df) {",
      "  d <- .prep(df)$des",
      "  if (!nrow(d)) return(st)",
      sprintf("  g <- d[[%s]]", .sr_lit(cl)),
      "  if (anyNA(g)) stop(\"the cluster vector must not contain missing values\")",
      "  X <- .X(d)",
      "  Xk <- X[, .kc, drop = FALSE]",
      "  y <- d[[\".y\"]]",
      if (is_lm) sprintf("  .S$cl <- .h_st_cluster_step(.S$cl, Xk, %s, g)", resid)
      else sprintf("  .S$cl <- .h_st_cluster_step(.S$cl, %s, 1, g)", resid),
      "  .S$cn <- .S$cn + nrow(d)",
      "  st",
      "}, order = FALSE)",
      ".pg_tick()",
      ".S$cl <- .h_st_cluster_flush(.S$cl)",
      "if (.S$cn != .A$n) {",
      "  stop(\"the cluster vector must have one value for each fitted row (cluster has \", .h_st_count(.S$cn), \", the fit used \", .n, \")\")",
      "}",
      ".ncl <- .S$cl$G",
      "if (.ncl < 2L) stop(\"se = cluster needs at least two clusters (found \", .ncl, \")\")",
      ".V <- .h_st_vcov_finish(\"cluster\", .fit$B, .S$cl$S, .n, .fit$rank, .ncl)",
      if (is_lm) ".df <- min(.df, .ncl - 1)")
  )

  # the marginal block (R8): the separate pass, the in-memory new-level and binary checks, the rows
  marg_code <- if (marg_on) {
    over_lit <- .sr_lit(.sr_marg_over(spec))
    cov_checks <- if (all_rows && length(covs)) {
      vapply(covs, function(fc) {
        sprintf("  .h_st_newlev(%s, levels(.A$proto[[%s]])[.A$ovc[[%s]] > 0], .A$pres[[%s]])",
                .sr_lit(fc), .sr_lit(fc), .sr_lit(fc), .sr_lit(fc))
      }, character(1), USE.NAMES = FALSE)
    }
    distinct <- if (!fac) c(
      sprintf(".sr_esql <- paste0(.sr_base, \" ORDER BY \", DBI::dbQuoteIdentifier(.sr_con, %s), \", \\\".sr_row\\\"\")",
              .sr_lit(plan[[1L]]$var)),
      ".S$dk <- 0",
      ".S$dl <- NULL",
      ".h_st_pass(.sr_con, .sr_esql, .sr_chunk, NULL, function(st, df) {",
      "  d <- .prep(df)$des",
      "  if (!nrow(d)) return(st)",
      "  u <- unique(d[[\"t1\"]])",
      "  .S$dk <- .S$dk + length(u) - (!is.null(.S$dl) && u[1L] == .S$dl)",
      "  .S$dl <- u[length(u)]",
      "  st",
      "}, order = FALSE)",
      ".pg_tick()")
    c(
      if (!merged) c(
        "if (.mg_go) {",
        "  .h_st_pass(.sr_con, .sr_sql, .sr_chunk, NULL, function(st, df) {",
        "    .mg_add(.prep(df))",
        "    st",
        "  })",
        "  .pg_tick()",
        "}"),
      ".mg <- NULL",
      ".mg_note <- NULL",
      if (fac) c(
        ".lv <- .sr_fixed$levels[[\"t1\"]]",
        "for (.l in .lv[-1L]) {",
        "  .h_st_newlev(\"t1f\", .l, .A$pres[[\"t1f\"]])",
        cov_checks,
        "  .h_st_newlev(\"t1f\", .lv[1L], .A$pres[[\"t1f\"]])",
        "  .g <- .h_st_marg_finish(.S$mg, .V, c(.l, .lv[1L]))",
        "  .mg <- rbind(.mg, data.frame(level = .l, as.data.frame(.g), stringsAsFactors = FALSE))",
        "}")
      else c(
        "if (.mg_go) {",
        cov_checks,
        "  .g <- .h_st_marg_finish(.S$mg, .V, c(1, 0))",
        "  .mg <- data.frame(level = NA_character_, as.data.frame(.g), stringsAsFactors = FALSE)",
        "} else {",
        "  .nev <- length(.A$ev)",
        "  if (.A$ev_many) {",
        paste0("  ", distinct),
        "    .nev <- .h_st_count(.S$dk)",
        "  }",
        if (strict) {
          "  stop(\"marginal effects need a binary (0/1) or categorical exposure; the exposure has \", .nev, \" distinct values\")"
        } else {
          paste0("  .mg_note <- paste0(\"marginal effects were not computed: the exposure is not binary (0/1) ",
                 "or categorical (\", .nev, \" distinct values)\")")
        },
        "}"),
      sprintf(".n_over <- %s", if (all_rows) ".h_st_count(.A$n_over)" else ".n")
    )
  }
  marg_value <- if (marg_on) {
    sprintf("list(rows = .mg, n_over = .n_over, over = %s, note = .mg_note)", .sr_lit(.sr_marg_over(spec)))
  } else "NULL"

  value <- if (is_lm) {
    paste0("list(coef = .h_coef_table(.b, .V, df = .df), aliased = .fit$aliased, n = .n, ",
           "n_total = .h_st_count(.A$n_total), df = .df, df_resid = .fit$df_resid, sigma = .fit$sigma, ",
           "r2 = .fit$r2, adj_r2 = .fit$adj_r2, se_type = ", .sr_lit(se), ", n_clusters = .ncl, level = 0.95, ",
           "marginal = ", marg_value, ", levels = .sr_fixed$levels, fig = NULL, engine = \"stream\", ",
           "chunk = .sr_chunk, passes = .pg$k, iter = NA_integer_)")
  } else {
    c(sprintf(".obs <- .h_st_obs_finish(.A$obs, %s)", if (fac) ".sr_fixed$levels[[\"t1\"]]" else "NULL"),
      paste0("list(coef = .h_coef_table(.b, .V, df = Inf), aliased = .fit$aliased, n = .n, ",
             "n_total = .h_st_count(.A$n_total), events = .h_st_count(.A$events), df = Inf, se_type = ",
             .sr_lit(se), ", n_clusters = .ncl, level = 0.95, levels = .sr_fixed$levels, penalty = \"none\", ",
             "family = ", .sr_lit(fam), ", converged = .fit$converged, near_separation = .fit$near_separation, ",
             "risk_above_one = .fit$risk_above_one, obs = if (is.null(.obs)) data.frame() else .obs, ",
             "marginal = ", marg_value, ", engine = \"stream\", chunk = .sr_chunk, passes = .pg$k, ",
             "iter = .fit$iter)"))
  }

  paste(c(tc$setup, helpers, tc$design, head, fit, common, se_code, marg_code, value), collapse = "\n")
}
