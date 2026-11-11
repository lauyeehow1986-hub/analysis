# scale/table1_sql.R - the exact SQL Table 1 of the streaming engine (P1e.1, spec section 6.7, ruling R14).
#
# .h_t1_sql_main(con, src, by, vars, nonnormal) returns the value list of routes/table1.R's .h_t1_main()
# (the same names, kinds, types and element order) from SQL aggregates over the Parquet extract `src`
# (the read_parquet() expression of the run), so .sr_t1_present() gives identical strings. The SMDs are
# computed in R by the inlined .h_t1_smd_* helpers from the aggregates. The helper is emitted with
# .sr_inline(): it is ASCII and calls only base, stats, DBI, duckdb and the other .h_t1_* helpers.
#
#   * rows with a missing `by` (NULL or NaN) are dropped; the group values are the distinct `by`
#     values sorted with sort(method = "radix") (the in-memory ordering code); a registered two-column
#     table (sr_t1_g: value, index) joins each row to its group by the typed value;
#   * the column kind comes from DESCRIBE: a numeric DuckDB type with more than two (or no) distinct
#     non-missing values among the kept rows is continuous, every other column is categorical; a DATE,
#     TIME or TIMESTAMP* column is refused, as is a categorical column of more than 50 levels;
#   * continuous, per group and overall: n, the missing count (NULL or NaN), the mean from a compensated
#     (Kahan) sum, the SD from a second pass sum((x - mean)^2) / (n - 1) with the mean as a literal, and
#     the quartiles from quantile_cont (R's type 7);
#   * categorical: the level counts by (group, value) mapped on the in-memory levels; a NaN is neither a
#     level nor a missing value there (as.character(NaN) is "NaN"), and it is neither here.
#   * the mean is the correctly rounded quotient of an exact sum (Shewchuk partials accumulated in R over
#     the streamed values, so independent of order and threads), which is R's mean() to the bit; the SD
#     second pass uses that mean as a literal. A column with an infinite value has mean +-Inf (or NaN)
#     and SD NaN, as in memory, with no SQL pass.
# The compile passes one DuckDB thread (R14), so the summation order of the SQL sums is fixed.
# Known difference: a factor/ENUM column would have its levels sorted by radix here, not kept in the
# declared factor order of memory (unreachable today: ingest converts factors to character).

.h_t1_sql_main <- function(con, src, by, vars, nonnormal) {
  qid <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  qry <- function(sql) DBI::dbGetQuery(con, sql)
  lit <- function(x) sprintf("%.17g", x)
  # Shewchuk's grow-expansion: the exact sum of the partials p and x, as non-overlapping partials
  grow <- function(p, x) {
    out <- numeric(0)
    for (y in p) {
      if (abs(x) < abs(y)) {
        tmp <- x
        x <- y
        y <- tmp
      }
      hi <- x + y
      lo <- y - (hi - x)
      if (lo != 0) out <- c(out, lo)
      x <- hi
    }
    c(out, x)
  }
  # The correctly rounded mean of exact partials p over n values: m0 = sum / n, then the exact
  # residual sum(p) - m0 * n (Dekker's product) refines it once.
  exmean <- function(p, n) {
    m0 <- sum(p) / n
    c1 <- 134217729 * m0
    mh <- c1 - (c1 - m0)
    ml <- m0 - mh
    c2 <- 134217729 * n
    nh <- c2 - (c2 - n)
    nl <- n - nh
    ph <- m0 * n
    pl <- ((mh * nh - ph) + mh * nl + ml * nh) + ml * nl
    r <- grow(grow(p, -ph), -pl)
    m0 + sum(r) / n
  }
  ds <- qry(paste("DESCRIBE SELECT * FROM", src))
  ty <- toupper(as.character(ds$column_type))
  names(ty) <- ds$column_name
  miss <- setdiff(c(by, vars), names(ty))
  if (length(miss)) stop("column(s) not in the data: ", paste(miss, collapse = ", "))
  # the rows where a column is present (NULL, and NaN for a float column, are missing)
  ok <- function(id, t) {
    if (grepl("^(FLOAT|DOUBLE|REAL)", t)) paste0(id, " IS NOT NULL AND NOT isnan(", id, ")")
    else paste0(id, " IS NOT NULL")
  }
  bq <- qid(by)
  gv <- qry(paste0("SELECT DISTINCT ", bq, " AS v FROM ", src, " WHERE ", ok(bq, ty[[by]])))$v
  gs <- sort(unique(gv), method = "radix")
  lv <- as.character(gs)
  if (length(lv) < 2L) {
    stop("the grouping column needs at least two observed groups (found ", length(lv), ")")
  }
  ng <- length(lv)
  duckdb::duckdb_register(con, "sr_t1_g", data.frame(gv = gs, gi = seq_len(ng)), overwrite = TRUE)
  on.exit(try(duckdb::duckdb_unregister(con, "sr_t1_g"), silent = TRUE), add = TRUE)
  base <- function(sel) {
    paste0("(SELECT m.gi AS gi", sel, " FROM ", src, " s JOIN sr_t1_g m ON s.", bq, " = m.gv)")
  }
  total <- qry(paste("SELECT count(*) AS n FROM", src))$n
  gn <- qry(paste0("SELECT gi, count(*) AS n FROM ", base(""), " GROUP BY gi"))
  nvec <- numeric(ng)
  nvec[gn$gi] <- as.numeric(gn$n)
  nall <- c(nvec, sum(nvec))
  out <- vector("list", length(vars))
  for (i in seq_along(vars)) {
    v <- vars[i]
    t <- ty[[v]]
    if (grepl("^(DATE|TIME)", t)) {
      stop("column '", v, "' is a date or time; derive a numeric variable first")
    }
    xq <- qid(v)
    bx <- base(paste0(", s.", xq, " AS x"))
    good <- ok("x", t)
    isnum <- grepl("^(TINYINT|SMALLINT|INTEGER|BIGINT|HUGEINT|UTINYINT|USMALLINT|UINTEGER|UBIGINT|UHUGEINT|FLOAT|DOUBLE|DECIMAL)", t)
    nd <- if (isnum) {
      as.numeric(qry(paste0("SELECT count(DISTINCT x) AS n FROM ", bx, " WHERE ", good))$n)
    } else NA_real_
    if (isnum && (nd > 2 || nd == 0)) {
      xd <- paste0("SELECT gi, CAST(x AS DOUBLE) AS x FROM ", bx, " WHERE ", good)
      rows <- qry(paste0("SELECT gi, count(*) AS n FROM ", bx, " GROUP BY gi"))
      rv <- numeric(ng)
      rv[rows$gi] <- as.numeric(rows$n)
      stat <- function(by_group) {
        sel <- paste0("count(x) AS n, quantile_cont(x, 0.25) AS q1, ",
                      "quantile_cont(x, 0.5) AS q2, quantile_cont(x, 0.75) AS q3")
        if (by_group) qry(paste0("SELECT gi, ", sel, " FROM (", xd, ") GROUP BY gi"))
        else qry(paste0("SELECT ", sel, " FROM (", xd, ")"))
      }
      sg <- stat(TRUE)
      sa <- stat(FALSE)
      pos <- match(seq_len(ng), sg$gi)
      n_k <- c(as.numeric(sg$n[pos]), as.numeric(sa$n))
      n_k[is.na(n_k)] <- 0
      # the mean: an exact sum over the streamed values (Shewchuk partials, order independent), then
      # a correctly rounded division; this is R's mean() (long double sum, refined) to the bit
      acc <- lapply(seq_len(ng), function(k) list(p = numeric(0), nf = 0, nfn = 0))
      res <- DBI::dbSendQuery(con, xd)
      repeat {
        df <- DBI::dbFetch(res, n = 100000L)
        if (!nrow(df)) break
        for (k in unique(df$gi)) {
          z <- df$x[df$gi == k]
          fin <- is.finite(z)
          if (!all(fin)) {
            acc[[k]]$nf <- acc[[k]]$nf + sum(z[!fin])
            acc[[k]]$nfn <- acc[[k]]$nfn + sum(!fin)
            z <- z[fin]
          }
          p <- acc[[k]]$p
          for (v2 in z) p <- grow(p, v2)
          acc[[k]]$p <- p
        }
      }
      DBI::dbClearResult(res)
      pa <- numeric(0)
      nfa <- 0
      nfna <- 0
      for (k in seq_len(ng)) {
        for (v2 in acc[[k]]$p) pa <- grow(pa, v2)
        nfa <- nfa + acc[[k]]$nf
        nfna <- nfna + acc[[k]]$nfn
      }
      acc[[ng + 1L]] <- list(p = pa, nf = nfa, nfn = nfna)
      m_k <- vapply(seq_len(ng + 1L), function(k) {
        if (n_k[k] == 0) return(NA_real_)
        if (acc[[k]]$nfn > 0) return(acc[[k]]$nf / n_k[k])
        exmean(acc[[k]]$p, n_k[k])
      }, numeric(1L))
      q_k <- rbind(c(as.numeric(sg$q1[pos]), as.numeric(sa$q1)),
                   c(as.numeric(sg$q2[pos]), as.numeric(sa$q2)),
                   c(as.numeric(sg$q3[pos]), as.numeric(sa$q3)))
      ss_k <- rep(NA_real_, ng + 1L)
      # a non-finite mean (an infinite value) gives a NaN SD, as sd() does; no SQL pass for it
      bad <- which(n_k > 1 & !is.finite(m_k))
      ss_k[bad] <- NaN
      mg <- which(n_k[seq_len(ng)] > 1 & is.finite(m_k[seq_len(ng)]))
      if (length(mg)) {
        cs <- paste0("CASE gi ", paste0("WHEN ", mg, " THEN ", lit(m_k[mg]), collapse = " "), " END")
        r <- qry(paste0("SELECT gi, fsum((x - ", cs, ") * (x - ", cs, ")) AS ss FROM (", xd,
                        ") WHERE gi IN (", paste(mg, collapse = ", "), ") GROUP BY gi"))
        ss_k[r$gi] <- as.numeric(r$ss)
      }
      if (n_k[ng + 1L] > 1 && is.finite(m_k[ng + 1L])) {
        r <- qry(paste0("SELECT fsum((x - ", lit(m_k[ng + 1L]), ") * (x - ", lit(m_k[ng + 1L]),
                        ")) AS ss FROM (", xd, ")"))
        ss_k[ng + 1L] <- as.numeric(r$ss)
      }
      nm_k <- c(rv, sum(rv)) - n_k
      st <- vapply(seq_len(ng + 1L), function(k) {
        n <- n_k[k]
        if (n == 0) return(c(0, nm_k[k], rep(NA_real_, 5L)))
        c(n, nm_k[k], m_k[k], if (n > 1) sqrt(ss_k[k] / (n - 1)) else NA_real_,
          q_k[2L, k], q_k[1L, k], q_k[3L, k])
      }, numeric(7L))
      sm <- vapply(2:ng, function(k) {
        .h_t1_smd_cont(st[3L, k], st[4L, k], st[1L, k], st[3L, 1L], st[4L, 1L], st[1L, 1L])
      }, numeric(3L))
      out[[i]] <- list(var = v, kind = "continuous", nonnormal = v %in% nonnormal,
                       n = st[1L, ], n_miss = st[2L, ], mean = st[3L, ], sd = st[4L, ],
                       median = st[5L, ], q1 = st[6L, ], q3 = st[7L, ],
                       smd = sm[1L, ], smd_lo = sm[2L, ], smd_hi = sm[3L, ],
                       smd_method = "continuous")
    } else {
      if (!isnum) {
        nlv <- as.integer(qry(paste0("SELECT count(DISTINCT x) AS n FROM ", bx))$n)
        if (nlv > 50L) {
          stop("column '", v, "' has ", nlv, " distinct values; a categorical variable ",
               "may have at most 50 levels")
        }
      }
      vals <- qry(paste0("SELECT DISTINCT x FROM ", bx, " WHERE ", good))$x
      lvl <- as.character(sort(unique(vals), method = "radix"))
      tb <- qry(paste0("SELECT gi, x, count(*) AS n FROM ", bx, " WHERE x IS NOT NULL GROUP BY gi, x"))
      xs <- as.character(tb$x)
      cnt <- matrix(0, nrow = length(lvl), ncol = ng + 1L)
      for (k in seq_len(ng)) {
        for (j in seq_along(lvl)) cnt[j, k] <- sum(as.numeric(tb$n[tb$gi == k & xs == lvl[j]]))
      }
      cnt[, ng + 1L] <- rowSums(cnt[, seq_len(ng), drop = FALSE])
      nn <- qry(paste0("SELECT gi, count(*) AS n FROM ", bx, " WHERE x IS NULL GROUP BY gi"))
      nmv <- numeric(ng)
      nmv[nn$gi] <- as.numeric(nn$n)
      nmiss <- c(nmv, sum(nmv))
      method <- if (length(lvl) == 0L) "none" else if (length(lvl) == 2L) "binary" else "yang_dalton"
      sm <- vapply(2:ng, function(k) {
        if (method == "none") return(rep(NA_real_, 3L))
        if (method == "binary") return(.h_t1_smd_bin(cnt[, k], cnt[, 1L]))
        .h_t1_smd_multi(cnt[, k], cnt[, 1L])
      }, numeric(3L))
      out[[i]] <- list(var = v, kind = "categorical", levels = lvl, count = cnt,
                       n = colSums(cnt), n_miss = nmiss,
                       smd = sm[1L, ], smd_lo = sm[2L, ], smd_hi = sm[3L, ], smd_method = method,
                       pos_level = if (length(lvl) == 2L) lvl[2L] else NA_character_)
    }
  }
  list(by = by, groups = lv, n = nall, n_excluded = as.integer(total - sum(nvec)), vars = out)
}

#' The stream body of table1@1 (spec$engine = "stream"). It runs after
#' .sr_run_prologue_stream(ex, threads = .sr_stream_threads("table1")), whose `.sr_con` is opened with the
#' literal 1L threads (R14: one thread fixes the summation order and no SET follows the lockdown), and
#' returns the value of .h_t1_sql_main().
.sr_t1_compile_stream <- function(spec) {
  a <- .sr_t1_spec_args(spec)
  helpers <- c(
    .sr_inline(.h_t1_ci, ".h_t1_ci"), .sr_inline(.h_t1_smd_cont, ".h_t1_smd_cont"),
    .sr_inline(.h_t1_smd_bin, ".h_t1_smd_bin"), .sr_inline(.h_t1_pinv, ".h_t1_pinv"),
    .sr_inline(.h_t1_smd_multi, ".h_t1_smd_multi"), .sr_inline(.h_t1_sql_main, ".h_t1_sql_main")
  )
  paste(c(
    helpers,
    sprintf(".h_t1_sql_main(.sr_con, sr_src, %s, %s, %s)", .sr_lit(a$by), .sr_lit(a$vars), .sr_lit(a$nn))
  ), collapse = "\n")
}
