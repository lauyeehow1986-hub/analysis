# Streaming-fit scale test (manual tool; no testthat test):
#   Rscript tools/scale_fit.R [--rows 20000000] [--patients 2000000] [--chunk 500000]
#                             [--routes logistic,lm] [--dir <dir>] [--profile locked16|laptop]
#
# 1. generate  a synthetic Parquet extract of `rows` rows over `patients` cluster ids, written by DuckDB
#              straight to disk (the table is never held in R): id / .sr_row, pid (row i belongs to patient
#              (i mod patients) + 1, so every patient is spread over the whole file and every chunk), a 0/1
#              y, a continuous yc, x1 ~ N(0,1), x2 ~ Gamma-like, a text factor grp with a non-ASCII level,
#              and the exposure sex. Synthetic data only, never real data.
# 2. fit       for each route, the stream run prologue plus the compiled stream route (cluster SE on pid,
#              terms x1 + grp + rcs(x2)), evaluated in this process exactly as a run does.
# 3. report    wall time, passes, the estimate of the exposure and peak RSS per route, and PASS / FAIL.
#
# Peak RSS: this process is blocked inside DuckDB and R while it fits, so a watcher process
# (callr::r_bg, the .sc_watch pattern of tools/scale_test.R) polls ps::ps_memory_info()$rss of this
# process every 0.5 s; the OS peak working set (ps peak_wset, Windows only, since process start, so it
# includes the generate phase) is read at the end as a cross-check, and the larger is used.
# PASS means peak RSS < (caps$duckdb_memory_gb + 2) GB of the hardware profile, as in scale_test.R.
# The scratch folder (default: the session temp dir) is deleted at the end. No network.

.sf_gb <- 1024^3
.sf_say <- function(...) cat(sprintf(...), "\n", sep = "")
.sf_fmt_gb <- function(b) if (length(b) != 1L || is.na(b)) "n/a" else sprintf("%.2f GB", b / .sf_gb)
.sf_arg <- function(args, name, default) {
  i <- match(paste0("--", name), args)
  if (is.na(i) || i == length(args)) return(default)
  args[i + 1L]
}

# Watcher (runs in callr::r_bg): peak RSS of `pid` until `stopfile` exists; writes it to `out`.
.sf_watch <- function(pid, stopfile, out, interval) {
  h <- ps::ps_handle(pid)
  peak <- 0
  repeat {
    r <- tryCatch(as.numeric(ps::ps_memory_info(h)[["rss"]]), error = function(e) NA_real_)
    if (!is.na(r)) peak <- max(peak, r)
    if (file.exists(stopfile) || is.na(r)) break
    Sys.sleep(interval)
  }
  writeLines(sprintf("%.0f", peak), out)
  invisible(NULL)
}

.sf_generate <- function(path, rows, patients, memory_gb, threads) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, sprintf("SET memory_limit = '%dGB'", as.integer(max(1, floor(memory_gb)))))
  DBI::dbExecute(con, sprintf("SET threads = %d", as.integer(threads)))
  DBI::dbExecute(con, "SELECT setseed(0.42)")
  e_acute <- "\u00e9"
  sql <- paste0(
    "COPY (SELECT id, id AS \".sr_row\", pid, x1, x2, grp, sex, ",
    "CAST(random() < 1 / (1 + exp(-(-0.5 + 0.4 * x1 + 0.3 * (sex = 'M')::INTEGER))) AS INTEGER) AS y, ",
    "1 + 0.5 * x1 + 0.3 * (sex = 'M')::INTEGER + sqrt(-2 * ln(1 - random())) * cos(2 * pi() * random()) AS yc ",
    "FROM (SELECT i AS id, (i % ", format(patients, scientific = FALSE), ") + 1 AS pid, ",
    "sqrt(-2 * ln(1 - random())) * cos(2 * pi() * random()) AS x1, ",
    "-ln(1 - random()) - ln(1 - random()) AS x2, ",
    "(['A', 'b', 'C', '", e_acute, "'])[1 + CAST(floor(random() * 4) AS INTEGER)] AS grp, ",
    "CASE WHEN random() < 0.5 THEN 'M' ELSE 'F' END AS sex ",
    "FROM range(1, ", format(rows + 1, scientific = FALSE), ") t(i))) ",
    "TO '", gsub("'", "''", path), "' (FORMAT PARQUET, ROW_GROUP_SIZE 122880)")
  DBI::dbExecute(con, sql)
  invisible(path)
}

.sf_spec <- function(route, chunk) {
  list(purpose = "explain-causal", outcome = if (identical(route, "lm")) "yc" else "y", exposure = "sex",
       exposure_ref = "F", seed = 42L, se = "cluster", cluster = "pid", standardize_over = "analysis",
       terms = list(list(var = "x1"), list(var = "grp", type = "factor"), list(var = "x2", type = "rcs")),
       layers = list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                     framework = "frequentist", clustered = TRUE),
       engine = "stream", stream_chunk = chunk)
}

.sf_route <- function(id) {
  reg <- function(key, make) {
    if (!(.sr_route_key(key, 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(make())
  }
  reg("logistic", .sr_logistic_route)
  reg("modpois", .sr_modpois_route)
  reg("lm", .sr_lm_route)
  sr_route_get(id, 1L)
}

.sf_main <- function(args) {
  rows <- as.numeric(.sf_arg(args, "rows", "20000000"))
  patients <- as.numeric(.sf_arg(args, "patients", "2000000"))
  chunk <- as.numeric(.sf_arg(args, "chunk", "500000"))
  routes <- strsplit(.sf_arg(args, "routes", "logistic,lm"), ",", fixed = TRUE)[[1L]]
  prof <- .sf_arg(args, "profile", "")
  if (nzchar(prof)) Sys.setenv(SR_PROFILE = prof)
  if (!is.finite(rows) || rows < 100 || !is.finite(patients) || patients < 1 || patients > rows ||
      !is.finite(chunk) || chunk < 1) {
    stop("need --rows >= 100, 1 <= --patients <= rows and --chunk >= 1")
  }
  if (!all(routes %in% c("logistic", "modpois", "lm"))) stop("--routes is a comma list of logistic, modpois, lm")
  base <- .sf_arg(args, "dir", tempdir())
  if (!dir.exists(base)) stop("not a directory: ", base)
  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
  root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
  setwd(root)
  source(file.path(root, "app", "global.R"), encoding = "UTF-8")

  profile <- sr_profile()
  caps <- sr_profile_caps(profile)
  limit_gb <- caps$duckdb_memory_gb + 2
  run_dir <- file.path(normalizePath(base, winslash = "/", mustWork = TRUE),
                       sprintf("sr_scale_fit_%s_%d", format(Sys.time(), "%Y%m%d_%H%M%S"), Sys.getpid()))
  dir.create(file.path(run_dir, "data", "extracts"), recursive = TRUE)
  dir.create(file.path(run_dir, "data", "tmp"))
  on.exit(unlink(run_dir, recursive = TRUE, force = TRUE), add = TRUE)
  .sf_say("StatRouter streaming-fit scale test: %s rows, %s patients, chunk %s, profile %s (DuckDB cap %s GB, %d threads)",
          format(rows, big.mark = ",", scientific = FALSE), format(patients, big.mark = ",", scientific = FALSE),
          format(chunk, big.mark = ",", scientific = FALSE), profile, format(caps$duckdb_memory_gb),
          caps$duckdb_threads)

  t0 <- proc.time()[["elapsed"]]
  tmp_pq <- file.path(run_dir, "data", "gen.parquet")
  .sf_generate(tmp_pq, rows, patients, caps$duckdb_memory_gb, caps$duckdb_threads)
  sha <- paste(as.character(unclass(openssl::sha256(file(tmp_pq)))), collapse = "")
  rel <- paste0("data/extracts/", sha, ".parquet")
  file.rename(tmp_pq, file.path(run_dir, rel))
  .sf_say("generate : %.1f s, Parquet %s", proc.time()[["elapsed"]] - t0, .sf_fmt_gb(file.size(file.path(run_dir, rel))))
  invisible(gc())

  me <- ps::ps_handle()
  pro <- .sr_run_prologue_stream(list(rel = rel, sha = sha))
  ok <- TRUE
  for (route in routes) {
    stopfile <- tempfile("sr_fit_stop_")
    watch_out <- tempfile("sr_fit_watch_")
    baseline <- as.numeric(ps::ps_memory_info(me)[["rss"]])
    watcher <- callr::r_bg(.sf_watch, args = list(pid = Sys.getpid(), stopfile = stopfile, out = watch_out,
                                                   interval = 0.5))
    Sys.sleep(1)
    t1 <- proc.time()[["elapsed"]]
    e <- new.env(parent = globalenv())
    e$sr_project_dir <- run_dir
    e$sr_duck_memory <- paste0(caps$duckdb_memory_gb, "GB")
    e$sr_duck_threads <- as.integer(caps$duckdb_threads)
    e$sr_duck_temp <- file.path(run_dir, "data", "tmp")
    v <- tryCatch(eval(parse(text = paste(pro, sr_compile(.sf_route(route), .sf_spec(route, chunk)), sep = "\n")), e),
                  error = function(err) err)
    wall <- proc.time()[["elapsed"]] - t1
    file.create(stopfile)
    watcher$wait(10000)
    if (watcher$is_alive()) watcher$kill()
    w <- tryCatch(as.numeric(readLines(watch_out, warn = FALSE)[1L]), error = function(err) NA_real_)
    unlink(c(stopfile, watch_out))
    os_peak <- tryCatch(as.numeric(ps::ps_memory_info(me)[["peak_wset"]]), error = function(err) NA_real_)
    peak <- max(c(w, os_peak), na.rm = TRUE)
    if (inherits(v, "error")) {
      .sf_say("%-8s : ERROR after %.1f s: %s", route, wall, conditionMessage(v))
      ok <- FALSE
      next
    }
    est <- v$coef$est[match("sexM", v$coef$term)]
    if (is.na(est)) est <- v$coef$est[2L]
    pass <- is.finite(peak) && peak < limit_gb * .sf_gb
    ok <- ok && pass
    .sf_say("%-8s : %.1f s, %s passes, n %s, clusters %s, exposure est %.6f (se %.6f)", route, wall,
            format(v$passes), format(v$n, big.mark = ","), format(v$n_clusters, big.mark = ","), est,
            v$coef$se[match(est, v$coef$est)])
    .sf_say("           peak RSS %s (watcher %s; OS peak working set since start %s; baseline %s) -> %s",
            .sf_fmt_gb(peak), .sf_fmt_gb(w), .sf_fmt_gb(os_peak), .sf_fmt_gb(baseline),
            if (pass) "PASS" else "FAIL")
    rm(v, e)
    invisible(gc())
  }
  .sf_say("limit: peak RSS < %s GB (DuckDB cap + 2 GB)", format(limit_gb))
  .sf_say("total : %.1f s", proc.time()[["elapsed"]] - t0)
  .sf_say("%s", if (ok) "SCALE FIT: PASS" else "SCALE FIT: FAIL")
  ok
}

if (sys.nframe() == 0L) {
  ok <- .sf_main(commandArgs(trailingOnly = TRUE))
  quit(status = if (isTRUE(ok)) 0L else 1L)
}
