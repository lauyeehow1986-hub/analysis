# Scale test for the P1b data layer (manual tool; no testthat test):
#   Rscript tools/scale_test.R [rows=20000000] [cols=60] [dir] [--clean] [--profile=locked16|laptop]
#
# What it does, in order:
#   1. generate  a synthetic CSV of `rows` x `cols` (never real data), streamed with
#                data.table::fwrite(append = TRUE) in 1M-row blocks, so the file is never held in memory;
#   2. ingest    create a project and run sr_ingest() (chunked) in a callr::r_bg child; the parent polls the
#                committed part files, KILLS the child once 2 parts are committed, then RESUMES in-process by
#                calling sr_ingest() again on the same project and source (it picks up from the checkpoint);
#   3. profile   sr_profile_table() and sr_dq_run() on the ingested table;
#   4. report    wall time per phase, peak RSS, the data/tmp spill, the Parquet size, and PASS / FAIL.
#
# PASS means peak RSS < (caps$duckdb_memory_gb + 2) GB for the hardware profile of the project.
#
# How the peak RSS is measured (stated here and in the report):
#   * child phase (the ingest that is killed): the parent polls ps::ps_memory_info()$rss of the child every
#     0.5 s while it runs;
#   * in-process phase (resume, profile, DQ): the parent is blocked inside DuckDB, so a later::later() loop
#     could not fire (later callbacks run only when R is idle). A small watcher process (callr::r_bg) polls
#     ps::ps_memory_info()$rss of the parent through ps::ps_handle() every 0.5 s instead. The OS peak
#     working set of the parent (ps peak_wset) is read at the end as a cross-check, and the larger of
#     the two is the figure used.
#
# `dir` (default: the session temp dir) is where the CSV and the project are written, inside one folder
# named sr_scale_<timestamp>_<pid>. That folder, and nothing else, is deleted when the script is given
# --clean or when `dir` is the default temp dir. --profile forces the hardware profile (for example
# --profile=locked16 to check a laptop against the 16 GB PC with its 6 GB DuckDB cap). The full 20M x 60
# run is for the person on the locked16 PC: the laptop runs a smaller size as a smoke test.
# Synthetic data only. Run from anywhere; the script finds the repo from its own location.

# ---- helpers ----------------------------------------------------------------------------------------

.sc_gb <- 1024^3
.sc_fmt_gb <- function(bytes) sprintf("%.2f GB", bytes / .sc_gb)
.sc_fmt_secs <- function(s) {
  if (s < 90) sprintf("%.1f s", s) else sprintf("%.1f min (%.0f s)", s / 60, s)
}
.sc_say <- function(...) cat(sprintf(...), "\n", sep = "")

# Total size of the files under a directory (0 when it does not exist).
.sc_dir_bytes <- function(d) {
  f <- list.files(d, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  if (!length(f)) return(0)
  sum(file.size(f), na.rm = TRUE)
}

# Current RSS in bytes of a process handle, NA when it is gone.
.sc_rss <- function(h) tryCatch(as.numeric(ps::ps_memory_info(h)[["rss"]]), error = function(e) NA_real_)

# The committed parts of the staged store of a table (renamed into place by .sr_part_commit).
.sc_parts_on_disk <- function(project_dir, table) {
  d <- file.path(project_dir, "data", "ingest", table, "store")
  length(list.files(d, pattern = "^part-[0-9]{5}[.]parquet$"))
}

# The number of parts the checkpoint records as committed.
.sc_parts_in_checkpoint <- function(project_dir, table) {
  f <- file.path(project_dir, "data", "ingest", table, "checkpoint.json")
  if (!file.exists(f)) return(0L)
  ck <- tryCatch(.sr_read_json_file(f), error = function(e) NULL)
  if (is.null(ck)) 0L else length(ck$parts)
}

# ---- 1. generate ------------------------------------------------------------------------------------

# Column plan: an id, then kinds cycled int, dbl, cat, date, flag, txt.
.sc_plan <- function(cols) {
  kinds <- rep(c("int", "dbl", "cat", "date", "flag", "txt"), length.out = max(0L, cols - 1L))
  data.frame(name = c("patient_id", sprintf("%s_%02d", kinds, seq_along(kinds))),
             kind = c("id", kinds), stringsAsFactors = FALSE)[seq_len(cols), , drop = FALSE]
}

.sc_block <- function(plan, n, first_id) {
  vocab <- c("stable on review", "no complaints", "routine follow up", "medication adjusted", "awaiting results",
             "discharged home", "referred to clinic", "mild symptoms", "improving", "no change")
  na_at <- function(x, p) {
    x[stats::runif(length(x)) < p] <- NA
    x
  }
  cols <- lapply(seq_len(nrow(plan)), function(i) {
    switch(plan$kind[i],
      id   = first_id + seq_len(n) - 1,
      int  = na_at(sample.int(100L, n, replace = TRUE), 0.03),
      dbl  = na_at(round(stats::rnorm(n, 100, 15), 1), 0.05),
      cat  = sample(c("A", "B", "C", "D"), n, replace = TRUE),
      date = data.table::as.IDate("2015-01-01") + sample.int(3650L, n, replace = TRUE),
      flag = sample.int(2L, n, replace = TRUE) - 1L,
      txt  = sample(vocab, n, replace = TRUE))
  })
  names(cols) <- plan$name
  data.table::as.data.table(cols)
}

.sc_generate <- function(path, rows, cols, block = 1e6) {
  plan <- .sc_plan(cols)
  set.seed(20261004L)
  done <- 0
  while (done < rows) {
    n <- min(block, rows - done)
    b <- .sc_block(plan, n, done + 1)
    data.table::fwrite(b, path, append = done > 0, col.names = done == 0, quote = "auto", showProgress = FALSE)
    done <- done + n
    rm(b)
  }
  invisible(gc())
  invisible(path)
}

# ---- 2/3. the child and the watcher -----------------------------------------------------------------

# Runs in the callr::r_bg child: open the project and ingest. Killed part-way, on purpose.
.sc_child <- function(root, dir, path, table, chunk_bytes) {
  setwd(root)
  source(file.path(root, "app", "global.R"), encoding = "UTF-8")
  p <- sr_project_open(dir)
  sr_ingest(p, path, table = table, chunk_bytes = chunk_bytes)
  invisible(TRUE)
}

# Runs in a callr::r_bg watcher: polls the RSS of process `pid` and the size of `tmp` (the DuckDB spill
# directory) every `interval` s until the file `stopfile` exists, then writes "<peak rss> <peak tmp>" to `out`.
.sc_watch <- function(pid, tmp, stopfile, out, interval) {
  h <- ps::ps_handle(pid)
  peak <- 0
  tpeak <- 0
  repeat {
    r <- tryCatch(as.numeric(ps::ps_memory_info(h)[["rss"]]), error = function(e) NA_real_)
    if (!is.na(r)) peak <- max(peak, r)
    f <- list.files(tmp, recursive = TRUE, full.names = TRUE, all.files = TRUE)
    tb <- if (length(f)) sum(file.size(f), na.rm = TRUE) else 0
    tpeak <- max(tpeak, tb)
    if (file.exists(stopfile) || is.na(r)) break
    Sys.sleep(interval)
  }
  writeLines(sprintf("%.0f %.0f", peak, tpeak), out)
  invisible(NULL)
}

# ---- main -------------------------------------------------------------------------------------------

.sc_main <- function(args) {
  flags <- args[startsWith(args, "--")]
  pos <- args[!startsWith(args, "--")]
  clean_flag <- "--clean" %in% flags
  prof <- sub("^--profile=", "", grep("^--profile=", flags, value = TRUE))
  if (length(prof)) Sys.setenv(SR_PROFILE = prof[1L])
  rows <- if (length(pos) >= 1L) as.numeric(pos[1L]) else 20000000
  cols <- if (length(pos) >= 2L) as.integer(pos[2L]) else 60L
  default_dir <- length(pos) < 3L
  base <- if (default_dir) tempdir() else pos[3L]
  if (!is.finite(rows) || rows < 1 || is.na(cols) || cols < 1L) stop("rows and cols must be positive numbers")
  if (!dir.exists(base)) stop("not a directory: ", base)
  clean <- clean_flag || default_dir

  script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
  root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
  setwd(root)
  source(file.path(root, "app", "global.R"), encoding = "UTF-8")

  run_dir <- file.path(normalizePath(base, winslash = "/", mustWork = TRUE),
                       sprintf("sr_scale_%s_%d", format(Sys.time(), "%Y%m%d_%H%M%S"), Sys.getpid()))
  dir.create(run_dir)
  csv <- file.path(run_dir, "scale.csv")
  pdir <- file.path(run_dir, "project")
  table <- "scale_test"
  p <- NULL
  on.exit({
    if (!is.null(p)) {
      try(sr_duck_close(p), silent = TRUE)
      try(sr_project_close(p), silent = TRUE)
    }
    if (clean && startsWith(basename(run_dir), "sr_scale_")) {
      unlink(run_dir, recursive = TRUE, force = TRUE)
      .sc_say("cleaned: removed the generated CSV and the project")
    } else {
      .sc_say("kept: %s", run_dir)
    }
  }, add = TRUE)

  profile <- sr_profile()
  caps <- sr_profile_caps(profile)
  limit_gb <- caps$duckdb_memory_gb + 2
  me <- ps::ps_handle()
  t_all <- proc.time()[["elapsed"]]
  wall <- list()
  lap <- function(name, expr) {
    t0 <- proc.time()[["elapsed"]]
    force(expr)
    wall[[name]] <<- proc.time()[["elapsed"]] - t0
    invisible(NULL)
  }
  .sc_say("StatRouter scale test: %s rows x %d cols, profile %s (DuckDB cap %s GB, %d threads)",
          format(rows, big.mark = ",", scientific = FALSE), cols, profile, format(caps$duckdb_memory_gb),
          caps$duckdb_threads)

  # 1. generate
  .sc_say("generating the CSV in 1M-row blocks ...")
  lap("generate", .sc_generate(csv, rows, cols))
  csv_bytes <- file.size(csv)
  .sc_say("  csv: %s", .sc_fmt_gb(csv_bytes))

  # 2. create the project, then ingest in a child that is killed after 2 committed parts
  p <- sr_project_create(pdir, "scale test", profile = profile)
  sr_project_close(p)                                   # the child takes the project lock
  p <- NULL
  # several parts even at the smoke size: about 12 per file, between 8 MiB and the default 256 MiB
  chunk_bytes <- min(256 * 1024^2, max(8 * 1024^2, ceiling(csv_bytes / 12)))
  .sc_say("ingest: chunk %s MiB; child ingest, kill after 2 committed parts ...", format(round(chunk_bytes / 1024^2)))
  out_f <- tempfile("sr_scale_child_out_")
  err_f <- tempfile("sr_scale_child_err_")
  on.exit(unlink(c(out_f, err_f)), add = TRUE)
  tmp_dir <- file.path(pdir, "data", "tmp")
  child_peak <- 0
  spill_peak <- 0
  killed <- FALSE
  finished_early <- FALSE
  t0 <- proc.time()[["elapsed"]]
  bg <- callr::r_bg(.sc_child, args = list(root = root, dir = pdir, path = csv, table = table,
                                           chunk_bytes = chunk_bytes), stdout = out_f, stderr = err_f)
  ch <- ps::ps_handle(bg$get_pid())
  repeat {
    r <- .sc_rss(ch)
    if (!is.na(r)) child_peak <- max(child_peak, r)
    spill_peak <- max(spill_peak, .sc_dir_bytes(tmp_dir))
    if (!bg$is_alive()) {
      finished_early <- TRUE
      break
    }
    if (.sc_parts_on_disk(pdir, table) >= 2L) {
      bg$kill()
      killed <- TRUE
      break
    }
    Sys.sleep(0.5)
  }
  bg$wait(5000)
  wall$ingest_child <- proc.time()[["elapsed"]] - t0
  if (finished_early) {
    err <- tryCatch(paste(readLines(err_f, warn = FALSE), collapse = "\n"), error = function(e) "")
    stop("the child finished (or failed) before 2 parts were committed; use more rows or a smaller chunk.\n", err)
  }
  parts_at_resume <- .sc_parts_in_checkpoint(pdir, table)
  parts_files_at_resume <- .sc_parts_on_disk(pdir, table)
  .sc_say("  killed: %s; parts committed at resume: %d (part files on disk: %d)", killed, parts_at_resume,
          parts_files_at_resume)

  # 3. resume, profile and DQ in-process, with the watcher on this process
  p <- sr_project_open(pdir)                            # takes over the stale lock of the dead child
  if (isTRUE(p$readonly)) stop("could not open the project for writing after the kill")
  stopfile <- tempfile("sr_scale_stop_")
  watch_out <- tempfile("sr_scale_watch_")
  on.exit(unlink(c(stopfile, watch_out)), add = TRUE)
  gc()
  baseline <- .sc_rss(me)
  watcher <- callr::r_bg(.sc_watch, args = list(pid = Sys.getpid(), tmp = tmp_dir, stopfile = stopfile,
                                                out = watch_out, interval = 0.5))
  warns <- character()
  lap("resume", p <- withCallingHandlers(
    sr_ingest(p, csv, table = table, chunk_bytes = chunk_bytes),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }))
  .sc_say("resume: %s", .sc_fmt_secs(wall$resume))
  lap("profile", prof_res <- sr_profile_table(p, table))
  .sc_say("profile: %s", .sc_fmt_secs(wall$profile))
  lap("dq", p <- sr_dq_run(p, table))
  .sc_say("dq: %s", .sc_fmt_secs(wall$dq))
  findings <- sr_dq_findings(p, table)

  file.create(stopfile)
  watcher$wait(10000)
  w <- tryCatch(as.numeric(strsplit(readLines(watch_out, warn = FALSE)[1L], " ", fixed = TRUE)[[1L]]),
                error = function(e) c(NA_real_, NA_real_))
  if (watcher$is_alive()) watcher$kill()
  inproc_sampled <- w[1L]
  inproc_os <- tryCatch(as.numeric(ps::ps_memory_info(me)[["peak_wset"]]), error = function(e) NA_real_)
  inproc_peak <- max(inproc_sampled, inproc_os, na.rm = TRUE)
  spill_peak <- max(spill_peak, w[2L], na.rm = TRUE)
  spill_final <- .sc_dir_bytes(tmp_dir)

  # 4. checks and the report
  tab <- p$state$tables[[table]]
  rows_stored <- tab$store$rows
  part_rows <- sum(vapply(tab$store$parts, function(x) x$rows, numeric(1)))
  parquet_bytes <- .sc_dir_bytes(file.path(pdir, "data", "store", table))
  not_resumed <- grep("not resuming", warns, value = TRUE)
  resumed <- parts_at_resume >= 2L && !length(not_resumed) && length(tab$store$parts) > parts_at_resume
  rows_ok <- identical(as.numeric(rows_stored), as.numeric(rows)) && identical(as.numeric(part_rows), as.numeric(rows))
  peak <- max(child_peak, inproc_peak)
  rss_pass <- peak < limit_gb * .sc_gb
  total <- proc.time()[["elapsed"]] - t_all

  cat("\n==== scale test report ====\n")
  .sc_say("profile                : %s (DuckDB cap %s GB, threads %d)", profile, format(caps$duckdb_memory_gb),
          caps$duckdb_threads)
  .sc_say("size                   : %s rows x %d cols, CSV %s", format(rows, big.mark = ",", scientific = FALSE),
          cols, .sc_fmt_gb(csv_bytes))
  .sc_say("chunk size             : %s MiB; %d parts in the store", format(round(chunk_bytes / 1024^2)),
          length(tab$store$parts))
  .sc_say("wall: generate         : %s", .sc_fmt_secs(wall$generate))
  .sc_say("wall: ingest (child)   : %s (killed after 2 parts)", .sc_fmt_secs(wall$ingest_child))
  .sc_say("wall: ingest (resume)  : %s", .sc_fmt_secs(wall$resume))
  .sc_say("wall: profile          : %s", .sc_fmt_secs(wall$profile))
  .sc_say("wall: DQ run           : %s (%d findings)", .sc_fmt_secs(wall$dq), nrow(findings))
  .sc_say("wall: total            : %s", .sc_fmt_secs(total))
  .sc_say("kill and resume        : parts already committed at resume = %d; resumed from the checkpoint = %s",
          parts_at_resume, resumed)
  .sc_say("rows stored            : %s (expected %s): %s", format(rows_stored, big.mark = ",", scientific = FALSE),
          format(rows, big.mark = ",", scientific = FALSE), if (rows_ok) "OK" else "MISMATCH")
  .sc_say("peak RSS: child        : %s (ps rss of the child, polled every 0.5 s)", .sc_fmt_gb(child_peak))
  .sc_say("peak RSS: in-process   : %s (watcher process every 0.5 s: %s; OS peak working set: %s; baseline %s)",
          .sc_fmt_gb(inproc_peak), .sc_fmt_gb(inproc_sampled), .sc_fmt_gb(inproc_os), .sc_fmt_gb(baseline))
  .sc_say("peak RSS               : %s (limit %s GB = DuckDB cap %s + 2)", .sc_fmt_gb(peak), format(limit_gb),
          format(caps$duckdb_memory_gb))
  .sc_say("data/tmp spill         : peak %s, at the end %s", .sc_fmt_gb(spill_peak), .sc_fmt_gb(spill_final))
  .sc_say("parquet store          : %s (%.1f pct of the CSV)", .sc_fmt_gb(parquet_bytes),
          100 * parquet_bytes / csv_bytes)
  .sc_say("profiled columns       : %d", nrow(prof_res$columns))
  .sc_say("%s", if (rss_pass) "RSS CHECK: PASS" else "RSS CHECK: FAIL")
  ok <- rss_pass && rows_ok && resumed
  .sc_say("%s", if (ok) "PASS" else "FAIL")
  invisible(ok)
}

if (sys.nframe() == 0L && !interactive()) {
  ok <- .sc_main(commandArgs(trailingOnly = TRUE))
  quit(status = if (isTRUE(ok)) 0L else 1L)
}
