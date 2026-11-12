# Run every test file in its own Rscript process with a per-file timeout, and record the outcome per file.
#   Rscript tools/run_tests_each.R [repo=<this repo>] [out=tools/out/each-<stamp>] [timeout_s=1200] [filter]
#
# Each file runs as `testthat::test_dir(<repo>/tests/testthat, filter = "^<name>$")` with NOT_CRAN=true in a
# fresh process (tools/run_test_one.R), so a file that hangs is killed at the timeout and the next file runs.
# `repo` may be another checkout (for example a temp worktree of the base commit): the runner scripts are
# taken from this repo, the tests and the app from `repo`.
# Writes <out>/<name>.log per file and <out>/summary.csv (file, status, n_expect, failed, errors, skipped,
# warnings, seconds) where status is pass, fail, error (the process died) or timeout.
# Synthetic data only; no network calls.

args <- commandArgs(trailingOnly = TRUE)
here <- normalizePath(dirname(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])),
                      winslash = "/")
repo <- normalizePath(if (length(args) >= 1L && nzchar(args[1L])) args[1L] else file.path(here, ".."),
                      winslash = "/")
out <- if (length(args) >= 2L && nzchar(args[2L])) args[2L] else
  file.path(here, "out", paste0("each-", format(Sys.time(), "%Y%m%d-%H%M%S")))
timeout <- if (length(args) >= 3L && nzchar(args[3L])) as.numeric(args[3L]) else 1200
filter <- if (length(args) >= 4L && nzchar(args[4L])) args[4L] else NULL
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out, winslash = "/")

rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
one <- file.path(here, "run_test_one.R")
files <- sort(list.files(file.path(repo, "tests", "testthat"), pattern = "^test-.*[.]R$"))
names <- sub("^test-(.*)[.]R$", "\\1", files)
if (!is.null(filter)) names <- names[grepl(filter, names)]

summary_csv <- file.path(out, "summary.csv")
cat("file,status,n_expect,failed,errors,skipped,warnings,seconds\n", file = summary_csv)
for (nm in names) {
  log <- file.path(out, paste0(nm, ".log"))
  res <- file.path(out, paste0(nm, ".res"))
  t0 <- Sys.time()
  env <- c(Sys.getenv(), NOT_CRAN = "true")
  r <- list(timeout = FALSE)
  px <- processx::process$new(rscript, c(one, repo, nm, res), wd = repo, stdout = log, stderr = "2>&1",
                              env = env, cleanup_tree = TRUE)
  px$wait(timeout * 1000)
  if (px$is_alive()) {
    r$timeout <- TRUE
    px$kill_tree()
  }
  secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  counts <- if (file.exists(res)) as.integer(strsplit(readLines(res, warn = FALSE)[1L], ",")[[1L]]) else
    rep(NA_integer_, 5L)
  status <- if (isTRUE(r$timeout)) "timeout" else if (!file.exists(res)) "error" else
    if (counts[2L] > 0L || counts[3L] > 0L) "fail" else "pass"
  line <- sprintf("%s,%s,%s,%.0f", nm, status, paste(counts, collapse = ","), secs)
  cat(line, "\n", sep = "", file = summary_csv, append = TRUE)
  cat(line, "\n", sep = "")
}
cat("DONE", out, "\n")
