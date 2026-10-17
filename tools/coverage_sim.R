# The full CI-coverage simulation (manual; hours): Rscript tools/coverage_sim.R [cells] [reps_scale]
#   cells       "all" (default) or a comma-separated list of sr_coverage_cells()$cell
#   reps_scale  multiplies every cell's default replications (default 1; at least 2 are run)
# Workers: getOption("sr.coverage_workers", Sys.getenv("SR_COVERAGE_WORKERS", <cores - 2, at least 1>)).
# Results do not depend on the number of workers (replication r uses the r-th L'Ecuyer stream of
# seed 1). Prints the coverage table and a plain CSV block on stdout (progress goes to stderr) and
# writes nothing into the repo. The recorded results live in docs/validation.md.

args <- commandArgs(trailingOnly = TRUE)
options(width = 200)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
source(file.path(root, "app", "global.R"), encoding = "UTF-8")

cells_all <- sr_coverage_cells()
cells <- if (length(args) >= 1L && nzchar(args[1L]) && args[1L] != "all") {
  strsplit(args[1L], ",", fixed = TRUE)[[1L]]
} else {
  cells_all$cell
}
unknown <- setdiff(cells, cells_all$cell)
if (length(unknown)) {
  stop("unknown cell(s): ", paste(unknown, collapse = ", "), "; one of: ", paste(cells_all$cell, collapse = ", "),
       call. = FALSE)
}
scale <- if (length(args) >= 2L) suppressWarnings(as.numeric(args[2L])) else 1
if (!(length(scale) == 1L && is.finite(scale) && scale > 0)) stop("reps_scale must be a positive number", call. = FALSE)

cores <- parallel::detectCores()
default_workers <- if (is.na(cores)) 1L else max(1L, as.integer(cores) - 2L)
workers <- getOption("sr.coverage_workers", Sys.getenv("SR_COVERAGE_WORKERS", as.character(default_workers)))
workers <- suppressWarnings(as.integer(workers))
if (length(workers) != 1L || is.na(workers) || workers < 1L) {
  stop("the coverage workers (option sr.coverage_workers / env SR_COVERAGE_WORKERS) must be a whole number >= 1",
       call. = FALSE)
}

seed <- 1L
rows <- list()
errors <- list()
for (cell in cells) {
  def <- cells_all[cells_all$cell == cell, ]
  reps <- max(2L, as.integer(round(def$reps * scale)))
  message(sprintf("[%s] %s: %d replications on %d worker(s) ...", format(Sys.time(), "%H:%M:%S"), cell, reps, workers))
  t0 <- proc.time()[["elapsed"]]
  r <- sr_coverage_cell(cell, reps, seed = seed, workers = workers)
  secs <- proc.time()[["elapsed"]] - t0
  message(sprintf("[%s] %s: coverage %.4f, pass %s, failed %d, %.0f s", format(Sys.time(), "%H:%M:%S"), cell,
                  r$coverage, r$pass, r$failed, secs))
  rows[[cell]] <- data.frame(cell = cell, reps = r$reps, failed = r$failed, coverage = r$coverage, mcse = r$mcse,
                             pass = r$pass, bias = r$bias, mean_width = r$mean_width, truth = r$truth,
                             B = def$B, m = def$m, seconds = round(secs, 1), stringsAsFactors = FALSE)
  if (length(r$errors)) errors[[cell]] <- r$errors
}
tab <- do.call(rbind, unname(rows))

cat("StatRouter CI-coverage simulation\n")
cat(sprintf("date %s | %s | n 200 | seed %d | workers %d | reps_scale %s\n", format(Sys.Date()),
            R.version.string, seed, workers, format(scale)))
cat("pass = |coverage - 0.95| < 3 * mcse, mcse = sqrt(coverage * (1 - coverage) / (reps - failed))\n\n")
shown <- tab
for (nm in c("coverage", "mcse", "bias", "mean_width", "truth")) shown[[nm]] <- sprintf("%.4f", tab[[nm]])
print(shown, row.names = FALSE)
for (cell in names(errors)) cat(sprintf("\nfailed replications in %s: %s\n", cell, paste(errors[[cell]], collapse = "; ")))
cat(if (all(tab$pass)) "\nALL CELLS PASS\n" else
  sprintf("\nFAILING CELLS: %s\n", paste(tab$cell[!tab$pass], collapse = ", ")))
cat("\n--- CSV ---\n")
utils::write.csv(tab, stdout(), row.names = FALSE)
cat("--- END CSV ---\n")
