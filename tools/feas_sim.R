# The full feasibility SE validation (manual): Rscript tools/feas_sim.R [cells] [reps_scale]
#   cells       "all" (default) or a comma-separated list of sr_feas_sim_cells()$cell
#   reps_scale  multiplies every cell's default replications (default 1; at least 2 are run)
# Workers: getOption("sr.feas_sim_workers", Sys.getenv("SR_FEAS_SIM_WORKERS", <cores - 2, at least 1>)).
# Results do not depend on the number of workers (replication r uses the r-th L'Ecuyer stream of
# seed 1). Prints the table and a plain CSV block on stdout (progress goes to stderr) and writes
# nothing into the repo. The recorded results live in docs/validation.md.

args <- commandArgs(trailingOnly = TRUE)
options(width = 200)
script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
source(file.path(root, "app", "global.R"), encoding = "UTF-8")

cells_all <- sr_feas_sim_cells()
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
workers <- getOption("sr.feas_sim_workers", Sys.getenv("SR_FEAS_SIM_WORKERS", as.character(default_workers)))
workers <- suppressWarnings(as.integer(workers))
if (length(workers) != 1L || is.na(workers) || workers < 1L) {
  stop("the feasibility simulation workers (option sr.feas_sim_workers / env SR_FEAS_SIM_WORKERS) must be a whole number >= 1",
       call. = FALSE)
}

seed <- 1L
rows <- list()
errors <- list()
t_all <- proc.time()[["elapsed"]]
for (cell in cells) {
  def <- cells_all[cells_all$cell == cell, ]
  reps <- max(2L, as.integer(round(def$reps * scale)))
  message(sprintf("[%s] %s: %d replications on %d worker(s) ...", format(Sys.time(), "%H:%M:%S"), cell, reps, workers))
  t0 <- proc.time()[["elapsed"]]
  r <- sr_feas_sim_cell(cell, reps, seed = seed, workers = workers)
  secs <- proc.time()[["elapsed"]] - t0
  message(sprintf("[%s] %s: ratio %.4f, pass %s, failed %d, %.0f s", format(Sys.time(), "%H:%M:%S"), cell,
                  r$ratio, r$pass, r$failed, secs))
  rows[[cell]] <- data.frame(cell = cell, route = def$route, measure = def$measure, clustered = def$clustered,
                             n = def$n, reps = r$reps, pred_every = def$pred_every, pred_se = r$pred_se,
                             emp_sd = r$emp_sd, ratio = r$ratio, pass = r$pass, failed = r$failed,
                             seconds = round(secs, 1), stringsAsFactors = FALSE)
  if (length(r$errors)) errors[[cell]] <- r$errors
}
tab <- do.call(rbind, unname(rows))

cat("StatRouter feasibility SE validation\n")
cat(sprintf("date %s | %s | seed %d | workers %d | reps_scale %s | total %.0f s\n", format(Sys.Date()),
            R.version.string, seed, workers, format(scale), proc.time()[["elapsed"]] - t_all))
cat("ratio = pred_se / emp_sd (log scale for OR, RR, HR); pass = |ratio - 1| <= 0.10\n\n")
shown <- tab
for (nm in c("pred_se", "emp_sd", "ratio")) shown[[nm]] <- sprintf("%.5f", tab[[nm]])
print(shown, row.names = FALSE)
for (cell in names(errors)) cat(sprintf("\nfailed replications in %s: %s\n", cell, paste(errors[[cell]], collapse = "; ")))
if (all(tab$pass) && sum(tab$failed) == 0L) {
  cat("\nALL CELLS PASS\n")
} else {
  if (!all(tab$pass)) cat(sprintf("\nFAILING CELLS: %s\n", paste(tab$cell[!tab$pass], collapse = ", ")))
  hit <- tab$failed > 0L
  if (any(hit)) {
    cat(sprintf("\nFAILED REPLICATIONS: %s\n",
                paste(sprintf("%s %d", tab$cell[hit], as.integer(tab$failed[hit])), collapse = ", ")))
  }
}
cat("\n--- CSV ---\n")
utils::write.csv(tab, stdout(), row.names = FALSE)
cat("--- END CSV ---\n")
