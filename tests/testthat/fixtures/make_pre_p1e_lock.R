# Builds tests/testthat/fixtures/pre-p1e-lock/events.jsonl: the event log of a small locked project (synthetic data only) made by the
# SAP code BEFORE P1e.1 (no engine field), so test-scale-lock.R can pin that such a project keeps its SAP
# hash. Run once from the repo root against the pre-P1e code:
#   Rscript tests/testthat/fixtures/make_pre_p1e_lock.R
# The project keeps its stored sap_hash in events.jsonl (the sap.locked event).
suppressMessages(library(testthat))
root <- normalizePath(".", winslash = "/")
for (f in c("helper-load.R", "helper-data.R", "helper-run.R")) {
  source(file.path(root, "tests/testthat", f), local = FALSE)
}
options(sr.user = "sr-test")

build <- function() {
  set.seed(7)
  n <- 80L
  df <- data.frame(id = seq_len(n), x1 = round(rnorm(n), 3), sex = sample(c("M", "F"), n, TRUE))
  df$y <- rbinom(n, 1L, plogis(0.3 * df$x1))
  dir <- file.path(tempfile("p1e"), "proj")
  p <- sr_project_create(dir, "pre-p1e", profile = "laptop")
  csv <- file.path(dirname(dir), "t.csv")
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = "t", options = list(drive_info = function(path)
    list(free_bytes = 1e13, remote = FALSE, warnings = character())))
  p <- sr_roles_set(p, "t", c(id = "id", y = "outcome", sex = "exposure"))
  p <- sr_dq_run(p, "t")
  sg <- sr_suggest(p, "t")
  for (i in seq_len(nrow(sg))) p <- sr_action_decide(p, sg$finding_id[i], sg$action[i], table = "t")
  p <- sr_sap_add(p, "a1", "logistic", .run_spec("y", exposure = "sex", exposure_ref = "F"), "t",
                  label = "primary")
  p <- sr_sap_lock(.fx_feasible(p))
  cat("sap_hash:", p$state$sap$lock$sap_hash, "\n")
  stopifnot(identical(sr_sap_hash(p), p$state$sap$lock$sap_hash))
  sr_duck_close(p)
  sr_project_close(p)
  dir
}
dir <- build()
out <- file.path(root, "tests/testthat/fixtures/pre-p1e-lock")
unlink(out, recursive = TRUE)
dir.create(out, recursive = TRUE)
# Only the event log is kept: sr_sap_hash reads the replayed state, so the table is not needed (and the repo
# ignores data/ and *.parquet).
file.copy(file.path(dir, "events.jsonl"), file.path(out, "events.jsonl"))
