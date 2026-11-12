# Builds tests/testthat/fixtures/pre-p1e2-cox-lock/events.jsonl: the event log of a small locked project with a
# Cox analysis (survival::lung, public data) made by the code BEFORE P1e.2 (no cox_method field), so
# test-scale-surv-run.R can pin that such a project keeps its SAP hash and run key. Run once from the root of
# a checkout of the pre-P1e.2 code (main), with the output folder in SR_FIXTURE_OUT:
#   SR_FIXTURE_OUT=<folder> Rscript <path>/make_pre_p1e2_cox_lock.R
# It prints the sap_hash and the run key of a fixed extract sha, which the test pins.
suppressMessages(library(testthat))
root <- normalizePath(".", winslash = "/")
for (f in c("helper-load.R", "helper-data.R", "helper-run.R")) {
  source(file.path(root, "tests/testthat", f), local = FALSE)
}
options(sr.user = "sr-test")
if (!(.sr_route_key("cox", 1L) %in% ls(.sr_routes, all.names = TRUE))) sr_route_register(.sr_cox_route())

build <- function() {
  d <- survival::lung
  d$id <- seq_len(nrow(d))
  p <- .fx_ready_project(d, "t", c(id = "id", status = "outcome"), env = globalenv())
  spec <- list(purpose = "explain-causal", time = "time", status = "status", status_event = 2, exposure = "sex",
               terms = list(list(var = "age")),
               layers = list(missing = "cc", weights = "none", matching = FALSE, resampling = "none",
                             framework = "frequentist", clustered = FALSE), seed = 7L)
  p <- sr_sap_add(p, "a1", "cox", spec, "t", label = "primary")
  p <- sr_sap_lock(.fx_feasible(p))
  stopifnot(identical(sr_sap_hash(p), p$state$sap$lock$sap_hash))
  cat("sap_hash:", p$state$sap$lock$sap_hash, "\n")
  a <- p$state$analyses$a1
  s <- .sr_spec_from_state(a$spec)
  cat("run_key:", sr_content_hash(strrep("ab", 32L), list(route = .sr_route_key(a$route, a$route_version), spec = s),
                                   s$seed, "lib", "hw"), "\n")
  list(dir = p$paths$dir, p = p)
}
b <- build()
dir <- b$dir
out <- Sys.getenv("SR_FIXTURE_OUT", file.path(root, "tests/testthat/fixtures/pre-p1e2-cox-lock"))
unlink(out, recursive = TRUE)
dir.create(out, recursive = TRUE)
file.copy(file.path(dir, "events.jsonl"), file.path(out, "events.jsonl"))
