# Static lints over app/R: Rscript tools/lint.R [dir]   (run from the repo root)
#   p-gate  : compile() must not branch on a diagnostic test (only files under <dir>/routes)
#   air-gap : no network / package-provisioning calls anywhere under <dir>
# Prints one line per finding as file:line: msg and exits 1 if there are any, else prints LINT OK.

args <- commandArgs(trailingOnly = TRUE)
dir <- if (length(args)) args[1L] else "app/R"

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
source(file.path(root, "app", "R", "core", "lint.R"), encoding = "UTF-8")

rfiles <- function(d) {
  if (!dir.exists(d)) return(character())
  list.files(d, pattern = "[.][Rr]$", recursive = TRUE, full.names = TRUE)
}

all_files <- rfiles(dir)
route_files <- rfiles(file.path(dir, "routes"))   # absent in P1a: p-gate runs over zero files

findings <- rbind(sr_lint_pgate(route_files), sr_lint_airgap(all_files))

if (nrow(findings)) {
  cat(sprintf("%s:%d: %s", findings$file, findings$line, findings$msg), sep = "\n")
  quit(status = 1L)
}
cat("LINT OK\n")
