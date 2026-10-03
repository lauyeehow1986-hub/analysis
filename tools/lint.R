# Static lints over the app: Rscript tools/lint.R [dir]   (works from any working directory)
#   p-gate  : compile() must not branch on a diagnostic test (only files under <dir>/routes)
#   air-gap : no network / package-provisioning calls anywhere under <dir>, plus the top-level
#             app/*.R files (global.R, app.R) when <dir> is the default app/R
# <dir> defaults to <repo>/app/R, derived from this script's own location (never from the cwd).
# Prints one line per finding as file:line: msg and exits 1 if there are any. Exits 2 if the
# directory is missing or holds no .R files (a lint that scanned nothing must not pass).
# Otherwise prints "LINT OK (N files)".

args <- commandArgs(trailingOnly = TRUE)

script <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L])
root <- dirname(dirname(normalizePath(script, winslash = "/", mustWork = TRUE)))
source(file.path(root, "app", "R", "core", "lint.R"), encoding = "UTF-8")

default_dir <- length(args) == 0L
dir <- if (default_dir) file.path(root, "app", "R") else args[1L]

rfiles <- function(d, recursive = TRUE) {
  if (!dir.exists(d)) return(character())
  list.files(d, pattern = "[.][Rr]$", recursive = recursive, full.names = TRUE)
}

if (!dir.exists(dir)) {
  cat(sprintf("lint: directory not found: %s\n", dir))
  quit(status = 2L)
}

all_files <- rfiles(dir)
if (default_dir) all_files <- c(all_files, rfiles(file.path(root, "app"), recursive = FALSE))
if (!length(all_files)) {
  cat(sprintf("lint: no .R files found under %s\n", dir))
  quit(status = 2L)
}
route_files <- rfiles(file.path(dir, "routes"))   # absent in P1a: p-gate runs over zero files

findings <- rbind(sr_lint_pgate(route_files), sr_lint_airgap(all_files))

if (nrow(findings)) {
  shown <- findings$file
  pre <- paste0(root, "/")
  rel <- startsWith(shown, pre)
  shown[rel] <- substring(shown[rel], nchar(pre) + 1L)
  cat(sprintf("%s:%d: %s", shown, findings$line, findings$msg), sep = "\n")
  quit(status = 1L)
}
cat(sprintf("LINT OK (%d files)\n", length(all_files)))
