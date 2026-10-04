# Write the synthetic cardiac cohort to samples/out/ (gitignored; regenerate any time):
#   Rscript samples/make_samples.R
#     cardiac.csv          the cohort with planted defects
#     cardiac_clean.csv    the clean twin (same seed, nothing planted)
#     cardiac_dict.csv     the data dictionary
#     cardiac_planted.csv  one row per planted defect (the answer key)
# Synthetic data only.

args <- commandArgs(FALSE)
script <- sub("^--file=", "", grep("^--file=", args, value = TRUE)[1L])
here <- if (is.na(script)) "samples" else dirname(normalizePath(script, winslash = "/", mustWork = TRUE))
source(file.path(here, "gen_cardiac.R"))

out <- file.path(here, "out")
dir.create(out, showWarnings = FALSE, recursive = TRUE)
write_csv <- function(df, name) {
  utils::write.csv(df, file.path(out, name), row.names = FALSE, fileEncoding = "UTF-8")
}

s <- sr_sample_cardiac(n = 2000L, seed = 1L, defects = TRUE)
write_csv(s$data, "cardiac.csv")
write_csv(s$dict, "cardiac_dict.csv")
write_csv(s$planted, "cardiac_planted.csv")
write_csv(sr_sample_cardiac(n = 2000L, seed = 1L, defects = FALSE)$data, "cardiac_clean.csv")
cat("wrote", length(list.files(out)), "files to", out, "\n")
