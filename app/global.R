# StatRouter loader. Resolves sr_root (the app/ directory) and sources the core files in order.
# Works when sourced from the repo root (source("app/global.R")), when Shiny sources it with the
# working directory set to app/, and from tests.

sr_root <- local({
  ofile <- NULL
  for (fr in rev(sys.frames())) {
    f <- fr$ofile
    if (!is.null(f) && is.character(f) && length(f) == 1L && nzchar(f) && file.exists(f)) {
      ofile <- f
      break
    }
  }
  if (!is.null(ofile)) {
    dirname(ofile)
  } else if (file.exists("global.R") && dir.exists("R")) {
    "."
  } else {
    "app"
  }
})
sr_root <- normalizePath(sr_root, winslash = "/", mustWork = TRUE)

# Core source list, in load order. Files that do not exist yet are skipped; later tasks only add
# files (and Task 12 appends R/ui/*.R).
.sr_core_files <- c(
  "core/util", "core/audit", "core/events", "core/project", "core/hardware",
  "core/libsnap", "core/hash", "core/lint", "core/exec", "design/spec", "design/compose",
  "writer/numfmt", "io/sniff", "io/preflight", "io/duck", "io/fingerprint", "io/store",
  "io/read_text", "io/read_columnar", "io/read_sheet", "io/ingest", "io/dictionary", "io/redcap_dict", "io/join",
  "intake/roles"
)

for (.f in file.path(sr_root, "R", paste0(.sr_core_files, ".R"))) {
  if (file.exists(.f)) source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)

# UI modules (Shiny): every file under R/ui, in sorted order, after the core.
for (.f in sort(list.files(file.path(sr_root, "R", "ui"), pattern = "[.][Rr]$", full.names = TRUE))) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)
