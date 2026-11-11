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
  "core/util", "core/audit", "core/events", "core/project", "core/hardware", "core/libsnap",
  "core/hash", "core/lint", "core/exec", "design/spec", "design/compose", "writer/numfmt",
  "io/sniff", "io/preflight", "io/duck", "io/fingerprint", "io/store", "io/read_text",
  "io/read_columnar", "io/read_sheet", "io/ingest", "io/dictionary", "io/redcap_dict", "io/join",
  "intake/roles", "intake/pico", "router/sysone", "router/abstain", "router/graph", "router/state",
  "router/backends/rules", "router/backends/jev", "router/engine",
  "router/nodes/intake", "router/nodes/roles", "router/nodes/analysis",
  "router/shadow", "router/gate",
  "profile/profile", "profile/dq", "profile/dq_conformance", "profile/dq_completeness",
  "profile/dq_plausibility", "profile/dq_structure", "profile/dq_leakage", "profile/recipe",
  "io/cohort", "profile/suggest", "profile/query", "profile/flow", "routes/emit", "routes/result",
  "routes/terms", "sap/sap", "routes/run", "routes/table1", "routes/se", "routes/lm",
  "routes/marginal", "routes/glm", "routes/cox", "routes/rmst", "scale/engine", "scale/stream_extract",
  "impute/pool", "impute/impute",
  "resample/boot", "diagnostics/checks", "diagnostics/plots", "sap/deviations", "sap/feas_engine",
  "sap/feasibility", "resample/coverage", "resample/feas_sim", "writer/placeholders",
  "writer/sdc_out", "writer/fragments", "writer/tables", "writer/figures", "writer/cite",
  "writer/checklist", "writer/model", "writer/guard", "writer/docx", "writer/script",
  "writer/sapdoc", "writer/export", "archive/tar", "archive/freeze", "archive/baseline",
  "archive/srproj_export", "archive/srproj_import", "archive/diff"
)

for (.f in file.path(sr_root, "R", paste0(.sr_core_files, ".R"))) {
  if (file.exists(.f)) source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f)

# UI modules (Shiny): util.R and spec_form.R first (when present), then every other file under R/ui in
# sorted order, never twice, after the core.
.sr_ui_dir <- file.path(sr_root, "R", "ui")
.sr_ui_all <- sort(list.files(.sr_ui_dir, pattern = "[.][Rr]$", full.names = TRUE))
.sr_ui_first <- file.path(.sr_ui_dir, c("util.R", "spec_form.R"))
for (.f in unique(c(.sr_ui_first[file.exists(.sr_ui_first)], .sr_ui_all))) {
  source(.f, local = FALSE, encoding = "UTF-8")
}
rm(.f, .sr_ui_dir, .sr_ui_all, .sr_ui_first)
