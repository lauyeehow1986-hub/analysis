# Fixture helpers for P1b tests: the synthetic cardiac cohort and throwaway projects.
# The generator lives in samples/ (a deliverable in its own right); source it from the repo root.
local({
  root <- normalizePath(testthat::test_path("../.."), winslash = "/")
  source(file.path(root, "samples", "gen_cardiac.R"), local = FALSE)
})

# A fresh, open, writable project in a temp dir that is removed (and the project closed) when the
# calling test (or `env`) finishes.
.fx_project <- function(env = parent.frame()) {
  dir <- file.path(withr::local_tempdir(.local_envir = env), "proj")
  p <- sr_project_create(dir, "fixture", profile = "laptop")
  withr::defer(sr_project_close(p), envir = env)
  p
}

# Write a CSV as UTF-8 without row names; `...` goes to utils::write.csv (e.g. na = "").
.fx_write_csv <- function(df, path, ...) {
  utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8", ...)
  invisible(path)
}

# A project with the synthetic cardiac cohort ingested as table `cardiac`. Ingest exists from P1b
# Task 5 on; until then the project is returned with nothing ingested.
.fx_cardiac_project <- function(n = 2000L, defects = TRUE, env = parent.frame()) {
  p <- .fx_project(env = env)
  s <- sr_sample_cardiac(n = n, defects = defects)
  csv <- file.path(dirname(p$paths$dir), "cardiac.csv")
  .fx_write_csv(s$data, csv)
  if (exists("sr_ingest")) p <- sr_ingest(p, csv, table = "cardiac")
  p
}

# Write one data frame in every format the columnar and database readers take, under `dir`. Returns a
# named character vector of paths: parquet, rds, rdata, ndjson, json, sqlite, duckdb, plus feather when
# the arrow package is installed. `df` must have a column `id`. Nothing here touches a project.
.fx_write_formats <- function(df, dir, name = "fx") {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  f <- function(ext) file.path(dir, paste0(name, ".", ext))
  out <- character()
  # parquet and duckdb through a throwaway in-memory DuckDB (no extension needed)
  cn <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  duckdb::duckdb_register(cn, "fx_df", df)
  DBI::dbExecute(cn, sprintf("COPY (SELECT * FROM fx_df) TO %s (FORMAT parquet)", DBI::dbQuoteString(cn, f("parquet"))))
  DBI::dbDisconnect(cn, shutdown = TRUE)
  out[["parquet"]] <- f("parquet")
  if (requireNamespace("arrow", quietly = TRUE)) {
    arrow::write_feather(df, f("feather"))
    out[["feather"]] <- f("feather")
  }
  saveRDS(df, f("rds"))
  out[["rds"]] <- f("rds")
  fx_df <- df
  save(fx_df, file = f("rdata"))
  out[["rdata"]] <- f("rdata")
  con <- file(f("ndjson"), "wb")
  jsonlite::stream_out(df, con, verbose = FALSE, digits = NA)
  close(con)
  out[["ndjson"]] <- f("ndjson")
  jsonlite::write_json(df, f("json"), digits = NA)
  out[["json"]] <- f("json")
  lite <- DBI::dbConnect(RSQLite::SQLite(), f("sqlite"), extended_types = TRUE)
  DBI::dbWriteTable(lite, "fx", df)
  DBI::dbDisconnect(lite)
  out[["sqlite"]] <- f("sqlite")
  dk <- DBI::dbConnect(duckdb::duckdb(), dbdir = f("duckdb"))
  DBI::dbWriteTable(dk, "fx", df)
  DBI::dbDisconnect(dk, shutdown = TRUE)
  out[["duckdb"]] <- f("duckdb")
  out
}

# Bring a table read back from the store to the types of the reference frame `ref`, column by column,
# so formats that cannot carry a type compare equal: a Date column read as text or a number becomes a
# Date, a logical read as 0/1 becomes logical, an integer read as a double (or the reverse) becomes
# numeric, text stays text. Rows are in the order given.
.fx_norm <- function(got, ref) {
  out <- got[, names(ref), drop = FALSE]
  for (nm in names(ref)) {
    x <- out[[nm]]
    r <- ref[[nm]]
    out[[nm]] <- if (inherits(r, "Date")) {
      if (is.numeric(x)) as.Date(x, origin = "1970-01-01") else as.Date(as.character(x))
    } else if (is.logical(r)) {
      if (is.character(x)) x == "true" | x == "TRUE" else as.logical(x)
    } else if (is.numeric(r)) {
      as.numeric(x)
    } else {
      as.character(x)
    }
  }
  attr(out, "row.names") <- seq_len(nrow(out))
  out
}

# The project with the SAP marked as locked, in memory only: no event is written, so nothing is
# audited and nothing is replayed. For tests that need the unblinded view without drafting a SAP.
.fx_unblind <- function(p) {
  p$state$sap <- list(locked = TRUE)
  p
}

# Route the project so that the router gate of the SAP lock (router/gate.R) passes: set a PICO
# (population "test population", outcome "test outcome"; `pico`, a named list, overrides or extends
# those fields), decide the design (`design`) and the purpose (the route purpose of the first primary
# analysis, "explain-causal" when there is none), reason "test fixture", then run the rules (no backends)
# so rule-decided nodes such as the guideline are decided too. A covariate of a primary analysis that the
# covariate_timing rule marks mediator / post_outcome is decided baseline (override, "test fixture"), so
# an existing fixture is not blocked by the gate. Returns the project.
.fx_routed <- function(p, design = "cohort_retrospective", pico = NULL, decide = NULL) {
  q <- utils::modifyList(list(population = "test population", outcome = "test outcome"), as.list(pico))
  p <- sr_pico_set(p, .list = q)
  an <- .sr_sap_analyses(p)
  prim <- Filter(function(a) identical(a$label, "primary"), an[order(names(an), method = "radix")])
  purpose <- if (length(prim)) {
    sr_routes()$purpose[match(prim[[1L]]$route, sr_routes()$id)]
  } else {
    "explain-causal"
  }
  if (is.na(purpose)) purpose <- "explain-causal"
  p <- sr_router_decide(p, "design", design, reason = "test fixture")
  p <- sr_router_decide(p, "purpose", purpose, reason = "test fixture")
  # extra user decisions (list(node, choice, table, column, override, reason)), before the rules run
  for (d in decide) {
    p <- sr_router_decide(p, d$node, d$choice, reason = d$reason %||% "test fixture", table = d$table %||% NA_character_,
                          column = d$column %||% NA_character_, override = isTRUE(d$override))
  }
  p <- sr_router_run(p)
  adj <- unique(unlist(lapply(prim, function(a) lapply(a$spec$terms, function(t) paste(a$table, t$var, sep = "\r")))))
  tab <- sr_router_table(p)
  tab <- tab[tab$node == "covariate_timing" & tab$status == "decided" &
               tab$choice %in% c("mediator", "post_outcome"), , drop = FALSE]
  for (i in seq_len(nrow(tab))) {
    if (!paste(tab$table[i], tab$column[i], sep = "\r") %in% adj) next
    p <- sr_router_decide(p, "covariate_timing", "baseline", reason = "test fixture", table = tab$table[i],
                          column = tab$column[i], override = TRUE)
  }
  p
}

# Assess and decide every pre-specified primary analysis (no delta, reason "test fixture"), so that the
# SAP lock gate (sap/feasibility.R) passes: p <- sr_sap_lock(.fx_feasible(p)). A clustered analysis is
# given a test ICC (the engine refuses it without one). When `routed` is TRUE (the default), .fx_routed(p,
# ...) runs FIRST (the router gate of the lock needs a decided design and purpose); `...` goes to it.
# Returns the project.
.fx_feasible <- function(p, decision = "proceed", routed = TRUE, ...) {
  if (isTRUE(routed)) p <- .fx_routed(p, ...)
  an <- .sr_sap_analyses(p)
  for (id in sort(as.character(names(an)), method = "radix")) {
    a <- an[[id]]
    if (!identical(a$label, "primary")) next
    assumptions <- if (isTRUE(a$spec$layers$clustered)) {
      list(icc = list(value = 0.05, justification = "test fixture"))
    } else {
      list()
    }
    p <- sr_feasibility_assess(p, id, assumptions = assumptions)
    p <- sr_feasibility_decide(p, id, decision, reason = "test fixture")
  }
  p
}

# A project whose table `table` (from `df`) is analysis-ready: ingested, roles confirmed, the DQ
# checks run, and every warn / block finding decided with the action the suggestor proposes -- except
# keep_sensitivity, which would demand a sensitivity analysis and is decided as recheck instead.
# Stops, printing the readiness table, if any readiness row is still block. The project stays blind.
.fx_ready_project <- function(df, table = "t", roles, env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = table,
                 options = list(drive_info = function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())))
  p <- sr_roles_set(p, table, roles)
  p <- sr_dq_run(p, table)
  sg <- sr_suggest(p, table)
  for (i in seq_len(nrow(sg))) {
    if (identical(sg$action[i], "keep_sensitivity")) {
      p <- sr_action_decide(p, sg$finding_id[i], "recheck", reason = "test fixture", table = table)
    } else {
      p <- sr_action_decide(p, sg$finding_id[i], sg$action[i], table = table)
    }
  }
  rd <- sr_readiness(p, table)
  if (any(rd$status == "block")) {
    print(rd)
    stop(".fx_ready_project: the table is still not ready", call. = FALSE)
  }
  p
}

# One 512-byte ustar header (raw) for hand-built tar fixtures; Task 5 reuses it.
.fx_tar_header <- function(name, type = "0", size = 0, linkname = "") {
  h <- raw(512)
  oct <- function(x, w) {
    s <- as.character(as.octmode(as.integer(x)))
    paste0(strrep("0", w - nchar(s)), s)
  }
  put <- function(h, off, s, len) {
    b <- charToRaw(s)
    stopifnot(length(b) <= len)
    h[off + seq_along(b)] <- b
    h
  }
  h <- put(h, 0, name, 100)
  h <- put(h, 100, "0000644", 7)
  h <- put(h, 108, "0000000", 7)
  h <- put(h, 116, "0000000", 7)
  h <- put(h, 124, oct(size, 11), 11)
  h <- put(h, 136, "00000000000", 11)
  h <- put(h, 156, type, 1)
  h <- put(h, 157, linkname, 100)
  h <- put(h, 257, "ustar", 5)
  h <- put(h, 263, "00", 2)
  h[149:156] <- as.raw(32L)
  h[157] <- charToRaw(type)
  ck <- sum(as.integer(h))
  h <- put(h, 148, oct(ck, 6), 6)
  h[148 + 7] <- as.raw(0L)
  h[148 + 8] <- as.raw(32L)
  h
}
