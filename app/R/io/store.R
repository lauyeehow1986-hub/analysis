# io/store.R - the Parquet store: <data>/store/<table>/part-NNNN.parquet plus manifest.json.
#
# These are pure file and view operations. Parts are written by the ingest code (later tasks), which
# also builds the manifest; nothing here records events. A manifest is
#   list(table, parts = list(list(file, rows, sha256)), schema = list(list(name, type)), rows)
# and sr_store_hash() addresses a table's content by its schema and its parts' SHA-256s, in order.

.sr_table_name_re <- "^[a-z][a-z0-9_]{0,62}$"

.sr_store_check_table <- function(table) {
  if (!is.character(table) || length(table) != 1L || is.na(table) || !grepl(.sr_table_name_re, table)) {
    stop("invalid table name (must match ^[a-z][a-z0-9_]{0,62}$): ", paste(format(table), collapse = ","),
         call. = FALSE)
  }
  invisible(table)
}

# The store directory of a table (not created).
sr_store_dir <- function(project, table) {
  .sr_store_check_table(table)
  file.path(sr_project_paths(project$paths$dir)$data, "store", table)
}

.sr_store_manifest_path <- function(project, table) file.path(sr_store_dir(project, table), "manifest.json")

# Normalise a manifest to plain, typed fields and check it. Part files must be plain base names so
# a tampered manifest cannot point a view outside the table's directory.
.sr_store_norm_manifest <- function(m, table) {
  bad <- function(why) stop("invalid manifest: ", why, call. = FALSE)
  if (!is.list(m) || !is.list(m$parts) || !is.list(m$schema)) bad("needs table, parts, schema and rows")
  if (!is.null(m$table) && !identical(as.character(m$table), table)) bad("table does not match")
  if (length(m$rows) != 1L || !is.numeric(m$rows) || is.na(m$rows)) bad("rows must be a number")
  parts <- lapply(m$parts, function(p) {
    f <- p$file
    if (!is.character(f) || length(f) != 1L || is.na(f) || !grepl("^[A-Za-z0-9][A-Za-z0-9._-]*$", f)) {
      bad("part file must be a plain file name")
    }
    if (!is.character(p$sha256) || length(p$sha256) != 1L || !grepl("^[0-9a-f]{64}$", p$sha256)) {
      bad("part sha256 must be 64 hex digits")
    }
    if (length(p$rows) != 1L || !is.numeric(p$rows) || is.na(p$rows)) bad("part rows must be a number")
    list(file = f, rows = as.numeric(p$rows), sha256 = p$sha256)
  })
  schema <- lapply(m$schema, function(s) {
    if (!is.character(s$name) || length(s$name) != 1L || is.na(s$name) ||
        !is.character(s$type) || length(s$type) != 1L || is.na(s$type)) bad("schema entries need name and type")
    list(name = s$name, type = s$type)
  })
  list(table = table, parts = unname(parts), schema = unname(schema), rows = as.numeric(m$rows))
}

# Write a table's manifest atomically (creating its store directory).
sr_store_write_manifest <- function(project, table, manifest) {
  m <- .sr_store_norm_manifest(manifest, table)
  dir <- sr_store_dir(project, table)
  if (!dir.exists(dir) && !isTRUE(dir.create(dir, recursive = TRUE, showWarnings = FALSE))) {
    stop("cannot create store directory for table ", table, call. = FALSE)
  }
  .sr_write_json_file(file.path(dir, "manifest.json"), m)
  invisible(m)
}

# Read a table's manifest.
sr_store_manifest <- function(project, table) {
  path <- .sr_store_manifest_path(project, table)
  if (!file.exists(path)) stop("no manifest for table '", table, "'", call. = FALSE)
  .sr_store_norm_manifest(.sr_read_json_file(path), table)
}

# Content hash of a manifest: SHA-256 of the canonical JSON of its schema and its parts' SHA-256s
# in order. Row counts and file names are deliberately not in it.
sr_store_hash <- function(manifest) {
  schema <- lapply(manifest$schema, function(s) list(name = s$name, type = s$type))
  shas <- lapply(manifest$parts, function(p) p$sha256)
  .sr_sha256(sr_canonical_json(list(schema = unname(schema), parts = unname(shas))))
}

# Create or replace the DuckDB view "<table>" over read_parquet() of the table's parts; returns the
# table name.
sr_store_view <- function(project, table) {
  m <- sr_store_manifest(project, table)
  if (!length(m$parts)) stop("table '", table, "' has no parts", call. = FALSE)
  dir <- sr_store_dir(project, table)
  files <- file.path(dir, vapply(m$parts, function(p) p$file, character(1)))
  gone <- !file.exists(files)
  if (any(gone)) {
    stop("missing part file(s) for table '", table, "': ", paste(basename(files[gone]), collapse = ", "),
         call. = FALSE)
  }
  con <- sr_duck(project)
  lits <- DBI::dbQuoteString(con, normalizePath(files, winslash = "/", mustWork = FALSE))
  sr_duck_exec(con, sprintf("CREATE OR REPLACE VIEW %s AS SELECT * FROM read_parquet([%s])",
                            DBI::dbQuoteIdentifier(con, table), paste(lits, collapse = ", ")))
  table
}

# Names of the tables in the store (those with a manifest), sorted.
sr_store_tables <- function(project) {
  root <- file.path(sr_project_paths(project$paths$dir)$data, "store")
  if (!dir.exists(root)) return(character())
  d <- list.dirs(root, full.names = FALSE, recursive = FALSE)
  d <- d[grepl(.sr_table_name_re, d) & file.exists(file.path(root, d, "manifest.json"))]
  sort(d, method = "radix")
}

.sr_whole_number <- function(x, lo, hi = Inf) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == floor(x) && x >= lo && x <= hi
}

# One page of a table, paged server-side. limit is at most 1000. order_by is a character vector of
# existing column names (quoted, never pasted) and desc a logical for them; without order_by the row
# order is unspecified (the connection does not preserve insertion order), so callers that page
# must pass order_by.
sr_table_page <- function(project, table, offset, limit, order_by = NULL, desc = FALSE) {
  .sr_store_check_table(table)
  if (!.sr_whole_number(offset, 0)) stop("offset must be a non-negative whole number", call. = FALSE)
  if (!.sr_whole_number(limit, 0, 1000)) stop("limit must be a whole number between 0 and 1000", call. = FALSE)
  if (!is.logical(desc) || anyNA(desc) || !(length(desc) %in% c(1L, length(order_by)))) {
    stop("desc must be TRUE/FALSE, one per order_by column", call. = FALSE)
  }
  if (file.exists(.sr_store_manifest_path(project, table))) {
    sr_store_view(project, table)
    con <- sr_duck(project)
  } else {
    con <- sr_duck(project)
    if (!DBI::dbExistsTable(con, table)) stop("no manifest for table '", table, "'", call. = FALSE)
  }
  ord <- ""
  if (!is.null(order_by)) {
    cols <- DBI::dbListFields(con, table)
    if (!is.character(order_by) || anyNA(order_by) || !length(order_by) || !all(order_by %in% cols)) {
      stop("order_by must name existing columns of '", table, "'", call. = FALSE)
    }
    dirn <- ifelse(rep_len(desc, length(order_by)), " DESC", "")
    ord <- paste0(" ORDER BY ", paste0(DBI::dbQuoteIdentifier(con, order_by), dirn, collapse = ", "))
  }
  sr_duck_query(con, sprintf("SELECT * FROM %s%s LIMIT %.0f OFFSET %.0f",
                             DBI::dbQuoteIdentifier(con, table), ord, limit, offset))
}
