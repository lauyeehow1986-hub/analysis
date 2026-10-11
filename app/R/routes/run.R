# routes/run.R - the run engine: a hashed, byte-stable column extract and the locked, cached run.
#
# An analysis of the locked SAP runs on an EXTRACT: the columns its spec reads, materialised once from
# the table's Parquet parts with ORDER BY every column (the connection does not preserve insertion
# order, so the sort is what makes the bytes repeatable), written as serialize(version = 3) bytes to
# data/extracts/<sha>.rds. The extract is addressed by the SHA-256 of those bytes. The emitted code
# starts with a fixed prologue that reads the extract from file.path(sr_project_dir, <rel>), refuses
# to go on unless the bytes still hash to <sha> ("extract changed"), and binds sr_data. Everything in
# the result cache key is therefore content: the extract hash, the route and version, the spec and
# the seed (plus the library snapshot and hardware that sr_content_hash adds). The project folder path
# reaches the worker as an input but is not part of the cache identity, so a project copied to another
# drive reruns from its cache. No event holds an absolute path.
#
# State: state$runs[[analysis]] = list(analysis, key, extract_sha, ok, cached, error, elapsed,
#        lib_changed, ok_key); run.completed replaces it, and ok_key is the key of the latest ok run
#        (kept across a later failed run), the result sr_result() shows.

# ---- the spec as the state holds it (ruling R14) ----------------------------------------------------

#' A spec as events.jsonl hands it back (jsonlite, simplifyVector = FALSE: every array is a list, null
#' is NULL) turned back into the shape the validator and the routes read. An unnamed list whose
#' elements are all length-1 atomics of one type (integers and doubles count as one) becomes an
#' atomic vector; every other list stays a list, and its elements are normalised the same way, so
#' `terms` and `interactions` stay lists of lists while each inner array becomes a vector. Whole
#' numbers come back as integers (JSON cannot tell 40 from 40L); the term builder coerces them.
.sr_spec_from_state <- function(x) {
  if (!is.list(x) || is.data.frame(x)) return(x)
  if (is.null(names(x)) && length(x)) {
    scalar <- vapply(x, function(e) is.atomic(e) && !is.null(e) && length(e) == 1L && is.null(names(e)), NA)
    if (all(scalar)) {
      types <- unique(vapply(x, function(e) if (is.numeric(e)) "num" else typeof(e), ""))
      if (length(types) == 1L) return(unlist(x, use.names = FALSE))
    }
  }
  out <- lapply(x, .sr_spec_from_state)
  names(out) <- names(x)
  out
}

# ---- the reducer -----------------------------------------------------------------------------------

sr_reducer_register("run.completed", function(state, event) {
  p <- event$payload
  id <- p$analysis
  if (!is.character(id) || length(id) != 1L || is.na(id) || !nzchar(id)) {
    stop("run.completed needs an analysis id", call. = FALSE)
  }
  runs <- state$runs %||% list()
  ok <- isTRUE(p$ok)
  key <- .sr_sap_chr1(p$key)
  elapsed <- if (is.null(p$elapsed) || is.na(p$elapsed[[1L]])) NA_real_ else as.numeric(p$elapsed[[1L]])
  runs[[id]] <- list(analysis = id, key = key, extract_sha = .sr_sap_chr1(p$extract_sha), ok = ok,
                     cached = isTRUE(p$cached), error = .sr_sap_chr1(p$error), elapsed = elapsed,
                     lib_changed = isTRUE(p$lib_changed),
                     ok_key = if (ok) key else (runs[[id]]$ok_key %||% NA_character_))
  state$runs <- runs
  state
})

# ---- the extract -----------------------------------------------------------------------------------

#' Materialise `cols` of `table` as a data.frame with the store's types, rows sorted by every column,
#' serialised (version 3) to data/extracts/<sha>.rds under the project, `sha` being the SHA-256 of the
#' bytes. An existing file of that name is reused as it is: whether it still holds those bytes is
#' what the run's prologue checks. Returns list(rel, sha, n, cols); `rel` is relative to the project
#' folder.
sr_extract <- function(project, table, cols) {
  .sr_store_check_table(table)
  if (!is.character(cols) || !length(cols) || anyNA(cols) || any(!nzchar(cols))) {
    stop("cols must be a character vector naming at least one column", call. = FALSE)
  }
  cols <- unique(cols)
  if (!(table %in% sr_store_tables(project))) {
    stop("table '", table, "' is not in the project store", call. = FALSE)
  }
  have <- vapply(sr_store_manifest(project, table)$schema, function(s) s$name, "")
  unknown <- setdiff(cols, have)
  if (length(unknown)) {
    stop("unknown column(s) for table '", table, "': ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  sr_store_view(project, table)
  con <- sr_duck(project)
  q <- DBI::dbQuoteIdentifier(con, cols)
  df <- sr_duck_query(con, sprintf("SELECT %s FROM %s ORDER BY %s", paste(q, collapse = ", "),
                                   DBI::dbQuoteIdentifier(con, table), paste(q, collapse = ", ")))
  bytes <- serialize(df, NULL, version = 3)
  sha <- .sr_sha256(bytes)
  rel <- paste0("data/extracts/", sha, ".rds")
  path <- file.path(project$paths$dir, rel)
  if (!file.exists(path)) {
    if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
    .sr_write_atomic(path, bytes)
    if (!file.exists(path)) stop("could not write the extract", call. = FALSE)
  }
  list(rel = rel, sha = sha, n = nrow(df), cols = cols)
}

#' The fixed code that opens a run: read the extract at file.path(sr_project_dir, <rel>), stop with
#' "extract changed" unless its bytes hash to <sha>, and bind `sr_data`. `sr_project_dir` is an input
#' of the worker (and a variable the exported script defines). The text holds no absolute path.
.sr_run_prologue <- function(ex) {
  sha <- ex$sha
  if (!is.character(sha) || length(sha) != 1L || !grepl("^[0-9a-f]{64}$", sha)) {
    stop("the extract needs a 64-character lowercase hex sha", call. = FALSE)
  }
  if (!identical(ex$rel, paste0("data/extracts/", sha, ".rds"))) {
    stop("the extract path must be data/extracts/<sha>.rds", call. = FALSE)
  }
  code <- paste(c(
    sprintf(".sr_xf <- file.path(sr_project_dir, %s)", .sr_lit(ex$rel)),
    ".sr_xb <- readBin(.sr_xf, \"raw\", n = file.size(.sr_xf))",
    sprintf("if (!identical(paste(as.character(unclass(openssl::sha256(.sr_xb))), collapse = \"\"), %s)) stop(\"extract changed\")",
            .sr_lit(sha)),
    "sr_data <- unserialize(.sr_xb)",
    "rm(.sr_xf, .sr_xb)"), collapse = "\n")
  .sr_emit_check(code, "analysis", prologue = TRUE)
  code
}

# ---- the run ---------------------------------------------------------------------------------------

# The message with the project folder (either slash style) replaced, so no event holds a user path.
.sr_run_scrub <- function(msg, dir) {
  msg <- paste(msg, collapse = " ")
  for (d in unique(c(dir, gsub("/", "\\", dir, fixed = TRUE)))) {
    if (nzchar(d)) msg <- gsub(d, "<project>", msg, fixed = TRUE)
  }
  if (nchar(msg) > 2000L) msg <- paste0(substr(msg, 1L, 2000L), "...")
  msg
}

#' Run one analysis of the locked SAP and record run.completed. Refused (nothing is written) when the
#' project is read-only, the analysis is unknown, the SAP is not locked, the table changed after the
#' lock, the spec or layer stack no longer passes, the analysis does not fit in memory or its compiled
#' code is rejected. Otherwise the analysis runs in the isolated worker behind the fits cache, and a
#' failed fit (an error, a result that is not plain data, a presentation that breaks the result
#' contract) is recorded with ok = FALSE and the project returned. `lib_changed` says that the library
#' snapshot differs from the one recorded at the lock.
#' @param timeout seconds the worker may run
#' @return the project (run.completed).
sr_run <- function(project, analysis_id, timeout = 3600) {
  if (isTRUE(project$readonly)) stop("project is read-only", call. = FALSE)
  .sr_sap_id(analysis_id, "analysis_id")
  a <- project$state$analyses[[analysis_id]]
  if (is.null(a)) stop("unknown analysis ", .sr_sap_q(analysis_id), call. = FALSE)
  if (sr_blind(project)) stop("the SAP is not locked", call. = FALSE)

  lock <- project$state$sap$lock
  locked_hash <- lock$tables[[a$table]]$store_hash
  if (is.null(locked_hash)) {
    stop("table '", a$table, "' was not part of the SAP lock", call. = FALSE)
  }
  if (!identical(sr_store_hash(sr_store_manifest(project, a$table)), locked_hash)) {
    stop("table '", a$table, "' changed after the SAP lock", call. = FALSE)
  }

  route <- sr_route_get(a$route, a$route_version)
  spec <- .sr_spec_from_state(a$spec)
  v <- sr_spec_validate(spec, route)
  if (!v$ok) {
    stop("the spec of ", .sr_sap_q(analysis_id), " is not valid for route ", .sr_sap_q(a$route), ": ",
         paste(v$errors, collapse = "; "), call. = FALSE)
  }
  cc <- sr_compose_check(spec)
  if (!cc$ok) {
    stop("the layer combination of ", .sr_sap_q(analysis_id), " is not allowed: ",
         paste(c(cc$refused, cc$unmet, names(cc$choices)), collapse = ", "), call. = FALSE)
  }
  if (is.null(spec$seed)) stop("the spec of ", .sr_sap_q(analysis_id), " has no seed", call. = FALSE)
  .sr_check_seed(spec$seed)
  cols <- sr_spec_cols(spec)
  if (!length(cols)) stop("the spec of ", .sr_sap_q(analysis_id), " reads no data column", call. = FALSE)

  est <- sr_mem_estimate(project, a$table, cols, .sr_mem_method(a$route))
  if (!isTRUE(est$fits)) {
    stop("analysis ", .sr_sap_q(analysis_id), " does not fit in memory: ", est$reason, call. = FALSE)
  }

  body <- sr_compile(route, spec)
  .sr_emit_check(body, "analysis")
  ex <- sr_extract(project, a$table, cols)
  route_key <- .sr_route_key(a$route, a$route_version)
  key <- sr_content_hash(ex$sha, list(route = route_key, spec = spec), spec$seed)

  res <- sr_exec_cached(project, key, paste(.sr_run_prologue(ex), body, sep = "\n"),
                        inputs = list(sr_project_dir = project$paths$dir), seed = spec$seed,
                        cache_inputs = list(extract = ex$sha), timeout = timeout)
  ok <- isTRUE(res$ok)
  err <- if (ok) NA_character_ else .sr_run_scrub(res$error, project$paths$dir)
  if (ok) {
    bad <- tryCatch({
      if (!.sr_value_plain(res$value)) stop("the result is not plain data", call. = FALSE)
      sr_result_check(route$present(res$value, spec), route)
      NULL
    }, error = function(e) .sr_run_scrub(conditionMessage(e), project$paths$dir))
    if (!is.null(bad)) {
      ok <- FALSE
      err <- bad
      unlink(file.path(project$paths$fits, paste0(key, ".rds")))   # a rejected value is not kept
    }
  }
  sr_project_append(project, "run.completed", list(
    analysis = analysis_id, key = key, extract_sha = ex$sha, ok = ok, cached = isTRUE(res$cached),
    error = err, elapsed = res$elapsed, lib_changed = !identical(sr_libsnap_hash(), lock$libsnap_hash)))
}

#' The presented result (checked against the result contract) of the latest ok run of an analysis,
#' rebuilt from the cached value in fits/. Errors when the analysis has not run successfully, or when
#' its cached value is gone (run it again).
sr_result <- function(project, analysis_id) {
  .sr_sap_id(analysis_id, "analysis_id")
  a <- project$state$analyses[[analysis_id]]
  if (is.null(a)) stop("unknown analysis ", .sr_sap_q(analysis_id), call. = FALSE)
  r <- project$state$runs[[analysis_id]]
  if (is.null(r) || is.null(r$ok_key) || is.na(r$ok_key)) {
    stop("analysis ", .sr_sap_q(analysis_id), " has not run successfully", call. = FALSE)
  }
  stored <- tryCatch(suppressWarnings(readRDS(file.path(project$paths$fits, paste0(r$ok_key, ".rds")))),
                     error = function(e) NULL)
  if (!is.list(stored) || !isTRUE(stored$ok)) {
    stop("the stored result of analysis ", .sr_sap_q(analysis_id), " is missing; run it again", call. = FALSE)
  }
  route <- sr_route_get(a$route, a$route_version)
  sr_result_check(route$present(stored$value, .sr_spec_from_state(a$spec)), route)
}
