# io/duck.R - the locked-down DuckDB connection and the SQL guard that fronts it.
#
# Air-gap rules (see the P1b Global Constraints):
#   * every connection is in-memory, with an empty project-owned extension directory, extension
#     autoinstall/autoload and community extensions off, INSTALL pointed at an empty local
#     repository (so even a raw INSTALL cannot reach the network), then lock_configuration = true;
#   * all application SQL goes through sr_duck_exec() / sr_duck_query(), which run .sr_duck_guard()
#     first: INSTALL / FORCE INSTALL / LOAD / SET / RESET / PRAGMA / ATTACH statements and any
#     'scheme://' string literal are refused (comments are stripped before the check);
#   * the connection's own setup runs through .sr_duck_setup(), which talks to DBI directly and so
#     bypasses the guard. An internal caller that needs a refused statement does the same, with
#     DBI::dbExecute() on the connection (never through the user-facing helpers);
#   * never DuckDB read_json (use jsonlite::stream_in) and never DuckDB's sqlite extension (RSQLite).
#
# Connections are cached per project dir in a package-level environment; sr_duck_close() closes one
# with shutdown = TRUE so no file handle or temp dir outlives the project.

.sr_duck_env <- new.env(parent = emptyenv())

.sr_duck_key <- function(project) {
  normalizePath(project$paths$dir, winslash = "/", mustWork = FALSE)
}

#' The project-owned DuckDB directories under <data>/tmp: temp spill, the (always empty) extension
#' directory, and the (always empty) local "repository" that INSTALL is pointed at.
.sr_duck_dirs <- function(project) {
  tmp <- file.path(sr_project_paths(project$paths$dir)$data, "tmp")
  list(tmp = tmp, ext = file.path(tmp, "duckdb_ext"), repo = file.path(tmp, "duckdb_ext_repo"))
}

# Create the directories and empty the two extension ones: whatever is in there is not ours, and a
# stray *.duckdb_extension would defeat the lockdown.
.sr_duck_prepare_dirs <- function(dirs) {
  for (d in dirs) {
    if (!dir.exists(d) && !isTRUE(dir.create(d, recursive = TRUE, showWarnings = FALSE))) {
      stop("cannot create DuckDB directory: ", d, call. = FALSE)
    }
  }
  for (d in c(dirs$ext, dirs$repo)) {
    stray <- list.files(d, all.files = TRUE, recursive = TRUE, full.names = TRUE, no.. = TRUE)
    if (length(stray)) unlink(stray, recursive = TRUE, force = TRUE)
  }
  invisible(dirs)
}

#' Apply the lockdown to a fresh connection. Internal and unguarded by design: it is the one place
#' that issues SET statements. `dirs` is from .sr_duck_dirs(); `caps` from sr_profile_caps().
.sr_duck_setup <- function(con, dirs, caps) {
  run <- function(sql) DBI::dbExecute(con, sql)
  path <- function(p) DBI::dbQuoteString(con, normalizePath(p, winslash = "/", mustWork = FALSE))
  run(paste0("SET extension_directory = ", path(dirs$ext)))
  run(paste0("SET custom_extension_repository = ", path(dirs$repo)))
  run(paste0("SET autoinstall_extension_repository = ", path(dirs$repo)))
  run("SET autoinstall_known_extensions = false")
  run("SET autoload_known_extensions = false")
  run("SET allow_community_extensions = false")
  run(paste0("SET memory_limit = ", DBI::dbQuoteString(con, paste0(format(caps$duckdb_memory_gb), "GB"))))
  run(sprintf("SET threads = %d", as.integer(caps$duckdb_threads)))
  run(paste0("SET temp_directory = ", path(dirs$tmp)))
  run("SET preserve_insertion_order = false")
  run("SET lock_configuration = true")   # last: nothing above can be changed after this
  got <- DBI::dbGetQuery(con, paste(
    "SELECT current_setting('autoinstall_known_extensions') AS a,",
    "current_setting('autoload_known_extensions') AS b,",
    "current_setting('allow_community_extensions') AS c,",
    "current_setting('lock_configuration') AS l"))
  if (!identical(c(as.logical(got$a), as.logical(got$b), as.logical(got$c), as.logical(got$l)),
                 c(FALSE, FALSE, FALSE, TRUE))) {
    stop("DuckDB lockdown did not take effect", call. = FALSE)
  }
  invisible(con)
}

#' The locked-down in-memory DuckDB connection of a project (cached per project dir).
sr_duck <- function(project) {
  key <- .sr_duck_key(project)
  con <- .sr_duck_env[[key]]
  if (!is.null(con) && DBI::dbIsValid(con)) return(con)
  caps <- sr_profile_caps(project$meta$profile)
  dirs <- .sr_duck_dirs(project)
  .sr_duck_prepare_dirs(dirs)
  con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  ok <- FALSE
  on.exit(if (!ok) try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), add = TRUE)
  .sr_duck_setup(con, dirs, caps)
  ok <- TRUE
  assign(key, con, envir = .sr_duck_env)
  con
}

#' Close and forget a project's DuckDB connection (a no-op when there is none).
sr_duck_close <- function(project) {
  key <- .sr_duck_key(project)
  con <- .sr_duck_env[[key]]
  if (exists(key, envir = .sr_duck_env, inherits = FALSE)) rm(list = key, envir = .sr_duck_env)
  closed <- !is.null(con) && DBI::dbIsValid(con)
  if (closed) DBI::dbDisconnect(con, shutdown = TRUE)
  invisible(closed)
}

# ---- the SQL guard --------------------------------------------------------------------------------

# Scan SQL once, string- and comment-aware: single and double quotes with doubling, E'...' strings
# (backslash escapes, as DuckDB reads them), $tag$ dollar quotes, -- line comments and nested block
# comments. Returns list(ok, stripped, code, statements, code_statements):
#  - `stripped`: the SQL with every comment replaced by one space;
#  - `code`: the same with the inside of every string or quoted identifier blanked to x, so a keyword
#    can only be found where DuckDB would read it as code;
#  - `statements` / `code_statements`: the top-level pieces of both, split at `;`.
# ok = FALSE when a quote, comment or dollar quote is left open (what DuckDB would reject anyway; not
# guessable, so the guard refuses it).
.sr_sql_scan <- function(sql) {
  fail <- list(ok = FALSE, stripped = "", code = "", statements = character(), code_statements = character())
  cp <- utf8ToInt(enc2utf8(sql))
  if (anyNA(cp)) return(fail)
  n <- length(cp)
  res <- integer(n + 1L)       # stripped text
  cod <- integer(n + 1L)       # code-only text (same indexing)
  k <- 0L
  semis <- integer()
  i <- 1L
  is_id_start <- function(x) (x >= 65L & x <= 90L) | (x >= 97L & x <= 122L) | x == 95L
  is_id <- function(x) is_id_start(x) | (x >= 48L & x <= 57L)
  put_literal <- function(from, to) {          # copy a quoted piece; blank it in `cod`
    len <- to - from + 1L
    res[k + seq_len(len)] <<- cp[from:to]
    cod[k + seq_len(len)] <<- 120L
    cod[k + 1L] <<- cp[from]
    cod[k + len] <<- cp[to]
    k <<- k + len
  }
  while (i <= n) {
    ch <- cp[i]
    if (ch == 39L || ch == 34L) {                               # '...' or "..." with doubling
      esc <- ch == 39L && i > 1L && cp[i - 1L] %in% c(69L, 101L) && (i < 3L || !is_id(cp[i - 2L]))
      j <- i + 1L
      closed <- FALSE
      while (j <= n) {
        if (esc && cp[j] == 92L) { j <- j + 2L; next }          # E'..\'..' : backslash escapes
        if (cp[j] == ch) {
          if (j < n && cp[j + 1L] == ch) { j <- j + 2L; next }
          closed <- TRUE
          break
        }
        j <- j + 1L
      }
      if (!closed) return(fail)
      put_literal(i, j)
      i <- j + 1L
    } else if (ch == 45L && i < n && cp[i + 1L] == 45L) {      # -- line comment
      j <- i + 2L
      while (j <= n && cp[j] != 10L) j <- j + 1L
      k <- k + 1L
      res[k] <- 32L
      cod[k] <- 32L
      i <- j
    } else if (ch == 47L && i < n && cp[i + 1L] == 42L) {       # /* block comment, nested */
      depth <- 1L
      j <- i + 2L
      while (j <= n && depth > 0L) {
        if (j < n && cp[j] == 47L && cp[j + 1L] == 42L) { depth <- depth + 1L; j <- j + 2L }
        else if (j < n && cp[j] == 42L && cp[j + 1L] == 47L) { depth <- depth - 1L; j <- j + 2L }
        else j <- j + 1L
      }
      if (depth > 0L) return(fail)
      k <- k + 1L
      res[k] <- 32L
      cod[k] <- 32L
      i <- j
    } else if (ch == 36L) {                                     # $$...$$ or $tag$...$tag$
      j <- i + 1L
      if (j <= n && is_id_start(cp[j])) {
        while (j <= n && is_id(cp[j])) j <- j + 1L
      }
      if (j <= n && cp[j] == 36L) {
        tag <- cp[i:j]
        tl <- length(tag)
        m <- j + 1L
        found <- 0L
        while (m + tl - 1L <= n) {
          if (all(cp[m:(m + tl - 1L)] == tag)) { found <- m; break }
          m <- m + 1L
        }
        if (found == 0L) return(fail)
        put_literal(i, found + tl - 1L)
        i <- found + tl
      } else {
        k <- k + 1L
        res[k] <- ch
        cod[k] <- ch
        i <- i + 1L
      }
    } else {
      k <- k + 1L
      res[k] <- ch
      cod[k] <- ch
      if (ch == 59L) semis <- c(semis, k)
      i <- i + 1L
    }
  }
  stripped <- if (k > 0L) intToUtf8(res[seq_len(k)]) else ""
  code <- if (k > 0L) intToUtf8(cod[seq_len(k)]) else ""
  starts <- c(1L, semis + 1L)
  ends <- c(semis - 1L, k)
  last <- pmax(ends, starts - 1L)
  list(ok = TRUE, stripped = stripped, code = code,
       statements = substring(stripped, starts, last), code_statements = substring(code, starts, last))
}

# Index of the `)` that closes the `(` at position i of the code points cp (quotes honoured), or 0.
.sr_close_paren <- function(cp, i) {
  depth <- 0L
  n <- length(cp)
  while (i <= n) {
    ch <- cp[i]
    if (ch == 39L || ch == 34L) {
      j <- i + 1L
      while (j <= n && cp[j] != ch) j <- j + 1L
      if (j > n) return(0L)
      i <- j
    } else if (ch == 40L) {
      depth <- depth + 1L
    } else if (ch == 41L) {
      depth <- depth - 1L
      if (depth == 0L) return(i)
    }
    i <- i + 1L
  }
  0L
}

# What a statement really runs: peel EXPLAIN / ANALYZE / ANALYSE words and any (...) option list
# that follows them, repeatedly (DuckDB executes the wrapped statement, so EXPLAIN ANALYZE INSTALL is
# an INSTALL). `stmt` is one comment-free statement; returns the remaining text, or NA when an option
# list is unbalanced (not guessable: the caller refuses).
.sr_duck_peel <- function(stmt) {
  s <- sub("^[[:space:]]+", "", stmt)
  repeat {
    m <- regexpr("^(EXPLAIN|ANALYZE|ANALYSE)(?![A-Za-z0-9_])", s, ignore.case = TRUE, perl = TRUE)
    if (m < 0L) return(s)
    s <- sub("^[[:space:]]+", "", substring(s, attr(m, "match.length") + 1L))
    while (startsWith(s, "(")) {
      cp <- utf8ToInt(s)
      close_at <- .sr_close_paren(cp, 1L)
      if (close_at == 0L) return(NA_character_)
      s <- sub("^[[:space:]]+", "", intToUtf8(cp[-seq_len(close_at)]))
    }
  }
}

# A statement that may not run through the application SQL helpers.
.sr_duck_refused_re <- paste0(
  "^(?:FORCE[[:space:]]+INSTALL|INSTALL|LOAD|SET|RESET|PRAGMA|ATTACH)(?![A-Za-z0-9_])")

#' Refuse INSTALL / FORCE INSTALL / LOAD / SET / RESET / PRAGMA / ATTACH statements and any
#' 'scheme://' string literal (single-quoted or dollar-quoted), on the SQL with comments stripped.
#' Errors with "refused: ..."; returns TRUE invisibly otherwise.
#'  - Statements come from the string- and comment-aware scan above (split at top-level `;`), so
#'    free text such as 'Discharged; set for review' is data, and a keyword is found after any
#'    number of comments.
#'  - EXPLAIN / ANALYZE / (options) prefixes are peeled before the keyword test.
#'  - Independently, an INSTALL with a FROM clause anywhere in the code (outside strings and quoted
#'    identifiers) is refused: INSTALL ... FROM <repo> overrides the repository lockdown.
#' Deliberately NOT caught: a URL assembled at run time ('http' || '://x'). That is left to the
#' extension lockdown (project-owned empty extension dir and repository, autoload/autoinstall off,
#' lock_configuration), because httpfs cannot be loaded and so nothing can open such a URL.
.sr_duck_guard <- function(sql) {
  if (!is.character(sql) || length(sql) != 1L || is.na(sql)) {
    stop("sql must be a single string", call. = FALSE)
  }
  sc <- .sr_sql_scan(sql)
  if (!sc$ok) {
    stop("refused: SQL has an unterminated quote, comment or dollar-quoted string", call. = FALSE)
  }
  for (stmt in sc$statements) {
    body <- .sr_duck_peel(stmt)
    if (is.na(body)) {
      stop("refused: unbalanced option list after EXPLAIN", call. = FALSE)
    }
    hit <- regmatches(body, regexpr(.sr_duck_refused_re, body, ignore.case = TRUE, perl = TRUE))
    if (length(hit)) {
      kw <- gsub("[[:space:]]+", " ", toupper(hit))
      kw <- regmatches(kw, regexpr("(FORCE INSTALL|INSTALL|LOAD|SET|RESET|PRAGMA|ATTACH)$", kw))
      stop("refused: ", kw, " statements are not allowed in application SQL", call. = FALSE)
    }
  }
  inst <- "(?<![A-Za-z0-9_])INSTALL(?![A-Za-z0-9_])"
  from <- "(?<![A-Za-z0-9_])FROM(?![A-Za-z0-9_])"
  for (cs in sc$code_statements) {
    m <- regexpr(inst, cs, ignore.case = TRUE, perl = TRUE)
    if (m > 0L && grepl(from, substring(cs, m), ignore.case = TRUE, perl = TRUE)) {
      stop("refused: INSTALL ... FROM is not allowed (it would override the extension repository lockdown)",
           call. = FALSE)
    }
  }
  if (grepl("(?:'|[$](?:[A-Za-z_][A-Za-z0-9_]*)?[$])[A-Za-z][A-Za-z0-9+.-]*://", sc$stripped, perl = TRUE)) {
    stop("refused: a scheme:// string literal is not allowed (paths are never URLs; air-gap)",
         call. = FALSE)
  }
  invisible(TRUE)
}

#' Run one guarded statement; returns DBI::dbExecute's affected-row count.
sr_duck_exec <- function(con, sql) {
  .sr_duck_guard(sql)
  DBI::dbExecute(con, sql)
}

#' Run one guarded query; returns a data.frame.
sr_duck_query <- function(con, sql) {
  .sr_duck_guard(sql)
  DBI::dbGetQuery(con, sql)
}
