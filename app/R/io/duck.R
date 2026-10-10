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

# The ONE quote/comment parser of the guard. Scans SQL once, string- and comment-aware, the way
# DuckDB's lexer reads it: single and double quotes with doubling, $tag$ dollar quotes (tags may hold
# non-ASCII letters), -- line comments (ended by LF or CR), nested block comments. Returns
# list(ok, why, stripped, code, statements, code_statements):
#  - `stripped`: the SQL with every comment replaced by one space;
#  - `code`: the same with the inside of every string or quoted identifier blanked to x, so a keyword
#    or parenthesis can only be found where DuckDB would read it as code;
#  - `statements` / `code_statements`: the top-level pieces of both, split at `;`.
# ok = FALSE (with `why`) when the text cannot be read with certainty, and the guard then refuses:
#  - an open quote, comment or dollar quote;
#  - an E'...' string whose body contains a backslash. DuckDB reads backslash escapes there, so the
#    end of the string is not the first unpaired quote. Without a backslash the E and plain parses are
#    identical, so no identifier-character model is needed (fail closed);
#  - a $tag$ / $$ right after an identifier character (letter, digit, _, $, non-ASCII): DuckDB reads
#    that $ as part of the identifier, not as the start of a dollar quote.
.sr_sql_scan <- function(sql) {
  bad <- function(why) {
    list(ok = FALSE, why = why, stripped = "", code = "", statements = character(),
         code_statements = character())
  }
  cp <- utf8ToInt(enc2utf8(sql))
  if (anyNA(cp)) return(bad("it is not valid text"))
  n <- length(cp)
  res <- integer(n + 1L)       # stripped text
  cod <- integer(n + 1L)       # code-only text (same indexing)
  k <- 0L
  semis <- integer()
  i <- 1L
  # DuckDB (like PostgreSQL) counts every non-ASCII character as a letter in identifiers and tags
  is_letter <- function(x) (x >= 65L & x <= 90L) | (x >= 97L & x <= 122L) | x == 95L | x >= 128L
  is_id <- function(x) is_letter(x) | (x >= 48L & x <= 57L) | x == 36L
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
      j <- i + 1L
      closed <- FALSE
      while (j <= n) {
        if (cp[j] == ch) {
          if (j < n && cp[j + 1L] == ch) { j <- j + 2L; next }
          closed <- TRUE
          break
        }
        j <- j + 1L
      }
      if (!closed) return(bad("a quote is left open"))
      if (ch == 39L && i > 1L && cp[i - 1L] %in% c(69L, 101L) && j > i + 1L && any(cp[(i + 1L):(j - 1L)] == 92L)) {
        return(bad("an E'...' string contains a backslash escape"))
      }
      put_literal(i, j)
      i <- j + 1L
    } else if (ch == 45L && i < n && cp[i + 1L] == 45L) {      # -- line comment (ends at LF or CR)
      j <- i + 2L
      while (j <= n && cp[j] != 10L && cp[j] != 13L) j <- j + 1L
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
      if (depth > 0L) return(bad("a block comment is left open"))
      k <- k + 1L
      res[k] <- 32L
      cod[k] <- 32L
      i <- j
    } else if (ch == 36L) {                                     # $$...$$ or $tag$...$tag$
      j <- i + 1L
      if (j <= n && is_letter(cp[j])) {
        while (j <= n && is_id(cp[j]) && cp[j] != 36L) j <- j + 1L
      }
      if (j <= n && cp[j] == 36L) {
        if (i > 1L && is_id(cp[i - 1L])) {
          return(bad("a dollar sign follows an identifier character"))
        }
        tag <- cp[i:j]
        tl <- length(tag)
        m <- j + 1L
        found <- 0L
        while (m + tl - 1L <= n) {
          if (all(cp[m:(m + tl - 1L)] == tag)) { found <- m; break }
          m <- m + 1L
        }
        if (found == 0L) return(bad("a dollar-quoted string is left open"))
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
  list(ok = TRUE, why = "", stripped = stripped, code = code,
       statements = substring(stripped, starts, last), code_statements = substring(code, starts, last))
}

# What a statement really runs, read on the scanner's string-blanked code (every literal is already
# gone, so plain parenthesis counting is exact): peel EXPLAIN / ANALYZE / ANALYSE words and any
# (...) option list that follows them, repeatedly (DuckDB executes the wrapped statement, so
# EXPLAIN ANALYZE INSTALL is an INSTALL). Returns the remaining text, or NA when an option list is
# unbalanced (not guessable: the caller refuses).
.sr_duck_peel <- function(code_stmt) {
  s <- sub("^[[:space:]]+", "", code_stmt)
  repeat {
    m <- regexpr("^(EXPLAIN|ANALYZE|ANALYSE)(?![A-Za-z0-9_])", s, ignore.case = TRUE, perl = TRUE)
    if (m < 0L) return(s)
    s <- sub("^[[:space:]]+", "", substring(s, attr(m, "match.length") + 1L))
    while (startsWith(s, "(")) {
      cp <- utf8ToInt(s)
      depth <- cumsum((cp == 40L) - (cp == 41L))
      close_at <- which(depth == 0L)[1L]
      if (is.na(close_at)) return(NA_character_)
      s <- sub("^[[:space:]]+", "", intToUtf8(cp[-seq_len(close_at)]))
    }
  }
}

# The only statements the application may run through sr_duck_exec / sr_duck_query: the first keyword
# of each statement (after peeling EXPLAIN / ANALYZE) must be one of these.
.sr_duck_allowed <- c("SELECT", "WITH", "FROM", "VALUES", "TABLE", "CREATE", "INSERT", "UPDATE", "DELETE",
                      "DROP", "ALTER", "COPY", "DESCRIBE", "SHOW", "SUMMARIZE", "PIVOT", "UNPIVOT",
                      "BEGIN", "COMMIT", "ROLLBACK", "CHECKPOINT")

#' Allow a statement only if its first keyword is on the allowlist above (EXPLAIN / ANALYZE /
#' ANALYSE and their option lists are peeled and the remainder re-checked). Everything else is
#' refused: INSTALL, LOAD, FORCE, ATTACH, DETACH, SET, RESET, PRAGMA, CALL, EXPORT, IMPORT, USE, an
#' unknown word, anything not starting with a word. Errors with "refused: ..."; returns TRUE
#' invisibly otherwise.
#'  - Statements come from the single string- and comment-aware scan above, split at top-level `;`
#'    (stray empty pieces from `;;` or a trailing `;` are no-ops; SQL with no statement at all is
#'    refused), so free text such as 'Discharged; set for review' is data.
#'  - Extra layers: an INSTALL with a FROM clause anywhere in the code (outside strings and quoted
#'    identifiers) is refused, because INSTALL ... FROM <repo> overrides the repository lockdown; and
#'    any 'scheme://' literal (single-quoted or dollar-quoted) is refused.
#' Deliberately NOT caught: a URL assembled at run time ('http' || '://x'). That is left to the
#' extension lockdown (project-owned empty extension dir and repository, autoload/autoinstall off,
#' lock_configuration), because httpfs cannot be loaded and so nothing can open such a URL.
.sr_duck_guard <- function(sql) {
  if (!is.character(sql) || length(sql) != 1L || is.na(sql)) {
    stop("sql must be a single string", call. = FALSE)
  }
  sc <- .sr_sql_scan(sql)
  if (!sc$ok) stop("refused: SQL cannot be read with certainty (", sc$why, ")", call. = FALSE)
  seen <- 0L
  for (cs in sc$code_statements) {
    if (!nzchar(trimws(cs))) next
    seen <- seen + 1L
    body <- .sr_duck_peel(cs)
    if (is.na(body)) stop("refused: unbalanced option list after EXPLAIN", call. = FALSE)
    word <- regmatches(body, regexpr("^[A-Za-z]+(?![A-Za-z0-9_$])", body, perl = TRUE))
    if (!length(word) || !(toupper(word) %in% .sr_duck_allowed)) {
      stop("refused: ", if (length(word)) toupper(word) else "this kind of",
           " statement is not allowed in application SQL (allowlist: ",
           paste(.sr_duck_allowed, collapse = " "), ")", call. = FALSE)
    }
    # UPDATE is allowed for tables, but UPDATE EXTENSIONS reaches the extension repository
    if (grepl("^UPDATE[[:space:]]+EXTENSIONS(?![A-Za-z0-9_$])", body, ignore.case = TRUE, perl = TRUE)) {
      stop("refused: UPDATE EXTENSIONS is not allowed (extensions are locked down; air-gap)", call. = FALSE)
    }
  }
  if (seen == 0L) stop("refused: no SQL statement", call. = FALSE)
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

# ---- attaching a user's DuckDB file (read-only source) ----------------------------------------------

#' The ONLY ATTACH in the application. A DuckDB file that is a data source is attached READ_ONLY under
#' a random alias so it can be read with COPY; the caller always DETACHes (on.exit). It talks to DBI
#' directly, as the connection's setup does: ATTACH and DETACH stay off the allowlist of
#' sr_duck_exec() / sr_duck_query(), so user SQL can never reach them. The path and the alias are
#' quoted, never pasted. Returns the alias.
.sr_duck_attach <- function(con, path, alias = paste0("sr_src_", paste(as.character(openssl::rand_bytes(6L)), collapse = ""))) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("path must be a single non-empty string", call. = FALSE)
  }
  if (grepl(.sr_sniff_url_re, path)) stop("refusing to attach a URL: only local files can be read", call. = FALSE)
  if (!file.exists(path) || dir.exists(path)) stop("file not found", call. = FALSE)
  if (!is.character(alias) || length(alias) != 1L || !grepl("^sr_src_[0-9a-f]{12}$", alias)) {
    stop("invalid alias", call. = FALSE)
  }
  f <- normalizePath(path, winslash = "/", mustWork = TRUE)
  DBI::dbExecute(con, sprintf("ATTACH %s AS %s (READ_ONLY)", DBI::dbQuoteString(con, f),
                              DBI::dbQuoteIdentifier(con, alias)))
  alias
}

#' Detach an alias made by .sr_duck_attach (never an error: it runs in on.exit).
.sr_duck_detach <- function(con, alias) {
  try(DBI::dbExecute(con, paste("DETACH", DBI::dbQuoteIdentifier(con, alias))), silent = TRUE)
  invisible(alias)
}
