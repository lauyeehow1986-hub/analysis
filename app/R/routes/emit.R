# routes/emit.R - building and vetting emitted analysis code.
#
# A route's compile() returns an R code string. User-controlled text (column names, labels, levels)
# reaches that string only through .sr_q() (names) and .sr_lit() (literals), never by pasting. Shared
# helpers are inlined with .sr_inline() so the exported analysis.R runs without StatRouter. Before
# anything runs, .sr_emit_check() walks the parsed code and refuses what the Global Constraints forbid.
# The diagnostic-call and p-name lists are the ones of core/lint.R, referenced and not restated.
#
# Threat model: the checker guards against hostile data text (column names, labels, levels) and
# against route-author bugs. It is NOT a sandbox against a hostile route author. But every rule it
# states holds for the obvious aliases: a forbidden function used as a value ((system)('x'),
# f <- system, Negate(system)), and a branch on a diagnostic hidden behind a replacement assignment
# (res$z <- cox.zph(f)), a subscript (x[z$table[, 3] < 0.05]) or && / ||.

#' Backtick-quoted name for one non-NA, non-empty string. Round-trips any text, including a name with
#' a backtick, quote, `);`, newline, backslash or non-ASCII letter, as a single symbol.
.sr_q <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(".sr_q: a name must be one non-NA, non-empty string", call. = FALSE)
  }
  deparse(as.name(x), backtick = TRUE)
}

#' A single-line R literal for a plain value; eval(parse()) of it is identical() to x (NA kept, 17
#' significant digits, integers, names and attributes kept).
.sr_lit <- function(x) {
  if (!.sr_value_plain(x)) {
    stop(".sr_lit: only plain data (atomic vectors, factors, data frames, matrices, lists) can be emitted",
         call. = FALSE)
  }
  paste(deparse(x, control = c("keepNA", "keepInteger", "niceNames", "showAttributes", "digits17")), collapse = "")
}

#' "<name> <- <deparse(fn)>": the function's source as text. The name must be `.h_<lower snake>`;
#' the function's environment is not part of the text, so the result is deterministic.
.sr_inline <- function(fn, name) {
  if (!is.function(fn)) stop(".sr_inline: fn must be a function", call. = FALSE)
  if (!is.character(name) || length(name) != 1L || is.na(name) || !grepl("^[.]h_[a-z0-9_]+$", name)) {
    stop(".sr_inline: name must match ^[.]h_[a-z0-9_]+$", call. = FALSE)
  }
  paste0(name, " <- ", paste(deparse(fn), collapse = "\n"))
}

#' TRUE when x is, recursively, only NULL, atomic vectors, factors, data frames, matrices and lists:
#' no functions, environments, formulas, calls, symbols or S4 objects, also not inside attributes.
.sr_value_plain <- function(x) {
  if (is.null(x)) return(TRUE)
  if (isS4(x) || is.function(x) || is.environment(x) || is.language(x) || is.expression(x) ||
      is.pairlist(x) || typeof(x) %in% c("externalptr", "weakref", "bytecode", "promise", "...")) {
    return(FALSE)
  }
  if (!is.atomic(x) && !is.list(x)) return(FALSE)
  at <- attributes(x)
  if (length(at) && !all(vapply(at, .sr_value_plain, logical(1)))) return(FALSE)
  if (is.list(x) && !all(vapply(x, .sr_value_plain, logical(1)))) return(FALSE)
  TRUE
}


# ---- the emitted-code checker ----------------------------------------------------------------

# Namespaces emitted code may call into. openssl is added for the prologue only.
.sr_emit_pkgs <- c("base", "stats", "utils", "methods", "parallel", "survival", "mice", "sandwich",
                   "splines", "Hmisc", "logistf")
# Never callable, bare or namespaced, and never usable as a value (alias). The second block is
# indirection (look a function up by name, reach into an environment), which would defeat the checks.
.sr_emit_forbidden <- c(
  "system", "system2", "shell", "shell.exec", "Sys.setenv", "Sys.unsetenv", "setwd", "unlink",
  "file.remove", "file.rename", "file.copy", "download.file", "url", "socketConnection", "source",
  "sys.source", "library", "require", "requireNamespace", "attachNamespace", "loadNamespace",
  "dyn.load", "eval", "evalq", "parse", "get", "assign", "q", "quit", "saveRDS", "writeLines",
  "writeBin", "sink",
  "get0", "mget", "match.fun", "getExportedValue", "getFromNamespace", "asNamespace",
  "getNamespace", "baseenv", "as.environment"
)
# Callable only from the prologue (which reads the hashed extract).
.sr_emit_prologue_only <- c("readBin", "readRDS", "file.size")
# Higher-order callers: a forbidden name handed to one of these as a symbol or string is a call by
# another route (lapply(x, system), Reduce("system", ...)).
.sr_emit_hof <- c("lapply", "sapply", "vapply", "Map", "mapply", "Reduce", "Filter", "Position",
                  "Find", "do.call", "outer", "apply", "tapply", "rapply", "eapply", "Vectorize")
# Calls whose result depends on a condition: the checks for diagnostic-gated branching apply to them.
.sr_emit_branch <- c("if", "while", "switch", "ifelse", "if_else", "case_when", "&&", "||")

#' Short text of an expression for an error message.
.sr_emit_show <- function(e) {
  s <- paste(deparse(e, width.cutoff = 500L), collapse = " ")
  if (nchar(s) > 80L) s <- paste0(substr(s, 1L, 77L), "...")
  s
}

#' Names (symbols and strings) mentioned anywhere inside e, plus "$p" for a `$p` access.
.sr_emit_atoms <- function(e) {
  out <- character()
  rec <- function(x) {
    if (is.symbol(x)) {
      s <- as.character(x)
      if (nzchar(s)) out <<- c(out, s)
    } else if (is.character(x)) {
      out <<- c(out, x)
    } else if (is.call(x)) {
      if (identical(x[[1L]], as.name("$")) && length(x) == 3L && identical(x[[3L]], as.name("p"))) {
        out <<- c(out, "$p")
      }
      for (i in seq_along(x)) if (!identical(x[[i]], quote(expr = ))) rec(x[[i]])
    } else if (is.pairlist(x) || is.list(x)) {
      for (i in seq_along(x)) if (!identical(x[[i]], quote(expr = ))) rec(x[[i]])
    }
  }
  rec(e)
  out
}

#' What makes an expression diagnostic: the diagnostic calls and p-names of core/lint.R, the
#' Pr(...) column names, and (in a branch condition only) a `$p` access.
.sr_emit_diag_names <- function() {
  c(.sr_lint_diag_calls, .sr_lint_pnames, .sr_lint_pr_strings, "$p")
}

#' The variable a (replacement) assignment target modifies: x for x, x$a, x[[i]], x[i], names(x).
.sr_emit_root <- function(lhs) {
  while (is.call(lhs) && length(lhs) >= 2L) lhs <- lhs[[2L]]
  if (is.character(lhs) && length(lhs) == 1L) lhs <- as.name(lhs)
  if (is.symbol(lhs)) as.character(lhs) else NA_character_
}

#' Every `target <- rhs` (also `=` and `<<-`, and replacement forms) in the parse trees:
#' list(name = root variable of the target, rhs).
.sr_emit_assigns <- function(exprs) {
  nm <- character()
  rhs <- list()
  rec <- function(x) {
    if (!is.call(x)) return(invisible())
    h <- x[[1L]]
    if (is.symbol(h) && as.character(h) %in% c("<-", "=", "<<-") && length(x) == 3L) {
      root <- .sr_emit_root(x[[2L]])
      if (!is.na(root)) {
        nm <<- c(nm, root)
        rhs[length(rhs) + 1L] <<- list(x[[3L]])   # list(): a NULL right-hand side keeps its slot
      }
    }
    for (i in seq_along(x)) if (!identical(x[[i]], quote(expr = ))) rec(x[[i]])
  }
  for (e in exprs) rec(e)
  list(name = nm, rhs = rhs)
}

#' Variables whose value derives (through assignments, to a fixpoint) from a diagnostic, so that
#' `z <- cox.zph(f); if (z$table[1, 3] < 0.05)` is caught as well as the direct form. A replacement
#' assignment (res$z <- cox.zph(f)) taints the root variable.
.sr_emit_tainted <- function(exprs) {
  a <- .sr_emit_assigns(exprs)
  diag <- setdiff(.sr_emit_diag_names(), "$p")
  tainted <- character()
  repeat {
    new <- character()
    for (i in seq_along(a$name)) {
      if (a$name[i] %in% tainted) next
      at <- .sr_emit_atoms(a$rhs[[i]])
      if (any(at %in% diag) || any(at %in% tainted)) new <- c(new, a$name[i])
    }
    if (!length(new)) break
    tainted <- unique(c(tainted, new))
  }
  tainted
}

#' The condition expressions of a branching call (if / while / switch / ifelse / case_when / && / ||).
.sr_emit_conditions <- function(e) {
  h <- as.character(e[[1L]])
  if (h %in% c("if", "while")) return(list(e[[2L]]))
  if (h %in% c("&&", "||")) return(as.list(e)[-1L])
  if (length(e) < 2L) return(list())
  if (h == "case_when") {
    return(lapply(as.list(e)[-1L], function(a) {
      if (is.call(a) && identical(a[[1L]], as.name("~"))) a[[2L]] else NULL
    }))
  }
  nm <- names(e)
  key <- c("test", "condition", "EXPR")
  pick <- if (!is.null(nm) && any(nm %in% key)) match(TRUE, nm %in% key) else 2L
  list(e[[pick]])
}

#' Check emitted code against the Global Constraints. Stops, naming the offending call, on the
#' first violation; otherwise returns TRUE invisibly.
#' A forbidden function name also counts when it appears as a bare symbol used as a value (an alias),
#' except inside a formula, after `$` / `@`, as an argument name, as a string, or as an assignment
#' target. Emitted code never binds a forbidden name, so there is no lexical-scope exemption.
#' @param code one string of R code
#' @param kind "analysis" (no diagnostic call or p-name at all) or "diagnostic" (diagnostics may
#'   run, but no branch condition, subscript or && / || operand may depend on one)
#' @param prologue TRUE for the fixed prologue, which may also read the extract (readBin, readRDS,
#'   file.size) and call openssl
.sr_emit_check <- function(code, kind = c("analysis", "diagnostic"), prologue = FALSE) {
  kind <- match.arg(kind)
  if (!is.character(code) || length(code) != 1L || is.na(code)) {
    stop("emitted code must be one string", call. = FALSE)
  }
  exprs <- tryCatch(
    as.list(parse(text = code, keep.source = FALSE)),
    error = function(e) stop("emitted code does not parse: ", conditionMessage(e), call. = FALSE)
  )
  pkgs <- c(.sr_emit_pkgs, if (isTRUE(prologue)) "openssl")
  diag <- .sr_emit_diag_names()
  diag_subscript <- setdiff(diag, "$p")
  tainted_vars <- if (kind == "diagnostic") .sr_emit_tainted(exprs) else character()
  barred_values <- c(.sr_emit_forbidden, if (!isTRUE(prologue)) .sr_emit_prologue_only)
  bad <- function(e, why) {
    stop("emitted code rejected: ", why, " in `", .sr_emit_show(e), "`", call. = FALSE)
  }
  depends <- function(expr, names) {
    at <- .sr_emit_atoms(expr)
    any(at %in% names) || any(at %in% tainted_vars)
  }

  # a name used as a function, bare or namespaced
  check_fn <- function(fn, e) {
    if (fn %in% .sr_emit_forbidden) bad(e, sprintf("call to forbidden function '%s'", fn))
    if (fn %in% .sr_emit_prologue_only && !isTRUE(prologue)) {
      bad(e, sprintf("'%s' is allowed in the prologue only", fn))
    }
    if (kind == "analysis" && fn %in% diag) bad(e, sprintf("diagnostic '%s' in analysis code", fn))
  }
  check_pname <- function(s, e) {
    if (kind == "analysis" && nzchar(s) && s %in% .sr_lint_pnames) {
      bad(e, sprintf("p-value name '%s' in analysis code", s))
    }
  }

  # fm: inside a formula, where bare symbols are variable names, not values
  walk <- function(e, fm = FALSE) {
    if (is.symbol(e)) {
      s <- as.character(e)
      check_pname(s, e)
      if (!fm && s %in% barred_values) {
        bad(e, sprintf("forbidden function '%s' used as a value", s))
      }
      return(invisible())
    }
    if (is.character(e)) {
      if (kind == "analysis" && any(e %in% .sr_lint_pnames)) bad(e, "p-value name in analysis code")
      return(invisible())
    }
    if (!is.call(e)) return(invisible())
    h <- e[[1L]]
    hs <- if (is.symbol(h)) as.character(h) else NA_character_

    if (!is.na(hs) && hs %in% c("::", ":::")) {
      if (hs == ":::") bad(e, "':::' is not allowed")
      pkg <- as.character(e[[2L]])
      fn <- as.character(e[[3L]])
      if (length(e) != 3L || length(pkg) != 1L || length(fn) != 1L) bad(e, "malformed namespace access")
      if (!(pkg %in% pkgs)) bad(e, sprintf("namespace '%s' is not allowed", pkg))
      check_fn(fn, e)
      return(invisible())
    }

    if (!is.na(hs)) {
      check_fn(hs, e)
      if (kind == "diagnostic" && hs %in% .sr_emit_branch) {
        for (cond in .sr_emit_conditions(e)) {
          if (!is.null(cond) && depends(cond, diag)) bad(e, "a branch condition depends on a diagnostic")
        }
      }
      # x[<diagnostic>] selects by a diagnostic; z$table[1, 3] (indexing the diagnostic itself) does not
      if (kind == "diagnostic" && hs %in% c("[", "[[") && length(e) >= 3L &&
          !identical(e[[2L]], quote(expr = )) && !depends(e[[2L]], diag)) {
        for (i in seq_along(e)[-(1:2)]) {
          if (!identical(e[[i]], quote(expr = )) && depends(e[[i]], diag_subscript)) {
            bad(e, "a subscript (a branch condition) depends on a diagnostic")
          }
        }
      }
      # subset(x, <cond>) and Filter(<pred>, x) select by a condition, like x[<cond>]
      if (kind == "diagnostic" && hs %in% c("subset", "Filter") && length(e) >= 3L) {
        obj <- if (hs == "subset") 2L else 3L
        conds <- if (hs == "subset") seq_along(e)[-(1:2)] else 2L
        if (!identical(e[[obj]], quote(expr = )) && !depends(e[[obj]], diag)) {
          for (i in conds) {
            if (!identical(e[[i]], quote(expr = )) && depends(e[[i]], diag_subscript)) {
              bad(e, sprintf("the condition of %s() depends on a diagnostic", hs))
            }
          }
        }
      }
      if (hs == "do.call") {
        nm <- names(e)
        pos <- if (!is.null(nm) && "what" %in% nm) match("what", nm) else if (length(e) >= 2L) 2L else NA_integer_
        if (!is.na(pos)) {
          first <- e[[pos]]
          strish <- c("c", "paste", "paste0", "as.character", "sprintf")
          if (is.character(first) ||
              (is.call(first) && is.symbol(first[[1L]]) && as.character(first[[1L]]) %in% strish)) {
            bad(e, "do.call with a character first argument")
          }
        }
      }
      if (hs %in% .sr_emit_hof) {
        for (i in seq_along(e)[-1L]) {
          a <- e[[i]]
          nm <- if (is.symbol(a)) as.character(a) else if (is.character(a) && length(a) == 1L) a else NA_character_
          if (!is.na(nm) && (nm %in% .sr_emit_forbidden ||
                             (nm %in% .sr_emit_prologue_only && !isTRUE(prologue)))) {
            bad(e, sprintf("forbidden function '%s' passed by name", nm))
          }
        }
      }
    } else if (is.call(h)) {
      head_fn <- if (is.symbol(h[[1L]])) as.character(h[[1L]]) else ""
      if (!(head_fn %in% c("::", ":::", "function", "("))) bad(e, "call through a computed function")
      walk(h, fm)
    } else {
      bad(e, "call through a computed function")
    }

    # a function literal: formals defaults, then the body
    if (!is.na(hs) && hs == "function") {
      fml <- e[[2L]]
      for (i in seq_along(fml)) if (!identical(fml[[i]], quote(expr = ))) walk(fml[[i]], fm)
      if (length(e) >= 3L && !identical(e[[3L]], quote(expr = ))) walk(e[[3L]], fm)
      return(invisible())
    }
    child_fm <- fm || identical(hs, "~")
    for (i in seq_along(e)[-1L]) {
      if (identical(e[[i]], quote(expr = ))) next
      a <- e[[i]]
      # the right-hand side of $ and @ is a field name, not a value
      if (identical(hs, "$") || identical(hs, "@")) {
        if (i == 3L && is.symbol(a)) {
          check_pname(as.character(a), a)
          next
        }
      }
      # an assignment target is a binding, not a value
      if (!is.na(hs) && hs %in% c("<-", "=", "<<-") && i == 2L && is.symbol(a)) {
        check_pname(as.character(a), a)
        next
      }
      walk(a, child_fm)
    }
    invisible()
  }

  for (e in exprs) walk(e)
  invisible(TRUE)
}
