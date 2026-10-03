# core/lint.R - static lints that keep two design rules mechanical.
#
#   * p-gate (M1.1): a route's compile() must never branch on the outcome of a diagnostic test
#     (normality, heteroscedasticity, proportional hazards ...). The analysis is fixed by the SAP,
#     not chosen by a p-value.
#   * air-gap: nothing under app/R/ may reach the network or provision packages.
#
# Both lints work on getParseData() tokens, never on raw text, so string literals (including the
# forbidden names listed below as data) and variables that merely share a name never flag.

#' A zero-row findings table (file, line, msg).
.sr_lint_empty <- function() {
  data.frame(file = character(), line = integer(), msg = character(), stringsAsFactors = FALSE)
}

#' Build a findings table from parallel vectors.
.sr_lint_rows <- function(file, line, msg) {
  if (!length(line)) return(.sr_lint_empty())
  data.frame(file = rep(file, length(line)), line = as.integer(line), msg = as.character(msg),
             stringsAsFactors = FALSE)
}

#' Parse a file into its token table plus a children index, or NULL if it cannot be parsed.
.sr_lint_parse <- function(file) {
  pd <- tryCatch(
    utils::getParseData(parse(file = file, keep.source = TRUE, encoding = "UTF-8"), includeText = NA),
    error = function(e) NULL
  )
  if (is.null(pd)) return(NULL)
  if (!nrow(pd)) return(list(pd = pd, kids = list()))
  pd <- pd[order(pd$line1, pd$col1, -pd$line2, -pd$col2), , drop = FALSE]
  rownames(pd) <- as.character(pd$id)
  list(pd = pd, kids = split(pd$id, factor(pd$parent, levels = unique(pd$parent))))
}

#' Ordered child ids of node id.
.sr_lint_kids <- function(ps, id) {
  k <- ps$kids[[as.character(id)]]
  if (is.null(k)) integer() else k
}

#' All descendant ids of node id (not including id).
.sr_lint_desc <- function(ps, id) {
  out <- integer()
  todo <- .sr_lint_kids(ps, id)
  while (length(todo)) {
    out <- c(out, todo)
    todo <- unlist(lapply(todo, function(i) .sr_lint_kids(ps, i)), use.names = FALSE)
  }
  out
}

#' Token type and text of node ids.
.sr_lint_tok <- function(ps, ids) ps$pd[as.character(ids), "token"]
.sr_lint_txt <- function(ps, ids) ps$pd[as.character(ids), "text"]

#' Strip one pair of surrounding quotes from string-literal token text.
.sr_lint_unquote <- function(x) {
  q <- paste0("[", intToUtf8(34L), "']")
  gsub(paste0("^", q, "|", q, "$"), "", x)
}

#' Is node id a function definition expression (function(...) or a backslash lambda)?
.sr_lint_is_fun <- function(ps, id) {
  k <- .sr_lint_kids(ps, id)
  if (!length(k)) return(FALSE)
  bs <- intToUtf8(92L)
  .sr_lint_tok(ps, k[1L]) %in% c("FUNCTION", paste0("'", bs, bs, "'"))
}

#' Arguments of a call expression as list(name, id): name is "" for positional arguments.
.sr_lint_args <- function(ps, call_id) {
  k <- .sr_lint_kids(ps, call_id)
  tk <- .sr_lint_tok(ps, k)
  open <- which(tk == "'('")
  if (!length(open)) return(list())
  args <- list()
  i <- open[1L] + 1L
  while (i <= length(k)) {
    if (tk[i] %in% c("SYMBOL_SUB", "STR_CONST") && i + 2L <= length(k) && tk[i + 1L] == "EQ_SUB") {
      args[[length(args) + 1L]] <- list(name = .sr_lint_unquote(.sr_lint_txt(ps, k[i])), id = k[i + 2L])
      i <- i + 3L
      next
    }
    if (tk[i] == "expr") args[[length(args) + 1L]] <- list(name = "", id = k[i])
    i <- i + 1L
  }
  args
}

# ---- p-gate ---------------------------------------------------------------------------------

.sr_lint_pnames <- c("p.value", "p_value", "pvalue")
.sr_lint_diag_calls <- c(
  "shapiro.test", "ks.test", "bartlett.test", "leveneTest", "fligner.test", "mauchly.test",
  "cox.zph", "check_normality", "check_heteroscedasticity"
)
.sr_lint_branch_calls <- c("switch", "ifelse", "if_else")

#' Ids of the function-definition expressions assigned to a name `compile`.
.sr_lint_compile_funs <- function(ps) {
  pd <- ps$pd
  out <- integer()
  is_name <- (pd$token == "SYMBOL" & pd$text == "compile") |
    (pd$token %in% c("SYMBOL_SUB", "STR_CONST") & .sr_lint_unquote(pd$text) == "compile")
  for (id in pd$id[is_name]) {
    parent <- pd[as.character(id), "parent"]
    if (pd[as.character(id), "token"] == "SYMBOL") {
      # lhs of  compile <- function  /  compile = function  /  x$compile <- function
      if (parent == 0L) next
      gp <- pd[as.character(parent), "parent"]
      if (gp == 0L) next
      gk <- .sr_lint_kids(ps, gp)
      if (length(gk) < 3L || gk[1L] != parent) next
      if (!(.sr_lint_tok(ps, gk[2L]) %in% c("LEFT_ASSIGN", "EQ_ASSIGN"))) next
      if (.sr_lint_is_fun(ps, gk[3L])) out <- c(out, gk[3L])
    } else {
      # compile = function(...) as a list element or call argument
      sib <- .sr_lint_kids(ps, parent)
      pos <- match(id, sib)
      if (is.na(pos) || pos + 2L > length(sib)) next
      if (.sr_lint_tok(ps, sib[pos + 1L]) != "EQ_SUB") next
      if (.sr_lint_is_fun(ps, sib[pos + 2L])) out <- c(out, sib[pos + 2L])
    }
  }
  unique(out)
}

#' Does the expression node `cond` mention a diagnostic test (symbol, string, $p, or call)?
.sr_lint_cond_is_diag <- function(ps, cond) {
  ids <- c(cond, .sr_lint_desc(ps, cond))
  tk <- .sr_lint_tok(ps, ids)
  tx <- .sr_lint_unquote(.sr_lint_txt(ps, ids))
  if (any(tk %in% c("SYMBOL", "SYMBOL_SUB", "STR_CONST") & tx %in% .sr_lint_pnames)) return(TRUE)
  if (any(tk == "SYMBOL_FUNCTION_CALL" & tx %in% .sr_lint_diag_calls)) return(TRUE)
  # a `$p` access: the symbol p directly after a dollar sign
  for (i in which(tk == "SYMBOL" & tx == "p")) {
    sib <- .sr_lint_kids(ps, ps$pd[as.character(ids[i]), "parent"])
    pos <- match(ids[i], sib)
    if (pos > 1L && .sr_lint_tok(ps, sib[pos - 1L]) == "'$'") return(TRUE)
  }
  FALSE
}

#' The condition node of an `if` token or of an ifelse/if_else/switch call token, or NA.
.sr_lint_condition <- function(ps, tok_id) {
  if (ps$pd[as.character(tok_id), "token"] == "IF") {
    k <- .sr_lint_kids(ps, ps$pd[as.character(tok_id), "parent"])
    ex <- k[.sr_lint_tok(ps, k) == "expr"]
    return(if (length(ex)) ex[1L] else NA_integer_)
  }
  # SYMBOL_FUNCTION_CALL -> function-name expr -> call expr
  call_id <- ps$pd[as.character(ps$pd[as.character(tok_id), "parent"]), "parent"]
  args <- .sr_lint_args(ps, call_id)
  if (!length(args)) return(NA_integer_)
  nm <- vapply(args, function(a) a$name, character(1))
  named <- which(nm %in% c("test", "condition", "EXPR"))
  args[[if (length(named)) named[1L] else 1L]]$id
}

#' p-gate: flag compile() bodies that branch on a diagnostic test (M1.1).
#' @param file character vector of R files
#' @return data.frame(file, line, msg); zero rows when clean
sr_lint_pgate <- function(file) {
  res <- lapply(file, function(f) {
    ps <- .sr_lint_parse(f)
    if (is.null(ps)) return(.sr_lint_rows(f, 1L, "cannot parse file (p-gate)"))
    funs <- .sr_lint_compile_funs(ps)
    if (!length(funs)) return(.sr_lint_empty())
    body_ids <- unique(unlist(lapply(funs, function(id) .sr_lint_desc(ps, id)), use.names = FALSE))
    tk <- .sr_lint_tok(ps, body_ids)
    tx <- .sr_lint_txt(ps, body_ids)
    cand <- body_ids[tk == "IF" | (tk == "SYMBOL_FUNCTION_CALL" & tx %in% .sr_lint_branch_calls)]
    hit <- integer()
    for (tid in cand) {
      cond <- .sr_lint_condition(ps, tid)
      if (!is.na(cond) && .sr_lint_cond_is_diag(ps, cond)) hit <- c(hit, tid)
    }
    if (!length(hit)) return(.sr_lint_empty())
    pd <- ps$pd[as.character(hit), ]
    pd <- pd[order(pd$line1, pd$col1), ]
    .sr_lint_rows(f, pd$line1, "compile() branches on a diagnostic test (M1.1)")
  })
  do.call(rbind, c(list(.sr_lint_empty()), res))
}

# ---- air-gap --------------------------------------------------------------------------------

.sr_lint_net_calls <- c("download.file", "url", "socketConnection", "install.packages", "py_install")
.sr_lint_net_pkgs <- c("curl", "httr", "httr2")

#' Is the file covered by the allowlist (normalised, forward-slash, suffix match)?
.sr_lint_allowed <- function(file, allow) {
  if (!length(allow)) return(FALSE)
  bs <- intToUtf8(92L)
  norm <- function(p) {
    p <- gsub(bs, "/", normalizePath(p, winslash = "/", mustWork = FALSE), fixed = TRUE)
    sub("^[.]/", "", p)
  }
  f <- norm(file)
  any(vapply(allow, function(a) {
    a <- sub("^[.]/", "", gsub(bs, "/", a, fixed = TRUE))
    f == a || endsWith(f, paste0("/", a))
  }, logical(1)))
}

#' Is the argument node TRUE / T?
.sr_lint_is_true <- function(ps, id) {
  k <- .sr_lint_kids(ps, id)
  length(k) == 1L && .sr_lint_txt(ps, k) %in% c("TRUE", "T")
}

#' Air-gap: flag network and package-provisioning calls.
#' @param files character vector of R files
#' @param allow character vector of paths (suffix match, any separator) that may use the network
#' @return data.frame(file, line, msg); zero rows when clean
sr_lint_airgap <- function(files, allow = "app/R/router/backends/jev.R") {
  res <- lapply(files, function(f) {
    if (.sr_lint_allowed(f, allow)) return(.sr_lint_empty())
    ps <- .sr_lint_parse(f)
    if (is.null(ps)) return(.sr_lint_rows(f, 1L, "cannot parse file (air-gap)"))
    pd <- ps$pd
    line <- integer()
    col <- integer()
    msg <- character()
    add <- function(id, m) {
      line <<- c(line, pd[as.character(id), "line1"])
      col <<- c(col, pd[as.character(id), "col1"])
      msg <<- c(msg, m)
    }
    for (id in pd$id[pd$token == "SYMBOL_PACKAGE" & pd$text %in% .sr_lint_net_pkgs]) {
      sib <- .sr_lint_kids(ps, pd[as.character(id), "parent"])
      pos <- match(id, sib)
      fn <- if (pos + 2L <= length(sib)) .sr_lint_txt(ps, sib[pos + 2L]) else "?"
      add(id, sprintf("network call: %s::%s (air-gap)", pd[as.character(id), "text"], fn))
    }
    for (id in pd$id[pd$token == "SYMBOL_FUNCTION_CALL"]) {
      nm <- pd[as.character(id), "text"]
      if (!(nm %in% c(.sr_lint_net_calls, "py_available", "options"))) next
      parent <- pd[as.character(id), "parent"]
      sib <- .sr_lint_kids(ps, parent)
      stk <- .sr_lint_tok(ps, sib)
      pos <- match(id, sib)
      # a method call (x$url(...)) is not the base function; a curl/httr namespace is reported above
      if (pos > 1L && stk[pos - 1L] %in% c("'$'", "'@'")) next
      if (pos > 2L && stk[pos - 2L] == "SYMBOL_PACKAGE" &&
          .sr_lint_txt(ps, sib[pos - 2L]) %in% .sr_lint_net_pkgs) next
      args <- .sr_lint_args(ps, pd[as.character(parent), "parent"])
      nms <- vapply(args, function(a) a$name, character(1))
      if (nm %in% .sr_lint_net_calls) {
        add(id, sprintf("network call: %s (air-gap)", nm))
      } else if (nm == "py_available") {
        init <- NA_integer_
        if ("initialize" %in% nms) {
          init <- args[[match("initialize", nms)]]$id
        } else if (length(args) && nms[1L] == "") {
          init <- args[[1L]]$id
        }
        if (!is.na(init) && .sr_lint_is_true(ps, init)) {
          add(id, "network call: py_available(initialize = TRUE) (air-gap)")
        }
      } else if ("repos" %in% nms) {
        add(id, "network call: options(repos = ) (air-gap)")
      }
    }
    if (!length(line)) return(.sr_lint_empty())
    o <- order(line, col)
    .sr_lint_rows(f, line[o], msg[o])
  })
  do.call(rbind, c(list(.sr_lint_empty()), res))
}
