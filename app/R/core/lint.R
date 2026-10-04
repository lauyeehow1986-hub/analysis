# core/lint.R - static lints that keep two design rules mechanical.
#
#   * p-gate (M1.1): a route's compile() must never branch on the outcome of a diagnostic test
#     (normality, heteroscedasticity, proportional hazards ...). The analysis is fixed by the SAP,
#     not chosen by a p-value. compile() therefore must not even run such a test.
#   * air-gap: nothing under app/ may reach the network or provision packages.
#
# Both lints work on getParseData() tokens, never on raw-text matching of code, so string literals
# (including the forbidden names listed below as data) and variables that merely share a name never
# flag. The one place a string is inspected is the p-gate's emitted-code rule (3d below).

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

#' Parse a file into its token table plus a children index, or NULL if it cannot be read or parsed
#' (missing file, embedded NUL, invalid UTF-8, syntax error, any warning while parsing).
.sr_lint_parse <- function(file) {
  tryCatch({
    bytes <- readBin(file, what = "raw", n = file.size(file))
    txt <- rawToChar(bytes)
    Encoding(txt) <- "UTF-8"
    if (!validUTF8(txt)) stop("invalid UTF-8")
    txt <- sub(intToUtf8(0xFEFFL), "", txt, fixed = TRUE)
    txt <- gsub(intToUtf8(13L), "", txt, fixed = TRUE)
    pd <- utils::getParseData(parse(text = txt, keep.source = TRUE), includeText = NA)
    if (!is.null(pd) && nrow(pd)) {
      # getParseData abbreviates string tokens over ~1000 chars; restore their full text
      long <- pd$token == "STR_CONST" & startsWith(pd$text, "[") & grepl(" chars quoted", pd$text, fixed = TRUE)
      if (any(long)) pd$text[long] <- utils::getParseText(pd, pd$id[long])
    }
    if (is.null(pd) || !nrow(pd)) {
      pd <- data.frame(id = integer(), parent = integer(), token = character(), text = character(),
                       line1 = integer(), col1 = integer(), line2 = integer(), col2 = integer(),
                       stringsAsFactors = FALSE)
      return(list(pd = pd, kids = list()))
    }
    pd <- pd[order(pd$line1, pd$col1, -pd$line2, -pd$col2), , drop = FALSE]
    rownames(pd) <- as.character(pd$id)
    list(pd = pd, kids = split(pd$id, factor(pd$parent, levels = unique(pd$parent))))
  }, error = function(e) NULL, warning = function(w) NULL)
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

#' Every `name <- rhs` / `name = rhs` / `x$name <- rhs` as data.frame(name, rhs, dollar).
.sr_lint_assigns <- function(ps) {
  pd <- ps$pd
  name <- character()
  rhs <- integer()
  dollar <- logical()
  for (op in pd$id[pd$token %in% c("LEFT_ASSIGN", "EQ_ASSIGN")]) {
    gk <- .sr_lint_kids(ps, pd[as.character(op), "parent"])
    if (length(gk) != 3L || gk[2L] != op) next
    lk <- .sr_lint_kids(ps, gk[1L])
    ltk <- .sr_lint_tok(ps, lk)
    nm <- NA_character_
    dl <- FALSE
    if (length(lk) == 1L && ltk %in% c("SYMBOL", "STR_CONST")) {
      nm <- .sr_lint_unquote(.sr_lint_txt(ps, lk))
    } else if (length(lk) == 3L && ltk[2L] == "'$'" && ltk[3L] %in% c("SYMBOL", "STR_CONST")) {
      nm <- .sr_lint_unquote(.sr_lint_txt(ps, lk[3L]))
      dl <- TRUE
    }
    if (is.na(nm)) next
    name <- c(name, nm)
    rhs <- c(rhs, gk[3L])
    dollar <- c(dollar, dl)
  }
  data.frame(name = name, rhs = rhs, dollar = dollar, stringsAsFactors = FALSE)
}

# ---- p-gate ---------------------------------------------------------------------------------
# compile() is lint-clean only if it neither runs a diagnostic test nor mentions a p-value:
#   3a  compile bound to a named helper (compile = cmp, compile <- cmp) lints that helper's body
#   3b  any diagnostic-test call or p-name symbol/string anywhere in a compile body flags (this
#       closes local-variable taint: normal <- shapiro.test(x)$p.value > .05; if (normal) ...)
#   3c  branch constructs (if, while, switch, ifelse, if_else, case_when) whose condition has a
#       $p access or a "Pr(>F)"/"Pr(>Chisq)" string flag; $p is condition-only because spec$p
#       may legitimately be a predictor count
#   3d  a string literal that contains BOTH a branch keyword call AND a diagnostic/p-name flags
#       (emitted code that branches on a test); a string holding only a diagnostic name, such as
#       an emitted "cox.zph(fit)" for a report panel, stays clean. 3b's string rule is an exact
#       match on a p-name (res[["p.value"]]), whereas 3d is a substring test that additionally
#       requires a branch keyword, so the two never overlap on "cox.zph(fit)".
# One finding per offending line.

.sr_lint_pnames <- c("p.value", "p_value", "pvalue", "pval", "p_val")
.sr_lint_diag_calls <- c(
  "shapiro.test", "ks.test", "bartlett.test", "leveneTest", "fligner.test", "mauchly.test",
  "cox.zph", "check_normality", "check_heteroscedasticity", "ad.test", "check_homogeneity"
)
.sr_lint_branch_calls <- c("switch", "ifelse", "if_else", "case_when")
.sr_lint_pr_strings <- c("Pr(>F)", "Pr(>Chisq)")

#' Function-definition expression ids that `rhs` denotes: itself, or (for a bare symbol) the
#' functions bound to that name elsewhere in the file, following alias chains a few levels deep.
.sr_lint_resolve_fun <- function(ps, rhs, defs, depth = 0L) {
  if (.sr_lint_is_fun(ps, rhs)) return(rhs)
  if (depth >= 5L) return(integer())
  k <- .sr_lint_kids(ps, rhs)
  if (length(k) != 1L || .sr_lint_tok(ps, k) != "SYMBOL") return(integer())
  idx <- which(defs$name == .sr_lint_txt(ps, k) & !defs$dollar)
  as.integer(unlist(lapply(defs$rhs[idx], function(r) .sr_lint_resolve_fun(ps, r, defs, depth + 1L))))
}

#' Ids of the function-definition expressions bound to a name `compile`.
.sr_lint_compile_funs <- function(ps) {
  pd <- ps$pd
  defs <- .sr_lint_assigns(ps)
  rhs <- defs$rhs[defs$name == "compile"]
  # compile = <expr> as a list element or call argument
  is_arg <- pd$token %in% c("SYMBOL_SUB", "STR_CONST") & .sr_lint_unquote(pd$text) == "compile"
  for (id in pd$id[is_arg]) {
    sib <- .sr_lint_kids(ps, pd[as.character(id), "parent"])
    pos <- match(id, sib)
    if (is.na(pos) || pos + 2L > length(sib)) next
    if (.sr_lint_tok(ps, sib[pos + 1L]) != "EQ_SUB") next
    rhs <- c(rhs, sib[pos + 2L])
  }
  unique(as.integer(unlist(lapply(rhs, function(r) .sr_lint_resolve_fun(ps, r, defs)))))
}

#' Condition nodes of an `if`/`while` token or of a branch-call token (case_when has several).
.sr_lint_conditions <- function(ps, tok_id) {
  tk <- ps$pd[as.character(tok_id), "token"]
  if (tk %in% c("IF", "WHILE")) {
    k <- .sr_lint_kids(ps, ps$pd[as.character(tok_id), "parent"])
    ex <- k[.sr_lint_tok(ps, k) == "expr"]
    return(if (length(ex)) ex[1L] else integer())
  }
  # SYMBOL_FUNCTION_CALL -> function-name expr -> call expr
  call_id <- ps$pd[as.character(ps$pd[as.character(tok_id), "parent"]), "parent"]
  args <- .sr_lint_args(ps, call_id)
  if (!length(args)) return(integer())
  if (ps$pd[as.character(tok_id), "text"] == "case_when") {
    # each argument is  condition ~ value : take the left-hand side
    lhs <- lapply(args, function(a) {
      k <- .sr_lint_kids(ps, a$id)
      tl <- which(.sr_lint_tok(ps, k) == "'~'")
      if (length(tl) && tl[1L] > 1L) k[1L] else integer()
    })
    return(as.integer(unlist(lhs)))
  }
  nm <- vapply(args, function(a) a$name, character(1))
  named <- which(nm %in% c("test", "condition", "EXPR"))
  args[[if (length(named)) named[1L] else 1L]]$id
}

#' Does a condition node mention a diagnostic test, a p-name, a Pr(...) column, or a `$p` access?
.sr_lint_cond_is_diag <- function(ps, cond) {
  ids <- c(cond, .sr_lint_desc(ps, cond))
  tk <- .sr_lint_tok(ps, ids)
  tx <- .sr_lint_unquote(.sr_lint_txt(ps, ids))
  if (any(tk %in% c("SYMBOL", "SYMBOL_SUB", "STR_CONST") & tx %in% .sr_lint_pnames)) return(TRUE)
  if (any(tk == "STR_CONST" & tx %in% .sr_lint_pr_strings)) return(TRUE)
  if (any(tk %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL") & tx %in% .sr_lint_diag_calls)) return(TRUE)
  # a `$p` access: the symbol p directly after a dollar sign
  for (i in which(tk == "SYMBOL" & tx == "p")) {
    sib <- .sr_lint_kids(ps, ps$pd[as.character(ids[i]), "parent"])
    pos <- match(ids[i], sib)
    if (pos > 1L && .sr_lint_tok(ps, sib[pos - 1L]) == "'$'") return(TRUE)
  }
  FALSE
}

#' Lines (and nothing else) of the p-gate violations inside one function-definition expression.
.sr_lint_pgate_lines <- function(ps, fun_id) {
  pd <- ps$pd
  ids <- c(fun_id, .sr_lint_desc(ps, fun_id))
  tk <- pd[as.character(ids), "token"]
  tx <- pd[as.character(ids), "text"]
  ux <- .sr_lint_unquote(tx)
  ln <- pd[as.character(ids), "line1"]
  hit <- logical(length(ids))

  # 3b: diagnostic calls / references and p-names anywhere in the body
  hit <- hit | (tk %in% c("SYMBOL_FUNCTION_CALL", "SYMBOL") & tx %in% .sr_lint_diag_calls)
  hit <- hit | (tk %in% c("SYMBOL", "SYMBOL_SUB", "STR_CONST") & ux %in% .sr_lint_pnames)

  # 3c: branch constructs whose condition is diagnostic
  cand <- which(tk %in% c("IF", "WHILE") | (tk == "SYMBOL_FUNCTION_CALL" & tx %in% .sr_lint_branch_calls))
  for (i in cand) {
    for (cond in .sr_lint_conditions(ps, ids[i])) {
      if (.sr_lint_cond_is_diag(ps, cond)) {
        hit[i] <- TRUE
        break
      }
    }
  }

  # 3d: emitted code that branches on a diagnostic
  bs <- intToUtf8(92L)
  branch_re <- paste0(bs, "b(if|ifelse|if_else|switch|case_when)", bs, "s*", bs, "(")
  words <- c(.sr_lint_diag_calls, .sr_lint_pnames)
  for (i in which(tk == "STR_CONST")) {
    if (grepl(branch_re, tx[i], perl = TRUE) && any(vapply(words, grepl, logical(1), x = tx[i], fixed = TRUE))) {
      hit[i] <- TRUE
    }
  }
  ln[hit]
}

#' p-gate: flag compile() bodies that branch on, or even run, a diagnostic test (M1.1).
#' @param file character vector of R files
#' @return data.frame(file, line, msg); zero rows when clean
sr_lint_pgate <- function(file) {
  res <- lapply(file, function(f) {
    ps <- .sr_lint_parse(f)
    if (is.null(ps)) return(.sr_lint_rows(f, 1L, "cannot read or parse file (p-gate)"))
    funs <- .sr_lint_compile_funs(ps)
    lines <- sort(unique(unlist(lapply(funs, function(id) .sr_lint_pgate_lines(ps, id)))))
    .sr_lint_rows(f, lines, "compile() branches on a diagnostic test (M1.1)")
  })
  do.call(rbind, c(list(.sr_lint_empty()), res))
}

# ---- air-gap --------------------------------------------------------------------------------

.sr_lint_net_calls <- c(
  "download.file", "url", "socketConnection", "install.packages", "py_install",
  "curl_fetch_memory", "curl_fetch_disk", "curl_fetch_stream", "curl_download", "req_perform",
  "getURL", "getBinaryURL",
  "update.packages", "install_miniconda", "conda_install", "virtualenv_create", "virtualenv_install",
  "font_google", "font_link"
)
# jsonlite::fromJSON / read_json fetch a non-JSON string that looks like a URL and read one that names
# a file, so text read from disk could trigger a fetch: app code parses with jsonlite::parse_json.
.sr_lint_json_calls <- c("fromJSON", "read_json")
# names flagged even when only referenced (lapply(u, download.file)); url stays call-only
.sr_lint_net_refs <- c("download.file", "install.packages", "socketConnection", "py_install",
                       .sr_lint_json_calls)
.sr_lint_net_pkgs <- c("curl", "httr", "httr2", "RCurl", "remotes", "pak")
.sr_lint_lib_pkgs <- c("curl", "httr", "httr2", "RCurl")
.sr_lint_lib_calls <- c("library", "require", "requireNamespace", "loadNamespace", "attachNamespace")

# DuckDB extensions cannot be fetched on an air-gapped box, and DuckDB read_json needs the (absent)
# json extension and fetches URL-looking names. Flagged: a SQL string that starts with INSTALL or
# LOAD (or FORCE INSTALL) followed by a blank, quote or semicolon (so "install.packages" is fine),
# and one that calls read_json / read_json_auto / read_ndjson ...
.sr_lint_sql_ext_re <- "^[[:space:]]*(FORCE[[:space:]]+)?(INSTALL|LOAD)(?=[[:space:];'\"]|$)"
.sr_lint_sql_json_re <- "(?<![A-Za-z0-9_])read_(nd)?json[A-Za-z0-9_]*[[:space:]]*[(]"

#' The value of a STR_CONST token's text (escapes resolved, raw strings handled), or NA.
.sr_lint_str_value <- function(txt) {
  tryCatch({
    v <- parse(text = txt, keep.source = FALSE)[[1L]]
    if (is.character(v) && length(v) == 1L && !is.na(v)) v else NA_character_
  }, error = function(e) NA_character_)
}

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
    if (is.null(ps)) return(.sr_lint_rows(f, 1L, "cannot read or parse file (air-gap)"))
    pd <- ps$pd
    line <- integer()
    col <- integer()
    msg <- character()
    add <- function(id, m) {
      line <<- c(line, pd[as.character(id), "line1"])
      col <<- c(col, pd[as.character(id), "col1"])
      msg <<- c(msg, m)
    }
    # position of a token among its siblings, with the preceding token types and texts
    ctx <- function(id) {
      sib <- .sr_lint_kids(ps, pd[as.character(id), "parent"])
      pos <- match(id, sib)
      list(sib = sib, pos = pos, stk = .sr_lint_tok(ps, sib))
    }
    accessed <- function(x) x$pos > 1L && x$stk[x$pos - 1L] %in% c("'$'", "'@'")
    net_qualified <- function(x) {
      x$pos > 2L && x$stk[x$pos - 2L] == "SYMBOL_PACKAGE" &&
        .sr_lint_txt(ps, x$sib[x$pos - 2L]) %in% .sr_lint_net_pkgs
    }

    # pkg::fn with a network or package-installing namespace
    for (id in pd$id[pd$token == "SYMBOL_PACKAGE" & pd$text %in% .sr_lint_net_pkgs]) {
      x <- ctx(id)
      fn <- if (x$pos + 2L <= length(x$sib)) .sr_lint_txt(ps, x$sib[x$pos + 2L]) else "?"
      add(id, sprintf("network call: %s::%s (air-gap)", pd[as.character(id), "text"], fn))
    }

    json_msg <- "network %s: %s (fetches URLs, reads files; use jsonlite::parse_json) (air-gap)"

    # bare references: lapply(u, download.file), dl <- download.file
    for (id in pd$id[pd$token == "SYMBOL" & pd$text %in% .sr_lint_net_refs]) {
      x <- ctx(id)
      if (accessed(x) || net_qualified(x)) next
      nm <- pd[as.character(id), "text"]
      add(id, if (nm %in% .sr_lint_json_calls) sprintf(json_msg, "reference", nm)
              else sprintf("network reference: %s (air-gap)", nm))
    }

    # calls
    watched <- c(.sr_lint_net_calls, .sr_lint_json_calls, "py_available", "options", .sr_lint_lib_calls)
    for (id in pd$id[pd$token == "SYMBOL_FUNCTION_CALL" & pd$text %in% watched]) {
      nm <- pd[as.character(id), "text"]
      x <- ctx(id)
      # a method call (x$url(...)) is not the base function; a curl/httr namespace is reported above
      if (accessed(x) || net_qualified(x)) next
      args <- .sr_lint_args(ps, pd[as.character(pd[as.character(id), "parent"]), "parent"])
      nms <- vapply(args, function(a) a$name, character(1))
      if (nm %in% .sr_lint_net_calls) {
        add(id, sprintf("network call: %s (air-gap)", nm))
      } else if (nm %in% .sr_lint_json_calls) {
        add(id, sprintf(json_msg, "call", nm))
      } else if (nm %in% .sr_lint_lib_calls) {
        pick <- if ("package" %in% nms) match("package", nms) else match("", nms)
        if (!is.na(pick)) {
          k <- .sr_lint_kids(ps, args[[pick]]$id)
          if (length(k) == 1L && .sr_lint_tok(ps, k) %in% c("SYMBOL", "STR_CONST")) {
            pkg <- .sr_lint_unquote(.sr_lint_txt(ps, k))
            if (pkg %in% .sr_lint_lib_pkgs) add(id, sprintf("network call: %s(%s) (air-gap)", nm, pkg))
          }
        }
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

    # SQL written inside string constants: DuckDB extension provisioning and read_json (see the
    # constants above). The constant is parsed back to its value, so escapes and raw strings count.
    for (id in pd$id[pd$token == "STR_CONST"]) {
      val <- .sr_lint_str_value(pd[as.character(id), "text"])
      if (is.na(val)) next
      if (grepl(.sr_lint_sql_ext_re, val, ignore.case = TRUE, perl = TRUE)) {
        add(id, "SQL string issues INSTALL/LOAD (DuckDB extensions are locked down; air-gap)")
      }
      if (grepl(.sr_lint_sql_json_re, val, ignore.case = TRUE, perl = TRUE)) {
        add(id, "SQL string calls read_json, use jsonlite::stream_in on a file connection (air-gap)")
      }
    }
    if (!length(line)) return(.sr_lint_empty())
    o <- order(line, col)
    .sr_lint_rows(f, line[o], msg[o])
  })
  do.call(rbind, c(list(.sr_lint_empty()), res))
}
