# Tests for router/state.R: the metadata-only state, masking, the budget and the payload preview.

.rs_dict <- function(variable, label) {
  n <- length(variable)
  d <- data.frame(variable = variable, label = label, type = "text", units = "", min = "", max = "",
                  stringsAsFactors = FALSE)
  d$levels <- rep(list(data.frame(code = character(), label = character(), stringsAsFactors = FALSE)), n)
  d$missing_codes <- rep(list(character()), n)
  d$source <- "test"
  d
}

# A project with `df` ingested as table `table` (no DQ run, no roles beyond `roles`).
.rs_ingested <- function(df, table = "t", roles = NULL, env = parent.frame()) {
  p <- .fx_project(env = env)
  withr::defer(sr_duck_close(p), envir = env)
  csv <- file.path(dirname(p$paths$dir), paste0(table, ".csv"))
  .fx_write_csv(df, csv)
  p <- sr_ingest(p, csv, table = table,
                 options = list(drive_info = function(path) list(free_bytes = 1e13, remote = FALSE,
                                                                 warnings = character())))
  if (length(roles)) p <- sr_roles_set(p, table, roles)
  p
}

.rs_df <- function() {
  data.frame(id = 1:4, age = c(61.5, 72.25, 48.75, 55.5), sex = factor(c("F", "M", "F", "M")),
             note = rep("SECRET-NOTE", 4), outcome = c(0, 1, 0, 1), grp = c("a", "b", "a", "b"),
             stringsAsFactors = FALSE)
}

.rs_project <- function(env = parent.frame()) {
  p <- .fx_ready_project(.rs_df(), "t", c(id = "id", outcome = "outcome", age = "covariate"), env = env)
  sr_dict_set(p, "t", .rs_dict(c("id", "age", "sex", "note", "outcome", "grp"),
                               c("Identifier", "Zebra measurement", "Quokka group", "Free note", "Result",
                                 "Arm")), source = "test")
}

# A table-scope toy node whose question needs the columns; `column_options` makes it a column choice.
.rs_col_node <- function(id = "toy_col", column_options = FALSE, env = parent.frame()) {
  nd <- sr_node(id, "Toy columns", c("x", "y"), scope = "table", facts_columns = TRUE,
                facts = function(ctx) list(purpose = "describe", tags = c("p", "q")),
                question = function(ctx) {
                  if (!column_options) {
                    return(list(instructions = "Pick x or y.", criteria = list(x = "first", y = "second")))
                  }
                  k <- sr_router_columns(ctx$project, ctx$table)$key
                  list(instructions = "Pick the outcome column.",
                       criteria = stats::setNames(as.list(rep("a column", length(k))), k))
                })
  sr_node_register(nd)
  withr::defer(.sr_node_unregister(id), envir = env)
  nd
}

test_that("counts and buckets", {
  expect_identical(.sr_router_count(c(0, 1, 4, 5, 120)), c("0", "<5", "<5", "5", "120"))
  expect_identical(.sr_router_bucket(c(0, 1, 2, 3, 9, 10, 4000)), c("0", "1", "2", "3-9", "3-9", "10+", "10+"))
})

test_that("the state holds metadata only", {
  p <- .rs_project()
  nd <- .rs_col_node()
  st <- sr_router_state(.sr_router_ctx(p, nd, "t"), mask = FALSE)
  expect_named(st, c("text", "lines", "tokens", "omitted"))
  for (bad in c("SECRET-NOTE", "F;", "M;", "61.5", "72.25", "48.75", "55.5")) {
    expect_false(grepl(bad, st$text, fixed = TRUE), info = bad)
  }
  expect_match(st$text, "rows: <5", fixed = TRUE)
  expect_match(st$text, "column.c002: name=age", fixed = TRUE)
  expect_match(st$text, "distinct=", fixed = TRUE)
  expect_match(st$text, "role=covariate", fixed = TRUE)
  expect_match(st$text, "role=unassigned", fixed = TRUE)
  expect_match(st$text, "label=Zebra measurement", fixed = TRUE)
  expect_identical(st$lines[1L], "node: toy_col")
  expect_identical(st$lines[2L], "table: t")
  expect_identical(st$lines[3L], "rows: <5")
  expect_identical(st$lines[4:5], c("fact.purpose: describe", "fact.tags: p, q"))
  expect_identical(st$omitted, 0L)
  expect_identical(st$tokens, as.integer(ceiling(nchar(st$text) / 4)))
  # a node that does not need columns carries no column lines
  nf <- sr_node("toy_nocol", "No columns", c("x", "y"), scope = "table")
  expect_false(any(grepl("^column\\.", sr_router_state(.sr_router_ctx(p, nf, "t"), FALSE)$lines)))
})

test_that("the PICO lines come after the node line", {
  p <- .rs_project()
  p <- sr_pico_set(p, population = "adults", outcome = "mortality", aim = "predict")
  nd <- .rs_col_node()
  st <- sr_router_state(.sr_router_ctx(p, nd, "t"), mask = FALSE)
  pico <- .sr_pico_text(p)
  expect_identical(st$lines[1L], "node: toy_col")
  expect_identical(st$lines[1L + seq_along(pico)], pico)
  expect_identical(st$lines[2L + length(pico)], "table: t")
})

test_that("masking hides names and labels", {
  p <- .rs_project()
  nd <- .rs_col_node()
  st <- sr_router_state(.sr_router_ctx(p, nd, "t"), mask = TRUE)
  expect_false(grepl("age", st$text, fixed = TRUE))
  for (lab in c("Identifier", "Zebra", "Quokka", "Free note", "Result", "Arm")) {
    expect_false(grepl(lab, st$text, fixed = TRUE), info = lab)
  }
  expect_match(st$text, "column.c002: name=c002; label=;", fixed = TRUE)
  cols <- sr_router_columns(p, "t", mask = TRUE)
  expect_identical(cols$name, cols$key)
  expect_identical(cols$label, rep("", 6L))
  expect_identical(sr_router_columns(p, "t", mask = FALSE)$name[2L], "age")
  # the keymap still maps the key back
  cn <- .rs_col_node("toy_colopt", column_options = TRUE)
  r <- sr_router_request(.sr_router_ctx(p, cn, "t"), mask = TRUE)
  expect_identical(r$keymap[["c002"]], "age")
  expect_false(grepl("age", r$request$state, fixed = TRUE))
  # a column-scope node's own name and label facts are masked to the key and ""
  fx <- function(name) {
    function(ctx) list(column_name = name, column_label = "Zebra measurement", column_type = "DOUBLE")
  }
  cc <- sr_node("toy_cc", "Toy column", c("u", "v"), scope = "column", columns = function(ctx) "age",
                facts = fx("age"))
  ctx <- .sr_router_ctx(p, cc, "t", "age")
  open_st <- sr_router_state(ctx, mask = FALSE)
  expect_match(open_st$text, "fact.column_name: age", fixed = TRUE)
  expect_match(open_st$text, "fact.column_label: Zebra measurement", fixed = TRUE)
  m <- sr_router_state(ctx, mask = TRUE)
  expect_false(grepl("age", m$text, fixed = TRUE))
  expect_false(grepl("Zebra", m$text, fixed = TRUE))
  expect_match(m$text, "fact.column_name: c002", fixed = TRUE)
  expect_true("fact.column_label: " %in% m$lines)
  expect_match(m$text, "fact.column_type: DOUBLE", fixed = TRUE)
  # the input hash still sees the real values
  cc2 <- sr_node("toy_cc", "Toy column", c("u", "v"), scope = "column", columns = function(ctx) "age",
                 facts = fx("c002"))
  expect_false(identical(sr_router_input_hash(ctx),
                         sr_router_input_hash(.sr_router_ctx(p, cc2, "t", "age"))))
})

test_that("odd names are escaped and single-line", {
  nm <- c("a;b=c", "two\nlines", "caf\u00e9", "plain")
  df <- stats::setNames(data.frame(1:4, 2:5, 3:6, 4:7), nm)
  p <- .rs_ingested(df)
  nd <- .rs_col_node()
  st <- sr_router_state(.sr_router_ctx(p, nd, "t"), mask = FALSE)
  cl <- grep("^column\\.", st$lines, value = TRUE)
  expect_length(cl, 4L)
  expect_false(any(grepl("\n", st$lines, fixed = TRUE)))
  expect_length(strsplit(st$text, "\n", fixed = TRUE)[[1L]], length(st$lines))
  expect_match(cl[1L], "name=a\\;b\\=c; label=;", fixed = TRUE)
  expect_match(cl[2L], "name=two lines; label=;", fixed = TRUE)
  expect_match(cl[3L], "name=caf\u00e9; label=;", fixed = TRUE)
  # the only unescaped semicolons are the field separators
  for (l in cl) {
    body <- sub("^column\\.c[0-9]+: ", "", l)
    expect_length(strsplit(gsub("\\;", "", body, fixed = TRUE), ";", fixed = TRUE)[[1L]], 5L)
  }
})

test_that("a label with a newline stays on one line", {
  p <- .rs_project()
  p <- sr_dict_set(p, "t", .rs_dict(c("id", "age"), c("line one\nline two", "a=b;c")), source = "test")
  st <- sr_router_state(.sr_router_ctx(p, .rs_col_node(), "t"), mask = FALSE)
  expect_match(st$text, "label=line one line two;", fixed = TRUE)
  expect_match(st$text, "label=a\\=b\\;c;", fixed = TRUE)
})

test_that("the token budget drops column lines and stops the column question", {
  n <- 3000L
  df <- as.data.frame(matrix(seq_len(n * 2L), nrow = 2L))
  names(df) <- sprintf("variable_with_a_longish_name_%04d", seq_len(n))
  p <- .rs_ingested(df)
  nd <- .rs_col_node()
  st <- sr_router_state(.sr_router_ctx(p, nd, "t"), mask = FALSE)
  expect_lte(st$tokens, SR_ROUTER_MAX_TOKENS)
  expect_gt(st$omitted, 0L)
  expect_identical(sum(grepl("^column\\.", st$lines)) + st$omitted, n)
  # the dropped lines are the last ones
  expect_match(st$lines[length(st$lines)], sprintf("^column\\.c%03d:", n - st$omitted))
  # the budget counts the whole text
  expect_lte(ceiling(nchar(st$text) / 4), SR_ROUTER_MAX_TOKENS)
  cn <- .rs_col_node("toy_colopt", column_options = TRUE)
  expect_null(sr_router_request(.sr_router_ctx(p, cn, "t"), FALSE))
  # a fixed-option question that needs the columns is stopped too
  expect_null(sr_router_request(.sr_router_ctx(p, nd, "t"), FALSE))
})

test_that("300 columns exceed the criteria limit", {
  n <- 300L
  df <- as.data.frame(matrix(seq_len(n * 2L), nrow = 2L))
  names(df) <- sprintf("v%03d", seq_len(n))
  p <- .rs_ingested(df)
  cn <- .rs_col_node("toy_colopt", column_options = TRUE)
  ctx <- .sr_router_ctx(p, cn, "t")
  expect_identical(sr_router_state(ctx, FALSE)$omitted, 0L)
  expect_null(sr_router_request(ctx, FALSE))
  expect_null(sr_router_preview(p, cn, "t"))
})

test_that("a node without a question has no request", {
  p <- .rs_project()
  nf <- sr_node("toy_nq", "No question", c("x", "y"), scope = "table")
  expect_null(sr_router_request(.sr_router_ctx(p, nf, "t"), FALSE))
})

test_that("fixed options map to themselves and column options to the column names", {
  p <- .rs_project()
  nd <- .rs_col_node()
  r <- sr_router_request(.sr_router_ctx(p, nd, "t"), FALSE)
  expect_identical(r$keymap, c(x = "x", y = "y"))
  cn <- .rs_col_node("toy_colopt", column_options = TRUE)
  r2 <- sr_router_request(.sr_router_ctx(p, cn, "t"), FALSE)
  expect_identical(unname(r2$keymap), c("id", "age", "sex", "note", "outcome", "grp"))
  expect_identical(names(r2$keymap), sprintf("c%03d", 1:6))
  expect_identical(names(r2$request$questions), "choice")
  expect_identical(r2$sha, sr_sysone_sha(r2$request))
})

test_that("the preview sha is the request sha", {
  p <- .rs_project()
  nd <- .rs_col_node()
  pv <- sr_router_preview(p, nd, "t")
  req <- sr_router_request(.sr_router_ctx(p, nd, "t"), FALSE)
  expect_identical(pv$sha, sr_sysone_sha(req$request))
  expect_identical(pv$body, sr_sysone_body(req$request))
  expect_identical(pv$key, "toy_col@t")
  expect_identical(pv$text, req$request$state)
  p2 <- sr_pico_set(p, population = "adults", outcome = "mortality")
  expect_false(identical(sr_router_preview(p2, nd, "t")$sha, pv$sha))
  expect_false(identical(sr_router_preview(p, nd, "t", mask = TRUE)$sha, pv$sha))
  # a project-scope node with a question and no table
  .fx_toy_nodes()
  pa <- sr_router_preview(p, "toy_a")
  expect_identical(pa$key, "toy_a")
  expect_false(grepl("table:", pa$text, fixed = TRUE))
  expect_null(sr_router_preview(p, "toy_b"))
})

test_that("the mask option is read from sr_opt", {
  p <- .rs_project()
  nd <- .rs_col_node()
  withr::local_options(sr.router_mask = TRUE)
  expect_false(grepl("name=age", sr_router_state(.sr_router_ctx(p, nd, "t"))$text, fixed = TRUE))
  expect_identical(sr_router_columns(p, "t")$name[2L], "c002")
})
