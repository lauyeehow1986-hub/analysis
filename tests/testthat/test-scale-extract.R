# The stream extract (P1e.1 Task 4). Synthetic data only.

.sx_df <- function(n = 300L) {
  set.seed(7L)
  data.frame(id = seq_len(n),
             y = stats::rbinom(n, 1L, 0.4),
             sex = sample(c("M", "F"), n, replace = TRUE),
             x1 = ifelse(stats::runif(n) < 0.1, NA, round(stats::rnorm(n), 1)),
             grp = ifelse(stats::runif(n) < 0.1, NA, sample(c("A", "b", "C"), n, replace = TRUE)),
             stringsAsFactors = FALSE)
}

.sx_project <- function(env = parent.frame()) {
  .fx_ready_project(.sx_df(), "t", c(id = "id", y = "outcome", sex = "exposure"), env = env)
}

.sx_read <- function(p, ex) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  path <- file.path(p$paths$dir, ex$rel)
  DBI::dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s') ORDER BY \".sr_row\"", path))
}

.sx_ok_drive <- function(path) list(free_bytes = 1e13, remote = FALSE, warnings = character())

test_that(".sr_row equals the .sr_extract_frame position", {
  p <- .sx_project()
  cols <- c("sex", "x1", "grp", "y")   # ties and NAs in the sort columns
  ex <- sr_extract_stream(p, "t", cols, drive_info = .sx_ok_drive)
  expect_identical(ex$kind, "parquet")
  expect_identical(ex$cols, cols)
  df <- .sx_read(p, ex)
  ref <- .sr_extract_frame(p, "t", cols)
  expect_equal(ex$n, nrow(ref))
  expect_equal(as.numeric(df$.sr_row), seq_len(nrow(ref)))
  a <- df[cols]; b <- ref[cols]
  rownames(a) <- NULL; rownames(b) <- NULL
  expect_identical(a, b)
})

test_that("the name is the streaming SHA-256 and a rerun reuses it", {
  p <- .sx_project()
  ex <- sr_extract_stream(p, "t", c("sex", "x1", "y"), drive_info = .sx_ok_drive)
  full <- file.path(p$paths$dir, ex$rel)
  expect_identical(ex$rel, paste0("data/extracts/", ex$sha, ".parquet"))
  expect_true(ex$sha == as.character(openssl::sha256(file(full))))
  expect_equal(ex$bytes, file.size(full))
  ex2 <- sr_extract_stream(p, "t", c("sex", "x1", "y"), drive_info = .sx_ok_drive)
  expect_identical(ex2$rel, ex$rel)
  d <- file.path(p$paths$dir, "data", "extracts")
  expect_length(list.files(d, pattern = "^[.]tmp-.*[.]parquet$", all.files = TRUE), 0L)
})

test_that("space refusal before writing", {
  p <- .sx_project()
  d <- file.path(p$paths$dir, "data", "extracts")
  before <- list.files(d, all.files = TRUE)
  low <- function(path) list(free_bytes = 1, remote = FALSE, warnings = character())
  expect_error(sr_extract_stream(p, "t", c("sex", "y"), drive_info = low),
               "not enough free space for the stream extract: about [0-9.]+ GB needed, [0-9.]+ GB free")
  expect_identical(list.files(d, all.files = TRUE), before)
  expect_equal(.sr_extract_stream_need(10, 8), 10 * 8 * 1.35 + 1024^3)
})

test_that("a COPY error leaves no temp file", {
  p <- .sx_project()
  .ui_stub(".sr_extract_copy", function(...) stop("boom"))
  expect_error(sr_extract_stream(p, "t", c("sex", "y"), drive_info = .sx_ok_drive), "boom")
  d <- file.path(p$paths$dir, "data", "extracts")
  expect_length(list.files(d, pattern = "^[.]tmp-", all.files = TRUE), 0L)
})

test_that(".sr_extract_order_sql orders by every quoted column", {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  expect_identical(.sr_extract_order_sql(con, c("a", "b c")), "ORDER BY a, \"b c\"")
})
