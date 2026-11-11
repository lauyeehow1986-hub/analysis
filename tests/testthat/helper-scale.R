# Shared helpers for the streaming-engine tests (P1e.1). Synthetic data only.

# A synthetic analysis table: clusters of 1-9 rows (so they cross chunk boundaries), a 0/1 outcome `y`, a
# continuous outcome `yc`, a tie-free `x2`, a heavily tied `xt`, text `grp` with a mixed-case and a
# non-ASCII level, `late` whose level "z" appears only in the last 5 rows, and the exposure `sex`.
.sc_frame <- function(n = 20000L, seed = 1L) {
  set.seed(seed)
  n <- as.integer(n)
  sizes <- sample.int(9L, n, replace = TRUE)
  pid <- rep(seq_along(sizes), sizes)[seq_len(n)]
  x1 <- stats::rnorm(n)
  x2 <- stats::rgamma(n, shape = 2, rate = 1)
  sex <- sample(c("M", "F"), n, replace = TRUE)
  y <- stats::rbinom(n, 1L, stats::plogis(-0.5 + 0.4 * x1 + 0.3 * (sex == "M")))
  yc <- 1 + 0.5 * x1 + 0.3 * (sex == "M") + stats::rnorm(n)
  late <- sample(c("a", "b"), n, replace = TRUE)
  late[max(1L, n - 4L):n] <- "z"
  data.frame(id = seq_len(n), pid = pid, y = y, yc = yc, x1 = x1, x2 = x2,
             xt = round(stats::runif(n) * 5),
             grp = sample(c("A", "b", "C", "\u00e9"), n, replace = TRUE),
             late = late, sex = sex, stringsAsFactors = FALSE)
}

# A locked project with one primary analysis a1 on route `route` over .sc_frame(). `spec` is the route
# spec (a list); the roles are id, y (outcome) and sex (exposure). Mirrors .run_locked of helper-run.R.
.sc_locked <- function(spec, route = "logistic", env = parent.frame(), df = .sc_frame(2000L)) {
  p <- .fx_ready_project(df, "t", c(id = "id", y = "outcome", sex = "exposure"), env = env)
  p <- sr_sap_add(p, "a1", route, spec, "t", label = "primary")
  sr_sap_lock(.fx_feasible(p))
}

# Write `df` plus a .sr_row column (1..n, unless df has one) to a temp parquet; returns the path.
.sc_parquet <- function(df, env = parent.frame()) {
  path <- tempfile(fileext = ".parquet")
  withr::defer(unlink(path), envir = env)
  if (is.null(df$.sr_row)) df$.sr_row <- seq_len(nrow(df))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  duckdb::duckdb_register(con, "sc_df", df)
  DBI::dbExecute(con, sprintf("COPY sc_df TO '%s' (FORMAT PARQUET)", gsub("'", "''", path)))
  path
}

# A locked-down connection from the inlined streaming helpers (.h_st_open, Task 5), closed on defer.
.sc_con <- function(path, env = parent.frame()) {
  con <- .h_st_open(path, "2GB", 2L, tempdir())
  withr::defer(try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE), envir = env)
  con
}
