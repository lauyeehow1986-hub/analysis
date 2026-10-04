# synthetic fixture: DuckDB extension provisioning and JSON reading written as SQL (never sourced)
f <- function(con) {
  DBI::dbExecute(con, "INSTALL httpfs")
  DBI::dbExecute(con, "force install httpfs")
  DBI::dbExecute(con, "  LOAD 'x.duckdb_extension'")
  DBI::dbGetQuery(con, "SELECT * FROM read_json('a.json')")
  ok <- "SELECT 1"
}
