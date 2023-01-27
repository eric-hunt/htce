
##-------------------------------------------------------------------------
##  db_connection.R                                                      --
##-------------------------------------------------------------------------

##----------------------------------------------
##  `withr`-style DuckDB connection handling  --
##----------------------------------------------

#' DuckDB Connection
#'
#' DuckDB connection handling à la `withr::local_db_connection`.
#'
#' @param db_file_path a string - path to a DuckDB file
#' @param read_only a Boolean - passed to [DBI::dbConnect()];
#'default is `FALSE`
#' @param .local_envir an environment - passed to [withr::defer()];
#'default is `parent.frame()`
#'
#' @return A database connection of class *DBIConnection*.
#'
#' @seealso [DBI::dbConnect()], [withr::defer()], [withr::local_db_connection()],
#' [duckdb/duckdb#5525](https://github.com/duckdb/duckdb/pull/5525)
#'
#' @export
#'
with_duckdb_connection <- function(db_file_path, read_only = FALSE,
                                   .local_envir = parent.frame()) {
  requireNamespace("DBI", quietly = TRUE)
  requireNamespace("duckdb", quietly = TRUE)
  requireNamespace("withr", quietly = TRUE)
  con <- DBI::dbConnect(
    duckdb::duckdb(),
    dbdir = db_file_path,
    read_only = read_only
  )
  stopifnot(methods::is(con, "DBIConnection"))
  withr::defer(DBI::dbDisconnect(con, shutdown = TRUE), envir = .local_envir)
  return(con)
}
