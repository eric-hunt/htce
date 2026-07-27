
##-------------------------------------------------------------------------
##  db_connection.R                                                      --
##-------------------------------------------------------------------------

##----------------------------------------------
##  `withr`-style DuckDB connection handling  --
##----------------------------------------------

#' DuckDB Connection
#'
#' DuckDB connection handling à la `withr::local_db_connection`, backed by
#' [adbi::adbi()] (a `{DBI}`-compliant wrapper around
#' [adbcdrivermanager::adbc_connection_init()]) rather than a raw ADBC
#' connection.
#'
#' @param db_file_path a string - path to a DuckDB file
#' @param read_only a Boolean - opens the database with `access_mode = "READ_ONLY"`;
#'default is `FALSE`
#' @param .local_envir an environment - passed to [withr::defer()];
#'default is `parent.frame()`
#'
#' @return A database connection of class *AdbiConnection* (inherits from
#' `DBI::DBIConnection`).
#'
#' @seealso [adbi::adbi()], [DBI::dbConnect()], [withr::defer()],
#' [duckdb::duckdb_adbc()]
#'
#' @export
#'
with_duckdb_connection <- function(db_file_path, read_only = FALSE,
                                   .local_envir = parent.frame()) {
  requireNamespace("adbi", quietly = TRUE)
  requireNamespace("DBI", quietly = TRUE)
  requireNamespace("duckdb", quietly = TRUE)
  requireNamespace("withr", quietly = TRUE)

  con <- DBI::dbConnect(
    adbi::adbi(duckdb::duckdb_adbc()),
    path = db_file_path,
    access_mode = if (read_only) "READ_ONLY" else "READ_WRITE"
  )
  stopifnot(methods::is(con, "DBIConnection"))
  withr::defer(DBI::dbDisconnect(con), envir = .local_envir)
  return(con)
}
