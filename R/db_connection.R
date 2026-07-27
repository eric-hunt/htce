
##-------------------------------------------------------------------------
##  db_connection.R                                                      --
##-------------------------------------------------------------------------

##----------------------------------------------
##  `withr`-style DuckDB connection handling  --
##----------------------------------------------

#' DuckDB Connection
#'
#' DuckDB connection handling à la `withr::local_db_connection`, backed by
#' [adbcdrivermanager::adbc_connection_init()] rather than `DBI::dbConnect()`.
#'
#' @param db_file_path a string - path to a DuckDB file
#' @param read_only a Boolean - opens the database with `access_mode = "READ_ONLY"`;
#'default is `FALSE`
#' @param .local_envir an environment - passed to [adbcdrivermanager::local_adbc()];
#'default is `parent.frame()`
#'
#' @return A database connection of class *adbc_connection*.
#'
#' @seealso [adbcdrivermanager::adbc_connection_init()], [adbcdrivermanager::local_adbc()],
#' [duckdb::duckdb_adbc()]
#'
#' @export
#'
with_duckdb_connection <- function(db_file_path, read_only = FALSE,
                                   .local_envir = parent.frame()) {
  requireNamespace("adbcdrivermanager", quietly = TRUE)
  requireNamespace("duckdb", quietly = TRUE)

  db <- adbcdrivermanager::adbc_database_init(
    duckdb::duckdb_adbc(),
    path = db_file_path,
    access_mode = if (read_only) "READ_ONLY" else "READ_WRITE"
  )
  adbcdrivermanager::local_adbc(db, .local_envir = .local_envir)

  con <- adbcdrivermanager::adbc_connection_init(db)
  adbcdrivermanager::local_adbc(con, .local_envir = .local_envir)

  stopifnot(methods::is(con, "adbc_connection"))
  return(con)
}
