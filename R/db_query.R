
##-------------------------------------------------------------------------
##  database_access.R                                                    --
##-------------------------------------------------------------------------



##----------------------------------------
##  Query DuckDB                        --
##----------------------------------------

#' DuckDB Query
#'
#' Execute or collect results from a query to a DuckDB database.
#'
#' @param query_string A string - SQL query
#' @param ... Key-value pairs - additional injected values passed to [glue::glue_sql()]
#' @param .quiet A Boolean - supress console messages?
#' @param .db_loc A string - local path to the DuckDB file
#' @param .db_con A valid DBIConnection object
#' @param .pg_install A Boolean - install the DuckDB Postgres extension?
#' @param .pg_load A Boolean - use the DuckDB Postgres extension?
#'
#' @name db_query



##-----------------------
##  Execute query      --
##-----------------------

#' @rdname db_query
#'
#' @details `dkdb_execute` executes a query without collecting results.
#'
#' @export
#'
dkdb_execute <- function(query_string, ..., .quiet = TRUE,
                         .db_loc = "./htCE.duckdb", .db_con = NULL,
                         .pg_install = FALSE, .pg_load = FALSE) {

  if (is.null(.db_con)) {
    db <- with_duckdb_connection(.db_loc)
  } else {
    db <- .db_con
  }

  if (!.quiet) {
    print(db)
    cat("\n")
  }

  if (is.null(.db_con) & !.quiet) {
    withr::defer(
      cat("\nConnection closed? ", !DBI::dbIsValid(db), "\n"),
      priority = "last"
    )
  }

  varargs <- rlang::dots_list(
    ...,
    .named = TRUE,
    .ignore_empty = "all",
    .homonyms = "first"
  )

  statement <- rlang::inject(
    glue::glue_sql(
      query_string,
      !!!varargs,
      .con = db
    )
  )

  if (!.quiet) {
    print(statement)
  }

  if (.pg_install) {
    DBI::dbExecute(db, "INSTALL postgres_scanner;")
  }

  if (.pg_load) {
    DBI::dbExecute(db, "LOAD postgres_scanner;")
  }

  if (!.quiet) {
    cat("\nExecuting query..\n\n")
  }

  DBI::dbExecute(db, statement)
}



##-----------------------
##  Collect query      --
##-----------------------

#' @rdname db_query
#'
#' @details `dkdb_collect` collects the results from a query and returns them as a `tibble`.
#'
#' @returns A `tibble` containing collected results.
#'
#' @export
#'
dkdb_collect <- function(query_string, ..., .quiet = TRUE,
                         .db_loc = "./htCE.duckdb", .db_con = NULL,
                         .pg_install = FALSE, .pg_load = FALSE) {

  if (is.null(.db_con)) {
    db <- with_duckdb_connection(.db_loc)
  } else {
    db <- .db_con
  }

  if (!.quiet) {
    print(db)
    cat("\n")
  }

  if (is.null(.db_con) & !.quiet) {
    withr::defer(
      cat("\nConnection closed? ", !DBI::dbIsValid(db), "\n"),
      priority = "last"
    )
  }

  varargs <- rlang::dots_list(
    ...,
    .named = TRUE,
    .ignore_empty = "all",
    .homonyms = "first"
  )

  statement <- rlang::inject(
    glue::glue_sql(
      query_string,
      !!!varargs,
      .con = db
    )
  )

  if (!.quiet) {
    print(statement)
  }

  if (.pg_install) {
    DBI::dbExecute(db, "INSTALL postgres_scanner;")
  }

  if (.pg_load) {
    DBI::dbExecute(db, "LOAD postgres_scanner;")
  }

  if (!.quiet) {
    cat("\nCollecting the query results..\n\n")
  }

  DBI::dbGetQuery(db, statement) |>
    # don't initially error out if duplicates exist at tibble creation
    tibble::as_tibble(.name_repair = "minimal") |>
    # drop the duplicates if both column name and content are duplicated
    {\(df) df[!(duplicated(colnames(df)) & duplicated(as.list(df)))]}()
}
