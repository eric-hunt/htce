
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
#' @param .quiet A Boolean - suppress console messages?
#' @param .db_loc A string - local path to the DuckDB file
#' @param .db_con A valid DBIConnection object
#' @param .pg_install A Boolean - install the DuckDB Postgres extension?
#' @param .pg_load A Boolean - use the DuckDB Postgres extension?
#' @param .return A string - how collected data should be returned, default is a `data.frame` (alias *df*, see [DBI::dbGetQuery()]), but can also be coerced to a `tibble` (alias *tbl*, see [tibble::as_tibble()]) or `data.table` (alias *dt*, see [data.table::as.data.table()])
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
#' @returns A `data.frame` (default), `tibble`, or `data.table` containing the collected query results
#'
#' @export
#'
dkdb_collect <- function(query_string, ..., .quiet = TRUE,
                         .db_loc = "./htCE.duckdb", .db_con = NULL,
                         .pg_install = FALSE, .pg_load = FALSE,
                         .return = NULL) {
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

  if (!is.null(.return)) {
    .return <- match.arg(
      .return,
      choices = c("data.frame", "df", "tibble", "tbl", "data.table", "dt")
    )
  } else {
    .return <- getOption("htce.tabular_class")
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

  collected <- DBI::dbGetQuery(db, statement)

  if (.return %in% c("tibble", "tbl")) {
    collected |>
      # don't initially error out if duplicates exist at tibble creation
      tibble::as_tibble(.name_repair = "minimal")
  } else if (.return %in% c("data.table", "dt")) {
    collected |>
      data.table::as.data.table()
  } else if (.return %in% c("data.frame", "df")) {
    as.data.frame(collected)
  } else {
    as.data.frame(collected)
  }
}
