
##--------------------------------------------------------------------------
##  db_helpers.R                                                          --
##--------------------------------------------------------------------------

##-----------------------------------------
##  Get existing barcodes                --
##-----------------------------------------

#' Title
#'
#' @param source_only A Boolean - only retrieve source plate barcodes
#' @param .db_loc A string - local path to the DuckDB file; passed to [dkdb_collect()]
#' @param .db_con A valid DBIConnection object; passed to [dkdb_collect()]
#'
#' @return A named character vector of barcodes that exist in the database
#' @export
#'
existing_barcodes <- function(source_only = FALSE, .db_loc = "./htCE.duckdb",
                              .db_con = NULL) {
  boolset <- c(TRUE, FALSE)
  if (source_only) {
    boolset <- c(TRUE)
  }

  htce::dkdb_collect(
    "SELECT barcode FROM plates
    WHERE is_src_plate IN ({vals*});",
    vals = boolset,
    .db_loc = .db_loc,
    .db_con = .db_con,
  )[[1]] |>
    purrr::set_names()
}



##----------------------------------------
##  Get 'plate_id'                      --
##----------------------------------------

#' Retrieve a plate_id from a barcode string
#'
#' @param barcode A string - the unique barcode identifying the physical plate
#' @param ...
#' @param .db_loc A string - local path to the DuckDB file; passed to [dkdb_collect()]
#' @param .db_con A valid DBIConnection object; passed to [dkdb_collect()]
#' @param .pg_load A Boolean - use the DuckDB Postgres extension?; passed to [dkdb_collect()]
#'
#' @return A double value representing the `plate_id` integer primary key which corresponds to the barcode argument
#' @export
#'
barcode_to_plate_id <- function(barcode, ..., .db_loc = "./htCE.duckdb",
                                .db_con = NULL, .pg_load = FALSE) {
  dkdb_collect(
    "SELECT id
    FROM plates
    WHERE barcode = {barcode};",
    barcode = barcode,
    ...,
    .db_loc = .db_loc,
    .db_con = .db_con,
    .pg_load = .pg_load
  )[[1]]# |>
  # bit64::as.integer64()
}



##-----------------------------------------
##  Get ebase 'well_id'                  --
##-----------------------------------------

#' Retrieve a well_id from barcode and address strings
#'
#' @param barcode A string - the unique barcode identifying the physical plate
#' @param address A string - the well identifier (e.g. A1, H12, etc.)
#' @param .db_loc A string - local path to the DuckDB file; passed to [dkdb_collect()]
#' @param .db_con A valid DBIConnection object; passed to [dkdb_collect()]
#' @param .pg_load A Boolean - use the DuckDB Postgres extension?; passed to [dkdb_collect()]
#'
#' @return A double value representing the `well_id` integer primary key which corresponds to the barcode and address arguments
#' @export
#'
address_to_well_id <- function(barcode, address, .db_loc = "./htCE.duckdb",
                               .db_con = NULL, .pg_load = FALSE) {
  dkdb_collect(
    "SELECT w.id
    FROM wells w
    WHERE EXISTS (
      SELECT *
      FROM plates p
      WHERE p.barcode = {barcode}
        AND p.id = w.plate_id
    )
      AND w.address = {address};",
    barcode = barcode,
    address = address,
    .db_loc = .db_loc,
    .db_con = .db_con,
    .pg_load = .pg_load
  )[[1]]# |>
  # bit64::as.integer64()
}
