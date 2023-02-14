
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
