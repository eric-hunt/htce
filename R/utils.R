
##-------------------------------------------------------------------------
##  utilities.R                                                          --
##-------------------------------------------------------------------------

##----------------------------------------
##  Number of rows                      --
##----------------------------------------

#' Return the number of lines in a file
#'
#' @param path a string - path to file
#' @param remove_header a Boolean - should the first row be ignored?
#' @param ignore_trailing a Boolean - should a trailing '\n' be ignored?
#'
#' @return
#' @export
#'
count_newlines <- function(path, remove_header = TRUE, ignore_trailing = TRUE) {

  assertthat::assert_that(
    is.logical(remove_header), msg = "`remove_header` must be true or false."
  )
  assertthat::assert_that(
    is.logical(ignore_trailing), msg = "`ignore_trailing` must be true or false."
  )

  linecount <- R.utils::countLines(path)
  returncount <- as.vector(linecount)

  if (attr(linecount, "lastLineHasNewline") & ignore_trailing) {
    returncount <- returncount - 1
  }

  if (remove_header) {
    returncount <- returncount - 1
  }

  # attributes(returncount) <-  NULL # as.vector removes attributes

  assertthat::assert_that(
    identical(returncount, 384, ignore.environment = TRUE),
    msg = glue::glue("File {path} does not contain 384 rows. ",
                     "Something might be wrong with the file.")
  )

  as.integer(returncount)
}



##-----------------------------------------
##  Generate well address order          --
##-----------------------------------------

#' Create a vector of well addresses
#'
#' @param num_wells An integer - the number of wells in the plate format (96 or 384)
#' @param pad A Boolelan - whether or not to pad the column numbers with zeros (e.g. A1 vs. A01)
#'
#' @return A character vector of well addresses in top to bottom, left to right order (e.g. for 96 wells, A1 -> H1, A2...H11, A12 -> H12)
#' @export
#'
make_well_order <- function(num_wells, pad = FALSE) {
  assertthat::assert_that(num_wells %in% c(96, 384),
                          msg = "number of wells should be 96 or 384")
  if (num_wells == 96) {
    well_letters <- rep(c(LETTERS[1:8]), 12)
    well_numbers <- purrr::flatten_chr(
      purrr::map(as.character(c(1:12)), rep, 8)
    )
  } else if (num_wells == 384) {
    well_letters <- rep(c(LETTERS[1:16]), 24)
    well_numbers <- purrr::flatten_chr(
      purrr::map(as.character(c(1:24)), rep, 16)
    )
  }

  if (pad) {
    well_numbers <- stringr::str_pad(well_numbers, 2, "left", "0")
  }

  well_order <- purrr::map2_chr(
    well_letters,
    well_numbers,
    paste0
  )

  return(well_order)
}



##----------------------------------------
##  Find skip values                    --
##----------------------------------------

#' Find and return skip values
#'
#' Returns a named list of skip values for importing Echo transfer report metadata and transfer data.
#'
#' @param file_path A string - the path to the .csv delimited file to read
#'
#' @return A named list of skip values to use during Echo transfer report import
#' @export
#'
find_skips <- function(file_path) {
  skips <- c()

  skip_search <- function(path, pattern) {
    tryCatch(
      min(grep(pattern = pattern, x = readr::read_lines(file_path))),
      error = function(e) NULL,
      warning = function(w) NULL,
      message = function(m) NULL
    )
  }

  skips$header <- skip_search(file_path, "Run ID") - 1
  skips$exceptions <- skip_search(file_path, "\\[EXCEPTIONS\\]")
  skips$details <- skip_search(file_path, "\\[DETAILS\\]")
  skips$footer <- skip_search(file_path, "Instrument Name") - 1

  skips
}
