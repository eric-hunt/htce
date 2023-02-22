
##-------------------------------------------------------------------------
##  utilities.R                                                          --
##-------------------------------------------------------------------------

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



##-----------------------------------------
##  Generate submission files            --
##-----------------------------------------

#' Create CE submission files
#'
#' Creates tab-delimited files for submitting CE plates to the NEB sequencing core via the LIMS system.
#'
#' @param initials A string - your initials
#' @param datetime A date - use `base::as.Date` to convert a string to a date
#' @param plate_code An integer - a code representing the type of plate
#' @param num_wells An integer - 96 or 384, the number of wells on the submission plate
#' @param num_plates An integer - the number of plate submission files to create
#' @param dest_dir A string - the directory to create the tab-delimited submission files
#' @param .pad_well_num A Boolean - passed to [htce::make_well_order()] *pad* argument
#'
#' @export
#'
make_submission_files <- function(initials = "EH", datetime = NULL,
                                 plate_code = 5,
                                 num_wells = 96, num_plates = 12,
                                 dest_dir = NULL, .pad_well_num = FALSE) {

  assertthat::assert_that(num_wells %in% c(96, 384),
                          msg = "number of wells should be 96 or 384")

  assertthat::assert_that(plate_code %in% c(1:6),
                          msg = "plate code must be in c(1:6)")

  assertthat::assert_that(!is.null(dest_dir),
                          msg = "must provide destination directory to write to")
  assertthat::assert_that(fs::dir_exists(dest_dir),
                          msg = "destination directory does not exist")

  datetime_format <- "%y%m%d"
  datetime_to_string <- function(datetime_obj) {
    format(datetime_obj, datetime_format)
  }

  if (is.null(datetime))
    datetime <- datetime_to_string(lubridate::today())
  else
    datetime <- datetime_to_string(datetime)

  well_locs <- make_well_order(num_wells = num_wells, pad = .pad_well_num)

  if (num_wells == 96) {
    sample_str_padding <- list(width = 2, side = "left", pad = "0")
  } else if (num_wells == 384) {
    sample_str_padding <- list(width = 3, side = "left", pad = "0")
  }

  sample_nums <- rlang::inject(
    stringr::str_pad(
      as.character(1:num_wells),
      !!!sample_str_padding
    )
  )

  if (num_plates > 99) {
    plate_str_padding <- list(
      width = floor(log10(num_plates)) + 1,
      side = "left",
      pad = "0"
    )
  } else {
    plate_str_padding <- list(
      width = 2,
      side = "left",
      pad = "0"
    )
  }

  sample_prefixes <- purrr::map_chr(
    rlang::inject(
      stringr::str_pad(
        as.character(seq_along(1:num_plates)),
        !!!plate_str_padding
      )
    ),
    function(plate_num) {
      paste(initials,
            as.character(plate_code),
            as.character(datetime),
            plate_num,
            sep = "_"
      )
    }
  )

  submission_dfs <- purrr::map(
    sample_prefixes,
    function(prefix) {
      submission_df <- tibble::tibble(
        `Well` = well_locs,
        `Sample_Name` = purrr::map_chr(
          sample_nums,
          \(num) paste(prefix, num, sep = "-")
        )
      )
    }
  )

  purrr::walk2(
    submission_dfs,
    sample_prefixes,
    function(df, filename) {
      readr::write_tsv(df, fs::path(dest_dir, paste0(filename, ".txt")))
    }
  )
}



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
