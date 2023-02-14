
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
