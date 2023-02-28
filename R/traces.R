
##--------------------------------------------------------------------------
##  traces.R                                                              --
##--------------------------------------------------------------------------

##----------------------------------------
##  Constructors                        --
##----------------------------------------

# new_fatools_JSON <- function(s = character()) {
#   stopifnot(is.character(s))
#   structure(s, class = c("fatools_JSON", "character"))
# }

#' Title
#'
#' @param m
#'
#' @return
#' @export
#'
#' @examples
new_fatools_matrix <- function(m = matrix()) {
  stopifnot(is.matrix(m))
  structure(m, class = c("fatools_matrix", "matrix"))
}


##----------------------------------------
##  Validators                          --
##----------------------------------------

#' Title
#'
#' @param m
#'
#' @return
#' @export
#'
#' @examples
validate_fatools_matrix <- function(m) {
  num_dim <- length(dim(m))
  num_cols <- dim(m)[2]

  if (num_dim > 2) {
    stop(
      "This fatools matrix is an array.",
      call. = FALSE
    )
  }

  if (!(num_cols == 3)) {
    stop(
      "This matrix has too many variables.
      Only three should be present: scan, bp, and RFU.
      This matrix has ", num_cols, ".",
      call. = FALSE
    )
  }

  return(m)
}


##-----------------------------------------
##  Helpers                              --
##-----------------------------------------

#' Title
#'
#' @param m
#'
#' @return
#' @export
#'
#' @examples
fatools_matrix <- function(m) {
  if (is.character(m)) {
    m <- jsonlite::fromJSON(m)
    colnames(m) <- c("scan", "bp", "RFU")
  }
  new_fatools_matrix(
    validate_fatools_matrix(m)
  )
}


##-----------------------------------------
##  Methods                              --
##-----------------------------------------

#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
tidy.fatools_matrix <- function(x, ...) {
  tibble::tibble(
    "scan" = as.numeric(x[, "scan"]),
    "bp" = as.numeric(x[, "bp"]),
    "RFU" = as.numeric(x[, "RFU"])
  )
}
