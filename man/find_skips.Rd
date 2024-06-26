% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{find_skips}
\alias{find_skips}
\title{Find and return skip values}
\usage{
find_skips(file_path)
}
\arguments{
\item{file_path}{A string - the path to the .csv delimited file to read}
}
\value{
A named list of skip values to use during Echo transfer report import
}
\description{
Returns a named list of skip values for importing Echo transfer report metadata and transfer data.
}
