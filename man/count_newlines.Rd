% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{count_newlines}
\alias{count_newlines}
\title{Return the number of lines in a file}
\usage{
count_newlines(path, remove_header = TRUE, ignore_trailing = TRUE)
}
\arguments{
\item{path}{A string - path to file}

\item{remove_header}{A Boolean - should the first row be ignored?}

\item{ignore_trailing}{A Boolean - should a trailing '\n' be ignored?}
}
\value{
An integer - the number of lines in the file
}
\description{
Return the number of lines in a file
}
