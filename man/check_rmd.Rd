% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_rmd.R
\name{check_rmd}
\alias{check_rmd}
\title{Check if a File is Rmd}
\usage{
check_rmd(fname, verbose = TRUE)
}
\arguments{
\item{fname}{A character string. It is the name of the student submission
file.}

\item{verbose}{A logical value that prints messages if a non-rmd file
is found.}
}
\value{
The function will return TRUE if all the (3) checks pass, and FALSE
otherwise.
}
\description{
Checks if a file actually is an Rmd file.
}
\details{
It runs three checks. First, it checks for the file extension to be
Rmd or rmd or any such variant. Second, it checks for a YAML header at the
beginning of file. Finally, it checks if there is at least one properly
defined R chunk within the file.
}
\seealso{
\code{\link{get_libraries}}
}
