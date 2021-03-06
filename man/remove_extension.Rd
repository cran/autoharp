% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{remove_extension}
\alias{remove_extension}
\title{Obtains the Root File Name, without Extension.}
\usage{
remove_extension(fname)
}
\arguments{
\item{fname}{A character string of the filename, with the extension present.}
}
\value{
A character string, with the extension removed.
}
\description{
This function hard codes some of the common extensions that we deal with.
}
\details{
If none of the known extensions knit.md, utf8.md, R or Rmd are 
found, then the last period onwards are removed. See the examples.

If no extensions are found, the original filename is returned.
}
\examples{

remove_extension("test.Rmd")
remove_extension("test.knit.md")
remove_extension("test.r.txt")
remove_extension("test_no_extension")

}
