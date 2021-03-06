% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{extract_chunks}
\alias{extract_chunks}
\title{Extract chunks that match a pattern.}
\usage{
extract_chunks(rmd_name, pattern)
}
\arguments{
\item{rmd_name}{A character string, the name of the rmd file to get the
chunks from.}

\item{pattern}{The pattern to match within the label. In fact, the match is
applied to the whole chunk option.}
}
\value{
A list of character vectors. Each vector contains the chunk from the
file. If no pattern is specified, all chunks are returned. Remember that
the chunk header and tail are also included in the returned list.
}
\description{
Extracts chunks whose labels match a pattern from the rmd file.
}
