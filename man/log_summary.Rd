% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/log_summary.R
\name{log_summary}
\alias{log_summary}
\title{Generate a dataframe from the log file.}
\usage{
log_summary(log_file)
}
\arguments{
\item{log_file}{The name of the log file generated from 
\code{\link{render_one}}.}
}
\value{
The function returns a dataframe summarising the details in the
log file.
}
\description{
Generate a dataframe from the log file.
}
\details{
This provides a table view of the log file, which is updated 
in a more natural format by simply concatenating new updates. The output 
of this function makes it easier to group entries by filename, time, or
status, or even error message.

The output table does not contain correctness output. It only contains the 
columns name, timestamp, status (SUCCESS/FAIL), error message, 
number of libraries used and number of libraries installed.
}
\seealso{
\code{\link{render_one}}
}
