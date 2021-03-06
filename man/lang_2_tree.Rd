% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lang_2_tree.R
\name{lang_2_tree}
\alias{lang_2_tree}
\title{Convert language object to tree.}
\usage{
lang_2_tree(lang_obj, node_id, ni_env)
}
\arguments{
\item{lang_obj}{A language object.}

\item{node_id}{The calling node to this language object. This should only 
be greater than 0 if the \code{ni_env} already contains a partial adjacency 
list and corresponding node information. This will happen when this function 
is called recursively.}

\item{ni_env}{An environment to store the adjacency list and node information.}
}
\value{
Nothing
}
\description{
A recursive function for converting a language object to treeharp.
}
\details{
This function is used by TreeHarp constructors. It should not have
to be called by a user. It works by bulding up an adjacency list and node 
node information data frame within the supplied environment.
}
\examples{
e1 <- new.env() 
lang_2_tree(quote(X <- 1), 0, e1) 
e1$adj_list 
e1$node_info
}
