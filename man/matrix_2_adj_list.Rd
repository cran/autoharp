% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_routines.R
\name{matrix_2_adj_list}
\alias{matrix_2_adj_list}
\title{Convert adjacency matrix to a list.}
\usage{
matrix_2_adj_list(mat)
}
\arguments{
\item{mat}{A symmetric matrix of 1's and 0's, with 1 in entry (i,j)
representing an edge between the two vertices.}
}
\value{
The adjacency list of the tree.
}
\description{
Converts a binary matrix that represents a tree into an adjacency list.
}
\details{
Remember that the list that is finally output is for a tree, not a general
graph. Please see other help pages for more specifications.

The input matrix should be BFS ordered. The adjacency list only notes the
child node(s) of a particular node. If a matrix denotes multiple parents,
it will not be picked up.

This is a low-level function, used within the S4 class TreeHarp. It is not
generally meant for use by the user.
}
