% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tree_routines.R
\name{find_branch_num}
\alias{find_branch_num}
\title{Find the branch that leads from one node to another.}
\usage{
find_branch_num(th, child_id, ancestor_id)
}
\arguments{
\item{th}{A TreeHarp object.}

\item{child_id}{An integer node id. It corresponds to the node to trace up
from.}

\item{ancestor_id}{An integer node id. It corresponds to the node to trace 
down from.}
}
\value{
An integer that denotes the branch to follow down (from the ancestor)
to reach the child.
}
\description{
Given two nodes that are on the same path to the root, this function
determines the branch that leads to the child node.
}
\details{
This is used when trying to find a sub-call from a TreeHarp object.
It is useful in determining the indices to use when extracting the sub-call.
}
\examples{
ex3 <- quote(x <- f(y = g(3, 4), z=1L))
t1 <- TreeHarp(ex3, TRUE)
find_branch_num(t1, 8, 3) # should be 1
find_branch_num(t1, 5, 3) # should be 2
}
