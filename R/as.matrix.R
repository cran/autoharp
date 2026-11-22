#' @include treeharp.R th_getter-length.R
NULL

#' TreeHarp Cast a TreeHarp to Matrix.
#'
#' Convert a treeharp object to an adjacency matrix.
#'
#' @param from  A treeharp object.
#' @param to The class to convert to; always \code{"matrix"} here.
#' @param strict Not used; it comes from the \code{coerce()} generic.
#'
#' @name as.matrix
#' @aliases coerce,TreeHarp,matrix-method
#' @return A matrix.
#'
setAs("TreeHarp", "matrix",
      function(from) {
        mat <- adj_list_2_matrix(from@adjList)
        mat
      })
