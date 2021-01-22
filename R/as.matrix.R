#' @include treeharp.R th_getter-length.R
NULL

#' TreeHarp Cast a TreeHarp to Matrix.
#'
#' Convert a treeharp object to an adjacency matrix.
#'
#' @param from  A treeharp object.
#'
#' @name as.matrix
#' @return A matrix.
#'
setAs("TreeHarp", "matrix",
      function(from) {
        mat <- adj_list_2_matrix(from@adjList)
        mat
      })
