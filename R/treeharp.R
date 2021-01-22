#' An R expression as a tree.
#'
#' This class is used to represent a \emph{single} R expression as a tree.
#'
#' @slot adjList The adjacency list of the tree. The list must be named. The
#'   nodes should be labelled in Breadth-First Order. The first component must
#'   be the root of the tree. Leaves of the tree should be NULL elements.
#' @slot nodeTypes A data frame describing the type of node. The columns in the 
#'   data frame will be derived from the expression used to instantiate the 
#'   object. The column names will be id (node id), name, call_status, 
#'   formal_arg and depth. This slot can be left missing (i.e., populated with 
#'   NA). This latter feature is useful when we just wish to test something out.
#'   
#'   This slot is only populated automatically when an R expression is provided 
#'   as \code{lang_obj} and \code{quote_arg} is TRUE.
#' @slot repr A string representation of the tree. This will be printed when the 
#'   show method of TreeHarp is called.
#' @slot call The language object that was used to construct the tree (if it 
#' was). If the object was constructed from a list/matrix, this will be NA.
#'
#' @importFrom methods setClass new setValidity as show slot
#' @name TreeHarp-class
#' @rdname TreeHarp-class
#' @details The following validity checks are conducted on the object:
#' \enumerate{
#' \item Is the graph connected? If no, the object is invalid.
#' \item Are there cycles? If yes, the object is invalid.
#' \item Are the nodes labelled in a BFS ordering? If not, the object is not 
#' valid.
#' }
#' 
#' @export 
#'
#' @examples 
#' l1 <- list(a=c(2,3), b=NULL, c=NULL)
#' # directly using new() 
#' treeharp1 <- new("TreeHarp", adjList = l1, nodeTypes = NA)
#' 
#' # using one of the constructor methods (for lists)
#' treeharp2 <- TreeHarp(l1)
#' 
#' # using the constructor for matrices.
#' m1 <- matrix(0L, 3, 3)
#' dimnames(m1) <- list(letters[1:3], letters[1:3])
#' m1[1, ] <- c(0, 1L, 1L)
#' m1[, 1] <- c(0, 1L, 1L)
#' treeharp3 <- TreeHarp(m1)
#' 
#' # Supplying a language object to get the same tree (with nodeTypes 
#' # populated)
#' ex1 <- quote(a(b,c))
#' TreeHarp(ex1, TRUE)
#' 
TreeHarp <- setClass("TreeHarp",
                     slots = c(adjList = "list", nodeTypes = "ANY", 
                               repr="character", call="ANY"),
                     prototype = list(adjList = list(a = NULL), nodeTypes = NA,
                                      repr="", call=NA)
)

#' @return Constructors return an object of class TreeHarp.
#' @rdname TreeHarp-class
#' @export
setGeneric("TreeHarp", function(lang_obj, quote_arg, ...) standardGeneric("TreeHarp"),
           signature = "quote_arg")

#' @describeIn TreeHarp A constructor for TreeHarp.
#'
#' Converts either adjacency list or matrix into a TreeHarp object.
#'
#' @param lang_obj This should be an adjacency list for a tree (not a graph), or the
#' adjacency matrix of a tree, or the expression to be parsed. If it is a list,
#' only child nodes should be indicated (see the examples).
#' @param quote_arg If this argument is missing or FALSE, then the class of 
#' \code{lang_obj} will be evaluated, and, if it is either a list or matrix, 
#' the TreeHarp object will be returned. 
#' 
#' If this argument is TRUE, the \code{lang_obj} argument will be quoted and a 
#' parse tree for the expression will be computed and used as the tree.
#' @param ... Unused at the moment.
#'
#' @importFrom rlang enexpr
#'
#' @export
setMethod("TreeHarp", signature=c(quote_arg="logical"),
          function(lang_obj, quote_arg, ...) {
            if(!quote_arg){
              if("list" %in% class(lang_obj)){
                out <- list_2_tree(lang_obj)
                return(out)
              } else if("matrix" %in% class(lang_obj)){
                out <- mat_2_tree(lang_obj)
                return(out)
              } else {
                stop("Not sure what to do with this.\n")
              }
            } else {
              # ex1 <- enexpr(lang_obj)
              env1 <- new.env()
              lang_2_tree(lang_obj, 0, env1)
              out_list <- to_BFS(env1$adj_list, env1$node_info)
              out <- new("TreeHarp", adjList = out_list[[1]],
                         nodeTypes = out_list[[2]], repr=deparse(lang_obj),
                         call=lang_obj)
              return(out)
            }
})

#' @describeIn TreeHarp A constructor for TreeHarp.
#' 
#' Converts language object into a TreeHarp object.
#' 
#' @export
setMethod("TreeHarp", signature=c(quote_arg="missing"),
          function(lang_obj, quote_arg, ...) {
            TreeHarp(lang_obj, FALSE)
          })

list_2_tree <- function(x) {
  # check all components named.
  name_lens <- nchar(names(x))
  nonzero_lens <- sum(name_lens > 0)
  if(nonzero_lens != length(x))
    stop("Not all nodes named!")

  # check at least one null.
  null_id <- sapply(x, is.null)
  number_null_ids <- sum(null_id)
  if(number_null_ids == 0){
    stop("At least one component in the list should be NULL.")
  }

  # convert to integer (if numeric)
  for(ii in 1:length(x)){
    tmp <- x[[ii]][1]
    if(!is.null(tmp) && !is.integer(tmp)){
      x[[ii]] <- as.integer(x[[ii]])
    }
  }
  new("TreeHarp", adjList = x, nodeTypes = NA, 
      repr=paste0(names(x), collapse="-"), call=NA)
}


mat_2_tree <- function(x) {
   # check named, symmetric
   if(!isSymmetric(x)){
     stop("non-symmetric matrix.")
   }
   nn <- dimnames(x)
   if(!identical(nn[[1]], nn[[2]])){
     stop("node names do not match.")
   }
   name_lens <- nchar(nn[[1]])
   nonzero_lens <- sum(name_lens > 0)
   if(nonzero_lens != nrow(x))
     stop("Not all nodes named!")

   # convert to adj list.
   adj_list <- matrix_2_adj_list(x)
   new("TreeHarp", adjList = adj_list, nodeTypes = NA, 
       repr=paste0(nn[[1]], collapse="-"), call=NA)
  }

setValidity("TreeHarp", function(object) {
  # is it connected?
  adj_list <- object@adjList

  # if it is a single leaf node, return TRUE.
  if(length(adj_list) == 1 && is.null(adj_list[[1]])){
    return(TRUE)
  }

  if(!is_connected(adj_list)){
    return("Graph is not connected.")
  }
  # is it in BFS order?
  ll <- get_levels(adj_list)
  if(!identical(ll, sort(ll))){
    return("Not in BFS ordering.")
  }

  # does it contain cycles?
  adj_mat <- adj_list_2_matrix(adj_list)
  e1 <- new.env()
  e1$visited <- rep(FALSE, length(adj_list))
  if(is_cyclic_r(adj_mat, 1, -1, e1)){
    return("Graph contains cycles.")
  }

  return(TRUE)
})

