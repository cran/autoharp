#' @include treeharp.R
NULL

#' @describeIn TreeHarp To get the length of a tree.
#'
#' The length of the tree refers to the number of nodes in the tree.
#'
#' @param x A Treeharp object.
#'
#' @return \code{length}: An integer of length 1.
#' @export
setMethod("length", signature=c(x="TreeHarp"),
          function(x) {
            return(length(x@adjList))
            })

#' @describeIn TreeHarp To print a tree representation.
#'
#' A string representation of a TreeHarp object.
#' 
#' @param object A TreeHarp object.
#'
#' @return \code{print}: Returns NULL. It prints a string representation of a 
#' TreeHarp object.
#' @export
setMethod("show",
    signature(object = "TreeHarp"),
    function (object) 
    {
            cat(object@repr, "\n")
            return(invisible(NULL))
    }
)

#' @describeIn TreeHarp To get tree labels
#'
#' This function returns the node labels of the tree.
#'
#' @return \code{names}: A character vector with length equal to the number of 
#' nodes.
#' @export
setMethod("names", signature=c(x="TreeHarp"),
          function(x) {
            return(names(x@adjList))
            })

#' Generic for Getting Parent Node Id.
#'
#' The generic method definition for getting parent node id.
#'
#' @param x An object of class TreeHarp or an adjacency list.
#' @param node_num An integer, length 1. This the node whose parent we are
#' after. If node_num is equal to 1, then NULL is returned because that should
#' be the root node.
#'
#' @return An integer, indicating the parent node.
#' @export
setGeneric("get_parent_id", function(x, node_num) standardGeneric("get_parent_id"),
           signature = "x"
           )

#' @describeIn get_parent_id Obtain parent node id.
#'
#' Extracts parent id of a node from a TreeHarp object.
#'
#' @export
#' @seealso \code{\link{get_child_ids}}
setMethod("get_parent_id", signature=c(x="TreeHarp"),
          function(x, node_num) {
            get_parent_id2(x@adjList, node_num)
            })

#' @describeIn get_parent_id Obtain parent node id.
#'
#' Extracts parent id of a node from an adjacency list object.
#'
#' @export
setMethod("get_parent_id", signature=c(x="list"),
          function(x, node_num) {
            get_parent_id2(x, node_num)
            })

#' Generic for Getting Child Node Ids
#'
#' The generic method definition for getting child node ids.
#'
#' @param x An object of class TreeHarp.
#' @param node_num An integer, length 1. This the node whose children we are
#' after. If the specified node is a leaf, the NULL is returned.
#'
#' @return An integer vector, indicating the children node ids.
#' @seealso \code{\link{get_parent_id}}
#' @export
setGeneric("get_child_ids", function(x, node_num) standardGeneric("get_child_ids"),
           signature = "x"
           )

#' @describeIn get_child_ids Obtain child nodes.
#'
#' Allows user to extract the child nodes from a specified node from TreeHarp
#' object.
#'
#' @export
setMethod("get_child_ids", signature=c(x="TreeHarp"),
          function(x, node_num) {
            get_child_ids2(x@adjList, node_num)
            })

#' @describeIn get_child_ids Obtain child nodes.
#'
#' Allows user to extract the child nodes from a specified node from an 
#' adjacency list.
#'
#' @export
setMethod("get_child_ids", signature=c(x="list"),
          function(x, node_num) {
            get_child_ids2(x, node_num)
            })

# #' To check if two trees are equal.
# #'
# #' This function checks if two trees are equal.
# #'
# #' @param x A Treeharp object.
# #' @param y A Treeharp object.
# #' @param num.eq See original
# #' @param single.NA See original
# #' @param attrib.as.set See original
# #' @param ignore.bytecode See original
# #' @param ignore.environment See original
# #' @param ignore.srcref See original
# #'
# #' @return A logical indicating if two TreeHarp objects are equal.
# #' @export
# setMethod("identical",
#     signature(x = "TreeHarp", y = "TreeHarp"),
#     function (x, y, num.eq = TRUE, single.NA = TRUE, attrib.as.set = TRUE,
#         ignore.bytecode = TRUE, ignore.environment = FALSE, ignore.srcref = TRUE)
#     {
#             x_list <- adjList(x)
#             y_list <- adjList(y)
#             return(identical(x_list, y_list))
#     }
# )

#' Generic for Getting Adjacency List
#'
#' The generic method definition for getting adjacency list from a
#' TreeHarp object.
#'
#' @param x An object of class TreeHarp.
#' @param ... Unused arguments, for now.
#'
#' @return The adjacency list for a TreeHarp object.
#' @export
setGeneric("get_adj_list", function(x, ...) standardGeneric("get_adj_list"),
           signature = "x"
           )

#' @describeIn get_adj_list A getter.
#'
#' Allows user to extract the adjacency list of a treeharp object.
#'
#' @export
setMethod("get_adj_list", signature=c(x="TreeHarp"),
          function(x, ...) {
            return(x@adjList)
            })

#' Generic for Getting Node Types
#'
#' The generic method definition for getting node types from a
#' TreeHarp object.
#'
#' @param x An object of class TreeHarp.
#' @param ... Unused arguments, for now.
#'
#' @return A data frame containing the node types for a TreeHarp object. If the 
#' slot is empty, NA is returned.
#' @export
setGeneric("get_node_types", function(x, ...) standardGeneric("get_node_types"),
           signature = "x"
           )

#' @describeIn get_node_types A getter.
#'
#' Allows user to extract the node types of a treeharp object.
#'
#' @export
setMethod("get_node_types", signature=c(x="TreeHarp"),
          function(x, ...) {
            return(x@nodeTypes)
            })

#' TreeHarp Plotting TreeHarp Objects
#'
#' A plot method for visualising treeharp objects.
#'
#' @param x An object of class TreeHarp.
#' @param y Unused.
#' @param ... Additional arguments passed on to plot.igraph().
#'
#' @details The treeharp object is converted to an igraph object before it
#' is plotted.
#'
#' @return Returns NULL, invisibly.
#' @export
setMethod("plot", signature=c("TreeHarp"),
          function(x, y, ...){
            # convert to igraph
            ig <- igraph::graph_from_adj_list(get_adj_list(x), mode="all",
                                              duplicate=FALSE)
            ll <- names(x@adjList)
            igraph::plot.igraph(ig,
                                layout=igraph::layout_as_tree(ig, root=1),
                                vertex.label=ll, ...)
          })
