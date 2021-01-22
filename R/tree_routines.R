#' Get the children node ids
#'
#' This function retrieves the child node ids of a given node from an
#' adjacency list of a tree.
#'
#' @param adj_list The adjacency list of the tree.
#' @param at_node The node whose children should be extracted.
#'
#' @details
#' Remember that the list has to be for a tree, not a general graph. Please
#' see other help pages for more specifications.
#'
#' This is a low-level function, used within the S4 class TreeHarp. It is not
#' generally meant for use by the user.
#'
#' @return A vector of integers specifying the children of that particular node.
#' If the node is a leaf, it returns NULL.
get_child_ids2 <- function(adj_list, at_node){
  adj_list[[at_node]]
}

#' Get the parent node id
#'
#' This function retrieves the parent node id of a given node from an
#' adjacency list of a tree.
#'
#' @param adj_list The adjacency list of the tree.
#' @param at_node The node whose parent should be extracted.
#'
#' @details
#' Remember that the list has to be for a tree, not a general graph. Please
#' see other help pages for more specifications.
#'
#' This is a low-level function, used within the S4 class TreeHarp. It is not
#' generally meant for use by the user.
#'
#' @return A integer of length 1 should be returned for all nodes except the
#' root. For the latter, the function will return NULL.
#'
#' @details
#' If there are nodes that have more than one parent, then a warning is issued.
get_parent_id2 <- function(adj_list, at_node) {
  if(at_node == 1){
    return(NULL)
  }
  ids <- which(sapply(adj_list, function(x) at_node %in% x))
  if(length(ids) == 0){
    stop("Node does not exist")
  }
  if(length(ids) > 1){
    warning("More than 1 parent! Is it really a tree?")
  }
  unname(ids)
}

#' Extract a sub-tree.
#'
#' Extracts a sub-tree rooted at a particular node.
#'
#' @param obj An object of class TreeHarp
#' @param at_node The root of the new sub-tree. An integer, not a label, that
#' corresponds to BFS indexing of the tree.
#' @param preserve_call A logical value that indicates if a sub-call should be 
#' extracted. This might be slower, but it allows you to evaluate it later.
#'
#' @return An object of class TreeHarp.
#' @export
#' 
#' @details This is meant for internal use, so the nodeTypes slot is silently
#' dropped, unless preserve_call is set to TRUE
#'
#' @examples
#' th3 <- list(a= c(2L,3L,4L), b=NULL, c=c(5L, 6L), d=7L, e=NULL, f=NULL, g=NULL)
#' subtree_at(TreeHarp(th3), 3)
#' st <- subtree_at(TreeHarp(th3), 4)
#' plot(st)
subtree_at <- function(obj, at_node, preserve_call=FALSE) {
  if(class(obj) != "TreeHarp"){
    stop("object should be of class TreeHarp")
  }
  if(!preserve_call) {
    th_obj <- get_adj_list(obj)
  
    nodes_list <- at_node
    last_done <- 0
    while(last_done < length(nodes_list)){
      nodes_list <- append(nodes_list, 
                           get_child_ids(th_obj, nodes_list[last_done+1]))
      last_done <- last_done + 1
    }
    # re-order nodes
    rep_vals <- 1:length(nodes_list)
    names(rep_vals) <- nodes_list
  
    sub_list <- th_obj[nodes_list]
    for(ii in 1:length(sub_list)) {
      if(is.null(sub_list[[ii]][1])){
        next
      } else{
        sub_list[[ii]] <- dplyr::recode(sub_list[[ii]], !!!rep_vals)
      }
    }
    return(TreeHarp(sub_list))
  } else {
    if("logical" %in% class(obj@call)){
      stop("TreeHarp should be from a language object.")
    }
    th_call <- obj@call
    node_types <- get_node_types(obj)
    if(!node_types$call_status[at_node]) {
      stop("The new root node should be a call.")
    }
    rec_id <- get_recursive_index(obj, at_node)
    #browser()
    t1 <- TreeHarp(th_call[[rec_id + 1]], TRUE)
    return(t1)
  }
}

#' Convert adjacency list to a matrix
#'
#' Converts a list that represents a tree into a binary matrix.
#'
#' @param adj_list The adjacency list of the tree.
#'
#' @return A symmetric matrix of 1's and 0's, with 1 in entry (i,j) representing
#' an edge between the two vertices.
#'
#' @details
#' Remember that the list has to be for a tree, not a general graph. Please
#' see other help pages for more specifications.
#'
#' This is a low-level function, used within the S4 class TreeHarp. It is not
#' generally meant for use by the user.
#'
#' It works by filling up the upper diagonal of the matrix before reflecting it.
adj_list_2_matrix <- function(adj_list) {
  out_mat <- matrix(0L, nrow=length(adj_list), ncol=length(adj_list))
  node_names <- names(adj_list)
  dimnames(out_mat) <- list(node_names, node_names)

  for(ii in 1:nrow(out_mat)) {
    if(is.null(adj_list[[ii]][1])){
      next
    }
    out_mat[ii, adj_list[[ii]]] <- 1L
  }
  out_mat[lower.tri(out_mat)] <- t(out_mat)[lower.tri(t(out_mat))]
  if(!isSymmetric.matrix(out_mat))
    stop("Non-symmetrix matrix created!")
  out_mat
}

#' Convert adjacency matrix to a list.
#'
#' Converts a binary matrix that represents a tree into an adjacency list.
#'
#' @param mat  A symmetric matrix of 1's and 0's, with 1 in entry (i,j)
#'   representing an edge between the two vertices.
#'
#' @return The adjacency list of the tree.
#'
#' @details
#' Remember that the list that is finally output is for a tree, not a general
#' graph. Please see other help pages for more specifications.
#'
#' The input matrix should be BFS ordered. The adjacency list only notes the
#' child node(s) of a particular node. If a matrix denotes multiple parents,
#' it will not be picked up.
#'
#' This is a low-level function, used within the S4 class TreeHarp. It is not
#' generally meant for use by the user.
#'
matrix_2_adj_list <- function(mat) {
  node_names <- colnames(mat)
  out_list <- vector("list", length(node_names))
  names(out_list) <- node_names

  for(ii in seq_along(out_list)) {
    id <- which(mat[ii,] == 1)
    id <- id[id > ii]
    if(length(id) > 0){
      out_list[[ii]] <- unname(id)
    }
  }
  out_list
}

#' Checks if a graph is connected.
#'
#' A tree is a graph that is connected but does not have any cycles. This
#' function checks if a provided adjacency list is connected.
#'
#' @param adj_list The adjacency list of the tree.
#' @param root The root node to start checking from. This defaults to the
#' first node in the adjacency list.
#'
#' @details This function is used as one of the validity checks within the
#' definition of the TreeHarp class. It is a low-level function, not really
#' meant for the general user of the package. Hence it is not exported.
#'
#' The nodes are traversed in a BFS order. The function could actually be
#' combined with \link{is_cyclic_r}, but it is kept separate for modularity
#' reasons.
#'
#' An alternative was to convert the list to an adjacency matrix and check
#' for a row and column of zeros.
#'
#' @return The function returns a TRUE if the graph is connected and FALSE
#' otherwise.
is_connected <- function(adj_list, root=1) {
  nodes_list <- root
  last_done <- 0

  while(last_done < length(nodes_list)){
    nodes_list <- append(nodes_list,
                         get_child_ids(adj_list, nodes_list[last_done+1]))
    last_done <- last_done + 1
  }

  nodes_list <- sort(unique(nodes_list))
  if(length(nodes_list) == length(adj_list))
    return(TRUE)
  else return(FALSE)
}

#' Checks if a graph contains any cycles.
#'
#' A tree is a graph that is connected but does not have any cycles. This
#' function checks if a provided adjacency matrix contains cycles.
#'
#' @param adj_mat  A symmetric matrix of 1's and 0's, with 1 in entry (i,j)
#'   representing an edge between the two vertices.
#' @param node_v The node to begin searching for cycles from. An integer.
#' @param parent_node The parent node of node_v. Also an integer. Use -1 if
#' you are starting from node 1. This is in fact the default.
#' @param visited_env An environment containing a logical vector indicating which
#' nodes have already been visited. The vector has to be named "visited".
#' See the details.
#'
#' The function works by traversing all the nodes, in a BFS order. If it finds
#' a node has a parent that has already been visited, it concludes that there
#' is a cycle.
#'
#' The function is recursive, and has to update the vector of visisted nodes
#' within each call. Hence the visited vector is stored in an environment
#' that is passed along. It will return an error if no such environment is
#' provided. It is a very specific input that the function requires, and this
#' is another reason that this function is not exported.
#'
#' This function is used within the validity checks for the S4 class. It is
#' not exported for the user.
#'
#' @return A logical value indicating if the graph contains cycles.
is_cyclic_r <- function(adj_mat, node_v, parent_node=-1, visited_env) {
  if(!exists("visited", envir=visited_env)){
    stop("visited vector does not exist!")
  }
  visited_env$visited[node_v] <- TRUE
  nbr_ids <- which(adj_mat[node_v, ] == 1)
  for(jj in nbr_ids){
    if(!visited_env$visited[jj]){
      if(is_cyclic_r(adj_mat, jj, node_v, visited_env))
        return(TRUE)
    }else{
      if(jj != parent_node)
        return(TRUE)
    }
  }
  return(FALSE)
}

#' Generate the next sub-tree.
#'
#' This generates the next sub-tree in the enumeration list.
#'
#' @param obj An object of class TreeHarp.
#' @param char_arr A vector of 1's and 0's indicating which nodes to keep. The
#' vector should have length equal to the number of nodes in obj.
#'
#' @details Need to reference the paper. This generates the next sub-tree,
#' rooted at the root node of this tree. It will generate singletons on it's
#' own. It has to be used within a loop to do that.
#'
#' @return A vector of 1's and 0's, which denotes the next sub-tree in the list.
#' @export
#' @seealso \code{\link{generate_all_subtrees}}
#' 
#' @examples 
#' th1 <- TreeHarp(list(a=c(2,3), b=NULL, c=NULL))
#' get_next_subtree(th1, c(1,0,0))
#' get_next_subtree(th1, c(1,1,0))
#'
get_next_subtree <- function(obj, char_arr) {
  th_obj <- get_adj_list(obj)
  # error-checking
  if(length(char_arr) != length(th_obj)){
    stop("char_arr should be a 0-1 vector length node.")
  }
  if(all(char_arr == 1)) {
    #message("No sub-trees left!")
    return(NULL)
  }
  #
  for(ii in length(char_arr):1){
    if(char_arr[ii] == 1) {
      char_arr[ii] <- 0
    } else {
      # print("OK")
      # get parent node
      parent_id <- get_parent_id(th_obj, ii)
      # return(parent_id)
      if(char_arr[parent_id] == 1) {
        char_arr[ii] <- 1
        return(char_arr)
      } else {
        next
      }
    }
  }
  char_arr
}

#' Generate all subtrees from a tree.
#'
#' This routines generates all subtrees rooted at the root node for a particular
#' tree.
#'
#' @param th An object of class TreeHarp.
#'
#' @return A 0-1 matrix with n rows and m columns. n is the number of sub-trees
#' rooted at the root node of th. m is the number of nodes in this given tree.
#' The leading column will be a 1 for all the rows.
#'
#' @seealso \code{\link{get_next_subtree}}
#' @references 
#' \emph{Listing and counting subtrees of a tree}, F Ruskey, \emph{SIAM Journal on Computing}, 1981
#' 
#' @export
#' @examples
#' th1 <- TreeHarp(list(a=c(2,3), b=NULL, c=NULL))
#' generate_all_subtrees(th1)
generate_all_subtrees <- function(th){
  num_nodes <- length(th)

  init_tree <- rep(0, num_nodes)
  init_tree[1] <- 1
  all_trees <- init_tree

  while(!is.null(init_tree)){
    init_tree <- get_next_subtree(th, init_tree)
    all_trees <- rbind(all_trees, init_tree, deparse.level = 0L)
  }

  all_trees
}

#' Carve out branches to form a new tree.
#'
#' This functions keeps only the indicated nodes, returning a new sub-tree.
#'
#' @param obj An object of class TreeHarp.
#' @param char_arr A vector of 1's and 0's indicating which nodes to keep. The
#' vector should have length equal to the number of nodes in obj.
#'
#' @return An object of class TreeHarp.
#' @details This returns an error if the sub-tree does not define a new tree.
#' @export
#'
#' @examples
#' th3 <- list(a= c(2L,3L,4L), b=NULL, c=c(5L, 6L), d=7L, e=NULL, f=NULL, g=NULL)
#' carve_subtree(TreeHarp(th3), c(1,0,0,0,0,0,0))
#' st <- subtree_at(TreeHarp(th3), 4)
#' plot(st)
carve_subtree <- function(obj, char_arr) {
  th_obj <- get_adj_list(obj)
  th_mat <- adj_list_2_matrix(th_obj)
  out_mat <- th_mat[which(char_arr == 1), which(char_arr == 1), drop=FALSE]
  #TreeHarp(out_mat)
  nT <- get_node_types(obj)
  if(class(nT) %in% "data.frame"){
    nT <- nT[which(char_arr==1), ]
    nT$id <- 1:nrow(nT)
    out_obj <-  tryCatch(new("TreeHarp", adjList = matrix_2_adj_list(out_mat),
                             nodeTypes = nT),
                         error = function(e) return(e))
  } else {
    out_obj <-  tryCatch(TreeHarp(out_mat),
                         error = function(e) return(e))
  }

  if("error" %in% class(out_obj)){
    stop("Selected nodes do not define a tree.")
  }
  return(out_obj)
  # out_mat
  # mat_2_treeharp(out_mat)
}

#' Obtains the node levels from a tree.
#'
#' This function obtains the node levels from a tree.
#'
#' @param adj_list The adjacency list of the tree.
#'
#' @details This function is used to check if the specification of the tree is
#' in BFS order. If that is indeed the case, the levels of each node should be
#' sorted.
#'
#' This function is not exported for the general user.
#'
#' @return It returns a vector of integers. The length of this vector will be
#' the number of nodes in the tree. The root is at level 1, the next is at level
#'  2, and so on.
#'
get_levels <- function(adj_list) {
  num_nodes <- length(adj_list)
  # if it is a single leaf node, return TRUE.
  if(num_nodes == 1 && is.null(adj_list[[1]])){
    return(1L)
  }

  level_vec <- rep(NA, num_nodes)
  level_vec[1] <- 1
  for(jj in 2:num_nodes){
    steps <- 1
    anc <- get_parent_id(adj_list, jj)
    while(is.na(level_vec[anc])){
      anc <- get_parent_id(adj_list, anc)
      steps <- 1 + steps
    }
    level_vec[jj] <- steps + level_vec[anc]
  }
  level_vec
}

#' Checks if a tree is rooted at a node of another tree.
#'
#' This function checks if a given tree is a sub-tree of another tree at a
#' particular node.
#'
#' @param x An object of class TreeHarp.
#' @param y An object of class TreeHarp.
#' @param at_node An integer, corresponding to a node in object y. The sub-tree
#' of y, rooted at at_node, is compared to x.
#'
#' @details
#' Here's how it works: The sub-tree of y, rooted at at_node is first
#' extracted. The tree x is then compared to this. If x is a sub-tree of it,
#' then this function returns FALSE. Otherwise it returns TRUE.
#'
#' @return A logical value indicating if x is a sub-tree of y, rooted at
#' at_node.
#' @export
#'
#' @examples
#' thb1 <- TreeHarp(list(b=2, d=NULL))
#' tha1 <- TreeHarp(list(a=c(2,3), b=4, c = NULL, d=NULL))
#' is_subtree_rooted_at(thb1, tha1, 1) # FALSE
#' is_subtree_rooted_at(thb1, tha1, 2) # TRUE
is_subtree_rooted_at <- function(x, y, at_node) {
  if(class(x) != "TreeHarp" || class(y) != "TreeHarp"){
    stop("objects should both be of class TreeHarp")
  }
  y_sub <- subtree_at(y, at_node)

  if(length(x) == 1){
    if(names(x)[1] == names(y_sub)[1]){
      return(TRUE)
    } else{
      return(FALSE)
    }
  }

  adj_list_x <- get_adj_list(x)
  levels_x <- get_levels(adj_list_x)
  names_x <- names(x)
  df_x <- data.frame(names=names_x, levels=levels_x,
                     stringsAsFactors = FALSE)

  adj_list_y <- get_adj_list(y_sub)
  levels_y <- get_levels(adj_list_y)
  names_y <- names(y_sub)
  df_y <- data.frame(names=names_y, levels=levels_y,
                     stringsAsFactors = FALSE)

  aj <- dplyr::anti_join(df_x, df_y, by=c("names", "levels"))
  if(nrow(aj) > 0) {
    return(FALSE)
  }
  df_y$id <- 1:nrow(df_y)
  sj <- dplyr::semi_join(df_y, df_x, by=c("names", "levels"))
  char_arr <- rep(0, nrow(df_y))
  char_arr[sj$id] <- 1L

  y_test <- carve_subtree(y_sub, char_arr)
  return(identical(x@adjList, y_test@adjList))
}


#' Prune a tree up to a specified depth.
#' 
#' Prunes a tree up to a depth specified by a set of node names.
#'
#' @param th A TreeHarp object.
#' @param names_to_keep  The node names to keep in the pruned tree.
#' 
#' @details This is a seldom used function. It works in this way. Given a set 
#' of node names, it identifies the node with the greatest depth in that set.
#' The function then returns the sub-tree, that contains all the nodes with a
#' depth smaller than or equal to that depth. If the node types slot is not NA,
#' then that data frame is filtered and returned too.
#' 
#' Take a look at the examples for a clearer picture.
#'
#' @return An object of class TreeHarp.
#' @export
#' @seealso \code{\link{carve_subtree}}, \code{\link{path_to_root}}, 
#' \code{\link{carve_mst}}
#'
#' @examples
#' ex1 <- quote(x <- f(y, g(5)))
#' th1 <- TreeHarp(ex1, TRUE)
#' s1 <- prune_depth(th1, c("f", "y"))
#' s2 <- prune_depth(th1, c("f", "z")) # node not present!
#' plot(s1)
#' plot(s2)
prune_depth <- function(th, names_to_keep) {
  lvls <- get_levels(get_adj_list(th))
  ids <- which(names(th) %in% names_to_keep)
  if(length(ids) == 0){
    return(th)
  }
  last_id <- max(ids)
  keep_depth <- lvls[last_id]
  char_arr <- rep(0, length(lvls))
  char_arr[lvls <= keep_depth] <- 1
  carve_subtree(th, char_arr)
}

#' Extract a path from node to root.
#' 
#' Identifies the nodes on the path from a node up to the root of a TreeHarp
#' object.
#'
#' @param th A TreeHarp object.
#' @param node_num A node number to start tracking upwards from.
#' 
#' @details This function allows the user to identify the branch from a node 
#' up to the root of a tree. 
#'
#' @return A vector of 1's and 0's that can be used to carve out the branch 
#' alone, using \code{\link{carve_subtree}}.
#' @export
#'
#' @examples
#' ex1 <- quote(x <- f(y, g(5)))
#' th1 <- TreeHarp(ex1, TRUE)
#' path_to_root(th1, 5)
path_to_root <- function(th, node_num) {
  adj_list <- get_adj_list(th)
  char_array <- rep(0, length(th))
  char_array[node_num] <- 1

  while(char_array[1] != 1) {
    node_num <- get_parent_id(adj_list, node_num)
    char_array[node_num] <- 1
  }
  char_array
}

#' Carve a Minimal Spanning Tree Out
#' 
#' Given node names, this function retrieves the smallest tree containing at most 
#' those nodes.
#'
#' @param th An object of class TreeHarp.
#' @param node_names A character vector of node names. Nodes outside this set 
#' will not be returned in the tree. It must include the root node name.
#'
#' @return An object of class TreeHarp.
#' @export
#' 
#' @details The function starts from each node specified and works it's way up
#' to the root. If a branch contains nodes outside the list, it is shortened.
#' 
#' In the end, the tree that is returned will try to contain all the named nodes,
#' but if that's not possible some will dropped to ensure a tree is returned, 
#' not a disconnected graph.
#' 
#' @examples
#' ex1 <- quote(x <- f(y, g(5)))
#' th1 <- TreeHarp(ex1, TRUE)
#' carve_mst(th1, c("<-", "x", "f", "5")) ## note: 5 is dropped.
#' carve_mst(th1, c("<-", "x", "f", "y")) 
#' carve_mst(th1, c("<-", "f", "g")) 
#' 
carve_mst <- function(th, node_names) {
  all_names <- names(th)
  if(!all_names[1] %in% node_names){
    stop("root name must be included.")
  }
  keep_these <- which(all_names %in% node_names)
  char_array <- rep(FALSE, length(all_names))

  for(ii in keep_these) {
    char_tmp <- path_to_root(th, ii)
    if(any(!(all_names[which(char_tmp == 1)] %in% node_names))){
      next
    }
    char_array <- (char_array | char_tmp)
  }
  char_array <- ifelse(char_array, 1, 0)
  carve_subtree(th, char_array)
}

#' Keep only branches specified by node numbers
#' 
#' Retains only specific branches, that are identified by their node numbers.
#'
#' @param th  A TreeHarp object.
#' @param branch_nodes An integer vector, specifying the nodes to keep.
#' @param include_lower A logical value - whether or not the lower branches should 
#' also be kept.
#'
#' @return A TreeHarp object.
#' @export
#'
#' @examples
#' ex1 <- quote(x <- f(y, g(5)))
#' th1 <- TreeHarp(ex1, TRUE)
#' keep_branches(th1, 3)
#' keep_branches(th1, 3, include_lower = FALSE)
#' keep_branches(th1, c(2,3), FALSE)
#' keep_branches(th1, c(3, 4), FALSE)

keep_branches <- function(th, branch_nodes, include_lower = TRUE) {
  if(length(branch_nodes) == 1){
    upper_ids <- which(path_to_root(th, branch_nodes) == 1)
  } else {
    paths_to_root <- lapply(branch_nodes, path_to_root, th=th)
    upper_ids <- which(Reduce("|", paths_to_root))
  }

  if(include_lower) {
    nodes_list <- branch_nodes
    last_done <- 0
    adj_list <- get_adj_list(th)

    while(last_done < length(nodes_list)){
      nodes_list <- append(nodes_list,
                           get_child_ids(adj_list, nodes_list[last_done+1]))
      last_done <- last_done + 1
    }

    upper_ids <- unique(c(upper_ids, nodes_list))
  }

  char_arr <- rep(0, length(th))
  char_arr[upper_ids] <- 1L
  carve_subtree(th, char_arr)
}

#' Find the branch that leads from one node to another.
#' 
#' Given two nodes that are on the same path to the root, this function
#' determines the branch that leads to the child node.
#' 
#' @param th A TreeHarp object.
#' @param child_id An integer node id. It corresponds to the node to trace up
#'   from.
#' @param ancestor_id An integer node id. It corresponds to the node to trace 
#' down from.
#' 
#' @details This is used when trying to find a sub-call from a TreeHarp object.
#' It is useful in determining the indices to use when extracting the sub-call.
#'
#' @return An integer that denotes the branch to follow down (from the ancestor)
#' to reach the child.
#' @export
#'
#' @examples
#' ex3 <- quote(x <- f(y = g(3, 4), z=1L))
#' t1 <- TreeHarp(ex3, TRUE)
#' find_branch_num(t1, 8, 3) # should be 1
#' find_branch_num(t1, 5, 3) # should be 2
find_branch_num <- function(th, child_id, ancestor_id) {
  node_to_root <- path_to_root(th, child_id)
  nodes_on_path <- which(node_to_root == 1L)
  
  if(!ancestor_id %in% nodes_on_path) {
    stop("child is not descendant of ancestor.")
  }
  p_id <- get_parent_id(th, child_id)
  ancestors_children <- get_child_ids(th, ancestor_id)
  if(p_id == ancestor_id){
    # direct descendant
    return(which(ancestors_children == child_id))
  }
  
  while(!p_id %in% ancestors_children) {
    p_id <- get_parent_id(th, p_id)
  }
  which(ancestors_children == p_id)
}

#' Obtain an index to extract out a sub-call
#' 
#' Obtains an index that can be used to extract a sub-call from a language object.
#' 
#' @param th A TreeHarp object.
#' @param node_id An integer corresponding to a call within the parse tree 
#' (not a literal, symbol or a formal argument).
#'
#' @return A vector of indices, that can be used (together with "[[") to obtain a 
#' sub-call
#' @export
#'
#' @examples
#' ex3 <- quote(x <- f(y = g(3, 4), z=1L))
#' t1 <- TreeHarp(ex3, TRUE)
#' rec_index <- get_recursive_index(t1, 6)
#' ex3[[rec_index + 1]]
#' ex3[[get_recursive_index(t1, 3)+1]]
get_recursive_index <- function(th, node_id) {
  node_types <- get_node_types(th)
  if((class(node_types)[1] == "logical") || (!node_types$call_status[node_id])) {
    return(NA)
  }
  call_status <- node_types$call_status
  nodes_to_root <- which(path_to_root(th, node_id) == 1L)
  
  fns_to_root <- nodes_to_root[call_status[nodes_to_root]]
  parent_fns_to_root <- sapply(fns_to_root[-1], get_parent_call_id, x=th)
  branch_nums <- mapply(find_branch_num, child_id = fns_to_root[-1], 
                        ancestor_id = parent_fns_to_root, 
                        MoreArgs = list(th=th))
  branch_nums
  #parent_nodes <- sapply(nodes_to_root[-1], get_parent_call_id, x=th)
  #child_nodes <- lapply(parent_nodes, get_child_ids, x=th)
  #child_nodes
}

#' Get Node Id of Parent Call
#' 
#' Get the node id of the parent call for a given node.
#'
#' @param x  A TreeHarp object.
#' @param node_id  The id of the node whose parent call is to be found. An 
#' integer value.
#'
#' @details When we need to go up the parse tree to obtain the function that 
#' called this node, we use this function. It is similar to \code{get_parent_id},
#' except that that function only returns the immediate parent.
#' 
#' It is not useful to call this function when the TreeHarp object is not 
#' constructed from a language object.
#' 
#' Perhaps this function is necessary only because of the way language objects 
#' are represented by the autoharp: formal arguments are included in the 
#' tree representation. When we wish to find the calling function, we have to 
#' walk up the branches till we reach a function call.
#' 
#' @return An integer corresponding to the node id of the calling function.
#' @export
#'
#' @seealso \code{\link{get_parent_id}}
#' @examples
#' ex3 <- quote(x <- f(y = g(3, 4), z=1L))
#' t1 <- TreeHarp(ex3, TRUE)
#' 
#' # get the function that calls g:
#' get_parent_call_id(t1, 6) 
#' #contrast with this:
#' get_parent_id(t1, 6)
get_parent_call_id <- function(x, node_id) {
  node_types <- get_node_types(x)
  if(class(node_types)[1] == "logical") {
    return(NA)
  }
  call_status <- node_types$call_status
  p_id <- get_parent_id(x, node_id)
  while(!call_status[p_id]){
    p_id <- get_parent_id(x, p_id)
  }
  p_id
}
