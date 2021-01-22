
#' Update node information.
#' 
#' Updates the node information regarding an R expression.
#'
#' @param id The id of the node to be added. This should be an integer
#' of length 1.
#' @param name The name of the node.
#' @param call_status Is the language object a call or a symbol/literal? This 
#' should a logical value.
#' @param formal_arg Is the language object a formal argument or not? This 
#' should be a logical value.
#' @param depth An integer indicating the depth of this language object in 
#' the parse tree.
#' @param env_ni An environment object, possibly containing a data frame with
#' columns id, name, call_status, formal_arg and depth.
#'
#' @return TRUE is returned invisibly. 
#' 
#' @details This is for internal use. It may be removed from user-view soon!
rbind_to_nodes_info <- function(id, name, call_status, formal_arg, depth, 
                                env_ni) {
  tmp_df <- data.frame(id = id, name = name, call_status = call_status, 
                       formal_arg = formal_arg, depth = depth, 
                       stringsAsFactors = FALSE)
  if(!exists("node_info", env_ni)){
    assign("node_info", tmp_df, env_ni)
  } else {
    old_parent <- parent.env(env_ni)
    parent.env(env_ni) <- sys.frame(sys.nframe()) # set parent to be evaluation env.
    evalq(node_info <- rbind(node_info, tmp_df), envir = env_ni)
    # reset parent
    parent.env(env_ni) <- old_parent
  }
  return(invisible(TRUE))
}

#' Get the id and depth of a child node.
#'
#' From the parent's depth and the last labelled node, we obtain the node id 
#' and depth of a child.
#' 
#' @param parent_node_id The id of the parent node we are considering.
#' @param env_ni An environment object, possibly containing a data frame with
#' columns id, name, call_status, arg_type and depth.
#'
#' @return A list containing the id and depth of the next node. 
#' @details This is for internal use. It may be removed from user-view soon!
get_next_depth_id <- function(parent_node_id, env_ni) {
  if(!exists("node_info", env_ni)){
    if(parent_node_id != 0){
      stop("If no prior node information exists, the caller node should be 0.")
    }
    next_id <- 1L
    next_depth <- 1L
  } else {
    if(!parent_node_id %in% env_ni$node_info$id) {
      stop("caller node id does not exist in node info table.")
    }
    next_id <- max(env_ni$node_info$id) + 1L
    next_depth <- 
      env_ni$node_info$depth[env_ni$node_info$id == parent_node_id] + 1L
  }
  return(list(id=next_id, depth=next_depth))
}

#' Update adjacency list.
#' 
#' Updates the adjacency list for an R expression parse tree.
#'
#' @param update_type This should be either "new_node" or "add_child". If it 
#' is a new node, an empty list component is added. If it is add child, 
#' then child_node should be provided too.
#' @param node_id An integer.
#' @param node_name The name of the new node to be added. This must be provided 
#' if the update_type is "new_node".
#' @param child_node An integer.
#' @param env_ni An environment object, possibly containing an adjacency list 
#' that will later be used to construct a TreeHarp object.
#'
#' @return An invisible TRUE is returned.
#' 
#' 
#' @details This is for internal use. It may be removed from user-view soon!
update_adj_list <- function(update_type=c("new_node", "add_child"), 
                            node_id, node_name, child_node, env_ni) {
  if(!exists("adj_list", env_ni)){
    if(update_type == "add_child"){
      stop("No existing nodes; can't add child.")
    }
    adj_list <- list(NULL)
    names(adj_list) <- node_name
    assign("adj_list", adj_list, env_ni)
  } else {
    old_parent <- parent.env(env_ni)
    parent.env(env_ni) <- sys.frame(sys.nframe()) # set parent to be evaluation env.
    if(update_type == "new_node"){
      evalq(adj_list[node_id] <- list(NULL), envir = env_ni)   
      evalq(names(adj_list)[node_id] <- node_name, envir = env_ni)   
    } else {
      evalq(adj_list[[node_id]] <- append(adj_list[[node_id]], 
                                          child_node), envir=env_ni)
    }
    parent.env(env_ni) <- old_parent
  }
  return(invisible(TRUE))
}