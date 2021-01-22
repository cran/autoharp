#' Root a list of trees.
#' 
#' Given a list of trees, this will root them.
#'
#' @param ... A list of Treeharp objects.
#' 
#' @details This function combines TreeHarp objects into a single TreeHarp.
#' The function will root all of them at a node called "script", which is 
#' neither a function call nor an argument nor a symbol. The BFS ordering is 
#' then updated.
#' 
#' Objects that are not of class TreeHarp will be dropped from the list 
#' before the rooting takes place. 
#'
#' @return A TreeHarp object
#' @export
#' 
join_treeharps <- function(...) {
  # remove NA trees
  all_trees <- c(...)
  all_trees <- Filter(function(x) class(x) == "TreeHarp", all_trees)
  all_trees_info <- NULL
  
  for(ii in 1:length(all_trees)) {
    tmp <- get_node_types(all_trees[[ii]])
    if(class(tmp) == "logical" && is.na(tmp))
      stop("Need node type information..")
    tmp$tree_id <- ii
    all_trees_info <- rbind(all_trees_info, tmp)
  }
  all_trees_info$new_depth <- all_trees_info$depth + 1
  all_trees_info <- rbind(all_trees_info,
                          data.frame(id=0L, name = "script", call_status = FALSE,
                                     formal_arg=FALSE, depth=0, tree_id=0,
                                     new_depth=1))
  all_trees_info <- dplyr::arrange(all_trees_info, .data$new_depth, .data$tree_id) %>%
    dplyr::mutate(new_id = dplyr::row_number())

  new_adj_list <- vector("list", length=nrow(all_trees_info))
  new_adj_list[[1]] <- all_trees_info$new_id[all_trees_info$new_depth == 2]
  names(new_adj_list)[1] <- all_trees_info$name[1]
  for(ii in 2:nrow(all_trees_info)){
    org_tree_id <- all_trees_info$tree_id[ii]
    org_node_id <- all_trees_info$id[ii]
    new_adj_list[ii] <- get_adj_list(all_trees[[org_tree_id]])[org_node_id]
    names(new_adj_list)[ii] <- all_trees_info$name[ii]

    tree_df <- dplyr::filter(all_trees_info, .data$tree_id == org_tree_id)
    old_ids <- tree_df$id
    new_ids <- tree_df$new_id
    names(new_ids) <- old_ids

    if(!is.null(new_adj_list[[ii]])){
      new_adj_list[[ii]] <- dplyr::recode(new_adj_list[[ii]], !!!new_ids)
    }
  }
  new("TreeHarp", adjList = new_adj_list, nodeTypes= all_trees_info)
  #all_trees_info
}
