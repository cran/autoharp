#' Function to rearrage nodes in BFS
#'
#' @param adj_list The output of lang_2_tree.
#' @param node_info The output of lang_2_tree.
#'
#' @details This function is for an internal TreeHarp constructor use. It is
#' not exported.
#'
#' @return An adjacency list and nodes info data frame in BFS order.
#'
#' @importFrom rlang .data
to_BFS <- function(adj_list, node_info) {
  n_nodes <- nrow(node_info)
  node_info <- dplyr::arrange(node_info, .data$depth)
  old_ids <- node_info$id
  node_info$id <- 1:n_nodes

  new_adj_list <- vector(mode="list", length=n_nodes)
  for(ii in 1:n_nodes) {
    new_adj_list[ii] <- adj_list[old_ids[ii]]
    names(new_adj_list)[ii] <- names(adj_list)[old_ids[ii]]
  }

  rep_vals <- 1:n_nodes
  names(rep_vals) <- old_ids

  for(ii in 1:n_nodes) {
    if(is.null(new_adj_list[[ii]][1])){
      next
    } else{
      new_adj_list[[ii]] <- dplyr::recode(new_adj_list[[ii]], !!!rep_vals)
    }
  }

  list(new_adj_list, node_info)
}
