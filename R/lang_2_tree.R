#library(codetools)
# node_id is the CALLING node.

#' Convert language object to tree.
#'
#' A recursive function for converting a language object to treeharp.
#'
#' @param lang_obj A language object.
#' @param node_id  The calling node to this language object. This should only 
#' be greater than 0 if the \code{ni_env} already contains a partial adjacency 
#' list and corresponding node information. This will happen when this function 
#' is called recursively.
#' @param ni_env  An environment to store the adjacency list and node information.
#'
#' @importFrom rlang is_symbol is_syntactic_literal call_args call_args_names
#' @importFrom rlang call_name is_pairlist is_na is_formula
#' @return Nothing
#' 
#' @details This function is used by TreeHarp constructors. It should not have
#' to be called by a user. It works by bulding up an adjacency list and node 
#' node information data frame within the supplied environment.
#' 
#' @export
#' @examples 
#' e1 <- new.env() 
#' lang_2_tree(quote(X <- 1), 0, e1) 
#' e1$adj_list 
#' e1$node_info

lang_2_tree <- function(lang_obj, node_id, ni_env) {
  #if(is.null(lang_obj)) {
  #  print("NULL!")
  #  return(NULL)
  #}

  if(is_symbol(lang_obj) || is_syntactic_literal(lang_obj)){
    #cat("It's a symbol or literal!\n")
    id_depth <- get_next_depth_id(parent_node_id = node_id, ni_env)
    if(is.null(lang_obj)){
      sym_name <- "NULL"
    } else if (rlang::is_na(lang_obj)){
      sym_name <- "NA"
    } else {
      sym_name <- as.character(lang_obj)
    }
    rbind_to_nodes_info(id_depth$id, sym_name, FALSE,
                        FALSE, id_depth$depth, ni_env)
    update_adj_list("new_node", node_id = id_depth$id,  node_name = sym_name,
                    env_ni = ni_env)
    if(id_depth$id > 1) { # need to check this branch!!
      update_adj_list("add_child", node_id = node_id, child_node = id_depth$id,
                      env_ni = ni_env)
    }
  } else if(is_pairlist(lang_obj)) {
    #cat("It's a pairlist.\n")
    lang_list <- as.list(lang_obj)
    formal_args <- names(lang_list)
    if(length(formal_args) > 0) {
      for(ii in seq_along(formal_args)) {
        # ii <- 1
        id_depth <- get_next_depth_id(parent_node_id = node_id, ni_env)
        rbind_to_nodes_info(id_depth$id, formal_args[ii], FALSE, TRUE,
                            id_depth$depth, ni_env)
        update_adj_list("new_node", node_id = id_depth$id,
                        node_name = formal_args[ii], env_ni = ni_env)
        if(id_depth$id > 1) { # need to check this branch!!
          update_adj_list("add_child", node_id = node_id, child_node = id_depth$id,
                          env_ni = ni_env)
        }
        #browser()
        #if(as.character(lang_list[[ii]]) != "") {
        if(is.null(lang_list[[ii]])) {
          lang_2_tree(quote(NULL), id_depth$id, ni_env)
        }else if(as.character(lang_list[[ii]])[1] != "") {
          lang_2_tree(lang_obj[[ii]], id_depth$id, ni_env)
        }
      }
    }
    }   else {
    #cat("It's a call!\n")
      # rlang functiosn call_ do not work well when the object is a one-sided
      # formula. When the call name is NULL, we also need to process that
      # first.
      if(is_formula(lang_obj) && length(lang_obj) == 2) {
        arg_num <- 1
        arg_names <- ""
        call_name1 <- "~"
        call_args1 <- lang_obj[2]
      } else {
        arg_num <- length(call_args(lang_obj))
        arg_names <-  call_args_names(lang_obj)
        call_name1 <- call_name(lang_obj)
        call_args1 <- call_args(lang_obj)
      }
    #
    # for call itself:
    id_depth <- get_next_depth_id(parent_node_id = node_id, ni_env)
    ### This is a fall-back fix if call_name returns NULL:
    # if(is.null(call_name1)){
    #   call_name1 <- as_label(lang_obj[[1]])
    # }
    if(is.null(call_name1)) {
      # browser()
      lang_2_tree(lang_obj[[1]], node_id, ni_env)
      fn_node_id <- max(ni_env$node_info$id)
    } else {
      if(call_name1 == "function") {
        arg_num <- arg_num - 1
      }
      rbind_to_nodes_info(id_depth$id, call_name1, TRUE, FALSE, id_depth$depth,
                          ni_env)
      update_adj_list("new_node", node_id = id_depth$id,  node_name = call_name1,
                      env_ni = ni_env)
      if(id_depth$id > 1) { # need to check this branch!!
        update_adj_list("add_child", node_id = node_id, child_node = id_depth$id,
                        env_ni = ni_env)
      }
      fn_node_id <- id_depth$id
    }
    if(arg_num > 0) {
      for(ii in 1:arg_num) {
        if(nchar(arg_names[ii]) > 0){
          id_depth <- get_next_depth_id(parent_node_id = fn_node_id, ni_env)
          rbind_to_nodes_info(id_depth$id, arg_names[ii], FALSE, TRUE,
                              id_depth$depth, ni_env)
          update_adj_list("new_node", node_id = id_depth$id,
                          node_name = arg_names[ii],  env_ni = ni_env)
          update_adj_list("add_child", node_id = fn_node_id,
                          child_node = id_depth$id, env_ni = ni_env)
          # lang_2_tree(call_args(lang_obj)[[ii]], fn_node_id+1, ni_env)
          lang_2_tree(call_args1[[ii]], id_depth$id, ni_env)
          # lang_2_tree
        } else {
          # lang_2_tree
          # cat("recurse\n")
          lang_2_tree(call_args1[[ii]], fn_node_id, ni_env)
        }
      }
    }
  }
}
