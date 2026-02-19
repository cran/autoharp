#' Forestharp helpers
#'
#' Example of functions that can be \emph{directly used on TreeHarp objects
#' individually,} and on forestharp objects via \code{\link{fapply}}.
#'
#' @param th A TreeHarp object.
#'
#' @details These are examples of functions that be called on a list of TreeHarp
#'   objects, which we refer to as a forestharp object. Such objects are not
#'   formally defined yet, but can be created using
#'   \code{\link{rmd_to_forestharp}} or using \code{\link{join_treeharps}}.
#'
#' @return On their own, each of these functions should return a scalar or a
#' 1-dimensional array. When called with \code{\link{fapply}}, the scalar
#' numerical values can be combined (by taking the sum, any other provided
#' combiner function).
#'
#' The ultimate idea is that fapply should return a single feature for each rmd
#' file that it is called upon.
#'
#' @examples
#' # Dummy trees
#' th1 <- TreeHarp(quote(X <- rnorm(10, mean=0.9, sd=4)), TRUE)
#' th2 <- TreeHarp(quote(Y <- rbeta(10, shape1=3, shape2=5)), TRUE)
#' th3 <- TreeHarp(quote(fn1 <- function(x) x + 2), TRUE)
#' th4 <- TreeHarp(quote(df1 <- mutate(df1, new_col=2*old_col)), TRUE)
#'
#' # Run helpers
#' count_self_fn(th3)
#' count_fn_call(th4, pkg_name="dplyr")
#' count_fn_call(th1, pattern="^r.*")
#'
#' @name forestharp-helpers
NULL

#' @describeIn forestharp-helpers Counts the number of self-defined functions.
#'
#' This helper counts the number of self-defined functions. It excludes
#' lambda functions. It returns an integer scalar.
#'
#' As long as the function \code{function} was called and assigned, it will be
#' counted.
#'
#' @export
count_self_fn <- function(th) {
  #adj_list <- get_adj_list(th)
  node_types <- get_node_types(th)

  fn_defs <- dplyr::filter(node_types, .data$name == "function",
                           .data$call_status == TRUE)
  if(nrow(fn_defs) == 0)
    return(0L)

  fn_def_ids <- fn_defs$id
  parent_ids <- sapply(fn_def_ids, get_parent_id, x=th)

  sum(node_types$name[parent_ids] %in% c("=", "<-"))
}

#' @describeIn forestharp-helpers Counts the number of anonymous functions.
#'
#' Counts the number of anonymous functions, typically used in sapply, etc. It
#' returns an integer scalar. As long as the function \code{function} was
#' called but \emph{not} assigned, it will be counted here.
#'
#' @export
count_lam_fn <- function(th) {
  #adj_list <- get_adj_list(th)
  node_types <- get_node_types(th)

  fn_defs <- dplyr::filter(node_types, .data$name == "function",
                           .data$call_status == TRUE)
  if(nrow(fn_defs) == 0)
    return(0L)

  fn_def_ids <- fn_defs$id
  parent_ids <- sapply(fn_def_ids, get_parent_id, x = th)

  sum(!node_types$name[parent_ids] %in% c("=", "<-"))
}

#' @describeIn forestharp-helpers Counts the number of function calls that match a pattern.
#'
#' This helper counts the number of function calls that match a pattern. It
#' returns a count, i.e. an integer vector of length 1.
#'
#' If \code{pkg_name} is provided instead of \code{pattern}, then this function
#' counts the number of function calls from that package.
#'
#' @param pattern A regular expression to pick up function names.
#' @param pkg_name The name of a package to match functions with. This should
#' be an exact match for the package name. The package should be attached for
#' this to work. In order to avoid picking up duplicate names, for instance
#' \code{tolower} is a function in base R and in ggplot2, run
#' \code{\link{get_libraries}} on the file as well, and match against it.
#'
#' @export
count_fn_call <- function(th, pattern, pkg_name) {
  #adj_list <- get_adj_list(th)
  node_types <- get_node_types(th)
  
  # modify node_types for presence of magrittr pipe operator
  # check for "%<%" or "%>%"
  # For "%<%", 
  # - extract left hand node
  # - if no children, this must be a call; change call status to TRUE
  # For "%>%", 
  # - extract right hand node
  # - if no children, this must be a call; change call status to TRUE
  # Without this block, count_fn_call() misses out "View" on 
  # expressions such as x %>% View.
  pipe_matches <- match(node_types$name, c("%<%", "%>%"))
  pipe_ids <- which(!is.na(pipe_matches))
  if(length(pipe_ids) > 0){
    for(ii in pipe_ids) {
      if(node_types$name[ii] == "%<%"){
        left_child_id <- get_child_ids(th, ii)[1]
        children_of_left <- get_child_ids(th, left_child_id)
        if(is.null(children_of_left)) {
          node_types$call_status[left_child_id] <- TRUE
        }
      } else {
        # "%>%"
        right_child_id <- get_child_ids(th, ii)[2]
        children_of_right <- get_child_ids(th, right_child_id)
        if(is.null(children_of_right)) {
          node_types$call_status[right_child_id] <- TRUE
        }
      }
    }
  }
  
  if(!missing(pattern) && !missing(pkg_name)){
    stop("Only one of pattern or pkg_name should be supplied.")
  }

  if(!missing(pattern)) {
    fn_calls <- dplyr::filter(node_types, stringr::str_detect(.data$name, pattern),
                              .data$call_status == TRUE)
    return(nrow(fn_calls))
  }
  if(!missing(pkg_name)){
    #requireNamespace(pkg_name)
    fn_list <- ls(getNamespace(pkg_name))
    fn_calls <- dplyr::filter(node_types, .data$call_status == TRUE)

    return(sum(fn_calls$name %in% fn_list))
  }
}

#' @describeIn forestharp-helpers Extracts function calls as a string.
#'
#' Extracts the function calls that match a pattern. It returns a character
#' vector. Remember to set \code{combine = FALSE} when calling
#' \code{\link{fapply}} with it.
#'
#' @export
extract_fn_call <- function(th, pattern, pkg_name) {
  # adj_list <- get_adj_list(th)
  node_types <- get_node_types(th)
  if(!missing(pattern) && !missing(pkg_name)){
    stop("Only one of pattern or pkg_name should be supplied.")
  }

  if(!missing(pattern)) {
    fn_calls <- dplyr::filter(node_types, stringr::str_detect(.data$name, pattern),
                              .data$call_status == TRUE)
    return(fn_calls$name)
  }
  if(!missing(pkg_name)){
    #requireNamespace(pkg_name)
    fn_list <- ls(getNamespace(pkg_name))
    fn_calls <- dplyr::filter(node_types, .data$call_status == TRUE)

    return(fn_calls$name[fn_calls$name %in% fn_list])
  }
}

#' @describeIn forestharp-helpers Extracts function formal arguments called.
#'
#' Extracts the function \emph{formal} arguments from functions with a given
#' name. The name must match the function name exactly. This returns a character
#' vector or NULL, if no formal arguments are used.
#'
#' @param fn_name The (exact) function name.
#'
#' @export
extract_formal_args <- function(th, fn_name) {
  # adj_list <- get_adj_list(th)
  node_types <- get_node_types(th)

  fn_calls <- dplyr::filter(node_types, .data$name == fn_name,
                            .data$call_status == TRUE)
  fn_call_ids <- fn_calls$id
  child_ids <- lapply(fn_call_ids, get_child_ids, x=th)
  if(length(child_ids) == 0L) {
    return(NULL)
  } else {
    child_ids <- unlist(child_ids)
  }
  fn_args <- dplyr::filter(node_types, .data$id %in% child_ids,
                           .data$formal_arg == TRUE)
  if(nrow(fn_args) == 0)
    return(NULL)
  return(fn_args$name)
}

#' @describeIn forestharp-helpers Extracts names of assigned objects
#'
#' Extracts the names of assigned objects. This was written to assist in
#' detecting missed opportunities to use the pipe operator.
#'
#' @export
extract_assigned_objects <- function(th) {
  # adj_list <- get_adj_list(th)
  node_types <- get_node_types(th)

  all_assign_rows <- dplyr::filter(node_types, .data$name %in% c("<-", "="))
  if(nrow(all_assign_rows) == 0)
    return(character(0))
  child_id_list <- lapply(all_assign_rows$id, get_child_ids, x=th)
  first_child_id <- sapply(child_id_list, function(x) x[1])
  #first_child_call_status <- node_types$call_status[first_child_id]
  #browser()
  sub_tree_list <- lapply(first_child_id, function(x) {
    if(node_types$call_status[x]){
      subtree_at(th, x, TRUE)
    } else {
      subtree_at(th, x, FALSE)
    }
  })
#      subtree_at, obj=th,
#                          preserve_call=TRUE)

  sapply(sub_tree_list, function(x) x@repr, USE.NAMES = FALSE)
}

#' @describeIn forestharp-helpers Extracts actual argument names
#'
#' Extracts the actual arguments from an expression, not the formal
#' arguments. It only returns syntactic literals. It should be improved
#' to return the actual arguments for a specified function so that something
#' similar to \code{extract_assigned_objects} could be returned.
#'
#' @param include_assigned_obj A Boolean flag. If TRUE (default), it also returns
#' the name of the assigned object. If FALSE, it drops the name of the assigned 
#' object. The FALSE flag is useful for when checking if the we wish to match the 
#' assigned object from a previous call with the actual argument in this call.
#'
#' @export
extract_actual_args <- function(th, include_assigned_obj=TRUE) {
  node_types <- get_node_types(th)
  
  if(!include_assigned_obj) {
    # browser()
    all_assign_rows <- dplyr::filter(node_types, .data$name %in% c("<-", "="))
    if(NROW(all_assign_rows) > 0){
      top_assign_id <- min(all_assign_rows$id)
      child_ids <- get_child_ids(th, top_assign_id)
      if(!node_types$call_status[child_ids[2]]) {
        return(node_types$name[child_ids[2]])
      }
      th_sub <- subtree_at(th, child_ids[2], TRUE)
      node_types <- get_node_types(th_sub)
    }
    
  }
  
  actual_arg_rows <- dplyr::filter(node_types, !.data$call_status,
                                   !.data$formal_arg)
  if(nrow(actual_arg_rows) == 0)
    return(0L)
  actual_arg_rows$name
}

#' @describeIn forestharp-helpers Counts number of lines in a for loop
#'
#' Extracts the actual arguments from an expression, not the formal
#' First, this function checks if the expression contains a for loop. 
#' If it does, the innermost \code{for} loop is identified. Finally, the 
#' number of expressions below it is counted and returned.
#'
#' @export
count_num_lines_for_loop <- function(th) {
  if(count_fn_call(th, "^for$") == 0){
    return(0)
  }
  
  node_types <- get_node_types(th)
  
  # extract id of innermost "for" loop, and the children of it
  innermost_for_id <- max(which(node_types$name == "for"))
  for_children <- get_child_ids(th, innermost_for_id)
  
  # Get the third child
  th_sub <- subtree_at(th, for_children[3], TRUE)
  
  node_types_sub <- get_node_types(th_sub)
  if(node_types_sub$name[1] == "{"){
    return(length(get_child_ids(th_sub, 1)))
  } else {
    return(1)
  }
  
}


#' @describeIn forestharp-helpers Detects if a vector is being grown.
#'
#' It detects if there is an expression of form: x <- c(x, new_val). This is
#' generally bad programming practice
#'
#' @param count For \code{detect_growing}, this is a logical value that indicates
#' if the number of "grow" expressions should be counted and returned, or if just a
#' logical value should be returned.
#' @param within_for If TRUE, only expresssions within a for loop are included.
#'
#' @export
detect_growing <- function(th, count=FALSE, within_for=FALSE) {
  nt <- get_node_types(th)

  # Get nodes named c/append, get parents
  c_rows <- dplyr::filter(nt, .data$name %in% c("c", "append"))
  if(nrow(c_rows) == 0)  {
    if(count) return(0) else return(FALSE)
  }
  if(within_for) {
    # browser()
    paths_to_root <- lapply(c_rows$id, function(x) path_to_root(th, x))
    keep_ids <- sapply(paths_to_root,
                       function(x) {
                         "for" %in% nt$name[x==1]
                         })
    c_rows <- c_rows[keep_ids,]
    if(nrow(c_rows) == 0)  {
      if(count) return(0) else return(FALSE)
    }
  }

  # Check if parents of c/append is an assignment operator
  c_parents <- sapply(c_rows$id, get_parent_id, x=th)
  c_parents <- Filter(function(x) nt$name[x] %in% c("=", "<-"), c_parents)
  if(length(c_parents) == 0)
    return(FALSE)
  # browser()

  # check if assigned name is in argument to c/append
  # will fail if append called with formal argument
  c_assigned_name <- sapply(c_parents,
                                  function(y){
                                    child_ids <- get_child_ids(th, y)
                                    nt$name[child_ids[1]]
                                    })
  c_children_names <- lapply(c_rows$id,
                             function(y) {
                               child_ids <- get_child_ids(th, y)
                               nt$name[child_ids]
                               })
  detect_out <- mapply(function(x, y) x %in% y,
                       x=c_assigned_name, y=c_children_names)
  if(count){
    return(sum(unname(detect_out)))
  }
  any(detect_out)
}

#' @describeIn forestharp-helpers Detects if a for loop is present within a function
#'
#' It detects if a for loop is present within a function definition.
#'
#' @param fn_name Function name, as a character string
#'
#' @export
detect_for_in_fn_def <- function(th, fn_name) {
  # check if this is an assignment
  nt <- get_node_types(th)
  nt_names <- nt$name
  if((nt_names[1] != "<-") || (nt_names[2] != fn_name) || (nt_names[3] != "function"))
      return(FALSE)

  # look for within the nodes
  for_id <- which(nt_names == "for")
  if(length(for_id) > 0  && nt$depth[for_id] > 2)
      return(TRUE) else return(FALSE)
}

#' @describeIn forestharp-helpers Count use of a function within another.
#'
#' It counts the number of times a function is used within another.
#'
#' @param sub_fn (For count_fn_in_fn), the function to count (to look for within
#' fn_name).
#'
#' @export
count_fn_in_fn <- function(th, fn_name, sub_fn) {
  # check if this is an assignment
  nt <- get_node_types(th)
  nt_names <- nt$name
  if((nt_names[1] != "<-") || (nt_names[2] != fn_name) || (nt_names[3] != "function"))
      return(0)

  # look for within the nodes
  sub_fn_count <- count_fn_call(th, pattern=sub_fn)
  sub_fn_count
}

#' @describeIn forestharp-helpers Detect for loop to call a function
#'
#' Checks if a function has been called within a for loop.
#'
#' @export
detect_fn_call_in_for <- function(th, fn_name) {
  for_loop_indicator <- count_fn_call(th, pattern="for")
  send_ltr_indicator <- count_fn_call(th, pattern=fn_name)
  if((for_loop_indicator == 0) || (send_ltr_indicator == 0)){
    return(FALSE)
  }

  nt <- get_node_types(th)
  send_ids <- which(nt$name == fn_name)
  paths_to_parent <- lapply(send_ids, path_to_root, th=th)
  parent_names <- lapply(paths_to_parent, function(x) nt$name[which(x == 1)])
  for_in_parent <- lapply(parent_names, function(x) "for" %in% x)
  return(any(unlist(for_in_parent)))
}

#' @describeIn forestharp-helpers Extract names of functions defined by user.
#'
#' Extracts names of user-defined functions. They may not all look nice, because
#' sum functions may be anonymous functions. This function needs to be improved.
#'
#' @export
extract_self_fn <- function(th) {
    fn_def <- count_self_fn(th)
    if(fn_def==0){
        return(NULL)
    }
    node_types <- get_node_types(th)
    out_tree <- subtree_at(th, 2, node_types$call_status[2])
    out_tree@repr
}

#' @describeIn forestharp-helpers Was a function called with a particular argument?
#'
#' Checks if a function was called with a particular argument, which could be
#' the formal or actual one. The immediate child of the function call node is
#' checked.
#'
#' @param arg The argument to check for within fn_name (as a character string).
#'
#' @export
detect_fn_arg <- function(th, fn_name, arg) {
  nt <- get_node_types(th)
  nt_names <- nt$name

  fn_ids <- which(nt_names == fn_name)
  if(length(fn_ids) == 0){
    return(FALSE)
  }
  arg_ids <- lapply(fn_ids, get_child_ids, x=th)
  check_args <- sapply(arg_ids, function(x) arg %in% nt_names[x])
  any(check_args)
}

#' @describeIn forestharp-helpers Was a nested "for" loop called anywhere within the code?
#'
#' Checks if a nested for-loop was called anywhere within the code. This returns
#' a logical scalar for each TreeHarp object given.
#'
#' @export
detect_nested_for <- function(th) {
  node_types <- get_node_types(th)
  for_ids <- which(node_types$name == "for")
  if(length(for_ids) == 0){
    return(FALSE)
  } else {
    for_count_along_branch <- vapply(for_ids,
      function(x) sum(node_types$name[which(path_to_root(th, x) == 1)] == "for"),
      FUN.VALUE=1L)
  }
  any(for_count_along_branch > 1)
}
