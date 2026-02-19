#### from rmd/r to forest-harp

#' Convert to TreeHarp objects
#'
#' Reads in an Rmd file or an R script and converts it to a list of TreeHarp
#' objects.
#'
#' @param fname The filename that is to be read in.
#' @param verbose If TRUE, then messages will be printed to enable debugging.
#' dropped.
#'
#' @return There are three possible return outcomes: (1) NA, indicating that 
#' \code{\link[knitr]{purl}} was unable to extract the R expressions from the 
#' Rmd/qmd file. (2) A list with two components, named \code{forest} and
#' \code{line_nums}, each being NA. (3) A list with two components, named as in 
#' (2), containing the TreeHarp objects and a vector of line numbers.
#' 
#' If there are syntax errors in the file, the expressions may not be parsed
#' correctly. The \code{verbose} option will reflect this. 
#'
#' @details If an Rmd or qmd file is supplied as input, it is first converted
#' to an R script before \code{\link[lintr]{get_source_expressions}} is used 
#' to try to parse the expressions. These expressions are then matched to the
#' original file to retrieve line numbers. Unlike previous versions of this 
#' function, line numbers will always be returned.
#'
#' Expressions that could not be parsed will be returned as NA.
#' 
#' Line numbers are extracted using \code{\link[lintr]{get_source_expressions}} from
#' the lintr package.
#' 
#' @importFrom methods is
#' @importFrom utils tail
#' @export
#' @seealso \code{\link{fapply}}, \code{\link{extract_chunks}},
#' \code{\link{extract_chunks}}, \code{\link[lintr]{get_source_expressions}}
rmd_to_forestharp <- function(fname, verbose=FALSE) {
  # purl the file with doc 0L
  # delete the temp file
  file_extension <- stringr::str_to_lower(xfun::file_ext(fname))
  if(file_extension %in% c("rmd", "qmd")){
    if(verbose){
      message("[R|q]md found.")
    }
    tmp_fname <- tempfile("purl_out", fileext = ".R")
    out_fname <- tryCatch({
      knitr::purl(input=fname, output=tmp_fname, documentation = 0L, quiet=TRUE)
    }, error = function(e) return(e))
    if(is(out_fname, "error")){
      message("Error in purl call to ", fname)
      return(NA)
    }
  } else {
    if(verbose){
      message("R script found. Skipping purl..")
    }
    out_fname <- fname
  }
  
  src_expr <- lintr::get_source_expressions(filename = out_fname)
  num_expr <- length(src_expr$expressions) - 1
  if(num_expr == 0) {
    message("No expressions in ", fname)
    return(list(forest=NA, line_nums=NA))
  }
  if(verbose){
    message(num_expr, " expressions found.")
  }
  tree_list <- list()
  expr_found <- integer(0)
  jj <- 1
  # browser()
  if(file_extension %in% c("rmd", "qmd")){ 
    unlink(tmp_fname)
  }
  
  if(verbose){
    message("Parsing expressions, converting to trees...")
  }
  for(ii in 1:num_expr){
    # remove comment lines
    parsed_c <- src_expr$expressions[[ii]]$parsed_content
    if(parsed_c$token[1] == "COMMENT" && NROW(parsed_c) == 1){
      next
    }
    ex1 <- tryCatch(str2lang(src_expr$expressions[[ii]]$content),
                    error = function(e) return(e))
    if(!is(ex1, "error")){
      tree_list[[jj]] <- autoharp::TreeHarp(ex1, TRUE)
      expr_found[jj] <- ii
      jj <- jj + 1
    } else{
      if(verbose){
        message(src_expr$expressions[[ii]]$content, " could not be parsed.")
      }
    }
  }
  if(length(tree_list) == 0){
    message("No expressions could be parsed in ", fname)
    return(list(forest=NA, line_nums=NA))
  }
  
  if(verbose){
    message("Matching expressions with lines in original file..")
  }
  # browser()
  orig_r_lines <- readLines(fname, warn=FALSE)
  search_start <- 1
  line_nums <- integer(0)
  for(ii in 1:length(tree_list)){
    
    match_output <- match_code_lines(src_expr$expressions[[expr_found[ii]]]$lines,
                                     orig_r_lines, search_start)
    if(is.na(match_output[1])) {
      if(verbose){
        message("No match for expression!\nSyntax errors may be present in: ", 
                src_expr$expressions[[expr_found[ii]]]$lines, "\n")
      }
      line_nums[ii] <- NA_integer_
    } else {
      line_nums[ii] <- match_output[1]
      search_start <- tail(match_output, n=1) + 1
    }
  }
  if(verbose){
    message("Done!")
  }
  
  remove_ids <- which(is.na(line_nums))
  if(length(remove_ids) > 0){
    tree_list <- tree_list[-remove_ids]
    line_nums <- line_nums[-remove_ids]
  }
  list(forest=tree_list, line_nums=line_nums)
}

match_code_lines <- function(code_lines, code_table, start_search=1) {
  table_len <- length(code_table)
  code_len <- length(code_lines)
  unnamed_code_lines <- unname(code_lines)
  if(start_search > table_len - code_len + 1) {
    return(NA)
  }
  match_found <- FALSE
  for(jj in start_search:(table_len - code_len + 1)) {
    if(identical(unnamed_code_lines, code_table[jj:(jj+code_len-1)])) {
      match_found <- TRUE
      restricted_match_ids <- jj:(jj+code_len-1)
      break
    }
  }
  if(match_found){
    return(restricted_match_ids)
  } else{
    return(NA)
  }
}

#### forest-harp apply

#' Apply a function to a forest of trees.
#'
#' A convenience function, for applying a function to many trees.
#'
#' @param fharp The output of rmd_to_forestharp. It could also just be a 
#' list of TreeHarp objects.
#' @param TFUN A function that works on a single TreeHarp and returns an output. 
#' See \link{forestharp-helpers} for examples.
#' @param combine A logical value that indicates if the output from all function
#' applications should be combined.
#' @param combiner_fn A function to use to combine the individual output from 
#' each tree into a single scalar for each forest. It should handle NA 
#' values in the input vector or list. If it is missing, it defaults to 
#' sum, with na.rm=TRUE.
#' @param ... Additional arguments to be passed on to TFUN.
#' 
#' @details The input is simply a list of TreeHarp objects. First, the TFUN 
#' function is lapply-ed to each TreeHarp item, resulting in either a list, 
#' or a vector with possible NA elements.
#' 
#' The combiner function should be aware of this sort of output, and summarise
#' the list or vector accordingly, handling NA's and returning a \emph{scalar}.
#' 
#' If you need to create a partial function out of a forestharp helper, 
#' use an anonymous function, as shown in the examples below.
#'
#' @return A vector, list or a single value. If TFUN returned an error for a 
#' particular TreeHarp, that component in the list or vector would be NA. This 
#' input vector or list will then be combined by combiner_fn.
#' @importFrom methods is
#' @export
#' 
#' @examples 
#' 
#' ex1 <- quote(X <- rnorm(10, mean=0.9, sd=4))
#' ex2 <- quote(Y <- rbeta(10, shape1=3, shape2=5))
#' f1 <- lapply(c(ex1, ex2), TreeHarp, quote_arg=TRUE) 
#' 
#' # returns all function calls that begin with "r", like rnorm and rbeta.
#' # calls are returned as a list.
#' fapply(f1, extract_fn_call, combine =FALSE, pattern="^r.*")
#' 
#' # list is catenated.
#' fapply(f1, extract_fn_call, combine =TRUE, pattern="^r.*", 
#'        combiner_fn = function(x) {paste0(unlist(x), collapse=",")})
#'
fapply <-  function(fharp, TFUN, combine = TRUE, combiner_fn, ...) {
  if("line_nums" %in% names(fharp)){
    fharp <- fharp$forest
  }
  
  f_wrap <- function(x, ...) {
    if(!is(x, "TreeHarp")){
      return(NA)
    }
    # trycatch it here.
    tryCatch(TFUN(x, ...), error = function(e) return(NA))
  }
  out_list <- lapply(fharp, f_wrap, ...)
  if(combine) {
    if(missing(combiner_fn)){
      return(sum(unlist(out_list), na.rm = TRUE))
    } else {
      out <- combiner_fn(out_list)
      return(out)
    }
  } else {
    return(out_list)
  }
}

#' Identify if use of pipe operator can improve a code section
#'
#' @param fname A filename - either a Rmd/qmd or R script.
#' @param window_len A window length to analyse. 
#' 
#' @details A rolling window approach is used here. For each 
#' expression, the assigned object is extracted (if any). Subsequent lines are 
#' checked to see if this object appears as an actual argument.
#' 
#' Window length two means that only one more line is checked.
#'
#' @returns A numeric vector corresponding to start of the window to be reviewed.
#' If no lines are found, NULL is returned.
#' 
#' @export
#'
pipe_can_improve_this <- function(fname, window_len = 2) {
  # need line numbers
  fh <- rmd_to_forestharp(fname, verbose=FALSE)
  lines_found <- NULL
  
  fh_len <- length(fh$line_nums)
  for(i in 1:(fh_len - window_len + 1)) {
    node_types <- get_node_types(fh$forest[[i]])
    
    # check for assignment
    is_assignment <- node_types$name[1] %in% c("<-", "=")
    if(!is_assignment) 
      next
    
    # if assignment, 
      # extract assigned obj
    assigned_objs <- extract_assigned_objects(fh$forest[[i]])
    actual_args_window <- lapply(fh$forest[(i+1):(i+window_len-1)], 
                                 extract_actual_args,
                                 include_assigned_obj=FALSE)
    actual_args_window <- unique(unlist(actual_args_window))
    if(any(!is.na(match(assigned_objs, actual_args_window)))) {
      #browser()
      lines_found <- c(lines_found, fh$line_nums[i])
    }
    
  }
  
  # return identified lines
  lines_found
}

#' Checks if two expressions differ only in terms of actual arguments
#'
#' @param th1 A TreeHarp object.
#' @param th2 A TreeHarp object.
#' 
#' @details From the node types, only calls and formal arguments are retained.
#' If these are identical, then a 1 is returned. Otherwise 0 is returned.
#'
#' @returns Either 1 or 0. 1 means that the two expressions differ only in their 
#' actual arguments. 
#' 
#' @export
#'
#' @examples
#' ex1 <- TreeHarp(quote(X <- rnorm(10, mean=0.9, sd=4)), TRUE)
#' ex2 <- TreeHarp(quote(Y <- rnorm(20, mean=9, sd=4)), TRUE)
#' only_actual_args_differ(ex1, ex2)
only_actual_args_differ <- function(th1, th2) {
  nodes1_minus_aa <- get_node_types(th1) %>% 
    dplyr::filter(.data$call_status | .data$formal_arg)
  nodes2_minus_aa <- get_node_types(th2) %>% 
    dplyr::filter(.data$call_status | .data$formal_arg)
  
  if(identical(nodes1_minus_aa, nodes2_minus_aa)) {
    return(1L)
  } else {
    return(0L)
  }
}


#' Identifies if an apply function can improve the code
#'
#' @param fname A filename: either Rmd/qmd or R.
#' @param window_size A window size to analyse.
#' 
#' @details First, all library() calls and read() calls are removed. Next,
#' \code{only_actual_args_differ} is run on all pairwise expressions. The 
#' matrix is searched for blocks of 1's along the diagonal.
#' 
#'
#' @returns A numeric vector corresponding to windows for review is returned. If 
#' none are found, then NULL is returned.
#' @export
#'
apply_can_improve_this <- function(fname, window_size=2) {
  # filter to remove "library" and "read" calls
  f1 <- rmd_to_forestharp(fname, verbose=FALSE)
  remove_these <-  which(fapply(f1, count_fn_call, 
                                pattern="(^library$)|(^read.*)", 
                                combiner_fn = function(x) unlist(x)) > 0)
  if(length(remove_these) > 0) {
    f1a <- list(forest = f1$forest[-remove_these], 
                line_nums = f1$line_nums[-remove_these])
  } else {
    f1a <- f1
  }
  
  only_actual_args_differ_v <- Vectorize(only_actual_args_differ, 
                                         vectorize.args = c("th1", "th2"))
  
  sim_mat <- outer(f1a$forest, f1a$forest, only_actual_args_differ_v) 
  
  window_slider <- matrix(1L, nrow=window_size, ncol=window_size)
  line_nums <- NULL
  for(i in 1:(NROW(sim_mat)-window_size +1)) {
    if(identical(window_slider, sim_mat[i:(i+window_size-1),i:(i+window_size-1)])){
      line_nums <- c(line_nums, f1a$line_nums[i])
    }
  }
  line_nums
}
