#' Conducts checks before rendering file
#'
#' This routine conducts two checks before rendering the file: presence of 
#' View function, and presence of system() calls. The former is a nuisance,
#' while the latter is possibly nefarious.
#' 
#' @param fname The name of the Rmd/qmd file to check.
#' @param in_place If TRUE, the original file will be overwritten in place.
#' @param new_name If in_place is FALSE, this should be the full path to the 
#' new file to be written (including directory location).
#' @param verbose If TRUE, debugging messages will be printed.
#'
#' @returns Either TRUE or FALSE. If TRUE, all View() lines have been commented,
#' and there are no system calls. If FALSE, it means there are system calls (to 
#' be screened further).
#' @export
#'
render_prechecks <- function(fname, in_place=TRUE, new_name, verbose=FALSE) {
  if(!in_place){
    if(missing(new_name)){
      stop("If in_place is FALSE, new_name cannot be missing.")
    }
  } else {
    new_name <- fname
  }
  f1 <- rmd_to_forestharp(fname, FALSE)
  if(is(f1, "logical")){
    if(verbose) {
      message("Could not run purl on:", fname)
    }
    return(FALSE)
  }
  if(identical(f1, list(forest=NA, line_nums=NA))) {
    if(verbose) {
      message("No expressions could be found in: ", fname)
    }
    return(FALSE)
  }
  
  # Comment out View calls
  contain_view <- unlist(fapply(f1, count_fn_call, 
                                pattern="^[vV]iew$", combine=FALSE))
  if(sum(contain_view) > 0) {
    # browser()
    ids <- f1$line_nums[contain_view  > 0]
    fname_org_lines <- readLines(fname)
    fname_org_lines[ids] <- paste0("# ", fname_org_lines[ids])
    writeLines(fname_org_lines, con=new_name)
    if(verbose){
      message(paste(length(ids), "lines commented."))
    }
  } else {
    if(verbose){
      message("No calls to View()")
    }
  }
  
  # Skip file if system calls present
  contain_system <- unlist(fapply(f1, count_fn_call, 
                                  pattern="^system2{0,1}$", combine=FALSE)) 
  if(sum(contain_system) > 0) {
    if(verbose){
      message("System calls found.. skipping render.")
    }
    return(FALSE)
  } else {
    return(TRUE)
  }
}
