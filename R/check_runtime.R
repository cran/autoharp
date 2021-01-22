#' Calculate Run-time Statistics
#'
#' This is stand-alone function. It computes the runtime stats
#' without rendering the md/html/pdf file.
#'
#' @param stud_fname  The rmd filename of the student.
#' @param knit_root_dir  The working directory to use when knitting the file.
#' @param return_env A logical value to indicate if the environment from the
#' rmd file should be return. If FALSE, an NA value is returned.
#' 
#' @details This routine is not used within any other function within the 
#' package. Figures are not cleaned or removed.
#'
#' @return A list containing the running time in seconds, the memory used
#' by the final environment in bytes (as a numeric scalar), and the
#' environment object containing all the generated objects from the rmd file.
#' @seealso \code{\link{render_one}}
#' @export
#'
check_runtime <- function(stud_fname, knit_root_dir, return_env=FALSE) {
  # create tempdir
  if(!missing(knit_root_dir)){
    old_root <- knitr::opts_knit$get("root.dir")
    knitr::opts_knit$set(root.dir = knit_root_dir)
  }

  e_stud <- new.env()
  start_time <- Sys.time()
  stud_out <- knitr::knit(stud_fname, quiet=TRUE, envir=e_stud)
  end_time <- Sys.time()
  mem_usage <- env_size(e_stud)
  if(file.exists(stud_out)) {
    unlink(stud_out) # does not clean figures!
  }
  if(!missing(knit_root_dir)){
    knitr::opts_knit$set(root.dir = old_root)
  }
  runtime_secs <- as.numeric(difftime(end_time, start_time, units="secs"))
  out_obj <- list(run_time = runtime_secs,
                  mem_usage = mem_usage,
                  env=NA)
  if(return_env)
    out_obj$env <- e_stud
  return(out_obj)
}
