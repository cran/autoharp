# wraps a chunk with autoharp.scalars hook with a try expression.
wrap_chunk <- function(chunk) {
  new_chunk <- chunk
  if(stringr::str_detect(chunk[1], "autoharp.scalars")){
    chunk_head <- chunk[1]
    chunk_tail <- chunk[length(chunk)]
    chunk_body <- chunk[-c(1, length(chunk))]
    new_chunk <- c(chunk_head, 
                   "try_out <- try({",
                   chunk_body,
                   "})", chunk_tail)
    
  }
  new_chunk
}

#' Returns solution environment and test code from template.
#' 
#' Generates objects for checking solution correctness.
#'
#' @param soln_fname An rmd file containing the checks to be
#'   run on the student solution.
#' @param pattern The pattern that identifies which chunks in the solution are 
#' are testing chunks. If this argument is missing, 
#' the default pattern used is "test".
#' @param knit_root_dir The root directory to use for knitting the rmd file. This
#' argument is optional. If it is missing, it uses the root directory in
#' knitr::opts_knit$get('root.dir').
#' @param render_only A logical value. If this is TRUE, then the solution is
#' run, rendered and returned. Otherwise the rendered html is deleted.
#' @param output The path to the knitted solution md file. This is usually 
#' deleted immediately, but sometimes we may want to keep it. This 
#' argument is passed on to \code{\link[knitr]{knit}}, so please refer to 
#' that page for the warnings about setting this argument when figures are 
#' involved.
#' @param dummy A logical value. If TRUE, then a dummy solution object is created
#' The environment will be empty, and the temporary test script will only have 
#' one line in it: `ah_dummy <- TRUE`. This solution object can be used to render 
#' a solution file without any correctness checks. Of course, one could just knit 
#' it directly, but this is still useful when testing things out.
#'
#' @details  Test code should be written in a chunk that generates scalars 
#' from student objects.
#' 
#' The solution file has to be an Rmd file (not an R script), because it 
#' relies on the autoharp.obj and autoharp.scalars knitr hooks being present.
#' 
#' In addition, if it is required that a solution object is to be tested against
#' the analogous object within the student environment, these objects should be 
#' listed within the autoharp.objs option of a code chunk. These objects will be 
#' copied with the "." prefix.
#' 
#' Here is an overview of how the function works:
#' \enumerate{
#'   \item Knit the solution file to generate the solution (or "correct") 
#'   objects. 
#'   \item Rename these with the "." prefix in the solution environment
#'   object.
#'   \item Extract the lines of test code into a temporary R script.
#'   \item Wrap those chunks that contain autoharp.scalars hook with tryCatch.
#'   \item Return the solution environment and path to the R test script.
#' }
#' 
#' Typically, the next step is to call \code{\link{check_correctness}}.
#' 
#' @return If render_only is FALSE, a list containing 2 components: the
#' environment populated by the solution rmd and the path to an R script
#' containing the test code.
#'
#' If render_only is TRUE, then the output list consists of the 
#' aforementioned environment, and the path to the rendered solution file
#' (html). This option is useful for debugging the solution file.
#' 
#' @export
#' @seealso \code{\link{check_correctness}}, \code{\link{render_one}}
#'
populate_soln_env <- function(soln_fname, pattern, knit_root_dir, 
                              render_only=FALSE, output=NULL, dummy=FALSE) {

  if(dummy) {
      e_soln <- new.env()
      tmp_fname <- tempfile(pattern = "test", fileext = ".R")
      cat("ah_dummy <- TRUE\n", file=tmp_fname)
      return(list(env=e_soln, test_fname = tmp_fname))
  }
  e_soln <- new.env()
  e_soln$.myfilename <- normalizePath(soln_fname)
  if(!missing(knit_root_dir)){
    old_root <- knitr::opts_knit$get("root.dir")
    knitr::opts_knit$set(root.dir = knit_root_dir)
  }
  soln_out <- knitr::knit(soln_fname, quiet=TRUE, envir=e_soln, output=output)
  if(render_only) {
      return(list(env=e_soln, soln_out=soln_out))
  }
  if(file.exists(soln_out))
    unlink(soln_out)

  # Check that all model scalars are generated and present:
  if(exists(".scalars_to_keep", e_soln)){
    scalars_in_soln <- vapply(e_soln$.scalars_to_keep, exists, envir=e_soln, FUN.VALUE=TRUE)
    if(any(!scalars_in_soln)){
      missing <- names(scalars_in_soln)[!scalars_in_soln]
      message("The following scalars are missing from solution environment: ", missing)
      message("Try running populate_soln_env with render_only = TRUE to figure this out.")
    }
  }

  # the chunk labels for test code should be "test"
  if(missing(pattern)) {
    pattern <- "test"
  }

  # get expect_calls
  #tt_list <- get_expect_calls(soln_fname)

  test_code <- extract_chunks(soln_fname, pattern=pattern)
  if(length(test_code) > 0) {
    test_code <- lapply(test_code, wrap_chunk)
    lines_all <- test_code %>% lapply(function(x) x[2:(length(x) - 1)]) %>% unlist
    # if(".scalars_to_keep" %in% names(e_soln)){
    # lines_all <- c(lines_all, " ",
    #                "get_objs <- mget(.scalars_to_keep, ifnotfound=NA)",   
    #                "mapply(base::assign, x=.scalars_to_keep, value=get_objs, MoreArgs = list(envir=.myenv))")
    # } 
  } else {
    stop("No testing code detected.")
    lines_all <- c("NULL")
  }
  
  tmp_fname <- tempfile(pattern = "test_", fileext = ".R")
  writeLines(text = lines_all, con = tmp_fname)

  if(!missing(knit_root_dir)){
    knitr::opts_knit$set(root.dir = old_root)
  }
  
  return(list(env=e_soln, #chunk=test_code,
              test_fname = tmp_fname))#, tt_list =tt_list))
}
