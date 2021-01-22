# To reset search path by detaching missing paths:

#' Reset search path of current R session
#' 
#' This function is used to detach packages that have been added by a student 
#' script.
#'
#' @param old_path A character vector of package namespaces. This is usually 
#' the output of \code{\link[base]{search}}, run before an R script or Rmd 
#' file is rendered, which could cause the search path to change.
#'
#' @return There is no object returned. This function is called for it's side-
#' effect of altering the search path.
#' 
#' @details When a student script is rendered using \code{\link{render_one}}, 
#' new packages might be added to the search path. These may conflict with the 
#' instructors' search path order, or with subsequent runs of 
#' \code{\link{render_one}} on students. Hence there is a need to reset the 
#' search path before this is done.
#' 
#' This function does not unload namespaces. It only detaches them from the 
#' search path. For a difference between the two, please see Hadley's page.
#' 
#' @export
#' @examples 
#' opath <- search()
#' # Load a package
#' reset_path(opath)
reset_path <- function(old_path) {
    cur_search_path <- search()
    path_mods <- setdiff(cur_search_path, old_path)
    while(length(path_mods) > 0) {
      id <- which(cur_search_path == path_mods[1])
      detach(pos=id[1])
      cur_search_path <- search()
      path_mods <- setdiff(cur_search_path, old_path)
    }
    return(invisible(1L))
}