#' Check if a File is Rmd
#'
#' Checks if a file actually is an Rmd file.
#'
#' @param fname A character string. It is the name of the student submission
#' file.
#' @param verbose A logical value that prints messages if a non-rmd file
#' is found.
#'
#' @details It runs three checks. First, it checks for the file extension to be
#' Rmd or rmd or any such variant. Second, it checks for a YAML header at the
#' beginning of file. Finally, it checks if there is at least one properly
#' defined R chunk within the file.
#'
#' @export
#'
#' @seealso \code{\link{get_libraries}}
#'
#' @return The function will return TRUE if all the (3) checks pass, and FALSE
#' otherwise.
#'
check_rmd <- function(fname, verbose = TRUE) {
  fname_lower <- stringr::str_to_lower(fname)
  if(!stringr::str_detect(fname_lower, "\\.rmd$")){
    if(verbose) {
      message(paste(fname, "File extension is not Rmd.", sep=": "))
    }
    return(FALSE)
  }
  file_text <- readLines(fname, warn=FALSE)

  # YAML header should be at the beginning of the file.
  yaml_pos <- stringr::str_which(file_text, "^---$")
  if(length(yaml_pos) == 0 || yaml_pos[1] != 1) {
    if(verbose) {
      message(paste(fname, "YAML header not properly defined.", sep=": "))
    }
    return(FALSE)
  }

  # No. of starts should equal number of ends, and should be
  # positive.
  begin_ids <- which(stringr::str_detect(file_text, knitr::all_patterns$md$chunk.begin))
  end_ids <- which(stringr::str_detect(file_text, knitr::all_patterns$md$chunk.end))
  l_rchunk_starts <- length(begin_ids)
  l_rchunk_ends <- length(end_ids)
  
  if(l_rchunk_starts == 0 || l_rchunk_ends == 0 || 
     l_rchunk_ends != l_rchunk_starts) {
    if(verbose) {
      message(paste(fname, "No proper R chunks found.", sep=": "))
    }
    return(FALSE)
  }

  return(TRUE)
}
