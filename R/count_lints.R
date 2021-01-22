#' File lint counter
#' 
#' Count number of lints in one file
#' 
#' @param rmd_file The path to the rmd file to check for lints.
#' @param lint_list List of lints to check for. 
#' @param lint_labels List of labels to name the vector to return.   
#' 
#' @details The function will count the number of lints in a file. The lints to be 
#' checked can be passed as an argument. Else, the default will be used. 
#' The defaults are as follows: 
#' * T_and_F_symbol_linter
#' * line_length_linter
#' * assignment_linter
#' * absolute_path_linter
#' * pipe_continuation_linter
#' Note that labels would also need to be given if the non-default lints 
#' are chosen.
#' 
#' @return Vector containing the lints. 
#' 
#' @export

count_lints_one <- function(rmd_file, lint_list, lint_labels) {
  if (missing(lint_list)) {
    lint_list <- c(lintr::T_and_F_symbol_linter,
                   lintr::line_length_linter,
                   lintr::assignment_linter,
                   lintr::absolute_path_linter,
                   lintr::pipe_continuation_linter)
  }
  
  if (missing(lint_labels)) {
    lint_labels <-  c("T_and_F", "Line_Length", "Assignment", "Abs_Path",
                      "Pipe")
  }
  
  if (length(lint_labels) != length(lint_list)) {
    message("Lint list and labels are not of the same length")
    return(NULL)
  }
  lint_count <- rep(NA, times = length(lint_labels))
  tryCatch(
    {
      lint_count <- suppressWarnings(
        sapply(lint_list, function(x) length(lintr::lint(rmd_file, x))))
    }, 
    error = function(cond) {
      message(paste0("Error with file: ", rmd_file))
    }, 
    finally = {
      names(lint_count) <- lint_labels
    }
  )
  return(lint_count)
}


#' Lint counter
#' 
#' Count number of lints in one folder
#' 
#' @param file_names The path to the rmd files that need to be checked for 
#' lints.
#' @param lint_list List of lints to check for. 
#' @param lint_labels List of labels to name the vector to return.   
#' 
#' @details The function will count the number of lints in a file. The lints to be 
#' checked can be passed as an argument. Else, the default will be used. 
#' The defaults are as follows: 
#' * T_and_F_symbol_linter
#' * line_length_linter
#' * assignment_linter
#' * absolute_path_linter
#' * pipe_continuation_linter
#' 
#' Note that labels would also need to be given if the non-default lints 
#' are chosen.
#' @md
#' 
#' @return Dataframe containing the lints. 
#' 
#' @export

count_lints_all <- function(file_names, lint_list, lint_labels) {
  if (missing(lint_list)) {
    lint_list <- c(lintr::T_and_F_symbol_linter,
                   lintr::line_length_linter,
                   lintr::assignment_linter,
                   lintr::absolute_path_linter,
                   lintr::pipe_continuation_linter)
  }
  
  if (missing(lint_labels)) {
    lint_labels <-  c("T_and_F", "Line_Length", "Assignment", "Abs_Path",
                      "Pipe")
  }
  
  if (length(lint_labels) != length(lint_list)) {
    message("Lint list and labels are not of the same length")
    return(NULL)
  }

  tabulated_lints <- t(sapply(file_names, count_lints_one, 
                              lint_list = lint_list, 
                              lint_labels = lint_labels))

  total <- rowSums(tabulated_lints)
  final_df <- cbind(file_names, tabulated_lints, total)
  final_df <- as.data.frame(final_df)
  rownames(final_df) <- NULL
  return(final_df)
}
