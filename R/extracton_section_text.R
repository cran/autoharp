#' Extract section text from Rmd
#'
#' @param rmd_name The filename of the Rmd script.
#' @param hdr_pat  The regular expression pattern to pick up the section title.
#' @param ignore_case A boolean - whether or not to ignore case when matching for
#' the section title.
#'
#' @return
#'
#' Returns a character vector containing all the text written in the section
#' that begins with the specified pattern.
#'
#' The pattern should pick up a unique section/sub-section/sub-sub-section.
#' Otherwise, it will stop and raise an error.
#'
#' @export
#'
#' @details
#' The text that is picked up begins with the specified section, and ends with
#' the next string of pound symbols (#)
#'
extract_section_text <- function(rmd_name, hdr_pat, ignore_case=TRUE) {
  all_lines <- extract_non_chunks(rmd_name)
  num_lines <- length(all_lines)

  hdr_lines <- stringr::str_which(all_lines, "^#{1,6} ")
  if(length(hdr_lines) == 0){
    stop("No headings found.")
  } else {
    hdr_lines <- c(hdr_lines, num_lines)
  }

  r1 <- stringr::regex(paste0("^#{1,6} ", hdr_pat), ignore_case = ignore_case)
  match_hdr <- stringr::str_which(all_lines, r1)
  if(length(match_hdr) != 1){
    stop(paste(length(match_hdr), "matches found."))
  } else {
    next_stop <- hdr_lines[hdr_lines > match_hdr][1]
    next_stop <- ifelse(next_stop != hdr_lines[length(hdr_lines)], next_stop-1,
                        next_stop)
    out_lines <- all_lines[match_hdr:next_stop]
  }
  out_lines
}
