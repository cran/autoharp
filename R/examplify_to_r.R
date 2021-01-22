#' Convert from Examplify to R
#'
#' This function converts an Examplify script (from a student,html)
#' into an R script.
#'
#' The script has to be exported in html format, using a particular profile.
#' The questions are stored in tags nested under id = "answers" and tag type "h2".
#' The answers are stored in tags nested under id = "answers" and class
#' "content".
#'
#' Some of the student text may contain R code and text mixed up, so
#' tidy_source() may not work on those, since it is parsed.
#'
#' @param in_fname A file name of a student submission html file.
#' @param out_fname The output R script.
#' @param verbose Controls verbosity of output.
#'
#' @export
#' @return This will return NULL, but will generate an R script as output.
#'
examplify_to_r <- function(in_fname, out_fname, verbose=FALSE) {
  if(!requireNamespace("xml2", quietly=TRUE) ||
     !requireNamespace("rvest", quietly=TRUE) ||
     !requireNamespace("formatR", quietly=TRUE)) {
    stop("Please install rvest, xml2 and formatR before using this function.")
  }
  page1 <- xml2::read_html(in_fname)
  qns <- rvest::html_nodes(page1, "#answers h2") %>% rvest::html_text() %>%
    stringr::str_replace_all("\r\n", "\n#")
  ans <- rvest::html_nodes(page1, "#answers .content") %>%
    as.character
  #ans <- html_nodes(page1, "#answers .content") %>%
  #  html_text
  if(verbose){
    message("HTML parsed.\n")
  }
  if(length(qns) != length(ans)){
    warning(paste0("Questions and answers do not match up for ", in_fname))
  }

  full_text <- NULL
  for (ii in 1:length(qns)) {
    tmp_qn <- formatR::tidy_source(text = qns[ii], output=FALSE)$text.tidy
    tmp_ans <- stringr::str_split(ans[ii], "\n")[[1]]

    tmp_ans <- stringr::str_replace_all(ans[ii], "<br>", "\n") %>%
      xml2::read_xml() %>%
      rvest::xml_nodes("p") %>%
      rvest::html_text()
    full_text <- c(full_text, tmp_qn, "", tmp_ans, "")
    # full_text <- c(full_text, tmp_qn, "", ans[ii], "")
    #cat(ii, "\n")
  }
  writeLines(full_text, con=out_fname)
  if(verbose){
    message(out_fname, "written.\n")
  }
}
