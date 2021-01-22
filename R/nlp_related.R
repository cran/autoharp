#' Count tokens in R/Rmd
#'
#' Count the individual tokens. Part of the NLP analysis process.
#'
#' @param fname The Rmd or R file name.
#' @param include_actuals Whether actual arguments/literals should be included.
#' If this is FALSE, then only calls and formal arguments will be used in
#' the count.
#'
#' @return A tibble. The tibble will contain a the frequency count for all 
#' tokens present in the student script.
#' @export
#'
#'
rmd_to_token_count <- function(fname, include_actuals=TRUE) {
  # get fharp object
  fh <- rmd_to_forestharp(fname, FALSE)

  # remove NA entries
  fh <- Filter(function(x) class(x) == "TreeHarp", fh)

  all_dfs <- lapply(fh, get_node_types)

  # combine all dataframes
  raw_tokens <- dplyr::bind_rows(all_dfs)

  if(!include_actuals) {
    raw_tokens <- dplyr::filter(raw_tokens,
                         .data$call_status == TRUE |  .data$formal_arg == TRUE)
  }

  summary_count <- dplyr::group_by(raw_tokens, .data$name) %>% 
    dplyr::count() %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fname = basename(fname)) %>%
    dplyr::rename(token = .data$name)

  summary_count
}
