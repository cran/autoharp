#' Generate a dataframe from the log file.
#'
#' @param log_file The name of the log file generated from 
#' \code{\link{render_one}}.
#'
#' @return The function returns a dataframe summarising the details in the
#' log file.
#' 
#' @details This provides a table view of the log file, which is updated 
#' in a more natural format by simply concatenating new updates. The output 
#' of this function makes it easier to group entries by filename, time, or
#' status, or even error message.
#' 
#' The output table does not contain correctness output. It only contains the 
#' columns name, timestamp, status (SUCCESS/FAIL), error message, 
#' number of libraries used and number of libraries installed.
#'
#' @seealso \code{\link{render_one}}
#' 
#' @importFrom stats na.omit
#'
#' @export
#'
log_summary <- function(log_file) {
  file_details <- readLines(log_file,encoding = "UTF-8")

  front_blocks <- which(stringr::str_detect(file_details, "^---render_one"))
  end_blocks <- c(front_blocks[-1] - 1, length(file_details))
  render_blocks <- mapply(c, front_blocks, end_blocks, SIMPLIFY = FALSE)
  output <- list()
  for(each_block in render_blocks) {
    curr_block <- file_details[seq(each_block[1], each_block[2])]


    timestart <- stringr::str_extract(curr_block[1], "(?<=at )[^+]+(?=\\-\\-\\-)")
    front_chunk <- which(stringr::str_detect(curr_block, "Starting on"))
    end_chunk <- c(front_chunk[-1] - 2, length(curr_block))
    indiv_chunk <- mapply(c, front_chunk, end_chunk, SIMPLIFY = FALSE)


    df <- lapply(indiv_chunk, function(x) {
      # name, timestamp, status, error_msg, num_libr_used, num_libr_install
      # vector of 6
      indiv_entry <- rep(NA, 6)
      this_chunk <- curr_block[seq(x[1], x[2])]
      indiv_entry[1] <- stringr::str_extract(this_chunk, "(?<=Starting on ).+") %>% na.omit
      indiv_entry[2] <- timestart

      if(diff(x) + 1 >= 3) {
        indiv_entry[3] <- stringr::str_extract(this_chunk, "FAIL|SUCCESS|UNKNOWN") %>% na.omit

        indiv_entry[5] <- stringr::str_extract(this_chunk, "(?<=Libraries used:).+") %>%
          trimws %>% na.omit %>% stringr::str_split(",") %>% sapply(function(x) nchar(x) > 0) %>%
          sum(na.rm = TRUE)

        need_install <- stringr::str_extract(this_chunk, "(?<=Need to install:).+") %>%
          trimws %>% na.omit %>% stringr::str_split(",") %>% sapply(function(x) nchar(x) > 0)
        if(length(need_install) == 0L)
          indiv_entry[6] <- 0
        else
          indiv_entry[6] <- need_install %>% sum(na.rm = TRUE)
          #sum(na.rm = TRUE)
      } else {
        if(length(this_chunk[-1] > 0)){
          indiv_entry[3] <- this_chunk[-1]
        } else {
          indiv_entry[3] <- "UNKNOWN"
          indiv_entry[4:6] <- NA
        }
      }
      if(indiv_entry[3] == "FAIL") {
        indiv_entry[4] <- this_chunk[(which(stringr::str_detect(this_chunk, "FAIL"))+1):length(this_chunk)] %>%
          paste(sep = "", collapse = "")
      }

      indiv_entry
    })

    output <- c(output, df)
  }

  output <- data.frame(matrix(unlist(output), nrow = length(output), byrow = TRUE))
  colnames(output) <- c("name", "timestamp", "status",
                        "error_message", "num_libr_used", "num_libr_install")
  output
}

#a <- log_summary("../t11.log")
#b <- readLines("../t11.log", encoding = "UTF-8")
#c <- log_summary("../a.log")
#d <- readLines("../a.log", encoding = "UTF-8")

