#library(tidyverse)
#library(readxl)

# The local matching works in this way:
# The audit file needs to be downloaded. When doing so, set the folder properties
# on luminus to display the Student Name, not Number, and not anonymous!
# Next, unzip the downloaded files into a folder on your local machine.
#
# LumiNUS will append the student name (up to 15 characters max.) to the end of the
# original file name, whenever there are duplicate filenames.
#
# When checking if two files have the same name, case sensitivity is not used.
#
# When the same user uploads more than one file, a number is added to the end
# The most recent upload will not have one, but after that it begins from (1), etc.
#
# If modification time is very close, watch out!
#
# The final tibble "tmp" at the bottom should contain the local filenames
# mapped to the remote name, and more importantly, the student id.

name_string <- function(user_names) {
  if(length(user_names) == 1)
    return("")
  else{
    s_names = paste0("(", stringr::str_trim(stringr::str_sub(user_names, 1, 15)), ")")
    return(s_names)
  }
}

#' Match Filenames from LumiNUS.
#'
#' A utility function for resolving duplicate filenames on
#' LumiNUS. (Only useful for NUS instructors!)
#'
#' @param audit_file_path The audit file downloaded from LumiNUS. This must
#' be an Excel file. It comes from the "Download Activity" button for the
#' corresponding folder. It should contain columns such as "Action Time",
#' "Action", etc. In the folder settings, students should be identified by
#' their NAME.
#' @param local_files_dir The directory containing the files downloaded from
#' LumiNUS. It is usually downloaded as a zip-file and then extracted.
#' @param skip_name The username to skip. Usually this is the instructor's name. This 
#' must be present.
#'
#' @return It returns a tibble, containing the remote and local filenames, matched
#' to the userid of students. The columns in this tibble are
#' \enumerate{
#' \item mod_time: file modification time, from the downloaded file.
#' \item luminus_time: time that the file was uploaded to LumiNUS; retrieved
#' from audit trail.
#' \item local_fname: The downloaded local file name.
#' \item luminus_fname: The filename that we see on LumiNUS.
#' }
#'
#' @details Here is how LumiNUS works to resolve duplicate filenames: It
#' will append the students' filenames to the end of the file (in parenthesis),
#' but it will only use the first 15 characters of the students' name. In
#' LumiNUS, filenames are not case-sensitive - test.Rmd and test.rmd are
#' considered duplicate filenames.
#'
#' Here is how the function works: From the audit trail, it retrieves the name
#' of the most recent upload for each student. After converting these to
#' lowercase, duplicate file names have their student names appended. These
#' new names are matched to the filenames that were downloaded.
#'
#' Remember to clean up the filenames after this, because knitr does not
#' like parentheses in file names!
#'
#' @export
#' @seealso \code{\link{replace_sp_chars_filename}}
#'
#' @importFrom rlang .data
#'
lum_local_match <- function(audit_file_path, local_files_dir, skip_name) {
  if(!requireNamespace("readxl", quietly=TRUE)) {
    stop("Please install readxl before using this function.")
  }
  ## get file names and file dates
  fnames <- list.files(local_files_dir, full.names = TRUE)
  f_info <- file.info(fnames)
  f_info$local_fname <- row.names(f_info)
  f_info <- tibble::as_tibble(f_info) %>%
    dplyr::select("local_fname", "mtime") %>%
    dplyr::mutate(local_fname = basename(.data$local_fname))

  ## get filenames and upload times
  lum_info <- readxl::read_excel(audit_file_path) %>%
    dplyr::filter(.data$Name != skip_name,  .data$Action == "Upload File") %>%
    dplyr::rename(Action_Time = .data$`Action Time`, UserID = .data$`User ID`,
           S_Num = .data$`Student Number`, FName = .data$`File Name`) %>%
    dplyr::mutate(Action_Time = as.POSIXct(as.character(.data$Action_Time),
                                    tz="Asia/Singapore")) %>%
    dplyr::group_by(.data$Name) %>%
    dplyr::filter(.data$Action_Time == dplyr::last(.data$Action_Time, order_by=.data$Action_Time)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fname_lower = stringr::str_to_lower(.data$FName)) %>%
    dplyr::group_by(.data$fname_lower) %>% #arrange(Action_Time) %>%
    dplyr::mutate(file_root = stringr::str_sub(stringr::str_extract(.data$FName, "^.+\\.{1}"), end = -2),
           file_ext = stringr::str_sub(stringr::str_extract(.data$FName, "\\.{1}[^.]+$")),
           s_names = name_string(.data$Name)) %>%
           # s_names = name_string(Name, Action_Time)) %>%
    tidyr::unite("local_name", .data$file_root, .data$s_names, .data$file_ext, sep="") %>%
    dplyr::ungroup()

  tmp_r <- dplyr::select(lum_info, .data$`UserID`:.data$Name, .data$FName, .data$Action_Time, .data$local_name)
  tmp <-  dplyr::left_join(f_info, tmp_r, by=c("local_fname" = "local_name")) %>%
    dplyr::select(.data$mtime, .data$Action_Time, .data$UserID,
           .data$S_Num, .data$Name, .data$local_fname, .data$FName) %>%
    dplyr::rename(mod_time= .data$mtime, luminus_time = .data$Action_Time,
           local_fname = .data$local_fname, luminus_fname = .data$FName)
    # mutate(watch_out2 = as.numeric(difftime(mtime, Action_Time,units="secs")))
    #mutate(watch_out2 = if_else(abs(as.numeric(difftime(.data$mtime, .data$Action_Time,
    #                                                units="secs"))) > 5, TRUE, FALSE))
   tmp
}
