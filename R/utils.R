#' Replace Special Characters in File Name
#' 
#' Replaces special characters in the name of an R or Rmd script. 
#'
#' @param dir_name A character string, referring to the directory of Rmd
#' files whose names should be replaced.
#' @param return_df A logical value, indicating if the old and new names should
#' be returned (in a tibble).
#' 
#' @details If a filename contains one of the following special characters 
#' (ignore the quotes here): "\code{[ <>()|\\:&;#?*']}", the  
#' \link[knitr]{knit} function will replace 
#' them with underscores. Hence the filenames in the autoharp input directory 
#' and the output directory will not match, even allowing for the change in 
#' file extension. This will cause problems when we try to run 
#' \code{\link{render_one}} again on the same input directory.
#' 
#' This function renames the files in the input directory by replacing all 
#' special characters there.
#' 
#' The NUS LMS (LumiNUS) introduces parenthesized names or numbers in order 
#' to make filenames unique, so this function is necessary for NUS 
#' instructors.
#'
#' @return A tibble containing the old and new names.
#' @export
#'
replace_sp_chars_filename <- function(dir_name, return_df = TRUE) {
  fnames <- list.files(dir_name)

  new_names <- stringr::str_replace_all(fnames, "[\\[ <>()|\\:&;#?*'\\]]", "_")
  out <- file.rename(from=file.path(dir_name, fnames), to=file.path(dir_name, new_names))
  if(!all(out)) {
    message("Some files could not be renamed..")
  }
  if(return_df) {
    return(tibble::tibble(old_names = fnames, new_names=new_names))
  }
}

#' Removes md Files when no HTML Present
#' 
#' Cleans up the autoharp output directory.
#'
#' @param dir_name The directory containing the files to be cleaned.
#' @param verbose If TRUE, then the files and directories being removed will be 
#' printed.
#' 
#' @details When batch rendering Rmd files, it is inevitable that some files fail. These
#' files would have their knit.md and utf.md present, but they would not have a
#' corresponding html file generated.
#'
#' This function is called for its' side-effect, to remove those lonely md files.
#'
#' If this clean-up is not done, when we try to re-run the files (perhaps with
#' some of the errors fixed), these straggling md files will cause problems. 
#' The most crucial one is that the Rmd files will not be re-knitted, even 
#' though they have been changed.
#'
#' @return No return value.
#'
#' @export
#'
clean_dir <- function(dir_name, verbose=FALSE) {
  all_md_files <- list.files(dir_name, pattern="md$")
  root_names <- sapply(all_md_files, xfun::sans_ext, USE.NAMES = FALSE) %>%
    unique()

  html_ver <- paste0(root_names, ".html")
  to_remove <- !file.exists(file.path(dir_name, html_ver))
  if(any(to_remove)) {
    exts_to_remove <- c(".knit.md", ".utf8.md", "_files")
    all_to_remove <- sapply(root_names[to_remove], paste0, exts_to_remove)
    if(verbose) {
      message("The following files will be removed:")
      message(paste(all_to_remove, collapse="\n"))
    }
    unlink(file.path(dir_name, all_to_remove), recursive = TRUE)
  }
}

#' Extracts the Packages Used in An Rmd File.
#'
#' The input filename could correspond to an R script or an Rmd file.
#'
#' @param fname The Rmd filename or R script.
#'
#' @return A character vector containing the packages used within the Rmd
#' document.
#'
#' @details The file is assumed to be either an R script or an Rmd file.
#' If it is found to be an Rmd file using \link{extract_chunks}, it is purl-ed before
#' libraries are extracted. If it is found to be NOT an Rmd, it is assumed
#' to be an R script and nothing is done to process it.
#' 
#' The file is not parsed, so even text files will work with this function.
#'
#' @export
#'
get_libraries <- function(fname) {
  #all_chunks <- extract_chunks(fname)
  all_chunks <- tryCatch(extract_chunks(fname), error = function(e) return(e))
  if("error" %in% class(all_chunks)){
      return(all_chunks)
  }

  if(!is.null(all_chunks)){
    all_chunks <- Filter(function(x) length(x) > 2, all_chunks)
    all_lines <- lapply(all_chunks, function(x) x[2:(length(x)-1)])
    src_lines <- unlist(all_lines)
  } else {
    src_lines <- readLines(fname, warn=FALSE)
  }
  #src_lines <- readLines(out_file)
  non_comments <- src_lines[!stringr::str_detect(src_lines, "^#")]

  libr <- non_comments %>%
    unlist(use.names = FALSE) %>%
    stringr::str_replace_all("\"","") %>%
    #stringr::str_extract_all("(?<=library\\()[^)]+|[a-z0-9A-Z\\.]+(?=\\:\\:)")
    stringr::str_extract_all("(?<=library\\()[^)]+|[a-z0-9A-Z\\.]+(?=\\:\\:)|(?<=require\\()[^)]+")

  libr <- Filter(length, libr) %>% unlist()
  #unlink(out_file)

  sort(unique(libr))
}

#' Extract chunks that match a pattern.
#' 
#' Extracts chunks whose labels match a pattern from the rmd file.
#' 
#' @param rmd_name  A character string, the name of the rmd file to get the
#' chunks from.
#' @param pattern The pattern to match within the label. In fact, the match is
#' applied to the whole chunk option.
#' 
#' @return A list of character vectors. Each vector contains the chunk from the
#' file. If no pattern is specified, all chunks are returned. Remember that
#' the chunk header and tail are also included in the returned list.
#'
#' @export
#'
extract_chunks <- function(rmd_name, pattern) {
  if(missing(pattern)){
    pattern <- ".*"
  }
  all_lines <- readLines(rmd_name, warn=FALSE)
  kept_chunks <- NULL
  copy_out <- FALSE
  
  for(ii in seq_along(all_lines)){
    cur_line <- all_lines[ii]
    
    if(stringr::str_detect(cur_line, knitr::all_patterns$md$chunk.begin)){
      if(stringr::str_detect(cur_line, pattern)) {
        copy_out <- TRUE
        chunk_lines <- NULL
      }
    }
    
    if(copy_out){
      chunk_lines <- c(chunk_lines, cur_line)
    }
    
    if(stringr::str_detect(cur_line, knitr::all_patterns$md$chunk.end)){
      if(copy_out) {
        copy_out <- FALSE
        kept_chunks <- c(kept_chunks, list(chunk_lines))
      }
    }
    
  }
  
  if(length(kept_chunks) == 0)
    return(NULL)
  else 
    return(kept_chunks)
}

#' Extract non-chunks from an Rmd file.
#' 
#' Extracts non-chunks from an Rmd file.
#' 
#' @param rmd_name  A character string, the name of the rmd file to get the
#' chunks from.
#' @param out_name An output filename, to dump the text to.
#' 
#' @return If out_name is missing, then a character vector is returned. If 
#' outfname is specified, then nothing is returned. The text is written to the 
#' file instead.
#'
#' @export
#'
extract_non_chunks <- function(rmd_name, out_name) {
  all_lines <- readLines(rmd_name, warn=FALSE)
  begin_ids <- which(stringr::str_detect(all_lines, knitr::all_patterns$md$chunk.begin))
  end_ids <- which(stringr::str_detect(all_lines, knitr::all_patterns$md$chunk.end))
  if(length(begin_ids) == 0 || length(end_ids) == 0){
      keep_lines <- all_lines
  } else {
      within_chunks <- mapply(":", begin_ids, end_ids, 
                              SIMPLIFY = FALSE,USE.NAMES = FALSE )
      within_chunks <- unique(unlist(within_chunks))
      
      keep_lines <- all_lines[-within_chunks]
  }

  if(missing(out_name)) {
    return(keep_lines)
  } else {
    writeLines(keep_lines, out_name)
  }
}

#' Copy an object from one env to another.
#'
#' A wrapper function that uses assign and get.
#' 
#' @param from_obj The name of the object to copy. It has to be a string.
#' @param from_env The environment in which the object lives. It has to be an
#' object of class environment.
#' @param to_obj The name of the object to assign it to, in the new environment.
#' Also a string.
#' @param to_env The environment to which the new object is to be assigned. It
#' has to be an object of class environment.
#'
#' @return There is no return value. This function is called for its' side
#' effect.
#' @export
#'
#' @examples
#'
#' e1 <- new.env(); e2 <- new.env()
#' ls(e2)
#' evalq(x <- 1L, e1)
#' copy_e2e("x", e1, "y", e2)
#' ls(e2)
copy_e2e <- function(from_obj, from_env, to_obj, to_env) {
  assign(to_obj, get(from_obj, envir=from_env), envir = to_env)
}

# # run soln, gather objects into e_soln
# # run student, gather objects into e_stud
# # copy e2e
# # extract test_chunk into temp file.
# # execute tests
#
# stud_fname <- "test_many_things_student.Rmd"
# e_stud <- new.env()
# soln_fname <- "test_many_things.Rmd"
# obj_to_copy <- ls(e_soln, pattern="_soln")
# stud_out <- knit(stud_fname, quiet=TRUE, envir=e_stud)
# copy_out <- sapply(obj_to_copy, function(zzz) copy_e2e(zzz, e_soln, zzz, e_stud))
# tmp_fname <- tempfile(pattern="test_", ".", fileext = ".R")
# test_code <- extract_chunks(soln_fname, pattern="test")
# writeLines(test_code[2:(length(test_code)-1)], tmp_fname)
# test_file(tmp_fname, env = e_stud)
#
# #
# returns solution environment and test code chunks (as character vectors)
# knit_hooks set inside (yes)
#

autoharp_hooks <- list(
  autoharp.objs = function(before, options, envir) {
    if(before) {
      ss <- options$autoharp.objs
      sapply(ss, function(x) copy_e2e(x, from_env=envir,
                                      to_obj = paste0(".", x), 
                                      to_env = envir))
    }
  },
  autoharp.scalars = function(before, options, envir) {
    if(before) {
      checkout <- options$autoharp.scalars
      
      if(".scalars_to_keep" %in% names(envir)) {
        xset <- assign(".scalars_to_keep", 
                 c(get(".scalars_to_keep", envir = envir), checkout), envir = envir)
      } else {
        xset <- assign(".scalars_to_keep", checkout, envir = envir)
      }
    }
    invisible(1)
  }#,
  # autoharp.set = function(...) {
  #   dots <- list(...)
  #   ndots <- length(dots)
  #   if(ndots > 0L) {
  #     autoharp_hooks <<- c(autoharp_hooks, dots)
  #   }
  # }
)

