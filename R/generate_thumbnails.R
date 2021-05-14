#' Generate a html of thumbnails
#'
#' @param out_dir The directory in which student html files and the figures 
#' are kept.
#' @param html_fname The name of the master html file which will contain all
#' thumbnails. This file will be created in \code{out_dir}.
#' @param html_title The title tag of the master html page. This will be displayed 
#' on top of the output html page.
#' @param anonymise If TRUE, the original filenames will be replaced with
#'        inocuous numbers. If FALSE, the original filenames will be retained.
#'
#' @details After running \code{\link{render_one}} on a set of R/Rmd files 
#' in a directory, this function helps to consolidate them for review.
#' 
#' The output folder contains all the generated html files, images and a log 
#' file. This function will extract the images from each html file and display 
#' them as thumbnails on a new html page, with links to all individual files. 
#' 
#' @return The function returns nothing, but it should create a html page 
#' of thumbnails of all the images that students plotted, along with links to 
#' their individual pages.
#' @export
generate_thumbnails <- function(out_dir, html_fname, html_title,
                                anonymise=FALSE) {
  out_html <- file.path(out_dir, html_fname)
  # start from scratch again. 
  if(file.exists(out_html)) {
    unlink(out_html)
  } 
  overall_con <- file(out_html, open='wt')
  #cat("<!DOCTYPE html>", "<html>", "<head>", file=overall_con, sep='\n')
  #cat(as.character(tags$meta(charset="UTF-8")), file=overall_con, sep='\n')
  cat(write_css(), write_jscript(), file=overall_con, sep='\n')
  cat(as.character(shiny::tags$title(html_title)), file=overall_con, sep='\n')
  #cat("</head>", file=overall_con, sep='\n')
  
  cat("<body>", file=overall_con, sep='\n')
  cat(as.character(shiny::tags$h1(html_title)), file=overall_con, sep='\n')
  
  # removes the thumbnail html itself.
  h_files <- list.files(out_dir, "*html") %>% 
    setdiff(html_fname)
  
  i <- 1
  
  for(fname in h_files){
    root_fname <- remove_extension(fname)
    try_out_files <- file.path(out_dir, paste0(root_fname, "_files"))
    
    divsize <- c("small", "imgsmall")
    all_imgs <- NULL
    if(dir.exists(try_out_files)) {
      img_fnames <- list.files(try_out_files, recursive = TRUE)
      sq_imgs <- ceiling(sqrt(length(img_fnames)))
      
      if(sq_imgs < 3) divsize <- c("small", "imgsmall")
      else if(sq_imgs < 4) divsize <- c("medium", "imgmedium")
      else if(sq_imgs >= 4) divsize <- c("large", "imglarge")
      
      all_imgs <- file.path(basename(try_out_files), img_fnames) %>%
        lapply(function(x) 
          shiny::tags$figure(class = "figimg", class = divsize[2],
          shiny::tags$img(src = x, title = x, alt = x))) %>%
        lapply(as.character) %>% unlist() %>% paste(collapse = "\n")
    }

    cat(as.character(
      shiny::tags$div(class = "imgContainer", class = divsize[1],
        shiny::tags$figure(
          shiny::HTML(all_imgs),
          shiny::tags$figcaption(
            shiny::tags$a(href=fname, title=fname, 
                   ifelse(anonymise, paste0("File_Num_", sprintf("%03d", i)), 
                          root_fname))
        ))
      )), file=overall_con, sep="\n")
    i <- i + 1
  }
  cat("</body>", "</html>", file=overall_con,sep='\n')
  close(overall_con)
}
