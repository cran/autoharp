library(stringr)
library(knitr)

## initialise Rmd file with minimal YAML
init_rmd <- function(fname, rmd_title="Untitled"){ 
  cat("---\n", file=fname)
  cat("title: ", rmd_title, "\n", sep="", file=fname, append=TRUE)
  cat("output: html_document\n", file=fname, append=TRUE)
  cat("---\n", file=fname, append=TRUE)
  cat("\n\n", file=fname, append=TRUE)
}

## add lines of code as a chunk to an Rmd file
add_chunk_rmd <- function(fname, chunk_name, chunk_lines, add_tryCatch=TRUE) {
  cat("```{r ", chunk_name, "}\n", sep="", file=fname, append=TRUE)
  if(add_tryCatch) {
    cat("tryCatch({ \n", file=fname, append=TRUE)
  }
  
  cat(chunk_lines, sep="\n", file=fname, append=TRUE)
  
  if(add_tryCatch) {
    cat("}, error = function(e) e )\n", file=fname, append=TRUE)
  }
  cat("```", "\n", sep="", file=fname, append=TRUE)
  cat("\n", file=fname, append=TRUE)
}

## add lines of text to an Rmd
add_text_rmd <- function(fname, sec_name="", text_lines) {
  if(sec_name != ""){
    cat("#### ", sec_name, "\n", sep="", file=fname, append=TRUE)
  }
  cat(text_lines, sep="\n", file=fname, append=TRUE)
  
  cat("\n", file=fname, append=TRUE)
}

## add lines of text to an Rmd
add_libraries <- function(fname, lib_names) {
  lines_to_add <- paste("library(", lib_names, ")", sep="")
  cat(lines_to_add, sep="\n", file=fname, append=TRUE)
  cat("\n", file=fname, append=TRUE)
}

## wrap chunks in trycatch expression
insert_trycatch <- function(in_fname, out_fname) {
  inlines <- readLines(in_fname)
  if(file.exists(out_fname)) 
    file.remove(out_fname)
  inside_trycatch <- FALSE
  for(ii in seq_along(inlines)) {
    ss <- inlines[ii]
    if(str_detect(ss, all_patterns$md$chunk.begin)){
      cat(ss, "\n", file=out_fname, append=TRUE)
      if(str_detect(inlines[ii+1], "tryCatch")){
        inside_trycatch <- TRUE
      } else {
        cat("tryCatch({ \n", file=out_fname, append=TRUE)
      }
    } else if(str_detect(ss, all_patterns$md$chunk.end)){
      if(inside_trycatch){
        inside_trycatch <- FALSE        
      } else {
        cat("}, error = function(e) e )\n", file=out_fname, append=TRUE)
      }
      cat(ss, "\n", file=out_fname, append=TRUE)
    } else{
      cat(ss, "\n", file=out_fname, append=TRUE)
    }
  }
  
}

extract_text <- function(rmd_name, pattern) {
  tmp <- extract_section_text(rmd_name, pattern)
  tmp <- str_trim(tmp)
  tmp <- tmp[tmp != ""]
  paste0(tmp[-1], collapse = "\n")
}
