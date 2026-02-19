## LOAD LIBRARIES NEEDED HERE
library(autoharp)

## UTILITY FUNCTIONS
get_summary_output <- function (rmd_file, summary_header = "# Summary Output", 
                                dir = tempdir()){
  all_non_chunks <- extract_non_chunks(rmd_file)
  ind <- which(all_non_chunks == summary_header)
  if(length(ind) != 0) {
    display_chunks <- all_non_chunks[ind:length(all_non_chunks)]
    fp <- file.path(dir, 'summary_chunk_info.Rmd') 
    writeLines(display_chunks, con = fp)
    summary_df <- shiny::withMathJax(shiny::includeMarkdown(fp))
    unlink(fp)
  } else {
    summary_df <- h5(br(), "No summary found")
  }
  return(summary_df)
}

## INITIALISATION OF VARIABLES
soln_templates_dir <- "/home/viknesh/NUS/coursesTaught/autoharp/mytesting/secure_tuner/soln_templates"
knit_wd <- "/home/viknesh/NUS/coursesTaught/autoharp/mytesting/secure_tuner/"
tabs <- c("lint", "html", "correctness")
app_title <- "R Solution Checker"
permission_to_install <- FALSE
max_time <- 120
summary_header <- "# Summary Output"
corr_cols_to_drop = c(1,2,4,5)
db_key <- Sys.getenv("AUTOHARP_TUNER_DB_KEY")

soln_fnames <- list.files(soln_templates_dir, full.names = TRUE)
soln_choices <- sapply(soln_fnames, function(x) rmarkdown::yaml_front_matter(x)$title, 
                       USE.NAMES = FALSE)
if(anyDuplicated(soln_choices)){
  stop("Duplicate titles found in solution templates. Please resolve.")
}

### This is populating the soln env
soln_out_all <- lapply(soln_fnames, populate_soln_env, knit_root_dir = knit_wd)
names(soln_out_all) <- soln_choices
  
### This is populating the summary outputs 
chunk_out_all <- lapply(soln_fnames, get_summary_output)
names(chunk_out_all) <- soln_choices
  
  
### default list of lints
lint_list <- list(lintr::T_and_F_symbol_linter(),
                  lintr::assignment_linter(),
                  lintr::brace_linter(), 
                  lintr::commas_linter(), 
                  lintr::equals_na_linter(), 
                  lintr::function_left_parentheses_linter(), 
                  lintr::infix_spaces_linter(), 
                  lintr::line_length_linter(), 
                  lintr::whitespace_linter(),
                  lintr::absolute_path_linter(), 
                  lintr::pipe_continuation_linter(), 
                  lintr::spaces_inside_linter(), 
                  lintr::trailing_blank_lines_linter(), 
                  lintr::trailing_whitespace_linter(), 
                  lintr::unnecessary_concatenation_linter())