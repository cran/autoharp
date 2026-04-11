## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  eval      = FALSE,
  fig.align = "center"
)

## ----workflow-img, echo=FALSE, eval=TRUE, out.width="90%", fig.cap="The four-phase autoharp grading workflow: Prepare → Distribute → Grade → Review"----
knitr::include_graphics("figs/workflow.png")

## ----install, eval=FALSE------------------------------------------------------
# install.packages("autoharp")

## ----installdev---------------------------------------------------------------
# # install.packages("devtools")
# devtools::install_github("namanlab/autoharp")

## ----load---------------------------------------------------------------------
# library(autoharp)

## ----populate-----------------------------------------------------------------
# soln <- populate_soln_env("solution_template.Rmd", pattern = "test")
# 
# # soln is a list with two elements:
# # $env: the solution environment (contains .rf, .X and other solution objects)
# # $test_fname: path to the extracted test script (code from test chunks)
# str(soln)

## ----render-------------------------------------------------------------------
# result <- render_one(
#   rmd_name      = "student01.Rmd",
#   out_dir       = "output/",
#   knit_root_dir = getwd(),
#   soln_stuff    = soln
# )
# 
# # The result is a one-row data frame
# print(result)

## ----summarise----------------------------------------------------------------
# # Grade all students in a directory
# student_files <- list.files("submissions/", pattern = "\\.Rmd$", full.names = TRUE)
# 
# results_list <- lapply(student_files, function(f) {
#   render_one(rmd_name = f, out_dir = "output/", knit_root_dir = getwd(),
#              soln_stuff = soln)
# })
# 
# all_results <- do.call(rbind, results_list)
# 
# # Print a summary from the log file
# log_summary("output/render_one.log")

## ----lints--------------------------------------------------------------------
# # Count lint violations in a single script
# lint_count <- count_lints_one("student01.R")
# 
# # Count across all submissions
# all_lints <- count_lints_all(
#   file_names = list.files("submissions/", pattern = "\\.R$", full.names = TRUE)
# )
# print(all_lints)

## ----check-rmd----------------------------------------------------------------
# # Check that the submitted file is a valid Rmd
# # Returns TRUE if file has .Rmd extension, YAML header, and R chunks
# rmd_check <- check_rmd(fname = "student01.Rmd")
# print(rmd_check)

## ----grading-app, eval=FALSE--------------------------------------------------
# # Launch the full grading GUI
# shiny::runApp(system.file("shiny/grading_app", package = "autoharp"))

