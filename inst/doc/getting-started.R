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

## ----define-rf, autoharp.objs = "rf"------------------------------------------
# # Reference solution: saved as .rf in the solution environment
# rf <- function(n) {
#   u <- runif(n)
#   u^(1/4)  # inverse CDF of f(x) = 4x^3
# }

## ----generate-X, autoharp.objs = "X"------------------------------------------
# set.seed(2022)
# X <- rf(10000)  # saved as .X

## ----tests, autoharp.scalars = "test_results"---------------------------------
# # Each line produces TRUE/FALSE: these become the student's test results
# length(formals(rf)) == 1          # rf has exactly 1 argument
# length(X) == 10000                # X has 10,000 elements
# abs(mean(X) - 0.8) < 0.02        # Mean close to 0.8 (theoretical: 0.8)
# abs(sd(X) - 0.1633) < 0.02       # SD close to 0.163 (theoretical)

## ----populate-----------------------------------------------------------------
# soln <- populate_soln_env("solution_template.Rmd")
# 
# # soln is a list with two elements:
# # $soln_env: the knitted solution environment (contains .rf, .X, .test_results)
# # $test_file: path to the generated test script
# str(soln)

## ----render-------------------------------------------------------------------
# result <- render_one(
#   rmd_name  = "student01.Rmd",
#   soln_env  = soln$soln_env,
#   test_file = soln$test_file,
#   out_dir   = "output/"
# )
# 
# # The result is a one-row data frame
# print(result)

## ----summarise----------------------------------------------------------------
# # Grade all students in a directory
# student_files <- list.files("submissions/", pattern = "\\.Rmd$", full.names = TRUE)
# 
# results_list <- lapply(student_files, function(f) {
#   render_one(f, soln_env = soln$soln_env, test_file = soln$test_file,
#              out_dir = "output/")
# })
# 
# all_results <- do.call(rbind, results_list)
# 
# # Print a summary table (pass rates, runtime distribution, etc.)
# log_summary(all_results)

## ----lints--------------------------------------------------------------------
# # Count lint violations in a single script
# lint_count <- count_lints_one("student01.R")
# 
# # Count across all submissions
# all_lints <- count_lints_all(
#   files = list.files("submissions/", pattern = "\\.R$", full.names = TRUE)
# )
# print(all_lints)

## ----check-rmd----------------------------------------------------------------
# # Check that the submitted Rmd has the required sections
# rmd_check <- check_rmd(
#   rmd_name          = "student01.Rmd",
#   expected_sections = c("Introduction", "Analysis", "Conclusion")
# )
# print(rmd_check)

## ----grading-app, eval=FALSE--------------------------------------------------
# # Launch the full grading GUI
# shiny::runApp(system.file("shiny/grading_app", package = "autoharp"))

