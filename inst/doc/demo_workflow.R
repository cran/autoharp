## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(autoharp)

## ----ex01_files, echo=TRUE----------------------------------------------------
list.files(system.file("examples", "example-01", package="autoharp"))

## ----ex_fn, echo=FALSE--------------------------------------------------------
rf <- function(n) {
  U <- runif(n)
  X <- U^(1/4)
  X
}

## ----run_fn-------------------------------------------------------------------
set.seed(33)
rf(n = 5)

## ----motivating_example, eval=FALSE, echo=TRUE--------------------------------
# soln_fname <- system.file("examples", "example-01", "soln_template_01.Rmd",
# 			                    package="autoharp")
# temp_dir <- tempdir()
# s_env <- populate_soln_env(soln_fname, pattern = "test", knit_root_dir = temp_dir)
# stud_script_names <- list.files(system.file("examples", "example-01",
# 					                                  "student_scripts", package="autoharp"),
#                                 full.names = TRUE)
# 
# corr_out <- lapply(stud_script_names, render_one, out_dir = temp_dir,
#                    knit_root_dir = temp_dir, soln_stuff = s_env)
# 
# do.call("rbind", corr_out)

