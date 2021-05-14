## ----comment_style, include = FALSE-------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

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
        assign(".scalars_to_keep", 
               c(checkout, get(".scalars_to_keep", envir = envir)),
               envir = envir)
      } else {
        assign(".scalars_to_keep", checkout, envir = envir)
      }
    }
  }
)
knitr::knit_hooks$set(autoharp_hooks)

## ----obtain_sys_paths---------------------------------------------------------
system.file("examples", package="autoharp")

## ----list_example_dirs--------------------------------------------------------
list.dirs(system.file("examples", package="autoharp"))

## ----ex_fn, echo=FALSE--------------------------------------------------------
rf <- function(n) {
  U <- runif(n)
  X <- U^(1/4)
  X
}

## ----run_fn-------------------------------------------------------------------
set.seed(33)
rf(n = 5)

## ----motivating_example, message=FALSE----------------------------------------
library(autoharp)
knitr::opts_chunk$set(purl =FALSE)

# retrieve soln template path
soln_template_path <- system.file("examples", "soln_templates",  
                                  "soln_template_01.Rmd", package="autoharp")
# retrieve installation-specific filenames of examples
stud_script_paths <- system.file("examples", "student_scripts", package="autoharp")
stud_script_names <- file.path(stud_script_paths, c("qn01_scr_01.R", "qn01_scr_02.R"))

 if(rmarkdown::pandoc_available()) {
   # populate solution environment
   s_env <- populate_soln_env(soln_template_path, pattern="test", 
                              knit_root_dir = getwd(), output=tempfile())
   
   # run autoharp function "render_one" on student scripts.
   #corr_out <- lapply(stud_script_names, render_one, out_dir = "test_out",   
   corr_out <- lapply(stud_script_names, render_one, out_dir = tempdir(),
                      knit_root_dir = getwd(), soln_stuff = s_env)
   
   # combine output, dropping initial columns which pertain to runtime stats.
   do.call("rbind", corr_out)[, -(1:5)]
 }

