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

# retrieve soln template path
soln_template_path <- system.file("examples", "soln_templates",  
                                  "soln_template_01.Rmd", package="autoharp")
# populate solution environment
s_env <- populate_soln_env(soln_template_path, pattern="test", getwd())

# retrieve installation-specific filenames of examples
stud_script_paths <- system.file("examples", "student_scripts", package="autoharp")
stud_script_names <- file.path(stud_script_paths, c("qn01_scr_01.R", "qn01_scr_02.R"))

# run autoharp function "render_one" on student scripts.
corr_out <- lapply(stud_script_names, render_one, out_dir = "test_out",   
                   knit_root_dir = getwd(), soln_stuff = s_env)

# combine output, dropping initial columns which pertain to runtime stats.
do.call("rbind", corr_out)[, -(1:5)]

## ----overview_fig, fig.align='center', out.width='80%', echo=FALSE, fig.cap='Overview'----
knitr::include_graphics("instructor_overview.png")

## ----testthat_example, eval=FALSE---------------------------------------------
#  test_that("check X properties", {
#    expect_true(exists("X"))
#    expect_equal(mean(X), mean(.X))
#  })

## ----ah01_example, eval=FALSE-------------------------------------------------
#  max_X <- max(X)
#  min_X <- min(X)

## ----ah02_example, eval=FALSE-------------------------------------------------
#  f1 <- rmd_to_forestharp(.myfilename)
#  mutate_count <- fapply(f1, count_fn_call, combine = TRUE, pattern="mutate")

## ----soln_template_fig, fig.align='center', out.width='80%', echo=FALSE, fig.cap='Solution template'----
knitr::include_graphics("soln_template_diagram.png")

## ----populate_example_00------------------------------------------------------
names(s_env)

## ----populate_example_01------------------------------------------------------
ls(s_env$env, all.names=TRUE)

## ----populate_example_02, echo=FALSE,comment=''-------------------------------
cat(readLines(s_env$test_fname), sep = '\n')

## ----instructor_workflow_fig, fig.align='center', out.width='80%', echo=FALSE, fig.cap='Instructor workflow'----
knitr::include_graphics("instructor_workflow_diagram.png")

## ----render_one_fig, fig.align='center', out.width='80%', echo=FALSE, fig.cap='Details of render_one'----
knitr::include_graphics("inside_render_one.png")

## ----pop_args-----------------------------------------------------------------
args(populate_soln_env)

## ----ro_args------------------------------------------------------------------
args(render_one)

## ----trailing_lines, echo=FALSE, comment=''-----------------------------------
test_lines <- readLines(s_env$test_fname)
cat(tail(test_lines, n=2), sep='\n')

## ----manual_run_01------------------------------------------------------------
.myenv <- new.env()
rmarkdown::render(stud_script_names[2], output_dir =getwd(), envir=.myenv)
.myenv$.myfilename <- normalizePath(stud_script_names[2])
check_correctness(.myenv, s_env$env, s_env$test_fname)

