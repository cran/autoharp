## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----solution-checker-files, echo=TRUE----------------------------------------
list.files(system.file("shiny", "solution_checker", package="autoharp"))

## ----solution-checker-db-creation, eval=FALSE---------------------------------
# create_db(credentials_data = credentials,
#           sqlite_path = "st2137.sqlite",
#           passphrase = Sys.getenv("AUTOHARP_TUNER_DB_KEY")
#           )

## ----eval=FALSE---------------------------------------------------------------
# soln_templates_dir <- "/home/viknesh/NUS/coursesTaught/autoharp/mytesting/secure_tuner/soln_templates"
# knit_wd <- "/home/viknesh/NUS/coursesTaught/autoharp/mytesting/secure_tuner/"
# permission_to_install <- FALSE
# max_time <- 120
# 
# summary_header <- "# Summary Output"
# tabs <- c("lint", "html", "correctness")
# app_title <- "R Solution Checker"
# corr_cols_to_drop = c(1,2,4,5)
# db_key <- Sys.getenv("AUTOHARP_TUNER_DB_KEY")

