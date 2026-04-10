## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----similarity-app-run, eval=FALSE, echo=TRUE--------------------------------
# library(shiny)
# runApp(system.file(file.path('shiny', 'similarity_app', 'app.R'),
#                    package = 'autoharp'))

## ----similarity-app-files, echo=TRUE------------------------------------------
list.files(system.file("shiny", "similarity_app", package = "autoharp"))

