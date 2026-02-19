## Database creation and access
##
## Use this script to create an SQLite database. For the db passphrase, use the 
## environment variable that appears in R/globals.R.
## 
## The environment variable should be stored in .Renviron
## Currently, the name used is AUTOHARP_TUNER_DB_KEY
library(shinymanager)

credentials <- data.frame(
  user = c("st2137-user", "st2137-admin"),
  password = c("***", "***"), # please set your own password here!!!
  admin = c(FALSE, TRUE),
  stringsAsFactors = FALSE
)
create_db(credentials_data = credentials,
          sqlite_path = "st2137.sqlite",
          passphrase = Sys.getenv("AUTOHARP_TUNER_DB_KEY")
)
