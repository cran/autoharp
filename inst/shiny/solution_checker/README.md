# Introduction

The `solution_checker` shiny application is a website for students to double-check
their assignment *before* actual grading. From experience, especially in the
initial portion of a course, many student mistakes are due to incorrect path
settings, or not coding with the intent of making it easier for someone else to
run their code.

The solution checker allows students to upload code to a server. The instructor 
may want to prepare a stripped-down solution template to make basic checks, e.g.
whether the script can be knitted and rendered, existence of particular functions
for the particular assignment, etc. 

## File Structure Overview

* `data/` folder can be used to store datasets that the script will use.
* `R/` contains scripts that will be executed by R before the shiny application
  begins. At present, there is only one script: `globals.R`. It can be used to 
  customise options for the shiny application.
* `soln_templates/` contains all the solution templates that the server will use.
* `app.R` contains the UI and server logic.
* `db_creation.R` is a helper script to guide the instructor to setting up an
  SQLite database.
  * Remember to set the value for AUTOHARP_TUNER_DB_KEY in .Renviron,
    which should be stored in ~/.Renviron
  * To generate one, you can use: `openssl rand -hex 16`
  
## Global Variables

The following variables are initialised in `globals.R`:

1.  `soln_templates_dir`: Full path to solution templates that this application will use.
2.  `knit_wd`: The working directory for knitting.
3.  `tabs`: The tabs to show. By default, the checker does three things:
    1. Lint the file.
    2. Render it.
    3. Display a summary of checks conducted.
4.  `app_title`: The title to display on the page.
5.  `permission_to_install`: Whether the application should have permission to install
    new packages. You are recommended to set this as `FALSE`.
6.  `max_time`: The maximum time in seconds before the applications gives up 
    running a script. The purpose of this is to avoid hogging the CPU and resulting 
    in a DoS to others.
7.  `summary_header`: The summary header to look for in the solution template. The 
    text in this chunk will be displayed on the correctness checks tab. 
8.  `corr_cols_to_drop`: The correctness output contains several columns that are 
    probably not important to the student. Examples are running time, memory used, 
    and so on. These columns can be dropped for purpose of these basic, student-facing 
    checks.
    
# Deploying on Shiny Server

## Securing the UI

Before deployment, use `db_creation.R` to create an SQLite database with
usernames and passwords for students, and an admin.

The key for the database can be stored as an environment variable in `.Renviron`
(see `R/globals.R` for how it is read in and used).

If you need to generate a secure hex string, you can use this command:

```
openssl rand -hex 16
```

## Shiny Server

It is also recommended to have a web proxy in front of your shiny server, and to
run applications as a non-sudo user.
