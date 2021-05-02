#' Runs the student-facing feedback app
#'
#' This function runs the shiny app that students submit to in order to
#' obtain feedback on their Rmd submission file.
#'
#' @param app_title A character string of the title of the app.
#' @param soln_templates_dir This should be the directory containing all
#' solution templates. Solution templates are Rmd files.
#' @param knit_wd The working directory for knitting (to HTML).
#' @param tabs A character vector of type of check to be done
#' @param lint_list A list of lints (from lintr package) to be run on the 
#' uploaded script. If missing, a default list of lints is run. See the 
#' details section.
#' @param corr_cols_to_drop This should be an integer vector of columns to drop 
#' from the correctness check. By default, the columns corresponding to filename,
#' timestamp, run-time timing and memory are dropped.
#' @param max_time The maximum time (in seconds) allocated to rendering before 
#' failing. This is passed on to \code{render_one}.
#' @param summary_header This the header to search for when generating the 
#' description for the correctness check. 
#' @param permission_to_install This is the argument to toggle for auto 
#' installation of libraries. Default is set to FALSE. 
#' @param ... Extra arguments passed on to runApp from shiny. Useful for
#' specifying port, etc.
#' 
#' @details If the \code{lint_list} argument is missing, the following list of 
#' lints is run:
#' \enumerate{
#'   \item T_and_F_symbol_linter,
#'   \item assignment_linter,
#'   \item closed_curly_linter,
#'   \item commas_linter,
#'   \item equals_na_linter,
#'   \item function_left_parentheses_linter,
#'   \item infix_spaces_linter,
#'   \item line_length_linter,
#'   \item no_tab_linter,
#'   \item open_curly_linter,
#'   \item paren_brace_linter,
#'   \item absolute_path_linter,
#'   \item pipe_continuation_linter,
#'   \item spaces_inside_linter,
#'   \item trailing_blank_lines_linter,
#'   \item trailing_whitespace_linter,
#'   \item unneeded_concatenation_linter
#' }
#' The full list of available lints can be found here: \code{\link[lintr]{linters}}.
#'
#' @import shiny
#'
#' @return This function is run for its side-effect.
#' @export
#'
#'
run_tuner <- function(app_title, soln_templates_dir, knit_wd, 
                       tabs = c("lint","html","correctness"), 
                       lint_list, corr_cols_to_drop = c(1,2,4,5),
                       max_time = 120, summary_header = "# Summary Output",
                      permission_to_install = FALSE,
                       ...) {
  
  # Reading in solution files
  soln_fnames <- list.files(soln_templates_dir, full.names = TRUE)
  soln_choices <- sapply(soln_fnames, function(x) rmarkdown::yaml_front_matter(x)$title,
                         USE.NAMES = FALSE)
  if(anyDuplicated(soln_choices)){
    stop("Duplicate titles found in solution templates. Please resolve.")
  }
  tabs <- match.arg(tabs,several.ok = TRUE)
  
  # sol_dir  <- tempfile("solutions")
  # Right now everything is in the main temp folder itself
  
  ## This is populating the soln env
  soln_out_all <- lapply(soln_fnames, populate_soln_env, knit_root_dir = knit_wd)
  names(soln_out_all) <- soln_choices
  
  ## This is populating the summary outputs 
  chunk_out_all <- lapply(soln_fnames, get_summary_output)
  names(chunk_out_all) <- soln_choices
  
  
  # default list of lints
  if(missing(lint_list)) {
    lint_list <- c(lintr::T_and_F_symbol_linter,
                   lintr::assignment_linter,
                   lintr::closed_curly_linter,
                   lintr::commas_linter,
                   lintr::equals_na_linter,
                   lintr::function_left_parentheses_linter,
                   lintr::infix_spaces_linter,
                   lintr::line_length_linter,
                   lintr::no_tab_linter,
                   lintr::open_curly_linter,
                   lintr::paren_brace_linter,
                   lintr::absolute_path_linter,
                   lintr::pipe_continuation_linter,
                   lintr::spaces_inside_linter,
                   lintr::trailing_blank_lines_linter,
                   lintr::trailing_whitespace_linter,
                   lintr::unneeded_concatenation_linter)
  }
  
  #For future update of tabs need to change from here
  full_tabs = c("lint","html","correctness")
  lint_tabs <- tabPanel("Lint Check",uiOutput(outputId = "lint_check"))
  html_tabs <- tabPanel("HTML Check",uiOutput(outputId = "html_check"))
  correctness_tabs <- tabPanel("Correctness check",tableOutput(outputId = "corr_check"))
  list1 <- list(lint_tabs,html_tabs,correctness_tabs)
  #Change up to here by adding new tabs to the list
  
  index_match = match(tabs , full_tabs)
  list_of_tabs <- list1[index_match]
  internalwrapper <- function(...){
    tabsetPanel(..., id = NULL, selected = NULL, type ="tabs",
                position = NULL)
  }
  panelOutput <- do.call("internalwrapper",list_of_tabs)
  
  
  ui <- fluidPage(
    titlePanel(title = app_title),
    
    fluidRow(
      column(4, wellPanel(
        selectInput(
          inputId = "selectfileweek",
          label = h4("Select tutorial number:"),
          choices = soln_choices
        ),
        fileInput(
          inputId = "fileupload",
          label = h4("Upload your solution:"),
          #accept = c(".Rmd", ".rmd", ".R", ".r"),
          width = '100%',
          multiple = FALSE
        ),
        actionButton("goButton", "Check solution!"),
        hr(),
        tags$small('autoharp solution checker, 2020.', br(),
                   'Found a bug? Report it',
                   #a(href='https://github.com/singator/autoharp/issues',
                   a(href='mailto:vik.gopal@nus.edu.sg','here.'))
        #style = "background: #ECF1F1;"
      )
      ),
      column(8, panelOutput)
    )
  )
  
  ## Define the server function
  server <- function(input, output, session) {
    lint_df <- reactive({
      input$goButton
      isolate({
        if(is.null(input$fileupload)){
          return(NULL)
        } else {
          file1 <- input$fileupload
          all_lints <- lintr::lint(file1$datapath, linters= lint_list)
          if(length(all_lints) > 0) {
            ll <- sapply(all_lints, function(x) x$line_number)
            mm <- sapply(all_lints, function(x) x$message)
            l_df <- data.frame(ll, mm, stringsAsFactors = FALSE)
            colnames(l_df) <- c("Line", "Message")
            l_df2 <- renderTable(l_df)
          } else {
            l_df2 <- h5(br(), "No lints found. Nice job!")
          }
        }
      })
      l_df2
    })
    
    sess_tmp_dir  <- tempfile("rmd_out")
    dir.create(sess_tmp_dir)
    
    session$onSessionEnded(function() {
      unlink(sess_tmp_dir, TRUE)
    })
    
    output$lint_check <- renderUI({
      fluidPage(lint_df())
    })
    
    htmlUI <- reactive({
      input$goButton
      isolate({
        if(is.null(input$fileupload)) {
          return(NULL)
        } else {
          file1 <- input$fileupload
          #unlink(sess_tmp_dir, recursive = TRUE)
          
          progress <- Progress$new(session, min=1, max=10)
          on.exit(progress$close())
          progress$set(message="Checking libraries")
          
          lib_used <- get_libraries(file1$datapath)
          lib_used_msg <- paste('Libraries used: ',
                                paste0(lib_used, collapse=", "))
          lib_to_install <- 'Libraries to install: None'
          lib_error <- 'Installation logs: --'
          if(!is.null(lib_used)) {
            
            id <- !(sapply(lib_used, quietly=TRUE, requireNamespace))
            lib_needed <- lib_used[id]
            
            if(length(lib_needed) > 0) {
              
              if(!permission_to_install) {
                lib_error <- paste('Installation logs: Do Not Install')
                lib_to_install <- paste('Need to install:',
                                        paste0(lib_needed, collapse=", "))
                return(list(used=lib_used_msg, install=lib_to_install, 
                            error=lib_error, html=NULL, correctness=NULL))
              }
              
              lib_to_install <- paste('Need to install:',
                                      paste0(lib_needed, collapse=", "))
              showNotification("Installing packages, please wait a while..")
              progress$set(message="Installing libraries", value=2)
              
              try_install <- tryCatch(utils::install.packages(lib_needed,
                                                              dependencies = TRUE),
                                      error = function(e) return(e))
              #warning = function(w) return(w))
              #if("warning" %in% class(try_install) ||
              if("error" %in% class(try_install)) {
                lib_error <- paste('Installation logs:', try_install$message)
                return(list(used=lib_used_msg, install=lib_to_install, error=lib_error,
                            html=NULL, correctness=NULL))
                #return(p(h5(lib_used_msg), h5(lib_to_install),
                #         h5(lib_error), hr()))
              } else {
                lib_error <- paste('Installation logs: All OK!')
              }
            }
          }
          soln_to_use <- input$selectfileweek
          
          #soln_to_use <- soln_fnames[which(soln_choices == input$selectfileweek)]
          
          # make sure "test" is used to label test chunks.
          #soln_out <- populate_soln_env(soln_to_use, knit_root_dir = knit_wd)
          # Move things to session temp directory.
          #old_name <- soln_out$test_fname
          #file.rename(old_name, file.path(sess_tmp_dir, basename(old_name)))
          #soln_out$test_fname <- file.path(sess_tmp_dir, basename(old_name))
          # print(soln_out)
          
          #soln_env <- soln_out$env
          #test_fname <- soln_out$test_fname
          #stud_env <- new.env()
          
          
          progress$set(message="Rendering file", value=4)
          try_html <- tryCatch(render_one(file1$datapath, out_dir = sess_tmp_dir, 
                                          knit_root_dir = knit_wd, 
                                          max_time_per_run = max_time,
                                          soln_stuff = soln_out_all[[soln_to_use]]), 
                               error = function(e) return(e))
          #print(try_html)
          #	  try_html <- tryCatch(rmarkdown::render(file1$datapath,
          #	                                       output_format = 'html_document',
          #					       output_dir =  sess_tmp_dir, clean=FALSE,
          #					       intermediates_dir = sess_tmp_dir,
          #                                               envir = stud_env,
          #					       knit_root_dir = knit_wd),
          #                               error = function(e) return(e))
          #	  correctness_out <- check_correctness(stud_env, soln_env, test_fname)
          progress$set(message="Almost done.. ", value=8)
          return(list(used=lib_used_msg, install=lib_to_install, error=lib_error, 
                      html=try_html))
        }
      })
    })
    
    output$html_check <- renderUI({
      # fluidPage(lint_df())
      # fluidPage(htmlUI())
      if(is.null(htmlUI()$html)){
        return(p(h5(htmlUI()$used), h5(htmlUI()$install), h5(htmlUI()$error), hr()))
        #return(p(h5(lib_used_msg), h5(lib_to_install), h5(lib_error), hr()))
      } else {
    	if(!("data.frame" %in% class(htmlUI()$html))) {
          log_out <- log_summary(file.path(sess_tmp_dir, "render_one.log"))
    	  render_error <- paste('Render logs:', utils::tail(log_out$error_message, 1))
    	  #render_error <- paste('Render logs:', htmlUI()$html$message)
    	  #return(p(h6(lib_used_msg), h6(lib_to_install),
    	  #	   h6(lib_error), hr(), h6(render_error)))
          return(p(h6(htmlUI()$used), h6(htmlUI()$install), h6(htmlUI()$error), 
                   hr(), h6(render_error)))
        }
        if(htmlUI()$html$run_status[1] == "FAIL") {
          log_out <- log_summary(file.path(sess_tmp_dir, "render_one.log"))
          render_error <- paste('Render logs:', utils::tail(log_out$error_message, 1))
          #print(render_error)
          
          return(p(h6(htmlUI()$used), h6(htmlUI()$install), h6(htmlUI()$error), 
                   hr(), h6(render_error)))
        }
        addResourcePath("test", sess_tmp_dir)
        return(p(h6(htmlUI()$used), h6(htmlUI()$install), h6(htmlUI()$error), 
                 hr(), 
                 tags$iframe(src="test/0.html", height="1000", width="800", frameborder="0")))
      }
    })
    
    
    summary_df <- reactive({
      input$goButton
      isolate({
        if(is.null(input$fileupload)){ # see if can change to the file select instead
          return(NULL)
        } else {
          soln_to_use <- input$selectfileweek
          summary_df2 <- chunk_out_all[[soln_to_use]]
        }
      })
      summary_df2
    })
    
    
    output$corr_check <- renderUI({
      fluidPage(
        fluidRow(
          renderTable({
            if(is.null(htmlUI())){
              return(NULL)
            }
            if("data.frame" %in% class(htmlUI()$html)){
              if(ncol(htmlUI()$html) == 3) {
                return( htmlUI()$html[,3])
              } else {
                return(htmlUI()$html[,-corr_cols_to_drop])
              }
            }
            #return(NULL)
          })
        ),
        fluidRow(
          fluidPage(summary_df())
        )
      )
      
    })
    
  }
  
  ## Create the app object
  app <- shinyApp(ui, server)
  
  app
  ## Run the app
  #runApp(app, ...)
}

#' Function to extract the summary content 
#' 
#' This function will look for the explanation for the checks being done. 
#' If there is an explanation, the function will return the summary in HTML 
#' format. If not it will return 'not found' in HTML format. 
#' 
#' @param rmd_file The path to the rmd file to search for the summary. 
#' @param summary_header The header to look for. 
#' @param dir A temporary directory to store the temporary Rmarkdown file
#'        before extracting the html content. The temp file will be deleted
#'        before the function exits.
#' 
#' @return The function is used as a helper function. Returns the HTML 
#' formatted string. 
#' 
get_summary_output <- function (rmd_file, summary_header = "# Summary Output", 
                                dir = tempdir()){
  all_non_chunks <- extract_non_chunks(rmd_file)
  ind <- which(all_non_chunks == summary_header)
  if(length(ind) != 0) {
    display_chunks <- all_non_chunks[ind:length(all_non_chunks)]
    fp <- file.path(dir, 'summary_chunk_info.Rmd') 
    writeLines(display_chunks, con = fp)
    summary_df <-withMathJax(includeMarkdown(fp))
    unlink(fp)
  } else {
    summary_df <- h5(br(), "No summary found")
  }
  return(summary_df)
}
