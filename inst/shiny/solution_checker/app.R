library(shiny)
library(shinymanager)

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
  tabsetPanel(..., id = NULL, selected = NULL, type ="tabs")
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
      shiny::tags$small('autoharp solution checker, 2020.', br(),
                 'Found a bug? Report it',
                 #a(href='https://github.com/singator/autoharp/issues',
                 a(href='mailto:vik.gopal@nus.edu.sg','here.'))
      #style = "background: #ECF1F1;"
    )
    ),
    column(8, panelOutput)
  )
)

ui <- secure_app(ui, enable_admin = TRUE)

server <- function(input, output, session) {
  
  res_auth <- secure_server(
    check_credentials = check_credentials("st2137.sqlite", db_key)
  )
    
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
  # print(sess_tmp_dir)
  
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
        
        progress <- shiny::Progress$new(session, min=1, max=10)
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
              lib_error <- paste('Installation logs:', conditionMessage(try_install))
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
        
        progress$set(message="Rendering file", value=4)
        try_html <- tryCatch(render_one(file1$datapath, out_dir = sess_tmp_dir, 
                                        knit_root_dir = knit_wd, 
                                        max_time_per_run = max_time,
                                        soln_stuff = soln_out_all[[soln_to_use]]), 
                             error = function(e) return(e))
        progress$set(message="Almost done.. ", value=8)
        return(list(used=lib_used_msg, install=lib_to_install, error=lib_error, 
                    html=try_html))
      }
    })
  })
  
  output$html_check <- renderUI({
    if(is.null(htmlUI()$html)){
      return(p(h5(htmlUI()$used), h5(htmlUI()$install), h5(htmlUI()$error), hr()))
    } else {
  	if(!("data.frame" %in% class(htmlUI()$html))) {
        log_out <- log_summary(file.path(sess_tmp_dir, "render_one.log"))
  	  render_error <- paste('Render logs:', utils::tail(log_out$error_message, 1))
        return(p(h6(htmlUI()$used), h6(htmlUI()$install), h6(htmlUI()$error), 
                 hr(), h6(render_error)))
      }
      if(htmlUI()$html$run_status[1] == "FAIL") {
        log_out <- log_summary(file.path(sess_tmp_dir, "render_one.log"))
        render_error <- paste('Render logs:', utils::tail(log_out$error_message, 1))
        
        return(p(h6(htmlUI()$used), h6(htmlUI()$install), h6(htmlUI()$error), 
                 hr(), h6(render_error)))
      }
      addResourcePath("test", sess_tmp_dir)
      return(p(h6(htmlUI()$used), h6(htmlUI()$install), h6(htmlUI()$error), 
               hr(), 
               shiny::tags$iframe(src="test/0.html", height="1000", width="800", frameborder="0")))
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

# Create Shiny app ----
shinyApp(ui = ui, server = server)
