library(shiny)
library(DT)
library(base64enc)
library(rmarkdown)
library(autoharp)
library(shinyFiles)

# Check if shinyFiles is available
# if (!requireNamespace("shinyFiles", quietly = TRUE)) {
#   message("Note: shinyFiles package not available. Folder selection buttons will not be functional.")
#   SHINYFILES_AVAILABLE <- FALSE
# } else {
#   library(shinyFiles)
#   SHINYFILES_AVAILABLE <- TRUE
# }

SHINYFILES_AVAILABLE <- TRUE
source(file.path("R", "helpers.R"))

# Helper function to check if a path is absolute
# Returns TRUE if the path starts with:
#   - Windows drive letter: C:, D:, etc.
#   - Unix absolute path: /
#   - UNC path: \\server or //server
is_absolute_path <- function(path) {
  # Pattern breakdown:
  # [A-Za-z]: - Windows drive letter (C:, D:, etc.)
  # / - Unix absolute path starting with /
  # \\\\\\\\ - UNC path starting with \\ (escaped for R)
  # // - UNC path starting with // (forward slash variant)
  grepl("^([A-Za-z]:|/|\\\\\\\\|//)", path)
}

# Helper function to normalize a path based on whether it exists
normalize_path_safe <- function(path) {
  if (dir.exists(path)) {
    normalizePath(path, winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }
}


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      // Function to open plot modal from button with data attributes
      function openPlotModalFromButton(button) {
        var imgSrc = button.getAttribute('data-img-src');
        var plotTitle = button.getAttribute('data-plot-title');
        var modal = document.getElementById('plotModal');
        var modalImg = document.getElementById('modalPlotImg');
        var modalTitle = document.getElementById('modalPlotTitle');
        modalImg.src = imgSrc;
        modalTitle.textContent = plotTitle;
        modal.classList.add('active');
      }
      
      // Function to close plot modal
      function closePlotModal() {
        var modal = document.getElementById('plotModal');
        modal.classList.remove('active');
      }
      
      // Move the modal to document.body on load so it escapes any
      // Shiny/Bootstrap stacking contexts that clip fixed elements
      document.addEventListener('DOMContentLoaded', function() {
        var modal = document.getElementById('plotModal');
        if (modal && modal.parentNode !== document.body) {
          document.body.appendChild(modal);
        }
        if (modal) {
          modal.addEventListener('click', function(e) {
            if (e.target === modal) {
              closePlotModal();
            }
          });
        }
      });
      
      // Close modal with Escape key
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') {
          closePlotModal();
        }
      });
    "))
  ),
  div(class = "main-header",
      h2("Autoharp Instructor Grading App"),
      p("Automated grading of R/Rmd/QMD student submissions")
  ),
  
  # Main content with tabs
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",
    
    # Tab 0: Session Management
    tabPanel(
      "Session",
      value = "session",
      
      br(),
      
      fluidRow(
        column(
          width = 6,
          div(class = "panel-section",
              h4(icon("save"), " Load Previous Session"),
              p("Resume a previous grading session by uploading a session file."),
              
              fileInput(
                inputId = "session_file",
                label = "Upload Session File (.rds):",
                accept = c(".rds"),
                width = "100%"
              ),
              
              uiOutput("session_status"),
              
              actionButton("load_session", "Load Session", 
                           class = "btn-primary", 
                           icon = icon("upload"))
          )
        ),
        
        column(
          width = 6,
          div(class = "panel-section",
              h4(icon("info-circle"), " Current Session Info"),
              
              uiOutput("current_session_info"),
              
              br(),
              
              actionButton("save_session_manual", "Save Session Now", 
                           class = "btn-success", 
                           icon = icon("save")),
              
              p(class = "text-muted small", style = "margin-top: 10px;",
                "Sessions are auto-saved after grading operations. Use this to save manually.")
          )
        )
      )
    ),
    
    # Tab 1: Object Testing
    tabPanel(
      "Object Testing",
      value = "testing",
      
      br(),
      
      fluidRow(
        # Left sidebar - Setup
        column(
          width = 3,
          div(class = "sidebar-panel",
              h4(icon("cog"), " Setup"),
              
              fileInput(
                inputId = "solution_file",
                label = "Upload Solution Template (RMD/QMD):",
                accept = c(".Rmd", ".rmd", ".qmd", ".Qmd"),
                width = "100%"
              ),
              
              textInput(
                inputId = "student_folder",
                label = "Path to Student Scripts Folder:",
                placeholder = "../comprehensive_tests/soln_dir",
                width = "100%"
              ),
              
              # Add folder selection button if shinyFiles is available
              if (SHINYFILES_AVAILABLE) {
                shinyFiles::shinyDirButton("student_folder_select", "Browse...", 
                                           "Select student scripts folder",
                                           class = "btn-secondary",
                                           style = "width: 100%; margin-top: -10px;")
              } else {
                NULL
              },
              
              textInput(
                inputId = "output_folder",
                label = "Output Directory:",
                value = "grading_output",
                placeholder = "grading_output",
                width = "100%"
              ),
              
              p(class = "text-muted small", 
                "Directory for plots, results, and session files."),
              
              div(class = "divider"),
              
              h5(icon("list-check"), " Objects to Test"),
              div(class = "object-checkbox-group",
                  uiOutput("object_selector")
              ),
              
              div(class = "divider"),
              
              actionButton("run_grading", "Run Automated Grading", 
                           class = "btn-primary", 
                           style = "width: 100%;",
                           icon = icon("play")),
              
              br(), br(),
              
              downloadButton("download_csv", "Download Results as CSV",
                             class = "btn-success",
                             style = "width: 100%")
          )
        ),
        
        # Main content area
        column(
          width = 9,
          
          # Status messages
          uiOutput("grading_status"),
          
          # Summary Results
          div(class = "panel-section",
              h4(icon("chart-bar"), " Summary Results"),
              p("Summary of test results by student and object."),
              DTOutput("grading_results")
          ),
          
          div(class = "panel-section",
              h4(icon("list-alt"), " Detailed Results"),
              p("Detailed view of failed tests."),
              DTOutput("detailed_results")
          )
        )
      )
    ),
    
    # Tab 2: Template Download
    tabPanel(
      "Template Download",
      value = "download",
      
      br(),
      
      fluidRow(
        column(
          width = 3,
          div(class = "sidebar-panel",
              h4(icon("info-circle"), " Instructions"),
              
              p("This tab generates an autoharp-compatible solution template with test chunks."),
              
              tags$ol(
                tags$li("Upload a solution RMD file in the 'Object Testing' tab"),
                tags$li("Select which objects to include in tests"),
                tags$li("Click 'Generate Template' to create the autoharp version"),
                tags$li("Download the generated template")
              ),
              
              div(class = "divider"),
              
              h5(icon("cogs"), " Options"),
              
              textInput(
                inputId = "test_prefix",
                label = "Test chunk prefix:",
                value = "test",
                width = "100%"
              ),
              
              checkboxInput(
                inputId = "include_all_tests",
                label = "Include all comprehensive tests",
                value = TRUE
              ),
              
              div(class = "divider"),
              
              actionButton("generate_template", "Generate Template",
                           class = "btn-primary",
                           style = "width: 100%;",
                           icon = icon("magic")),
              
              br(), br(),
              
              downloadButton("download_template", "Download Template",
                             class = "btn-success",
                             style = "width: 100%")
          )
        ),
        
        # Main content - Preview
        column(
          width = 9,
          
          div(class = "panel-section",
              h4(icon("file-code"), " Generated Template Preview"),
              p("Preview of the autoharp-compatible solution template with test chunks."),
              
              uiOutput("template_status"),
              
              div(
                style = "background: #1e1e1e; color: #d4d4d4; padding: 20px; border-radius: 8px; font-family: 'Consolas', 'Monaco', monospace; font-size: 13px; max-height: 600px; overflow-y: auto; white-space: pre-wrap;",
                verbatimTextOutput("template_preview")
              )
          ),
          
          # tests documentation
          div(class = "panel-section",
              h4(icon("book"), " Generated Tests Documentation"),
              p("List of all tests that will be generated for each object type."),
              uiOutput("tests_documentation")
          )
        )
      )
    ),
    
    # Tab 3: Autoharp Grading
    tabPanel(
      "Autoharp Grading",
      value = "autoharp",
      
      br(),
      
      fluidRow(
        column(
          width = 3,
          div(class = "sidebar-panel",
              h4(icon("cogs"), " Autoharp Setup"),
              
              p("Use the autoharp render_one function to grade student scripts against a template."),
              
              div(class = "divider"),
              
              h5(icon("cog"), " Configuration"),
              
              # Template file upload
              fileInput(
                inputId = "autoharp_template_file",
                label = "Upload Template File (RMD/QMD):",
                accept = c(".Rmd", ".rmd", ".qmd", ".Qmd"),
                width = "100%"
              ),
              
              textInput(
                inputId = "autoharp_student_folder",
                label = "Path to Student Scripts Folder:",
                value = "sol",
                placeholder = "sol",
                width = "100%"
              ),
              
              # Add folder selection button if shinyFiles is available
              if (SHINYFILES_AVAILABLE) {
                shinyFiles::shinyDirButton("autoharp_student_folder_select", "Browse...", 
                                           "Select student scripts folder",
                                           class = "btn-secondary",
                                           style = "width: 100%; margin-top: -10px;")
              } else {
                NULL
              },
              
              textInput(
                inputId = "autoharp_output_folder",
                label = "Output Directory:",
                value = "student_out",
                placeholder = "student_out",
                width = "100%"
              ),
              
              p(class = "text-muted small", 
                "Directory for render_one output and session files."),
              
              # test pattern input
              textInput(
                inputId = "test_pattern",
                label = "Test Chunk Pattern:",
                value = "test",
                placeholder = "test",
                width = "100%"
              ),
              
              p(class = "text-muted small", 
                "Pattern to identify test chunks in template (default: 'test')."),
              
              # Checkbox for permission to install packages
              checkboxInput(
                inputId = "permission_to_install",
                label = "Allow package installation during grading",
                value = FALSE,
                width = "100%"
              ),
              
              p(class = "text-muted small", 
                "If checked, render_one will install missing packages automatically."),
              
              div(class = "divider"),
              
              actionButton("run_autoharp_grading", "Run Autoharp Grading", 
                           class = "btn-primary", 
                           style = "width: 100%;",
                           icon = icon("play")),
              
              br(), br(),
              
              downloadButton("download_autoharp_excel", "Download Results as Excel",
                             class = "btn-success",
                             style = "width: 100%")
          )
        ),
        
        # Main content area
        column(
          width = 9,
          
          # Status messages
          uiOutput("autoharp_status"),
          
          # Comprehensive Autoharp Results with Plot Grades
          div(class = "panel-section",
              h4(icon("table"), " Comprehensive Grading Results"),
              p("Combined results from autoharp render_one and manual plot grades."),
              DTOutput("autoharp_results")
          )
        )
      )
    ),
    
    # Tab 4: Plot Grading
    tabPanel(
      "Plot Grading",
      value = "plots",
      
      br(),
      
      fluidRow(
        column(
          width = 3,
          div(class = "sidebar-panel",
              h4(icon("user"), " Student Selection"),
              
              uiOutput("student_selector"),
              
              br(),
              
              fluidRow(
                column(6, actionButton("prev_student", icon("arrow-left"), 
                                       class = "btn-secondary",
                                       style = "width: 100%;")),
                column(6, actionButton("next_student", icon("arrow-right"), 
                                       class = "btn-secondary",
                                       style = "width: 100%;"))
              ),
              
              div(class = "divider"),
              
              h5(icon("star"), " Grading"),
              
              # Grade input
              numericInput("plot_grade", "Grade (0-100):", 
                           value = 0, min = 0, max = 100,
                           width = "100%"),
              
              textAreaInput("plot_comments", "Comments:", 
                            rows = 3, 
                            placeholder = "Optional feedback...",
                            width = "100%"),
              
              actionButton("save_grade", "Save Grade", 
                           class = "btn-success",
                           style = "width: 100%;",
                           icon = icon("save")),
              
              div(class = "divider"),
              
              h5(icon("tasks"), " Progress"),
              uiOutput("grading_progress")
          )
        ),
        
        # Main content area
        column(
          width = 9,
          
          # Student Plots
          div(class = "panel-section",
              h4(icon("image"), " Student Plots"),
              uiOutput("plot_display")
          ),
          
          # Test Results for current student
          div(class = "panel-section",
              h4(icon("clipboard-check"), " Test Results for Selected Student"),
              p("View all test results (built-in object checks) for the currently selected student."),
              DTOutput("student_test_results")
          ),
          
          # Plot grades summary
          div(class = "panel-section",
              h4(icon("table"), " Plot Grades Summary"),
              DTOutput("plot_grades_table")
          )
        )
      )
    )
  ),
  
  # Plot Modal - Global level to ensure proper z-index stacking
  tags$div(
    id = "plotModal",
    class = "plot-modal",
    tags$div(
      class = "plot-modal-content",
      tags$button(
        class = "plot-modal-close",
        onclick = "closePlotModal()",
        HTML("&times;")
      ),
      tags$h4(id = "modalPlotTitle", style = "margin-bottom: 15px; color: #495057;"),
      tags$img(id = "modalPlotImg", src = "", alt = "Expanded Plot")
    )
  )
)

# server def
server <- function(input, output, session) {
  
  # Constants for knitr temp directories and default figure path.
  # These are used when knitting solution files to avoid creating stray files
  # in the app's working directory.
  KNITR_DEFAULT_FIG_PATH <- "figure/"
  KNITR_SOLN_FIG_DIR     <- file.path(tempdir(), "soln_figs")
  KNITR_SOLN_FIG_RESTORE <- file.path(tempdir(), "soln_figs_restore")
  
  # reactive values to store state
  rv <- reactiveValues(
    solution_env = NULL,
    solution_objects = NULL,
    solution_file_content = NULL,
    solution_file_extension = NULL,  # store original file extension
    student_files = NULL,
    grading_results = NULL,
    plot_grades = NULL,
    current_student_index = 1,
    student_plots = list(),
    processing = FALSE,
    generated_template = NULL,
    # autoharp grading state
    autoharp_template_path = NULL,
    autoharp_student_files = NULL,
    autoharp_results = NULL,
    autoharp_processing = FALSE,
    # folder synchronization flag to prevent circular updates
    sync_in_progress = FALSE,
    # session management
    session_id = NULL,
    session_loaded = FALSE,
    restoring_session = FALSE,
    advancing_student = FALSE,
    last_save_time = NULL
  )
  
  # helper function to save current student's grade/comments into rv$plot_grades.
  save_current_grade <- function() {
    # skip during session restoration or auto-advance: inputs have stale values
    if (isTRUE(rv$restoring_session) || isTRUE(rv$advancing_student)) {
      return(invisible())
    }
    if (!is.null(rv$plot_grades) && !is.null(rv$current_student_index)) {
      if (rv$current_student_index > 0 && rv$current_student_index <= nrow(rv$plot_grades)) {
        grade_val <- input$plot_grade
        comment_val <- input$plot_comments
        rv$plot_grades$grade[rv$current_student_index] <- grade_val
        rv$plot_grades$comments[rv$current_student_index] <- if (is.null(comment_val)) "" else comment_val
        # Handle NA values to prevent "missing value where TRUE/FALSE needed"
        has_grade <- !is.null(grade_val) && !is.na(grade_val) && grade_val > 0
        has_comments <- !is.null(comment_val) && nchar(trimws(comment_val)) > 0
        if (has_grade || has_comments) {
          rv$plot_grades$graded[rv$current_student_index] <- TRUE
        }
      }
    }
  }
  
  # helper function to save session state.
  # When out_dir is provided (e.g. from autoharp grading), sessions are saved
  # there.  Otherwise falls back to the Object Testing output folder so there
  # is a consistent default location.
  save_session_state <- function(out_dir = NULL) {
    tryCatch({
      # Use the explicitly provided out_dir first, then the Object Testing
      # output folder, then the hard-coded default.
      output_dir <- if (!is.null(out_dir) && !is.na(out_dir) && out_dir != "") {
        out_dir
      } else if (!is.null(input$output_folder) && !is.na(input$output_folder) && input$output_folder != "") {
        input$output_folder
      } else {
        "grading_output"
      }
      
      # Create output directory if it doesn't exist
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      # Create session subdirectory
      session_dir <- file.path(output_dir, "sessions")
      if (!dir.exists(session_dir)) {
        dir.create(session_dir, recursive = TRUE)
      }
      
      # Generate session ID if not exists
      if (is.null(rv$session_id)) {
        rv$session_id <- format(Sys.time(), "session_%Y%m%d_%H%M%S")
      }
      
      # Prepare session data
      session_data <- list(
        session_id = rv$session_id,
        timestamp = Sys.time(),
        solution_objects = rv$solution_objects,
        solution_file_content = rv$solution_file_content,
        solution_file_extension = rv$solution_file_extension,
        student_files = rv$student_files,
        grading_results = rv$grading_results,
        plot_grades = rv$plot_grades,
        current_student_index = rv$current_student_index,
        student_plots = rv$student_plots,
        generated_template = rv$generated_template,
        autoharp_template_path = rv$autoharp_template_path,
        autoharp_student_files = rv$autoharp_student_files,
        autoharp_results = rv$autoharp_results,
        # Save input values
        student_folder = input$student_folder,
        output_folder = output_dir,
        autoharp_student_folder = input$autoharp_student_folder,
        autoharp_output_folder = input$autoharp_output_folder,
        test_pattern = input$test_pattern
      )
      
      # Save to file
      session_file <- file.path(session_dir, paste0(rv$session_id, ".rds"))
      saveRDS(session_data, session_file)
      rv$last_save_time <- Sys.time()
      
      return(list(success = TRUE, file = session_file))
    }, error = function(e) {
      return(list(success = FALSE, error = e$message))
    })
  }
  
  # Helper function to load session state
  load_session_state <- function(session_file) {
    tryCatch({
      session_data <- readRDS(session_file)
      
      # Guard: prevent save_current_grade and student_folder observer from
      # clobbering restored data while we are restoring reactive values
      rv$restoring_session <- TRUE
      rv$session_loaded <- TRUE
      
      # Restore reactive values
      rv$session_id <- session_data$session_id
      rv$solution_objects <- session_data$solution_objects
      rv$solution_file_content <- session_data$solution_file_content
      rv$solution_file_extension <- session_data$solution_file_extension
      rv$student_files <- session_data$student_files
      rv$grading_results <- session_data$grading_results
      rv$plot_grades <- session_data$plot_grades
      rv$current_student_index <- session_data$current_student_index
      rv$student_plots <- session_data$student_plots
      rv$generated_template <- session_data$generated_template
      rv$autoharp_template_path <- session_data$autoharp_template_path
      rv$autoharp_student_files <- session_data$autoharp_student_files
      rv$autoharp_results <- session_data$autoharp_results
      
      # Restore input values (these trigger observers asynchronously)
      updateTextInput(session, "student_folder", value = session_data$student_folder)
      updateTextInput(session, "output_folder", value = session_data$output_folder)
      updateTextInput(session, "autoharp_student_folder", value = session_data$autoharp_student_folder)
      updateTextInput(session, "autoharp_output_folder", value = session_data$autoharp_output_folder)
      updateTextInput(session, "test_pattern", value = session_data$test_pattern)
      
      # Restore the grade/comment inputs for the current student
      if (!is.null(session_data$plot_grades) && !is.null(session_data$current_student_index)) {
        idx <- session_data$current_student_index
        if (idx > 0 && idx <= nrow(session_data$plot_grades)) {
          saved_grade <- session_data$plot_grades$grade[idx]
          saved_comment <- session_data$plot_grades$comments[idx]
          updateNumericInput(session, "plot_grade",
                             value = if (is.na(saved_grade)) 0 else saved_grade)
          updateTextAreaInput(session, "plot_comments",
                              value = if (is.null(saved_comment)) "" else saved_comment)
        }
      }
      
      # Restore solution environment if solution was loaded
      if (!is.null(rv$solution_file_content)) {
        temp_env <- new.env()
        temp_file <- tempfile(fileext = rv$solution_file_extension)
        writeLines(rv$solution_file_content, temp_file)
        
        # Direct knitr output to a tempfile and redirect fig.path to avoid
        # creating stray files in the app's working directory.
        temp_md <- tempfile(fileext = ".md")
        old_fig_path <- knitr::opts_chunk$get("fig.path")
        knitr::opts_chunk$set(fig.path = file.path(KNITR_SOLN_FIG_RESTORE, "fig-"))
        tryCatch({
          knitr::knit(temp_file, output = temp_md, envir = temp_env, quiet = TRUE)
        }, finally = {
          knitr::opts_chunk$set(
            fig.path = if (!is.null(old_fig_path)) old_fig_path else KNITR_DEFAULT_FIG_PATH)
          if (file.exists(temp_md)) unlink(temp_md)
        })
        
        rv$solution_env <- temp_env
        unlink(temp_file)
      }
      
      # Clear the restoring flag after a short delay so that any
      # observers triggered by the updateTextInput calls above still
      # see the flag.  session$onFlushed runs after the current flush
      # cycle completes, i.e. after all pending observers have fired.
      session$onFlushed(function() {
        rv$restoring_session <- FALSE
      }, once = TRUE)
      
      return(list(success = TRUE, session_id = session_data$session_id, timestamp = session_data$timestamp))
    }, error = function(e) {
      return(list(success = FALSE, error = e$message))
    })
  }
  
  # Setup shinyFiles folder choosers if available
  if (SHINYFILES_AVAILABLE) {
    # Allow browsing from root directory (/) for Unix-like systems
    # or from all available volumes for Windows
    roots <- if (.Platform$OS.type == "windows") {
      # getVolumes() returns a function that, when called, returns the volumes
      # This double call pattern is by design in the shinyFiles package
      shinyFiles::getVolumes()()
    } else {
      c(root = "/", home = path.expand("~"))
    }
    
    # Setup folder choosers
    shinyFiles::shinyDirChoose(input, "student_folder_select", roots = roots, session = session)
    shinyFiles::shinyDirChoose(input, "autoharp_student_folder_select", roots = roots, session = session)
    
    # Observer for Object Testing tab folder selection
    observeEvent(input$student_folder_select, {
      if (!is.integer(input$student_folder_select)) {
        selected_path <- shinyFiles::parseDirPath(roots, input$student_folder_select)
        if (length(selected_path) > 0) {
          updateTextInput(session, "student_folder", value = selected_path)
          # Also update the Autoharp Grading tab folder input
          updateTextInput(session, "autoharp_student_folder", value = selected_path)
        }
      }
    })
    
    # Observer for Autoharp Grading tab folder selection
    observeEvent(input$autoharp_student_folder_select, {
      if (!is.integer(input$autoharp_student_folder_select)) {
        selected_path <- shinyFiles::parseDirPath(roots, input$autoharp_student_folder_select)
        if (length(selected_path) > 0) {
          updateTextInput(session, "autoharp_student_folder", value = selected_path)
        }
      }
    })
  }
  
  # Session management observers
  
  # Load session button
  observeEvent(input$load_session, {
    req(input$session_file)
    
    result <- load_session_state(input$session_file$datapath)
    
    if (result$success) {
      showNotification(
        paste0("Session loaded successfully! ID: ", result$session_id),
        type = "message",
        duration = 5
      )
    } else {
      showNotification(
        paste0("Error loading session: ", result$error),
        type = "error",
        duration = 10
      )
    }
  })
  
  # Manual save session button
  observeEvent(input$save_session_manual, {
    result <- save_session_state()
    
    if (result$success) {
      showNotification(
        paste0("Session saved to: ", result$file),
        type = "message",
        duration = 5
      )
    } else {
      showNotification(
        paste0("Error saving session: ", result$error),
        type = "error",
        duration = 10
      )
    }
  })
  
  # Session status output
  output$session_status <- renderUI({
    if (!is.null(input$session_file)) {
      tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        paste(" Selected:", input$session_file$name)
      )
    }
  })
  
  # Current session info output
  output$current_session_info <- renderUI({
    if (!is.null(rv$session_id)) {
      tags$div(
        tags$p(tags$strong("Session ID: "), rv$session_id),
        if (!is.null(rv$last_save_time)) {
          tags$p(tags$strong("Last Saved: "), format(rv$last_save_time, "%Y-%m-%d %H:%M:%S"))
        } else {
          tags$p(tags$strong("Last Saved: "), "Not yet saved")
        },
        tags$p(tags$strong("Students: "), if (!is.null(rv$student_files)) length(rv$student_files) else 0),
        tags$p(tags$strong("Graded: "), if (!is.null(rv$plot_grades)) sum(rv$plot_grades$graded) else 0)
      )
    } else {
      tags$div(
        class = "alert alert-secondary",
        icon("info-circle"),
        " No active session. Start grading to create a new session."
      )
    }
  })
  
  # load soln template when uploaded
  observeEvent(input$solution_file, {
    req(input$solution_file)
    
    tryCatch({
      # create temp env and knit solution
      temp_env <- new.env()
      temp_file <- input$solution_file$datapath
      
      # Store the original file extension (detect from filename)
      # Preserve the original case-sensitive extension (.qmd or .Rmd)
      file_ext_lower <- tolower(tools::file_ext(input$solution_file$name))
      if (file_ext_lower == "qmd") {
        rv$solution_file_extension <- ".qmd"
      } else {
        # Default to .Rmd for RMD files or any other extension
        # (already validated by fileInput accept parameter)
        rv$solution_file_extension <- ".Rmd"
      }
      
      # read the original file content for template generation
      rv$solution_file_content <- readLines(temp_file, warn = FALSE)
      
      # Knit the solution to populate the environment.
      # Direct output to a tempfile and redirect fig.path to avoid creating
      # stray .md files or figure/ folders in the app's working directory.
      temp_md <- tempfile(fileext = ".md")
      old_fig_path <- knitr::opts_chunk$get("fig.path")
      knitr::opts_chunk$set(fig.path = file.path(KNITR_SOLN_FIG_DIR, "fig-"))
      tryCatch({
        knitr::knit(temp_file, output = temp_md, quiet = TRUE, envir = temp_env)
      }, finally = {
        knitr::opts_chunk$set(
          fig.path = if (!is.null(old_fig_path)) old_fig_path else KNITR_DEFAULT_FIG_PATH)
        if (file.exists(temp_md)) unlink(temp_md)
      })
      
      rv$solution_env <- temp_env
      
      # get all objects created in the solution (excluding internal ones)
      all_objects <- ls(temp_env, all.names = FALSE)
      # filter out functions that start with dots and internal objects
      rv$solution_objects <- all_objects[!grepl("^\\.", all_objects)]
      
      showNotification("Solution template loaded successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading solution:", e$message), type = "error")
    })
  })
  
  # render object selector based on solution objects
  output$object_selector <- renderUI({
    req(rv$solution_objects)
    
    checkboxGroupInput(
      inputId = "selected_objects",
      label = "Select Objects to Test:",
      choices = rv$solution_objects,
      selected = rv$solution_objects
    )
  })
  
  # load student files when folder path is provided
  observeEvent(input$student_folder, {
    req(input$student_folder)
    
    # if a session was just loaded, skip the entire observer.
    # load_session_state already restored student_files, plot_grades, etc.
    # the updateTextInput that triggered this observer was only meant to
    # sync the UI text; not to re-scan the folder and wipe state.
    if (isTRUE(rv$session_loaded)) {
      rv$session_loaded <- FALSE
      return(invisible())
    }
    
    folder_path <- input$student_folder
    
    # sync the folder path to Autoharp Grading tab if it's different.
    # Keep rv$sync_in_progress TRUE until the autoharp_student_folder observer
    # fires and clears it, so the autoharp observer can detect and skip the
    # programmatic update (avoiding a double notification).
    if (!rv$sync_in_progress &&
        !is.null(input$autoharp_student_folder) &&
        input$autoharp_student_folder != folder_path) {
      rv$sync_in_progress <- TRUE
      updateTextInput(session, "autoharp_student_folder", value = folder_path)
      # Do NOT reset rv$sync_in_progress here; the autoharp_student_folder
      # observer will clear it when it fires.
    }
    
    if (dir.exists(folder_path)) {
      # get all R, Rmd, and QMD files
      files <- list.files(folder_path,
                          pattern = "\\.(R|r|Rmd|rmd|qmd|Qmd)$",
                          full.names = TRUE)
      
      if (length(files) > 0) {
        rv$student_files <- files
        
        # fresh folder selection: initialize empty plot grades
        rv$plot_grades <- data.frame(
          student_file = basename(files),
          grade = rep(NA_real_, length(files)),
          comments = rep("", length(files)),
          graded = rep(FALSE, length(files)),
          stringsAsFactors = FALSE
        )
        
        showNotification(paste("Found", length(files), "student files"), type = "message")
      } else {
        showNotification("No R, Rmd, or QMD files found in the specified folder", type = "warning")
      }
    } else {
      showNotification("Folder does not exist", type = "error")
    }
  })
  
  # Mirror of the student_folder observer for the Autoharp Grading tab.
  # Shows the "Found xxx student files" notification when the autoharp student
  # folder path is entered directly (not via sync from the Object Testing tab).
  observeEvent(input$autoharp_student_folder, {
    req(input$autoharp_student_folder)
    
    # Skip if a session is being restored (updateTextInput already set the value)
    if (isTRUE(rv$restoring_session)) return(invisible())
    
    # If the Object Testing tab synced this input programmatically, clear the
    # flag and skip - the Object Testing observer already showed a notification.
    if (isTRUE(rv$sync_in_progress)) {
      rv$sync_in_progress <- FALSE
      return(invisible())
    }
    
    folder_path <- input$autoharp_student_folder
    
    if (dir.exists(folder_path)) {
      # Look for Rmd/QMD files (autoharp only processes these)
      files <- list.files(folder_path,
                          pattern = "\\.(R|Rmd|rmd|qmd|Qmd)$",
                          full.names = TRUE)
      
      if (length(files) > 0) {
        rv$autoharp_student_files <- files
        showNotification(paste("Found", length(files), "student files"), type = "message")
      } else {
        showNotification("No Rmd or QMD files found in the specified folder", type = "warning")
      }
    } else {
      showNotification("Folder does not exist", type = "error")
    }
  })
  
  # run automated grading
  observeEvent(input$run_grading, {
    req(rv$solution_env)
    req(rv$student_files)
    req(input$selected_objects)
    
    rv$processing <- TRUE
    
    # create progress indicator
    withProgress(message = "Grading students...", value = 0, {
      
      results_list <- list()
      student_plots_list <- list()
      n_students <- length(rv$student_files)
      
      # Get output directory from input or use default
      output_dir <- if (!is.null(input$output_folder) && !is.na(input$output_folder) && input$output_folder != "") {
        input$output_folder
      } else {
        "grading_output"
      }
      
      # Create output directory if it doesn't exist
      if (!dir.exists(output_dir)) {
        dir.create(output_dir, recursive = TRUE)
      }
      
      # create a plots output directory
      plots_output_dir <- file.path(output_dir, "student_plots")
      if (!dir.exists(plots_output_dir)) {
        dir.create(plots_output_dir, recursive = TRUE)
      }
      
      for (i in seq_along(rv$student_files)) {
        student_file <- rv$student_files[i]
        student_name <- basename(student_file)
        student_base <- tools::file_path_sans_ext(student_name)
        
        incProgress(1/n_students, detail = paste("Processing", student_name))
        
        # create student env
        student_env <- new.env()
        
        tryCatch({
          if (grepl("\\.(Rmd|rmd|qmd|Qmd)$", student_file)) {
            # for Rmd and QMD files, use knitr::knit to generate plots in a known location
            # create student-specific output directory for plots
            student_plot_dir <- file.path(plots_output_dir, student_base)
            if (!dir.exists(student_plot_dir)) {
              dir.create(student_plot_dir, recursive = TRUE)
            }
            
            # Snapshot knitr chunk options so they can be restored regardless of
            # whether knitting succeeds or fails (tryCatch finally block).
            old_opts <- list(
              fig.path = knitr::opts_chunk$get("fig.path"),
              dev = knitr::opts_chunk$get("dev"),
              error = knitr::opts_chunk$get("error")
            )
            
            # Redirect figures to the student-specific directory and allow
            # chunks to continue past errors.
            knitr::opts_chunk$set(
              fig.path = file.path(student_plot_dir, "fig-"),
              dev = "png",
              error = TRUE  # Continue knitting even if a chunk has errors
            )
            
            # knit to temporary file but keep figure output in student_plot_dir
            temp_output <- tempfile(fileext = ".md")
            tryCatch({
              knitr::knit(student_file, output = temp_output, quiet = TRUE, envir = student_env)
            }, finally = {
              # Always restore knitr options and remove the temp md file
              knitr::opts_chunk$set(
                fig.path = if (!is.null(old_opts$fig.path)) old_opts$fig.path else KNITR_DEFAULT_FIG_PATH,
                dev = if (!is.null(old_opts$dev)) old_opts$dev else "png",
                error = if (!is.null(old_opts$error)) old_opts$error else FALSE
              )
              if (file.exists(temp_output)) unlink(temp_output)
            })
          } else {
            # wrap each expression in try-catch by reading and evaluating separately
            source_with_trycatch(student_file, student_env)
          }
          
          # run tests on selected objects
          student_results <- run_object_tests(
            student_env = student_env,
            solution_env = rv$solution_env,
            objects_to_test = input$selected_objects
          )
          
          student_results$student_file <- student_name
          results_list[[i]] <- student_results
          
          # collect any plots generated from the student plot directory
          student_plots_list[[student_name]] <- collect_student_plots(student_file, plots_output_dir)
          
        }, error = function(e) {
          # If there's an error, record it
          error_result <- data.frame(
            student_file = student_name,
            object = "ERROR",
            test = "Execution",
            result = "FAIL",
            details = e$message,
            stringsAsFactors = FALSE
          )
          results_list[[i]] <- error_result
        })
      }
      
      # combine all results
      rv$grading_results <- bind_test_results(results_list)
      rv$student_plots <- student_plots_list
      rv$processing <- FALSE
    })
    
    # Auto-save session after grading
    save_result <- save_session_state()
    if (save_result$success) {
      showNotification(
        paste0("Grading complete! Session auto-saved to: ", basename(save_result$file)),
        type = "message"
      )
    } else {
      showNotification("Grading complete!", type = "message")
    }
  })
  
  # grading status output
  output$grading_status <- renderUI({
    if (rv$processing) {
      tags$div(
        class = "alert alert-info",
        "Processing student submissions..."
      )
    } else if (!is.null(rv$grading_results)) {
      n_students <- length(unique(rv$grading_results$student_file))
      n_tests <- nrow(rv$grading_results)
      n_passed <- sum(rv$grading_results$result == "PASS", na.rm = TRUE)
      
      tags$div(
        class = "alert alert-success",
        paste("Completed:", n_students, "students,", n_tests, "tests run,", 
              n_passed, "passed")
      )
    }
  })
  
  # grading results table (Summary)
  output$grading_results <- renderDT({
    req(rv$grading_results)
    
    # create a summary table using base R
    results <- rv$grading_results
    
    # asggregate by student_file and object
    summary_list <- by(results, list(results$student_file, results$object), function(x) {
      data.frame(
        Student = unique(x$student_file),
        Object = unique(x$object),
        `Tests Run` = nrow(x),
        Passed = sum(x$result == "PASS", na.rm = TRUE),
        Failed = sum(x$result == "FAIL", na.rm = TRUE),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    })
    
    summary_df <- do.call(rbind, summary_list)
    rownames(summary_df) <- NULL
    
    summary_df$`Pass Rate` <- paste0(round(summary_df$Passed / summary_df$`Tests Run` * 100, 1), "%")
    
    datatable(
      summary_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        ordering = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    )
  })
  
  # detailed test results
  output$detailed_results <- renderDT({
    req(rv$grading_results)
    
    failed <- rv$grading_results[rv$grading_results$result == "FAIL", 
                                 c("student_file", "object", "test", "details")]
    
    if (nrow(failed) > 0) {
      colnames(failed) <- c("Student", "Object", "Test", "Details")
      datatable(
        failed,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "300px",
          dom = 'Bfrtip',
          ordering = TRUE
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    } else {
      # Return empty datatable with message
      datatable(
        data.frame(Message = "No failed tests! All tests passed."),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # download handler for CSV
  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("grading_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(rv$grading_results)
      
      # combine object test results with plot grades using base R
      results <- rv$grading_results
      
      # agg by student_file
      summary_list <- by(results, results$student_file, function(x) {
        data.frame(
          student_file = unique(x$student_file),
          total_tests = nrow(x),
          tests_passed = sum(x$result == "PASS", na.rm = TRUE),
          tests_failed = sum(x$result == "FAIL", na.rm = TRUE),
          stringsAsFactors = FALSE
        )
      })
      
      object_summary <- do.call(rbind, summary_list)
      rownames(object_summary) <- NULL
      object_summary$object_score <- round(object_summary$tests_passed / object_summary$total_tests * 100, 2)
      
      if (!is.null(rv$plot_grades)) {
        # merge with plot grades
        plot_subset <- rv$plot_grades[, c("student_file", "grade", "comments")]
        names(plot_subset)[2:3] <- c("plot_grade", "plot_comments")
        
        final_results <- merge(object_summary, plot_subset, 
                               by = "student_file", all.x = TRUE)
      } else {
        final_results <- object_summary
      }
      
      write.csv(final_results, file, row.names = FALSE)
    }
  )
  
  # stu selector for plot grading
  output$student_selector <- renderUI({
    req(rv$student_files)
    
    selectInput(
      inputId = "current_student",
      label = "Select Student:",
      choices = setNames(
        seq_along(rv$student_files),
        basename(rv$student_files)
      ),
      selected = rv$current_student_index
    )
  })
  
  # update current student index when selector changes
  observeEvent(input$current_student, {
    # save current student's grade/comments before switching
    # save_current_grade checks advancing_student/restoring_session internally
    save_current_grade()
    
    rv$current_student_index <- as.integer(input$current_student)
    
    # clear the advancing flag here: this is the observer that the flag
    # was protecting against. Now that it has fired, it's safe to clear
    if (isTRUE(rv$advancing_student)) {
      rv$advancing_student <- FALSE
    }
  })
  
  # update grade inputs when student changes
  observeEvent(rv$current_student_index, {
    req(rv$plot_grades)
    req(rv$current_student_index)
    
    # Bounds checking
    if (rv$current_student_index > 0 && rv$current_student_index <= nrow(rv$plot_grades)) {
      # Get saved values for current student
      current_grade <- rv$plot_grades$grade[rv$current_student_index]
      current_comments <- rv$plot_grades$comments[rv$current_student_index]
      
      # Update inputs to show saved values
      updateNumericInput(session, "plot_grade", value = current_grade)
      updateTextAreaInput(session, "plot_comments", value = current_comments)
    }
  })
  
  # prev student button
  observeEvent(input$prev_student, {
    if (rv$current_student_index > 1) {
      # Save current student's grade/comments before switching
      save_current_grade()
      
      rv$advancing_student <- TRUE
      rv$current_student_index <- rv$current_student_index - 1
      updateSelectInput(session, "current_student",
                        selected = rv$current_student_index)
    }
  })
  
  # next student button
  observeEvent(input$next_student, {
    req(rv$student_files)
    if (rv$current_student_index < length(rv$student_files)) {
      # Save current student's grade/comments before switching
      save_current_grade()
      
      rv$advancing_student <- TRUE
      rv$current_student_index <- rv$current_student_index + 1
      updateSelectInput(session, "current_student",
                        selected = rv$current_student_index)
    }
  })
  
  # plot display
  output$plot_display <- renderUI({
    req(rv$student_files)
    req(rv$current_student_index)
    
    student_name <- basename(rv$student_files[rv$current_student_index])
    
    if (!is.null(rv$student_plots) && student_name %in% names(rv$student_plots)) {
      plots <- rv$student_plots[[student_name]]
      
      if (length(plots) > 0) {
        # display all plots for this student using base64 encoding
        plot_tags <- lapply(seq_along(plots), function(i) {
          plot_path <- plots[i]
          if (file.exists(plot_path)) {
            # read file and encode as base64
            img_data <- base64enc::base64encode(plot_path)
            # determine MIME type
            ext <- tolower(tools::file_ext(plot_path))
            mime_type <- switch(ext,
                                "png" = "image/png",
                                "jpg" = "image/jpeg",
                                "jpeg" = "image/jpeg",
                                "gif" = "image/gif",
                                "svg" = "image/svg+xml",
                                "image/png")
            img_src <- paste0("data:", mime_type, ";base64,", img_data)
            plot_title <- paste("Plot", i)
            # Create unique ID for this plot's data
            plot_id <- paste0("plot_", i, "_", rv$current_student_index)
            
            tags$div(
              class = "plot-container",
              tags$button(
                class = "expand-icon",
                `data-img-src` = img_src,
                `data-plot-title` = plot_title,
                onclick = "openPlotModalFromButton(this)",
                title = "Expand plot",
                shiny::icon("expand")
              ),
              tags$img(src = img_src, style = "max-width: 300px; height: auto;"),
              tags$p(plot_title, style = "text-align: center; margin-top: 5px; font-weight: bold;")
            )
          } else {
            tags$div(
              class = "alert alert-warning",
              paste("Plot file not found:", plot_path)
            )
          }
        })
        
        do.call(tagList, plot_tags)
      } else {
        tags$div(
          class = "alert alert-info",
          tags$i(class = "fa fa-info-circle"),
          " No plots found for this student. Run grading first to generate plots."
        )
      }
    } else {
      tags$div(
        class = "alert alert-warning",
        tags$i(class = "fa fa-exclamation-triangle"),
        " Run automated grading first to collect student plots."
      )
    }
  })
  
  # Student test results for Plot Grading tab
  output$student_test_results <- renderDT({
    req(rv$grading_results)
    req(rv$student_files)
    req(rv$current_student_index)
    
    student_name <- basename(rv$student_files[rv$current_student_index])
    
    # filter results for current student
    student_results <- rv$grading_results[rv$grading_results$student_file == student_name, ]
    
    if (nrow(student_results) > 0) {
      # select and rename columns for display
      display_df <- student_results[, c("object", "test", "result", "details")]
      colnames(display_df) <- c("Object", "Test", "Result", "Details")
      
      datatable(
        display_df,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          scrollY = "300px",
          dom = 'Bfrtip',
          ordering = TRUE
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      ) %>%
        DT::formatStyle(
          'Result',
          backgroundColor = DT::styleEqual(
            c('PASS', 'FAIL', 'SKIP', 'INFO'),
            c('#d4edda', '#f8d7da', '#fff3cd', '#d1ecf1')
          )
        )
    } else {
      datatable(
        data.frame(Message = "No test results available for this student. Run automated grading first."),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # save plot grade
  observeEvent(input$save_grade, {
    req(rv$plot_grades)
    req(rv$current_student_index)
    
    grade_val <- input$plot_grade
    comment_val <- input$plot_comments
    
    # safely store grade (may be NA from empty numeric input)
    rv$plot_grades$grade[rv$current_student_index] <- grade_val
    rv$plot_grades$comments[rv$current_student_index] <- if (is.null(comment_val)) "" else comment_val
    
    # mark as graded: the user explicitly clicked "Save Grade"
    rv$plot_grades$graded[rv$current_student_index] <- TRUE
    
    message("[save_grade] Student ", rv$current_student_index,
            " (", rv$plot_grades$student_file[rv$current_student_index], ")",
            " grade=", grade_val, " comments='", comment_val, "'",
            " graded=TRUE")
    message("[save_grade] plot_grades snapshot: ")
    message(paste(capture.output(print(rv$plot_grades)), collapse = "\n"))
    
    # Auto-save session to disk immediately
    save_result <- save_session_state()
    if (save_result$success) {
      showNotification("Grade saved and session auto-saved!", type = "message")
    } else {
      showNotification("Grade saved!", type = "message")
    }
    
    # Auto-advance to next student.
    # Set advancing_student flag to prevent save_current_grade() from firing
    # during the student switch (inputs still show old student's values).
    if (rv$current_student_index < nrow(rv$plot_grades)) {
      rv$advancing_student <- TRUE
      rv$current_student_index <- rv$current_student_index + 1
      updateSelectInput(session, "current_student",
                        selected = rv$current_student_index)
    }
  })
  
  # grading progress
  output$grading_progress <- renderUI({
    req(rv$plot_grades)
    
    n_graded <- sum(rv$plot_grades$graded)
    n_total <- nrow(rv$plot_grades)
    
    tags$div(
      tags$h6("Grading Progress"),
      tags$div(
        class = "progress",
        tags$div(
          class = "progress-bar",
          role = "progressbar",
          style = paste0("width: ", round(n_graded/n_total * 100), "%;"),
          paste0(n_graded, " / ", n_total)
        )
      )
    )
  })
  
  # plot grades table
  output$plot_grades_table <- renderDT({
    req(rv$plot_grades)
    
    graded_subset <- rv$plot_grades[rv$plot_grades$graded, 
                                    c("student_file", "grade", "comments")]
    
    if (nrow(graded_subset) > 0) {
      colnames(graded_subset) <- c("Student", "Grade", "Comments")
      datatable(
        graded_subset,
        options = list(
          pageLength = 10,
          scrollX = TRUE,
          dom = 'Bfrtip',
          ordering = TRUE
        ),
        rownames = FALSE,
        class = 'cell-border stripe hover'
      )
    } else {
      datatable(
        data.frame(Message = "No plots have been graded yet."),
        options = list(dom = 't'),
        rownames = FALSE
      )
    }
  })
  
  # gen template with autoharp test chunks
  observeEvent(input$generate_template, {
    req(rv$solution_env)
    req(rv$solution_objects)
    req(input$selected_objects)
    
    tryCatch({
      # Generate the autoharp-compatible template
      template <- generate_autoharp_template(
        original_content = rv$solution_file_content,
        solution_env = rv$solution_env,
        objects_to_test = input$selected_objects,
        test_prefix = input$test_prefix,
        include_all_tests = input$include_all_tests
      )
      
      rv$generated_template <- template
      showNotification("Template generated successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error generating template:", e$message), type = "error")
    })
  })
  
  # template status
  output$template_status <- renderUI({
    if (is.null(rv$solution_env)) {
      tags$div(class = "status-alert status-warning",
               icon("exclamation-triangle"),
               " Please upload a solution template in the 'Object Testing' tab first.")
    } else if (is.null(rv$generated_template)) {
      tags$div(class = "status-alert status-info",
               icon("info-circle"),
               " Click 'Generate Template' to create an autoharp-compatible template.")
    } else {
      tags$div(class = "status-alert status-success",
               icon("check-circle"),
               " Template generated successfully! You can preview it below and download it.")
    }
  })
  
  # template preview
  output$template_preview <- renderText({
    if (!is.null(rv$generated_template)) {
      paste(rv$generated_template, collapse = "\n")
    } else {
      "# Template will appear here after generation..."
    }
  })
  
  # tests documentation
  output$tests_documentation <- renderUI({
    tags$div(
      tags$h5("Numeric Vectors"),
      tags$ul(
        tags$li(tags$strong("length"), " - Compares length of vectors"),
        tags$li(tags$strong("mean"), " - Compares mean values (tolerance: 1e-6)"),
        tags$li(tags$strong("sd"), " - Compares standard deviation (tolerance: 1e-6)"),
        tags$li(tags$strong("min"), " - Compares minimum values"),
        tags$li(tags$strong("max"), " - Compares maximum values"),
        tags$li(tags$strong("sum"), " - Compares sum of values"),
        tags$li(tags$strong("na_count"), " - Compares count of NA values"),
        tags$li(tags$strong("identical"), " - Checks if all values match exactly")
      ),
      
      tags$h5("Character Vectors"),
      tags$ul(
        tags$li(tags$strong("length"), " - Compares length of vectors"),
        tags$li(tags$strong("identical"), " - Checks if all values match exactly"),
        tags$li(tags$strong("unique_count"), " - Compares count of unique values"),
        tags$li(tags$strong("nchar_total"), " - Compares total character count")
      ),
      
      tags$h5("Data Frames"),
      tags$ul(
        tags$li(tags$strong("nrow"), " - Compares number of rows"),
        tags$li(tags$strong("ncol"), " - Compares number of columns"),
        tags$li(tags$strong("colnames"), " - Checks if column names match"),
        tags$li(tags$strong("column_types"), " - Checks if column types match"),
        tags$li(tags$strong("column_means"), " - Compares means of numeric columns"),
        tags$li(tags$strong("column_sds"), " - Compares SDs of numeric columns")
      ),
      
      tags$h5("Matrices"),
      tags$ul(
        tags$li(tags$strong("nrow"), " - Compares number of rows"),
        tags$li(tags$strong("ncol"), " - Compares number of columns"),
        tags$li(tags$strong("rownames"), " - Checks if row names match"),
        tags$li(tags$strong("colnames"), " - Checks if column names match"),
        tags$li(tags$strong("mean"), " - Compares overall mean"),
        tags$li(tags$strong("max_diff"), " - Checks maximum element difference")
      ),
      
      tags$h5("Lists"),
      tags$ul(
        tags$li(tags$strong("length"), " - Compares number of elements"),
        tags$li(tags$strong("names"), " - Checks if element names match"),
        tags$li(tags$strong("element_tests"), " - Tests each named element")
      ),
      
      tags$h5("Functions"),
      tags$ul(
        tags$li(tags$strong("num_args"), " - Compares number of arguments"),
        tags$li(tags$strong("arg_names"), " - Checks if argument names match")
      ),
      
      tags$h5("Factors"),
      tags$ul(
        tags$li(tags$strong("length"), " - Compares length"),
        tags$li(tags$strong("nlevels"), " - Compares number of levels"),
        tags$li(tags$strong("levels"), " - Checks if levels match"),
        tags$li(tags$strong("level_order"), " - Checks if level order matches"),
        tags$li(tags$strong("frequencies"), " - Compares level frequencies")
      )
    )
  })
  
  # download template handler
  output$download_template <- downloadHandler(
    filename = function() {
      # Use the original file extension if available, otherwise default to .Rmd
      extension <- if (!is.null(rv$solution_file_extension)) rv$solution_file_extension else ".Rmd"
      paste0("solution_template_autoharp_", format(Sys.time(), "%Y%m%d_%H%M%S"), extension)
    },
    content = function(file) {
      req(rv$generated_template)
      writeLines(rv$generated_template, file)
    }
  )
  
  # =========================================
  # Tab 4: Autoharp Grading Server Logic
  # =========================================
  
  # run autoharp grading with user-selected template and folder
  observeEvent(input$run_autoharp_grading, {
    
    rv$autoharp_processing <- TRUE
    
    # Create progress indicator
    withProgress(message = "Running autoharp grading...", value = 0, {
      
      tryCatch({
        # get template path from user input or use default
        if (!is.null(input$autoharp_template_file)) {
          template_path <- input$autoharp_template_file$datapath
          
          # validate file extension (case-insensitive by converting to lowercase)
          # handles .Rmd, .rmd, .RMD, .qmd, .Qmd, .QMD, etc.
          if (!grepl("\\.(rmd|qmd)$", tolower(input$autoharp_template_file$name))) {
            stop("Template file must be an RMD or QMD file (.Rmd, .rmd, .qmd, or .Qmd)")
          }
          
          # validate file exists and is readable
          if (!file.exists(template_path) || file.access(template_path, mode = 4) != 0) {
            stop("Template file cannot be read")
          }
        } else {
          # use default template file if no file uploaded (RMD format for backward compatibility)
          template_path <- "solution_template_autoharp_20251218_121348.Rmd"
        }
        
        # check if template exists
        if (!file.exists(template_path)) {
          stop(paste("Template file not found:", template_path))
        }
        
        # get pattern from input, default to "test"
        pattern <- input$test_pattern
        if (is.null(pattern) || pattern == "") {
          pattern <- "test"
        }
        
        # get student folder from user input or use default
        sol_folder <- input$autoharp_student_folder
        if (is.null(sol_folder) || sol_folder == "") {
          sol_folder <- "sol"
        }
        
        # Normalize the path (handle both absolute and relative paths)
        sol_folder_normalized <- tryCatch({
          if (is_absolute_path(sol_folder)) {
            # Absolute path - normalize it directly
            normalize_path_safe(sol_folder)
          } else {
            # Relative path - construct relative to cwd
            cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
            candidate_path <- file.path(cwd, sol_folder)
            normalize_path_safe(candidate_path)
          }
        }, error = function(e) {
          stop(paste("Invalid folder path:", e$message))
        })
        
        # use normalized path
        sol_folder <- sol_folder_normalized
        
        if (!dir.exists(sol_folder)) {
          stop(paste("Student scripts folder not found:", sol_folder))
        }
        
        student_files <- list.files(sol_folder, 
                                    pattern = "\\.(R|Rmd|rmd|qmd|Qmd)$", 
                                    full.names = TRUE)
        
        if (length(student_files) == 0) {
          stop(paste("No Rmd or QMD files found in folder:", sol_folder))
        }
        
        incProgress(0.1, detail = paste("Found", length(student_files), "student files"))
        
        # Get output directory from input or use default
        out_dir <- if (!is.null(input$autoharp_output_folder) && !is.na(input$autoharp_output_folder) && input$autoharp_output_folder != "") {
          input$autoharp_output_folder
        } else {
          "student_out"
        }

        # create output directory for autoharp
        if (!dir.exists(out_dir)) {
          dir.create(out_dir, recursive = TRUE)
        }

        # Convert out_dir to absolute path so it works regardless of knit_root_dir
        out_dir <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)
        
        # pop solution environment using autoharp function
        incProgress(0.1, detail = "Loading template...")
        # print(student_files[1])
        s_env <- tryCatch({
          populate_soln_env(template_path, pattern = pattern,
                            knit_root_dir = 
                            dirname(normalizePath(student_files[1], mustWork = TRUE)))
        }, error = function(e) {
          stop(paste("Failed to process autoharp template:", e$message,
                     "\nPlease ensure the template contains valid autoharp test chunks with pattern:", pattern))
        })
        
        n_students <- length(student_files)
        
        # run render_one on all student scripts using lapply
        incProgress(0.1, detail = "Processing student scripts...")
        
        # helper function to create consistent error dataframe
        create_error_result <- function(student_file, error_msg) {
          data.frame(
            fname = student_file,
            time_stamp = as.character(Sys.time()),
            run_status = "FAIL",
            run_time = NA_real_,
            run_mem = NA_real_,
            error_message = error_msg,
            stringsAsFactors = FALSE
          )
        }
        
        # track progress more precisely
        progress_per_student <- 0.6 / n_students
        
        corr_out <- lapply(seq_along(student_files), function(i) {
          student_file <- student_files[i]
          incProgress(progress_per_student, detail = paste("Processing", basename(student_file)))

          # Use the student file's directory as the working directory for knitting
          # This ensures relative paths in student scripts (e.g., "../data/file.csv") work correctly
          student_dir <- dirname(student_file)

          tryCatch({
            result <- render_one(
              rmd_name = student_file,
              out_dir = out_dir,
              knit_root_dir = student_dir,
              soln_stuff = s_env,
              permission_to_install = input$permission_to_install
            )
            # Ensure consistent columns by adding error_message as NA
            if (!"error_message" %in% names(result)) {
              result$error_message <- NA_character_
            }
            # Ensure we only return one row per student (take first row if multiple)
            if (nrow(result) > 1) {
              result <- result[1, , drop = FALSE]
            }
            result
          }, error = function(e) {
            # Return error result with consistent structure
            create_error_result(student_file, e$message)
          })
        })
        
        # combine results into a single dataframe by normalizing columns
        # handles cases where different results may have different columns
        all_cols <- unique(unlist(lapply(corr_out, names)))
        normalized_results <- lapply(corr_out, function(df) {
          missing_cols <- setdiff(all_cols, names(df))
          for (col in missing_cols) {
            df[[col]] <- NA
          }
          df[, all_cols, drop = FALSE]
        })
        combined_results <- do.call("rbind", normalized_results)

        # Deduplicate: ensure only one row per student file
        # Keep only the first occurrence of each fname
        if (!is.null(combined_results) && nrow(combined_results) > 0) {
          combined_results <- combined_results[!duplicated(combined_results$fname), , drop = FALSE]
        }
        rv$autoharp_results <- combined_results
        rv$autoharp_student_files <- student_files
        
        # Populate student_files and plot_grades for Plot Grading tab if not already set
        # from Object Testing workflow
        if (is.null(rv$student_files)) {
          rv$student_files <- student_files
          rv$plot_grades <- data.frame(
            student_file = basename(student_files),
            grade = rep(NA_real_, length(student_files)),
            comments = rep("", length(student_files)),
            graded = rep(FALSE, length(student_files)),
            stringsAsFactors = FALSE
          )
        }
        
        # Collect plots from autoharp output directory for Plot Grading tab
        incProgress(0.05, detail = "Collecting student plots...")
        autoharp_plots <- list()
        for (sf in student_files) {
          sname <- basename(sf)
          autoharp_plots[[sname]] <- collect_student_plots(sf, out_dir)
        }
        # Merge with any existing plots from Object Testing (don't overwrite)
        for (sname in names(autoharp_plots)) {
          if (length(autoharp_plots[[sname]]) > 0 &&
              length(rv$student_plots[[sname]]) == 0) {
            rv$student_plots[[sname]] <- autoharp_plots[[sname]]
          }
        }
        
        incProgress(0.05, detail = "Complete!")
        
        # Auto-save session after autoharp grading, using the autoharp output
        # directory (out_dir) so the session file lives alongside the grading
        # results rather than in the Object Testing output folder.
        save_result <- save_session_state(out_dir = out_dir)
        if (save_result$success) {
          showNotification(
            paste0("Autoharp grading complete! Session auto-saved to: ", basename(save_result$file)),
            type = "message"
          )
        } else {
          showNotification("Autoharp grading complete!", type = "message")
        }
        
      }, error = function(e) {
        showNotification(paste("Error during autoharp grading:", e$message), type = "error")
      })
      
      rv$autoharp_processing <- FALSE
    })
  })
  
  # autoharp status output
  output$autoharp_status <- renderUI({
    if (rv$autoharp_processing) {
      tags$div(
        class = "alert alert-info",
        "Processing student submissions with autoharp..."
      )
    } else if (!is.null(rv$autoharp_results)) {
      # Count unique student files (in case there are duplicate rows)
      n_students <- length(unique(rv$autoharp_results$fname))
      n_success <- sum(rv$autoharp_results$run_status == "SUCCESS", na.rm = TRUE)
      
      tags$div(
        class = "alert alert-success",
        paste("Completed:", n_students, "students processed,", 
              n_success, "successful,", n_students - n_success, "failed")
      )
    } else {
      tags$div(
        class = "alert alert-info",
        icon("info-circle"),
        " Upload a template file and specify the student scripts folder, then click 'Run Autoharp Grading'."
      )
    }
  })
  
  # autoharp results table with plot grades
  output$autoharp_results <- renderDT({
    req(rv$autoharp_results)
    
    results <- rv$autoharp_results
    
    # Create comprehensive display dataframe
    display_df <- data.frame(
      Student = basename(results$fname),
      Status = results$run_status,
      Timestamp = results$time_stamp,
      stringsAsFactors = FALSE
    )
    
    # Add run_time and run_mem if available
    if ("run_time" %in% names(results)) {
      display_df$`Run Time (s)` <- round(results$run_time, 2)
    }
    if ("run_mem" %in% names(results)) {
      display_df$`Memory Used` <- results$run_mem
    }
    
    # Add any correctness check columns (they have specific patterns)
    corr_cols <- names(results)[!names(results) %in% c("fname", "time_stamp", "run_status", "run_time", "run_mem", "error", "error_message")]
    for (col in corr_cols) {
      display_df[[col]] <- results[[col]]
    }
    
    # Add plot grades if available
    if (!is.null(rv$plot_grades)) {
      # Match students between autoharp results and plot grades
      # Extract basename for consistent matching
      plot_grades_df <- rv$plot_grades[rv$plot_grades$graded, c("student_file", "grade", "comments")]
      
      if (nrow(plot_grades_df) > 0) {
        # Ensure consistent basename matching
        plot_grades_df$student_file <- basename(plot_grades_df$student_file)
        
        # Merge by student name
        display_df <- merge(
          display_df, 
          plot_grades_df, 
          by.x = "Student", 
          by.y = "student_file", 
          all.x = TRUE
        )
        names(display_df)[names(display_df) == "grade"] <- "Plot_Grade"
        names(display_df)[names(display_df) == "comments"] <- "Plot_Comments"
      }
    }
    
    datatable(
      display_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = 'Bfrtip',
        ordering = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatStyle(
        'Status',
        backgroundColor = DT::styleEqual(
          c('SUCCESS', 'FAIL', 'UNKNOWN'),
          c('#d4edda', '#f8d7da', '#fff3cd')
        )
      )
  })
  
  # Download handler for Excel file with two sheets
  output$download_autoharp_excel <- downloadHandler(
    filename = function() {
      paste0("autoharp_grading_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(rv$autoharp_results)
      
      # Check for openxlsx package with user-friendly error handling
      if (!requireNamespace("openxlsx", quietly = TRUE)) {
        showNotification(
          "Package 'openxlsx' is required for Excel export but is not installed.\nPlease install it with: install.packages('openxlsx')",
          type = "error",
          duration = 10
        )
        stop("openxlsx package not available")
      }
      
      results <- rv$autoharp_results
      
      # Create comprehensive grading results dataframe
      sheet1_df <- data.frame(
        student_file = basename(results$fname),
        autoharp_status = results$run_status,
        timestamp = results$time_stamp,
        stringsAsFactors = FALSE
      )
      
      # Add all available columns from autoharp results
      for (col in names(results)) {
        if (!col %in% c("fname", "time_stamp", "run_status")) {
          sheet1_df[[col]] <- results[[col]]
        }
      }
      
      # Create workbook
      wb <- openxlsx::createWorkbook()
      
      # Add first sheet: Grading Results
      openxlsx::addWorksheet(wb, "Grading Results")
      openxlsx::writeData(wb, "Grading Results", sheet1_df)
      
      # Add second sheet: Plot Grades/Comments
      if (!is.null(rv$plot_grades)) {
        sheet2_df <- rv$plot_grades[, c("student_file", "grade", "comments", "graded")]
        names(sheet2_df) <- c("Student", "Plot Grade", "Comments", "Graded")
        
        openxlsx::addWorksheet(wb, "Plot Grades")
        openxlsx::writeData(wb, "Plot Grades", sheet2_df)
      } else {
        # Create empty sheet with message
        openxlsx::addWorksheet(wb, "Plot Grades")
        openxlsx::writeData(wb, "Plot Grades", 
                            data.frame(Message = "No plot grades available. Use Tab 2 to grade plots."))
      }
      
      # Save workbook
      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)


# devtools::load_all()
# 
# s_env <- populate_soln_env("solution_template_autoharp.Rmd", pattern="test")
# stud_script_names <- c("sol/student_01.rmd", "sol/student_02.rmd")
# 
# # run autoharp function "render_one" on student scripts.
# corr_out <- lapply(stud_script_names, render_one, out_dir = "student_out",
#                    knit_root_dir = getwd(), soln_stuff = s_env)
# a = do.call("rbind", corr_out)



