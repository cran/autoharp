library(shiny)
library(DT)
library(base64enc)
library(rmarkdown)
library(autoharp)
library(shinyFiles)

SHINYFILES_AVAILABLE <- TRUE
source(file.path("R", "helpers.R"))

# =========================================
# Path helpers
# =========================================

# Helper function to check if a path is absolute
# Returns TRUE if the path starts with:
#   - Windows drive letter: C:, D:, etc.
#   - Unix absolute path: /
#   - UNC path: \\server or //server
is_absolute_path <- function(path) {
  grepl("^([A-Za-z]:|/|\\\\\\\\|//)", path)
}

# TRUE for NULL, NA, empty or whitespace-only input values
is_blank <- function(x) {
  is.null(x) || length(x) == 0 || is.na(x[1]) || !nzchar(trimws(x[1]))
}

# Resolve a typed folder path: absolute paths are used as they are, relative
# paths are resolved against the app's working directory. NULL if blank.
resolve_path <- function(path) {
  if (is_blank(path)) return(NULL)
  path <- path.expand(trimws(path))
  if (!is_absolute_path(path)) {
    path <- file.path(getwd(), path)
  }
  # normalize the part that exists, so "../" is resolved even when the
  # folder itself does not exist yet
  tidy <- function(p) {
    if (file.exists(p) || dirname(p) == p) return(normalizePath(p, winslash = "/", mustWork = FALSE))
    file.path(tidy(dirname(p)), basename(p))
  }
  tidy(path)
}

# Resolved folder path if the folder exists, otherwise NULL
existing_folder <- function(path) {
  resolved <- resolve_path(path)
  if (!is.null(resolved) && dir.exists(resolved)) resolved else NULL
}

# Student submission files (R, Rmd, QMD) in a folder
STUDENT_FILE_PATTERN <- "\\.(R|r|Rmd|rmd|qmd|Qmd)$"
list_student_files <- function(folder) {
  list.files(folder, pattern = STUDENT_FILE_PATTERN, full.names = TRUE)
}

# Status of a typed folder path: its resolved location, whether it exists,
# and the student files in it
folder_info <- function(path) {
  resolved <- resolve_path(path)
  exists <- !is.null(resolved) && dir.exists(resolved)
  files <- if (exists) list_student_files(resolved) else character(0)
  list(path = resolved, exists = exists, files = files)
}

# e.g. "3 .Rmd, 2 .R"
file_type_summary <- function(files) {
  counts <- table(paste0(".", tools::file_ext(files)))
  paste(paste(counts, names(counts)), collapse = ", ")
}

# Temporary copy of a student file that keeps running after an error, as the
# original Object Testing grading did (rmarkdown::render, used by render_one,
# otherwise stops at the first error). A hidden first chunk sets
# knitr::opts_chunk$set(error = TRUE). The student's own file is not changed.
allow_errors_copy <- function(student_file) {
  copy_dir <- tempfile("graded_")
  dir.create(copy_dir)
  copy <- file.path(copy_dir, basename(student_file))
  lines <- readLines(student_file, warn = FALSE)
  if (grepl("\\.[Rr]$", student_file)) {
    # R scripts are rendered via knitr::spin; "#+" lines start chunks
    lines <- c("#+ autoharp_allow_errors, include=FALSE",
               "knitr::opts_chunk$set(error = TRUE)",
               "#+",
               lines)
  } else {
    setup <- c("```{r autoharp_allow_errors, include=FALSE}",
               "knitr::opts_chunk$set(error = TRUE)",
               "```",
               "")
    # insert after the YAML header, if there is one
    yaml_end <- 0
    if (length(lines) > 0 && grepl("^---\\s*$", lines[1])) {
      closing <- which(grepl("^---\\s*$", lines))[2]
      if (!is.na(closing)) yaml_end <- closing
    }
    lines <- append(lines, setup, after = yaml_end)
  }
  writeLines(lines, copy)
  copy
}

# Errors that occurred in a student's code chunks, read from the knitted
# markdown that render_one leaves in the output folder
student_code_errors <- function(out_dir, student_file) {
  md_file <- file.path(out_dir, paste0(tools::file_path_sans_ext(basename(student_file)), ".knit.md"))
  if (!file.exists(md_file)) return(character(0))
  sub("^Error: ", "", extract_knit_errors(readLines(md_file, warn = FALSE)))
}

# Where grading output (rendered files, plots, session RDS files) is written.
# Absolute paths are used as they are. Relative paths are placed next to the
resolve_output_dir <- function(output, student_folder) {
  if (is_blank(output)) output <- "student_out"
  output <- path.expand(trimws(output))
  if (is_absolute_path(output)) {
    return(normalizePath(output, winslash = "/", mustWork = FALSE))
  }
  student_dir <- existing_folder(student_folder)
  if (is.null(student_dir)) return(NULL)
  normalizePath(file.path(dirname(student_dir), output), winslash = "/", mustWork = FALSE)
}

# Width and height of a PNG file read from its header, or NULL for other files
png_dimensions <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con))
  bytes <- readBin(con, "raw", 24)
  png_signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (length(bytes) < 24 || !identical(bytes[1:8], png_signature)) return(NULL)
  to_int <- function(b) sum(as.integer(b) * 256^(3:0))
  c(width = to_int(bytes[17:20]), height = to_int(bytes[21:24]))
}

# Short names of the default tests generate_type_tests() writes for an object
suggested_tests <- function(obj, obj_name, include_all_tests = TRUE) {
  scalars <- generate_type_tests(obj, obj_name, include_all_tests)$scalars
  prefix <- paste0(sanitize_name(obj_name), "_")
  tests <- substring(scalars, nchar(prefix) + 1)
  gsub("_", " ", sub("_check$", "", tests))
}

# Error messages from knitted markdown (knitr keeps going past chunk errors)
extract_knit_errors <- function(md_lines) {
  idx <- grep("^## Error", md_lines)
  vapply(idx, function(i) {
    msg <- sub("^## ", "", md_lines[i])
    if (i < length(md_lines) && grepl("^## !", md_lines[i + 1])) {
      msg <- paste(msg, sub("^## ! ?", "", md_lines[i + 1]))
    }
    # name the missing file: it is only given in the warning printed just before
    if (grepl("cannot open the connection", msg)) {
      before <- paste(sub("^## ", "", md_lines[max(1, i - 8):max(1, i - 1)]), collapse = " ")
      missing_file <- regmatches(before, regexpr("cannot open file '[^']+'", before))
      if (length(missing_file) > 0) msg <- paste0(msg, " (", missing_file, ")")
    }
    msg
  }, character(1))
}

# Text input with a folder browser button underneath (as in the original
# layout) and a live status line (files found / folder not found)
folder_input <- function(id, label, button_id, placeholder = NULL) {
  tagList(
    textInput(id, label, placeholder = placeholder, width = "100%"),
    if (SHINYFILES_AVAILABLE) {
      shinyFiles::shinyDirButton(button_id, "Browse...", label,
                                 class = "btn-secondary btn-browse")
    },
    uiOutput(paste0(id, "_status"))
  )
}

# Sidebar and main column widths: 3/9 on large screens (as in the original
# layout), 4/8 on medium screens so the sidebar inputs are not cramped
sidebar_column <- function(...) div(class = "col-sm-4 col-lg-3", ...)
main_column <- function(...) div(class = "col-sm-8 col-lg-9", ...)

# Documentation of the default tests generated for each object type
# (matches generate_type_tests() in R/helpers.R; "extra" tests are only added
# when "Include all comprehensive tests" is ticked)
tests_documentation_ui <- function() {
  doc <- list(
    "Numeric Vectors" = list(
      basic = c(length = "compares length", mean = "compares mean (tolerance 1e-6)",
                identical = "checks all values match (tolerance 1e-6)"),
      extra = c(sd = "compares standard deviation", "min / max" = "compares minimum and maximum",
                sum = "compares sum", na_count = "compares number of NA values")),
    "Character Vectors" = list(
      basic = c(length = "compares length", identical = "checks all values match"),
      extra = c(unique_count = "compares number of unique values",
                nchar_total = "compares total number of characters")),
    "Logical Vectors" = list(
      basic = c(length = "compares length", identical = "checks all values match",
                sum_true = "compares number of TRUE values"),
      extra = c(na_count = "compares number of NA values")),
    "Data Frames" = list(
      basic = c("nrow / ncol" = "compares dimensions", colnames = "checks column names match"),
      extra = c(coltypes = "checks column types match",
                "col_<name>_mean" = "compares means of numeric columns (up to 3)")),
    "Matrices" = list(
      basic = c("nrow / ncol / dim" = "compares dimensions"),
      extra = c("rownames / colnames" = "checks names match", mean = "compares overall mean",
                max_diff = "checks largest element difference")),
    "Lists" = list(
      basic = c(length = "compares number of elements", names = "checks element names match"),
      extra = c("elem_<name>" = "compares named elements (up to 3)")),
    "Functions" = list(
      basic = c(num_args = "compares number of arguments", arg_names = "checks argument names match"),
      extra = c(is_function = "checks the object is a function")),
    "Factors" = list(
      basic = c(length = "compares length", "nlevels / levels" = "compares the levels"),
      extra = c(level_order = "checks level order matches", frequencies = "compares level frequencies")),
    "Other Objects" = list(
      basic = c(class = "checks classes match", identical = "checks objects are equal"),
      extra = character(0))
  )
  test_items <- function(tests, extra = FALSE) {
    lapply(names(tests), function(test) {
      tags$li(class = if (extra) "extra-test",
              tags$strong(test), paste0(" - ", tests[[test]]),
              if (extra) tags$span(class = "extra-mark", "*"))
    })
  }
  items <- lapply(names(doc), function(type) {
    div(class = "col-sm-6 col-lg-4",
        div(class = "tests-doc-item",
            tags$h5(type),
            tags$ul(test_items(doc[[type]]$basic),
                    test_items(doc[[type]]$extra, extra = TRUE))))
  })
  tagList(
    div(class = "row tests-doc", items),
    p(class = "text-muted small",
      "* Comprehensive tests: only added when \"Include all comprehensive tests\" is ticked.")
  )
}


# =========================================
# UI
# =========================================

ui <- fluidPage(
  title = "Autoharp Instructor Grading App",
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      // Function to open plot modal from button with data attributes
      function openPlotModalFromButton(button) {
        var modal = document.getElementById('plotModal');
        document.getElementById('modalPlotImg').src = button.getAttribute('data-img-src');
        document.getElementById('modalPlotTitle').textContent = button.getAttribute('data-plot-title');
        modal.classList.add('active');
      }

      // Function to close plot modal
      function closePlotModal() {
        document.getElementById('plotModal').classList.remove('active');
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
            if (e.target === modal) closePlotModal();
          });
        }
      });

      // Close modal with Escape key
      document.addEventListener('keydown', function(e) {
        if (e.key === 'Escape') closePlotModal();
      });
    "))
  ),

  # Header with session controls in the top-right corner
  div(class = "main-header",
      div(class = "header-content",
          div(class = "header-title",
              h2("Autoharp Instructor Grading App"),
              p("Automated grading of R/Rmd/QMD student submissions")
          ),
          div(class = "header-session",
              div(class = "header-session-buttons",
                  actionButton("save_session_btn", "Save Session", icon = icon("save"),
                               class = "btn-header"),
                  actionButton("load_session_btn", "Load Session", icon = icon("folder-open"),
                               class = "btn-header")
              ),
              uiOutput("autosave_status")
          )
      )
  ),

  # Main content with tabs
  tabsetPanel(
    id = "main_tabs",
    type = "tabs",

    # Tab 1: Template Generation
    tabPanel(
      "Template Generation",
      value = "template",

      br(),

      fluidRow(
        # Left sidebar - Setup
        sidebar_column(
          div(class = "sidebar-panel",
              h4(icon("cog"), " Setup"),

              fileInput(
                inputId = "solution_file",
                label = "Upload Model Solution (RMD/QMD):",
                accept = c(".Rmd", ".rmd", ".qmd", ".Qmd"),
                width = "100%"
              ),

              folder_input("tg_run_folder", "Run Model Solution From (optional):",
                           "tg_run_folder_select",
                           placeholder = "/path/to/folder"),

              p(class = "text-muted small",
                "Set this if the model solution reads data files: relative paths in it ",
                "(e.g. ../data/file.csv) are resolved from this folder. ",
                "Leave it empty if the solution does not read any files."),

              div(class = "divider"),

              h5(icon("cogs"), " Template Options"),

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

              p(class = "text-muted small", style = "margin-top: 10px;",
                "Runs the model solution, identifies the objects it creates, ",
                "and generates default tests based on their types."),

              uiOutput("solution_status")
          )
        ),

        # Main content area
        main_column(
          div(class = "panel-section",
              h4(icon("list-check"), " Select Objects to Test"),
              p("Choose the objects to include in the grading template and review the suggested tests."),

              uiOutput("objects_panel"),

              conditionalPanel(
                condition = "output.template_ready",
                div(class = "action-row",
                    actionButton("update_template", "Update Template (optional)",
                                 class = "btn-info", icon = icon("sync")),
                    downloadButton("download_template", "Download Template",
                                   class = "btn-success"),
                    actionButton("use_template", "Use for Autoharp Grading",
                                 class = "btn-primary", icon = icon("arrow-right"))
                )
              )
          ),

          div(class = "panel-section",
              h4(icon("file-code"), " Generated Template Preview"),
              p("Preview of the autoharp-compatible template with test chunks."),
              uiOutput("template_status"),
              conditionalPanel(
                condition = "output.template_ready",
                div(class = "code-preview", verbatimTextOutput("template_preview"))
              )
          ),

          div(class = "panel-section",
              h4(icon("book"), " Generated Tests Documentation"),
              p("List of all tests that will be generated for each object type."),
              tests_documentation_ui()
          )
        )
      )
    ),

    # Tab 2: Autoharp Grading
    tabPanel(
      "Autoharp Grading",
      value = "autoharp",

      br(),

      fluidRow(
        sidebar_column(
          div(class = "sidebar-panel",
              h4(icon("cogs"), " Grading Setup"),

              p("Run automated evaluation on student submissions using a grading template."),

              div(class = "divider"),

              h5(icon("cog"), " Configuration"),

              fileInput(
                inputId = "autoharp_template_file",
                label = "Grading Template (RMD/QMD):",
                accept = c(".Rmd", ".rmd", ".qmd", ".Qmd"),
                width = "100%"
              ),
              uiOutput("autoharp_template_source"),

              folder_input("autoharp_student_folder", "Path to Student Scripts Folder:",
                           "autoharp_student_folder_select",
                           placeholder = "/path/to/student_scripts"),

              folder_input("autoharp_run_folder", "Run Grading Template From (optional):",
                           "autoharp_run_folder_select",
                           placeholder = "Defaults to student scripts folder"),

              p(class = "text-muted small",
                "Student scripts always run from their own folder."),

              textInput(
                inputId = "autoharp_output_folder",
                label = "Output Directory (optional):",
                value = "student_out",
                width = "100%"
              ),

              uiOutput("output_dir_hint"),

              textInput(
                inputId = "test_pattern",
                label = "Test Chunk Pattern:",
                value = "test",
                width = "100%"
              ),

              p(class = "text-muted small",
                "Pattern to identify test chunks in the template (default: 'test')."),

              checkboxInput(
                inputId = "permission_to_install",
                label = "Allow package installation during grading",
                value = FALSE,
                width = "100%"
              ),

              p(class = "text-muted small",
                "If checked, the app will install any packages the student scripts need that are missing."),

              checkboxInput(
                inputId = "continue_on_error",
                label = "Continue past errors in student code",
                value = TRUE,
                width = "100%"
              ),

              p(class = "text-muted small",
                "If checked, the rest of a student's code still runs after an error, so the ",
                "remaining objects are tested. Unchecked, a script stops at its first error and is marked FAIL."),

              div(class = "divider"),

              actionButton("run_autoharp_grading", "Run Automated Grading",
                           class = "btn-primary",
                           style = "width: 100%;",
                           icon = icon("play")),

              br(), br(),

              downloadButton("download_autoharp_excel", "Export Results (Excel)",
                             class = "btn-success",
                             style = "width: 100%")
          )
        ),

        main_column(
          # Status messages
          uiOutput("autoharp_status"),

          div(class = "panel-section",
              h4(icon("table"), " Grading Results"),
              p("Evaluation results from the automated checks."),
              DTOutput("autoharp_results"),
              uiOutput("plots_detected")
          )
        )
      )
    ),

    # Tab 3: Plot Grading
    tabPanel(
      "Plot Grading",
      value = "plots",

      br(),

      fluidRow(
        sidebar_column(
          div(class = "sidebar-panel",
              h4(icon("user"), " Student Selection"),

              uiOutput("student_selector"),

              fluidRow(
                column(6, actionButton("prev_student", "Previous", icon = icon("arrow-left"),
                                       class = "btn-secondary",
                                       style = "width: 100%;")),
                column(6, actionButton("next_student", "Next", icon = icon("arrow-right"),
                                       class = "btn-secondary",
                                       style = "width: 100%;"))
              ),

              div(class = "divider"),

              h5(icon("star"), " Plot Grading"),

              numericInput("plot_grade", "Plot Mark:",
                           value = NA,
                           width = "100%"),

              textAreaInput("plot_comments", "Comments:",
                            rows = 3,
                            placeholder = "Optional feedback...",
                            width = "100%"),

              actionButton("save_grade", "Save Plot Grade",
                           class = "btn-success",
                           style = "width: 100%;",
                           icon = icon("save")),

              div(class = "divider"),

              h5(icon("tasks"), " Progress"),
              uiOutput("grading_progress")
          )
        ),

        main_column(
          div(class = "panel-section",
              div(class = "panel-heading-row",
                  h4(icon("image"), " Student Plot"),
                  div(class = "plot-nav",
                      actionButton("prev_plot", NULL, icon = icon("chevron-left"),
                                   class = "btn-secondary btn-sm"),
                      span(class = "plot-counter", textOutput("plot_counter", inline = TRUE)),
                      actionButton("next_plot", NULL, icon = icon("chevron-right"),
                                   class = "btn-secondary btn-sm")
                  )
              ),
              uiOutput("plot_display")
          ),

          div(class = "panel-section",
              h4(icon("clipboard-check"), " Test Results for Selected Student"),
              p("All autoharp test results for the currently selected student."),
              uiOutput("student_run_summary"),
              DTOutput("student_test_results")
          ),

          div(class = "panel-section",
              h4(icon("table"), " Consolidated Grading Results"),
              p("Combined results from the automated checks and manual plot grading."),
              DTOutput("consolidated_results"),
              div(class = "action-row",
                  downloadButton("download_consolidated_excel", "Export Results (Excel)",
                                 class = "btn-success")
              )
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
      tags$button(class = "plot-modal-close", onclick = "closePlotModal()", HTML("&times;")),
      tags$h4(id = "modalPlotTitle", style = "margin-bottom: 15px; color: #495057;"),
      tags$img(id = "modalPlotImg", src = "", alt = "Expanded Plot")
    )
  )
)


# =========================================
# Server
# =========================================

server <- function(input, output, session) {

  # knitr figure folders used when knitting the model solution and grading
  # template, so that no stray figure/ folders are created in the app folder
  KNITR_DEFAULT_FIG_PATH <- "figure/"
  KNITR_SOLN_FIG_DIR     <- file.path(tempdir(), "soln_figs")

  rv <- reactiveValues(
    # template generation
    solution_name = NULL,
    solution_content = NULL,
    solution_ext = NULL,
    solution_env = NULL,
    solution_objects = NULL,
    solution_errors = character(0),
    solution_run_dir = NULL,
    default_selection = NULL,
    generated_template = NULL,
    template_objects = NULL,
    # autoharp grading
    autoharp_template = NULL,   # list(name, content, source)
    autoharp_results = NULL,
    student_files = NULL,
    student_plots = list(),
    output_dir = NULL,
    # plot grading
    plot_grades = NULL,
    current_student_index = 1,
    plot_index = 1,
    # session management
    session_id = NULL,
    last_save_time = NULL,
    last_save_file = NULL,
    last_save_kind = NULL,
    pending_session_path = NULL
  )

  # Non-reactive state: values last copied from Template Generation to the
  # Autoharp Grading inputs, and whether the plot grade inputs were edited
  sync_state <- new.env()
  sync_state$run <- ""
  sync_state$prefix <- "test"
  grade_state <- new.env()
  grade_state$dirty <- FALSE

  # =========================================
  # Shared helpers
  # =========================================

  # Knit the model solution in a fresh environment, running it from run_dir
  # (NULL = knitr's default, the temp folder the file is written to)
  knit_solution <- function(content, ext, run_dir) {
    env <- new.env()
    soln_file <- tempfile(fileext = ext)
    temp_md <- tempfile(fileext = ".md")
    writeLines(content, soln_file)
    old_fig_path <- knitr::opts_chunk$get("fig.path")
    old_root_dir <- knitr::opts_knit$get("root.dir")
    knitr::opts_chunk$set(fig.path = file.path(KNITR_SOLN_FIG_DIR, "fig-"))
    knitr::opts_knit$set(root.dir = run_dir)
    errors <- character(0)
    tryCatch({
      knitr::knit(soln_file, output = temp_md, envir = env, quiet = TRUE)
      errors <- extract_knit_errors(readLines(temp_md, warn = FALSE))
    }, finally = {
      knitr::opts_chunk$set(
        fig.path = if (!is.null(old_fig_path)) old_fig_path else KNITR_DEFAULT_FIG_PATH)
      knitr::opts_knit$set(root.dir = old_root_dir)
      unlink(c(soln_file, temp_md))
    })
    list(env = env, errors = errors)
  }

  # Folder the model solution is run from in Template Generation (NULL when
  # none is set: the solution then runs in a temporary folder)
  solution_run_dir <- function(run_folder) {
    existing_folder(run_folder)
  }

  # Generate the grading template from the current object selection
  build_template <- function() {
    selected <- isolate(input$selected_objects)
    if (length(selected) == 0) stop("Select at least one object to test.")
    prefix <- isolate(input$test_prefix)
    if (is_blank(prefix)) prefix <- "test"
    template <- generate_autoharp_template(
      original_content = rv$solution_content,
      solution_env = rv$solution_env,
      objects_to_test = selected,
      test_prefix = trimws(prefix),
      include_all_tests = isTRUE(isolate(input$include_all_tests))
    )
    rv$generated_template <- template
    rv$template_objects <- selected
    template
  }

  generated_template_name <- function() {
    base <- if (!is.null(rv$solution_name)) tools::file_path_sans_ext(rv$solution_name) else "solution"
    ext <- if (!is.null(rv$solution_ext)) rv$solution_ext else ".Rmd"
    paste0(base, "_autoharp", ext)
  }

  # Folder where session files are saved: the last grading output folder,
  # otherwise the output folder implied by the current inputs (or NULL)
  current_output_dir <- function() {
    if (!is.null(rv$output_dir)) return(rv$output_dir)
    resolve_output_dir(input$autoharp_output_folder, input$autoharp_student_folder)
  }

  # =========================================
  # Session save / load
  # =========================================

  session_payload <- function() {
    if (is.null(rv$session_id)) {
      rv$session_id <- format(Sys.time(), "session_%Y%m%d_%H%M%S")
    }
    list(
      app = "autoharp_grading_app",
      version = 2,
      session_id = rv$session_id,
      timestamp = Sys.time(),
      # template generation
      solution_name = rv$solution_name,
      solution_content = rv$solution_content,
      solution_ext = rv$solution_ext,
      solution_objects = rv$solution_objects,
      selected_objects = input$selected_objects,
      generated_template = rv$generated_template,
      tg_run_folder = input$tg_run_folder,
      test_prefix = input$test_prefix,
      include_all_tests = input$include_all_tests,
      # autoharp grading
      autoharp_template = rv$autoharp_template,
      autoharp_student_folder = input$autoharp_student_folder,
      autoharp_run_folder = input$autoharp_run_folder,
      autoharp_output_folder = input$autoharp_output_folder,
      test_pattern = input$test_pattern,
      permission_to_install = input$permission_to_install,
      continue_on_error = input$continue_on_error,
      autoharp_results = rv$autoharp_results,
      student_files = rv$student_files,
      student_plots = rv$student_plots,
      output_dir = rv$output_dir,
      # plot grading
      plot_grades = rv$plot_grades,
      current_student_index = rv$current_student_index
    )
  }

  # Save the session to <output folder>/sessions; kind is "auto" or "manual"
  save_session_state <- function(kind = "auto") {
    out_dir <- current_output_dir()
    if (is.null(out_dir)) {
      return(list(success = FALSE, error = "no output folder"))
    }
    tryCatch({
      session_dir <- file.path(out_dir, "sessions")
      dir.create(session_dir, recursive = TRUE, showWarnings = FALSE)
      payload <- session_payload()
      session_file <- file.path(session_dir, paste0(payload$session_id, ".rds"))
      saveRDS(payload, session_file)
      rv$last_save_time <- Sys.time()
      rv$last_save_file <- session_file
      rv$last_save_kind <- kind
      list(success = TRUE, file = session_file)
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
  }

  # Restore a session saved by this app (older session files are mapped
  # onto the new fields where possible)
  load_session_state <- function(session_file) {
    tryCatch({
      s <- readRDS(session_file)
      if (!is.list(s)) stop("This file is not a grading app session.")
      pick <- function(...) {
        for (x in list(...)) if (!is.null(x)) return(x)
        NULL
      }

      rv$session_id <- s$session_id
      rv$last_save_time <- NULL
      rv$last_save_file <- NULL
      restore_state$until <- Sys.time() + 3

      # template generation
      rv$solution_name <- s$solution_name
      rv$solution_content <- pick(s$solution_content, s$solution_file_content)
      rv$solution_ext <- pick(s$solution_ext, s$solution_file_extension, ".Rmd")
      rv$generated_template <- s$generated_template
      rv$template_objects <- s$selected_objects
      rv$default_selection <- s$selected_objects
      rv$solution_env <- NULL
      rv$solution_errors <- character(0)
      rv$solution_objects <- NULL
      if (!is.null(rv$solution_content)) {
        # sessions saved by earlier versions ran the solution from their
        # Template Generation student folder when no run folder was set
        run_dir <- solution_run_dir(s$tg_run_folder)
        if (is.null(run_dir)) run_dir <- existing_folder(s$tg_student_folder)
        knitted <- knit_solution(rv$solution_content, rv$solution_ext, run_dir)
        rv$solution_env <- knitted$env
        rv$solution_errors <- knitted$errors
        rv$solution_run_dir <- run_dir
        rv$solution_objects <- pick(s$solution_objects,
                                    ls(knitted$env)[!grepl("^\\.", ls(knitted$env))])
      }

      # autoharp grading
      rv$autoharp_template <- s$autoharp_template
      rv$autoharp_results <- s$autoharp_results
      rv$student_files <- pick(s$student_files, s$autoharp_student_files)
      rv$student_plots <- pick(s$student_plots, list())
      rv$output_dir <- s$output_dir

      # plot grading
      rv$plot_grades <- s$plot_grades
      idx <- pick(s$current_student_index, 1)
      rv$current_student_index <- if (!is.null(rv$plot_grades) && idx <= nrow(rv$plot_grades)) idx else 1
      rv$plot_index <- 1

      # inputs; record the Template Generation values as already synced so
      # restoring them does not overwrite the restored Autoharp inputs
      sync_state$run <- pick(s$tg_run_folder, "")
      sync_state$prefix <- pick(s$test_prefix, "test")
      updateTextInput(session, "tg_run_folder", value = pick(s$tg_run_folder, ""))
      updateTextInput(session, "test_prefix", value = pick(s$test_prefix, "test"))
      updateCheckboxInput(session, "include_all_tests", value = pick(s$include_all_tests, TRUE))
      updateTextInput(session, "autoharp_student_folder", value = pick(s$autoharp_student_folder, ""))
      updateTextInput(session, "autoharp_run_folder", value = pick(s$autoharp_run_folder, ""))
      updateTextInput(session, "autoharp_output_folder", value = pick(s$autoharp_output_folder, "student_out"))
      updateTextInput(session, "test_pattern", value = pick(s$test_pattern, "test"))
      updateCheckboxInput(session, "permission_to_install", value = pick(s$permission_to_install, FALSE))
      updateCheckboxInput(session, "continue_on_error", value = pick(s$continue_on_error, TRUE))
      push_grade_inputs()

      list(success = TRUE, session_id = s$session_id)
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
  }

  output$autosave_status <- renderUI({
    if (is.null(rv$last_save_file)) {
      div(class = "autosave-status",
          icon("info-circle"),
          " Progress is auto-saved after grading and each plot grade")
    } else {
      label <- if (identical(rv$last_save_kind, "manual")) "Session saved:" else "Progress auto-saved:"
      div(class = "autosave-status", title = rv$last_save_file,
          icon("check-circle"),
          paste0(" ", label, " ", basename(rv$last_save_file), " (",
                 format(rv$last_save_time, "%H:%M:%S"), ")"))
    }
  })

  # Current session details (as in the original Session tab)
  session_info_ui <- function() {
    if (is.null(rv$session_id)) {
      return(div(class = "status-alert status-info",
                 icon("info-circle"), " No active session. Start grading to create a new session."))
    }
    div(class = "well session-info",
        h5(icon("info-circle"), " Current Session Info"),
        tags$p(tags$strong("Session ID: "), rv$session_id),
        tags$p(tags$strong("Last Saved: "),
               if (!is.null(rv$last_save_time)) format(rv$last_save_time, "%Y-%m-%d %H:%M:%S") else "Not yet saved"),
        if (!is.null(rv$last_save_file)) tags$p(tags$strong("Saved To: "), tags$code(rv$last_save_file)),
        tags$p(tags$strong("Students: "), if (!is.null(rv$student_files)) length(rv$student_files) else 0),
        tags$p(tags$strong("Graded: "), if (!is.null(rv$plot_grades)) sum(rv$plot_grades$graded) else 0)
    )
  }

  observeEvent(input$save_session_btn, {
    result <- save_session_state(kind = "manual")
    if (result$success) {
      showNotification(paste0("Session saved to: ", result$file), type = "message", duration = 6)
    } else if (identical(result$error, "no output folder")) {
      showModal(modalDialog(
        title = tagList(icon("save"), " Save Session"),
        p("The session can't be saved to an output folder yet, because no student scripts ",
          "folder (or absolute output directory) has been set."),
        p("You can download the session file instead and load it later."),
        session_info_ui(),
        footer = tagList(
          modalButton("Cancel"),
          downloadButton("download_session", "Download Session (.rds)", class = "btn-success")
        ),
        easyClose = TRUE
      ))
    } else {
      showNotification(paste0("Error saving session: ", result$error), type = "error", duration = 10)
    }
  })

  output$download_session <- downloadHandler(
    filename = function() {
      paste0(if (!is.null(rv$session_id)) rv$session_id else format(Sys.time(), "session_%Y%m%d_%H%M%S"), ".rds")
    },
    content = function(file) {
      saveRDS(session_payload(), file)
    }
  )

  observeEvent(input$load_session_btn, {
    rv$pending_session_path <- NULL
    showModal(modalDialog(
      title = tagList(icon("folder-open"), " Load Previous Session"),
      p("Resume a previous grading session by uploading a session file. Session files are saved in the ",
        tags$code("sessions"), " folder inside the grading output directory."),
      fileInput("session_file", "Upload Session File (.rds):", accept = ".rds", width = "100%"),
      uiOutput("session_file_status"),
      session_info_ui(),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_load_session", "Load Session", icon = icon("upload"),
                     class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$session_file, {
    rv$pending_session_path <- input$session_file$datapath
  })

  output$session_file_status <- renderUI({
    req(rv$pending_session_path, input$session_file)
    div(class = "status-alert status-info",
        icon("info-circle"), paste(" Selected:", input$session_file$name))
  })

  observeEvent(input$confirm_load_session, {
    if (is.null(rv$pending_session_path)) {
      showNotification("Choose a session file first.", type = "warning")
      return()
    }
    result <- load_session_state(rv$pending_session_path)
    removeModal()
    if (result$success) {
      showNotification(paste0("Session loaded successfully! ID: ", result$session_id),
                       type = "message", duration = 5)
    } else {
      showNotification(paste0("Error loading session: ", result$error), type = "error", duration = 10)
    }
  })

  # =========================================
  # Folder browsers
  # =========================================

  if (SHINYFILES_AVAILABLE) {
    # Allow browsing from root directory (/) for Unix-like systems
    # or from all available volumes for Windows
    roots <- if (.Platform$OS.type == "windows") {
      shinyFiles::getVolumes()()
    } else {
      c(root = "/", home = path.expand("~"))
    }
    folder_buttons <- c(
      tg_run_folder_select = "tg_run_folder",
      autoharp_student_folder_select = "autoharp_student_folder",
      autoharp_run_folder_select = "autoharp_run_folder"
    )
    lapply(names(folder_buttons), function(button_id) {
      shinyFiles::shinyDirChoose(input, button_id, roots = roots, session = session)
      observeEvent(input[[button_id]], {
        if (!is.integer(input[[button_id]])) {
          selected_path <- shinyFiles::parseDirPath(roots, input[[button_id]])
          if (length(selected_path) > 0) {
            updateTextInput(session, folder_buttons[[button_id]], value = selected_path)
          }
        }
      })
    })
  }

  # =========================================
  # Template Generation -> Autoharp Grading sync
  # =========================================

  # Copy a Template Generation value to its Autoharp Grading input, unless the
  # Autoharp input was changed by hand (it no longer matches the last copy)
  sync_to_autoharp <- function(tg_id, autoharp_id, key) {
    observeEvent(input[[tg_id]], {
      new_value <- input[[tg_id]]
      old_value <- sync_state[[key]]
      if (identical(new_value, old_value)) return()
      autoharp_value <- input[[autoharp_id]]
      if (is_blank(autoharp_value) || identical(autoharp_value, old_value)) {
        updateTextInput(session, autoharp_id, value = new_value)
      }
      sync_state[[key]] <- new_value
    }, ignoreInit = TRUE)
  }
  sync_to_autoharp("tg_run_folder", "autoharp_run_folder", "run")
  sync_to_autoharp("test_prefix", "test_pattern", "prefix")

  # =========================================
  # Folder checks: live status lines and "Found N student files" messages
  # =========================================

  # Folder inputs, debounced so checks run once typing pauses
  folder_values <- list(
    autoharp_student_folder = debounce(reactive(input$autoharp_student_folder), 600),
    tg_run_folder = debounce(reactive(input$tg_run_folder), 600),
    autoharp_run_folder = debounce(reactive(input$autoharp_run_folder), 600)
  )

  # Status line under a student scripts folder input
  student_folder_status <- function(value) {
    if (is_blank(value)) return(NULL)
    info <- folder_info(value)
    if (!info$exists) {
      div(class = "folder-status folder-error",
          icon("times-circle"), " Folder does not exist",
          div(class = "folder-path", info$path))
    } else if (length(info$files) == 0) {
      div(class = "folder-status folder-warning",
          icon("exclamation-triangle"), " No R, Rmd, or QMD files found in this folder",
          div(class = "folder-path", info$path))
    } else {
      n <- length(info$files)
      div(class = "folder-status folder-ok",
          icon("check-circle"),
          paste0(" Found ", n, " student file", if (n != 1) "s", " (", file_type_summary(info$files), ")"),
          div(class = "folder-path", info$path))
    }
  }

  # Status line under a "run from" folder input; blank_text explains what
  # happens when the field is left empty
  run_folder_status <- function(value, blank_text) {
    if (is_blank(value)) {
      return(div(class = "folder-status folder-muted",
                 icon("info-circle"), paste0(" ", blank_text)))
    }
    info <- folder_info(value)
    if (!info$exists) {
      div(class = "folder-status folder-error",
          icon("times-circle"), " Folder does not exist",
          div(class = "folder-path", info$path))
    } else {
      div(class = "folder-status folder-ok",
          icon("check-circle"), " Folder found",
          div(class = "folder-path", info$path))
    }
  }

  output$autoharp_student_folder_status <- renderUI(student_folder_status(folder_values$autoharp_student_folder()))
  output$tg_run_folder_status <- renderUI(run_folder_status(
    folder_values$tg_run_folder(),
    "No folder set: the solution runs in a temporary folder (fine if it reads no data files)"))
  output$autoharp_run_folder_status <- renderUI(run_folder_status(
    folder_values$autoharp_run_folder(), "Using the student scripts folder"))

  # Messages are not shown for values restored from a session
  restore_state <- new.env()
  restore_state$until <- Sys.time() - 1

  notify_student_folder <- function(value) {
    if (is_blank(value) || Sys.time() < restore_state$until) return(invisible())
    info <- folder_info(value)
    if (!info$exists) {
      showNotification("Folder does not exist", type = "error")
    } else if (length(info$files) == 0) {
      showNotification("No R, Rmd, or QMD files found in the specified folder", type = "warning")
    } else {
      showNotification(paste("Found", length(info$files), "student files"), type = "message")
    }
  }

  observeEvent(folder_values$autoharp_student_folder(), {
    notify_student_folder(folder_values$autoharp_student_folder())
  }, ignoreInit = TRUE)

  # =========================================
  # Tab 1: Template Generation
  # =========================================

  observeEvent(input$solution_file, {
    file_ext <- tolower(tools::file_ext(input$solution_file$name))
    rv$solution_name <- input$solution_file$name
    rv$solution_ext <- if (file_ext == "qmd") ".qmd" else ".Rmd"
    rv$solution_content <- readLines(input$solution_file$datapath, warn = FALSE)
    # a new solution needs to be executed again
    rv$solution_env <- NULL
    rv$solution_objects <- NULL
    rv$solution_errors <- character(0)
    rv$generated_template <- NULL
    rv$template_objects <- NULL
    showNotification(paste0("Solution template loaded successfully! Click 'Generate Template' to run ",
                            input$solution_file$name, "."),
                     type = "message")
  })

  observeEvent(input$generate_template, {
    if (is.null(rv$solution_content)) {
      showNotification("Upload a model solution first.", type = "warning")
      return()
    }
    if (!is_blank(input$tg_run_folder) && is.null(existing_folder(input$tg_run_folder))) {
      showNotification(paste("Folder not found:", input$tg_run_folder), type = "error")
      return()
    }
    run_dir <- solution_run_dir(input$tg_run_folder)

    withProgress(message = "Running model solution...", value = 0.3, {
      tryCatch({
        knitted <- knit_solution(rv$solution_content, rv$solution_ext, run_dir)
        objects <- ls(knitted$env)
        objects <- objects[!grepl("^\\.", objects)]
        if (length(objects) == 0) stop("The model solution did not create any objects.")

        # keep objects the user unticked earlier unticked; select all others
        unticked <- setdiff(rv$solution_objects, input$selected_objects)
        rv$default_selection <- setdiff(objects, unticked)
        rv$solution_env <- knitted$env
        rv$solution_errors <- knitted$errors
        rv$solution_run_dir <- run_dir
        rv$solution_objects <- objects

        incProgress(0.5, detail = "Generating template...")
        rv$generated_template <- generate_autoharp_template(
          original_content = rv$solution_content,
          solution_env = knitted$env,
          objects_to_test = rv$default_selection,
          test_prefix = if (is_blank(input$test_prefix)) "test" else trimws(input$test_prefix),
          include_all_tests = isTRUE(input$include_all_tests)
        )
        rv$template_objects <- rv$default_selection

        if (length(knitted$errors) > 0) {
          showNotification("Template generated, but the model solution had errors. See the Setup panel.",
                           type = "warning", duration = 8)
        } else {
          showNotification(paste0("Template generated successfully! ", length(objects),
                                  " objects found in the model solution."),
                           type = "message")
        }
      }, error = function(e) {
        showNotification(paste("Error loading solution:", e$message), type = "error", duration = 10)
      })
    })
  })

  output$solution_status <- renderUI({
    if (is.null(rv$solution_content)) return(NULL)
    run_dir_text <- if (!is.null(rv$solution_run_dir)) rv$solution_run_dir else "a temporary folder (no folder set)"
    if (is.null(rv$solution_objects)) {
      return(div(class = "status-alert status-info",
                 icon("info-circle"), " ", tags$strong(rv$solution_name),
                 " uploaded. Click Generate Template to run it."))
    }
    if (length(rv$solution_errors) > 0) {
      div(class = "status-alert status-warning",
          tags$strong(icon("exclamation-triangle"),
                      paste0(" ", length(rv$solution_errors), " error(s) while running the model solution")),
          if (is.null(rv$solution_run_dir)) {
            p("Objects that depend on the failing code are missing. No 'Run Model Solution From' folder ",
              "is set, so the solution ran in a temporary folder: if it reads data files, set that folder ",
              "and click Generate Template again.")
          } else {
            tagList(
              p("Objects that depend on the failing code are missing. Check that the data paths work from:"),
              tags$code(run_dir_text))
          },
          tags$ul(lapply(utils::head(rv$solution_errors, 3), tags$li))
      )
    } else {
      div(class = "status-alert status-success",
          icon("check-circle"), " ", tags$strong(rv$solution_name),
          " ran without errors from ", tags$code(run_dir_text))
    }
  })

  output$template_ready <- reactive(!is.null(rv$solution_objects))
  outputOptions(output, "template_ready", suspendWhenHidden = FALSE)

  output$objects_panel <- renderUI({
    if (is.null(rv$solution_objects)) {
      return(div(class = "status-alert status-info",
                 icon("info-circle"),
                 " Upload a model solution and click 'Generate Template' to list the objects it creates."))
    }
    fluidRow(
      column(4,
             h5(icon("list-check"), " Objects"),
             div(class = "object-checkbox-group",
                 checkboxGroupInput("selected_objects", NULL,
                                    choices = rv$solution_objects,
                                    selected = isolate(rv$default_selection))
             ),
             div(class = "select-links",
                 actionLink("select_all_objects", "Select all"), " | ",
                 actionLink("select_no_objects", "Select none"))
      ),
      column(8,
             h5(icon("clipboard-check"), " Suggested Tests (for selected objects)"),
             uiOutput("suggested_tests"))
    )
  })

  observeEvent(input$select_all_objects, {
    updateCheckboxGroupInput(session, "selected_objects", selected = rv$solution_objects)
  })
  observeEvent(input$select_no_objects, {
    updateCheckboxGroupInput(session, "selected_objects", selected = character(0))
  })

  output$suggested_tests <- renderUI({
    req(rv$solution_env)
    selected <- input$selected_objects
    if (length(selected) == 0) {
      return(p(class = "text-muted", "No objects selected."))
    }
    rows <- lapply(selected, function(obj_name) {
      obj <- get(obj_name, envir = rv$solution_env)
      tags$tr(
        tags$td(tags$strong(obj_name)),
        tags$td(class(obj)[1]),
        tags$td(paste(suggested_tests(obj, obj_name, isTRUE(input$include_all_tests)), collapse = ", "))
      )
    })
    div(class = "suggested-tests",
        tags$table(class = "table suggested-tests-table",
                   tags$thead(tags$tr(tags$th("Object"), tags$th("Type"), tags$th("Tests"))),
                   tags$tbody(rows)))
  })

  observeEvent(input$update_template, {
    tryCatch({
      build_template()
      showNotification("Template generated successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error generating template:", e$message), type = "error")
    })
  })

  output$template_status <- renderUI({
    if (is.null(rv$solution_content)) {
      div(class = "status-alert status-warning",
          icon("exclamation-triangle"),
          " Please upload a model solution and click 'Generate Template' first.")
    } else if (is.null(rv$generated_template)) {
      div(class = "status-alert status-info",
          icon("info-circle"),
          " Click 'Generate Template' to create an autoharp-compatible template.")
    } else if (!setequal(rv$template_objects, input$selected_objects)) {
      div(class = "status-alert status-info",
          icon("info-circle"),
          " The object selection has changed. Click 'Update Template' to refresh the preview ",
          "(downloads always use the current selection).")
    } else {
      div(class = "status-alert status-success",
          icon("check-circle"),
          paste0(" Template generated successfully with tests for ", length(rv$template_objects),
                 " object(s)! You can preview it below and download it."))
    }
  })

  output$template_preview <- renderText({
    req(rv$generated_template)
    paste(rv$generated_template, collapse = "\n")
  })

  output$download_template <- downloadHandler(
    filename = function() generated_template_name(),
    content = function(file) {
      writeLines(build_template(), file)
    }
  )

  observeEvent(input$use_template, {
    tryCatch({
      template <- build_template()
      rv$autoharp_template <- list(name = generated_template_name(), content = template,
                                   source = "generated")
      if (!is_blank(input$tg_run_folder)) {
        updateTextInput(session, "autoharp_run_folder", value = input$tg_run_folder)
        sync_state$run <- input$tg_run_folder
      }
      prefix <- if (is_blank(input$test_prefix)) "test" else trimws(input$test_prefix)
      updateTextInput(session, "test_pattern", value = prefix)
      sync_state$prefix <- input$test_prefix
      updateTabsetPanel(session, "main_tabs", selected = "autoharp")
      showNotification("Template loaded into Autoharp Grading.", type = "message")
    }, error = function(e) {
      showNotification(e$message, type = "error")
    })
  })

  # =========================================
  # Tab 2: Autoharp Grading
  # =========================================

  observeEvent(input$autoharp_template_file, {
    if (!grepl("\\.(rmd|qmd)$", tolower(input$autoharp_template_file$name))) {
      showNotification("Template file must be an RMD or QMD file.", type = "error")
      return()
    }
    rv$autoharp_template <- list(
      name = input$autoharp_template_file$name,
      content = readLines(input$autoharp_template_file$datapath, warn = FALSE),
      source = "upload"
    )
    showNotification(paste("Grading template loaded:", input$autoharp_template_file$name), type = "message")
  })

  output$autoharp_template_source <- renderUI({
    tmpl <- rv$autoharp_template
    if (is.null(tmpl)) {
      p(class = "text-muted small template-source",
        "Upload a template, or use one from the Template Generation tab.")
    } else {
      source_text <- if (identical(tmpl$source, "generated")) "generated in Template Generation" else "uploaded"
      div(class = "template-source template-source-active",
          icon("check-circle"), " Using ", tags$strong(tmpl$name),
          tags$span(class = "text-muted", paste0(" (", source_text, ")")))
    }
  })

  output$output_dir_hint <- renderUI({
    out_dir <- resolve_output_dir(input$autoharp_output_folder, input$autoharp_student_folder)
    if (is.null(out_dir)) {
      p(class = "text-muted small",
        "Rendered output, plots and session files are saved next to the student scripts folder.")
    } else {
      p(class = "text-muted small",
        "Rendered output, plots and session files will be saved to: ", tags$code(out_dir))
    }
  })

  observeEvent(input$run_autoharp_grading, {
    tmpl <- rv$autoharp_template
    student_folder <- existing_folder(input$autoharp_student_folder)
    if (is.null(tmpl)) {
      showNotification("Upload a grading template, or use one from Template Generation.", type = "error")
      return()
    }
    if (is.null(student_folder)) {
      showNotification(paste("Student scripts folder not found:",
                             if (is_blank(input$autoharp_student_folder)) "(not set)" else resolve_path(input$autoharp_student_folder)),
                       type = "error")
      return()
    }
    if (!is_blank(input$autoharp_run_folder) && is.null(existing_folder(input$autoharp_run_folder))) {
      showNotification(paste("Folder not found:", input$autoharp_run_folder), type = "error")
      return()
    }
    student_files <- list_student_files(student_folder)
    if (length(student_files) == 0) {
      showNotification(paste("No R, Rmd, or QMD files found in folder:", student_folder), type = "error")
      return()
    }
    run_dir <- if (!is_blank(input$autoharp_run_folder)) existing_folder(input$autoharp_run_folder) else student_folder
    out_dir <- resolve_output_dir(input$autoharp_output_folder, student_folder)
    pattern <- if (is_blank(input$test_pattern)) "test" else trimws(input$test_pattern)

    withProgress(message = "Running autoharp grading...", value = 0, {
      tryCatch({
        dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
        out_dir <- normalizePath(out_dir, winslash = "/", mustWork = TRUE)

        # write the template under its own name so render logs stay readable
        template_dir <- tempfile("template_")
        dir.create(template_dir)
        template_path <- file.path(template_dir, tmpl$name)
        writeLines(tmpl$content, template_path)

        incProgress(0.1, detail = "Loading template...")
        old_root_dir <- knitr::opts_knit$get("root.dir")
        old_fig_path <- knitr::opts_chunk$get("fig.path")
        knitr::opts_chunk$set(fig.path = file.path(KNITR_SOLN_FIG_DIR, "fig-"))
        s_env <- tryCatch({
          populate_soln_env(template_path, pattern = pattern, knit_root_dir = run_dir)
        }, error = function(e) {
          stop(paste0("Failed to process the grading template: ", e$message,
                      ". Check that it contains test chunks matching the pattern '", pattern, "'."))
        }, finally = {
          knitr::opts_knit$set(root.dir = old_root_dir)
          knitr::opts_chunk$set(
            fig.path = if (!is.null(old_fig_path)) old_fig_path else KNITR_DEFAULT_FIG_PATH)
        })

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

        continue_on_error <- isTRUE(input$continue_on_error)
        progress_per_student <- 0.75 / length(student_files)
        corr_out <- lapply(student_files, function(student_file) {
          incProgress(progress_per_student, detail = paste("Processing", basename(student_file)))
          graded_file <- student_file
          result <- tryCatch({
            # grade a copy that keeps running after errors, still run from the
            # student's own folder so relative paths work as before
            if (continue_on_error) graded_file <- allow_errors_copy(student_file)
            result <- render_one(
              rmd_name = graded_file,
              out_dir = out_dir,
              knit_root_dir = dirname(student_file),
              soln_stuff = s_env,
              permission_to_install = isTRUE(input$permission_to_install)
            )
            if (!"error_message" %in% names(result)) {
              result$error_message <- NA_character_
            }
            result <- result[1, , drop = FALSE]
            result$fname <- student_file
            result
          }, error = function(e) {
            create_error_result(student_file, e$message)
          }, finally = {
            if (!identical(graded_file, student_file)) unlink(dirname(graded_file), recursive = TRUE)
          })
          errors <- student_code_errors(out_dir, student_file)
          result$code_errors <- length(errors)
          result$code_error_messages <- if (length(errors) > 0) paste(errors, collapse = " | ") else NA_character_
          result
        })

        # combine results, filling in columns missing for some students
        all_cols <- unique(unlist(lapply(corr_out, names)))
        corr_out <- lapply(corr_out, function(df) {
          for (col in setdiff(all_cols, names(df))) df[[col]] <- NA
          df[, all_cols, drop = FALSE]
        })
        combined_results <- do.call("rbind", corr_out)
        combined_results <- combined_results[!duplicated(combined_results$fname), , drop = FALSE]

        incProgress(0.1, detail = "Collecting student plots...")
        student_plots <- lapply(student_files, collect_student_plots, plots_output_dir = out_dir)
        names(student_plots) <- basename(student_files)

        # keep existing plot grades for students that are graded again
        plot_grades <- data.frame(
          student_file = basename(student_files),
          grade = NA_real_,
          comments = "",
          graded = FALSE,
          stringsAsFactors = FALSE
        )
        if (!is.null(rv$plot_grades)) {
          m <- match(plot_grades$student_file, rv$plot_grades$student_file)
          keep <- !is.na(m)
          plot_grades[keep, c("grade", "comments", "graded")] <-
            rv$plot_grades[m[keep], c("grade", "comments", "graded")]
        }

        rv$autoharp_results <- combined_results
        rv$student_files <- student_files
        rv$student_plots <- student_plots
        rv$output_dir <- out_dir
        rv$plot_grades <- plot_grades
        rv$current_student_index <- 1
        rv$plot_index <- 1
        push_grade_inputs()

        save_result <- save_session_state(kind = "auto")
        showNotification(
          paste0("Autoharp grading complete!",
                 if (save_result$success) paste0(" Session auto-saved to: ", basename(save_result$file)) else ""),
          type = "message"
        )
      }, error = function(e) {
        showNotification(paste("Error during autoharp grading:", e$message), type = "error", duration = 10)
      })
    })
  })

  output$autoharp_status <- renderUI({
    if (is.null(rv$autoharp_results)) {
      return(div(class = "status-alert status-info",
                 icon("info-circle"),
                 " Choose a grading template and the student scripts folder, then click 'Run Automated Grading'."))
    }
    results <- rv$autoharp_results
    n_students <- nrow(results)
    n_success <- sum(results$run_status == "SUCCESS", na.rm = TRUE)
    n_with_errors <- if ("code_errors" %in% names(results)) {
      sum(results$run_status == "SUCCESS" & results$code_errors > 0, na.rm = TRUE)
    } else 0
    all_clean <- n_success == n_students && n_with_errors == 0
    div(class = if (all_clean) "status-alert status-success" else "status-alert status-warning",
        icon(if (all_clean) "check-circle" else "exclamation-triangle"),
        tags$strong(" Grading completed: "),
        paste0(n_students, " students processed, ", n_success, " successful",
               if (n_with_errors > 0) paste0(" (", n_with_errors, " with errors in their code)"),
               ", ", n_students - n_success, " failed."),
        if (n_with_errors > 0) {
          tags$div(class = "small", style = "margin-top: 4px;",
                   "Code after an error still ran, so the remaining objects were tested. ",
                   "See the Code Errors column.")
        })
  })

  # columns of autoharp results that hold test results (scalars)
  test_columns <- function(results) {
    setdiff(names(results), c("fname", "time_stamp", "run_status", "run_time", "run_mem",
                              "error", "error_message", "code_errors", "code_error_messages"))
  }

  # "1: object 'S' not found" style summary of the code errors per student
  # ("" when there were none, or for results from older sessions)
  code_error_summary <- function(results) {
    if (!"code_errors" %in% names(results)) return(rep("", nrow(results)))
    first <- sub(" \\|.*$", "", results$code_error_messages)
    ifelse(!is.na(results$code_errors) & results$code_errors > 0,
           paste0(results$code_errors, ": ", first,
                  ifelse(results$code_errors > 1, " ...", "")),
           "")
  }

  has_code_errors <- function(results) {
    "code_errors" %in% names(results) && any(results$code_errors > 0, na.rm = TRUE)
  }

  # shorter column names for the on-screen tables (Excel keeps the full names)
  display_names <- function(df) {
    names(df) <- sub("_check$", "", names(df))
    df
  }

  output$autoharp_results <- renderDT({
    req(rv$autoharp_results)
    results <- rv$autoharp_results
    display_df <- data.frame(
      Student = basename(results$fname),
      Status = results$run_status,
      stringsAsFactors = FALSE
    )
    if ("run_time" %in% names(results)) display_df$`Run Time (s)` <- round(results$run_time, 2)
    if ("run_mem" %in% names(results)) display_df$`Memory Used` <- results$run_mem
    for (col in test_columns(results)) display_df[[col]] <- results[[col]]
    if ("error_message" %in% names(results) && any(!is.na(results$error_message))) {
      display_df$Error <- results$error_message
    }
    if (has_code_errors(results)) display_df$`Code Errors` <- code_error_summary(results)
    # Add plot grades once any student has been graded (as in the original app)
    pg <- rv$plot_grades
    if (!is.null(pg) && any(pg$graded)) {
      m <- match(display_df$Student, pg$student_file)
      graded <- !is.na(m) & pg$graded[m]
      display_df$`Plot Mark` <- ifelse(graded, pg$grade[m], NA)
      display_df$`Plot Comments` <- ifelse(graded, pg$comments[m], NA)
    }
    datatable(
      display_names(display_df),
      options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
      rownames = FALSE,
      class = "cell-border stripe hover"
    ) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(c("SUCCESS", "FAIL", "UNKNOWN"),
                                               c("#d4edda", "#f8d7da", "#fff3cd")))
  })

  output$plots_detected <- renderUI({
    req(rv$autoharp_results)
    n_plots <- lengths(rv$student_plots)
    if (sum(n_plots) == 0) {
      return(div(class = "status-alert status-info plots-detected",
                 icon("image"), " No plots were detected in the student submissions."))
    }
    div(class = "status-alert status-info plots-detected",
        div(icon("image"), tags$strong(" Plots detected: "),
            paste0(sum(n_plots > 0), " students generated ", sum(n_plots), " plots.")),
        actionButton("go_plot_grading", "Go to Plot Grading", icon = icon("arrow-right"),
                     class = "btn-primary btn-sm")
    )
  })

  observeEvent(input$go_plot_grading, {
    updateTabsetPanel(session, "main_tabs", selected = "plots")
  })

  # Combined autoharp results and plot grades; test results shown as PASS/FAIL
  consolidated_results <- function(pass_fail = TRUE, details = FALSE) {
    results <- rv$autoharp_results
    df <- data.frame(Student = basename(results$fname), Status = results$run_status,
                     stringsAsFactors = FALSE)
    if (details) {
      df$run_time <- results$run_time
      df$run_mem <- results$run_mem
    }
    for (col in test_columns(results)) {
      values <- results[[col]]
      if (pass_fail && is.logical(values)) {
        values <- ifelse(is.na(values), NA, ifelse(values, "PASS", "FAIL"))
      }
      df[[col]] <- values
    }
    if (details && "error_message" %in% names(results)) df$error_message <- results$error_message
    if (details && "code_errors" %in% names(results)) {
      df$code_errors <- results$code_errors
      df$code_error_messages <- results$code_error_messages
    } else if (!details && has_code_errors(results)) {
      df$`Code Errors` <- code_error_summary(results)
    }
    pg <- rv$plot_grades
    m <- if (!is.null(pg)) match(df$Student, pg$student_file) else rep(NA_integer_, nrow(df))
    df$`Plot Mark` <- if (!is.null(pg)) pg$grade[m] else NA
    df$Comments <- if (!is.null(pg)) pg$comments[m] else NA
    df$Graded <- if (!is.null(pg)) ifelse(!is.na(m) & pg$graded[m], "Yes", "No") else "No"
    df
  }

  make_excel_handler <- function() {
    downloadHandler(
      filename = function() {
        paste0("autoharp_grading_results_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
      },
      content = function(file) {
        if (!requireNamespace("openxlsx", quietly = TRUE)) {
          showNotification("Package 'openxlsx' is required for Excel export: install.packages('openxlsx')",
                           type = "error", duration = 10)
          stop("openxlsx package not available")
        }
        wb <- openxlsx::createWorkbook()
        openxlsx::addWorksheet(wb, "Grading Results")
        if (!is.null(rv$autoharp_results)) {
          openxlsx::writeData(wb, "Grading Results", consolidated_results(pass_fail = FALSE, details = TRUE))
        } else {
          openxlsx::writeData(wb, "Grading Results", data.frame(Message = "No grading results yet."))
        }
        openxlsx::addWorksheet(wb, "Plot Grades")
        if (!is.null(rv$plot_grades)) {
          sheet2 <- rv$plot_grades[, c("student_file", "grade", "comments", "graded")]
          names(sheet2) <- c("Student", "Plot Mark", "Comments", "Graded")
          openxlsx::writeData(wb, "Plot Grades", sheet2)
        } else {
          openxlsx::writeData(wb, "Plot Grades", data.frame(Message = "No plot grades yet."))
        }
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
      }
    )
  }
  output$download_autoharp_excel <- make_excel_handler()
  output$download_consolidated_excel <- make_excel_handler()

  # =========================================
  # Tab 3: Plot Grading
  # =========================================

  # Show the stored grade/comments of the current student in the inputs
  push_grade_inputs <- function() {
    pg <- isolate(rv$plot_grades)
    idx <- isolate(rv$current_student_index)
    grade_state$dirty <- FALSE
    if (is.null(pg) || idx < 1 || idx > nrow(pg)) return(invisible())
    updateNumericInput(session, "plot_grade", value = pg$grade[idx])
    updateTextAreaInput(session, "plot_comments", value = pg$comments[idx])
  }

  # Store the grade/comment inputs for student idx
  store_grade <- function(idx, mark_graded = TRUE) {
    pg <- rv$plot_grades
    grade <- input$plot_grade
    grade <- if (is.null(grade) || length(grade) == 0) NA_real_ else as.numeric(grade)
    comments <- if (is.null(input$plot_comments)) "" else input$plot_comments
    pg$grade[idx] <- grade
    pg$comments[idx] <- comments
    if (mark_graded || !is.na(grade) || nzchar(trimws(comments))) pg$graded[idx] <- TRUE
    rv$plot_grades <- pg
    grade_state$dirty <- FALSE
  }

  # Inputs edited by hand for the current student (compared with stored values)
  observeEvent(list(input$plot_grade, input$plot_comments), {
    pg <- rv$plot_grades
    idx <- rv$current_student_index
    if (is.null(pg) || idx < 1 || idx > nrow(pg)) return()
    grade <- input$plot_grade
    grade <- if (is.null(grade) || length(grade) == 0) NA_real_ else as.numeric(grade)
    comments <- if (is.null(input$plot_comments)) "" else input$plot_comments
    stored_grade <- pg$grade[idx]
    same_grade <- (is.na(grade) && is.na(stored_grade)) ||
      (!is.na(grade) && !is.na(stored_grade) && isTRUE(all.equal(grade, stored_grade)))
    grade_state$dirty <- !(same_grade && identical(comments, pg$comments[idx]))
  }, ignoreInit = TRUE, ignoreNULL = FALSE)

  # Switch student, keeping any edited grade/comments of the current one
  go_to_student <- function(idx) {
    pg <- rv$plot_grades
    if (is.null(pg) || idx < 1 || idx > nrow(pg)) return(invisible())
    if (isTRUE(grade_state$dirty)) {
      store_grade(rv$current_student_index, mark_graded = FALSE)
      save_session_state(kind = "auto")
    }
    rv$current_student_index <- idx
    rv$plot_index <- 1
    updateSelectInput(session, "current_student", selected = idx)
    push_grade_inputs()
  }

  output$student_selector <- renderUI({
    if (is.null(rv$student_files)) {
      return(p(class = "text-muted small", "Run autoharp grading first to list the students."))
    }
    selectInput("current_student", "Select Student:",
                choices = setNames(seq_along(rv$student_files), basename(rv$student_files)),
                selected = isolate(rv$current_student_index),
                width = "100%")
  })

  observeEvent(input$current_student, {
    idx <- as.integer(input$current_student)
    if (!identical(idx, as.integer(rv$current_student_index))) go_to_student(idx)
  })
  observeEvent(input$prev_student, go_to_student(rv$current_student_index - 1))
  observeEvent(input$next_student, go_to_student(rv$current_student_index + 1))

  observeEvent(input$save_grade, {
    if (is.null(rv$plot_grades)) {
      showNotification("Run autoharp grading first.", type = "warning")
      return()
    }
    idx <- rv$current_student_index
    store_grade(idx, mark_graded = TRUE)
    save_result <- save_session_state(kind = "auto")
    showNotification(
      if (save_result$success) "Grade saved and session auto-saved!" else "Grade saved!",
      type = "message"
    )
    if (idx < nrow(rv$plot_grades)) go_to_student(idx + 1)
  })

  current_plots <- reactive({
    req(rv$student_files)
    student_name <- basename(rv$student_files[rv$current_student_index])
    plots <- rv$student_plots[[student_name]]
    if (is.null(plots)) character(0) else plots
  })

  observeEvent(input$prev_plot, {
    if (rv$plot_index > 1) rv$plot_index <- rv$plot_index - 1
  })
  observeEvent(input$next_plot, {
    if (rv$plot_index < length(current_plots())) rv$plot_index <- rv$plot_index + 1
  })

  output$plot_counter <- renderText({
    n <- if (is.null(rv$student_files)) 0 else length(current_plots())
    if (n == 0) "No plots" else paste("Plot", min(rv$plot_index, n), "of", n)
  })

  output$plot_display <- renderUI({
    if (is.null(rv$student_files)) {
      return(div(class = "alert alert-warning",
                 icon("exclamation-triangle"),
                 " Run automated grading first to collect student plots."))
    }
    plots <- current_plots()
    if (length(plots) == 0) {
      return(div(class = "alert alert-info",
                 icon("info-circle"),
                 " No plots found for this student."))
    }
    plot_path <- plots[min(rv$plot_index, length(plots))]
    if (!file.exists(plot_path)) {
      return(div(class = "alert alert-warning",
                 icon("exclamation-triangle"), " Plot file not found: ", tags$code(plot_path),
                 tags$br(),
                 "The output folder may have been moved or deleted since grading. Re-run the grading to recreate the plots."))
    }
    ext <- tolower(tools::file_ext(plot_path))
    mime_type <- switch(ext, png = "image/png", jpg = "image/jpeg", jpeg = "image/jpeg",
                        gif = "image/gif", svg = "image/svg+xml", "image/png")
    img_src <- paste0("data:", mime_type, ";base64,", base64enc::base64encode(plot_path))
    dims <- if (ext == "png") png_dimensions(plot_path) else NULL
    student_name <- basename(rv$student_files[rv$current_student_index])
    plot_title <- paste0(student_name, " - Plot ", min(rv$plot_index, length(plots)))

    fluidRow(
      div(class = "col-lg-8",
             div(class = "plot-viewer",
                 tags$button(
                   class = "expand-icon",
                   `data-img-src` = img_src,
                   `data-plot-title` = plot_title,
                   onclick = "openPlotModalFromButton(this)",
                   title = "Expand plot",
                   icon("expand")
                 ),
                 tags$img(src = img_src, alt = plot_title))),
      div(class = "col-lg-4",
             div(class = "well plot-info",
                 h5(icon("info-circle"), " Plot Information"),
                 tags$table(class = "table table-condensed plot-info-table",
                            tags$tr(tags$th("File"), tags$td(class = "file-cell", basename(plot_path))),
                            tags$tr(tags$th("Size"),
                                    tags$td(if (!is.null(dims)) paste(dims[["width"]], "×", dims[["height"]], "px") else "-")),
                            tags$tr(tags$th("Generated"),
                                    tags$td(format(file.mtime(plot_path), "%Y-%m-%d %H:%M")))),
                 tags$button(class = "btn btn-secondary",
                             style = "width: 100%;",
                             `data-img-src` = img_src,
                             `data-plot-title` = plot_title,
                             onclick = "openPlotModalFromButton(this)",
                             icon("expand"), " View Full Size")))
    )
  })

  # autoharp results row of the current student (NULL if not graded)
  current_student_result <- reactive({
    req(rv$autoharp_results, rv$student_files)
    results <- rv$autoharp_results
    row <- match(basename(rv$student_files[rv$current_student_index]), basename(results$fname))
    if (is.na(row)) NULL else results[row, , drop = FALSE]
  })

  output$student_run_summary <- renderUI({
    if (is.null(rv$autoharp_results)) return(NULL)
    res <- current_student_result()
    if (is.null(res)) return(NULL)
    code_errors <- if (!is.null(res$code_errors) && !is.na(res$code_errors) && res$code_errors > 0) {
      strsplit(res$code_error_messages, " | ", fixed = TRUE)[[1]]
    } else character(0)
    clean <- identical(res$run_status, "SUCCESS") && length(code_errors) == 0
    div(class = paste("status-alert student-run-summary", if (clean) "status-success" else "status-warning"),
        tags$strong(basename(res$fname)), " - ", res$run_status,
        if (!is.null(res$run_time) && !is.na(res$run_time)) paste0(" | Run time: ", round(res$run_time, 2), " s"),
        if (!is.null(res$run_mem) && !is.na(res$run_mem)) paste0(" | Memory used: ", res$run_mem),
        if (!is.null(res$error_message) && !is.na(res$error_message)) tags$div(tags$code(res$error_message)),
        if (length(code_errors) > 0) {
          tags$div(style = "margin-top: 6px;",
                   tags$strong(icon("exclamation-triangle"),
                               paste0(" ", length(code_errors), " error(s) in the student's code"),
                               " (the rest of the code still ran):"),
                   tags$ul(lapply(utils::head(code_errors, 5), function(msg) tags$li(tags$code(msg)))))
        })
  })

  output$student_test_results <- renderDT({
    no_results <- function(msg) {
      datatable(data.frame(Message = msg), options = list(dom = "t"), rownames = FALSE)
    }
    if (is.null(rv$autoharp_results)) {
      return(no_results("No test results available for this student. Run automated grading first."))
    }
    res <- current_student_result()
    tests <- test_columns(rv$autoharp_results)
    if (is.null(res) || length(tests) == 0) {
      return(no_results("No test results available for this student."))
    }
    values <- lapply(tests, function(col) res[[col]])
    display_df <- data.frame(
      Test = sub("_check$", "", tests),
      Result = vapply(values, function(v) {
        if (is.logical(v)) { if (is.na(v)) "NA" else if (v) "PASS" else "FAIL" } else "INFO"
      }, character(1)),
      Value = vapply(values, function(v) format(v), character(1)),
      stringsAsFactors = FALSE
    )
    datatable(
      display_df,
      options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
      rownames = FALSE,
      selection = "none",
      class = "cell-border stripe hover"
    ) %>%
      formatStyle("Result",
                  backgroundColor = styleEqual(c("PASS", "FAIL", "NA", "INFO"),
                                               c("#d4edda", "#f8d7da", "#fff3cd", "#d1ecf1")))
  })

  output$grading_progress <- renderUI({
    pg <- rv$plot_grades
    if (is.null(pg)) return(p(class = "text-muted small", "No students to grade yet."))
    n_graded <- sum(pg$graded)
    n_total <- nrow(pg)
    pct <- if (n_total > 0) round(n_graded / n_total * 100) else 0
    tags$div(
      tags$p(class = "small", paste0("Graded: ", n_graded, " of ", n_total, " students (", pct, "%)")),
      tags$div(
        class = "progress",
        tags$div(
          class = "progress-bar",
          role = "progressbar",
          style = paste0("width: ", pct, "%;"),
          if (pct > 0) paste0(n_graded, " / ", n_total)
        )
      )
    )
  })

  output$consolidated_results <- renderDT({
    req(rv$autoharp_results)
    df <- display_names(consolidated_results(pass_fail = TRUE))
    tests <- sub("_check$", "", test_columns(rv$autoharp_results))
    table <- datatable(
      df,
      options = list(pageLength = 10, scrollX = TRUE, dom = "frtip"),
      rownames = FALSE,
      selection = "none",
      class = "cell-border stripe hover"
    ) %>%
      formatStyle("Status",
                  backgroundColor = styleEqual(c("SUCCESS", "FAIL", "UNKNOWN"),
                                               c("#d4edda", "#f8d7da", "#fff3cd")))
    if (length(tests) > 0) {
      table <- formatStyle(table, tests,
                           color = styleEqual(c("PASS", "FAIL"), c("#155724", "#721c24")),
                           fontWeight = styleEqual(c("PASS", "FAIL"), c("600", "600")))
    }
    table
  })
}

# Run the application
shinyApp(ui = ui, server = server)
