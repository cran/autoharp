library(shiny)
library(DT)
library(Rtsne)
library(stringr)
library(plotly)

# check if shinyFiles is available
# if (!requireNamespace("shinyFiles", quietly = TRUE)) {
#   message("Note: shinyFiles package not available. Folder selection buttons will not be functional.")
#   SHINYFILES_AVAILABLE <- FALSE
# } else {
#   library(shinyFiles)
#   SHINYFILES_AVAILABLE <- TRUE
# }

# source("helpers.R")
SHINYFILES_AVAILABLE <- TRUE

# helper function to check if a path is absolute
is_absolute_path <- function(path) {
  grepl("^([A-Za-z]:|/|\\\\\\\\|//)", path)
}

# helper function to normalize a path
normalize_path_safe <- function(path) {
  if (dir.exists(path)) {
    normalizePath(path, winslash = "/", mustWork = TRUE)
  } else {
    normalizePath(path, winslash = "/", mustWork = FALSE)
  }
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  div(class = "main-header",
    h2(icon("project-diagram"), " Script Similarity Analysis"),
    p("Explore similarity between student scripts (R, RMD, QMD) using t-SNE and MDS visualization")
  ),
  
  fluidRow(
    # left sidebar: Configuration
    column(
      width = 3,
      div(class = "sidebar-panel",
        h4(icon("cog"), " Configuration"),
        
        textInput(
          inputId = "scripts_folder",
          label = "Path to Scripts Folder:",
          value = "sol",
          placeholder = "sol",
          width = "100%"
        ),
        
        # add folder selection button if shinyFiles is available
        if (SHINYFILES_AVAILABLE) {
          shinyFiles::shinyDirButton("scripts_folder_select", "Browse...", 
                       "Select scripts folder",
                       class = "btn-secondary",
                       style = "width: 100%; margin-top: -10px;")
        } else {
          NULL
        },
        
        div(class = "divider"),
        
        h5(icon("ruler"), " Similarity Method"),
        selectInput(
          inputId = "similarity_method",
          label = NULL,
          choices = c(
            "Cosine (Token-based)" = "cosine",
            "Jaccard (Set-based)" = "jaccard",
            "Edit Distance" = "edit"
            # ,"AST (Tree-based)" = "ast"
          ),
          selected = "cosine",
          width = "100%"
        ),
        
        # cosine-specific options
        conditionalPanel(
          condition = "input.similarity_method == 'cosine'",
          sliderInput(
            inputId = "ngram_size",
            label = "N-gram Size:",
            min = 1,
            max = 5,
            value = 1,
            step = 1,
            width = "100%"
          ),
          checkboxInput(
            inputId = "include_actuals",
            label = "Include actual arguments/literals",
            value = TRUE,
            width = "100%"
          ),
          checkboxInput(
            inputId = "exclude_library_calls",
            label = "Exclude library function calls",
            value = TRUE,
            width = "100%"
          ),
          helpText("N-gram size: 1=unigrams, 2=bigrams, etc. Include actuals: whether to include literal values in token analysis. Exclude library calls: removes function calls from loaded packages to focus on code structure.")
        ),
        
        h5(icon("chart-scatter"), " Visualization Method"),
        selectInput(
          inputId = "viz_method",
          label = NULL,
          choices = c(
            "t-SNE" = "tsne",
            "MDS (Multi-Dimensional Scaling)" = "mds"
          ),
          selected = "tsne",
          width = "100%"
        ),
        
        # t-SNE specific options
        conditionalPanel(
          condition = "input.viz_method == 'tsne'",
          sliderInput(
            inputId = "tsne_perplexity",
            label = "t-SNE Perplexity:",
            min = 1,
            max = 50,
            value = 5,
            step = 1,
            width = "100%"
          )
        ),
        
        div(class = "divider"),
        
        h5(icon("filter"), " Clustering Options"),
        sliderInput(
          inputId = "cluster_threshold",
          label = "Similarity Threshold:",
          min = 0.5,
          max = 0.95,
          value = 0.7,
          step = 0.05,
          width = "100%"
        ),
        
        div(class = "divider"),
        
        actionButton("run_analysis", "Run Similarity Analysis", 
                    class = "btn-primary", 
                    style = "width: 100%;",
                    icon = icon("play")),
        
        br(), br(),
        
        downloadButton("download_results", "Download Results",
                      class = "btn-success",
                      style = "width: 100%")
      )
    ),
    
    # main content area
    column(
      width = 9,
      
      # status messages
      uiOutput("analysis_status"),
      
      # tabbed interface
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        
        # Tab 1: Similarity Analysis
        tabPanel(
          title = "Similarity Analysis",
          icon = icon("chart-line"),
          value = "similarity_tab",
          
          br(),
          
          # Summary statistics
          div(class = "panel-section",
            h4(icon("chart-bar"), " Summary Statistics"),
            uiOutput("summary_stats")
          ),
          
          # Main visualization
          div(class = "panel-section",
            h4(icon("project-diagram"), " Similarity Visualization"),
            p("2D projection of script similarities. Points closer together indicate more similar scripts. Click on points to view scripts (multiple files can be opened)."),
            plotlyOutput("similarity_plot", height = "500px", width = "100%")
          ),
          
          # Similarity matrix heatmap
          div(class = "panel-section",
            h4(icon("th"), " Similarity Matrix Heatmap"),
            plotOutput("heatmap_plot", height = "500px", width = "100%")
          ),
          
          # Top similar pairs
          div(class = "panel-section",
            h4(icon("list-ol"), " Top Similar Script Pairs"),
            p("Scripts with highest similarity scores."),
            DTOutput("similar_pairs_table")
          ),
          
          # Detected clusters
          div(class = "panel-section",
            h4(icon("layer-group"), " Detected Similarity Clusters"),
            p(paste("Clusters of scripts with similarity above the threshold.",
                    "These may indicate potential plagiarism or similar approaches.")),
            uiOutput("clusters_info")
          ),
          
          # Full similarity matrix
          div(class = "panel-section",
            h4(icon("table"), " Complete Similarity Matrix"),
            DTOutput("similarity_matrix_table")
          )
        ),
        
        # Tab 2: Diff View
        tabPanel(
          title = "Diff View",
          icon = icon("columns"),
          value = "diff_tab",
          
          br(),
          
          # Diff view controls
          div(class = "panel-section",
            h4(icon("columns"), " Side-by-Side Script Comparison"),
            p("Select two scripts to compare them line-by-line."),
            
            fluidRow(
              column(6,
                selectInput(
                  inputId = "diff_file1",
                  label = "First Script:",
                  choices = NULL,
                  width = "100%"
                )
              ),
              column(6,
                selectInput(
                  inputId = "diff_file2",
                  label = "Second Script:",
                  choices = NULL,
                  width = "100%"
                )
              )
            ),
            
            fluidRow(
              column(12,
                actionButton("run_diff", "Compare Scripts", 
                           class = "btn-primary",
                           icon = icon("exchange-alt"))
              )
            )
          ),
          
          # Diff output
          div(class = "panel-section",
            h4(icon("file-code"), " Line-by-Line Comparison"),
            uiOutput("diff_output")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Helper function to generate safe button IDs from file keys
  generate_btn_id <- function(file_key) {
    paste0("close_", gsub("[^a-zA-Z0-9]", "_", file_key))
  }
  
  # Reactive values to store state
  rv <- reactiveValues(
    script_files = NULL,
    similarity_matrix = NULL,
    viz_coords = NULL,
    clusters = NULL,
    processing = FALSE,
    stats = NULL,
    open_files = list(),  # Store multiple open file contents
    file_observers = list()  # Track observers to clean them up
  )
  
  # Setup shinyFiles folder chooser if available
  if (SHINYFILES_AVAILABLE) {
    roots <- if (.Platform$OS.type == "windows") {
      shinyFiles::getVolumes()()
    } else {
      c(root = "/", home = path.expand("~"))
    }
    
    shinyFiles::shinyDirChoose(input, "scripts_folder_select", roots = roots, session = session)
    
    observeEvent(input$scripts_folder_select, {
      if (!is.integer(input$scripts_folder_select)) {
        selected_path <- shinyFiles::parseDirPath(roots, input$scripts_folder_select)
        if (length(selected_path) > 0) {
          updateTextInput(session, "scripts_folder", value = selected_path)
        }
      }
    })
  }
  
  # Load script files when folder path is provided
  observeEvent(input$scripts_folder, {
    req(input$scripts_folder)
    
    folder_path <- input$scripts_folder
    
    # Normalize path
    folder_path_normalized <- tryCatch({
      if (is_absolute_path(folder_path)) {
        normalize_path_safe(folder_path)
      } else {
        cwd <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
        candidate_path <- file.path(cwd, folder_path)
        normalize_path_safe(candidate_path)
      }
    }, error = function(e) {
      folder_path
    })
    
    if (dir.exists(folder_path_normalized)) {
      # Get all R, Rmd, and QMD files
      files <- list.files(folder_path_normalized, 
                          pattern = "\\.(R|r|Rmd|rmd|qmd|Qmd)$", 
                          full.names = TRUE)
      
      if (length(files) > 0) {
        rv$script_files <- files
        showNotification(paste("Found", length(files), "script files"), type = "message")
      } else {
        rv$script_files <- NULL
        showNotification("No R, Rmd, or QMD files found in the specified folder", type = "warning")
      }
    } else {
      rv$script_files <- NULL
      showNotification("Folder does not exist", type = "error")
    }
  })
  
  # Run similarity analysis
  observeEvent(input$run_analysis, {
    req(rv$script_files)
    req(input$similarity_method)
    req(input$viz_method)
    
    if (length(rv$script_files) < 2) {
      showNotification("Need at least 2 scripts for similarity analysis", type = "error")
      return()
    }
    
    rv$processing <- TRUE
    
    withProgress(message = "Analyzing script similarities...", value = 0, {
      
      tryCatch({
        # Step 1: Calculate similarity matrix
        incProgress(0.1, detail = "Calculating pairwise similarities...")
        
        # Get cosine-specific parameters (with defaults for other methods)
        ngram_size <- if (!is.null(input$ngram_size)) input$ngram_size else 1
        include_actuals <- if (!is.null(input$include_actuals)) input$include_actuals else TRUE
        exclude_library_calls <- if (!is.null(input$exclude_library_calls)) input$exclude_library_calls else TRUE
        
        sim_matrix <- calculate_similarity_matrix(
          rv$script_files,
          method = input$similarity_method,
          progress_callback = function(progress) {
            incProgress(0.5 * progress, detail = paste("Processing pairs:", 
                                                       round(progress * 100), "%"))
          },
          ngram_size = ngram_size,
          include_actuals = include_actuals,
          exclude_library_calls = exclude_library_calls
        )
        
        rv$similarity_matrix <- sim_matrix
        
        # Step 2: Calculate statistics
        incProgress(0.1, detail = "Computing statistics...")
        rv$stats <- calculate_similarity_stats(sim_matrix)
        
        # Step 3: Identify clusters
        incProgress(0.1, detail = "Identifying clusters...")
        rv$clusters <- identify_clusters(sim_matrix, threshold = input$cluster_threshold)
        
        # Step 4: Perform dimensionality reduction
        incProgress(0.1, detail = paste("Running", toupper(input$viz_method), "..."))
        
        dist_matrix <- similarity_to_distance(sim_matrix)
        
        if (input$viz_method == "tsne") {
          perplexity <- input$tsne_perplexity
          rv$viz_coords <- perform_tsne(dist_matrix, perplexity = perplexity)
        } else {
          rv$viz_coords <- perform_mds(dist_matrix)
        }
        
        # Add cluster information to coordinates
        if (!is.null(rv$clusters$cluster_ids)) {
          rv$viz_coords$Cluster <- factor(rv$clusters$cluster_ids)
        }
        
        # Add file paths for click functionality
        rv$viz_coords$FilePath <- rv$script_files[match(rv$viz_coords$Script, basename(rv$script_files))]
        
        incProgress(0.1, detail = "Complete!")
        
        showNotification("Similarity analysis complete!", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error during analysis:", e$message), type = "error")
        print(e)
      })
      
      rv$processing <- FALSE
    })
  })
  
  # Status output
  output$analysis_status <- renderUI({
    if (rv$processing) {
      tags$div(
        class = "status-alert status-info",
        icon("spinner", class = "fa-spin"),
        " Processing script similarities..."
      )
    } else if (!is.null(rv$similarity_matrix)) {
      n_scripts <- nrow(rv$similarity_matrix)
      n_clusters <- length(rv$clusters$clusters)
      
      tags$div(
        class = "status-alert status-success",
        icon("check-circle"),
        paste(" Analysis complete:", n_scripts, "scripts analyzed,", 
              n_clusters, "similarity clusters detected")
      )
    } else if (!is.null(rv$script_files)) {
      tags$div(
        class = "status-alert status-info",
        icon("info-circle"),
        paste(" Ready to analyze", length(rv$script_files), 
              "scripts. Click 'Run Similarity Analysis' to begin.")
      )
    }
  })
  
  # Summary statistics
  output$summary_stats <- renderUI({
    req(rv$stats)
    
    stats <- rv$stats
    
    fluidRow(
      column(4,
        div(class = "stat-box",
          h5("Mean Similarity"),
          p(round(stats$mean, 3))
        )
      ),
      column(4,
        div(class = "stat-box",
          h5("Median Similarity"),
          p(round(stats$median, 3))
        )
      ),
      column(4,
        div(class = "stat-box",
          h5("Std Dev"),
          p(round(stats$sd, 3))
        )
      ),
      column(4,
        div(class = "stat-box",
          h5("High Similarity (≥0.7)"),
          p(stats$high_similarity_count)
        )
      ),
      column(4,
        div(class = "stat-box",
          h5("Medium Similarity (0.4-0.7)"),
          p(stats$medium_similarity_count)
        )
      ),
      column(4,
        div(class = "stat-box",
          h5("Low Similarity (<0.4)"),
          p(stats$low_similarity_count)
        )
      )
    )
  })
  
  # Similarity visualization plot
  output$similarity_plot <- renderPlotly({
    req(rv$viz_coords)
    
    coords <- rv$viz_coords
    
    # Determine colors based on clusters
    if ("Cluster" %in% names(coords) && length(unique(coords$Cluster)) > 1) {
      # Color by cluster
      cluster_levels <- unique(coords$Cluster)
      colors <- rainbow(length(cluster_levels), s = 0.6, v = 0.8)
      point_colors <- colors[as.numeric(coords$Cluster)]
      
      # Identify singleton clusters (cluster 0)
      singleton_mask <- coords$Cluster == 0
      point_colors[singleton_mask] <- "#cccccc"
    } else {
      # Single color if no clusters
      point_colors <- "#667eea"
    }
    
    # Create plotly plot
    p <- plot_ly(coords, 
            x = ~X, 
            y = ~Y, 
            text = ~Script,
            customdata = ~FilePath,
            type = 'scatter',
            mode = 'markers+text',
            marker = list(size = 12, color = point_colors, line = list(color = 'white', width = 1)),
            textposition = 'top center',
            textfont = list(size = 10),
            hovertemplate = paste('<b>%{text}</b><br>',
                                  'Click to view script<br>',
                                  '<extra></extra>'),
            source = "similarity_plot") %>%
      layout(
        title = list(
          text = paste(toupper(input$viz_method), "Projection of Script Similarities"),
          font = list(size = 16, weight = 'bold')
        ),
        xaxis = list(title = "Dimension 1", zeroline = FALSE),
        yaxis = list(title = "Dimension 2", zeroline = FALSE),
        hovermode = 'closest',
        showlegend = FALSE
      )
    
    event_register(p, 'plotly_click')
    p
  })
  
  # handle click events on the scatterplot
  observeEvent(event_data("plotly_click", source = "similarity_plot"), {
    click_data <- event_data("plotly_click", source = "similarity_plot")
    if (!is.null(click_data)) {
      # Get the point index (plotly uses 0-based indexing)
      point_index <- click_data$pointNumber + 1
      
      # Get the corresponding file path from the reactive value
      coords <- rv$viz_coords
      
      if (point_index <= nrow(coords)) {
        script_path <- coords$FilePath[point_index]
        script_name <- coords$Script[point_index]
        
        # Read the script content
        script_content <- tryCatch({
          paste(readLines(script_path, warn = FALSE), collapse = "\n")
        }, error = function(e) {
          paste("Error reading file:", e$message)
        })
        
        # Add to open files list (avoid duplicates)
        file_key <- script_name
        if (!(file_key %in% names(rv$open_files))) {
          rv$open_files[[file_key]] <- list(
            name = script_name,
            path = script_path,
            content = script_content
          )
        }
        
        # Show modal dialog with all open files
        show_open_files_modal()
      }
    }
  })
  
  # Helper function to display all open files in a modal
  show_open_files_modal <- function() {
    if (length(rv$open_files) == 0) return(NULL)
    
    # Create tabs for each open file
    file_tabs <- lapply(names(rv$open_files), function(file_key) {
      file_info <- rv$open_files[[file_key]]
      tabPanel(
        title = file_info$name,
        value = file_key,
        br(),
        tags$div(
          style = "position: relative;",
          actionButton(
            generate_btn_id(file_key),
            "Close this file",
            class = "btn-secondary btn-sm",
            style = "margin-bottom: 10px;"
          ),
          tags$pre(
            style = "max-height: 450px; overflow-y: auto; background-color: #f5f5f5; padding: 15px; border-radius: 5px; font-family: 'Courier New', monospace; font-size: 12px;",
            file_info$content
          )
        )
      )
    })
    
    # Show modal dialog with tabbed interface
    showModal(modalDialog(
      title = paste("Viewing", length(rv$open_files), "file(s) - Click on more points to add files"),
      do.call(tabsetPanel, c(list(id = "file_tabs"), file_tabs)),
      footer = tagList(
        actionButton("clear_all_files", "Close All Files", class = "btn-secondary"),
        modalButton("Done")
      ),
      size = "l",
      easyClose = FALSE
    ))
  }
  
  # Observer for close buttons - use a single observer with conditional logic
  observeEvent(rv$open_files, {
    # Clean up old observers with error handling
    lapply(rv$file_observers, function(obs) {
      tryCatch({
        if (!is.null(obs)) obs$destroy()
      }, error = function(e) {
        # Silently handle observer destruction errors
        NULL
      })
    })
    rv$file_observers <- list()
    
    # create new observers for current files
    if (length(rv$open_files) > 0) {
      rv$file_observers <- lapply(names(rv$open_files), function(file_key) {
        btn_id <- generate_btn_id(file_key)
        observeEvent(input[[btn_id]], {
          # Remove this file from open_files
          rv$open_files[[file_key]] <- NULL
          
          # If there are still files open, refresh the modal
          if (length(rv$open_files) > 0) {
            show_open_files_modal()
          } else {
            # Close the modal if no files left
            removeModal()
          }
        }, ignoreInit = TRUE, ignoreNULL = TRUE)
      })
    }
  }, ignoreNULL = FALSE)
  
  # Observer for clear all files button
  observeEvent(input$clear_all_files, {
    rv$open_files <- list()
    removeModal()
  }, ignoreInit = TRUE)
  
  # heatmap plot
  output$heatmap_plot <- renderPlot({
    req(rv$similarity_matrix)
    
    sim_matrix <- rv$similarity_matrix
    
    # Perform hierarchical clustering
    # Convert similarity to distance for clustering
    dist_matrix_for_hclust <- as.dist(1 - sim_matrix)
    hc <- hclust(dist_matrix_for_hclust, method = "complete")
    
    # Reorder the matrix based on clustering
    order_idx <- hc$order
    sim_matrix_ordered <- sim_matrix[order_idx, order_idx]
    
    # Create heatmap
    par(mar = c(8, 8, 2, 2))
    
    # Define color palette
    color_palette <- colorRampPalette(c("#fff5f0", "#fee0d2", "#fcbba1", 
                                       "#fc9272", "#fb6a4a", "#ef3b2c",
                                       "#cb181d", "#99000d"))(100)
    
    image(1:nrow(sim_matrix_ordered), 1:ncol(sim_matrix_ordered), sim_matrix_ordered,
          col = color_palette,
          xlab = "", ylab = "",
          axes = FALSE,
          main = "Pairwise Similarity Matrix (Hierarchically Clustered)",
          cex.main = 1.2,
          font.main = 2)
    
    # Add axes with reordered labels
    axis(1, at = 1:nrow(sim_matrix_ordered), labels = rownames(sim_matrix_ordered), 
         las = 2, cex.axis = 0.7)
    axis(2, at = 1:ncol(sim_matrix_ordered), labels = colnames(sim_matrix_ordered), 
         las = 2, cex.axis = 0.7)
    
    # Add color scale legend
    legend_values <- seq(0, 1, length.out = 5)
    legend("bottomright",
           legend = format(legend_values, digits = 2),
           fill = color_palette[seq(1, 100, length.out = 5)],
           title = "Similarity",
           cex = 0.8,
           bg = "white")
  })
  
  # top similar pairs table
  output$similar_pairs_table <- renderDT({
    req(rv$similarity_matrix)
    
    pairs <- get_top_similar_pairs(rv$similarity_matrix, n = 20)
    
    datatable(
      pairs,
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
        'Similarity',
        background = DT::styleColorBar(c(0, 1), '#667eea'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Clusters information
  output$clusters_info <- renderUI({
    req(rv$clusters)
    
    clusters <- rv$clusters$clusters
    
    if (length(clusters) == 0) {
      return(tags$div(
        class = "status-alert status-info",
        icon("info-circle"),
        " No clusters detected with the current threshold. Try lowering the similarity threshold."
      ))
    }
    
    cluster_items <- lapply(seq_along(clusters), function(i) {
      cluster_scripts <- rv$clusters$labels[clusters[[i]]]
      
      tags$div(
        class = "stat-box",
        tags$h5(paste("Cluster", i, "-", length(cluster_scripts), "scripts")),
        tags$p(paste(cluster_scripts, collapse = ", "),
               style = "font-size: 14px; font-weight: normal;")
      )
    })
    
    do.call(tagList, cluster_items)
  })
  
  # full similarity matrix table
  output$similarity_matrix_table <- renderDT({
    req(rv$similarity_matrix)
    
    # Convert matrix to data frame
    sim_df <- as.data.frame(round(rv$similarity_matrix, 4))
    sim_df <- cbind(Script = rownames(sim_df), sim_df)
    
    datatable(
      sim_df,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'Bfrtip',
        ordering = TRUE
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover'
    ) %>%
      DT::formatStyle(
        columns = 2:ncol(sim_df),
        background = DT::styleColorBar(c(0, 1), '#667eea'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # Download handler
  output$download_results <- downloadHandler(
    filename = function() {
      paste0("similarity_analysis_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      req(rv$similarity_matrix)
      
      # Create comprehensive results
      sim_matrix <- rv$similarity_matrix
      
      # Get all pairwise similarities
      n <- nrow(sim_matrix)
      results_list <- list()
      
      for (i in 1:(n-1)) {
        for (j in (i+1):n) {
          results_list[[length(results_list) + 1]] <- data.frame(
            Script1 = rownames(sim_matrix)[i],
            Script2 = rownames(sim_matrix)[j],
            Similarity = sim_matrix[i, j],
            Method = input$similarity_method,
            stringsAsFactors = FALSE
          )
        }
      }
      
      results_df <- do.call(rbind, results_list)
      results_df <- results_df[order(results_df$Similarity, decreasing = TRUE), ]
      
      write.csv(results_df, file, row.names = FALSE)
    }
  )
  
  # Update file selectors when scripts are loaded
  observeEvent(rv$script_files, {
    if (!is.null(rv$script_files) && length(rv$script_files) > 0) {
      file_choices <- setNames(rv$script_files, basename(rv$script_files))
      
      updateSelectInput(session, "diff_file1", 
                       choices = file_choices,
                       selected = rv$script_files[1])
      
      updateSelectInput(session, "diff_file2", 
                       choices = file_choices,
                       selected = if(length(rv$script_files) > 1) rv$script_files[2] else rv$script_files[1])
    }
  })
  
  # Diff view logic
  observeEvent(input$run_diff, {
    req(input$diff_file1)
    req(input$diff_file2)
    
    file1 <- input$diff_file1
    file2 <- input$diff_file2
    
    if (file1 == file2) {
      showNotification("Please select two different files to compare", type = "warning")
      return()
    }
    
    tryCatch({
      # Read files
      lines1 <- readLines(file1, warn = FALSE)
      lines2 <- readLines(file2, warn = FALSE)
      
      # Create diff output
      output$diff_output <- renderUI({
        create_diff_view(lines1, lines2, basename(file1), basename(file2))
      })
      
      showNotification("Diff comparison complete", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error creating diff:", e$message), type = "error")
    })
  })
}

#' Create a side-by-side diff view
#' 
#' @param lines1 Lines from first file
#' @param lines2 Lines from second file
#' @param name1 Name of first file
#' @param name2 Name of second file
#' @return HTML UI elements for diff view
create_diff_view <- function(lines1, lines2, name1, name2) {
  # Perform simple line-by-line diff
  max_lines <- max(length(lines1), length(lines2))
  
  # Pad shorter file with empty lines
  if (length(lines1) < max_lines) {
    lines1 <- c(lines1, rep("", max_lines - length(lines1)))
  }
  if (length(lines2) < max_lines) {
    lines2 <- c(lines2, rep("", max_lines - length(lines2)))
  }
  
  # Build diff HTML
  diff_html <- tags$div(
    style = "display: flex; width: 100%; font-family: 'Courier New', monospace; font-size: 12px;",
    
    # Left panel (file 1)
    tags$div(
      style = "flex: 1; border-right: 2px solid #ccc; overflow-x: auto;",
      tags$div(
        style = "background-color: #e3f2fd; padding: 10px; font-weight: bold; border-bottom: 1px solid #ccc;",
        name1
      ),
      tags$div(
        style = "max-height: 600px; overflow-y: auto;",
        lapply(seq_along(lines1), function(i) {
          line1 <- lines1[i]
          line2 <- lines2[i]
          is_different <- line1 != line2
          
          bg_color <- if (is_different) {
            if (line1 == "") "#ffebee" else if (line2 == "") "#e8f5e9" else "#fff9c4"
          } else {
            "#ffffff"
          }
          
          tags$div(
            style = sprintf("padding: 2px 10px; background-color: %s; border-bottom: 1px solid #f0f0f0;", bg_color),
            tags$span(
              style = "display: inline-block; width: 40px; color: #999; text-align: right; margin-right: 10px;",
              i
            ),
            tags$span(
              style = "white-space: pre-wrap; word-break: break-all;",
              if (line1 == "") "\u00a0" else line1  # Non-breaking space for empty lines
            )
          )
        })
      )
    ),
    
    # Right panel (file 2)
    tags$div(
      style = "flex: 1; overflow-x: auto;",
      tags$div(
        style = "background-color: #e3f2fd; padding: 10px; font-weight: bold; border-bottom: 1px solid #ccc;",
        name2
      ),
      tags$div(
        style = "max-height: 600px; overflow-y: auto;",
        lapply(seq_along(lines2), function(i) {
          line1 <- lines1[i]
          line2 <- lines2[i]
          is_different <- line1 != line2
          
          bg_color <- if (is_different) {
            if (line2 == "") "#ffebee" else if (line1 == "") "#e8f5e9" else "#fff9c4"
          } else {
            "#ffffff"
          }
          
          tags$div(
            style = sprintf("padding: 2px 10px; background-color: %s; border-bottom: 1px solid #f0f0f0;", bg_color),
            tags$span(
              style = "display: inline-block; width: 40px; color: #999; text-align: right; margin-right: 10px;",
              i
            ),
            tags$span(
              style = "white-space: pre-wrap; word-break: break-all;",
              if (line2 == "") "\u00a0" else line2  # Non-breaking space for empty lines
            )
          )
        })
      )
    )
  )
  
  # Add legend
  legend <- tags$div(
    style = "margin-top: 20px; padding: 10px; background-color: #f5f5f5; border-radius: 5px;",
    tags$h5("Color Legend:"),
    tags$div(
      style = "display: flex; gap: 20px;",
      tags$div(
        tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #fff9c4; border: 1px solid #ccc; vertical-align: middle;"),
        " Modified lines"
      ),
      tags$div(
        tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #e8f5e9; border: 1px solid #ccc; vertical-align: middle;"),
        " Added lines"
      ),
      tags$div(
        tags$span(style = "display: inline-block; width: 20px; height: 20px; background-color: #ffebee; border: 1px solid #ccc; vertical-align: middle;"),
        " Removed lines"
      )
    )
  )
  
  tagList(diff_html, legend)
}

# Run the application
shinyApp(ui = ui, server = server)




