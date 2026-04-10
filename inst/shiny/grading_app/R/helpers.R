# helper functions for App

# constants for test generation
DEFAULT_NUMERIC_TOLERANCE <- 1e-6
MAX_ELEMENTS_TO_TEST <- 3

#' Sanitize object name for use as R variable prefix
#' 
#' @param obj_name Object name to sanitize
#' @return Sanitized name safe for use in R variable names
sanitize_name <- function(obj_name) {
  gsub("[^a-zA-Z0-9_]", "_", obj_name)
}

#' Create a test result data frame
#' Helper function to create consistent test result format
#' 
#' @param object Object name
#' @param test Test name
#' @param result Test result (PASS, FAIL, SKIP, INFO)
#' @param details Details about the test
#' @return A data.frame with one row
create_test_result <- function(object, test, result, details) {
  data.frame(
    object = object,
    test = test,
    result = result,
    details = details,
    stringsAsFactors = FALSE
  )
}

#' Bind multiple data frames by row
#' Helper function that works like dplyr::bind_rows but with base R
#' 
#' @param df_list List of data frames to bind
#' @return A single data frame with all rows
bind_test_results <- function(df_list) {
  if (length(df_list) == 0) {
    return(data.frame(
      object = character(0),
      test = character(0),
      result = character(0),
      details = character(0),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, df_list)
}

#' Source an R file with each expression wrapped in try-catch
#' 
#' This function reads an R file, parses it into expressions, and evaluates
#' each expression independently wrapped in tryCatch. This allows the script
#' to continue even if some expressions have errors.
#' 
#' @param file Path to the R file to source
#' @param envir Environment in which to evaluate the expressions
#' @return Invisible NULL. Expressions are evaluated for their side effects.
source_with_trycatch <- function(file, envir = parent.frame()) {
  # Read and parse the file
  exprs <- tryCatch({
    parse(file = file)
  }, error = function(e) {
    warning(paste("Failed to parse file:", file, "-", e$message))
    return(NULL)
  })
  
  if (is.null(exprs)) return(invisible(NULL))
  
  # Evaluate each expression wrapped in tryCatch
  for (i in seq_along(exprs)) {
    tryCatch({
      eval(exprs[[i]], envir = envir)
    }, error = function(e) {
      # Continue execution - the error is caught and the script continues
      # The test framework will detect which objects were not created
      # Warning is issued to help with debugging if needed
      warning(paste("Error in expression", i, "of", file, ":", e$message))
    })
  }
  
  invisible(NULL)
}

#' Run comprehensive tests on objects
#' 
#' @param student_env Environment containing student objects
#' @param solution_env Environment containing solution objects
#' @param objects_to_test Character vector of object names to test
#' @return A data.frame with test results
run_object_tests <- function(student_env, solution_env, objects_to_test) {
  
  results <- list()
  
  for (obj_name in objects_to_test) {
    # Check if object exists in solution
    if (!exists(obj_name, envir = solution_env)) {
      results[[length(results) + 1]] <- create_test_result(
        object = obj_name,
        test = "existence_in_solution",
        result = "SKIP",
        details = "Object not found in solution template"
      )
      next
    }
    
    # Check if object exists in student environment
    if (!exists(obj_name, envir = student_env)) {
      results[[length(results) + 1]] <- create_test_result(
        object = obj_name,
        test = "existence",
        result = "FAIL",
        details = "Object not found in student submission"
      )
      next
    }
    
    # Get both objects
    solution_obj <- get(obj_name, envir = solution_env)
    student_obj <- get(obj_name, envir = student_env)
    
    # Determine object type and run appropriate tests
    obj_tests <- run_type_specific_tests(student_obj, solution_obj, obj_name)
    results <- c(results, obj_tests)
  }
  
  bind_test_results(results)
}

#' Run tests specific to object type
#' 
#' @param student_obj Student's object
#' @param solution_obj Solution object
#' @param obj_name Name of the object
#' @return List of test result data.frame objects
run_type_specific_tests <- function(student_obj, solution_obj, obj_name) {
  
  results <- list()
  
  # Class check
  results[[length(results) + 1]] <- test_class(student_obj, solution_obj, obj_name)
  
  # Type-specific tests
  if (is.vector(solution_obj) && !is.list(solution_obj)) {
    results <- c(results, test_vector(student_obj, solution_obj, obj_name))
  } else if (is.data.frame(solution_obj)) {
    results <- c(results, test_dataframe(student_obj, solution_obj, obj_name))
  } else if (is.matrix(solution_obj)) {
    results <- c(results, test_matrix(student_obj, solution_obj, obj_name))
  } else if (is.list(solution_obj)) {
    results <- c(results, test_list(student_obj, solution_obj, obj_name))
  } else if (is.function(solution_obj)) {
    results <- c(results, test_function(student_obj, solution_obj, obj_name))
  } else if (is.factor(solution_obj)) {
    results <- c(results, test_factor(student_obj, solution_obj, obj_name))
  }
  
  results
}

#' Test class of object
test_class <- function(student_obj, solution_obj, obj_name) {
  student_class <- class(student_obj)[1]
  solution_class <- class(solution_obj)[1]
  
  create_test_result(
    object = obj_name,
    test = "class",
    result = ifelse(student_class == solution_class, "PASS", "FAIL"),
    details = ifelse(
      student_class == solution_class,
      paste("Class:", student_class),
      paste("Expected:", solution_class, "| Got:", student_class)
    )
  )
}

#' Test vector objects
#' 
#' Tests: length, mean, sd, min, max, sum, identical values
test_vector <- function(student_obj, solution_obj, obj_name) {
  results <- list()
  
  # Length test
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "length",
    result = ifelse(length(student_obj) == length(solution_obj), "PASS", "FAIL"),
    details = paste("Expected:", length(solution_obj), "| Got:", length(student_obj))
  )
  
  # For numeric vectors, run statistical tests
  if (is.numeric(solution_obj) && is.numeric(student_obj)) {
    # Mean test
    solution_mean <- mean(solution_obj, na.rm = TRUE)
    student_mean <- mean(student_obj, na.rm = TRUE)
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "mean",
      result = ifelse(
        (is.na(solution_mean) && is.na(student_mean)) || 
          (!is.na(solution_mean) && !is.na(student_mean) && 
             abs(solution_mean - student_mean) < 1e-6),
        "PASS", "FAIL"
      ),
      details = paste("Expected:", round(solution_mean, 6), "| Got:", round(student_mean, 6))
    )
    
    # SD test
    solution_sd <- sd(solution_obj, na.rm = TRUE)
    student_sd <- sd(student_obj, na.rm = TRUE)
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "sd",
      result = ifelse(
        (is.na(solution_sd) && is.na(student_sd)) || 
          (!is.na(solution_sd) && !is.na(student_sd) && 
             abs(solution_sd - student_sd) < 1e-6),
        "PASS", "FAIL"
      ),
      details = paste("Expected:", round(solution_sd, 6), "| Got:", round(student_sd, 6))
    )
    
    # Min test
    solution_min <- min(solution_obj, na.rm = TRUE)
    student_min <- min(student_obj, na.rm = TRUE)
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "min",
      result = ifelse(
        abs(solution_min - student_min) < 1e-6,
        "PASS", "FAIL"
      ),
      details = paste("Expected:", round(solution_min, 6), "| Got:", round(student_min, 6))
    )
    
    # Max test
    solution_max <- max(solution_obj, na.rm = TRUE)
    student_max <- max(student_obj, na.rm = TRUE)
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "max",
      result = ifelse(
        abs(solution_max - student_max) < 1e-6,
        "PASS", "FAIL"
      ),
      details = paste("Expected:", round(solution_max, 6), "| Got:", round(student_max, 6))
    )
    
    # Sum test
    solution_sum <- sum(solution_obj, na.rm = TRUE)
    student_sum <- sum(student_obj, na.rm = TRUE)
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "sum",
      result = ifelse(
        abs(solution_sum - student_sum) < 1e-6,
        "PASS", "FAIL"
      ),
      details = paste("Expected:", round(solution_sum, 6), "| Got:", round(student_sum, 6))
    )
    
    # NA count test
    solution_na <- sum(is.na(solution_obj))
    student_na <- sum(is.na(student_obj))
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "na_count",
      result = ifelse(solution_na == student_na, "PASS", "FAIL"),
      details = paste("Expected NA count:", solution_na, "| Got:", student_na)
    )
  }
  
  # Identical values test (if lengths match)
  if (length(student_obj) == length(solution_obj)) {
    is_identical <- all.equal(student_obj, solution_obj)
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "identical_values",
      result = ifelse(isTRUE(is_identical), "PASS", "FAIL"),
      details = ifelse(
        isTRUE(is_identical),
        "Values are identical",
        paste("Differences found:", 
              ifelse(is.character(is_identical), 
                     substr(paste(is_identical, collapse = "; "), 1, 100), 
                     "Values differ"))
      )
    )
  }
  
  results
}

#' Test data frame objects
#' 
#' Tests: nrow, ncol, column names, column types, means/sds for numeric cols,
#' unique values for factors, correlation matrix
test_dataframe <- function(student_obj, solution_obj, obj_name) {
  results <- list()
  
  # Check if student object is a data frame
  if (!is.data.frame(student_obj)) {
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "is_dataframe",
      result = "FAIL",
      details = paste("Expected data.frame, got:", class(student_obj)[1])
    )
    return(results)
  }
  
  # nrow test
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "nrow",
    result = ifelse(nrow(student_obj) == nrow(solution_obj), "PASS", "FAIL"),
    details = paste("Expected:", nrow(solution_obj), "| Got:", nrow(student_obj))
  )
  
  # ncol test
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "ncol",
    result = ifelse(ncol(student_obj) == ncol(solution_obj), "PASS", "FAIL"),
    details = paste("Expected:", ncol(solution_obj), "| Got:", ncol(student_obj))
  )
  
  # Column names test
  solution_names <- names(solution_obj)
  student_names <- names(student_obj)
  names_match <- setequal(solution_names, student_names)
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "column_names",
    result = ifelse(names_match, "PASS", "FAIL"),
    details = ifelse(
      names_match,
      paste("Columns:", paste(solution_names, collapse = ", ")),
      paste("Missing:", paste(setdiff(solution_names, student_names), collapse = ", "),
            "| Extra:", paste(setdiff(student_names, solution_names), collapse = ", "))
    )
  )
  
  # Column types test
  if (names_match) {
    for (col_name in solution_names) {
      solution_type <- class(solution_obj[[col_name]])[1]
      student_type <- class(student_obj[[col_name]])[1]
      
      results[[length(results) + 1]] <- create_test_result(
        object = obj_name,
        test = paste0("column_type_", col_name),
        result = ifelse(solution_type == student_type, "PASS", "FAIL"),
        details = paste("Column", col_name, "- Expected:", solution_type, "| Got:", student_type)
      )
    }
    
    # For numeric columns, test mean and sd
    numeric_cols <- names(solution_obj)[sapply(solution_obj, is.numeric)]
    for (col_name in numeric_cols) {
      if (col_name %in% names(student_obj) && is.numeric(student_obj[[col_name]])) {
        solution_mean <- mean(solution_obj[[col_name]], na.rm = TRUE)
        student_mean <- mean(student_obj[[col_name]], na.rm = TRUE)
        
        results[[length(results) + 1]] <- create_test_result(
          object = obj_name,
          test = paste0("column_mean_", col_name),
          result = ifelse(
            abs(solution_mean - student_mean) < 1e-6 ||
              (is.na(solution_mean) && is.na(student_mean)),
            "PASS", "FAIL"
          ),
          details = paste("Column", col_name, "mean - Expected:", 
                          round(solution_mean, 4), "| Got:", round(student_mean, 4))
        )
        
        solution_sd <- sd(solution_obj[[col_name]], na.rm = TRUE)
        student_sd <- sd(student_obj[[col_name]], na.rm = TRUE)
        
        results[[length(results) + 1]] <- create_test_result(
          object = obj_name,
          test = paste0("column_sd_", col_name),
          result = ifelse(
            abs(solution_sd - student_sd) < 1e-6 ||
              (is.na(solution_sd) && is.na(student_sd)),
            "PASS", "FAIL"
          ),
          details = paste("Column", col_name, "sd - Expected:", 
                          round(solution_sd, 4), "| Got:", round(student_sd, 4))
        )
      }
    }
    
    # For factor columns, test unique values
    factor_cols <- names(solution_obj)[sapply(solution_obj, is.factor)]
    for (col_name in factor_cols) {
      if (col_name %in% names(student_obj) && is.factor(student_obj[[col_name]])) {
        solution_levels <- levels(solution_obj[[col_name]])
        student_levels <- levels(student_obj[[col_name]])
        levels_match <- setequal(solution_levels, student_levels)
        
        results[[length(results) + 1]] <- create_test_result(
          object = obj_name,
          test = paste0("factor_levels_", col_name),
          result = ifelse(levels_match, "PASS", "FAIL"),
          details = paste("Column", col_name, "levels match:", levels_match)
        )
      }
    }
    
    # Correlation matrix test for numeric columns (if at least 2 numeric cols)
    if (length(numeric_cols) >= 2) {
      tryCatch({
        solution_cor <- cor(solution_obj[, numeric_cols, drop = FALSE], use = "pairwise.complete.obs")
        student_cor <- cor(student_obj[, numeric_cols, drop = FALSE], use = "pairwise.complete.obs")
        
        cor_diff <- max(abs(solution_cor - student_cor), na.rm = TRUE)
        
        results[[length(results) + 1]] <- create_test_result(
          object = obj_name,
          test = "correlation_matrix",
          result = ifelse(cor_diff < 0.01, "PASS", "FAIL"),
          details = paste("Max correlation difference:", round(cor_diff, 6))
        )
      }, error = function(e) {
        results[[length(results) + 1]] <- create_test_result(
          object = obj_name,
          test = "correlation_matrix",
          result = "SKIP",
          details = paste("Could not compute correlation:", e$message)
        )
      })
    }
  }
  
  results
}

#' Test matrix objects
#' 
#' Tests: dimensions, row/col names, element-wise comparison, matrix properties
test_matrix <- function(student_obj, solution_obj, obj_name) {
  results <- list()
  
  # Check if student object is a matrix
  if (!is.matrix(student_obj)) {
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "is_matrix",
      result = "FAIL",
      details = paste("Expected matrix, got:", class(student_obj)[1])
    )
    return(results)
  }
  
  # Dimension tests
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "nrow",
    result = ifelse(nrow(student_obj) == nrow(solution_obj), "PASS", "FAIL"),
    details = paste("Expected:", nrow(solution_obj), "| Got:", nrow(student_obj))
  )
  
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "ncol",
    result = ifelse(ncol(student_obj) == ncol(solution_obj), "PASS", "FAIL"),
    details = paste("Expected:", ncol(solution_obj), "| Got:", ncol(student_obj))
  )
  
  # Row names test
  if (!is.null(rownames(solution_obj))) {
    rownames_match <- identical(rownames(solution_obj), rownames(student_obj))
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "rownames",
      result = ifelse(rownames_match, "PASS", "FAIL"),
      details = ifelse(rownames_match, "Row names match", "Row names differ")
    )
  }
  
  # Column names test
  if (!is.null(colnames(solution_obj))) {
    colnames_match <- identical(colnames(solution_obj), colnames(student_obj))
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "colnames",
      result = ifelse(colnames_match, "PASS", "FAIL"),
      details = ifelse(colnames_match, "Column names match", "Column names differ")
    )
  }
  
  # Element-wise comparison (if dimensions match)
  if (all(dim(student_obj) == dim(solution_obj))) {
    if (is.numeric(solution_obj) && is.numeric(student_obj)) {
      max_diff <- max(abs(solution_obj - student_obj), na.rm = TRUE)
      
      results[[length(results) + 1]] <- create_test_result(
        object = obj_name,
        test = "max_element_difference",
        result = ifelse(max_diff < 1e-6, "PASS", "FAIL"),
        details = paste("Max element difference:", max_diff)
      )
      
      # Mean of all elements
      solution_mean <- mean(solution_obj, na.rm = TRUE)
      student_mean <- mean(student_obj, na.rm = TRUE)
      
      results[[length(results) + 1]] <- create_test_result(
        object = obj_name,
        test = "overall_mean",
        result = ifelse(abs(solution_mean - student_mean) < 1e-6, "PASS", "FAIL"),
        details = paste("Expected:", round(solution_mean, 6), "| Got:", round(student_mean, 6))
      )
    }
  }
  
  results
}

#' Test list objects
#' 
#' Tests: length, element names, recursive element tests
test_list <- function(student_obj, solution_obj, obj_name) {
  results <- list()
  
  # Check if student object is a list
  if (!is.list(student_obj)) {
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "is_list",
      result = "FAIL",
      details = paste("Expected list, got:", class(student_obj)[1])
    )
    return(results)
  }
  
  # Length test
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "length",
    result = ifelse(length(student_obj) == length(solution_obj), "PASS", "FAIL"),
    details = paste("Expected:", length(solution_obj), "| Got:", length(student_obj))
  )
  
  # Names test
  solution_names <- names(solution_obj)
  student_names <- names(student_obj)
  
  if (!is.null(solution_names)) {
    names_match <- setequal(solution_names, student_names)
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "element_names",
      result = ifelse(names_match, "PASS", "FAIL"),
      details = ifelse(
        names_match,
        paste("Names:", paste(solution_names, collapse = ", ")),
        paste("Missing:", paste(setdiff(solution_names, student_names), collapse = ", "))
      )
    )
    
    # Test each named element
    for (elem_name in solution_names) {
      if (elem_name %in% student_names) {
        solution_elem <- solution_obj[[elem_name]]
        student_elem <- student_obj[[elem_name]]
        
        # Class test for element
        results[[length(results) + 1]] <- create_test_result(
          object = obj_name,
          test = paste0("element_class_", elem_name),
          result = ifelse(class(solution_elem)[1] == class(student_elem)[1], "PASS", "FAIL"),
          details = paste("Element", elem_name, "- Expected class:", 
                          class(solution_elem)[1], "| Got:", class(student_elem)[1])
        )
        
        # Length test for element
        if (is.atomic(solution_elem) || is.list(solution_elem)) {
          results[[length(results) + 1]] <- create_test_result(
            object = obj_name,
            test = paste0("element_length_", elem_name),
            result = ifelse(length(solution_elem) == length(student_elem), "PASS", "FAIL"),
            details = paste("Element", elem_name, "- Expected length:", 
                            length(solution_elem), "| Got:", length(student_elem))
          )
        }
        
        # For numeric elements, test mean
        if (is.numeric(solution_elem) && is.numeric(student_elem)) {
          solution_mean <- mean(solution_elem, na.rm = TRUE)
          student_mean <- mean(student_elem, na.rm = TRUE)
          
          results[[length(results) + 1]] <- create_test_result(
            object = obj_name,
            test = paste0("element_mean_", elem_name),
            result = ifelse(
              abs(solution_mean - student_mean) < 1e-6 ||
                (is.na(solution_mean) && is.na(student_mean)),
              "PASS", "FAIL"
            ),
            details = paste("Element", elem_name, "mean - Expected:", 
                            round(solution_mean, 4), "| Got:", round(student_mean, 4))
          )
        }
      }
    }
  }
  
  results
}

#' Test function objects
#' 
#' Tests: number of arguments, argument names, body structure
test_function <- function(student_obj, solution_obj, obj_name) {
  results <- list()
  
  # Check if student object is a function
  if (!is.function(student_obj)) {
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "is_function",
      result = "FAIL",
      details = paste("Expected function, got:", class(student_obj)[1])
    )
    return(results)
  }
  
  # Number of arguments test
  solution_args <- names(formals(solution_obj))
  student_args <- names(formals(student_obj))
  
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "num_arguments",
    result = ifelse(length(solution_args) == length(student_args), "PASS", "FAIL"),
    details = paste("Expected:", length(solution_args), "args | Got:", length(student_args), "args")
  )
  
  # Argument names test
  args_match <- setequal(solution_args, student_args)
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "argument_names",
    result = ifelse(args_match, "PASS", "FAIL"),
    details = ifelse(
      args_match,
      paste("Args:", paste(solution_args, collapse = ", ")),
      paste("Expected:", paste(solution_args, collapse = ", "),
            "| Got:", paste(student_args, collapse = ", "))
    )
  )
  
  # Body length test (rough complexity measure)
  solution_body_length <- length(deparse(body(solution_obj)))
  student_body_length <- length(deparse(body(student_obj)))
  
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "body_length",
    result = "INFO",
    details = paste("Solution body:", solution_body_length, "lines | Student body:", 
                    student_body_length, "lines")
  )
  
  results
}

#' Test factor objects
#' 
#' Tests: levels, number of levels, level order
test_factor <- function(student_obj, solution_obj, obj_name) {
  results <- list()
  
  # Check if student object is a factor
  if (!is.factor(student_obj)) {
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "is_factor",
      result = "FAIL",
      details = paste("Expected factor, got:", class(student_obj)[1])
    )
    return(results)
  }
  
  # Number of levels test
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "num_levels",
    result = ifelse(nlevels(solution_obj) == nlevels(student_obj), "PASS", "FAIL"),
    details = paste("Expected:", nlevels(solution_obj), "levels | Got:", nlevels(student_obj), "levels")
  )
  
  # Levels match test
  levels_match <- setequal(levels(solution_obj), levels(student_obj))
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "levels_match",
    result = ifelse(levels_match, "PASS", "FAIL"),
    details = ifelse(
      levels_match,
      paste("Levels:", paste(levels(solution_obj), collapse = ", ")),
      paste("Missing:", paste(setdiff(levels(solution_obj), levels(student_obj)), collapse = ", "))
    )
  )
  
  # Level order test
  order_match <- identical(levels(solution_obj), levels(student_obj))
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "levels_order",
    result = ifelse(order_match, "PASS", "FAIL"),
    details = ifelse(order_match, "Level order matches", "Level order differs")
  )
  
  # Length test
  results[[length(results) + 1]] <- create_test_result(
    object = obj_name,
    test = "length",
    result = ifelse(length(solution_obj) == length(student_obj), "PASS", "FAIL"),
    details = paste("Expected:", length(solution_obj), "| Got:", length(student_obj))
  )
  
  # Value frequency test
  solution_table <- table(solution_obj)
  student_table <- table(student_obj)
  
  if (levels_match) {
    # Safely compare frequency tables handling potential missing names
    common_names <- intersect(names(solution_table), names(student_table))
    all_names_present <- length(common_names) == length(names(solution_table))
    
    if (all_names_present) {
      freq_match <- all(solution_table[common_names] == student_table[common_names])
    } else {
      freq_match <- FALSE
    }
    
    results[[length(results) + 1]] <- create_test_result(
      object = obj_name,
      test = "level_frequencies",
      result = ifelse(freq_match, "PASS", "FAIL"),
      details = ifelse(freq_match, "Level frequencies match", "Level frequencies differ")
    )
  }
  
  results
}

#' Collect plots generated by a student script
#' 
#' @param student_file Path to student script
#' @param plots_output_dir Optional path to dedicated plots output directory
#' @return Character vector of plot file paths (as data URIs or file paths)
collect_student_plots <- function(student_file, plots_output_dir = NULL) {
  # This function collects plots generated during knitting
  
  # Helper function to find image files in a directory
  find_plot_files <- function(dir_path) {
    if (dir.exists(dir_path)) {
      list.files(dir_path, 
                 pattern = "\\.(png|jpg|jpeg|gif|svg)$",
                 full.names = TRUE,
                 recursive = TRUE)
    } else {
      character(0)
    }
  }
  
  if (grepl("\\.(Rmd|rmd|qmd|Qmd)$", student_file)) {
    student_name <- basename(student_file)
    student_base <- tools::file_path_sans_ext(student_name)
    
    # List of directories to check, in order of priority
    dirs_to_check <- c()
    
    # First, check the dedicated plots output directory if provided
    if (!is.null(plots_output_dir)) {
      # Object Testing layout: {plots_output_dir}/{student_base}/
      dirs_to_check <- c(dirs_to_check, file.path(plots_output_dir, student_base))
      # Autoharp render_one layout: {out_dir}/{student_base}_files/figure-html/
      dirs_to_check <- c(dirs_to_check,
                         file.path(plots_output_dir, paste0(student_base, "_files"), "figure-html"),
                         file.path(plots_output_dir, paste0(student_base, "_files")))
    }
    
    # Fallback: look in the standard _files directory created by rmarkdown/quarto
    base_name <- sub("\\.(Rmd|rmd|qmd|Qmd)$", "", student_file)
    dirs_to_check <- c(dirs_to_check, paste0(base_name, "_files"))
    
    # Also check for figure-html subdirectory (common pattern)
    dirs_to_check <- c(dirs_to_check, 
                       file.path(dirname(student_file), paste0(student_base, "_files"), "figure-html"))
    
    # Check each directory in order, return first non-empty result
    for (dir_path in dirs_to_check) {
      plot_files <- find_plot_files(dir_path)
      if (length(plot_files) > 0) {
        return(plot_files)
      }
    }
  }
  
  # Return empty vector if no plots found
  character(0)
}

#' Generate autoharp-compatible template with test chunks
#' 
#' @param original_content Character vector of original RMD file lines
#' @param solution_env Environment containing solution objects
#' @param objects_to_test Character vector of object names to test
#' @param test_prefix Prefix for test chunk names
#' @param include_all_tests Whether to include all comprehensive tests
#' @return Character vector of new template lines
generate_autoharp_template <- function(original_content, solution_env, 
                                       objects_to_test, test_prefix = "test",
                                       include_all_tests = TRUE) {
  
  # Start with original content
  template_lines <- original_content
  
  # Generate test chunks for each object type group
  test_chunks <- generate_test_chunks(
    solution_env = solution_env,
    objects_to_test = objects_to_test,
    test_prefix = test_prefix,
    include_all_tests = include_all_tests
  )
  
  # Add test chunks at the end of the document
  template_lines <- c(
    template_lines,
    "",
    "## Autoharp Test Chunks",
    "",
    "The following chunks contain automated tests for correctness checking.",
    "",
    test_chunks
  )
  
  template_lines
}

#' Generate test chunks for objects
#' 
#' @param solution_env Environment containing solution objects
#' @param objects_to_test Character vector of object names to test
#' @param test_prefix Prefix for test chunk names
#' @param include_all_tests Whether to include all comprehensive tests
#' @return Character vector of test chunk lines
generate_test_chunks <- function(solution_env, objects_to_test, 
                                 test_prefix = "test",
                                 include_all_tests = TRUE) {
  
  all_chunks <- character(0)
  chunk_counter <- 1
  
  for (obj_name in objects_to_test) {
    if (!exists(obj_name, envir = solution_env)) next
    
    obj <- get(obj_name, envir = solution_env)
    
    # Generate tests based on object type
    test_code <- generate_type_tests(obj, obj_name, include_all_tests)
    
    if (length(test_code$scalars) > 0) {
      # Create chunk header
      chunk_name <- paste0(test_prefix, "_", sprintf("%02d", chunk_counter), "_", obj_name)
      objs_str <- paste0('"', obj_name, '"')
      scalars_str <- paste0('"', test_code$scalars, '"', collapse = ", ")
      
      chunk_header <- paste0(
        "```{r ", chunk_name, 
        ", autoharp.objs=c(", objs_str, ")",
        ", autoharp.scalars=c(", scalars_str, ")}"
      )
      
      # Create full chunk
      chunk <- c(
        chunk_header,
        paste0("# Tests for ", obj_name, " (", class(obj)[1], ")"),
        test_code$code,
        "```",
        ""
      )
      
      all_chunks <- c(all_chunks, chunk)
      chunk_counter <- chunk_counter + 1
    }
  }
  
  all_chunks
}

#' Generate type-specific test code
#' 
#' @param obj The object to test
#' @param obj_name Name of the object
#' @param include_all_tests Whether to include all tests
#' @return List with code (character vector) and scalars (character vector)
generate_type_tests <- function(obj, obj_name, include_all_tests = TRUE) {
  
  if (is.vector(obj) && !is.list(obj)) {
    if (is.numeric(obj)) {
      return(generate_numeric_vector_tests(obj_name, include_all_tests))
    } else if (is.character(obj)) {
      return(generate_character_vector_tests(obj_name, include_all_tests))
    } else if (is.logical(obj)) {
      return(generate_logical_vector_tests(obj_name, include_all_tests))
    } else {
      return(generate_generic_vector_tests(obj_name, include_all_tests))
    }
  } else if (is.data.frame(obj)) {
    return(generate_dataframe_tests(obj, obj_name, include_all_tests))
  } else if (is.matrix(obj)) {
    return(generate_matrix_tests(obj, obj_name, include_all_tests))
  } else if (is.list(obj)) {
    return(generate_list_tests(obj, obj_name, include_all_tests))
  } else if (is.function(obj)) {
    return(generate_function_tests(obj, obj_name, include_all_tests))
  } else if (is.factor(obj)) {
    return(generate_factor_tests(obj_name, include_all_tests))
  } else {
    return(generate_generic_tests(obj_name, include_all_tests))
  }
}

#' Generate tests for numeric vectors
generate_numeric_vector_tests <- function(obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_length_check"),
    paste0(prefix, "_mean_check"),
    paste0(prefix, "_identical_check")
  )
  
  code <- c(
    paste0("# Length check"),
    paste0(prefix, "_length_check <- length(", obj_name, ") == length(", dot_name, ")"),
    "",
    paste0("# Mean check (with tolerance)"),
    paste0(prefix, "_mean_check <- isTRUE(all.equal("),
    paste0("  mean(", obj_name, ", na.rm = TRUE),"),
    paste0("  mean(", dot_name, ", na.rm = TRUE),"),
    paste0("  tolerance = ", DEFAULT_NUMERIC_TOLERANCE, "))"),
    "",
    paste0("# Identical values check"),
    paste0(prefix, "_identical_check <- isTRUE(all.equal(", obj_name, ", ", dot_name, "))")
  )
  
  if (include_all_tests) {
    scalars <- c(scalars,
                 paste0(prefix, "_sd_check"),
                 paste0(prefix, "_min_check"),
                 paste0(prefix, "_max_check"),
                 paste0(prefix, "_sum_check"),
                 paste0(prefix, "_na_count_check")
    )
    
    code <- c(code,
              "",
              paste0("# Standard deviation check"),
              paste0(prefix, "_sd_check <- isTRUE(all.equal("),
              paste0("  sd(", obj_name, ", na.rm = TRUE),"),
              paste0("  sd(", dot_name, ", na.rm = TRUE),"),
              paste0("  tolerance = ", DEFAULT_NUMERIC_TOLERANCE, "))"),
              "",
              paste0("# Min check"),
              paste0(prefix, "_min_check <- isTRUE(all.equal("),
              paste0("  min(", obj_name, ", na.rm = TRUE),"),
              paste0("  min(", dot_name, ", na.rm = TRUE),"),
              paste0("  tolerance = ", DEFAULT_NUMERIC_TOLERANCE, "))"),
              "",
              paste0("# Max check"),
              paste0(prefix, "_max_check <- isTRUE(all.equal("),
              paste0("  max(", obj_name, ", na.rm = TRUE),"),
              paste0("  max(", dot_name, ", na.rm = TRUE),"),
              paste0("  tolerance = ", DEFAULT_NUMERIC_TOLERANCE, "))"),
              "",
              paste0("# Sum check"),
              paste0(prefix, "_sum_check <- isTRUE(all.equal("),
              paste0("  sum(", obj_name, ", na.rm = TRUE),"),
              paste0("  sum(", dot_name, ", na.rm = TRUE),"),
              paste0("  tolerance = ", DEFAULT_NUMERIC_TOLERANCE, "))"),
              "",
              paste0("# NA count check"),
              paste0(prefix, "_na_count_check <- sum(is.na(", obj_name, ")) == sum(is.na(", dot_name, "))")
    )
  }
  
  list(code = code, scalars = scalars)
}

#' Generate tests for character vectors
generate_character_vector_tests <- function(obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_length_check"),
    paste0(prefix, "_identical_check")
  )
  
  code <- c(
    paste0("# Length check"),
    paste0(prefix, "_length_check <- length(", obj_name, ") == length(", dot_name, ")"),
    "",
    paste0("# Identical values check"),
    paste0(prefix, "_identical_check <- identical(", obj_name, ", ", dot_name, ")")
  )
  
  if (include_all_tests) {
    scalars <- c(scalars,
                 paste0(prefix, "_unique_count_check"),
                 paste0(prefix, "_nchar_total_check")
    )
    
    code <- c(code,
              "",
              paste0("# Unique count check"),
              paste0(prefix, "_unique_count_check <- length(unique(", obj_name, ")) == length(unique(", dot_name, "))"),
              "",
              paste0("# Total character count check"),
              paste0(prefix, "_nchar_total_check <- sum(nchar(", obj_name, ")) == sum(nchar(", dot_name, "))")
    )
  }
  
  list(code = code, scalars = scalars)
}

#' Generate tests for logical vectors
generate_logical_vector_tests <- function(obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_length_check"),
    paste0(prefix, "_identical_check"),
    paste0(prefix, "_sum_true_check")
  )
  
  code <- c(
    paste0("# Length check"),
    paste0(prefix, "_length_check <- length(", obj_name, ") == length(", dot_name, ")"),
    "",
    paste0("# Identical values check"),
    paste0(prefix, "_identical_check <- identical(", obj_name, ", ", dot_name, ")"),
    "",
    paste0("# Sum of TRUE values check"),
    paste0(prefix, "_sum_true_check <- sum(", obj_name, ", na.rm = TRUE) == sum(", dot_name, ", na.rm = TRUE)")
  )
  
  if (include_all_tests) {
    scalars <- c(scalars,
                 paste0(prefix, "_na_count_check")
    )
    
    code <- c(code,
              "",
              paste0("# NA count check"),
              paste0(prefix, "_na_count_check <- sum(is.na(", obj_name, ")) == sum(is.na(", dot_name, "))")
    )
  }
  
  list(code = code, scalars = scalars)
}

#' Generate tests for generic vectors
generate_generic_vector_tests <- function(obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_length_check"),
    paste0(prefix, "_class_check"),
    paste0(prefix, "_identical_check")
  )
  
  code <- c(
    paste0("# Length check"),
    paste0(prefix, "_length_check <- length(", obj_name, ") == length(", dot_name, ")"),
    "",
    paste0("# Class check"),
    paste0(prefix, "_class_check <- identical(class(", obj_name, "), class(", dot_name, "))"),
    "",
    paste0("# Identical values check"),
    paste0(prefix, "_identical_check <- isTRUE(all.equal(", obj_name, ", ", dot_name, "))")
  )
  
  list(code = code, scalars = scalars)
}

#' Generate tests for data frames
generate_dataframe_tests <- function(obj, obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_nrow_check"),
    paste0(prefix, "_ncol_check"),
    paste0(prefix, "_colnames_check")
  )
  
  code <- c(
    paste0("# Number of rows check"),
    paste0(prefix, "_nrow_check <- nrow(", obj_name, ") == nrow(", dot_name, ")"),
    "",
    paste0("# Number of columns check"),
    paste0(prefix, "_ncol_check <- ncol(", obj_name, ") == ncol(", dot_name, ")"),
    "",
    paste0("# Column names check"),
    paste0(prefix, "_colnames_check <- setequal(names(", obj_name, "), names(", dot_name, "))")
  )
  
  if (include_all_tests) {
    # Add column type checks
    col_names <- names(obj)
    numeric_cols <- col_names[sapply(obj, is.numeric)]
    
    scalars <- c(scalars, paste0(prefix, "_coltypes_check"))
    code <- c(code,
              "",
              paste0("# Column types check"),
              paste0(prefix, "_coltypes_check <- identical("),
              paste0("  sapply(", obj_name, ", class),"),
              paste0("  sapply(", dot_name, ", class))")
    )
    
    # Add numeric column mean checks (limit to MAX_ELEMENTS_TO_TEST)
    if (length(numeric_cols) > 0) {
      for (col in numeric_cols[1:min(MAX_ELEMENTS_TO_TEST, length(numeric_cols))]) {
        safe_col <- sanitize_name(col)
        scalar_name <- paste0(prefix, "_col_", safe_col, "_mean_check")
        scalars <- c(scalars, scalar_name)
        code <- c(code,
                  "",
                  paste0("# Mean check for column '", col, "'"),
                  paste0(scalar_name, " <- isTRUE(all.equal("),
                  paste0("  mean(", obj_name, "[['", col, "']], na.rm = TRUE),"),
                  paste0("  mean(", dot_name, "[['", col, "']], na.rm = TRUE),"),
                  paste0("  tolerance = ", DEFAULT_NUMERIC_TOLERANCE, "))")
        )
      }
    }
  }
  
  list(code = code, scalars = scalars)
}

#' Generate tests for matrices
generate_matrix_tests <- function(obj, obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_nrow_check"),
    paste0(prefix, "_ncol_check"),
    paste0(prefix, "_dim_check")
  )
  
  code <- c(
    paste0("# Number of rows check"),
    paste0(prefix, "_nrow_check <- nrow(", obj_name, ") == nrow(", dot_name, ")"),
    "",
    paste0("# Number of columns check"),
    paste0(prefix, "_ncol_check <- ncol(", obj_name, ") == ncol(", dot_name, ")"),
    "",
    paste0("# Dimension check"),
    paste0(prefix, "_dim_check <- identical(dim(", obj_name, "), dim(", dot_name, "))")
  )
  
  if (include_all_tests) {
    scalars <- c(scalars,
                 paste0(prefix, "_rownames_check"),
                 paste0(prefix, "_colnames_check"),
                 paste0(prefix, "_mean_check"),
                 paste0(prefix, "_max_diff_check")
    )
    
    code <- c(code,
              "",
              paste0("# Row names check"),
              paste0(prefix, "_rownames_check <- identical("),
              paste0("  rownames(", obj_name, "),"),
              paste0("  rownames(", dot_name, "))"),
              "",
              paste0("# Column names check"),
              paste0(prefix, "_colnames_check <- identical("),
              paste0("  colnames(", obj_name, "),"),
              paste0("  colnames(", dot_name, "))"),
              "",
              paste0("# Overall mean check"),
              paste0(prefix, "_mean_check <- isTRUE(all.equal("),
              paste0("  mean(", obj_name, ", na.rm = TRUE),"),
              paste0("  mean(", dot_name, ", na.rm = TRUE),"),
              paste0("  tolerance = ", DEFAULT_NUMERIC_TOLERANCE, "))"),
              "",
              paste0("# Max element difference check"),
              paste0(prefix, "_max_diff_check <- tryCatch({"),
              paste0("  max(abs(", obj_name, " - ", dot_name, "), na.rm = TRUE) < ", DEFAULT_NUMERIC_TOLERANCE),
              paste0("}, error = function(e) FALSE)")
    )
  }
  
  list(code = code, scalars = scalars)
}

#' Generate tests for lists
generate_list_tests <- function(obj, obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_length_check"),
    paste0(prefix, "_names_check")
  )
  
  code <- c(
    paste0("# Length check"),
    paste0(prefix, "_length_check <- length(", obj_name, ") == length(", dot_name, ")"),
    "",
    paste0("# Names check"),
    paste0(prefix, "_names_check <- setequal(names(", obj_name, "), names(", dot_name, "))")
  )
  
  if (include_all_tests && !is.null(names(obj))) {
    # Add element checks for first few named elements
    elem_names <- names(obj)[1:min(MAX_ELEMENTS_TO_TEST, length(names(obj)))]
    elem_names <- elem_names[!is.na(elem_names) & elem_names != ""]
    
    for (elem in elem_names) {
      safe_elem <- sanitize_name(elem)
      scalar_name <- paste0(prefix, "_elem_", safe_elem, "_check")
      scalars <- c(scalars, scalar_name)
      code <- c(code,
                "",
                paste0("# Element '", elem, "' check"),
                paste0(scalar_name, " <- isTRUE(all.equal("),
                paste0("  ", obj_name, "[['", elem, "']],"),
                paste0("  ", dot_name, "[['", elem, "']]))")
      )
    }
  }
  
  list(code = code, scalars = scalars)
}

#' Generate tests for functions
generate_function_tests <- function(obj, obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_num_args_check"),
    paste0(prefix, "_arg_names_check")
  )
  
  code <- c(
    paste0("# Number of arguments check"),
    paste0(prefix, "_num_args_check <- length(formals(", obj_name, ")) == length(formals(", dot_name, "))"),
    "",
    paste0("# Argument names check"),
    paste0(prefix, "_arg_names_check <- setequal("),
    paste0("  names(formals(", obj_name, ")),"),
    paste0("  names(formals(", dot_name, ")))")
  )
  
  if (include_all_tests) {
    scalars <- c(scalars, paste0(prefix, "_is_function_check"))
    code <- c(code,
              "",
              paste0("# Is function check"),
              paste0(prefix, "_is_function_check <- is.function(", obj_name, ")")
    )
  }
  
  list(code = code, scalars = scalars)
}

#' Generate tests for factors
generate_factor_tests <- function(obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_length_check"),
    paste0(prefix, "_nlevels_check"),
    paste0(prefix, "_levels_check")
  )
  
  code <- c(
    paste0("# Length check"),
    paste0(prefix, "_length_check <- length(", obj_name, ") == length(", dot_name, ")"),
    "",
    paste0("# Number of levels check"),
    paste0(prefix, "_nlevels_check <- nlevels(", obj_name, ") == nlevels(", dot_name, ")"),
    "",
    paste0("# Levels match check"),
    paste0(prefix, "_levels_check <- setequal(levels(", obj_name, "), levels(", dot_name, "))")
  )
  
  if (include_all_tests) {
    scalars <- c(scalars,
                 paste0(prefix, "_level_order_check"),
                 paste0(prefix, "_frequencies_check")
    )
    
    code <- c(code,
              "",
              paste0("# Level order check"),
              paste0(prefix, "_level_order_check <- identical("),
              paste0("  levels(", obj_name, "),"),
              paste0("  levels(", dot_name, "))"),
              "",
              paste0("# Level frequencies check"),
              paste0(prefix, "_frequencies_check <- identical("),
              paste0("  as.vector(table(", obj_name, ")),"),
              paste0("  as.vector(table(", dot_name, ")))")
    )
  }
  
  list(code = code, scalars = scalars)
}

#' Generate tests for generic objects
generate_generic_tests <- function(obj_name, include_all_tests = TRUE) {
  dot_name <- paste0(".", obj_name)
  prefix <- sanitize_name(obj_name)
  
  scalars <- c(
    paste0(prefix, "_class_check"),
    paste0(prefix, "_identical_check")
  )
  
  code <- c(
    paste0("# Class check"),
    paste0(prefix, "_class_check <- identical(class(", obj_name, "), class(", dot_name, "))"),
    "",
    paste0("# Identical check"),
    paste0(prefix, "_identical_check <- isTRUE(all.equal(", obj_name, ", ", dot_name, "))")
  )
  
  list(code = code, scalars = scalars)
}