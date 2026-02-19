# Helper functions for Script Similarity Analysis App

# maximum number of TreeHarp objects to compare per file to prevent exponential computation
# tree kernel computation has O(n*m) complexity where n and m are the number of trees
# setting a reasonable limit prevents hanging on files with many expressions
MAX_AST_TREES <- 50

# timeout in seconds for each individual K2 tree kernel call
# prevents hanging on deeply nested or complex tree structures
K2_TIMEOUT_SECS <- 5

#' Extract code chunks from RMD or QMD files
#' 
#' @param file_path Path to RMD or QMD file
#' @return Character string containing all extracted code
extract_code_chunks <- function(file_path) {
  tryCatch({
    # Use autoharp's extract_chunks function if available
    if (requireNamespace("autoharp", quietly = TRUE)) {
      chunks <- autoharp::extract_chunks(file_path)
      
      if (is.null(chunks) || length(chunks) == 0) {
        warning(paste("No code chunks found in", file_path))
        return("")
      }
      
      # Extract code from each chunk (excluding chunk headers and footers)
      code_lines <- character(0)
      for (chunk in chunks) {
        # Chunks include header and footer, extract only the code
        if (length(chunk) > 2) {
          chunk_code <- chunk[2:(length(chunk) - 1)]
          code_lines <- c(code_lines, chunk_code)
        }
      }
      
      # Combine all code lines
      if (length(code_lines) > 0) {
        return(paste(code_lines, collapse = "\n"))
      } else {
        return("")
      }
    } else {
      # Fallback to knitr-based extraction
      lines <- readLines(file_path, warn = FALSE)
      
      if (!requireNamespace("knitr", quietly = TRUE)) {
        stop("knitr package is required for chunk extraction")
      }
      
      # Use base R grepl instead of stringr::str_detect
      begin_ids <- which(grepl(knitr::all_patterns$md$chunk.begin, lines))
      end_ids <- which(grepl(knitr::all_patterns$md$chunk.end, lines))
      
      if (length(begin_ids) == 0 || length(end_ids) == 0) {
        warning(paste("No code chunks found in", file_path))
        return("")
      }
      
      if (length(begin_ids) != length(end_ids)) {
        warning(paste("Malformed or mismatched code chunks in", file_path))
        return("")
      }
      
      code_lines <- character(0)
      for (i in seq_along(begin_ids)) {
        if (end_ids[i] > begin_ids[i] + 1) {
          chunk_code <- lines[(begin_ids[i] + 1):(end_ids[i] - 1)]
          code_lines <- c(code_lines, chunk_code)
        }
      }
      
      if (length(code_lines) > 0) {
        return(paste(code_lines, collapse = "\n"))
      } else {
        return("")
      }
    }
  }, error = function(e) {
    warning(paste("Error extracting code chunks from", file_path, ":", e$message))
    return("")
  })
}

#' Parse R script to tokens
#' 
#' @param file_path Path to R script
#' @param include_actuals Whether to include actual arguments/literals (default TRUE)
#' @param ngram_size Size of n-grams to generate (default 1 for unigrams)
#' @param exclude_library_calls Whether to exclude library function calls (default TRUE)
#' @return Character vector of tokens
parse_script_tokens <- function(file_path, include_actuals = TRUE, ngram_size = 1, exclude_library_calls = TRUE) {
  # Check if autoharp is available for more sophisticated token parsing
  if (requireNamespace("autoharp", quietly = TRUE)) {
    # Use autoharp's rmd_to_token_count for better token extraction
    token_df <- tryCatch({
      autoharp::rmd_to_token_count(file_path, include_actuals = include_actuals)
    }, error = function(e) {
      NULL
    })
    
    if (!is.null(token_df) && nrow(token_df) > 0) {
      # Extract tokens from the data frame
      tokens <- rep(token_df$token, token_df$n)
      
      # Exclude library function calls if requested
      if (exclude_library_calls && requireNamespace("autoharp", quietly = TRUE)) {
        # Get libraries used in the file
        libraries <- tryCatch({
          autoharp::get_libraries(file_path)
        }, error = function(e) {
          character(0)
        })
        
        if (length(libraries) > 0) {
          # Get function names from each library and exclude them from tokens
          library_fns <- character(0)
          for (lib in libraries) {
            tryCatch({
              lib_fns <- ls(envir=getNamespace(lib))
              library_fns <- c(library_fns, lib_fns)
            }, error = function(e) {
              NULL
            })
          }
          
          # remove library function names from tokens
          if (length(library_fns) > 0) {
            tokens <- tokens[!tokens %in% library_fns]
          }
        }
      }
      
      # Generate n-grams if requested
      if (ngram_size > 1 && length(tokens) >= ngram_size) {
        tokens <- generate_ngrams(tokens, ngram_size)
      }
      return(tokens)
    }
  }
}

#' Generate n-grams from a token vector
#' 
#' @param tokens Character vector of tokens
#' @param ngram_size Size of n-grams
#' @return Character vector of n-grams (concatenated with underscore)
generate_ngrams <- function(tokens, ngram_size = 2) {
  if (length(tokens) < ngram_size) {
    return(character(0))
  }
  
  ngrams <- character(length(tokens) - ngram_size + 1)
  for (i in 1:(length(tokens) - ngram_size + 1)) {
    ngrams[i] <- paste(tokens[i:(i + ngram_size - 1)], collapse = "_")
  }
  
  return(ngrams)
}

#' Calculate cosine similarity between two token vectors using TF-IDF
#' 
#' @param tokens1 First token vector
#' @param tokens2 Second token vector
#' @param all_tokens Optional list of all token vectors for IDF calculation
#' @return Cosine similarity score (0-1)
cosine_similarity <- function(tokens1, tokens2, all_tokens = NULL) {
  # Handle empty token vectors
  if (length(tokens1) == 0 || length(tokens2) == 0) {
    return(0)
  }
  
  # If all_tokens is provided, use TF-IDF weighting
  if (!is.null(all_tokens) && length(all_tokens) > 1) {
    # Create vocabulary from all documents
    vocab <- unique(unlist(all_tokens))
    n_docs <- length(all_tokens)
    
    # Calculate document frequencies (how many documents contain each term)
    doc_freq <- sapply(vocab, function(term) {
      sum(sapply(all_tokens, function(doc) term %in% doc))
    })
    
    # Calculate IDF: log(N / df)
    # Use pmax to ensure we never divide by zero (though this shouldn't happen with current logic)
    idf <- log(n_docs / pmax(doc_freq, 1))
    
    # Calculate TF for both documents
    tf1 <- sapply(vocab, function(t) sum(tokens1 == t))
    tf2 <- sapply(vocab, function(t) sum(tokens2 == t))
    
    # Apply TF-IDF weighting
    vec1 <- tf1 * idf
    vec2 <- tf2 * idf
  } else {
    # Fallback to simple term frequency if IDF data not available
    vocab <- unique(c(tokens1, tokens2))
    vec1 <- sapply(vocab, function(t) sum(tokens1 == t))
    vec2 <- sapply(vocab, function(t) sum(tokens2 == t))
  }
  
  # Calculate cosine similarity
  dot_product <- sum(vec1 * vec2)
  norm1 <- sqrt(sum(vec1^2))
  norm2 <- sqrt(sum(vec2^2))
  
  if (norm1 == 0 || norm2 == 0) return(0)
  
  similarity <- dot_product / (norm1 * norm2)
  return(similarity)
}

#' Compute cosine similarity matrix for all documents at once (vectorized)
#' 
#' This function computes the entire similarity matrix in one go using vectorized
#' operations, which is much faster than computing pairwise similarities one by one.
#' 
#' @param all_tokens List of token vectors for all documents
#' @param use_tfidf Whether to use TF-IDF weighting (default TRUE)
#' @return Similarity matrix (n x n)
cosine_similarity_matrix <- function(all_tokens, use_tfidf = TRUE) {
  n_docs <- length(all_tokens)
  
  # Create vocabulary from all documents
  vocab <- unique(unlist(all_tokens))
  n_terms <- length(vocab)
  
  if (n_terms == 0) {
    # Return matrix of zeros if no tokens
    sim_matrix <- matrix(0, nrow = n_docs, ncol = n_docs)
    diag(sim_matrix) <- 1
    return(sim_matrix)
  }
  
  # Build document-term matrix (DTM)
  # Rows = documents, Columns = terms
  dtm <- matrix(0, nrow = n_docs, ncol = n_terms)
  
  for (i in 1:n_docs) {
    if (length(all_tokens[[i]]) > 0) {
      # Count term frequencies for this document
      token_counts <- table(factor(all_tokens[[i]], levels = vocab))
      dtm[i, ] <- as.numeric(token_counts)
    }
  }
  
  # Apply TF-IDF weighting if requested
  # Note: TF-IDF requires at least 2 documents for meaningful IDF calculation
  # With n_docs=1, log(1/1)=0, making all TF-IDF values 0
  if (use_tfidf && n_docs > 1) {
    # Calculate document frequency (number of documents containing each term)
    doc_freq <- colSums(dtm > 0)
    
    # Calculate IDF: log(N / df)
    # Use pmax to ensure we never divide by zero
    idf <- log(n_docs / pmax(doc_freq, 1))
    
    # Apply IDF weighting to each row (document)
    # Use sweep to multiply each column by its IDF value
    dtm <- sweep(dtm, 2, idf, "*")
  }
  
  # compute cosine similarity matrix using vectorized operations
  # cosine(i, j) = dot(i, j) / (norm(i) * norm(j))
  
  # Step 1: Compute norms for each document (row)
  norms <- sqrt(rowSums(dtm^2))
  
  # Step 2: Handle zero-norm documents (empty documents)
  # documents with zero norm will have 0 similarity with all others (except self)
  zero_norm_docs <- norms == 0
  
  # Step 3: Compute all pairwise dot products at once
  # dtm %*% t(dtm) gives us all dot products in one operation
  dot_products <- tcrossprod(dtm)  # Equivalent to dtm %*% t(dtm) but faster
  
  # Step 4: Compute outer product of norms to get normalization factors
  norm_products <- outer(norms, norms)
  
  # Step 5: Compute cosine similarities
  # Avoid division by zero by setting zero denominators to 1
  # This will give 0 similarity (since numerator is also 0)
  norm_products[norm_products == 0] <- 1
  sim_matrix <- dot_products / norm_products
  
  # Handle any NaN values (shouldn't occur with above fix, but be safe)
  sim_matrix[is.nan(sim_matrix)] <- 0
  
  # Ensure diagonal is exactly 1 (self-similarity)
  # Even for zero-norm documents, self-similarity is defined as 1
  diag(sim_matrix) <- 1
  
  # Ensure symmetry (should already be symmetric, but numerical errors can occur)
  sim_matrix <- (sim_matrix + t(sim_matrix)) / 2
  
  return(sim_matrix)
}

#' Calculate Jaccard similarity between two token sets
#' 
#' @param tokens1 First token vector
#' @param tokens2 Second token vector
#' @return Jaccard similarity score (0-1)
jaccard_similarity <- function(tokens1, tokens2) {
  set1 <- unique(tokens1)
  set2 <- unique(tokens2)
  
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  
  if (union == 0) return(0)
  
  return(intersection / union)
}

#' Calculate edit distance-based similarity
#' 
#' @param file1 First file path
#' @param file2 Second file path
#' @return Normalized edit distance similarity (0-1)
edit_distance_similarity <- function(file1, file2) {
  tryCatch({
    # Check if files are RMD or QMD
    file1_ext <- tolower(tools::file_ext(file1))
    file2_ext <- tolower(tools::file_ext(file2))
    
    # Extract code if RMD/QMD, otherwise read normally
    if (file1_ext %in% c("rmd", "qmd")) {
      code1 <- extract_code_chunks(file1)
    } else {
      code1 <- paste(readLines(file1, warn = FALSE), collapse = "\n")
    }
    
    if (file2_ext %in% c("rmd", "qmd")) {
      code2 <- extract_code_chunks(file2)
    } else {
      code2 <- paste(readLines(file2, warn = FALSE), collapse = "\n")
    }
    
    # Handle empty code
    if (nchar(code1) == 0 && nchar(code2) == 0) {
      return(1)  # Both empty, consider them identical
    }
    if (nchar(code1) == 0 || nchar(code2) == 0) {
      return(0)  # One empty, one not, consider them completely different
    }
    
    # Calculate Levenshtein distance
    distance <- adist(code1, code2)[1, 1]
    
    # Normalize by max length
    max_len <- max(nchar(code1), nchar(code2))
    if (max_len == 0) return(0)
    
    similarity <- 1 - (distance / max_len)
    return(max(0, similarity))
  }, error = function(e) {
    warning(paste("Error calculating edit distance:", e$message))
    return(0)
  })
}

#' Tree kernel function for comparing sets of trees
#' 
#' Computes normalized tree kernel similarity between two sets of TreeHarp objects.
#' This function handles the entire computation including pairwise kernel calculations
#' and normalization by self-similarities.
#' 
#' @param trees1 A list of TreeHarp objects or a single TreeHarp object
#' @param trees2 A list of TreeHarp objects or a single TreeHarp object
#' @return Normalized kernel similarity score (0-1)
tree_kernel <- function(trees1, trees2) {
  if (!requireNamespace("autoharp", quietly = TRUE)) {
    stop("autoharp package is required for tree kernel computation")
  }
  # Ensure inputs are lists
  if (!is.list(trees1)) {
    trees1 <- list(trees1)
  }
  if (!is.list(trees2)) {
    trees2 <- list(trees2)
  }

  # Handle empty inputs
  if (length(trees1) == 0 || length(trees2) == 0) {
    return(0)
  }

  # Safe wrapper around K2 that enforces a timeout per call
  safe_K2 <- function(t1, t2) {
    setTimeLimit(cpu = K2_TIMEOUT_SECS, elapsed = K2_TIMEOUT_SECS, transient = TRUE)
    on.exit(setTimeLimit(cpu = Inf, elapsed = Inf, transient = FALSE), add = TRUE)
    tryCatch(
      autoharp::K2(t1, t2, verbose = FALSE),
      error = function(e) {
        warning(paste("K2 call failed or timed out:", e$message))
        0
      }
    )
  }

  # Calculate pairwise kernel values and aggregate
  total_kernel <- 0
  for (t1 in trees1) {
    for (t2 in trees2) {
      total_kernel <- total_kernel + safe_K2(t1, t2)
    }
  }

  # Normalize by self-similarities
  norm1_sum <- 0
  for (t1 in trees1) {
    norm1_sum <- norm1_sum + safe_K2(t1, t1)
  }

  norm2_sum <- 0
  for (t2 in trees2) {
    norm2_sum <- norm2_sum + safe_K2(t2, t2)
  }

  # Return normalized similarity
  if (norm1_sum == 0 || norm2_sum == 0) {
    return(0)
  }

  similarity <- total_kernel / sqrt(norm1_sum * norm2_sum)
  return(max(0, min(1, similarity)))
}

#' Calculate AST-based similarity using autoharp's tree structures
#' 
#' @param file1 First file path
#' @param file2 Second file path
#' @return AST similarity score (0-1)
ast_similarity <- function(file1, file2) {
  print(file1)
  print(file2)
  tryCatch({
    # Check if files are RMD or QMD
    file1_ext <- tolower(tools::file_ext(file1))
    file2_ext <- tolower(tools::file_ext(file2))
    
    # Extract code if RMD/QMD, otherwise parse file directly
    if (file1_ext %in% c("rmd", "qmd")) {
      code1 <- extract_code_chunks(file1)
      # Handle empty code chunks (warning already issued by extract_code_chunks)
      if (code1 == "") {
        return(0)
      }
      expr1 <- parse(text = code1, keep.source = FALSE)
    } else {
      expr1 <- parse(file1, keep.source = FALSE)
    }
    
    if (file2_ext %in% c("rmd", "qmd")) {
      code2 <- extract_code_chunks(file2)
      # Handle empty code chunks (warning already issued by extract_code_chunks)
      if (code2 == "") {
        return(0)
      }
      expr2 <- parse(text = code2, keep.source = FALSE)
    } else {
      expr2 <- parse(file2, keep.source = FALSE)
    }
    
    # Try to use autoharp's tree kernel if available
    if (requireNamespace("autoharp", quietly = TRUE)) {
      # Convert each expression to trees and calculate similarities
      # Use TreeHarp constructor with quote_arg=TRUE for language objects
      trees1 <- lapply(seq_along(expr1), function(i) {
        tryCatch({
          autoharp::TreeHarp(expr1[[i]], quote_arg = TRUE)
        }, error = function(e) {
          warning(paste("Failed to create TreeHarp for expression in file1:", e$message))
          NULL
        })
      })
      
      trees2 <- lapply(seq_along(expr2), function(i) {
        tryCatch({
          autoharp::TreeHarp(expr2[[i]], quote_arg = TRUE)
        }, error = function(e) {
          warning(paste("Failed to create TreeHarp for expression in file2:", e$message))
          NULL
        })
      })
      
      # Filter out NULL values (failed conversions)
      trees1 <- Filter(Negate(is.null), trees1)
      trees2 <- Filter(Negate(is.null), trees2)

      # Filter out excessively large trees that can cause K2 to hang
      # Trees with too many nodes lead to exponential computation
      safe_tree_size <- function(tree) {
        tryCatch({
          n <- length(tree)
          !is.null(n) && n <= 500
        }, error = function(e) TRUE)
      }
      trees1 <- Filter(safe_tree_size, trees1)
      trees2 <- Filter(safe_tree_size, trees2)
      
      # Handle case where no valid trees were created
      if (length(trees1) == 0 || length(trees2) == 0) {
        warning("No valid TreeHarp objects created, falling back to token similarity")
        tokens1 <- parse_script_tokens(file1)
        tokens2 <- parse_script_tokens(file2)
        return(cosine_similarity(tokens1, tokens2))
      }
      
      # Limit the number of trees to prevent hanging on large files
      # If we have too many trees, the computation becomes exponentially expensive
      if (length(trees1) > MAX_AST_TREES || length(trees2) > MAX_AST_TREES) {
        warning(sprintf("Too many expressions detected (%d, %d). Limiting to %d trees per file to prevent hanging.",
                       length(trees1), length(trees2), MAX_AST_TREES))
        # Keep first MAX_AST_TREES trees
        # We take the first trees rather than random sampling to ensure deterministic results
        # The first expressions typically contain key initialization and setup code
        if (length(trees1) > MAX_AST_TREES) {
          trees1 <- trees1[1:MAX_AST_TREES]
        }
        if (length(trees2) > MAX_AST_TREES) {
          trees2 <- trees2[1:MAX_AST_TREES]
        }
      }
      
      # Use the improved tree_kernel function which handles sets of trees
      similarity <- tree_kernel(trees1, trees2)
      print(similarity)
      return(similarity)
    } else {
      # Fallback: use token-based similarity
      tokens1 <- parse_script_tokens(file1)
      tokens2 <- parse_script_tokens(file2)
      return(cosine_similarity(tokens1, tokens2))
    }
  }, error = function(e) {
    warning(paste("Error in AST similarity:", e$message))
    # Fallback to token-based
    tokens1 <- parse_script_tokens(file1)
    tokens2 <- parse_script_tokens(file2)
    return(cosine_similarity(tokens1, tokens2))
  })
}

#' calculate pairwise similarity matrix for all scripts
#' 
#' @param file_paths Character vector of file paths
#' @param method Similarity method: "cosine", "jaccard", "edit", "ast"
#' @param progress_callback Optional function to report progress
#' @param ngram_size Size of n-grams to use for cosine similarity (default 1)
#' @param include_actuals Whether to include actual arguments/literals for cosine similarity (default TRUE)
#' @param exclude_library_calls Whether to exclude library function calls (default TRUE)
#' @return Similarity matrix
calculate_similarity_matrix <- function(file_paths, method = "cosine", 
                                       progress_callback = NULL,
                                       ngram_size = 1,
                                       include_actuals = TRUE,
                                       exclude_library_calls = TRUE) {
  n <- length(file_paths)
  sim_matrix <- matrix(0, nrow = n, ncol = n)
  rownames(sim_matrix) <- basename(file_paths)
  colnames(sim_matrix) <- basename(file_paths)
  
  # Diagonal is always 1 (self-similarity)
  diag(sim_matrix) <- 1
  
  # For cosine similarity, use optimized vectorized matrix computation
  if (method == "cosine") {
    # Pre-parse all tokens with the specified parameters
    if (!is.null(progress_callback)) {
      progress_callback(0.1)
    }
    
    all_tokens <- lapply(file_paths, function(fp) {
      parse_script_tokens(fp, include_actuals = include_actuals, ngram_size = ngram_size, 
                          exclude_library_calls = exclude_library_calls)
    })
    
    if (!is.null(progress_callback)) {
      progress_callback(0.3)
    }
    
    # use vectorized cosine similarity matrix computation
    # much faster than computing pairwise similarities one by one
    sim_matrix <- cosine_similarity_matrix(all_tokens, use_tfidf = TRUE)
    
    # set row and column names
    rownames(sim_matrix) <- basename(file_paths)
    colnames(sim_matrix) <- basename(file_paths)
    
    if (!is.null(progress_callback)) {
      progress_callback(1.0)
    }
    
    return(sim_matrix)
  }
  
  # For other methods, use pairwise computation
  # Calculate pairwise similarities
  total_pairs <- n * (n - 1) / 2
  current_pair <- 0
  
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      current_pair <- current_pair + 1
      
      if (!is.null(progress_callback)) {
        progress_callback(current_pair / total_pairs)
      }
      
      similarity <- switch(method,
        "jaccard" = {
          tokens1 <- parse_script_tokens(file_paths[i])
          tokens2 <- parse_script_tokens(file_paths[j])
          jaccard_similarity(tokens1, tokens2)
        },
        "edit" = {
          edit_distance_similarity(file_paths[i], file_paths[j])
        },
        "ast" = {
          ast_similarity(file_paths[i], file_paths[j])
        },
        stop("Unknown similarity method")
      )
      
      sim_matrix[i, j] <- similarity
      sim_matrix[j, i] <- similarity
    }
  }
  
  return(sim_matrix)
}

#' Convert similarity matrix to distance matrix
#' 
#' @param sim_matrix Similarity matrix (values 0-1)
#' @return Distance matrix
similarity_to_distance <- function(sim_matrix) {
  # Convert similarity (0-1) to distance
  # distance = 1 - similarity ensures 0 distance means identical
  dist_matrix <- 1 - sim_matrix
  
  # Ensure diagonal is exactly 0
  diag(dist_matrix) <- 0
  
  # Ensure symmetry (should already be symmetric)
  dist_matrix <- (dist_matrix + t(dist_matrix)) / 2
  
  return(dist_matrix)
}

#' Perform t-SNE dimensionality reduction
#' 
#' @param dist_matrix Distance matrix
#' @param perplexity t-SNE perplexity parameter
#' @param max_iter Maximum iterations
#' @return Data frame with 2D coordinates
perform_tsne <- function(dist_matrix, perplexity = NULL, max_iter = 1000) {
  if (!requireNamespace("Rtsne", quietly = TRUE)) {
    stop("Package 'Rtsne' is required for t-SNE. Install it with: install.packages('Rtsne')")
  }
  
  n <- nrow(dist_matrix)
  
  # Auto-adjust perplexity if not specified or invalid
  if (is.null(perplexity)) {
    perplexity <- min(30, max(1, floor((n - 1) / 3)))
  } else {
    # Ensure perplexity is valid
    perplexity <- min(perplexity, (n - 1) / 3)
    perplexity <- max(1, perplexity)
  }
  
  # Handle small datasets
  if (n < 4) {
    warning("Dataset too small for t-SNE. Using simple 2D projection.")
    # For very small datasets, use MDS or simple projection
    if (n == 2) {
      return(data.frame(
        X = c(0, dist_matrix[1, 2]),
        Y = c(0, 0),
        Script = rownames(dist_matrix)
      ))
    } else {
      # Use cmdscale for n=3
      mds_result <- cmdscale(as.dist(dist_matrix), k = 2)
      return(data.frame(
        X = mds_result[, 1],
        Y = mds_result[, 2],
        Script = rownames(dist_matrix)
      ))
    }
  }
  
  tryCatch({
    # Set seed for reproducibility
    set.seed(42)
    
    # Run t-SNE
    tsne_result <- Rtsne::Rtsne(
      as.dist(dist_matrix),
      dims = 2,
      perplexity = perplexity,
      max_iter = max_iter,
      is_distance = TRUE,
      check_duplicates = FALSE
    )
    
    result_df <- data.frame(
      X = tsne_result$Y[, 1],
      Y = tsne_result$Y[, 2],
      Script = rownames(dist_matrix)
    )
    
    return(result_df)
  }, error = function(e) {
    warning(paste("t-SNE failed:", e$message, "- falling back to MDS"))
    return(perform_mds(dist_matrix))
  })
}

#' Perform MDS (Multi-Dimensional Scaling)
#' 
#' @param dist_matrix Distance matrix
#' @return Data frame with 2D coordinates
perform_mds <- function(dist_matrix) {
  tryCatch({
    # classical MDS
    mds_result <- cmdscale(as.dist(dist_matrix), k = 2)
    
    result_df <- data.frame(
      X = mds_result[, 1],
      Y = mds_result[, 2],
      Script = rownames(dist_matrix)
    )
    
    return(result_df)
  }, error = function(e) {
    stop(paste("MDS failed:", e$message))
  })
}

#' Identify clusters of similar scripts
#' 
#' @param sim_matrix Similarity matrix
#' @param threshold Similarity threshold for clustering (0-1)
#' @return List of clusters (each cluster is a vector of script indices)
identify_clusters <- function(sim_matrix, threshold = 0.7) {
  n <- nrow(sim_matrix)
  
  # Create adjacency matrix based on threshold
  adj_matrix <- sim_matrix >= threshold
  diag(adj_matrix) <- FALSE  # Remove self-connections
  
  # Find connected components
  visited <- rep(FALSE, n)
  clusters <- list()
  
  for (i in 1:n) {
    if (!visited[i]) {
      # Start new cluster
      cluster <- c(i)
      visited[i] <- TRUE
      queue <- c(i)
      
      while (length(queue) > 0) {
        current <- queue[1]
        queue <- queue[-1]
        
        # Find neighbors
        neighbors <- which(adj_matrix[current, ] & !visited)
        
        if (length(neighbors) > 0) {
          cluster <- c(cluster, neighbors)
          visited[neighbors] <- TRUE
          queue <- c(queue, neighbors)
        }
      }
      
      # Only keep clusters with more than 1 member
      if (length(cluster) > 1) {
        clusters[[length(clusters) + 1]] <- cluster
      }
    }
  }
  
  # Assign cluster IDs
  cluster_ids <- rep(0, n)
  for (i in seq_along(clusters)) {
    cluster_ids[clusters[[i]]] <- i
  }
  
  return(list(
    clusters = clusters,
    cluster_ids = cluster_ids,
    labels = rownames(sim_matrix)
  ))
}

#' Get top N most similar pairs
#' 
#' @param sim_matrix Similarity matrix
#' @param n Number of pairs to return
#' @return Data frame with top similar pairs
get_top_similar_pairs <- function(sim_matrix, n = 10) {
  # get upper triangle (excluding diagonal)
  upper_tri_indices <- which(upper.tri(sim_matrix), arr.ind = TRUE)
  
  # get similarity values
  similarities <- sim_matrix[upper_tri_indices]
  
  # sort by similarity (descending)
  sorted_indices <- order(similarities, decreasing = TRUE)
  
  # take top n
  top_n <- min(n, length(sorted_indices))
  top_indices <- sorted_indices[1:top_n]
  
  result <- data.frame(
    Script1 = rownames(sim_matrix)[upper_tri_indices[top_indices, 1]],
    Script2 = rownames(sim_matrix)[upper_tri_indices[top_indices, 2]],
    Similarity = round(similarities[top_indices], 4),
    stringsAsFactors = FALSE
  )
  
  return(result)
}

#' Create a heatmap-friendly version of similarity matrix
#' 
#' @param sim_matrix Similarity matrix
#' @return Data frame in long format for heatmap
prepare_heatmap_data <- function(sim_matrix) {
  n <- nrow(sim_matrix)
  
  # Convert to long format
  heatmap_data <- data.frame(
    Script1 = rep(rownames(sim_matrix), each = n),
    Script2 = rep(rownames(sim_matrix), times = n),
    Similarity = as.vector(sim_matrix),
    stringsAsFactors = FALSE
  )
  
  return(heatmap_data)
}

#' calculate summary statistics for similarity matrix
#' 
#' @param sim_matrix Similarity matrix
#' @return List of summary statistics
calculate_similarity_stats <- function(sim_matrix) {
  # Get upper triangle (excluding diagonal)
  upper_tri <- sim_matrix[upper.tri(sim_matrix)]
  
  if (length(upper_tri) == 0) {
    return(list(
      mean = NA,
      median = NA,
      sd = NA,
      min = NA,
      max = NA,
      high_similarity_count = 0,
      medium_similarity_count = 0,
      low_similarity_count = 0
    ))
  }
  
  list(
    mean = mean(upper_tri, na.rm = TRUE),
    median = median(upper_tri, na.rm = TRUE),
    sd = sd(upper_tri, na.rm = TRUE),
    min = min(upper_tri, na.rm = TRUE),
    max = max(upper_tri, na.rm = TRUE),
    high_similarity_count = sum(upper_tri >= 0.7, na.rm = TRUE),
    medium_similarity_count = sum(upper_tri >= 0.4 & upper_tri < 0.7, na.rm = TRUE),
    low_similarity_count = sum(upper_tri < 0.4, na.rm = TRUE)
  )
}
