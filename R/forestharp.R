#### from rmd/r to forest-harp

#' Convert to TreeHarp objects
#'
#' Reads in an Rmd file or an R script and converts it to a list of TreeHarp
#' objects.
#'
#' @param fname The filename that is to be read in.
#' @param line_nums A logical value, indicating if the line numbers of
#' expressions should be returned along with the expressions. By default, this 
#' value is FALSE.
#'
#' @return A list of TreeHarp objects, or a list with 2 components containing
#' the TreeHarp objects and a vector of line numbers.
#'
#' @details The TreeHarp constructor is wrapped in a tryCatch loop, so that it
#' does not fail if an expression could not be converted to a TreeHarp object.
#' 
#' The object returned is not a specially defined class. It is either a list of 
#' length 2, or a list of TreeHarp objects. This output is meant to be used 
#' with \code{\link{fapply}}.
#' 
#' If the input file is an Rmd file (checked with \link{extract_chunks}), then the
#' chunks are extracted and converted to TreeHarp objects. If the input file is 
#' not an Rmd, it is assumed to be an R script. This script is then supplied to 
#' \link{parse}. In either case, a parsing error here could cause the function to 
#' fail.
#' 
#' Line numbers are extracted using \code{\link{get_source_expressions}} from
#' the lintr package.
#' 
#' @export
#' @seealso \code{\link{fapply}}, \code{\link{extract_chunks}},
#' \code{\link{extract_chunks}}, \code{\link[lintr]{get_source_expressions}}
rmd_to_forestharp <- function(fname, line_nums = FALSE) {
  #all_chunks <- extract_chunks(fname)
  all_chunks <- tryCatch(extract_chunks(fname), error = function(e) return(e))
  if("error" %in% class(all_chunks)){
      return(NA)
  }

  if(!is.null(all_chunks)){
    all_chunks <- Filter(function(x) length(x) > 2, all_chunks)
    all_lines <- lapply(all_chunks, function(x) x[2:(length(x)-1)])
    all_exp <- tryCatch(str2expression(unlist(all_lines)), error = function(e) return(e))
    if("error" %in% class(all_exp)){
        return(NA)
    }
  } else {
    all_exp <- parse(fname, keep.source = FALSE)
  }

  all_trees <- lapply(all_exp, function(x)
    tryCatch(TreeHarp(x, TRUE), error=function(e) return(NA)))
# out is of class fapply
  all_classes <- sapply(all_trees, class)
  if(any(all_classes != "TreeHarp")){
    message("Some expressions were not parsed.")
  }

  if(line_nums) {
    lint_out <- lintr::get_source_expressions(fname)
    lint_out <- lint_out[[1]][-length(lint_out[[1]])]
    lint_out <- Filter(function(x)
      x$parsed_content$token[1] != "COMMENT" || nrow(x$parsed_content) > 1,
      lint_out)
    lint_out <- Filter(function(x)
      x$parsed_content$token[1] != "';'" || nrow(x$parsed_content) > 1,
      lint_out)
    line_nums <- sapply(lint_out, function(x) x$line)
    return(list(forest=all_trees, line_nums=line_nums))
    #return(lint_out)
  }
  all_trees
}

#### forest-harp apply

#' Apply a function to a forest of trees.
#'
#' A convenience function, for applying a function to many trees.
#'
#' @param fharp The output of rmd_to_forestharp. It could also just be a 
#' list of TreeHarp objects.
#' @param TFUN A function that works on a single TreeHarp and returns an output. 
#' See \link{forestharp-helpers} for examples.
#' @param combine A logical value that indicates if the output from all function
#' applications should be combined.
#' @param combiner_fn A function to use to combine the individual output from 
#' each tree into a single scalar for each forest. It should handle NA 
#' values in the input vector or list. If it is missing, it defaults to 
#' sum, with na.rm=TRUE.
#' @param ... Additional arguments to be passed on to TFUN.
#' 
#' @details The input is simply a list of TreeHarp objects. First, the TFUN 
#' function is lapply-ed to each TreeHarp item, resulting in either a list, 
#' or a vector with possible NA elements.
#' 
#' The combiner function should be aware of this sort of output, and summarise
#' the list or vector accordingly, handling NA's and returning a \emph{scalar}.
#' 
#' If you need to create a partial function out of a forestharp helper, 
#' use an anonymous function, as shown in the examples below.
#'
#' @return A vector, list or a single value. If TFUN returned an error for a 
#' particular TreeHarp, that component in the list or vector would be NA. This 
#' input vector or list will then be combined by combiner_fn.
#' @export
#' 
#' @examples 
#' 
#' ex1 <- quote(X <- rnorm(10, mean=0.9, sd=4))
#' ex2 <- quote(Y <- rbeta(10, shape1=3, shape2=5))
#' f1 <- lapply(c(ex1, ex2), TreeHarp, quote_arg=TRUE) 
#' 
#' # returns all function calls that begin with "r", like rnorm and rbeta.
#' # calls are returned as a list.
#' fapply(f1, extract_fn_call, combine =FALSE, pattern="^r.*")
#' 
#' # list is catenated.
#' fapply(f1, extract_fn_call, combine =TRUE, pattern="^r.*", 
#'        combiner_fn = function(x) {paste0(unlist(x), collapse=",")})
#'
fapply <-  function(fharp, TFUN, combine = TRUE, combiner_fn, ...) {
  if("line_nums" %in% names(fharp)){
    fharp <- fharp$forest
  }
  
  f_wrap <- function(x, ...) {
    if(class(x)!= "TreeHarp"){
      return(NA)
    }
    # trycatch it here.
    tryCatch(TFUN(x, ...), error = function(e) return(NA))
  }
  out_list <- lapply(fharp, f_wrap, ...)
  if(combine) {
    if(missing(combiner_fn)){
      return(sum(unlist(out_list), na.rm = TRUE))
    } else {
      out <- combiner_fn(out_list)
      return(out)
    }
  } else {
    return(out_list)
  }
}
