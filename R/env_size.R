#' Calculates the Total Memory Used
#'
#' This function uses object_size from the pryr package to compute the total
#' amount of memory used by objects in an environment.
#'
#' @param env The environment whose size is to be computed.
#'
#' @return The size in bytes, as a numeric value (scalar).
#' @export
#' @details The names are wrapped in backticks. Otherwise, non-syntactic
#' names will cause problems. This function is used within
#' \code{\link{render_one}} as part of the runtime stats assessment.
#'
#' @seealso \code{\link[pryr]{object_size}}
#' @examples
#'
#' e1 <- new.env()
#' env_size(e1)
#' evalq(x <- 1:10000L, e1)
#' env_size(e1)
env_size <- function(env) {
  #if(!requireNamespace("pryr", quietly=TRUE)) {
  #  stop("Please install pryr before using this function. env_size() requires
  #       it.")
  #}
  size_byte <- pryr::object_size(mget(ls(envir = env), envir = env))
  as.numeric(size_byte)
#  obj_names <- ls(envir = env)

#  #obj_size_expr <- paste0("pryr::object_size(",
#  #                        paste(obj_names, collapse=","),
#  #                        ")")
#  if(length(obj_names) == 0L) {
#    obj_size_expr <- paste0("pryr::object_size(",
#                           paste(obj_names, collapse=","),
#                           ")")
#  } else {
#    obj_size_expr <- paste0("pryr::object_size(",
#                            paste0("`",obj_names, "`", collapse=","),
#                            ")")
#  }
#  size_bytes <- eval(str2lang(obj_size_expr), envir = env)
#  #tmp <- 1:1000L
#  #size_bytes <- object_size(tmp)
#  as.numeric(size_bytes)
}
