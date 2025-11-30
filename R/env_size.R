#' Calculates the Total Memory Used
#'
#' This function uses the utils package to compute the total
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
#' @examples
#'
#' e1 <- new.env()
#' env_size(e1)
#' evalq(x <- 1:10000L, e1)
#' env_size(e1)

env_size <- function(env) {
  obj_names <- ls(envir = env, all.names = TRUE)
  if (length(obj_names) == 0L) return(0)
  objs <- mget(obj_names, envir = env, inherits = FALSE)
  size_byte <- sum(vapply(objs, utils::object.size, numeric(1))) 
  size_byte
}
