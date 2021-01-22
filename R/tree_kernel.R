
# library(autoharp)
#
# T1 <- TreeHarp(list(a=c(2,3), b=NULL, c=4, d=NULL))
# T2 <- TreeHarp(list(a=c(2,3), b=NULL, d=NULL))
# T3 <- TreeHarp(list(a=c(2,3), b=NULL, c=NULL))
# #plot(T2)

#' Compute tree similarity
#'
#' Computes similarity between two trees (non-recursively)
#'
#' @param t1 A TreeHarp object
#' @param t2 Anothe TreeHarp object.
#' @param norm A logical value, indicating if the kernel function should be
#' normalised, to account for different tree lengths.
#' @param ... Unused arguments, reserved for mcmapply
#' 
#' @return A numerical value between 0 and 1 (if normed).
tree_sim <- function(t1, t2, norm=FALSE, ...) {
  Kval <- K_nr(t1, t2, ...)
  if(norm){
    Kval <- Kval/sqrt(K_nr(t1,t1,...)*K_nr(t2,t2, ...))
  }
  Kval
}

K <- function(t1, t2) {
  names1 <- names(t1)
  adj1 <- get_adj_list(t1)

  names2 <- names(t2)
  adj2 <- get_adj_list(t2)

  l1 <- length(t1)
  l2 <- length(t2)

  # Fill up matrix as much as possible.
  out_mat <- matrix(NA_integer_, nrow=l1, ncol=l2)
  for(i in seq_along(names1))
    for(j in seq_along(names2)) {
      if(names1[i] != names2[j]) {
        out_mat[i, j] <- 0L
      } else {
        if(is.null(adj1[[i]]) || is.null(adj2[[j]]))
          out_mat[i,j] <- 1L
      }
    }
  # assign to a new environment
  e1 <- new.env()
  assign("out_mat", out_mat, envir = e1)
  remaining <- which(is.na(out_mat), arr.ind = TRUE)
  if(nrow(remaining) > 0){
    for(jj in nrow(remaining):1){
      S_r(remaining[jj,"row"], t1, remaining[jj, "col"], t2, e1)
    }
  }
  sum(e1$out_mat)
}

# Recursive method
S_r <- function(n1, t1, n2, t2, mat_env) {
  tmp <- mat_env$out_mat[n1, n2]
  if(!is.na(tmp)){
    return(tmp)
  }
  nn1 <- get_child_ids(t1, n1)
  cn1_names <- names(t1)[nn1]
  t1_df <- data.frame(ids=nn1, names=cn1_names,
                      stringsAsFactors = FALSE)

  nn2 <- get_child_ids(t2, n2)
  cn2_names <- names(t2)[nn2]
  t2_df <- data.frame(ids=nn2, names=cn2_names,
                      stringsAsFactors = FALSE)

  common_names <- dplyr::inner_join(t1_df, t2_df, by="names")
  if(nrow(common_names) == 0) {
    mat_env$out_mat[n1, n2] <- 1
    return(1)
  }
  tmp_prod <- 1
  for(ii in seq_along(common_names$ids.x)){
    tmp_prod <- tmp_prod*(1 + S_r(common_names$ids.x[ii], t1,
                                  common_names$ids.y[ii], t2, mat_env))
  }
  mat_env$out_mat[n1, n2] <- tmp_prod
  return(tmp_prod)

}

# Brute force by working on only the minimal spanning tree.
S_nr2 <- function(n1, t1, n2, t2, verbose=FALSE) {
  # extract subtrees
  if(n1 == 1)
    s1 <- t1 else
    s1 <- subtree_at(t1, n1)

  if(n2 == 1)
    s2 <- t2 else
    s2 <- subtree_at(t2, n2)

  # find common nodes
  s1_names <- names(s1)
  s2_names <- names(s2)
  if(names(s1)[1] != names(s2)[1])
    return(0)
  common_names <- base::intersect(s1_names, s2_names)
  if(length(common_names) == 0)
    return(0)

  #common_names
  if(verbose) {
    nnodes_1 <- sum(s1_names %in% common_names)
    nnodes_2 <- sum(s2_names %in% common_names)
    message(paste("tree 1 has", nnodes_1, "in common.\n", sep=" "))
    message(paste("tree 2 has", nnodes_2, "in common.\n", sep=" "))
  }

  # prune trees
  #ss1 <- prune_depth(s1, common_names)
  ss1 <- carve_mst(s1, common_names)
  #ss2 <- prune_depth(s2, common_names)
  ss2 <- carve_mst(s2, common_names)

  # # generate all trees
  if(verbose){
    message("Generating all sub-trees of 1.\n")
  }
  s1_trees <- generate_all_subtrees(ss1)
  s1_mat <- apply(s1_trees, 1, function(x) as(carve_subtree(obj=ss1, x),
                                              "matrix"))
  # s1_mat <- apply(s1_trees, 1, carve_subtree, obj=ss1)
  if(verbose){
    message("Removing duplicates for tree 1.\n")
  }
  s1_dup_ids <- duplicated(s1_mat)
  s1_mat <- s1_mat[!s1_dup_ids]

  if(verbose){
    message("Generating all sub-trees of 2.\n")
  }
  s2_trees <- generate_all_subtrees(ss2)
  s2_mat <- apply(s2_trees, 1, function(x) as(carve_subtree(obj=ss2, x),
                                              "matrix"))
  #s2_mat <- apply(s2_trees, 1, carve_subtree, obj=ss2)
  if(verbose){
    message("Removing duplicates for tree 2.\n")
  }
  s2_dup_ids <- duplicated(s2_mat)
  s2_mat <- s2_mat[!s2_dup_ids]

  # # expand.grid, filter
  cross_df <- expand.grid(t1=1:length(s1_mat), t2=1:length(s2_mat))

  # call mapply or mcmapply
  if(verbose){
    message("Cross-comparing sub-trees.\n")
  }
  equality_ids <- mapply(function(x, y) identical(s1_mat[[x]], s2_mat[[y]]),
                         x=cross_df$t1, y=cross_df$t2)
  sum(equality_ids)
}

# Blind brute force
S_nr <- function(n1, t1, n2, t2, verbose=FALSE) {
  # extract subtrees
  s1 <- subtree_at(t1, n1)
  s2 <- subtree_at(t2, n2)

  # generate all trees
  # remove duplicates
  if(verbose){
    message("Generating all sub-trees of 1.\n")
  }
  s1_trees <- generate_all_subtrees(s1)
  s1_mat <- apply(s1_trees, 1, function(x) as(carve_subtree(obj=s1, x),
                                                 "matrix"))
  if(verbose){
    message("Removing duplicates for tree 1.\n")
  }
  s1_dup_ids <- duplicated(s1_mat)
  s1_mat <- s1_mat[!s1_dup_ids]

  if(verbose){
    message("Generating all sub-trees of 2.\n")
  }
  s2_trees <- generate_all_subtrees(s2)
  s2_mat <- apply(s2_trees, 1, function(x) as(carve_subtree(obj=s2, x),
                                                 "matrix"))
  if(verbose){
    message("Removing duplicates for tree 2.\n")
  }
  s2_dup_ids <- duplicated(s2_mat)
  s2_mat <- s2_mat[!s2_dup_ids]

  # expand.grid, filter
  cross_df <- expand.grid(t1=1:length(s1_mat), t2=1:length(s2_mat))

  # call mapply or mcmapply
  equality_ids <- mapply(function(x, y) identical(s1_mat[[x]], s2_mat[[y]]),
                         x=cross_df$t1, y=cross_df$t2)
  sum(equality_ids)
}

K_nr <- function(t1, t2, ...) {
  names1 <- names(t1)
  adj1 <- get_adj_list(t1)

  names2 <- names(t2)
  adj2 <- get_adj_list(t2)

  l1 <- length(t1)
  l2 <- length(t2)

  # Fill up matrix as much as possible.
  out_mat <- matrix(NA_integer_, nrow=l1, ncol=l2)
  for(i in seq_along(names1))
    for(j in seq_along(names2)) {
      if(names1[i] != names2[j]) {
        out_mat[i, j] <- 0L
      } else {
        if(is.null(adj1[[i]]) || is.null(adj2[[j]]))
          out_mat[i,j] <- 1L
      }
    }
  # assign to a new environment
  remaining <- which(is.na(out_mat), arr.ind = TRUE)
  if(nrow(remaining) > 0){
    out <- mapply(function(x, y) S_nr(x, t1, y, t2, ...),
                  x=remaining[,"row"], y=remaining[,"col"])
  }
  out_mat[remaining] <- out
  #out_mat
  #remaining
  sum(out_mat)
}

#' Compute tree similarity
#'
#' @param t1 A TreeHarp object.
#' @param t2 A TreeHarp object.
#' @param verbose A logical value, indicating if the output should be verbose.
#'
#' @return An integer, that counts the number of sub-trees in common between 
#' the two trees. Please see the reference papers for more information.
#' 
#' @details As far as possible, this function tries to do things recursively.
#' It sets up a n x m matrix and fills up as much as it can. Then it uses 
#' recursive relationships to fill in the rest. When it cannot, it uses
#' \code{\link{generate_all_subtrees}} to generate and count common subtrees.
#' 
#' @references 
#' 
#' \enumerate{
#' \item 
#' \emph{Convolution kernels for natural language}, M Collins and N Duffy, 
#' \emph{Advances in neural information processing systems}, 2002.
#' 
#' \item 
#' \emph{Convolution kernels on discrete structures}, D Haussler, 
#' \emph{Technical report, Department of Computer Science, UC Santa Cruz}, 1999.
#' }
#'
#' @export
#' @examples
#' tree1 <- TreeHarp(quote(x <- 1), TRUE)
#' tree2 <- TreeHarp(quote(y <- 1), TRUE)
#' K2(tree1, tree2, TRUE)
#' 
K2 <- function(t1, t2, verbose=FALSE) {
  names1 <- names(t1)
  adj1 <- get_adj_list(t1)

  names2 <- names(t2)
  adj2 <- get_adj_list(t2)

  l1 <- length(t1)
  l2 <- length(t2)

  if(verbose){
    message("Filling up matrix.\n")
  }
  # Fill up matrix as much as possible.
  out_mat <- matrix(NA_integer_, nrow=l1, ncol=l2)
  for(i in seq_along(names1))
    for(j in seq_along(names2)) {
      if(names1[i] != names2[j]) {
        out_mat[i, j] <- 0L
      } else {
        if(is.null(adj1[[i]]) || is.null(adj2[[j]]))
          out_mat[i,j] <- 1L
      }
    }
  # return(out_mat) # debug
  # assign to a new environment
  e1 <- new.env()
  assign("out_mat", out_mat, envir = e1)
  remaining <- which(is.na(out_mat), arr.ind = TRUE)

  if(verbose){
    message(paste0(nrow(remaining), " entries remaining.\n"))
  }
  # return(remaining) # debug

  if(nrow(remaining) > 0){
    for(jj in nrow(remaining):1){
      S_r2(remaining[jj,"row"], t1, remaining[jj, "col"], t2, e1, verbose)
    }
  }
  sum(e1$out_mat)
}

# Recursive method
S_r2 <- function(n1, t1, n2, t2, mat_env, verbose=FALSE) {
  tmp <- mat_env$out_mat[n1, n2]
  if(!is.na(tmp)){
    return(tmp)
  }
  tmp_prod <- 1

  nn1 <- get_child_ids(t1, n1)
  cn1_names <- names(t1)[nn1]
  t1_df <- data.frame(ids=nn1, names=cn1_names,
                      stringsAsFactors = FALSE)
  nn2 <- get_child_ids(t2, n2)
  cn2_names <- names(t2)[nn2]
  t2_df <- data.frame(ids=nn2, names=cn2_names,
                      stringsAsFactors = FALSE)
  dups_found <- (anyDuplicated(cn1_names) || anyDuplicated(cn2_names))
  # browser()

  if(dups_found) {
    if(verbose){
      message("Duplicate children nodes found.\n")
    }
    if(anyDuplicated(cn1_names)) {
      dup_names1 <- cn1_names[duplicated(cn1_names)]
    } else {
      dup_names1 <- cn1_names
    }
    if(anyDuplicated(cn2_names)) {
      dup_names2 <- cn2_names[duplicated(cn2_names)]
    } else {
      dup_names2 <- cn2_names
    }
    dup_names12 <- unique(base::intersect(dup_names1, dup_names2))
    if(length(dup_names12) > 0) {
      subtree_br1 <- subtree_at(t1, n1)
      new_dup1 <- base::intersect(get_child_ids(subtree_br1, 1),
                                  which(names(subtree_br1) %in% dup_names12))
      subtree_br1  <- keep_branches(subtree_br1, new_dup1)
      t1_df <- dplyr::filter(t1_df, !(names %in% dup_names12))
  
      subtree_br2 <- subtree_at(t2, n2)
      new_dup2 <- base::intersect(get_child_ids(subtree_br2, 1),
                                  which(names(subtree_br2) %in% dup_names12))
      subtree_br2  <- keep_branches(subtree_br2, new_dup2)
      t2_df <- dplyr::filter(t2_df, !(names %in% dup_names12))
  
      tmp_prod <- S_nr2(1, subtree_br1, 1, subtree_br2, verbose)
      # browser()
    }
  }

  common_names <- dplyr::inner_join(t1_df, t2_df, by="names")
  if(nrow(common_names) == 0) {
    mat_env$out_mat[n1, n2] <- tmp_prod
    return(tmp_prod)
  }
  for(ii in seq_along(common_names$ids.x)){
    tmp_prod <- tmp_prod*(1 + S_r2(common_names$ids.x[ii], t1,
                                  common_names$ids.y[ii], t2, mat_env))
  }
  mat_env$out_mat[n1, n2] <- tmp_prod
  return(tmp_prod)
}

#' Computes Jaccard Index
#' 
#' Computes the Jaccard index between two trees.
#'
#' @param th1 A TreeHarp object.
#' @param th2 A TreeHarp object.
#' @param weighted A logical value, indicating if the weighted Jaccard 
#' similarity should be computed.
#' 
#' @details The unweighted form is just the cardinality of the intersection 
#' of the two sets of tokens, divided by the union of the two sets.
#' 
#' The weighted form is described on the WIkipedia page:
#' https://en.wikipedia.org/wiki/Jaccard_index#Weighted_Jaccard_similarity_and_distance
#'
#' @return A real number between 0 and 1.
#' @export
jaccard_treeharp <- function(th1, th2, weighted=FALSE) {
  tokens1 <- names(th1)
  tokens2 <- names(th2)
  token_union <- union(tokens1, tokens2)
  
  if(!weighted) {
    denom <- length(token_union)
    numer <- length(base::intersect(tokens1, tokens2))
  } else {
    count1 <- sapply(token_union, function(x) sum(x == tokens1))
    count2 <- sapply(token_union, function(x) sum(x == tokens2))
    numer <- sum(pmin(count1, count2))
    denom <- sum(pmax(count1, count2))
  }
  numer/denom
}
