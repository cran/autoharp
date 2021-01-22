## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
library(autoharp)
library(dplyr)

## ----ex1, fig.align='center', fig.width=6-------------------------------------
tree1 <- TreeHarp(quote(lm(y ~ x1 + x2, data=mydata)), TRUE)
opar <- par(mar=c(0,0,0,0))
plot(tree1, vertex.size=25, asp=0.6, vertex.color="gray", vertex.frame.color=NA)
par(opar)

## ----ex1_adjlist--------------------------------------------------------------
slot(tree1, "adjList")

## ----ex1_nodeTypes------------------------------------------------------------
slot(tree1, "nodeTypes")

## ----ex1_call-----------------------------------------------------------------
slot(tree1, "call")

## ----ex1_repr-----------------------------------------------------------------
tree1

## ----call_vs_non_call, fig.align='center', fig.width=6------------------------
opar <- par(mar=c(0,0,0,0))
plot(tree1, vertex.size=25, asp=0.6, vertex.color=tree1@nodeTypes$call_status)
par(opar)

## ----fh1----------------------------------------------------------------------
f1 <- rmd_to_forestharp(system.file("examples", "student_scripts", 
                                    "qn01_scr_02.R", package="autoharp"))
length(f1)

## ----num_for_loops------------------------------------------------------------
fapply(f1, count_fn_call, pattern="for")

## ----logical_eg---------------------------------------------------------------
fapply(f1, count_fn_call, pattern="for", combine=TRUE, 
       combiner_fn = function(x) any(unlist(x) > 0))

## ----uncombined_list----------------------------------------------------------
fapply(f1, count_fn_call, pattern="for", combine=FALSE)

## ----fharp-helpers, eval=FALSE------------------------------------------------
#  ?`forestharp-helpers`

## ----subtree_eg---------------------------------------------------------------
th3 <- TreeHarp(quote(a <- f(x, y, g(v, w))), quote=TRUE)
sub_th <- subtree_at(th3, 3, TRUE) # preserves call status
sub_th

## ----all_subtrees-------------------------------------------------------------
th4 <- TreeHarp(quote(f(x, g(z=2))), quote=TRUE)
all_trees <- generate_all_subtrees(th4)
all_trees

## ----carve_sub----------------------------------------------------------------
carve_subtree(th4, all_trees[5,])

## ----sub_rooted_at------------------------------------------------------------
thb1 <- TreeHarp(quote(b(d)), TRUE)
tha1 <- TreeHarp(quote(a(b(d), c)), TRUE)
is_subtree_rooted_at(thb1, tha1, 1) # FALSE
is_subtree_rooted_at(thb1, tha1, 2) # TRUE

## ----path_to_root1------------------------------------------------------------
ex1 <- quote(x <- f(y, g(5)))
th1 <- TreeHarp(ex1, TRUE)
path_to_root(th1, 5)

## ----get_rec_ex---------------------------------------------------------------
ex3 <- quote(x <- f(y = g(3, 4), z=1L))
t1 <- TreeHarp(ex3, TRUE)
rec_index <- get_recursive_index(t1, 6)
ex3[[rec_index + 1]]
ex3[[get_recursive_index(t1, 3)+1]]

## ----get_parent_call----------------------------------------------------------
ex3 <- quote(x <- f(y = g(3, 4), z=1L))
t1 <- TreeHarp(ex3, TRUE)
# get the function that calls g:
get_parent_call_id(t1, 6) 
#contrast with this:
get_parent_id(t1, 6)

## ----undesirable1, eval=FALSE-------------------------------------------------
#  x1 <- my_function(arg1)
#  x2 <- my_function(arg2)
#  x3 <- my_function(arg3)

## ----jac_ex-------------------------------------------------------------------
jaccard_treeharp(tha1, thb1)

## ----k2_ex--------------------------------------------------------------------
K2(tha1, thb1)
# normalised to 0 - 1:
K2(tha1, thb1)/sqrt(K2(tha1, tha1)* K2(thb1, thb1))

## ----nlp_sims1, eval=FALSE----------------------------------------------------
#  library(tidytext)
#  library(text2vec)
#  library(tm)
#  
#  stud_script_paths <- system.file("examples", "student_scripts", package="autoharp")
#  stud_script_names <- list.files(stud_script_paths, full.names = TRUE)
#  
#  token_count_list <- lapply(stud_script_names, rmd_to_token_count)
#  token_count_df <- bind_rows(token_count_list)
#  #head(token_count_df)

## ----nlp_sims2, eval=FALSE----------------------------------------------------
#  dtm_out <- cast_dtm(token_count_df, fname, token, n, tm::weightTfIdf)
#  dtm_mat <- as.matrix(dtm_out)
#  sim2(dtm_mat)
#  #Docs            qn01_scr_01.R qn01_scr_02.R qn02_scr_01.R qn02_scr_02.R
#  #  qn01_scr_01.R     1.0000000    0.32776151    0.00000000    0.00000000
#  #  qn01_scr_02.R     0.3277615    1.00000000    0.01166493    0.01166493
#  #  qn02_scr_01.R     0.0000000    0.01166493    1.00000000    0.57194128
#  #  qn02_scr_02.R     0.0000000    0.01166493    0.57194128    1.00000000

