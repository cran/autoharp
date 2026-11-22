## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo=FALSE--------------------------------------------------------
library(magrittr)

## ----th-example-1, echo=TRUE--------------------------------------------------
library(autoharp)
tree1 <- TreeHarp(quote(lm(y ~ x1 + x2, data=mydata)), TRUE)

## ----th-example-1-plot, echo=TRUE, fig.align='center', collapse=TRUE, fig.cap='Example TreeHarp object'----
opar <- par(mar=c(0,0,0,0))
plot(tree1, vertex.size=25, asp=0.6, vertex.color="gray", vertex.frame.color=NA)
par(opar)

## ----th1-adjlist--------------------------------------------------------------
slot(tree1, "adjList")

## ----th1-nodetypes, echo=TRUE-------------------------------------------------
get_node_types(tree1)

## ----ex1_call-----------------------------------------------------------------
slot(tree1, "call")

## ----ex1_repr-----------------------------------------------------------------
tree1

## ----th-example-2-plot, echo=TRUE, fig.align='center', fig.cap='TreeHarp object with colored nodes'----
opar <- par(mar=c(0,0,0,0))
plot(tree1, vertex.size=25, asp=0.6, vertex.color=tree1@nodeTypes$call_status)
par(opar)

## ----th-routines, echo=FALSE--------------------------------------------------
fn_names <- c("subtree_at", "generate_all_subtrees", "carve_subtree", 
              "is_subtree_rooted_at", "path_to_root", "get_recursive_index",
              "get_parent_call_id")
fn_desc <- c("Returns a subtree, given a node number (integer) as an argument.",
             "Enumerates and prints all subtrees of a TreeHarp object.",
             "Given a list of nodes, this function returns a TreeHarp object with only the nodes indicated.",
             "This function can be used to check if one tree is rooted at a particular node of another tree.",
             "Identifies the nodes on the path from a node up to the root of a TreeHarp object.",
             "Returns a recursive call index that can be used to extract a sub-call from an R call (language expression).",
             "Returns the most recent call from a particular node id.")
th_routines_df <- data.frame(fn_names, fn_desc)
knitr::kable(th_routines_df, booktabs = TRUE, 
             col.names = c("Function name", "Function description"),
             align = "l") %>% 
  kableExtra::column_spec(2, width = "10cm") %>%
  kableExtra::kable_styling(font_size = 12, latex_options = c("hold_position"))
  # kableExtra::kable_styling(latex_options = c("hold_position"))


## ----fh1, echo=TRUE-----------------------------------------------------------
stud_script_path <- system.file("examples", "example-01", "student_scripts", 
                                "qn01_scr_02.R", package = "autoharp")
f1 <- rmd_to_forestharp(stud_script_path)
str(f1, max.level = 2)

## ----fh2-num-for-loops, echo=TRUE---------------------------------------------
fapply(f1, count_fn_call, pattern = "^for$")

## ----fh-helpers, echo=FALSE---------------------------------------------------
fn_names <- c("count_self_fn", "count_lam_fn", "count_fn_call", 
              "extract_fn_call", "extract_formal_args", 
              "extract_actual_args", "extract_assigned_objects", 
              "count_num_lines_for_loop", "detect_growing", 
              "detect_for_in_fn_def", "count_fn_in_fn", 
              "detect_fn_call_in_for", "detect_fn_arg", "detect_nested_for")

fn_desc <- c("Counts the number of functions defined by the student, in the script.",
             "Counts the number of anonymous functions (used in `sapply()`, etc).", 
             "Counts the number of times a function with a particular name was executed. The
function can be described by providing a regular expression, or a package namespace.",
             "Instead of just counting, this extracts the entire function call as a string.", 
             "Extracts the formal arguments in a function call.",
             "Extracts the actual arguments in a function call.", 
             "Extracts the name of object in an assignment expression.", 
             "Counts the number of sub-expressions (lines) in a for loop.", 
             "Detects use of a `for` loop without pre-allocating an iterable of the correct length.", 
             "Detects the presence of a `for` loop in a function definition.", 
             "Given functions A and B, this counts the number of times B is called within A.", 
             "Detects the call of a function within a `for` loop.", 
             "Detects if a specified argument was part of a function call.", 
             "Detects the presence of a nested `for` loop.")

fh_helpers_df <- data.frame(fn_names, fn_desc)
knitr::kable(fh_helpers_df, booktabs = TRUE, 
             col.names = c("Function name", "Function description"),
             align = "l") %>% 
             #caption = "ForestHarp helpers. The reader is referrred to the help page on 'forestharp-helpers' for examples on usage.") %>% 
  kableExtra::column_spec(2, width = "9cm") %>%
  kableExtra::kable_styling(font_size = 12, latex_options = c("hold_position"))

