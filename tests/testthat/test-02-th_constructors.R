# Test constructors

test_that("Constructors: from list", {
  expect_s4_class(TreeHarp(th1), "TreeHarp")
  expect_s4_class(TreeHarp(th2), "TreeHarp")
  expect_s4_class(TreeHarp(th3), "TreeHarp")
  expect_s4_class(TreeHarp(th00), "TreeHarp")
  expect_error(TreeHarp(th8))
  expect_error(TreeHarp(th5))

  expect_error(TreeHarp(th4), regexp = "not connected")
  expect_error(TreeHarp(th7), regexp = "in BFS ordering")
  expect_error(TreeHarp(th7a), regexp = "in BFS ordering")
})

test_that("Constructors: From matrix",{
  expect_s4_class(TreeHarp(m1), "TreeHarp")
  expect_error(TreeHarp(m1a), regexp = "Not all nodes named")
})

knitr_problem <- quote(knitr::opts_chunk$set(echo = TRUE))
slot_dollar_mix <- quote(y <- l$ll$mmm@the(x = 3))
fml_problem <- quote(~x)
assignment_problem <- quote(x$xx$xxx <- g(x))

test_that("Constructors: From language",{
  expect_s4_class(TreeHarp(ex1, TRUE), "TreeHarp")
  expect_s4_class(TreeHarp(ex2, TRUE), "TreeHarp")
  expect_s4_class(TreeHarp(fml_problem, TRUE), "TreeHarp")
  expect_error(TreeHarp(quote(x <- 1)))

  nT <- get_node_types(TreeHarp(ex4, TRUE))
  expect_setequal(nT$name, c("<-", "x", "NA"))

  nT <- get_node_types(TreeHarp(knitr_problem, TRUE))
  expect_setequal(nT$name, c("$", "::", "set", "knitr", "opts_chunk", "echo", "TRUE"))

  nT <- get_node_types(TreeHarp(slot_dollar_mix, TRUE))
  expect_setequal(nT$name, c("<-", "y", "@", "$", "the", "$", "mmm",
                             "x", "l", "ll", "3") )

  expect_true(extract_assigned_objects(TreeHarp(assignment_problem, TRUE))[1] != "$")

})

