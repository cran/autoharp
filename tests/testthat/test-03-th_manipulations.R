test_that("Carve trees: from char array", {
  expect_s4_class(carve_subtree(tho3a, c(1,0,1,0,1,1,0)), "TreeHarp")
  expect_s4_class(carve_subtree(tho3a, c(0,0,0,0,0,0,1)), "TreeHarp")
  expect_error(carve_subtree(tho3a, c(1,0,1,0,1,1,1)), regexp="define a tree")
})

test_that("Check rooted at", {
  expect_true(is_subtree_rooted_at(tho1, tho2, 1))
  expect_false(is_subtree_rooted_at(tho1, tho2, 2))
})

test_that("Identical trees", {
  expect_true(identical(carve_subtree(tho3a, c(1,1,1,0,0,0,0)), tho1))
  expect_false(identical(carve_subtree(tho1, c(1,0,0)), tho00))
})

ex1_tree <- TreeHarp(ex1, TRUE)
test_that("Path to root", {
  expect_equal(path_to_root(t6, 5), c(1, 0, 1, 0, 1))
  expect_equal(path_to_root(t6, 1), c(1, 0, 0, 0, 0))
  expect_equal(path_to_root(ex1_tree, 6), c(1,0,1,0,1,1,0,0,0))
})

test_that("Generate sub-tree counts", {
  expect_equal(nrow(generate_all_subtrees(t6)), 12)
  expect_equal(nrow(generate_all_subtrees(ex1_tree)), 33)
})

br1 <- keep_branches(ex1_tree, 7)
br2 <- keep_branches(ex1_tree, 7, FALSE)
test_that("Keep branches", {
  expect_equal(names(br1), c("+", "geom_point", "aes", "y", "y1"))
  expect_equal(names(br2), c("+", "geom_point", "aes", "y"))
})

tt3 <- TreeHarp(quote(f1 <- function(a = 4L) {x <- a; 2 -> y; z = 3; y}), 
                TRUE)
tt4 <- TreeHarp(quote(x$g[[34]] <- rnorm(10, mean = g(5, h(34)), sd = 3.4)), TRUE)
ex6 <- quote(function(y, x = g(z)) w <- 1)
tt5 <- TreeHarp(ex6, TRUE)
test_that("Helpers work", {
  expect_setequal(extract_assigned_objects(tt3), c("f1", "x", "y", "z"))
  expect_equal(get_recursive_index(tt3, 7), c(3,3,2))
  expect_equal(get_recursive_index(tt3, 9), c(3,3,4))
  expect_equal(get_recursive_index(tt5, 5), c(2,2))
  expect_equal(get_recursive_index(tt4, 14), c(3,3,3))
  expect_equal(get_recursive_index(tt4, 4), c(2,2))
  expect_equal(get_parent_call_id(tt4, 11), 3)
  expect_equal(get_parent_call_id(tt4, 12), 3)
})
