test_that("Utility: get children and parents", {
  expect_equal(get_child_ids(th1, 2), NULL)
  expect_equal(get_child_ids(th1, 1), c(2L, 3L))
  expect_equal(get_child_ids(th3, 3), 4L)

  expect_equal(get_parent_id(th1, 1L), NULL)
  expect_equal(get_parent_id(th1, 2L), 1L)
  expect_error(get_parent_id(th1, 6L), regexp = "does not exist")
  expect_warning(get_parent_id(th5, 4L), regexp = "More than 1")
})

test_that("Utility: conversion b/w adjacency list and matrix", {
  expect_equal(adj_list_2_matrix(th1), m1)
  expect_equal(matrix_2_adj_list(m2), th6)
})

test_that("Utility: connected", {
  expect_equal(is_connected(th4), FALSE)
  expect_equal(is_connected(th6), TRUE)
  expect_equal(is_connected(th1), TRUE)
})

test_that("Utility: cyclic", {
  e1$visited <- rep(FALSE, nrow(m2))
  expect_equal(is_cyclic_r(m2, 1, -1, e1), TRUE)
  e1$visited <- rep(FALSE, nrow(m1))
  expect_equal(is_cyclic_r(m1, 1, -1, e1), FALSE)
})

test_that("Utility: obtain levels", {
  expect_identical(get_levels(th7), c(1, 3, 2, 2, 3, 2))
  expect_identical(get_levels(th1), c(1, 2, 2))
  expect_equal(get_levels(th00), 1L)
})
