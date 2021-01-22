test_that("Tree Kernel: K2", {
  expect_equal(K2(t2, t2, FALSE), 6)
  expect_equal(K2(t2, t6, FALSE), 7)
  
  expect_equal(K2(t6, t6, FALSE), 17)
  expect_equal(K2(t7, t7, FALSE), 13)
  expect_equal(K2(t5, t5, FALSE), 11)
  
  expect_equal(K2(t4, t4, FALSE), 7)
  
  expect_equal(K2(t4, t2, FALSE), 4)
})


test_that("Tree Kernel: Jaccard", {
  expect_equal(jaccard_treeharp(t2, t6, FALSE), 0.75)
  expect_equal(jaccard_treeharp(t2, t6, TRUE), 0.6)
})
