soln_out <- populate_soln_env("soln_template_01.Rmd") 
corr_out_1 <- as.data.frame(render_one("example_01.R", "test_output", ".", soln_stuff = soln_out))
corr_out_4 <- as.data.frame(render_one("example_04.R", "test_output", ".", soln_stuff = soln_out))
test_that("Rendering1", {
  expect_equal(corr_out_1[1, 7], TRUE)
  expect_equal(corr_out_4[1, 7], FALSE)
})

soln_out <- populate_soln_env("soln_template_02.Rmd") 
corr_out_2 <- as.data.frame(render_one("example_02.R", "test_output", ".",  soln_stuff = soln_out))
corr_out_3 <- as.data.frame(render_one("example_03.R", "test_output", ".",  soln_stuff = soln_out))
test_that("Rendering2", {
  expect_equal(corr_out_2[1, "Y1"], "C")
  expect_equal(corr_out_3[1, 6], NA)
})
