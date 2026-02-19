test_that("get_libraries", {
  expect_equal(get_libraries("test03.Rmd"), c("dplyr", "knitr"))
})

pressure_chunk <- c("```{r pressure, echo=FALSE}", "plot(pressure)", "```")
test_that("extract_chunks", {
  expect_equal(extract_chunks("test03.Rmd", "pressure")[[1]], pressure_chunk)
})

if(rmarkdown::pandoc_available()){
    soln_out <- populate_soln_env("test04.Rmd", pattern = "test", ".")
    stud_env <- new.env()
    rmarkdown::render("test03.Rmd", clean = TRUE, envir = stud_env)
    .myenv <- stud_env

    test_that("correctness functions", {
      expect_true("x" %in% ls(soln_out$env))
      expect_true(".x" %in% ls(soln_out$env, all.names = TRUE))
      expect_equal(check_correctness(stud_env, soln_out$env, 
                                     soln_out$test_fname)[1,1], TRUE)
    })

    pop_out <- populate_soln_env("soln_template.Rmd", "test", ".")
    e1 <- new.env()
    source("s1.R", local=e1)
    .myenv <- e1
    out1 <- check_correctness(e1, pop_out$env, pop_out$test_fname)

    e2 <- new.env()
    source("s2.R", local=e2)
    .myenv <- e2
    out2 <- check_correctness(e2, pop_out$env, pop_out$test_fname)

    e3 <- new.env()
    source("s3.R", local=e3)
    .myenv <- e3
    out3 <- check_correctness(e3, pop_out$env, pop_out$test_fname)

    test_that("correctness2", {
      expect_equal(out1[1, "Y1"], "C")
      expect_equal(out2[1, 3], NA)
      expect_true(is.na(out3[1, "Zmean"]))
    })
}


