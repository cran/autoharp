## (01) Invalid Rmd file (no chunks)
file_contents <- c("---",
                   "output: html_document",
                   "---",
                   "",
                   "Hello!")
writeLines(file_contents, "test01.Rmd")

## (02) Invalid Rmd file (no yaml header)

file_contents <- c("## no yaml header, file extension correct",
                   "x <- 1")
writeLines(file_contents, "test02.Rmd")

## (03) Check libraries (valid Rmd file)
file_contents <- c("---",
		"title: \"Sample\"",
		"output: html_document",
		"---",
		"",
		"```{r setup, include=FALSE}",
		"knitr::opts_chunk$set(echo = TRUE)",
		"```",
		"",
		"## R Markdown",
		"testing",
		"```{r cars}",
		"library(dplyr)",
		"summary(cars)",
		" x <- rnorm(100)",
		"```",
		"",
		"## Including Plots",
		"",
		"You can also embed plots, for example:",
		"",
		"```{r pressure, echo=FALSE}",
		"plot(pressure)",
		"```")
writeLines(file_contents, "test03.Rmd")

## (04) valid solution template
file_contents <- c("---",
		"title: \"Solution template\"",
		"output: html_document",
		"---",
		"",
		"## R Markdown",
		"testing",
		"```{r cars}",
		"library(dplyr)",
		"summary(cars)",
		" x <- rnorm(100)",
		"```",
		"",
		"## Test chunks",
		"```{r test_01, autoharp.objs=c(\"x\"), autoharp.scalars=c(\"len_x\")}",
		"len_x <- (length(x) == length(.x))",
		"```")
writeLines(file_contents, "test04.Rmd")


## Solution template
file_contents <- c("---",
            "title: \"test template\"",
            "output: html_document",
            "---",
            "",
            "```{r}",
            "X <- 1:10",
            "Y <- LETTERS[1:5]",
            "Z <- 10:15",
            "```",
            "",
            "",
            "```{r test02, autoharp.objs=c(\"X\", \"Y\", \"Z\"), autoharp.scalars=c(\"Xmean\", \"Zmean\")}",
            "Xmean <- mean(X)",
            "Zmean <- mean(Z)",
            "```",
            "",
            "```{r test03, autoharp.scalars=c(\"Y1\")}",
            "Y1 <- Y[3]",
            "```")
writeLines(file_contents, "soln_template.Rmd")

## student file 1
file_contents <- c( "X <- 1:10",
            "Y <- LETTERS[1:5]",
            "Z <- 10:15")
writeLines(file_contents, "s1.R")

## student file 2
file_contents <- c( "X <- 1:10",
            "Z <- 10:15")
writeLines(file_contents, "s2.R")

## student file 3
file_contents <- c( "X <- 1:10", "Y <- 1:10")
writeLines(file_contents, "s3.R")

## student file 4
file_contents <- c( "X <- 1:10", 
                    "Y <- LETTERS[1:10]",
                    "X + Y")
writeLines(file_contents, "s4.R")
