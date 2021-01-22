## -----------------------------------------------------------------------------
library(testthat)
library(autoharp)
library(rlang)

## ----corr_solutions, echo=FALSE-----------------------------------------------
rf <- function(n) {
  U <- runif(n)
  X <- U^(1/4)
  X
}
X <- rf(1e4)

## ----test01, autoharp.objs=c("rf", "X"), autoharp.scalars=c("lenX", "lenfn"), echo=FALSE----
lenX <- (length(X) == length(.X))
lenfn <- (length(fn_fmls(rf)) == length(fn_fmls(.rf)))

## ----test02, autoharp.scalars=c("mean.X", "sd.X")-----------------------------
mean.X <- mean(X)
sd.X <- sd(X)

## ----test03, autoharp.scalars=c("for_loop")-----------------------------------
f1 <- rmd_to_forestharp(.myfilename)
for_loop <- fapply(f1, detect_for_in_fn_def, fn_name = "rf", combine=TRUE, 
                   combiner_fn = function(x) any(unlist(x)))

