rf <- function(n) {
  x <- c()
  for(i in 1:n) {
    x[i] <- (runif(1)/4)^(1/3)
  }
  return(x)
}

X <- rf(1e3)
