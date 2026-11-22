rf <- function(n) {
  x <- c()
  for(i in 1:n) {
    x[i] <- (runif(1)/4)^(1/3)
  }
  return(x)
}

X <- rf(1e3)

x_vals <- seq(0, 1, length=50)
hist(X, main="Histogram and pdf")
lines(x_vals, 4*x_vals^3 , col="red", lwd=2)
