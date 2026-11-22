rf <- function(n) {
  U <- runif(n)
  return(U^(1/4))
}

X <- rf(1e4)

x_vals <- 0:1
hist(X, freq=FALSE, main="Histogram and pdf")
lines(x_vals, 4*x_vals^3, col="red", lwd=2)
