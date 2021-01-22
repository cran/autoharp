rf <- function(n) {
  U <- runif(n)
  return(U^(1/4))
}

X <- rf(1e4)
