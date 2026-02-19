# Student Script 6 - Custom Functions

# Define a function to calculate factorial
factorial_calc <- function(n) {
  if (n < 0) {
    stop("Input must be non-negative")
  }
  if (n == 0 || n == 1) {
    return(1)
  }
  result <- 1
  for (i in 2:n) {
    result <- result * i
  }
  return(result)
}

# Test the function
test_values <- c(5, 0, 10)
for (val in test_values) {
  cat("Factorial of", val, "is", factorial_calc(val), "\n")
}

# Define a function to compute Fibonacci sequence
fibonacci <- function(n) {
  if (n <= 0) {
    return(numeric(0))
  }
  if (n == 1) {
    return(1)
  }
  fib_seq <- numeric(n)
  fib_seq[1] <- 1
  fib_seq[2] <- 1
  if (n > 2) {
    for (i in 3:n) {
      fib_seq[i] <- fib_seq[i-1] + fib_seq[i-2]
    }
  }
  return(fib_seq)
}

# Test Fibonacci
print(fibonacci(10))
