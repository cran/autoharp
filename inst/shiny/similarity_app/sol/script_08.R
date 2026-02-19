# Student Script 8 - Matrix Operations

# Create matrices
A <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3, byrow = TRUE)
B <- matrix(c(7, 8, 9, 10, 11, 12), nrow = 3, ncol = 2, byrow = TRUE)

# Matrix multiplication
C <- A %*% B
print("Matrix multiplication result:")
print(C)

# Transpose
A_transpose <- t(A)
print("Transpose of A:")
print(A_transpose)

# Create a square matrix
D <- matrix(c(4, 2, 2, 3), nrow = 2, ncol = 2)

# Calculate determinant
det_D <- det(D)
print(paste("Determinant of D:", det_D))

# Calculate inverse
D_inverse <- solve(D)
print("Inverse of D:")
print(D_inverse)

# Eigenvalues and eigenvectors
eigen_result <- eigen(D)
print("Eigenvalues:")
print(eigen_result$values)
print("Eigenvectors:")
print(eigen_result$vectors)
