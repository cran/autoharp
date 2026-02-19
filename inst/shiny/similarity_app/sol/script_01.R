# Student Script 1 - Linear Regression Analysis

# Load data
data(mtcars)

# Fit linear model
model <- lm(mpg ~ wt + hp, data = mtcars)

# Get summary
summary(model)

# Make predictions
predictions <- predict(model, mtcars)

# Calculate residuals
residuals <- residuals(model)

# Plot results
plot(mtcars$wt, mtcars$mpg, main = "MPG vs Weight",
     xlab = "Weight", ylab = "MPG", pch = 19, col = "blue")
abline(model, col = "red", lwd = 2)
