# Student Script 2 - Very Similar Linear Regression (Slight Variation)

# Load dataset
data(mtcars)

# Build linear regression model
linear_model <- lm(mpg ~ wt + hp, data = mtcars)

# Display model summary
summary(linear_model)

# Generate predictions
pred_values <- predict(linear_model, mtcars)

# Compute residual values
resid_values <- residuals(linear_model)

# Visualize the relationship
plot(mtcars$wt, mtcars$mpg, main = "Miles per Gallon vs Weight",
     xlab = "Weight (1000 lbs)", ylab = "MPG", pch = 19, col = "darkblue")
abline(linear_model, col = "red", lwd = 2)
