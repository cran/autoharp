# Student Script 7 - Similar to Script 1 (Plagiarism Case)

# Import mtcars dataset
data(mtcars)

# Create a linear regression model with weight and horsepower as predictors
regression_model <- lm(mpg ~ wt + hp, data = mtcars)

# Show the model summary statistics
summary(regression_model)

# Create prediction values from the model
predicted_mpg <- predict(regression_model, mtcars)

# Extract the residual values
residual_values <- residuals(regression_model)

# Generate a scatter plot with regression line
plot(mtcars$wt, mtcars$mpg, 
     main = "Fuel Efficiency vs Vehicle Weight",
     xlab = "Weight", ylab = "Miles per Gallon", 
     pch = 19, col = "blue")
abline(regression_model, col = "red", lwd = 2)
