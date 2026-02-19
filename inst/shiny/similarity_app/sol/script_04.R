# Student Script 4 - Plotting with ggplot2

library(ggplot2)

# Load data
data(diamonds)

# Create scatter plot
p1 <- ggplot(diamonds, aes(x = carat, y = price, color = cut)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Diamond Price vs Carat",
       x = "Carat",
       y = "Price ($)")

print(p1)

# Create boxplot
p2 <- ggplot(diamonds, aes(x = cut, y = price, fill = cut)) +
  geom_boxplot() +
  theme_classic() +
  labs(title = "Price Distribution by Cut Quality",
       x = "Cut",
       y = "Price ($)")

print(p2)

# Create histogram
p3 <- ggplot(diamonds, aes(x = price)) +
  geom_histogram(bins = 50, fill = "steelblue", color = "black") +
  theme_bw() +
  labs(title = "Distribution of Diamond Prices",
       x = "Price ($)",
       y = "Frequency")

print(p3)
