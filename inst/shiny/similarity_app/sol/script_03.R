# Student Script 3 - Data Manipulation with dplyr

library(dplyr)

# Load data
data(iris)

# Filter and summarize
result <- iris %>%
  filter(Species == "setosa") %>%
  group_by(Species) %>%
  summarize(
    mean_sepal_length = mean(Sepal.Length),
    mean_sepal_width = mean(Sepal.Width),
    count = n()
  )

# Display results
print(result)

# Create a new column
iris_modified <- iris %>%
  mutate(
    petal_ratio = Petal.Length / Petal.Width,
    size_category = ifelse(Sepal.Length > 6, "Large", "Small")
  )

# Summary statistics
summary(iris_modified)
