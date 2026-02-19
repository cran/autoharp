# Student Script 5 - Statistical Tests

# Load data
data(iris)

# T-test comparing two species
setosa_data <- iris$Sepal.Length[iris$Species == "setosa"]
versicolor_data <- iris$Sepal.Length[iris$Species == "versicolor"]

t_test_result <- t.test(setosa_data, versicolor_data)
print(t_test_result)

# ANOVA for all species
anova_result <- aov(Sepal.Length ~ Species, data = iris)
summary(anova_result)

# Correlation test
cor_test_result <- cor.test(iris$Sepal.Length, iris$Petal.Length)
print(cor_test_result)

# Chi-square test (create categorical data)
size_cat <- cut(iris$Sepal.Length, breaks = 3, labels = c("Small", "Medium", "Large"))
contingency_table <- table(size_cat, iris$Species)
chi_test <- chisq.test(contingency_table)
print(chi_test)
