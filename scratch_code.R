

set.seed(42) # For reproducibility

# Simulating the predictor variable
x <- 1:100

# Simulating the response variable with increasing variance
y <- 2*x + rnorm(100, mean = 0, sd = 0.2*x)

# Plotting the data to visualize heteroscedasticity
plot(x, y, main = "Simulated Heteroscedastic Data",
     xlab = "Predictor (x)", ylab = "Response (y)",
     pch = 20, col = "blue")
# Fitting a linear model
model <- lm(y ~ x)

# Adding the regression line to the plot
abline(model, col = "red")

# Displaying the summary of the model to check for signs of heteroscedasticity
summary(model)

plot(fitted(model), resid(model),
     main = "Residuals vs. Fitted Values",
     xlab = "Fitted Values", ylab = "Residuals",
     pch = 20, col = "darkgreen")
abline(h = 0, col = "red")



#### Simulate Data
set.seed(42)  # Ensure reproducibility

# Adjusted number of students per school for repeated measures
n_students_per_school <- 30
n_observations_per_student <- 4

# Total number of observations
n <- n_students_per_school * 4 * n_observations_per_student

# Generating the data
school <- factor(rep(1:4, each = n_students_per_school * n_observations_per_student))
intervention <- rep(c(1, 0, 1, 0), each = n_students_per_school * n_observations_per_student)
individual_id <- rep(1:(n_students_per_school * 4), each = n_observations_per_student)

# Assume intervention effect and time interact, creating a varying effect over time
time <- rep(1:n_observations_per_student, n_students_per_school * 4)

# Simulating test scores with individual variability, school effect, and time effect
baseline_performance <- c(75, 70, 80, 65)  # Baseline for each school
performance_variation <- 5  # Variation within schools
time_effect <- 2  # Effect of time on test scores

# Adjusted test_scores simulation to include time effect
test_scores <- rep(baseline_performance, each = n_students_per_school * n_observations_per_student) + 
  intervention * 10 +  # Effect of intervention
  time * time_effect +  # Time effect
  rnorm(n, 0, performance_variation)  # Individual variation

# Creating the dataframe
school_df <- data.frame(school, intervention, individual_id, time, test_scores)

mean_income_by_school <- c(50000, 52000, 54000, 56000)  # Mean income for each school
income_variation <- 10000  # Standard deviation around the mean income

# Generating household income for each student, constant across their observations
household_income <- rep(rnorm(n_students_per_school * 4, mean = rep(mean_income_by_school, each = n_students_per_school), sd = income_variation), each = n_observations_per_student)
#standardized_income <- scale(household_income)
# Updating the dataframe to include household income
school_df$household_income <- standardized_income
