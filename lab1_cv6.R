# Load necessary libraries
library(robustbase)  # For robust regression methods
library(cvTools)     # For cross-validation tools

# Load the dataset
data("coleman")

# Set the seed for reproducibility
set.seed(4321)

# Adjust control settings for robust regression to aid convergence
control_settings <- lmrob.control(maxit.scale = 1000, refine.tol = 1e-7, k.max = 500)

# Fit an MM regression model with improved control settings
fit_mm_direct <- lmrob(Y ~ ., data = coleman, control = control_settings)

# Perform cross-validation using the robustly fitted model
cv_results_direct <- cvFit(fit_mm_direct, data = coleman, y = coleman$Y, cost = rtmspe,
                           K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

# Print cross-validation results
print("Cross-validation results from direct model fit with robust settings:")
print(cv_results_direct)

# Check the structure and summary statistics of the dataset
print("Data summary:")
print(summary(coleman))

# Plot leverage vs. studentized residuals to identify potential influential points
print("Plotting leverage vs. studentized residuals:")
plot(lmrob(Y ~ ., data = coleman), which = 4)

# If necessary, try a simplified model excluding potential problematic predictors
# (Assuming a manual review of the data suggests specific changes)
fit_mm_simplified <- lmrob(Y ~ ., data = coleman[, c("most_relevant_predictors")], control = control_settings)
cv_results_simplified <- cvFit(fit_mm_simplified, data = coleman, y = coleman$Y, cost = rtmspe,
                               K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

# Print simplified model results
print("Cross-validation results from simplified model fit:")
print(cv_results_simplified)

