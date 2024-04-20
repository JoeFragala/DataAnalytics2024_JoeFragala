# Load necessary libraries
library(robustbase)  # For robust regression methods
library(cvTools)     # For cross-validation tools
library(MASS)        # For additional statistical tools

# Load the dataset
data("coleman")

# Set the seed for reproducibility
set.seed(1234)

# Define tuning parameters corresponding to 85% and 95% efficiency
tuning <- list(tuning.psi = c(3.443689, 4.685061))  # Psi values for the MM estimator

# Adjust control settings to potentially improve convergence
control_settings <- lmrob.control(maxit.scale = 500, refine.tol = 1e-7)

# Approach 1: Direct Model Fitting Function
# Perform cross-validation directly using the lmrob function with adjusted control settings
cv_results_function <- cvTuning(lmrob, formula = Y ~ ., data = coleman, tuning = tuning,
                                cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),
                                control = control_settings, seed = 1234)
print("Cross-validation results from direct model fitting function:")
print(cv_results_function)

# Approach 2: Via Function Call
# Set up function call dynamically for use in cross-validation with adjusted control settings
fit_call <- call("lmrob", formula = Y ~ .)
cv_results_call <- cvTuning(fit_call, data = coleman, y = coleman$Y, tuning = tuning,
                            cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),
                            control = control_settings, seed = 1234)
print("Cross-validation results from function call:")
print(cv_results_call)
