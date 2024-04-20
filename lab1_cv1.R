# Load necessary libraries
library(cvTools)
library(robustbase)
data(coleman)

# Define control settings for lmrob for better convergence
control_settings <- lmrob.control(k.max = 500, max.it = 50)

# Define a custom function to fit lmrob that includes the control settings and subsetting
fit_lmrob <- function(data, indices) {
  lmrob(Y ~ ., data = data[indices, ], control = control_settings)
}

# Set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

# Perform cross-validation
cv_result <- cvTool(fit_lmrob, data = coleman, y = coleman$Y, 
                    cost = rtmspe, folds = folds, 
                    costArgs = list(trim = 0.1))

# Output cross-validation results
print(cv_result)

# Correct the tuning sequence for psi
tuning <- list(tuning.psi = seq(2, 6, length.out = 5))

# Perform tuning with cross-validation
cvFitsLmrob <- cvTuning(fit_lmrob, data = coleman, y = coleman$Y, 
                        tuning = tuning, cost = rtmspe, folds = folds, 
                        costArgs = list(trim = 0.1))

# Output tuning results
print(cvFitsLmrob)

# Summarize the results from tuning
summary_results <- aggregate(cvFitsLmrob, FUN = summary)
print(summary_results)

