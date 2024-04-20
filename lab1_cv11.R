# Set up environment
library(robustbase)
data("coleman")
set.seed(1234)

# Adjust robust control settings
robust_control <- lmrob.control(maxit.scale = 500, refine.tol = 1e-7)

# Set up function call for an MM regression model with improved control settings
call <- call("lmrob", formula = Y ~ ., control = robust_control)

# Prepare folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

# Perform cross-validation
cv_results <- cvTool(call, data = coleman, y = coleman$Y, cost = rtmspe,
                     folds = folds, costArgs = list(trim = 0.1))
print("Cross-validation results with adjusted control settings:")
print(cv_results)
