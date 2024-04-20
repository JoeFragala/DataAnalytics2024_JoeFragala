# Load necessary libraries
library(robustbase)   # For robust statistical methods
library(cvTools)      # For cross-validation utilities
library(ggplot2)      # For advanced plotting

# Load data
data("coleman")

# Set seed for reproducibility
set.seed(1234)

# Set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

# Perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

# Perform cross-validation for an MM regression model with improved control settings
control_robust <- lmrob.control(maxit.scale = 500, refine.tol = 1e-9, k.max = 1000)
fitLmrob <- lmrob(Y ~ ., data = coleman, control = control_robust)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe, folds = folds, trim = 0.1)

# Perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe, folds = folds, trim = 0.1)

# Combine results into one object for comparison
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)

# Output the combined cross-validation results
print(cvFits)

# Visualizations for MM regression model results
plot(cvFitLmrob, method = "bw")
plot(cvFitLmrob, method = "density")

# Plot combined results of all models
plot(cvFits, method = "bw")
plot(cvFits, method = "density")
plot(cvFits, method = "xy")
plot(cvFits, method = "dot")

# Compare raw and reweighted LTS estimators for 50% and 75% subsets
# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

# Combine results into one object
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
print(cvFitsLts)

# Plot combined results for LTS estimators
plot(cvFitsLts, method = "bw")
plot(cvFitsLts, method = "density")
plot(cvFitsLts, method = "xy")
plot(cvFitsLts, method = "dot")

