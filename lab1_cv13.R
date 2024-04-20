# Load necessary libraries
library(robustbase)  # For robust regression methods
library(cvTools)     # For cross-validation tools
library(lattice)     # For creating dot plots

# Load the dataset
data("coleman")

# Set the seed for reproducibility
set.seed(1234)

# Set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

# Define robust control settings
control_settings <- lmrob.control(k.max = 500, maxit.scale = 1000, refine.tol = 1e-7)

# Perform cross-validation for different regression models
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

fitLmrob <- lmrob(Y ~ ., data = coleman, control = control_settings)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe, folds = folds, trim = 0.1)

fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe, folds = folds, trim = 0.1)

# Combine and plot results
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
print(dotplot(cvFits, main = "Combined Dot Plots for All Models"))

# Analyze LTS models with different trimming levels
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

# Combine and plot results for LTS models with different subset analyses
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
print(dotplot(cvFitsLts, main = "Dot Plots for LTS Models at 50% and 75% Subsets"))

