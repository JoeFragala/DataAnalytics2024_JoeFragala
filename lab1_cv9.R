# Load necessary libraries
library(robustbase)  # For robust regression methods
library(cvTools)     # For cross-validation tools
library(ggplot2)     # For creating plots

# Load the dataset
data("coleman")

# Set the seed for reproducibility
set.seed(4321)

# Prepare cross-validation folds
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

# Perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

# Perform cross-validation for an MM regression model
fitLmrob <- lmrob(Y ~ ., data = coleman)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe, folds = folds, trim = 0.1)

# Perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe, folds = folds, trim = 0.1)

# Combine and compare cross-validation results
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)

# Compare raw and reweighted LTS estimators for 50% and 75% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)

# Visualization of the cross-validation results
results_df <- data.frame(
  Model = rep(c("LS", "MM", "LTS", "LTS 50%", "LTS 75%"), each = 10),
  CV_Score = c(cvFitLm$delta, cvFitLmrob$delta, cvFitLts$delta, cvFitLts50$delta, cvFitLts75$delta)
)

ggplot(results_df, aes(x = Model, y = CV_Score, fill = Model)) +
  geom_boxplot() +
  labs(title = "Comparison of Cross-Validation Scores Across Regression Models",
       y = "CV Score", x = "Model Type") +
  theme_minimal()

# Print the combined CV results for review
print("Combined CV results for LS, MM, and LTS models:")
print(cvFits)

print("Combined CV results for LTS 50% and 75%:")
print(cvFitsLts)
