# Load necessary libraries
library(robustbase)
library(cvTools)
library(ggplot2)  # For plotting results

# Load data
data("coleman")

# Set seed for reproducibility
set.seed(1234)

# Set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

# Perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- repCV(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

# Control settings for robust regression to aid convergence
control_robust <- lmrob.control(maxit.scale = 500, refine.tol = 1e-8, k.max = 1000)

# Perform cross-validation for an MM regression model with improved control settings
fitLmrob <- lmrob(Y ~ ., data = coleman, control = control_robust)
cvFitLmrob <- repCV(fitLmrob, cost = rtmspe, folds = folds, trim = 0.1)

# Perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLtsRaw <- repCV(fitLts, cost = rtmspe, folds = folds, trim = 0.1)
cvFitLtsReweighted <- repCV(fitLts, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

# Verify structure and extract CV scores
print("Structure of LS Model CV Results:")
print(str(cvFitLm))
print("Structure of MM Model CV Results:")
print(str(cvFitLmrob))
print("Structure of LTS Model CV Results (Raw):")
print(str(cvFitLtsRaw))
print("Structure of LTS Model CV Results (Reweighted):")
print(str(cvFitLtsReweighted))

# Combine results into one object for visualization and analysis
cv_results <- data.frame(
  Model = c("LS", "MM", "LTS Raw", "LTS Reweighted"),
  CV_Score = c(cvFitLm$delta[1], cvFitLmrob$delta[1], cvFitLtsRaw$delta[1], cvFitLtsReweighted$delta[1])
)

# Print combined results
print("Combined CV Results:")
print(cv_results)

# Plotting the results
ggplot(cv_results, aes(x = Model, y = CV_Score, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Cross-Validation Scores for Regression Models",
       x = "Model Type",
       y = "CV Score") +
  theme_minimal()

