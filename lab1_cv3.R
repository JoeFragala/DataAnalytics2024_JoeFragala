library("robustbase")
require(cvTools)
data("coleman")
set.seed(1234)  # Ensure reproducibility

# Set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

# Fit LTS models for 50% and 75% subsets and perform cross-validation
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

# Combine and rename results
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
print(cvFitsLts)  # Checking combined results

# Renaming for clarity
cvNames(cvFitsLts) <- c("improved", "initial")
print(cvFitsLts)  # Print with new names

# Adding visualizations
library(ggplot2)
results_df <- data.frame(
  Subset = rep(c("50%", "75%"), each = 2),
  Type = rep(c("Improved", "Initial"), 2),
  Score = c(cvFitLts50$delta, cvFitLts75$delta)
)

ggplot(results_df, aes(x = Subset, y = Score, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Cross-Validation Results for LTS Fits", x = "Subset Size", y = "CV Score")

# Save results with error handling
tryCatch({
  save(cvFitsLts, file = "cvFitsLts.RData")
}, error = function(e) {
  cat("Error in saving results: ", e$message, "\n")
})

