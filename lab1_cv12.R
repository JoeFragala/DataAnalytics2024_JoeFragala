# Load necessary libraries
library(robustbase)  # For robust regression methods
library(cvTools)     # For cross-validation tools
library(lattice)     # For creating density plots
library(MASS)
# Load the dataset
data("coleman")

# Set the seed for reproducibility
set.seed(1234)

# Set up folds for cross-validation
folds <- cvFolds(nrow(coleman), K = 5, R = 50)

## Perform cross-validation for an LS regression model
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

## Perform cross-validation for an MM regression model
# Increase k.max for better convergence potential in robust fitting
fitLmrob <- lmrob(Y ~ ., data = coleman, control = lmrob.control(k.max = 500))
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe, folds = folds, trim = 0.1)

## Perform cross-validation for an LTS regression model
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe, folds = folds, trim = 0.1)

## Combine results into one object for easy comparison
cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)

## Plot results for the MM regression model using lattice
print(densityplot(cvFitLmrob, main = "Density Plot for MM Regression Model"))

## Plot combined results of all models using lattice
print(densityplot(cvFits, main = "Combined Density Plots for All Models"))

## Compare raw and reweighted LTS estimators for 50% and 75% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

## Combine and plot results for LTS models with different subset analyses
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
print(densityplot(cvFitsLts, main = "Density Plots for LTS Models at 50% and 75% Subsets"))

