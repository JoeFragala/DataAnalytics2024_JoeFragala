# Load necessary libraries
library(randomForest)  # For random forest modeling
library(rpart)         # Contains the kyphosis dataset

# Ensure the dataset is available
if (!"kyphosis" %in% data(package = "rpart")$results[, "Item"]) {
  stop("Dataset 'kyphosis' is not available in the 'rpart' package. Please check for the correct package.")
}

# Load the Kyphosis dataset
data("kyphosis", package = "rpart")

# Display the first few rows to ensure it's loaded correctly
head(kyphosis)

# Perform random forest analysis on the Kyphosis dataset
fitKF <- randomForest(Kyphosis ~ Age + Number + Start, data = kyphosis)
print(fitKF)  # View results of the random forest model

# View the importance of each predictor
importanceMatrix <- importance(fitKF)
print(importanceMatrix)

# Plot the importance of predictors
varImpPlot(fitKF)

# Additional random forest analysis on another dataset for comparison
# Load and prepare Swiss dataset from the datasets package
data("swiss", package = "datasets")
fitSwiss <- randomForest(Fertility ~ ., data = swiss)
print(fitSwiss)  # View results
importance(fitSwiss)  # View importance of each predictor

# Visualize the importance of predictors for the Swiss dataset
varImpPlot(fitSwiss)

# Plot the error rate and other metrics over trees in the forest
plot(fitSwiss)

# Retrieve and print a single tree from the Swiss model
treeSwiss <- getTree(fitSwiss, 1, labelVar = TRUE)
print(treeSwiss)
