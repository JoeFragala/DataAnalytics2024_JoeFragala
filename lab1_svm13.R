# Load necessary library
library(kernlab)

# Function to generate cross-validation folds
cv.folds <- function(n, folds=3) {
  # This function creates a list of indices for k-fold cross-validation
  # n: total number of samples
  # folds: number of folds
  split(sample(n), rep(1:folds, each=ceiling(n/folds), length.out=n))
}

# Function to perform k-fold cross-validation using SVM
cv.ksvm <- function(x, y, folds=3, ...) {
  # Prepare storage for predictions
  ypred <- vector("list", length=length(y))
  # Get folds
  indices <- cv.folds(length(y), folds)
  
  # Loop over each fold
  for (i in seq_along(indices)) {
    # Separate train and test data
    test_indices <- indices[[i]]
    train_indices <- unlist(indices[-i])
    
    # Train model
    model <- ksvm(x[train_indices, ], y[train_indices], ..., scaled=FALSE)
    
    # Predict on test set
    ypred[test_indices] <- predict(model, x[test_indices, ])
  }
  
  # Return all predictions as a vector
  unlist(ypred)
}

# Example Data Preparation
data(iris)
x <- as.matrix(iris[, 1:4])
y <- iris$Species

# Perform k-fold cross-validation
ypred <- cv.ksvm(x, y, folds=5, type="C-svc", kernel='vanilladot', C=1)

# Evaluate predictions (Example: Accuracy)
accuracy <- mean(ypred == y)
print(paste("Accuracy from custom CV function:", accuracy))

# Using ksvm's built-in cross-validation to compare
svp <- ksvm(x, y, type="C-svc", kernel='vanilladot', C=1, cross=5)
cross_validation_result <- cross(svp)  # This returns cross-validation error

# Print results from built-in cross-validation
print(paste("Cross-validation error from built-in function:", cross_validation_result))

