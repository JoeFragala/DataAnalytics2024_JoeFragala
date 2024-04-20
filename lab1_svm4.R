# Ensure the kernlab package is loaded for SVM and dataset
if (!require(kernlab)) install.packages("kernlab")
library(kernlab)

# Load the promotergene dataset
data(promotergene)

# Check if data has sufficient rows
if (dim(promotergene)[1] < 20) {
  stop("Not enough data points to create a test set of size 20.")
}

# Create test and training sets
set.seed(123)  # for reproducibility
ind <- sample(1:dim(promotergene)[1], 20)
genetrain <- promotergene[-ind, ]
genetest <- promotergene[ind, ]

# Train a support vector machine with specified parameters and cross-validation
gene <- ksvm(Class ~ ., data = genetrain, kernel = "rbfdot",
             kpar = list(sigma = 0.015), C = 70, cross = 4, prob.model = TRUE)

# Predict gene type probabilities on the test set
genetype <- predict(gene, genetest, type = "probabilities")

# Output predicted probabilities
print(genetype)

