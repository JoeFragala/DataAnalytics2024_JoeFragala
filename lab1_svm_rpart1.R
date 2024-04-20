# Install and load necessary packages
library(mlbench)
library(e1071)
library(rpart)

# Load the dataset
data(Glass, package = "mlbench")

# Partition the data
set.seed(1234)
index <- 1:nrow(Glass)
testindex <- sample(index, trunc(length(index)/3))
testset <- Glass[testindex,]
trainset <- Glass[-testindex,]

# Train models
svm.model <- svm(Type ~ ., data = trainset, cost = 100, gamma = 1)
svm.pred <- predict(svm.model, testset[,-10])  # Adjust column index as per actual data

rpart.model <- rpart(Type ~ ., data = trainset)
rpart.pred <- predict(rpart.model, testset[,-10], type = "class")

# Evaluate models
svmResults <- table(pred = svm.pred, true = testset[,10])  # Adjust column index as per actual data
print(svmResults)

rpartResults <- table(pred = rpart.pred, true = testset[,10])  # Adjust column index as per actual data
print(rpartResults)

