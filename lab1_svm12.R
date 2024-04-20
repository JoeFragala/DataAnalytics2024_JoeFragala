# Load the kernlab package
library(kernlab)

# Assuming xtrain and ytrain are already prepared and are appropriate
# Example: Let's assume xtrain and ytrain are part of iris dataset
data(iris)
xtrain <- as.matrix(iris[iris$Species != "setosa", c("Sepal.Length", "Sepal.Width")])
ytrain <- as.factor(iris[iris$Species != "setosa", "Species"])

# Train the SVM with a linear kernel (vanilladot) and high cost (C)
svp <- ksvm(xtrain, ytrain, type = "C-svc", kernel = 'vanilladot', C = 100, scaled = FALSE)

# General summary of the model
print(svp)

# Attributes that you can access
print(attributes(svp))

# Accessing specific components of the SVM model
cat("Support Vectors:\n")
print(alpha(svp))  # Coefficients for the support vectors in the decision function
cat("Indices of Support Vectors:\n")
print(alphaindex(svp))  # Indices of support vectors
cat("Intercept b in the decision function:\n")
print(b(svp))  # Intercept term in the decision function

# Use the built-in function to plot the classifier
# Note: Effective only for 2D data
plot(svp, data = xtrain)
