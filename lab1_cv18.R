# Load necessary libraries
library(MASS)    # For the datasets
library(boot)    # For cross-validation functions
library(ggplot2) # For visualization

# Load the mammals dataset from MASS package
data(mammals, package = "MASS")

# Set up the glm model for the mammals dataset
mammals.glm <- glm(log(brain) ~ log(body), data = mammals)

# Perform leave-one-out cross-validation
cv.err <- cv.glm(mammals, mammals.glm)$delta
print(paste("Leave-one-out CV error:", cv.err))

# Perform 6-fold cross-validation
cv.err.6 <- cv.glm(mammals, mammals.glm, K = 6)$delta
print(paste("6-fold CV error:", cv.err.6))

# As this is a linear model, calculate the leave-one-out cross-validation estimate without extra model-fitting
muhat <- fitted(mammals.glm)
mammals.diag <- glm.diag(mammals.glm)
cv.err.manual <- mean((mammals.glm$y - muhat)^2 / (1 - mammals.diag$h)^2)
print(paste("Manual calculation of LOOCV error:", cv.err.manual))

# Load the nodal dataset from MASS package
data(nodal, package = "MASS")

# Define a cost function appropriate for a binary outcome
cost <- function(r, pi = 0) mean(abs(r - pi) > 0.5)

# Set up the glm model for the nodal dataset using a binomial family
nodal.glm <- glm(r ~ stage + xray + acid, family = binomial, data = nodal)

# Perform leave-one-out cross-validation
cv.err.nodal <- cv.glm(nodal, nodal.glm, cost, K = nrow(nodal))$delta
print(paste("Leave-one-out CV error for nodal data:", cv.err.nodal))

# Perform 11-fold cross-validation
cv.11.err <- cv.glm(nodal, nodal.glm, cost, K = 11)$delta
print(paste("11-fold CV error for nodal data:", cv.11.err))

# Visualization of the fitted model against the actual data for mammals
ggplot(mammals, aes(x = log(body), y = log(brain))) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "gaussian"), se = FALSE, color = "blue") +
  labs(title = "Fit vs. Actual Values for Mammals Dataset", x = "Log(Body)", y = "Log(Brain)")
