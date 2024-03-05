library(rpart)
library(sandwich)
library(party)
# Lab03_Exercise1: Heatmap(), image() and hierarchical clustering example

# creating a matrix data with random numbers and plotting the matrix using the 
# image() function you will see there, it does not have a real pattern in the plot. 
set.seed(12345) 
help(par) # par can be used to set or query graphical parameters. 
# Parameters can be set by specifying them as arguments to par in tag = value 
#form, or by passing them as a list of tagged values. 
par(mar = rep(0.2,4)) 
data_Matrix <- matrix(rnorm(400), nrow = 40) 
image(1:10, 1:40, t(data_Matrix)[,nrow(data_Matrix):1])

# Now we can run a hierarchical cluster analysis on the dataset we will use the 
# heatmap() function that is available in R 
help("heatmap") # read the documentation for the heatmap() function 
help(rep) # read the documentation for rep()

par(mar=rep(0.2,4)) 
heatmap(data_Matrix) 
# When we run the heatmap() here, we get the dendrograms printed on the both 
# columns and the rows and still there is no real immerging pattern that is 
# interesting to us. it is because there is no real interesting pattern 
# underlying in the data we generated.

# Now we will add a pattern to the data by doing a random coin flip. we will 
# use the rbinom() function along with a for-loop. 
help("rbinom") # read the documentation for rbinom()

set.seed(678910) 
for(i in 1:40){  
  # flipping a coin and getting the data  
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)  
  # if the coin is "Heads", add a common pattern to that row,  
  if(coin_Flip){    
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)  
  } 
}

# What I did here is, I looped through all the rows and, on a random row, I 
# flipped a coin. During the coin flip, if is it turn out to be one (true), 
# then, just added a pattern to my data in a way that the five of the columns 
# have a mean of zero and others have mean of three.

# Now we will plot the data. Now we can see that the right hand five columns 
# have more yellow in them, which means they have a higher value and the left 
# hand five columns that are little bit more in red color which means they have 
# a lower value. it is because some of the rows have a mean of three in the 
# right hand side, and some of the rows have mean of zero. Now we have 
# introduced some pattern to it.

par(mar= rep(0.2, 4)) 
image(1:10, 1:40, t(data_Matrix)[, nrow(data_Matrix):1])

# Now we will run the heatmap() function on the data, we can see that, two sets 
# of columns are easily separated. 
par(mar=rep(0.2, 4)) 
heatmap(data_Matrix)

# The dendrogram is on the top of the of the matrix, (which is on the top of 
# the columns), has clearly splits into two separate clusters. five on the left, 
# and five on the right on the rows, there is no real pattern that goes along 
# the rows 

hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered <- data_Matrix[hh$order,]
par(mfrow = c(1,3))
image(t(data_Matrix_Ordered)[, nrow(data_Matrix_Ordered):1])
plot(rowMeans(data_Matrix_Ordered), 40:1, xlab = "The Row Mean", ylab = "Row", pch=19)
plot(colMeans(data_Matrix_Ordered), xlab = "Column", ylab = "Column Mean", pch = 19)

# left plot has the original data reordered according the the hierarchical 
# cluster analysis of the rows. Middle plot has the mean of the each rows.
# (there are 40 rows and therefore 40 dots representing the mean) right hand 
# side plot has the means of the each columns (there are 10 columns and 
# therefore 10 dots representing the mean)


# Lab03_Exercise2: Classification
# Load the abalone data-set file
Abalone_data <- read.csv("C:/Users/fragaj3/Dropbox/Spring2024/CSCI_4600_01_Data_Analytics/Lab/Lab03/abalone.csv")
summary(Abalone_data)  #Summary of all Abalone_data
attach(Abalone_data)  #Sets 'default' option
Abalone_data  #Prints Abalone_data

# Convert Sex to numeric if necessary
Abalone_data$Sex <- as.numeric(as.factor(Abalone_data$Sex))

# Normalize the data excluding the target variable Rings
AD_Maxs <- apply(Abalone_data[, -ncol(Abalone_data)], 2, max) 
AD_Mins <- apply(Abalone_data[, -ncol(Abalone_data)], 2, min)
AD_Normalized <- as.data.frame(scale(Abalone_data[, -ncol(Abalone_data)], center = AD_Mins, scale = AD_Maxs - AD_Mins))
summary(AD_Normalized)

# Add the target variable Rings back into the normalized data
AD_Normalized$Rings <- Abalone_data$Ring

# Split the data into training and testing sets
set.seed(123) # for reproducibility
indices <- sample(1:nrow(AD_Normalized), size = 0.8 * nrow(AD_Normalized))
train_data <- AD_Normalized[indices,]
test_data <- AD_Normalized[-indices,]

# Prepare the training and testing sets
train_labels <- train_data$Rings
train_data <- train_data[, -ncol(train_data)]
test_labels <- test_data$Ring
test_data <- test_data[, -ncol(test_data)]

# Perform knn classification
k <- 5 # Number of neighbors, this can be tuned
knn_pred <- knn(train = train_data, test = test_data, cl = train_labels, k = k)

# Output the predictions and check the first few predictions
knn_pred
head(knn_pred)


# Lab03_Exercise3: Clarification
# Load the Iris data-set file
data(iris)
iris

# Create a new data frame without the fifth column (species)
iris_without_species <- iris[, -5]

# Apply k-means clustering to the new dataset
set.seed(123) # Setting a seed for reproducibility
iris_clusters <- kmeans(iris_without_species, centers = 3, nstart = 20, iter.max = 1000)

# Use table to compare true species with clustering results and print
clustering_result <- table(iris[, 5], iris_clusters$cluster)
clustering_result

# Load the Titanic data-set file
data(Titanic)
# Convert the Titanic data to a data frame if necessary
Titanic <- as.data.frame(Titanic)
View(Titanic)

# Build the rpart model and visualize
rpart_model <- rpart(Survived ~ ., data = Titanic, method = "class")
plot(rpart_model)
text(rpart_model, use.n = TRUE)

# Build the ctree model and visualize
ctree_model <- ctree(Survived ~., data = Titanic)
plot(ctree_model)

# Build the hclust model and visualize
dist_matrix <- dist(Titanic[, -which(names(Titanic) == "Survived")])
hclust_model <- hclust(dist_matrix)
plot(hclust_model)
