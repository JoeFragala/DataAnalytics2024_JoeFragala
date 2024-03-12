# Cook's Distance' example using mtcars
mtcars
head(mtcars)
str(mtcars)
model1 <- lm(mpg ~ cyl + wt , data = mtcars)
model1
help("cooks.distance")
plot(model1, pch = 18, col= 'red', which = c(4))

# we can use the cooks.distance() function to identify the Cook's distance to
# each observation
cooks.distance(model1)
CooksDistance <- cooks.distance(model1)

# Now we will round off the values to 5 decimal points so that it is easy to read
# we can use the round() function to round off values in R.
round(CooksDistance, 5)

# we can sort the values in ascending order 
sort(round(CooksDistance, 5))


# Outlier Detection using "Cooks Distance"
# Multivariate Regression using Cook's Distance
# Cook’s Distance is an estimate of the influence of a data point.
# Cook’s Distance is a summary of how much a regression model changes when the ith observation is removed from the data.
library(ISLR)
library(dplyr)
# Let's look at the baseball hitters dataset in ISLR package.
head(Hitters)
dim(Hitters)
is.na(Hitters) # check for the missing values.
# Now we will remove the NA (missing values) using the na.omit() function
HittersData <- na.omit(Hitters)
dim(HittersData) # checking the dimensions after removing the NAs.
glimpse(HittersData)
head(HittersData)
# Now we will implement a multivariate regression model using all the features in the dataset to
# predict the salary of the baseball player
SalaryPredictModel1 <- lm(Salary ~ ., data = HittersData)
summary(SalaryPredictModel1)
# Multiple R-squared: 0.5461, Adjusted R-squared: 0.5106

# Cook's Distance.
cooksD <- cooks.distance(SalaryPredictModel1)
influential <- cooksD[cooksD > (3 * mean(cooksD, na.rm = TRUE))]
influential
# We see that 18 players have a Cook’s Distance greater than 3x the mean.
# Let’s exclude these 18 players and rerun the model to see if we have a better fit in our regression model.
names_of_influential <- names(influential) # checking the names of the influential/outlier players
names_of_influential
outliers <- HittersData[names_of_influential,]
Hitters_Without_Outliers <- HittersData %>% anti_join(outliers)

# Model 2: without the outliers
SalaryPredictModel2 <- lm(Salary ~ ., data = Hitters_Without_Outliers)
summary(SalaryPredictModel2)
# Multiple R-squared: 0.6721, Adjusted R-squared: 0.6445
# We have improved from an Adjusted R-Squared of 0.5106 to 0.6445 with the removal of only 18 observations


# Normality Tests...
# Normal Distribution
# Read the documentation of the random distribution function
help("rnorm")
set.seed(10)
data1 <- rnorm(50)

set.seed(30)
data2 <- rnorm(50)
# Shapiro-Wilk Normality Test
# Read the documentation of haprio-Wilk Normality Test
help("shapiro.test")
# As the test returns a p-value less than 0.05, we reject the null hypothesis
# and conclude that the population data is not normally distributed.
shapiro.test(data1)
hist(data1, col='green')
# the histogram shows that the curve is mildly left skewed in nature
shapiro.test(data2)
# As the test returns a p-value greater than 0.05, we accept the null hypothesis
# and conclude that the population data is normally distributed.
hist(data2, col='steelblue') # the histogram shows that the curve is normally distributed in nature

# we set the seed to make the example reproducible
set.seed(0)
#create dataset of 100 random values generated from a normal distribution
data <- rnorm(100)
#perform Shapiro-Wilk test for normality
shapiro.test(data)
# The p-value of the test turns out to be 0.6303.
# Since this value is not less than 0.05, we can assume the sample data comes
# from a population that is normally distributed.
# This result shouldn’t be surprising since we generated the sample data using the rnorm() function,
# which generates random values from a normal distribution with mean = 0 and standard deviation = 1.


# Example 2: Shapiro-Wilk Test on Non-Normal Data
# The following code shows how to perform a Shapiro-Wilk test on a dataset with sample size n=100 in which
# the values are randomly generated from a Poisson distribution:
# we set the seed to make the example reproducible make this example reproducible
set.seed(0)
# Poisson Distribution
help("rpois")
#create dataset of 100 random values generated from a Poisson distribution
data <- rpois(n=100, lambda=3)
#perform Shapiro-Wilk test for normality
shapiro.test(data)
# The p-value of the test turns out to be 0.0003393.
# Since this value is less than 0.05, we have sufficient evidence
# to say that the sample data does not come from a population that is normally distributed.
# This result shouldn’t be surprising since we generated the sample data using the rpois() function,
# rpois() generates random values from a Poisson distribution.
hist(data, col='yellow')
# We can see that the distribution is right-skewed and doesn’t have the typical “bell-shape”,
# so it is not normal distribution.
# Histogram matches the results of the Shapiro-Wilk test and confirms that our sample data
# does not come from a normal distribution.
# ==============================================================================

library(nortest)
# Anderson-Darling test for normality
help("ad.test")
# To conduct an Anderson-Darling Test in R, we can use the ad.test() function within the nortest library.
# The following examples shows how to conduct an Anderson-Darling Test to check
# whether or not a vector of 100 values follows a normal distribution:
# Make sure to install and load "nortest" library
# install.packages('nortest')
library(nortest)
# we use seed() function make this example reproducible
set.seed(1)
# defined vector of 100 values that are normally distributed
x <- rnorm(100, 0, 1)
# conduct Anderson-Darling Test to test for normality
ad.test(x)

# Anderson-Darling normality test
#data:  x
# A = 0.16021, p-value = 0.9471
# This test returns two values: A: the test statistic.
# p-value: the corresponding p-value of the test statistic.
# The null hypothesis for the A-D test is that the data does follow a normal distribution.
# Thus, if our p-value for the test is below our significance level (common choices are 0.05, and 0.01),
# then we can reject the null hypothesis and conclude that we have sufficient evidence
# to say our data does not follow a normal distribution.
# In this case, our p-value is 0.9471.
# Since this is not below our significance level (let's say 0.05),
# we do not have sufficient evidence to reject the null hypothesis.
# Therefore, it’s safe to say that our data follows a normal distribution,
# which makes sense considering we generated 100 values that follow
# a normal distribution with a mean of 0 and standard deviation of 1 using the rnorm() function in R.

# Suppose instead we generate a vector of 100 values that follow a uniform distribution between 0 and 1.
# We can conduct an A-D test once again to see if this data follows a normal distribution:

set.seed(1)
# The Uniform Distribution
# defined vector of 100 values that are uniformly distributed
help("runif")
x <- runif(100, 0, 1)

#conduct Anderson-Darling Test to test for normality
ad.test(x)

# Anderson-Darling normality test
#
#data:  x
#A = 1.1472, p-value = 0.005086

# Our test statistic A equals 1.1472 and the corresponding p-value equals 0.005086.
# Since our p-value is less than 0.05, we can reject the null hypothesis and conclude that we have sufficient evidence
# to say this data does not follow a normal distribution.
# This matches the result we expected since we know that our data actually follows a uniform distribution.
