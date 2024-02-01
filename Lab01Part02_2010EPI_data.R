# Lab01Part02_2010EPI_data
library(ggplot2)  # Load the ggplot2 package
library(MASS)  # Load the MASS package
library(gapminder)  # Load the gapminder package
library(dplyr)  # Load the dplyr
library(gcookbook)  # Load the gcookbook

# Load the EPI data-set file
EPI_data <- read.csv("C:/Users/fragaj3/Dropbox/Spring2024/CSCI_4600_01_Data_Analytics/Lab/Lab01/2010EPI_data.csv")  #Reads in data from csv

# Exploration of basic methods
View(EPI_data)
summary(EPI_data)  #Summary of all EPI_data
str(EPI_data)  #Displays structure of R object
attach(EPI_data)  #Sets 'default' option
fix(EPI_data)  #Launches simple data editor
EPI_data  #Prints EPI_data
EPIwNA = EPI_data$EPI  #Stores all EPI values with under EPI
EPI_TF <- is.na(EPIwNA)  #Records True if a value is NA
EPI <- EPIwNA[!EPI_TF]  #Filters out all NA
summary(EPI)  #Summary of EPI without NA

# Exercise 1: Test Cumulative Density
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPI); qqline(EPI)
EPIx<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), EPIx, xlab= "Q-Q plot for t dsn") 
qqline(EPIx)

# Exercise 1: Cumulative Density
# EPI
plot(ecdf(EPI),do.points=FALSE,verticals= TRUE) 
plot(ecdf(EPI),do.points=TRUE,verticals= TRUE) # points are visible on the plot. 
par(pty="s") 
help("qqnorm") # read the RStudiodocumentation for qqnorm 
help("qqplot") # read the RStudiodocumentation for qqplot 
qqnorm(EPI) 
qqline(EPI) # adding the line on the Q-Q plot 
EPIx <-seq(30,95,1) 
EPIx
EPIx2 <-seq(30,95,2) 
EPIx2 
EPIx2 <-seq(30,96,2) 
EPIx2 
qqplot(qt(ppoints(250),df=5),EPIx, xlab= "Q-Q plot") 
qqline(EPIx)
qqplot(qt(ppoints(250),df=5),EPIx2, xlab= "Q-Q plot") 
qqline(EPIx2)

# DALY
DALYwNA = EPI_data$DALY  #Stores all EPI values with under EPI
DALY_TF <- is.na(DALYwNA)  #Records True if a value is NA
DALY <- DALYwNA[!DALY_TF]  #Filters out all NA
summary(DALY)  #Summary of DALY without NA
DALY

plot(ecdf(DALY),do.points=FALSE,verticals= TRUE) 
plot(ecdf(DALY),do.points=TRUE,verticals= TRUE) # points are visible on the plot. 
par(pty="s") 
qqnorm(DALY) 
qqline(DALY) # adding the line on the Q-Q plot 
DALYx <-seq(30,95,1) 
DALYx
DALYx2 <-seq(30,95,2) 
DALYx2 
qqplot(qt(ppoints(250),df=5),DALYx, xlab= "Q-Q plot") 
qqline(DALYx)
qqplot(qt(ppoints(250),df=5),DALYx2, xlab= "Q-Q plot") 
qqline(DALYx2)

# WATER_H
WATER_HwNA = EPI_data$WATER_H  #Stores all EPI values with under EPI
WATER_H_TF <- is.na(WATER_HwNA)  #Records True if a value is NA
WATER_H <- WATER_HwNA[!WATER_H_TF]  #Filters out all NA
summary(WATER_H)  #Summary of DALY without NA
WATER_H

plot(ecdf(WATER_H),do.points=FALSE,verticals= TRUE) 
plot(ecdf(WATER_H),do.points=TRUE,verticals= TRUE) # points are visible on the plot. 
par(pty="s") 
qqnorm(WATER_H) 
qqline(WATER_H) # adding the line on the Q-Q plot 
WATER_Hx <-seq(30,95,1) 
WATER_Hx
WATER_Hx2 <-seq(30,95,2) 
WATER_Hx2 
qqplot(qt(ppoints(250),df=5),WATER_Hx, xlab= "Q-Q plot") 
qqline(WATER_Hx)
qqplot(qt(ppoints(250),df=5),WATER_Hx2, xlab= "Q-Q plot") 
qqline(WATER_Hx2)

# WATER_E
WATER_EwNA = EPI_data$WATER_E  #Stores all EPI values with under EPI
WATER_E_TF <- is.na(WATER_EwNA)  #Records True if a value is NA
WATER_E <- WATER_EwNA[!WATER_E_TF]  #Filters out all NA
summary(WATER_E)  #Summary of WATER_E without NA
WATER_E

plot(ecdf(WATER_E),do.points=FALSE,verticals= TRUE) 
plot(ecdf(WATER_E),do.points=TRUE,verticals= TRUE) # points are visible on the plot. 
par(pty="s") 
qqnorm(WATER_E) 
qqline(WATER_E) # adding the line on the Q-Q plot 
WATER_Ex <-seq(30,95,1) 
WATER_Ex
WATER_Ex2 <-seq(30,95,2) 
WATER_Ex2 
qqplot(qt(ppoints(250),df=5),WATER_Ex, xlab= "Q-Q plot") 
qqline(WATER_Ex)
qqplot(qt(ppoints(250),df=5),WATER_Ex2, xlab= "Q-Q plot") 
qqline(WATER_Ex2)

# AIR_H
AIR_HwNA = EPI_data$AIR_H  #Stores all EPI values with under EPI
AIR_H_TF <- is.na(AIR_HwNA)  #Records True if a value is NA
AIR_H <- AIR_HwNA[!AIR_H_TF]  #Filters out all NA
summary(AIR_H)  #Summary of AIR_H without NA
AIR_H

plot(ecdf(AIR_H),do.points=FALSE,verticals= TRUE) 
plot(ecdf(AIR_H),do.points=TRUE,verticals= TRUE) # points are visible on the plot. 
par(pty="s") 
qqnorm(AIR_H) 
qqline(AIR_H) # adding the line on the Q-Q plot 
AIR_Hx <-seq(30,95,1) 
AIR_Hx
AIR_Hx2 <-seq(30,95,2) 
AIR_Hx2 
qqplot(qt(ppoints(250),df=5),AIR_Hx, xlab= "Q-Q plot") 
qqline(AIR_Hx)
qqplot(qt(ppoints(250),df=5),AIR_Hx2, xlab= "Q-Q plot") 
qqline(AIR_Hx2)

# AIR_E
AIR_EwNA = EPI_data$AIR_E  #Stores all EPI values with under EPI
AIR_E_TF <- is.na(AIR_EwNA)  #Records True if a value is NA
AIR_E <- AIR_EwNA[!AIR_E_TF]  #Filters out all NA
summary(AIR_E)  #Summary of AIR_E without NA
AIR_E

plot(ecdf(AIR_E),do.points=FALSE,verticals= TRUE) 
plot(ecdf(AIR_E),do.points=TRUE,verticals= TRUE) # points are visible on the plot. 
par(pty="s") 
qqnorm(AIR_E) 
qqline(AIR_E) # adding the line on the Q-Q plot 
AIR_Ex <-seq(30,95,1) 
AIR_Ex
AIR_Ex2 <-seq(30,95,2) 
AIR_Ex2 
qqplot(qt(ppoints(250),df=5),AIR_Ex, xlab= "Q-Q plot") 
qqline(AIR_Ex)
qqplot(qt(ppoints(250),df=5),AIR_Ex2, xlab= "Q-Q plot") 
qqline(AIR_Ex2)

# ENVHEALTH
ENVHEALTHwNA = EPI_data$ENVHEALTH  #Stores all EPI values with under EPI
ENVHEALTH_TF <- is.na(ENVHEALTHwNA)  #Records True if a value is NA
ENVHEALTH <- ENVHEALTHwNA[!ENVHEALTH_TF]  #Filters out all NA
summary(ENVHEALTH)  #Summary of ENVHEALTH without NA
ENVHEALTH

plot(ecdf(ENVHEALTH),do.points=FALSE,verticals= TRUE) 
plot(ecdf(ENVHEALTH),do.points=TRUE,verticals= TRUE) # points are visible on the plot. 
par(pty="s") 
qqnorm(ENVHEALTH) 
qqline(ENVHEALTH) # adding the line on the Q-Q plot 
ENVHEALTHx <-seq(30,95,1) 
ENVHEALTHx
ENVHEALTHx2 <-seq(30,95,2) 
ENVHEALTHx2 
qqplot(qt(ppoints(250),df=5),ENVHEALTHx, xlab= "Q-Q plot") 
qqline(ENVHEALTHx)
qqplot(qt(ppoints(250),df=5),ENVHEALTHx2, xlab= "Q-Q plot") 
qqline(ENVHEALTHx2)

# ECOSYSTEM
ECOSYSTEMwNA = EPI_data$ECOSYSTEM  #Stores all EPI values with under EPI
ECOSYSTEM_TF <- is.na(DALYwNA)  #Records True if a value is NA
ECOSYSTEM <- ECOSYSTEMwNA[!ECOSYSTEM_TF]  #Filters out all NA
summary(ECOSYSTEM)  #Summary of ECOSYSTEM without NA
ECOSYSTEM

plot(ecdf(ECOSYSTEM),do.points=FALSE,verticals= TRUE) 
plot(ecdf(ECOSYSTEM),do.points=TRUE,verticals= TRUE) # points are visible on the plot. 
par(pty="s") 
qqnorm(ECOSYSTEM) 
qqline(ECOSYSTEM) # adding the line on the Q-Q plot 
ECOSYSTEMx <-seq(30,95,1) 
ECOSYSTEMx
ECOSYSTEMx2 <-seq(30,95,2) 
ECOSYSTEMx2 
qqplot(qt(ppoints(250),df=5),ECOSYSTEMx, xlab= "Q-Q plot") 
qqline(ECOSYSTEMx)
qqplot(qt(ppoints(250),df=5),ECOSYSTEMx2, xlab= "Q-Q plot") 
qqline(ECOSYSTEMx2)

# BIODIVERSITY
BIODIVERSITYwNA = EPI_data$BIODIVERSITY  #Stores all EPI values with under EPI
BIODIVERSITY_TF <- is.na(BIODIVERSITYwNA)  #Records True if a value is NA
BIODIVERSITY <- BIODIVERSITYwNA[!BIODIVERSITY_TF]  #Filters out all NA
summary(BIODIVERSITY)  #Summary of BIODIVERSITY without NA
BIODIVERSITY

plot(ecdf(BIODIVERSITY),do.points=FALSE,verticals= TRUE) 
plot(ecdf(BIODIVERSITY),do.points=TRUE,verticals= TRUE) # points are visible on the plot. 
par(pty="s") 
qqnorm(BIODIVERSITY) 
qqline(BIODIVERSITY) # adding the line on the Q-Q plot 
BIODIVERSITYx <-seq(30,95,1) 
BIODIVERSITYx
BIODIVERSITYx2 <-seq(30,95,2) 
BIODIVERSITYx2 
qqplot(qt(ppoints(250),df=5),BIODIVERSITYx, xlab= "Q-Q plot") 
qqline(BIODIVERSITYx)
qqplot(qt(ppoints(250),df=5),BIODIVERSITYx2, xlab= "Q-Q plot") 
qqline(BIODIVERSITYx2)


# Exercise 1: Comparing distributions
# EPI
boxplot(EPI,DALY) 
qqplot(EPI,DALY)

boxplot(EPI,WATER_H) 
qqplot(EPI,WATER_H)

boxplot(EPI,WATER_E) 
qqplot(EPI,WATER_E)

boxplot(EPI,AIR_H) 
qqplot(EPI,AIR_H)

boxplot(EPI,AIR_E) 
qqplot(EPI,AIR_E)

boxplot(EPI,ENVHEALTH) 
qqplot(EPI,ENVHEALTH)

boxplot(EPI,ECOSYSTEM) 
qqplot(EPI,ECOSYSTEM)

boxplot(EPI,BIODIVERSITY) 
qqplot(EPI,BIODIVERSITY)

# DALY
boxplot(DALY,WATER_H) 
qqplot(DALY,WATER_H)

boxplot(DALY,WATER_E) 
qqplot(DALY,WATER_E)

boxplot(DALY,AIR_H) 
qqplot(DALY,AIR_H)

boxplot(DALY,AIR_E) 
qqplot(DALY,AIR_E)

boxplot(DALY,ENVHEALTH) 
qqplot(DALY,ENVHEALTH)

boxplot(DALY,ECOSYSTEM) 
qqplot(DALY,ECOSYSTEM)

boxplot(DALY,BIODIVERSITY) 
qqplot(DALY,BIODIVERSITY)

# WATER_H
boxplot(WATER_H,WATER_E) 
qqplot(WATER_H,WATER_E)

boxplot(WATER_H,AIR_H) 
qqplot(WATER_H,AIR_H)

boxplot(WATER_H,AIR_E) 
qqplot(WATER_H,AIR_E)

boxplot(WATER_H,ENVHEALTH) 
qqplot(WATER_H,ENVHEALTH)

boxplot(WATER_H,ECOSYSTEM) 
qqplot(WATER_H,ECOSYSTEM)

boxplot(WATER_H,BIODIVERSITY) 
qqplot(WATER_H,BIODIVERSITY)

# WATER_E
boxplot(WATER_E,AIR_H) 
qqplot(WATER_E,AIR_H)

boxplot(WATER_E,AIR_E) 
qqplot(WATER_E,AIR_E)

boxplot(WATER_E,ENVHEALTH) 
qqplot(WATER_E,ENVHEALTH)

boxplot(WATER_E,ECOSYSTEM) 
qqplot(WATER_E,ECOSYSTEM)

boxplot(WATER_E,BIODIVERSITY) 
qqplot(WATER_E,BIODIVERSITY)

# AIR_H
boxplot(AIR_H,AIR_E) 
qqplot(AIR_H,AIR_E)

boxplot(AIR_H,ENVHEALTH) 
qqplot(AIR_H,ENVHEALTH)

boxplot(AIR_H,ECOSYSTEM) 
qqplot(AIR_H,ECOSYSTEM)

boxplot(AIR_H,BIODIVERSITY) 
qqplot(AIR_H,BIODIVERSITY)

# AIR_E
boxplot(AIR_E,ENVHEALTH) 
qqplot(AIR_E,ENVHEALTH)

boxplot(AIR_E,ECOSYSTEM) 
qqplot(AIR_E,ECOSYSTEM)

boxplot(AIR_E,BIODIVERSITY) 
qqplot(AIR_E,BIODIVERSITY)

# ENVHEALTH
boxplot(ENVHEALTH,ECOSYSTEM) 
qqplot(ENVHEALTH,ECOSYSTEM)

boxplot(ENVHEALTH,BIODIVERSITY) 
qqplot(ENVHEALTH,BIODIVERSITY)

# ECOSYSTEM
boxplot(ECOSYSTEM,BIODIVERSITY) 
qqplot(ECOSYSTEM,BIODIVERSITY)


# Exercise 2: Creating Bar Graphs
barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl)) # generate a table of counts.
qplot(mtcars$cyl) # cyl is continuous here
qplot(factor(mtcars$cyl)) # treat cyl as discrete
# Bar graph of counts
qplot(factor(cyl), data = mtcars)
ggplot(mtcars, aes(x=factor(cyl))) + geom_bar()

# Exercise 2: Creating Histogram
hist(mtcars$mpg)
hist(mtcars$mpg, breaks = 10) # specify approximate number of bins with breaks.
hist(mtcars$mpg, breaks = 5)
hist(mtcars$mpg, breaks = 12)

qplot(mpg, data = mtcars, binwidth=4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 4)
ggplot(mtcars, aes(x=mpg)) + geom_histogram(binwidth = 5)


# Exercise 2: Creating Box-plot
plot(ToothGrowth$supp, ToothGrowth$len) # using plot() function and pass it a factor of x-values and a vector of y-values.
# Formula Syntax
boxplot(len ~ supp, data = ToothGrowth) # if the two vectors are in the same dataframe, you can use the formula syntax. With this syntax you can combine two variables on the x-axis.
# put interaction of two variables on x-axis
boxplot(len ~ supp + dose, data = ToothGrowth)
qplot(ToothGrowth$supp, ToothGrowth$len, geom = "boxplot")
# if the two vectors are in the same dataframe, you can use the following syntax
qplot(supp, len, data = ToothGrowth, geom = "boxplot")
# in ggplot2, the above is equivalent to:
ggplot(ToothGrowth, aes(x=supp, y=len)) + geom_boxplot()

# Using three separate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose), ToothGrowth$len, geom = "boxplot")
# You can write the same thing above, get the columns from the dataframe
qplot(interaction(supp, dose), len, data = ToothGrowth, geom = "boxplot")
# Using ggplot() you can do the samething and it is equivalent to:
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) + geom_boxplot()
#Plotting a function curve

# Exercise 2: Visual Exercises
# China
str(gapminder)
China <- gapminder %>% filter(country == "China") %>% head(10)
View(China)
ggplot(data = China, aes(x=year,y=lifeExp)) + geom_point(color = 'red', size = 3) + xlab('Year') +ylab('Life Expectancy')+ggtitle("Life Expectancy in China")+ theme_bw(base_size = 18)
ggplot(data = gapminder, aes(x= year, y=lifeExp, group =country,color =continent)) + geom_line() +xlab('Year')+ylab('Life Expectancy')+ggtitle("Life Expectancy in Countries")+theme_bw()
ggplot(data = gapminder, aes(x= year, y=lifeExp, group=country,color=continent))+geom_line()+theme_bw()+facet_wrap(~continent)+ xlab('Year')+ylab('Life Expectancy')+ggtitle("Life Expectancy in Countries")
ggplot(data = China, aes(x=year,y=gdpPercap))+geom_line()+scale_y_log10(breaks=c(1000,2000,3000,4000,5000),labels=scales::dollar)+xlim(1940,2010)+theme_gray(base_size = 20)

# Diamonds
data(diamonds)
head(diamonds)          # look at the data!
#
ggplot(diamonds, aes(clarity, fill=cut)) + geom_bar()
ggplot(diamonds, aes(clarity)) + geom_bar() + facet_wrap(~ cut)
ggplot(diamonds) + geom_histogram(aes(x=price)) + geom_vline(xintercept=12000)

ggplot(
  data = diamonds,
  mapping = aes(color = cut_number(carat, 5), x = price)
) +
  geom_freqpoly() +
  labs(x = "Price", y = "Count", color = "Carat")

ggplot(diamonds, aes(x = cut_number(price, 10), y = carat)) +
  geom_boxplot() +
  coord_flip() +
  xlab("Price")

ggplot(diamonds, aes(x = cut_number(carat, 5), y = price, colour = cut)) +
  geom_boxplot()

# Viewed the Chapter3__Bar_Graphs_R_Graphics.R file separately