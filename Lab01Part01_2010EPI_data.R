# Lab01Part01_2010EPI_data
library(MASS)  # Load the necessary library for distribution fitting

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

# Exercise 1: exploring the distribution
summary(EPI)  # stats
fivenum(EPI, na.rm=TRUE)
help(stem)
stem(EPI)  # stem and leaf plot
help(hist)
hist(EPI)
hist(EPI, seq(30., 95., 1.0), prob=TRUE)
help(lines)
lines(density(EPI, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
help(rug)
rug(EPI) 

# Exercise 1: fitting a distribution beyond histograms
plot(ecdf(EPI), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPI); qqline(EPI)
EPIx<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), EPIx, xlab= "Q-Q plot for t dsn")
qqline(EPIx)

# Exercise 1: fitting a distribution. Do the same exploration 
# and fitting for another 2 variables in the EPI_data, i.e.
# primary variables (DALY, WATER_H, …). Try fitting other 
# distributions –i.e.as ecdf or qq

# DALY
DALYwNA = EPI_data$DALY  #Stores all EPI values with under EPI
DALY_TF <- is.na(DALYwNA)  #Records True if a value is NA
DALY <- DALYwNA[!DALY_TF]  #Filters out all NA
summary(DALY)  #Summary of DALY without NA

DALY
fivenum(DALY, na.rm=TRUE)
stem(DALY)  # stem and leaf plot
hist(DALY)
hist(DALY, seq(0., 95., 1.0), prob=TRUE)
lines(density(DALY, na.rm=TRUE, bw=1.))  # or try bw=“SJ” # look at data format if not working
rug(DALY)

plot(ecdf(DALY), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(DALY); qqline(DALY)
DALYx<-seq(0,95,1)
qqplot(qt(ppoints(250), df= 5), DALYx, xlab= "Q-Q plot for t dsn")
qqline(DALYx)

# WATER_H
WATER_HwNA = EPI_data$WATER_H  #Stores all EPI values with under EPI
WATER_H_TF <- is.na(WATER_HwNA)  #Records True if a value is NA
WATER_H <- WATER_HwNA[!WATER_H_TF]  #Filters out all NA
summary(WATER_H)  #Summary of DALY without NA

fivenum(WATER_H, na.rm=TRUE)
stem(WATER_H)  # stem and leaf plot
hist(WATER_H)
hist(WATER_H, seq(0., 100., 1.0), prob=TRUE)
lines(density(WATER_H, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
rug(WATER_H)

plot(ecdf(WATER_H), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(WATER_H); qqline(WATER_H)
WATER_Hx<-seq(0,100,1)
qqplot(qt(ppoints(250), df= 5), WATER_Hx, xlab= "Q-Q plot for t dsn")
qqline(WATER_Hx)

# WATER_E
WATER_EwNA = EPI_data$WATER_E  #Stores all EPI values with under EPI
WATER_E_TF <- is.na(WATER_EwNA)  #Records True if a value is NA
WATER_E <- WATER_EwNA[!WATER_E_TF]  #Filters out all NA
summary(WATER_E)  #Summary of WATER_E without NA

fivenum(WATER_E, na.rm=TRUE)
stem(WATER_E)  # stem and leaf plot
hist(WATER_E)
hist(WATER_E, seq(0., 100., 1.0), prob=TRUE)
lines(density(WATER_E, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
rug(WATER_E)

plot(ecdf(WATER_E), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(WATER_E); qqline(WATER_E)
WATER_Ex<-seq(0,100,1)
qqplot(qt(ppoints(250), df= 5), WATER_Ex, xlab= "Q-Q plot for t dsn")
qqline(WATER_Ex)

# AIR_H
AIR_HwNA = EPI_data$AIR_H  #Stores all EPI values with under EPI
AIR_H_TF <- is.na(AIR_HwNA)  #Records True if a value is NA
AIR_H <- AIR_HwNA[!AIR_H_TF]  #Filters out all NA
summary(AIR_H)  #Summary of AIR_H without NA

AIR_H
fivenum(AIR_H, na.rm=TRUE)
stem(AIR_H)  # stem and leaf plot
hist(AIR_H)
hist(AIR_H, seq(0., 100., 1.0), prob=TRUE)
lines(density(AIR_H, na.rm=TRUE, bw=1.))  # or try bw=“SJ” # look at data format if not working
rug(AIR_H)

plot(ecdf(AIR_H), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(AIR_H); qqline(AIR_H)
AIR_Hx<-seq(0,100,1)
qqplot(qt(ppoints(250), df= 5), AIR_Hx, xlab= "Q-Q plot for t dsn")
qqline(AIR_Hx)

# AIR_E
AIR_EwNA = EPI_data$AIR_E  #Stores all EPI values with under EPI
AIR_E_TF <- is.na(AIR_EwNA)  #Records True if a value is NA
AIR_E <- AIR_EwNA[!AIR_E_TF]  #Filters out all NA
summary(AIR_E)  #Summary of AIR_E without NA

AIR_E
fivenum(AIR_E, na.rm=TRUE)
stem(AIR_E)  # stem and leaf plot
hist(AIR_E)
hist(AIR_E, seq(10., 90., 1.0), prob=TRUE)
lines(density(AIR_E, na.rm=TRUE, bw=1.))  # or try bw=“SJ” # look at data format if not working
rug(AIR_E)

plot(ecdf(AIR_E), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(AIR_E); qqline(AIR_E)
AIR_Ex<-seq(10,90,1)
qqplot(qt(ppoints(250), df= 5), AIR_Ex, xlab= "Q-Q plot for t dsn")
qqline(AIR_Ex)

# ENVHEALTH
ENVHEALTHwNA = EPI_data$ENVHEALTH  #Stores all EPI values with under EPI
ENVHEALTH_TF <- is.na(ENVHEALTHwNA)  #Records True if a value is NA
ENVHEALTH <- ENVHEALTHwNA[!ENVHEALTH_TF]  #Filters out all NA
summary(ENVHEALTH)  #Summary of ENVHEALTH without NA

ENVHEALTH
fivenum(ENVHEALTH, na.rm=TRUE)
stem(ENVHEALTH)  # stem and leaf plot
hist(ENVHEALTH)
hist(ENVHEALTH, seq(0., 100., 1.0), prob=TRUE)
lines(density(ENVHEALTH, na.rm=TRUE, bw=1.))  # or try bw=“SJ” # look at data format if not working
rug(ENVHEALTH)

plot(ecdf(ENVHEALTH), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(ENVHEALTH); qqline(ENVHEALTH)
ENVHEALTHx<-seq(0,100,1)
qqplot(qt(ppoints(250), df= 5), ENVHEALTHx, xlab= "Q-Q plot for t dsn")
qqline(ENVHEALTHx)

# ECOSYSTEM
ECOSYSTEMwNA = EPI_data$ECOSYSTEM  #Stores all EPI values with under EPI
ECOSYSTEM_TF <- is.na(DALYwNA)  #Records True if a value is NA
ECOSYSTEM <- ECOSYSTEMwNA[!ECOSYSTEM_TF]  #Filters out all NA
summary(ECOSYSTEM)  #Summary of ECOSYSTEM without NA

ECOSYSTEM
fivenum(ECOSYSTEM, na.rm=TRUE)
stem(ECOSYSTEM)  # stem and leaf plot
hist(ECOSYSTEM)
hist(ECOSYSTEM, seq(0., 100., 1.0), prob=TRUE)
lines(density(ECOSYSTEM, na.rm=TRUE, bw=1.))  # or try bw=“SJ” # look at data format if not working
rug(ECOSYSTEM)

plot(ecdf(ECOSYSTEM), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(ECOSYSTEM); qqline(ECOSYSTEM)
ECOSYSTEMx<-seq(0,100,1)
qqplot(qt(ppoints(250), df= 5), ECOSYSTEMx, xlab= "Q-Q plot for t dsn")
qqline(ECOSYSTEMx)

# BIODIVERSITY
BIODIVERSITYwNA = EPI_data$BIODIVERSITY  #Stores all EPI values with under EPI
BIODIVERSITY_TF <- is.na(BIODIVERSITYwNA)  #Records True if a value is NA
BIODIVERSITY <- BIODIVERSITYwNA[!BIODIVERSITY_TF]  #Filters out all NA
summary(BIODIVERSITY)  #Summary of BIODIVERSITY without NA

BIODIVERSITY
fivenum(BIODIVERSITY, na.rm=TRUE)
stem(BIODIVERSITY)  # stem and leaf plot
hist(BIODIVERSITY)
hist(BIODIVERSITY, seq(0., 100., 1.0), prob=TRUE)
lines(density(BIODIVERSITY, na.rm=TRUE, bw=1.))  # or try bw=“SJ” # look at data format if not working
rug(BIODIVERSITY)

plot(ecdf(BIODIVERSITY), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(BIODIVERSITY); qqline(BIODIVERSITY)
BIODIVERSITYx<-seq(0,100,1)
qqplot(qt(ppoints(250), df= 5), BIODIVERSITYx, xlab= "Q-Q plot for t dsn")
qqline(BIODIVERSITYx)

# Comparing distributions
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


# Exercise 2: filtering (populations)/Conditional 
# Filtering and Repeat Exercise 1…: 
# Landlock
EPILandlockwNA <- EPI[!Landlock]
EPILandlock <- EPILandlockwNA[!is.na(EPILandlockwNA)]
summary(EPILandlock)

fivenum(EPILandlock, na.rm=TRUE)
stem(EPILandlock)  # stem and leaf plot
hist(EPILandlock)
hist(EPILandlock, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPILandlock, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
rug(EPILandlock)

plot(ecdf(EPILandlock), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPILandlock); qqline(EPILandlock)
EPILandlockx<-seq(0,100,1)
qqplot(qt(ppoints(250), df= 5), EPILandlockx, xlab= "Q-Q plot for t dsn")
qqline(EPILandlockx)

# No_surface_water
EPINo_surface_waterwNA <- EPI[!No_surface_water]
EPINo_surface_water <- EPINo_surface_waterwNA[!is.na(EPINo_surface_waterwNA)]
summary(EPINo_surface_water)

fivenum(EPINo_surface_water, na.rm=TRUE)
stem(EPINo_surface_water)  # stem and leaf plot
hist(EPINo_surface_water)
hist(EPINo_surface_water, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPINo_surface_water, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
rug(EPINo_surface_water)

plot(ecdf(EPINo_surface_water), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPINo_surface_water); qqline(EPINo_surface_water)
EPINo_surface_waterx<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), EPINo_surface_waterx, xlab= "Q-Q plot for t dsn")
qqline(EPINo_surface_waterx)

# Desert
EPIDesertwNA <- EPI[!Desert]
EPIDesert <- EPIDesertwNA[!is.na(EPIDesertwNA)]
summary(EPIDesert)

fivenum(EPIDesert, na.rm=TRUE)
stem(EPIDesert)  # stem and leaf plot
hist(EPIDesert)
hist(EPIDesert, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPIDesert, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
rug(EPIDesert)

plot(ecdf(EPIDesert), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPIDesert); qqline(EPIDesert)
EPIDesertx<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), EPIDesertx, xlab= "Q-Q plot for t dsn")
qqline(EPIDesertx)

# High_Population_Density
EPIHigh_Population_DensitywNA <- EPI[!High_Population_Density]
EPIHigh_Population_Density <- EPIDesertwNA[!is.na(EPIHigh_Population_DensitywNA)]
summary(EPIHigh_Population_Density)

fivenum(EPIHigh_Population_Density, na.rm=TRUE)
stem(EPIHigh_Population_Density)  # stem and leaf plot
hist(EPIHigh_Population_Density)
hist(EPIHigh_Population_Density, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPIHigh_Population_Density, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
rug(EPIHigh_Population_Density)

plot(ecdf(EPIHigh_Population_Density), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPIHigh_Population_Density); qqline(EPIHigh_Population_Density)
EPIHigh_Population_Densityx<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), EPIHigh_Population_Densityx, xlab= "Q-Q plot for t dsn")
qqline(EPIHigh_Population_Densityx)

# # Landlock
# boxplot(Landlock,No_surface_water) 
# qqplot(Landlock,No_surface_water)
# 
# boxplot(Landlock,Desert) 
# qqplot(Landlock,Desert)
# 
# boxplot(Landlock,High_Population_Density) 
# qqplot(Landlock,High_Population_Density)
# 
# # No_surface_water
# boxplot(No_surface_water,Desert) 
# qqplot(No_surface_water,Desert)
# 
# boxplot(No_surface_water,High_Population_Density) 
# qqplot(No_surface_water,High_Population_Density)
# 
# # Desert
# boxplot(Desert,High_Population_Density) 
# qqplot(Desert,High_Population_Density)

# Exercise 2: Filter on EPI_regions or GEO_subregion?
# South_Asia
EPISouth_AsiawNA <- EPI[!EPI_regions == "South Asia"]
EPISouth_Asia <- EPISouth_AsiawNA[!is.na(EPISouth_AsiawNA)]
summary(EPISouth_Asia)

fivenum(EPISouth_Asia, na.rm=TRUE)
stem(EPISouth_Asia)  # stem and leaf plot
hist(EPISouth_Asia)
hist(EPISouth_Asia, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPISouth_Asia, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
rug(EPISouth_Asia)

plot(ecdf(EPISouth_Asia), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPISouth_Asia); qqline(EPISouth_Asia)
EPISouth_Asiax<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), EPISouth_Asiax, xlab= "Q-Q plot for t dsn")
qqline(EPISouth_Asiax)

# Europe
EPIEuropewNA <- EPI[!EPI_regions == "Europe"]
EPIEurope <- EPIEuropewNA[!is.na(EPIEuropewNA)]
summary(EPIEurope)

fivenum(EPIEurope, na.rm=TRUE)
stem(EPIEurope)  # stem and leaf plot
hist(EPIEurope)
hist(EPIEurope, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPIEurope, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
rug(EPIEurope)

plot(ecdf(EPIEurope), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPIEurope); qqline(EPIEurope)
EPIEuropex<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), EPIEuropex, xlab= "Q-Q plot for t dsn")
qqline(EPIEuropex)

# Latin_America_and_Caribbean
EPILatinAmericaCaribbeanwNA <- EPI[!EPI_regions == "Latin America and Caribbean"]
EPILatinAmericaCaribbean <- EPILatinAmericaCaribbeanwNA[!is.na(EPILatinAmericaCaribbeanwNA)]
summary(EPILatinAmericaCaribbean)

fivenum(EPILatinAmericaCaribbean, na.rm=TRUE)
stem(EPILatinAmericaCaribbean)  # stem and leaf plot
hist(EPILatinAmericaCaribbean)
hist(EPILatinAmericaCaribbean, seq(30., 95., 1.0), prob=TRUE)
lines(density(EPILatinAmericaCaribbean, na.rm=TRUE, bw=1.))  # or try bw=“SJ”
rug(EPILatinAmericaCaribbean)

plot(ecdf(EPILatinAmericaCaribbean), do.points=FALSE, verticals=TRUE) 
par(pty="s")
qqnorm(EPILatinAmericaCaribbean); qqline(EPILatinAmericaCaribbean)
EPILatinAmericaCaribbeanx<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), EPILatinAmericaCaribbeanx, xlab= "Q-Q plot for t dsn")
qqline(EPILatinAmericaCaribbeanx)

# Comparing distributions
# South_Asia
boxplot(EPISouth_Asia,EPIEurope) 
qqplot(EPISouth_Asia,EPIEurope)

boxplot(EPISouth_Asia,EPILatinAmericaCaribbean) 
qqplot(EPISouth_Asia,EPILatinAmericaCaribbean)

# Europe
boxplot(EPIEurope,EPILatinAmericaCaribbean) 
qqplot(EPIEurope,EPILatinAmericaCaribbean)