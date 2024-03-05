library(base)
library(gdata)
library(readxl)

#faster xls reader but requires perl!
#bronx1<-read.xls(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="<SOMEWHERE>/perl/bin/perl.exe") 
#bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
#library("xlsx", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
#bronx1<-read.xlsx("<SOMEWHERE>/rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
#View(bronx1)
#

#MY ALTERNATE
excel_file_path <- "C:/Users/fragaj3/Dropbox/Spring2024/CSCI_4600_01_Data_Analytics/Lab/Lab03/rollingsales_bronx.xls" # Replace with your file path

# Replace 'path_to_file.xlsx' with the path to your actual Excel file
bronx1 <- read_excel(excel_file_path)

# View the first few rows of the data
head(bronx1)
View(bronx1)

attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE_PRICE<-sub("\\$","",SALE_PRICE) 
SALE_PRICE<-as.numeric(gsub(",","", SALE_PRICE)) 
GROSS_SQUARE_FEET<-as.numeric(gsub(",","", GROSS_SQUARE_FEET)) 
LAND_SQUARE_FEET<-as.numeric(gsub(",","", LAND_SQUARE_FEET)) 
plot(log(GROSS_SQUARE_FEET), log(SALE_PRICE)) 
m1<-lm(log(SALE_PRICE)~log(GROSS_SQUARE_FEET))
summary(m1)
abline(m1,col="red",lwd=2)
plot(resid(m1))

# Model 2

m2<-lm(log(bronx1$SALE_PRICE)~log(bronx1$GROSS_SQUARE_FEET)+log(bronx1$LAND_SQUARE_FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE_PRICE)~0+log(bronx1$GROSS_SQUARE_FEET)+log(bronx1$LAND_SQUARE_FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1$SALE_PRICE)~0+log(bronx1$GROSS_SQUARE_FEET)+log(bronx1$LAND_SQUARE_FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING_CLASS_CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE_PRICE)~0+log(bronx1$GROSS_SQUARE_FEET)+log(bronx1$LAND_SQUARE_FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING_CLASS_CATEGORY))
summary(m4)
plot(resid(m4))
#