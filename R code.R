######################################################
####            R-PROJECT                         ####
####                                              ####
####  DataSet :                                   ####
####  Used Car from Ebay-Kleinanzeigen            ####
####  Objective:                                  ####
####  Predict the estimated price of the car      ####
####                                              ####
######################################################

# Loading Dataset to  [Ref: T.B-chapter2]

opar <- par(no.readonly=TRUE)
getwd()
mydata <- read.csv("autos.csv",header = T,na.strings = c(""))

# PART 1) Reviewing the data set

View(mydata)

# Dataset is in German - Sorry!
# It contains 20 variables with nearly 190k observations from year 2016
# Variables include - 
# 1) dateCrawled : when this ad was first crawled, all field-values are taken from this date
# 2) name : "name" of the car
# 3) seller : private or dealer
# 4) offerType
# 5) price : the price on the ad to sell the car
# 6) abtest
# 7) vehicleType
# 8) yearOfRegistration : at which year the car was first registered
# 9) gearbox
# 10) powerPS : power of the car in PS
# 11) model
# 12) kilometer : how many kilometers the car has driven
# 13) monthOfRegistration : at which month the car was first registered
# 14) fuelType
# 15) brand
# 16) notRepairedDamage : if the car has a damage which is not repaired yet
# 17) dateCreated : the date for which the ad at ebay was created
# 18) nrOfPictures : number of pictures in the ad (unfortunately this field contains everywhere a 0 and is thus useless (bug in crawler!) )
# 19) postalCode
# 20) lastSeenOnline : when the crawler saw this ad last online

summary(mydata)
str(mydata)

# Converting to proper data types [Ref: T.B-chapter4 - 4.7]

data1 <- mydata
data1$dateCrawled <- as.Date(data1$dateCrawled,"%Y-%m-%d")
data1$lastSeen <- as.Date(data1$lastSeen,"%Y-%m-%d")
data1$name <- as.character(data1$name)

str(data1)


# PART-2 Cleaning the data set

summary(data1)

# Reviewing Seeler and offertype variable
table(data1$seller,data1$offerType)

# [Ref: T.B-chapter4 - 4.10]
# Removing 'gewerblich'or'commercial-(2 obs) and 'Gesuch'or petition-(8 obs) from Seller and OfferType resp.

data2 <- data1[which(data1$seller=="privat" & data1$offerType=="Angebot"),]  #'Angebot' means offer

# Trimming the Year of registration (1970-2016), as the dataset was created in 2016

table(data2$yearOfRegistration)
data2 <- data2[which(data2$yearOfRegistration > 1969 & data2$yearOfRegistration < 2017),]

# Removing 'nrOfPictures' as all the values are zero

data2 <- data2[-c(18)]

# Removing 'abtest' and 'Postal Code' varaible
data2 <- data2[-c(6,18)]

# Removing 'Seller' and 'OfferType' as both variables have only one value each
data2 <- data2[-c(3,4)]

summary(data2)

# Dealing with NA  [Ref: T.B-chapter4 - 4.5]

library(dplyr)
library(dlookr)
library(VIM)

diagnose(data2)
aggr(data2, prop=FALSE, numbers=TRUE)

# Best method to eliminate the NA could be search the model and fill out the missing details
# like vehicleType, gearbox and fuelType.

# Calculate the total number of rows with NA

row.has.na <- apply(data2, 1, function(x){any(is.na(x))})
sum(row.has.na)

# As the dataset is very huge, I have decided to omit the NA's.

# Omiting all rows with NA 

data3 <- na.omit(data2)                     # DATA-3
diagnose(data3)

# Verifying any major change in statistics in all variables

# Checking brand wise proportion after eliminating rows
par(mfrow=c(2,1))
plot(prop.table(table(data2$brand)),main= "With Na's")
plot(prop.table(table(data3$brand)),main="Without NA's")

# Brand proportion doesnt change much 

# Checking other characteristics
summary(data2)
summary(data3)

# Most of the variables doesnt get much affected except 
# a bit decrease in the mean and median of the powerPS variable. 
# So,We can proceed with the new data



# The primary purpose of the project is to study how brand affects the price considering varous other factors.
# The size of the dataset remains of 133k observation.
# To reduce the size of the data set, we will omit 5 or 6 highest frequency brands from the dataset.

par(mfrow=c(1,1))
barplot(summary(data3$brand),main="Number of cars per brand",col=21)
data3 <- subset(data3, brand %in% names(table(data3$brand))[table(data3$brand)<=10000])  # Removes 6 brands having more than 10000 cars
data3$brand <- droplevels(data3$brand)

# New dataset contains 49k observations
# Lets check the number of cars in the remaining brands.

barplot(table(data3$brand),main="Number of cars per brand",col=21)

# Eliminating other non-used variables - 

# As the ad was crawled the same day when the ad was uploaded,
# 'dateCrawled value is same as dateCreated'

# Removing 'dateCreated' variable

data3 <- data3[-c(14)]

str(data3)


# Spotting Outliers-Price


boxplot(data3$price)
plot(data3$price)       # Shows us there are outliers present in the Price variable


# Considering Price less than 200k
data4 = data3[data3$price<200000,]            # DATA-4
boxplot(data4$price)

# We can still omit the outliers
# Futher considering price less than 200000k

# Checking average price per brand
ag.brand <- aggregate(data4[,"price"],by=list(brand=data4$brand),FUN = mean)
plot(ag.brand)

# Highest average price is for Porsche which is 42k
# So, further considering price less than 50k

data4 <- data3[data3$price<50000,]
boxplot(data4$price)

data4 <- data4[data4$price>100,]
boxplot(data4$price)

# Spotting Outlier - PowerPS

summary(data4$powerPS)
boxplot(data4$powerPS)
data4 <- data4[data4$powerPS<3000,]

boxplot(data4$powerPS)
# Having a power of 3000PS is not soo uncommon with the cars, 
# So although we can spot some utliers , we decide not to eliminate it.



# PART-3 Analysing the data

# [Ref: T.B-chapter4]
# Adding new variable 'Age.years'- Difference between dateCrawled and yearOfRegistration

data4$age.years <- as.numeric(format(data4$dateCrawled,'%Y'))-(data4$yearOfRegistration)-1

# Adding new variable 'Driven' containing 3 factors-
# 'Less'- kms less than 50k
# 'Moderate'- kms between 50k and 100k
# 'More'- kms above 100k

table(data4$kilometer)
data4$Driven <- ifelse(data4$kilometer<50001,"Less",ifelse(data4$kilometer<90000,"Moderate","More"))
table(data4$Driven)

# Adding a variable km.per.year which is kilometer/age.years

data4$km.per.year <- ifelse(data4$age.years>0,data4$kilometer/data4$age.years,data4$kilometer)
class(data4$km.per.year)
summary(data4$km.per.year)

# Covariance and correlation    [Ref: T.B-chapter11 - 11.3]

library(corrgram)
corrgram(data4, lower.panel=panel.shade,
         upper.panel=panel.cor, text.panel=panel.txt,
         main="Corrgram ")

# Price shows more correlation with - yearOfRegisteration, powerPS, Kilometer and also other factors which are not shown in the graph.


# As our target variable is price, lets see how price is affecte by different variables.

# [Ref: T.B-chapter6: Basic Graphs; chapter5: Aggregate the data]
# Checking average price per brand

ag.brand <- aggregate(data4[,"price"],by=list(brand=data4$brand),FUN = mean)
plot(ag.brand)

# Shows us that brand value of Porsche is very high


# Checking average price per driven category

ag.driven <- aggregate(data4[,"price"],by=list(Driven=data4$Driven),FUN = mean)
ag.driven

# Shows us that less driven vehicles on an average has 1.5 times the price of Moderate driven vehicles
# and 3 times as that of more driven vehicles.


# Checking average price per vehicle type

ag.type <- aggregate(data4[,"price"],by=list(Type=data4$vehicleType),FUN = mean)
plot(ag.type)

# The result kind of divides the vehicle types into 3 groups:
# 1) Above 10k - SUV
# 2) Between 6k and 10k - Cabrio and Coupe
# 3) Below 6k - Bus,Kleinwagen,Kombi,limousine and andere

# Visualization

install.packages("ggfortify")
library(ggfortify)
library(ggplot2)
attach(data4)
par(mfrow=c(2,2))

# [Ref: T.B-chapter6]
plot(age.years,price, main="Price variation with age of car", col="red")
pie(table(fuelType),main="Number of cars per fuel Type")
plot(vehicleType,main="Number of cars per Type",col=11)
smoothScatter(price,pch=1,col="blue",main="Price variation")  # [Ref: T.B-chapter11 - 11.1]


# Checking vehicleType per brand present- 

par(mfrow=c(1,1))
counts <- table(fuelType,brand)
barplot(counts, main="Car Distribution by brand",
        xlab="Brands", col = rainbow(7))
        legend("topright",
               legend = rownames(counts),
               fill=rainbow(7),ncol=2)
       
     
# Checking price density with Kilometers driven  [Ref: T.B-chapter19: ggplot2]
        
g <- ggplot(data4, aes(price))
g + geom_density(aes(fill=factor(Driven)), alpha=0.8) + 
  labs(title="Density plot", 
       subtitle="Price variation with kilometers driven",
       x="Price",
       fill="Driven factors")


# PART 4- Modelling

# Finding proper regression model 

pr <- lm(price~kilometer+age.years+gearbox+fuelType+brand+notRepairedDamage+vehicleType,data=data4)
summary(pr)   
# p-value << 0.05
# R-squared = 0.65

par(mfrow=c(2,2))
plot(pr)
# The plot shows that the model is noot accurate enough and cann be improved

# We also notice that there are many factors having P-value more than 0.05 which arent affecting the price much
# So now we will group those factors and label it "others".

# Grouping out non-significant brands, fuelType and vehicleType
# Brands :- 
# Chevrolet
# chrysler
# citroen
# lancia
# smart
# mazda
# mitsubishi
# nissan
# subaru


# fuelType:-
# cng
# elektro

# vehicleType:-
# bus
# limousine

veh <- c("chevrolet","chrysler","citroen","lancia","smart","mazda","mitsubishi","nissan","subaru")

data4$vehicle <- ifelse(data4$brand %in% names(table(veh)),"others",as.character(data4$brand))
data4$vehicle <- as.factor(data4$vehicle)

ft <- c("cng","elektro")
data4$ftype <- ifelse(data4$fuelType %in% names(table(ft)),"others",as.character(data4$fuelType))
data4$ftype <- as.factor(data4$ftype)

vt <- c("bus","limousine","kombi","cabrio")
data4$vtype <- ifelse(data4$vehicleType %in% names(table(vt)),"others",as.character(data4$vehicleType))
data4$vtye <- as.factor(data4$vtype)

# Remodeling variables in regression

pr1 <- lm(log(price)~kilometer+age.years+notRepairedDamage+vehicle+powerPS+ftype+vtype,data=data4)
summary(pr1)
# p-value << 0.5
# R-squared = 0.712
# We have successfuly removed all the factors with p-value > 0.05





# Comaring 2 linear model with Anova chi-square method to see any change in residual sum of square value.

anova(pr,pr1,test = "Chisq")    # [Ref: T.B-chapter8 - 8.6]

# This shows an error as out target variable in both the models are different,
# linear model(pr) has price as the target variable, whereas
# linear model(pr1) has log(price) as the target variable

# But we have seen better graph and statistics from the linear model(pr1), so we will move forward with the same model.

# Predicting the values

library(caret)
library(pROC)
library(dataQualityR)
library(dplyr)
library(gbm)
install.packages("randomForest")
library(randomForest)


set.seed(1234)  # setting seed to reproduce results of random sampling
split<-(.70)
index <- createDataPartition(data4$price, p=split, list=FALSE)  # row indices for training data

train.df <- data4[index, ]  # model training data
test.df  <- data4[-index, ]   # test data

# Applying lm model to the train data

#train.pr1= lm(price~km+age.years+notRepairedDamage+vehicle+powerPS,data=train.df)
train.pr1 <- lm(log(price)~kilometer+age.years+notRepairedDamage+vehicle+ftype+powerPS+vtype,data=train.df)
summary(train.pr1)
par(mfrow=c(2,2))

plot(train.pr1)

# Predicting the price in the test model-

pred.price <- predict(train.pr1,test.df)
class(pred.price)

# Calculating the accuracy - 

compare <- data.frame(cbind(actual=log(test.df$price),predicted=pred.price))
accuracy <- cor(compare)
accuracy

# Our model shows 84.6% of accuracy.


# K-fold cross verification - 
# Checking if random samples affects the priction accuracy
install.packages("DAAG")
library(DAAG)

par(mfrow=c(1,1))
cvResults <- suppressWarnings(CVlm(data=data4,form.lm=pr1, m=5, dots=FALSE, seed=29, legend.pos="topleft",  printit=FALSE, main="Small symbols are predicted values while bigger ones are actuals."))  # performs the CV


# This test shows us that there is no statistical difference when selecting random models from the dataset
# So, our model is accurate to predict the estimated price of the car.
# Purpose:
# 1) User can refer to the esttimated price before updating their price for the vehicle sale.
# 2) The ebay platform can compare the estimated price and the actual price to provide better recommendation.