install.packages('class')
library(class)
library(dplyr)
###DATA CLEANING ###
#Selecting numeric columns
wdata1<-wsdata[,c(7,8,9,10,11,12,13,14,15,17,18,19,21,22)]
#Create training and testing sets
set.seed(123)
wsplit <- sample(1:nrow(wdata1),size=nrow(wdata1)*0.7,replace = FALSE) #random selection of 70% data.
train.wind<-wdata1[wsplit,]
test.wind<-wdata1[-wsplit,]
#Begin making Knn model
nrow(train.wind)
#we have 129117 observations, so we can do knn for 359 and 360
knn.359<-knn(train=train.wind,test=test.wind,cl=train.wind$BoPbin,k=359)
knn.360<-knn(train=train.wind,test=test.wind,cl=train.wind$BoPbin,k=360)
##Evaluate accuracy
ACC.359 <- 100 * sum(test.wind$BoPbin == knn.359)/NROW(test.wind$BoPbin)
ACC.360 <- 100 * sum(test.wind$BoPbin == knn.360)/NROW(test.wind$BoPbin)
##Create confusion matrix
table(knn.359,test.wind$BoPbin)
table(knn.360,test.wind$BoPbin)
###Daily Model###
daily<-read.csv("dailyData.csv")
#Take out non-numeric columns
dailybop<-daily[,-c(1,2,3)]
#Make training and testing sets
set.seed(35345)
dsplit <- sample(1:nrow(dailybop),size=nrow(dailybop)*0.7,replace = FALSE) #random selection of 70% data.
train.daily<-dailybop[dsplit,]
test.daily<-dailybop[-dsplit,]
#Begin making Knn model
nrow(train.daily)
#we have 931 observations, so we can do knn for 30 and 25
knn.1<-knn(train=train.daily,test=test.daily,cl=train.daily$BoPerror,k=1)
knn.10<-knn(train=train.daily,test=test.daily,cl=train.daily$BoPerror,k=10)
##Evaluate accuracy
ACC.1 <- 100 * sum(test.daily$BoPerror == knn.1)/NROW(test.daily$BoPerror)
ACC.10 <- 100 * sum(test.daily$BoPerror == knn.10)/NROW(test.daily$BoPerror)
#Both have 95.5 % accuracy
##Create confusion matrix
table(knn.30,test.daily$BoPerror)
table(knn.25,test.daily$BoPerror)
