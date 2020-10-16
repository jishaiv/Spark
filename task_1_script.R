
getwd()
setwd("D://internships//Spark")
options(scipen=999) ## Switches off the scientific notations in terms of no. representation: 



library(dplyr)
library(ggplot2)
library(data.table)

##Importing dataset

myData<-fread('https://raw.githubusercontent.com/AdiPersonalWorks/Random/master/student_scores%20-%20student_scores.csv')
head(myData) ## checking the data 

## Doing some basic sanity checks of data 

names(myData) ##only two columns
summary(myData) ## No missing values

##Plotting the distribution of scores :

ggplot(data=myData,aes(y=Scores,x=Hours))+geom_point()+labs(title="Hours Vs Percentage")

## graph shows a clear linear relationship between the two variables:

##Spliting data set into test(20%) and train (80%)

set.seed(200)
index<-sample(nrow(myData),0.80*nrow(myData),replace=F)
train<-myData[index, ]
test<-myData[-index, ]

head(train)
head(test)

## Model building 

model1<-lm(Scores~Hours,data=train)
summary(model1)

#Adjusted R-squred=0.9509

##checking linear regression assumptions:

## Testing on test data :

predicted_test<-predict(model1,test) #Predict Scores

actuals_predicted<-data.frame(actual=test$Scores,predicted=predicted_test)
View(actuals_predicted)

##Predicting for 9.25hrs/day sleeping time

predicted_ques<-predict(model1,data.frame(Hours=9.25))

Ans : 94.13 

