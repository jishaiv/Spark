setwd("D:\\internships\\Spark")
getwd()

#### Task #2 #####

##From the given ‘Iris’ dataset, predict the optimum number of clusters and represent it visually ####

### Algorithm using-> K-Means Clustering

### Loading required libraries 

library(dplyr)  ## contains functions of K Means cluster analysis
library(stats)
library(ggplot2)
library(ggfortify) ## for cluster plots


### Getting and Inspecting Iris dataset

myData<-read.csv("Iris.csv")
View(myData)

## Since K Means is Unsupervised Learning, converting to unlabelled data

newdata<-myData[ ,c(2,3,4,5)] ## Selecting all except Species and Id
View(newdata)


### WSS plot function to choose the optimum no of clusters.


wssplot <- function(data, nc=15, seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")
  wss
}

wssplot(newdata)

###Spoting the kink in the curve inorder to coose the optimum no of clusters.

## here, as the kink is at 3, k=3

# K- Means Cluster Analysis ##

KMmodel<-kmeans(newdata,3)

## Evaluating cluster Analysis ##

## Cluster plot

autoplot(KMmodel,newdata,frame=TRUE) ## 3 clusters are distinct, no overlaping

##Analysing cluster centres

KMmodel$centers

## Centres of both the clusters ave different value for all variables.

## Hence the clusters are distinct

save.image("D:/internships/Spark/task_2.RData")

#### Task #2 Completed ######






