# Title     : TODO
# Objective : TODO
# Created by: Localadmin_yinyin
# Created on: 19.3.2018

#install.packages('xlsx')
#install.packages("sqldf")
#install.packages('xlsx')
#install.packages("sqldf")
library(car)
library(tidyverse)
library(mgcv)
library('e1071')
library(caret)
library(nycflights13)
library(ggplot2)
library(pls)
library(reshape2)
library(ggplot2)
library(lattice)

mydata<- read.csv("C:\\Users\\yinyin\\Desktop\\herbpair\\step2\\feafru700.csv", header =T,sep=',')
uniquedata = uniquecombs(mydata)
uniquedata[is.na(uniquedata)] <- 0
uniquedata2=apply(uniquedata,2,as.numeric)
a=colSums(uniquedata2)
combin <- data.frame(rbind(t(data.frame(a)),data.frame(uniquedata2)))
uniquedata2=combin[,which(a!=0)][-1,]
rownames(uniquedata2)=uniquedata2[,1]
uniquedata2=uniquedata2[,-1]

##1.1 basic method

meridian1=function(data,organ,seednumber,method)
{
  set.seed(seednumber)
  intrain <- createDataPartition(y = organ[,1], p= 0.7, list = FALSE)
  training <- data[intrain,]
  testing <- data[-intrain,]
  databin=cbind(organ,data)
  trainingclass=databin[intrain,][,1]
  trainingclass= factor(trainingclass)
  testclass=databin[-intrain,][,1]
  testingclass= factor(testclass)

  # KNN for Liver
  trctrl <- trainControl(method = "cv", number = 10)
  knn_fit <- train (training, trainingclass, method = method,trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10)
  knn_fit
  knnPredict <- predict(knn_fit, newdata = testing)
  a=confusionMatrix(knnPredict, testingclass,positive='1')
  tocsv <- data.frame(cbind(t(a$overall),t(a$byClass)))
  k=c(seednumber,names(as.data.frame(data[,1])),method,names(organ))
  names(k)=c('seed','data','method','organ')
  tocsv1=data.frame(cbind(t(k),tocsv))
  return(tocsv1)
}

Liver=as.data.frame(uniquedata2$Liver)
#Liver[Liver>1]='1'
names(Liver)='Liver'


Lung=as.data.frame(uniquedata2$Lung)
#Lung[Lung>1]='1'
names(Lung)='Lung'
data1=select(uniquedata2, Lung:ExtFP1022)
lungdata=select(data1,-Lung)
method='rf'
seednumber=3333
meridian1(lungdata,Lung,seednumber,method)
#meridian1(data2,Liver,seednumber,method)


