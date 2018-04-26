# Title     : TODO
# Objective : TODO
# Created by: Localadmin_yinyin
# Created on: 20.4.2018

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
uniquedata2= uniquecombs(mydata)
uniquedata2[is.na(uniquedata2)] <- 0
a=colSums(uniquedata2)
combin <- data.frame(rbind(t(data.frame(a)),data.frame(uniquedata2)))
uniquedata2=combin[,which(a!=0)][-1,]
rownames(uniquedata2)=uniquedata2[,1]
uniquedata2=uniquedata2[,-1]

ADMET=select(uniquedata2,Heavyatoms:SyntheticAccessibility)
Pubchem=select(uniquedata2,PubchemFP0:PubchemFP868)
MACCS=select(uniquedata2,MACCSFP8:MACCSFP165)
Sub=select(uniquedata2,SubFP1:SubFP307)
Ext=select(uniquedata2,ExtFP1:ExtFP1021)
FourFinger=select(uniquedata2,ExtFP1:MACCSFP165)
ADMETFinger=select(uniquedata2,Heavyatoms:MACCSFP165)

Liver=as.data.frame(uniquedata2$Liver)
names(Liver)='Liver'
Spleen=as.data.frame(uniquedata2$Spleen)
names(Spleen)='Spleen'
Lung=as.data.frame(uniquedata2$Lung)
names(Lung)='Lung'
Heart=as.data.frame(uniquedata2$Heart)
names(Heart)='Heart'
Kidney=as.data.frame(uniquedata2$Kidney)
names(Kidney)='Kidney'
LargeIntestine=as.data.frame(uniquedata2$LargeIntestine)
names(LargeIntestine)='LargeIntestine'
Stomach=as.data.frame(uniquedata2$Stomach)
names(Stomach)='Stomach'

organlist=list(Lung,Spleen,Stomach,Heart,Kidney,LargeIntestine,Liver)
#organlist=list(Lung,Spleen)
datalist=list(ADMET,Pubchem,MACCS,Sub,Ext,FourFinger,ADMETFinger)
#datalist=list(ADMET,MACCS)
methodlist=c('knn','rf')
seedlist=c(1)

meridian1=function(data,organ,seednumber,method)
{
  set.seed(seednumber)
  intrain <- createDataPartition(y = organ[,1], p= 0.7, list = FALSE)
  training <- data[intrain,]
  testing <- data[-intrain,]
  databin=cbind(organ,data)
  trainingclass=databin[intrain,][,1]
  trainingclass= factor(trainingclass)
  testingclass=databin[-intrain,][,1]
  testingclass= factor(testingclass)

  # KNN for Liver
  trctrl <- trainControl(method = "cv", number = 5)
  knn_fit <- train (training, trainingclass, method = method,trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10)
  #trctrl <- trainControl(method = "cv", number = 2)
  #knn_fit <- train (training, trainingclass, method = method,trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 1)
  knn_fit
  knnPredict <- predict(knn_fit, newdata = testing)
  a=confusionMatrix(knnPredict,testingclass,positive='1')
  tocsv <- data.frame(cbind(t(a$overall),t(a$byClass)))
  k=c(seednumber,deparse(substitute(data)),method,names(organ))
  names(k)=c('seed','data','method','organ')
  tocsv1=data.frame(cbind(t(k),tocsv))
  return(tocsv1)
}



###2.1 differente methods
meridian2=function(data,organ,seednumber,methodlist)
{
  m1=data.frame()
  for(methods in methodlist)
  {
    m=meridian1(data,organ,seednumber,methods)
    m1= data.frame(rbind(m1, m))
  }
  return(m1)
}


##3.1 differente organs

meridian3=function(data,organlist,seednumber,methodlist)
{
  r1=data.frame()
  for(organs in organlist)
  {
    r=meridian2(data,organs,seednumber,methodlist)
    r1= data.frame(rbind(r1, r))
  }
  return(r1)
}


##4.1 differente seeds
meridian4=function(data,organlist,seedlist,methodlist)
{
  s1=data.frame()
  for(seeds in seedlist)
  {
    s=meridian3(data,organlist,seeds,methodlist)
    s1= data.frame(rbind(s1, s))
  }
  return(s1)
  #write.csv(s1,file=parse0(data),'.csv')
}


##5.1 differente data
meridian5=function(datalist,organlist,seedlist,methodlist)
{
  d1=data.frame()
  for(datas in datalist)
  {
    d=meridian4(datas,organlist,seedlist,methodlist)
    d1= data.frame(rbind(d1, d))
  }
  write.csv(d1,file='C:\\Users\\yinyin\\Desktop\\herbpair\\seed1com.csv')
  return(d1)
  #pBalancedAccuracy=ggplot(d1, aes(x=data,y=Balanced.Accuracy))+geom_boxplot(aes(fill=method))
  #pF1=ggplot(data=d1, aes(x=data,y=Balanced.Accuracy))+geom_boxplot(aes(fill=method))
  #dat.m <- melt(d1,id.vars=c('data','method'),measure.vars=c('Sensitivity','Specificity','F1','Balanced.Accuracy'))
  #four=ggplot(dat.m) +
  #+     geom_boxplot(aes(x=data, y=value,color=variable,fill=method))

  #png(filename='BalancedAccuracy.png')
  #plot(pBalancedAccuracy)
  #dev.off()
  #png(filename='F1.png')
  #plot(pF1)
  #dev.off()
  #png(filename='allfour.png')
  #plot(four)
  #dev.off()
}

meridian5(datalist,organlist,seedlist,methodlist)