# Title     : TODO
# Objective : TODO
# Created by: Localadmin_yinyin
# Created on: 21.3.2018

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
install.packages('xlsx')
install.packages("sqldf")
install.packages('xlsx')
install.packages("sqldf")
mydata<- read.csv("C:\\Users\\yinyin\\Desktop\\herbpair\\step2\\feafru700.csv", header =T,sep=',')
uniquedata = uniquecombs(mydata)
uniquedata[is.na(uniquedata)] <- 0
uniquedata2=apply(uniquedata,2,as.numeric)
a=colSums(uniquedata2)
combin <- data.frame(rbind(t(data.frame(a)),data.frame(uniquedata2)))
uniquedata2=combin[,which(a!=0)][-1,]
rownames(uniquedata2)=uniquedata2[,1]
uniquedata2=uniquedata2[,-1]

#organ=uniquedata2$Liver[which(uniquedata2$Liver>=1),)



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
write.csv(s1,file=parse0(data),'.csv')
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
return(d1)
setwd("C:\\Users\\yinyin\\Desktop\\herbpair")
pBalancedAccuracy=ggplot(d1, aes(x=data,y=Balanced.Accuracy))+geom_boxplot(aes(fill=method))
pF1=ggplot(data=d1, aes(x=data,y=Balanced.Accuracy))+geom_boxplot(aes(fill=method))
dat.m <- melt(d1,id.vars=c('data','method'),measure.vars=c('Sensitivity','Specificity','F1','Balanced.Accuracy'))
four=ggplot(dat.m) +
  +     geom_boxplot(aes(x=data, y=value,color=variable,fill=method))

png(filename='BalancedAccuracy.png')
plot(pBalancedAccuracy)
dev.off()
png(filename='F1.png')
plot(pF1)
dev.off()
png(filename='allfour.png')
plot(four)
dev.off()
write.csv(d1,file=all.csv)
}

##1.2 basic method try

Liver=as.data.frame(uniquedata2$Liver)
#Liver[Liver>1]='1'
names(Liver)='Liver'


Lung=as.data.frame(uniquedata2$Lung)
#Lung[Lung>1]='1'
names(Lung)='Lung'
data1=select(uniquedata2, Lung:ExtFP1022)
data2=select(data1,-Liver)
method='rf'
seednumber=3333
meridian1(data2,Lung,seednumber,method)
meridian1(data2,Liver,seednumber,method)


##1.2 basic method try
lung=as.data.frame(uniquedata$Lung)
names(lung)='lung'
compoundfeature=select(uniquedata, compoundid6.0:compoundid43021.0)
names(property[,1])='property'
pras.data.frame(property)
names(property)
method='knn'
seednumber=1
meridian1(property,lung,seednumber,method)

##2.2 differente methods try
methodlist=c('knn','rf')
#meridian2(property,liver,seednumber,methodlist)

##3.2 differente organs try
Liver=as.data.frame(uniquedata$Liver)
names(Liver)='liver'
organlist=list(lung,Liver)
methodlist=c('knn')
meridian3(property,organlist,seednumber,methodlist)

##4.2 differente seeds try
seedlist=c(1:2)
#meridian4(property,organlist,seedlist,methodlist)

#5.2 differente data try
othermeridian=as.data.frame(select (uniquedata, Kidney: SmallIntestine))
names(othermeridian)=othermeridian
property=data.frame(select (uniquedata, Warm: Punkery))
massc=data.frame(select (uniquedata, MACCSFP3: MACCSFP165))
disese=data.frame(select (uniquedata, Warm:MACCSFP165))
drug=data.frame(select (uniquedata, X100070: X616471))
target=data.frame(select (uniquedata, DB00001: DB13170))

datalist=list(property,massc)

meridian5(datalist,organlist,seedlist,methodlist)

methodlist=c('knn','rf')
Warm=uniquedata $Warm
Sweet=uniquedata $Sweet
Bitter=uniquedata $Bitter
Cold=uniquedata $Cold
Salty=uniquedata$Salty
Sour=uniquedata $Sour
Cool=uniquedata $Cool
Hot=uniquedata $Hot
Mild=uniquedata $Mild
Neutral=uniquedata $Neutral
Pungent=uniquedata $Pungent
Punkery=uniquedata $Punkery

organlist=list(Warm,Sweet,Bitter,Cold,Sour,Salty,Cool,Hot,Mild,Neutral,Pungent,Punkery)
organlist=list(Sour)
methodlist=c('knn')
datalist=list(othermeridian,massc)
seedlist=c(25)
meridian5(datalist,organlist,seedlist,methodlist)


data=data.frame(select (uniquedata, MACCSFP3: MACCSFP165))
organ=Liver=as.data.frame(uniquedata$Liver)
set.seed(3333)
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
knn_fit <- train (training, trainingclass, method = 'knn',trControl=trctrl, preProcess = c("center", "scale"),tuneLength = 10)
knn_fit
knnPredict <- predict(knn_fit, newdata = testing)
a=confusionMatrix(knnPredict, testingclass)
a
tocsv <- data.frame(cbind(t(a$overall),t(a$byClass)))
k=c(seednumber,deparse(substitute(data)),method,names(organ))
names(k)=c('seed','data','method','organ')
tocsv1=data.frame(cbind(t(k),tocsv))
return(tocsv1)

#mydata<- read.csv("C:\\Users\\yinyin\\Desktop\\herbpair\\2. propertyarrangement\\compound finger\\runitself\\used for finger generation\\addcompound-cut0.csv", header =T,sep=',')
uniquedata = uniquecombs(mydata)
uniquedata=as.data.frame(uniquedata)
uniquedata = uniquecombs(mydata)
a=colSums(uniquedata)
combin <- data.frame(rbind(t(data.frame(a)),data.frame(uniquedata)))
uniquedata=combin[,which(a!=0)][-1,]

