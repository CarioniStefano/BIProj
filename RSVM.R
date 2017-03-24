library(readr)
library(tseries)					#Time series analysis and computational finance.
library(quantmod)					#assist the quantitative trader in the development, testing, and deployment of statistically based trading models.
library(zoo)						#library necessarie per l'utilizzo di oggetti zoo (tipicamente data | valore | valore)
library(PerformanceAnalytics)		#Collection of econometric functions for performance and risk analysis.
library(caret)						#set of functions that attempt to streamline the process for creating predictive models
library(kernlab)					#Kernel-based machine learning methods for classification, regression, clustering, novelty detection, quantile regression and dimensionality reduction
library(nnet)						#Software for feed-forward neural networks with a single hidden layer, and for multinomial log-linear models
library(Metrics)
library(e1071)



inputFile.z <- read.zoo(file = "~/Downloads/train.csv", header = TRUE, index.column=3, sep = ",")

View(inputFile.z)
#inputFile.z[i,1]

store1.z = inputFile.z[which(inputFile.z$Store == 1),]
store1.z = store1.z[which(store1.z$Dept == 29),]
par(mfrow=c(1,1))


storeCut.z = head(store1.z,-3)
storeCut.z$Dept=NULL
storeCut.z$Store=NULL
storeCut.z$IsHoliday=NULL

#forecasting

#creazione dell'input del modello (12 mesi)


Input=merge(lag(storeCut.z,1),lag(storeCut.z,2),lag(storeCut.z,3),all=FALSE)

Data=merge(Input,storeCut.z,all=FALSE)
Data=na.omit(Data)

colnames(Data)=c("lag1","lag2","lag3","TARGET")
#fine creazione

#inizio creazione del training set
trainIndex=1:(nrow(Data)*0.75)
training=as.data.frame(Data[trainIndex])
rownames(training)=NULL
#fine creazione del training set

#inizio creazione del test set
test=as.data.frame(Data[-trainIndex])
rownames(test)=NULL
#fine creazione del test set

bootControl=trainControl(number=100)    #definisce il comportamento di apprendimento (k-folds cross validation?)
preProc=c("center","scale")                #imposta i parametri per il preprocessing
set.seed(2)                                #setta uno scenario casuale
indexTrn=ncol(training)                    # ???

#creazione del modello di apprendimento
svmFit=train(training[,-indexTrn],training[,indexTrn],method="svmRadial",tuneLength=25, trnControl=bootControl)

#svmFit=train(training[,-indexTrn],training[,indexTrn],method="svmLinear",tuneLength=100, trnControl=bootControl)

svmBest=svmFit$finalModel    #modello migliore trovato con i parametri forniti
predsvm=predict(svmBest, test[,-ncol(test)])
actualTS=test[,ncol(test)]
predictedTS=predsvm

mse(actual=actualTS, predicted=predictedTS)
mae(actual=actualTS, predicted=predictedTS )