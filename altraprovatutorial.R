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


#inputFile.z[i,1]

store1.z = inputFile.z[which(inputFile.z$Store == 1),]
store1.z = store1.z[which(store1.z$Dept == 29),]
par(mfrow=c(1,1))


storeCut.z = head(store1.z,-3)
storeCut.z$Dept=NULL
storeCut.z$Store=NULL
storeCut.z$IsHoliday=NULL


## Prepare Scatter Plot with Predicted Points
data=storeCut.z
#Scatter Plot
plot(data, pch=16)

#Predict Y using Linear Model
predY <- predict(model, data)

#Overlay Predictions on Scatter Plot
points(data$X, predY, col = "blue", pch=16)





## Calulate Root Mean Square Error (RMSE) for Linear Model

#Install Package
install.packages("hydroGOF")

#Load Library
library(hydroGOF)

#Calculate RMSE 
RMSE=rmse(predY,data$Y)





## Fit SVR Model and Prepare Scatter Plot

#Install Package
install.packges("e1071")

#Load Library
library(e1071)

#Scatter Plot
plot(data)

#Regression with SVM
modelsvm=svm(Y~X,data)

#Predict using SVM regression
predYsvm <- predict(modelsvm, data)

##Overlay SVM Predictions on Scatter Plot
points(data$X, predYsvm, col = "red", pch=16)





## Calculate parameters of the SVR Model
#Find value of W
W=t(modelsvm$coefs) %*% modelsvm$SV

#Find value of b
b=modelsvm$rho  




## RMSE for SVR Model

#Calculate RMSE 
RMSEsvm=rmse(predYsvm,data$Y)





##  Optimising SVR Model and Selecting Best Model

#Tune the above SVM model
OptModelsvm=tune(svm, Y~X, data=data,ranges=list(elsilon=seq(0,1,0.1), cost=1:100))

#Print optimum value of parameters
print(OptModelsvm)

#Plot the perfrormance of SVM Regression model
plot(OptModelsvm)

#Find out the best model
BstModel=OptModelsvm$best.model

#Predict Y using best model
PredYBst=predict(BstModel,data)

#Calculate RMSE of the best model 
RMSEBst=rmse(PredYBst,data$Y)





## Calculate parameters of the Best SVR Model
#Find value of W
W=t(BstModel$coefs) %*% BstModel$SV

#Find value of b
b=BstModel$rho  



## Plotting SVR Model and Tuned Model in same plot

plot(data, pch=16)
points(data$X, predYsvm, col = "blue", pch=3)
points(data$X, PredYBst, col = "red", pch=4)
points(data$X, predYsvm, col = "blue", pch=3, type="l")
points(data$X, PredYBst, col = "red", pch=4, type="l")







