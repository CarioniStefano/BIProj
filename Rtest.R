library(tseries)					#Time series analysis and computational finance.
library(quantmod)					#assist the quantitative trader in the development, testing, and deployment of statistically based trading models.
library(zoo)						#library necessarie per l'utilizzo di oggetti zoo (tipicamente data | valore | valore)
library(PerformanceAnalytics)		#Collection of econometric functions for performance and risk analysis.
library(caret)						#set of functions that attempt to streamline the process for creating predictive models
library(kernlab)					#Kernel-based machine learning methods for classification, regression, clustering, novelty detection, quantile regression and dimensionality reduction
library(nnet)						#Software for feed-forward neural networks with a single hidden layer, and for multinomial log-linear models
library(Metrics)					#Metrics is a set of evaluation metrics that is commonly used in supervised machine learning.




getSymbols("ADS.F")
ADS.F.z = as.zoo(to.monthly(ADS.F))
ADS.F.z$ADS.F.Open =  NULL
ADS.F.z$ADS.F.Close =  NULL
ADS.F.z$ADS.F.High =  NULL
ADS.F.z$ADS.F.Low =  NULL
ADS.F.z$ADS.F.Volume =  NULL


getSymbols("NKE")
NKE.z = as.zoo(to.monthly(NKE))
NKE.z$NKE.Open =  NULL
NKE.z$NKE.Close =  NULL
NKE.z$NKE.High =  NULL
NKE.z$NKE.Low =  NULL
NKE.z$NKE.Volume =  NULL



getSymbols("NESN.VX")
NESN.VX.z = as.zoo(to.monthly(NESN.VX))
NESN.VX.z$NESN.VX.Open =  NULL
NESN.VX.z$NESN.VX.Close =  NULL
NESN.VX.z$NESN.VX.High =  NULL
NESN.VX.z$NESN.VX.Low =  NULL
NESN.VX.z$NESN.VX.Volume =  NULL

getSymbols("DNN.MI")
DNN.MI.z = as.zoo(to.monthly(DNN.MI))
DNN.MI.z$DNN.MI.Open =  NULL
DNN.MI.z$DNN.MI.Close =  NULL
DNN.MI.z$DNN.MI.High =  NULL
DNN.MI.z$DNN.MI.Low =  NULL
DNN.MI.z$DNN.MI.Volume =  NULL

getSymbols("MS.MI")
MS.MI.z = as.zoo(to.monthly(MS.MI))
MS.MI.z$MS.MI.Open =  NULL
MS.MI.z$MS.MI.Close =  NULL
MS.MI.z$MS.MI.High =  NULL
MS.MI.z$MS.MI.Low =  NULL
MS.MI.z$MS.MI.Volume =  NULL

par(mfrow=c(1,1))


stocks.z = merge(ADS.F.z,NKE.z,NESN.VX.z,all=FALSE)
stocks.z = merge(stocks.z,DNN.MI.z,MS.MI.z,all=FALSE)
#taglio per split nestle