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
library(fitdistrplus)
library(logspline)
library(mclust)
library(scatterplot3d)
library(rgl)
library(evd)
library(car)
library(fpc)
library(cluster)
library(ggplot2)
library(gridExtra)
library(scales)
library(reshape2)
library(scales)
library(xts)


options(scipen = 999)


# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# TODO LIST
#correlazione per reparto con vendite/holiday
#correlazione per store con sommatoria vendite / holiday + tipo store + dimensione store + temperatura
#livello store, livello reparto per singolo store, livello reparto per tutti store, livello tutti i reparti di singolo store

#aggiungere classificazione store per tipo
#multicollinearità (selezione variabili modello)

# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# 
# trainFile.z <-
#   read.zoo(
#     file = "~/Lavoro/InputFiles/train.csv",
#     header = TRUE,
#     index.column = 3,
#     sep = ","
#   )
stores <- read_csv("~/Lavoro/InputFiles/stores.csv")
stores$Type[stores$Type=="A"]  <- 100
stores$Type[stores$Type=="B"]  <- 200
stores$Type[stores$Type=="C"]  <- 300

notZoo <- read.csv( file = "~/Lavoro/InputFiles/train.csv", header = TRUE, sep = ",")

notZoo <- merge.data.frame(notZoo,stores,all=TRUE)



notZoo <- xts(notZoo , order.by=make.time.unique(as.POSIXct(notZoo[,3],tz="UTC") ))


# notZoo <- cbind(as.data.frame(notZoo),stores$Type[which(stores$Store==notZoo$Store)])

# dataNumber <- table(trainFile.z$Store)



# 13 10474
# 10 10315
# 1 10244
# 2 10238
# 4 10272
# 24 10228


# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
storesNumbers <- c(1,2,4,10,13,24)
storesNumbersSecond <- c(1,2,4,10,13,24)
deptNumbers <- c(21,81,91,40,7,92)
# deptNumbers <- c(21)

deptDivisionList <- list()
par(mfrow = c(3, 2))
for(deptSelected in deptNumbers){
  
  print(deptSelected)
  # deptDivisionList <- list(deptDivisionList,list(newObj))
  
  
  
  storesNumbers <- c(1,2,4,10,13,24)
  storesBinded.z = notZoo[which(as.numeric(notZoo$Store) == head(storesNumbers,1)), ]
  
  storesBinded.z <- storesBinded.z[, c(3, 5, 2, 1,6,4)]
  
  
  
  
  storesNumbers=tail(storesNumbers,-1)
  
  for(storeNo in storesNumbers){
    
    print(storeNo)
    storesBinded.z = cbind(storesBinded.z , notZoo[which(as.numeric(notZoo$Store) == storeNo), c(1,6,4)])
    
  }
  
  storesBinded.z = storesBinded.z[which(as.numeric(storesBinded.z$Dept) == deptSelected), ]
}

from <- as.Date("2010-01-01")
to <- as.Date("2012-10-26")
months <- seq.Date(from=from,to=to,by="week")

values <- rep.int(0,length(months))

z1 <- zoo(values, months)

dio <- xts(z1 , order.by=make.time.unique(as.POSIXct(index(z1)), tz="UTC" ) )



asds <- merge(dio,storesBinded.z)
