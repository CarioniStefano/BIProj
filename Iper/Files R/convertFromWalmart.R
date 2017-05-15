library(readr)
library(readxl)
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
library(skmeans)
library(plyr)
library(fpc)




options(scipen = 999)

if(!exists("Estrazione")){
  
  sheets <- readxl::excel_sheets("~/Lavoro/Iper/Estrazione/Estrazione.xls")
  Estrazione <- list()
  for(sheet in sheets){
    Estrazione <- rbind(Estrazione, readxl::read_excel("~/Lavoro/Iper/Estrazione/Estrazione.xls",col_types = c("text", "text", "text",  "text", "text", "text", "text", "text",
                                                                                                               "numeric", "text", "text", "numeric",   "numeric", "numeric", "numeric",
                                                                                                               "numeric"), sheet = sheet))
  }
  
  View(Estrazione)
  
}




# table(Estrazione[,1])
# Dati per store
# 31 47622
# 09 25879
# 29 25788

# DATI PER REPARTO
# 4 23859
# 6 19754
# 3 15528

storesNumbers <- c(31,09,29)
deptNumbers <- c(3,4,6)


storesBinded = Estrazione[which(as.numeric(Estrazione$ENTE) == head(storesNumbers,1)), ]

# storesBinded.z <- storesBinded.z[, c(3, 5, 2, 1,6,4)]




storesNumbers <- tail(storesNumbers,-1)

for(storeNo in storesNumbers){
  
  # print(storeNo)
  storesBinded = rbind(storesBinded , Estrazione[which(as.numeric(Estrazione$ENTE) == storeNo), ])
  
}



selectedStoreDept = storesBinded[which(as.numeric(storesBinded$REPARTO) == head(deptNumbers,1)), ]

deptNumbers <- tail(deptNumbers,-1)

for(deptNo in deptNumbers){
  
  # print(storeNo)
  selectedStoreDept <- rbind(selectedStoreDept,storesBinded[which(as.numeric( storesBinded$REPARTO ) == deptNo),])
  
}

yearDivided <- list()

yearList <- c(2014,2015,2016,2017)

for(year in yearList){
  
  
  
  yearDivided[[length(yearDivided)+1]] <-  (selectedStoreDept[grepl(paste(year,"+",sep=""), selectedStoreDept$SETTIMANA, ignore.case = T),] )
  
  
}



selectedStoreDeptNoDuplicate<- selectedStoreDept[!(duplicated(selectedStoreDept[c("ENTE","REPARTO","SETTIMANA","VALORE VENDUTO TOTALE")]) | duplicated(selectedStoreDept[c("ENTE","REPARTO","SETTIMANA","VALORE VENDUTO TOTALE")], fromLast = FALSE)), ]

selectedStoreDeptAggregated <- aggregate( cbind(  `VALORE VENDUTO TOTALE`, `VALORE VENDUTO PROMO`,`QUANTITA' VENDUTA TOTALE` ) ~ ENTE + REPARTO+ SETTIMANA , data = selectedStoreDeptNoDuplicate , FUN = sum )
# TODO
# aggiungere colonna promo
# TUTTE LE RIGHE HANNO ALMENO UNA PROMO
# RIGA IDENTIFICATA DA ENTE,REPARTO,SETTIMANA




selectedStoreDeptAggregated$SETTIMANA <- as.character(selectedStoreDeptAggregated$SETTIMANA)

selectedStoreDeptAggregated$SETTIMANANO <- substr(selectedStoreDeptAggregated[,3],5,6)
selectedStoreDeptAggregated$ANNONO <- substr(selectedStoreDeptAggregated[,3],1,4)




