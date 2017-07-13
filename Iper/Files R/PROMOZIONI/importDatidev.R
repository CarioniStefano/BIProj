library(readr)
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
library(ISOweek)

# Estrazione_con_famiglia <- read_delim("~/Downloads/Estrazione con famiglia.csv", 
#                                       "|", escape_double = FALSE, trim_ws = TRUE)

if(!exists("Estrazione_con_famiglia")){
  Estrazione_con_famiglia <- read_delim("~/Downloads/Estrazione con famiglia.csv", 
                                        "|", escape_double = FALSE, trim_ws = TRUE, 
                                        col_types = list(col_number() , col_character() ,col_number() , col_character() ,col_number() , col_character() ,
                                                         col_number() , col_character() ,col_character(), col_character() , col_character(), col_character(),
                                                         col_character(), col_number(),col_character(), col_character(), col_character(),
                                                         col_number(), col_number(), col_number(), col_number()))
  
  
  
  
  selectedStorePromoNoDuplicate<- Estrazione_con_famiglia[!(duplicated(Estrazione_con_famiglia[c("ENTE","SETTORE","REPARTO","SETTIMANA","GRUPPO","FAMIGLIA","QUANTITA' VENDUTA TOTALE","QUANTITA' VENDUTA PROMO")])), ]
  View(Estrazione_con_famiglia)
  Estrazione_con_famiglia$SETTIMANANO <- substr(Estrazione_con_famiglia$SETTIMANA,5,6)
  Estrazione_con_famiglia$ANNONO <- substr(Estrazione_con_famiglia$SETTIMANA,1,4)
  
  selectedStorePromoNoDuplicate <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE %in% c(1,2,3,4,5) ) ,]
  selectedStorePromoNoDuplicate$SETTIMANANO <- substr(selectedStorePromoNoDuplicate$SETTIMANA,5,6)
  selectedStorePromoNoDuplicate$ANNONO <- substr(selectedStorePromoNoDuplicate$SETTIMANA,1,4)
  selectedStorePromoNoDuplicate <- selectedStorePromoNoDuplicate[,-c(2,3,4,6,8,11,15,17)]
}





storeOrderedByWeekPromo <- as.data.frame( selectedStorePromoNoDuplicate$ANNONO)
storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$SETTIMANANO)
storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$REPARTO)
storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$SETTORE)
storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$GRUPPO)
storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$FAMIGLIA)


storeOrderedByWeekPromo<- unique(storeOrderedByWeekPromo)
colnames(storeOrderedByWeekPromo) <- c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA")
storeOrderedByWeekPromo <- storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$ANNONO == 2014),]
for(a in 1:27){
  storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,NA)
}





for (currentYear in unique(storeOrderedByWeekPromo$ANNONO)){
  print(currentYear)
  for(currentWeek in unique(storeOrderedByWeekPromo$SETTIMANANO)){
    print(currentWeek)
    for(currentReparto in unique(storeOrderedByWeekPromo$REPARTO)){
      print(currentReparto)
      for(currentSettore in unique(storeOrderedByWeekPromo$SETTORE)){
        print(currentSettore)
        for(currentGruppo in unique(storeOrderedByWeekPromo$GRUPPO)){
          print(currentGruppo)
          for(currentFamiglia in unique(storeOrderedByWeekPromo$FAMIGLIA)){
            print(currentFamiglia)
            if( nrow(Estrazione_con_famiglia[which (as.numeric(as.character(Estrazione_con_famiglia$REPARTO)) == currentReparto &
                                                    as.numeric(as.character(Estrazione_con_famiglia$SETTIMANANO)) == currentWeek
                                                    &  as.numeric(as.character(Estrazione_con_famiglia$ANNONO)) == currentYear
                                                    &  as.numeric(as.character(Estrazione_con_famiglia$SETTORE)) == currentSettore
                                                    &  as.numeric(as.character(Estrazione_con_famiglia$GRUPPO)) == currentGruppo
                                                    &  as.character(Estrazione_con_famiglia$FAMIGLIA) == currentFamiglia), ] ) >0  )
            {
              
              storeOrderedByWeekPromo[which (as.numeric(as.character(storeOrderedByWeekPromo$REPARTO)) == currentReparto &
                                               as.numeric(as.character(storeOrderedByWeekPromo$SETTIMANANO)) == currentWeek
                                             &  as.numeric(as.character(storeOrderedByWeekPromo$ANNONO)) == currentYear
                                             &  as.numeric(as.character(storeOrderedByWeekPromo$SETTORE)) == currentSettore
                                             &  as.numeric(as.character(storeOrderedByWeekPromo$GRUPPO)) == currentGruppo
                                             &  as.character(storeOrderedByWeekPromo$FAMIGLIA) == currentFamiglia),c(7:33) ] <- Estrazione_con_famiglia[which (as.numeric(as.character(Estrazione_con_famiglia$REPARTO)) == currentReparto &
                                                                                                                                                                 as.numeric(as.character(Estrazione_con_famiglia$SETTIMANANO)) == currentWeek
                                                                                                                                                               &  as.numeric(as.character(Estrazione_con_famiglia$ANNONO)) == currentYear 
                                                                                                                                                               &  as.numeric(as.character(Estrazione_con_famiglia$SETTORE)) == currentSettore
                                                                                                                                                               &  as.numeric(as.character(Estrazione_con_famiglia$GRUPPO)) == currentGruppo
                                                                                                                                                               &  as.character(Estrazione_con_famiglia$FAMIGLIA) == currentFamiglia), ]$'QUANTITA\' VENDUTA PROMO'   
            }
            
          }
        }
      }
    }
  }
  
}

# plot(selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE==4 & selectedStorePromoNoDuplicate$SETTORE == 10 & selectedStorePromoNoDuplicate$REPARTO == 1 &
selectedStorePromoNoDuplicate$GRUPPO == 2 & selectedStorePromoNoDuplicate$FAMIGLIA == '0_'),]$'QUANTITA\' VENDUTA PROMO' , type="b" , xlab="asd",ylab="asd2")


