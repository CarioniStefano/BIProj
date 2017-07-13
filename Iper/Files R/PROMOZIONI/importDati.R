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

tabelladipartenza<- Estrazione_con_famiglia[,c(5,7,14,16)]
tabelladipartenza <- unique(tabelladipartenza)
tabelladate<- Estrazione_con_famiglia[,c(22,23)]
tabelladate <- unique(tabelladate)

storeOrderedByWeekPromo <- merge(x = tabelladate, y = tabelladipartenza, by = NULL)



# 
# storeOrderedByWeekPromo <- as.data.frame( selectedStorePromoNoDuplicate$ANNONO)
# storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$SETTIMANANO)
# storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$REPARTO)
# storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$SETTORE)
# storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$GRUPPO)
# storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,selectedStorePromoNoDuplicate$FAMIGLIA)
# storeOrderedByWeekPromo<- unique(storeOrderedByWeekPromo)
# colnames(storeOrderedByWeekPromo) <- c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA")
# storeOrderedByWeekPromo <- cbind(storeOrderedByWeekPromo,2)

# storeOrderedByWeekPromo <-merge(x = banana, y = storeOrderedByWeekPromo, by=c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA"), all.x = TRUE)

selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 1),16] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 1),]$'QUANTITA\' VENDUTA PROMO'
selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 2),17] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 2),]$'QUANTITA\' VENDUTA PROMO'
selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 3),18] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 3),]$'QUANTITA\' VENDUTA PROMO'
selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),19] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),]$'QUANTITA\' VENDUTA PROMO'
selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 5),20] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 5),]$'QUANTITA\' VENDUTA PROMO'

store1 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 1),]
store2 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 2),]
store3 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 3),]
store4 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),]
store5 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 5),]

table(store1$FAMIGLIA)
table(store2$FAMIGLIA)


storeOrderedByWeekPromo <-merge(x = storeOrderedByWeekPromo, y = store1[,c(2,3,8,9,13,14,15)], by=c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA"), all.x = TRUE)
colnames(storeOrderedByWeekPromo) <- c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA","ENTE1")
storeOrderedByWeekPromo <-merge(x = storeOrderedByWeekPromo, y = store2[,c(2,3,8,9,13,14,15)], by=c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA"), all.x = TRUE)
colnames(storeOrderedByWeekPromo) <- c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA","ENTE1","ENTE2")
storeOrderedByWeekPromo <-merge(x = storeOrderedByWeekPromo, y = store3[,c(2,3,8,9,13,14,15)], by=c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA"), all.x = TRUE)
colnames(storeOrderedByWeekPromo) <- c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA","ENTE1","ENTE2","ENTE3")
storeOrderedByWeekPromo <-merge(x = storeOrderedByWeekPromo, y = store4[,c(2,3,8,9,13,14,15)], by=c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA"), all.x = TRUE)
colnames(storeOrderedByWeekPromo) <- c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA","ENTE1","ENTE2","ENTE3","ENTE4")
storeOrderedByWeekPromo <-merge(x = storeOrderedByWeekPromo, y = store5[,c(2,3,8,9,13,14,15)], by=c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA"), all.x = TRUE)
colnames(storeOrderedByWeekPromo) <- c("ANNONO","SETTIMANANO","REPARTO","SETTORE","GRUPPO","FAMIGLIA","ENTE1","ENTE2","ENTE3","ENTE4","ENTE5")

storeOrderedByWeekPromo[is.na(storeOrderedByWeekPromo)] <- 0





# ##########################################################RIMOZIONE SETTIMANA 53



# I VALORI DELLA SETTIMANA 53 SONO MOLTO DIVERSI DALLA 52 E 1, MA NEL COMPLESSO DI AVVICINA MOLTO DI PI? ALLA SETTIMANA
# QUINDI MEDIO LA SETTIMANA 53 DEL 2015 CON LA SETTIMANA 52 DELLO STESSO ANNO


# TODO MEDIA SETTIMANA 52 - 53 E 1-53, VALORE MINIMO UNISCO

annoFastidio <- 2015
settimanaFastidio <- 53
weekBefore <- 52
weekAfter <- 1
yearAfter <- 2016

for(currentDept in unique(storeOrderedByWeekPromo$REPARTO)){
  
  for(currentSettore in unique(storeOrderedByWeekPromo$SETTORE)){
    
    for(currentGruppo in unique(storeOrderedByWeekPromo$GRUPPO)){
      
      for(currentFamiglia in unique(storeOrderedByWeekPromo$FAMIGLIA)){
        
        valueCurrentweek <- storeOrderedByWeekPromo[which( as.numeric(as.character(storeOrderedByWeekPromo$ANNONO)) == annoFastidio &
                                                             as.numeric(as.character(storeOrderedByWeekPromo$SETTIMANANO)) == settimanaFastidio &
                                                             as.numeric(as.character( storeOrderedByWeekPromo$REPARTO)) == currentDept &
                                                             as.numeric(as.character( storeOrderedByWeekPromo$SETTORE)) == currentSettore &
                                                             as.numeric(as.character(storeOrderedByWeekPromo$GRUPPO)) == currentGruppo & 
                                                             as.character(storeOrderedByWeekPromo$FAMIGLIA) == currentFamiglia),c(7:11)] 
        
        valueWeekBefore <- storeOrderedByWeekPromo[which(as.numeric(as.character(storeOrderedByWeekPromo$ANNONO)) == annoFastidio &
                                                           as.numeric(as.character(storeOrderedByWeekPromo$SETTIMANANO)) == weekBefore &
                                                           as.numeric(as.character(storeOrderedByWeekPromo$REPARTO)) == currentDept &
                                                           as.numeric(as.character( storeOrderedByWeekPromo$SETTORE)) == currentSettore &
                                                           as.numeric(as.character(storeOrderedByWeekPromo$GRUPPO)) == currentGruppo & 
                                                           as.character(storeOrderedByWeekPromo$FAMIGLIA) == currentFamiglia),c(7:11)] 
        
        valueWeekAfter <- storeOrderedByWeekPromo[which(as.numeric(as.character(storeOrderedByWeekPromo$ANNONO)) == yearAfter &
                                                          as.numeric(as.character(storeOrderedByWeekPromo$SETTIMANANO)) == weekAfter &
                                                          as.numeric(as.character(storeOrderedByWeekPromo$REPARTO)) == currentDept &
                                                          as.numeric(as.character( storeOrderedByWeekPromo$SETTORE)) == currentSettore &
                                                          as.numeric(as.character(storeOrderedByWeekPromo$GRUPPO)) == currentGruppo & 
                                                          as.character(storeOrderedByWeekPromo$FAMIGLIA) == currentFamiglia),c(7:11)] 
        
        meanBefore <- rowMeans(abs(valueWeekBefore - valueCurrentweek))
        meanAfter <- rowMeans(abs(valueWeekAfter - valueCurrentweek))
        
        print("banana")
        
        if(!(length(meanAfter) == 0)){
          
          if(meanBefore < meanAfter){
            print("inside")
            # print(currentWeek)
            
            valueWeek <- rbind(valueCurrentweek,valueWeekBefore)
            
            # print(colMeans(valueWeek))
            storeOrderedByWeekPromo[which(as.numeric(as.character(storeOrderedByWeekPromo$ANNONO)) == annoFastidio &
                                            as.numeric(as.character(storeOrderedByWeekPromo$SETTIMANANO)) == weekBefore &
                                            as.numeric(as.character(storeOrderedByWeekPromo$REPARTO)) == currentDept &
                                            as.numeric(as.character( storeOrderedByWeekPromo$SETTORE)) == currentSettore &
                                            as.numeric(as.character(storeOrderedByWeekPromo$GRUPPO)) == currentGruppo & 
                                            as.character(storeOrderedByWeekPromo$FAMIGLIA) == currentFamiglia),c(7:11)]<- colMeans(valueWeek)
            
            storeOrderedByWeekPromo <- storeOrderedByWeekPromo[which(!( as.numeric(as.character(storeOrderedByWeekPromo$ANNONO)) == annoFastidio &
                                                                          as.numeric(as.character(storeOrderedByWeekPromo$SETTIMANANO)) == settimanaFastidio &
                                                                          as.numeric(as.character( storeOrderedByWeekPromo$REPARTO)) == currentDept &
                                                                          as.numeric(as.character( storeOrderedByWeekPromo$SETTORE)) == currentSettore &
                                                                          as.numeric(as.character(storeOrderedByWeekPromo$GRUPPO)) == currentGruppo & 
                                                                          as.character(storeOrderedByWeekPromo$FAMIGLIA) == currentFamiglia)),] 
            
          }else{
            print("inside2")
            
            # print(currentWeek)
            
            valueWeek <- rbind(valueCurrentweek,valueWeekAfter)
            
            # print(colMeans(valueWeek))
            storeOrderedByWeekPromo[which(as.numeric(as.character(storeOrderedByWeekPromo$ANNONO)) == yearAfter &
                                            as.numeric(as.character(storeOrderedByWeekPromo$SETTIMANANO)) == weekAfter &
                                            as.numeric(as.character(storeOrderedByWeekPromo$REPARTO)) == currentDept &
                                            as.numeric(as.character( storeOrderedByWeekPromo$SETTORE)) == currentSettore &
                                            as.numeric(as.character(storeOrderedByWeekPromo$GRUPPO)) == currentGruppo & 
                                            as.character(storeOrderedByWeekPromo$FAMIGLIA) == currentFamiglia),c(7:11)]<- colMeans(valueWeek)
            
            storeOrderedByWeekPromo <- storeOrderedByWeekPromo[which(!( as.numeric(as.character(storeOrderedByWeekPromo$ANNONO)) == annoFastidio &
                                                                          as.numeric(as.character(storeOrderedByWeekPromo$SETTIMANANO)) == settimanaFastidio &
                                                                          as.numeric(as.character( storeOrderedByWeekPromo$REPARTO)) == currentDept &
                                                                          as.numeric(as.character( storeOrderedByWeekPromo$SETTORE)) == currentSettore &
                                                                          as.numeric(as.character(storeOrderedByWeekPromo$GRUPPO)) == currentGruppo & 
                                                                          as.character(storeOrderedByWeekPromo$FAMIGLIA) == currentFamiglia)),] 
          }
        }
      }
    }
  }
}


asdasd <- storeOrderedByWeekPromo


appoggio1 <- storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$SETTORE==10 & storeOrderedByWeekPromo$REPARTO==1 & 
                                             storeOrderedByWeekPromo$GRUPPO == 2 & (storeOrderedByWeekPromo$FAMIGLIA =="0_"   ) ),]
appoggio2 <- storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$SETTORE==10 & storeOrderedByWeekPromo$REPARTO==1 & 
                                             storeOrderedByWeekPromo$GRUPPO == 2 & ( storeOrderedByWeekPromo$FAMIGLIA == "21"  ) ),]

pairs(appoggio1[7:11],appoggio2[7:11])

plot(appoggio2$ENTE1,type="l",col="blue", ylim=c(0,5000))
lines(appoggio1$ENTE1,type="l",col="red")
lines(appoggio2$ENTE2,type="l",col="green")
lines(appoggio1$ENTE2,type="l",col="purple")
lines(appoggio1$ENTE3,type="l",col="green")
lines(appoggio1$ENTE4,type="l",col="black")
lines(appoggio1$ENTE5,type="l",col="purple")


