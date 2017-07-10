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




options(scipen = 999)

if(!exists("Estrazione2")){
  
  sheets <- readxl::excel_sheets("~/Downloads/Estrazione senza famiglia.xlsx")
  Estrazione2 <- list()
  for(sheet in sheets){
    Estrazione2 <- rbind(Estrazione2, readxl::read_excel("~/Downloads/Estrazione senza famiglia.xlsx",col_types = c("text", "text", "text", "text",  "text", "text", "text", "text", "text",
                                                                                                  "numeric", "text", "text", "numeric",   "numeric", "numeric", "numeric",
                                                                                                  "numeric"), sheet = sheet))
  }
  
  View(Estrazione2)
  
}




#order( table(Estrazione[,1]))
# Dati per store
# 31 47622
# 09 25879
# 29 25788

# DATI PER REPARTO
# 4 23859
# 6 19754
# 3 15528

storesNumbers <- c(1 , 4 , 6 , 7 , 8 , 9)
storesNumbersSecond <- c(1 , 4 , 6 , 7 , 8 , 9)
# storesNumbers <- c(31,9,29)
deptNumbers <- c(3,4,6)
deptNumbersSecond <- c(3,4,6)


storesBinded = Estrazione2[which(as.numeric(Estrazione2$ENTE) == head(storesNumbers,1)), ]

# storesBinded.z <- storesBinded.z[, c(3, 5, 2, 1,6,4)]




storesNumbers <- tail(storesNumbers,-1)

for(storeNo in storesNumbers){
  
  # print(storeNo)
  storesBinded = rbind(storesBinded , Estrazione2[which(as.numeric(Estrazione2$ENTE) == storeNo), ])
  
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


# CREAZIONE MATRICE PER VALORI DI VENDITA CON RIGA CHE IDENTIFICA SETTIMANA E ANNO
from <- as.Date("2013-12-30")
to <- as.Date("2017-05-01")
months <- seq.Date(from=from,to=to,by="week")

values <- rep.int(0,length(months))

Zooserie <- zoo(values, months)

asaasas <- Zooserie


storeOrderedByWeek2 <- as.data.frame( selectedStoreDeptAggregated$ANNONO)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,selectedStoreDeptAggregated$SETTIMANANO)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,selectedStoreDeptAggregated$REPARTO)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2 <- cbind(storeOrderedByWeek2,NA)
storeOrderedByWeek2<- unique(storeOrderedByWeek2)

colnames(storeOrderedByWeek2) <- c("ANNONO","SETTIMANANO","REPARTO","QUANTITAVENDUTATOTALE","VALOREVENDUTOTALE","VALOREVENDUTOPROMO")
storeOrderedByWeek2$ANNONO <- as.numeric(levels(storeOrderedByWeek2$ANNONO))[as.integer(storeOrderedByWeek2$ANNONO)] 
storeOrderedByWeek2$REPARTO <- as.numeric(as.character(storeOrderedByWeek2$REPARTO) )
storeOrderedByWeek2$SETTIMANANO <- as.numeric(levels(storeOrderedByWeek2$SETTIMANANO))[as.integer(storeOrderedByWeek2$SETTIMANANO)] 

for (currentYear in unique(storeOrderedByWeek2$ANNONO)){
  # print(currentYear)
  for(currentWeek in unique(storeOrderedByWeek2$SETTIMANANO)){
    # print(currentWeek)
    for(currentDept in unique(storeOrderedByWeek2$REPARTO)){
      
      if( nrow(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                  as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                  &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ] ) >0  )
      {
        storeOrderedByWeek2[which (as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                    as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == currentWeek
                                  &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == currentYear ),c(4,5,6,7,8,9) ]  <-(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                                                                                                                                    as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                                                                                                                                  &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ]$`VALORE VENDUTO TOTALE`   )
        storeOrderedByWeek2[which (as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                    as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == currentWeek
                                  &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == currentYear ),c(10,11,12,13,14,15) ]  <-(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                                                                                                                                          as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                                                                                                                                        &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ]$`VALORE VENDUTO PROMO`   )
        storeOrderedByWeek2[which (as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                    as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == currentWeek
                                  &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == currentYear ),c(16,17,18,19,20,21) ]  <-(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                                                                                                                                          as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                                                                                                                                        &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ]$`QUANTITA' VENDUTA TOTALE`   )
        
        
        
      }
      
      
      
      
    }
  }
}

colnames(storeOrderedByWeek2) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT1","VALORETOT2","VALORETOT3","VALORETOT4","VALORETOT5","VALORETOT6","VALOREPROMO1","VALOREPROMO2","VALOREPROMO3",
                                  "VALOREPROMO4","VALOREPROMO5","VALOREPROMO6","QUANTITA1","QUANTITA2","QUANTITA3","QUANTITA4","QUANTITA5","QUANTITA6")





# ##########################################################RIMOZIONE SETTIMANA 53

# I VALORI DELLA SETTIMANA 53 SONO MOLTO DIVERSI DALLA 52 E 1, MA NEL COMPLESSO DI AVVICINA MOLTO DI PI? ALLA SETTIMANA
# QUINDI MEDIO LA SETTIMANA 53 DEL 2015 CON LA SETTIMANA 52 DELLO STESSO ANNO


# TODO MEDIA SETTIMANA 52 - 53 E 1-53, VALORE MINIMO UNISCO

for (currentYear in unique(storeOrderedByWeek2$ANNONO)){
  # print(currentYear)
  for(currentWeek in unique(storeOrderedByWeek2$SETTIMANANO)){
    # print(currentWeek)
    for(currentDept in unique(storeOrderedByWeek2$REPARTO)){
      
      if( nrow(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                  as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                  &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ] ) >0  )
      {
        
        if(currentWeek == 53){
          print("week53")
          
          
          weekBefore <- 52
          weekAfter <- 1
          yearAfter <- currentYear+1
          valueCurrentweek <- storeOrderedByWeek2[which (as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                                          as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == currentWeek
                                                        &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == currentYear ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ] 
          valueWeekBefore <- storeOrderedByWeek2[which (as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                                         as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == weekBefore
                                                       &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == currentYear ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ]
          valueWeekAfter <- storeOrderedByWeek2[which (as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                                        as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == weekAfter
                                                      &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == yearAfter ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ]
          meanBefore <- rowMeans(valueWeekBefore - valueCurrentweek)
          meanAfter <- rowMeans(valueWeekAfter - valueCurrentweek)
          
          if(meanBefore < meanAfter){
            print("inside")
            print(currentWeek)
            
            valueWeek <- rbind(valueCurrentweek,valueWeekBefore)
            
            print(colMeans(valueWeek))
            storeOrderedByWeek2[which (as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                        as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == weekBefore
                                      &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == currentYear ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ]<- colMeans(valueWeek)
            
            storeOrderedByWeek2 <- storeOrderedByWeek2[ (which (!(as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                                                  as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == currentWeek
                                                                &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == currentYear ))), ] 
            
          }else{
            print("inside2")
            
            print(currentWeek)
            
            valueWeek <- rbind(valueCurrentweek,valueWeekAfter)
            
            print(colMeans(valueWeek))
            storeOrderedByWeek2[which (as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                        as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == weekAfter
                                      &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == yearAfter ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ]<- colMeans(valueWeek)
            storeOrderedByWeek2 <- storeOrderedByWeek2[ (which (!(as.numeric(as.character(storeOrderedByWeek2$REPARTO)) == currentDept &
                                                                  as.numeric(as.character(storeOrderedByWeek2$SETTIMANANO)) == currentWeek
                                                                &  as.numeric(as.character(storeOrderedByWeek2$ANNONO)) == currentYear ))), ] 
          }
          
        }
        
      }
      
      
      
      
    }
  }
}
