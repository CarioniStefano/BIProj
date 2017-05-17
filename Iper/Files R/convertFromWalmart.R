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


# CREAZIONE MATRICE PER VALORI DI VENDITA CON RIGA CHE IDENTIFICA SETTIMANA E ANNO
storeOrderedByWeek <- as.data.frame( selectedStoreDeptAggregated$ANNONO)
storeOrderedByWeek <- cbind(storeOrderedByWeek,selectedStoreDeptAggregated$SETTIMANANO)
storeOrderedByWeek <- cbind(storeOrderedByWeek,selectedStoreDeptAggregated$REPARTO)
storeOrderedByWeek <- cbind(storeOrderedByWeek,NA)
storeOrderedByWeek <- cbind(storeOrderedByWeek,NA)
storeOrderedByWeek <- cbind(storeOrderedByWeek,NA)
storeOrderedByWeek <- cbind(storeOrderedByWeek,NA)
storeOrderedByWeek <- cbind(storeOrderedByWeek,NA)
storeOrderedByWeek <- cbind(storeOrderedByWeek,NA)
storeOrderedByWeek <- cbind(storeOrderedByWeek,NA)
storeOrderedByWeek <- cbind(storeOrderedByWeek,NA)
storeOrderedByWeek <- cbind(storeOrderedByWeek,NA)
storeOrderedByWeek<- unique(storeOrderedByWeek)

colnames(storeOrderedByWeek) <- c("ANNONO","SETTIMANANO","REPARTO","QUANTITAVENDUTATOTALE","VALOREVENDUTOTALE","VALOREVENDUTOPROMO")
storeOrderedByWeek$ANNONO <- as.numeric(levels(storeOrderedByWeek$ANNONO))[as.integer(storeOrderedByWeek$ANNONO)] 
storeOrderedByWeek$REPARTO <- as.numeric(as.character(storeOrderedByWeek$REPARTO) )
storeOrderedByWeek$SETTIMANANO <- as.numeric(levels(storeOrderedByWeek$SETTIMANANO))[as.integer(storeOrderedByWeek$SETTIMANANO)] 

for (currentYear in unique(storeOrderedByWeek$ANNONO)){
  # print(currentYear)
  for(currentWeek in unique(storeOrderedByWeek$SETTIMANANO)){
    # print(currentWeek)
    for(currentDept in unique(storeOrderedByWeek$REPARTO)){
      
      if( nrow(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                  as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                  &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ] ) >0  )
      {
        storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                    as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == currentWeek
                                  &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ),c(4,5,6) ]  <-(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                                                                                                                              as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                                                                                                                            &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ]$`VALORE VENDUTO TOTALE`   )
        storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                    as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == currentWeek
                                  &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ),c(7,8,9) ]  <-(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                                                                                                                              as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                                                                                                                            &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ]$`VALORE VENDUTO PROMO`   )
        storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                    as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == currentWeek
                                  &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ),c(10,11,12) ]  <-(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                                                                                                                              as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                                                                                                                            &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ]$`QUANTITA' VENDUTA TOTALE`   )
        
        
        
        }
      
      
      
      
    }
  }
}

colnames(storeOrderedByWeek) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT1","VALORETOT2","VALORETOT3","VALOREPROMO1","VALOREPROMO2","VALOREPROMO3","QUANTITA1","QUANTITA2","QUANTITA3")

# TODO CONTROLLARE QUI SOPRA LE SETTIMANE MANCANTI DALLA 19 DEL 2017





# I VALORI DELLA SETTIMANA 53 SONO MOLTO DIVERSI DALLA 52 E 1, MA NEL COMPLESSO DI AVVICINA MOLTO DI PIù ALLA SETTIMANA
# QUINDI MEDIO LA SETTIMANA 53 DEL 2015 CON LA SETTIMANA 52 DELLO STESSO ANNO
storeOrderedByWeek[which( (as.numeric(as.character(storeOrderedByWeek$SETTIMANANO))) %in% c(53)) + c(-1:1), ]

selectedStoreDeptAggregated[which( (as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO))) %in% c(1) &  (as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)))  %in% c(2016) ), ]

# 928   09      03    201552             408109.31  X           24921.79     X           110066.65          52   2015
# 929   29      03    201552             237481.55             25309.98                 57879.66          52   2015
# 930   31      03    201552             421929.12             48977.22                109114.97          52   2015
# 
# 937   09      03    201553             427013.87             19011.85   X             126375.38          53   2015
# 938   29      03    201553             234532.32             15754.08                 61682.59          53   2015
# 939   31      03    201553             357890.05  X           26552.50                110177.30          53   2015
# 
# 946   09      03    201601             390052.86             55529.89                121440.55          01   2016
# 947   29      03    201601             230175.07             22916.00                 67709.39          01   2016
# 948   31      03    201601             348199.93  X         39477.75      X          111683.29          01   2016
# #########################################

# 931   09      04    201552             327981.34             10502.55  X                51288.00          52   2015
# 932   29      04    201552             140378.84             15980.92                 23519.60          52   2015
# 933   31      04    201552             163585.39  X            5076.75                 29409.00          52   2015
# 
# 940   09      04    201553             234673.83             15279.23                 47645.00          53   2015
# 941   29      04    201553              95353.60              3413.12                 19624.00          53   2015
# 942   31      04    201553             104300.63  X            2012.96    X             22716.00          53   2015
# 
# 949   09      04    201601             255481.61             59630.15                 50586.00          01   2016
# 950   29      04    201601              97470.48             12141.66                 19667.00          01   2016
# 951   31      04    201601             106573.71     X        10917.61     X            23529.00          01   2016
# #########################################

# 934   09      06    201552             134257.51              8772.26   X              12196.00          52   2015
# 935   29      06    201552              45304.48              4179.21                  4670.00          52   2015
# 936   31      06    201552              57713.02  X            9928.26                  5657.00          52   2015
# 
# 943   09      06    201553             118483.11              6651.14                 11759.00          53   2015
# 944   29      06    201553              43260.15              9855.58                  4921.00          53   2015
# 945   31      06    201553              43707.58  X           12958.15                  5419.00          53   2015
# 
# 952   09      06    201601             152008.15             24533.77                 13700.00          01   2016
# 953   29      06    201601              60804.13             19846.70                  6570.00          01   2016
# 954   31      06    201601              57951.89  X           20262.91   X               6205.00          01   2016

# #########################################

# BASANDOSI SU QUESTO METODO ALTAMENTE SCIENTIFICO POSSIAMO DIRE CHE ASSOMIGLIA DI PIù ALLA SETTIMANA 1 CHE ALLA 52, QUINDI ACCORPIAMO LA 53 CON LA 1 



# TODO MEDIARE LA SETTIMANA 53 CON LA 1 DELL'ANNO DOPO E SOSTITUIRE IL VALORE ALLA SETTIMANA 1
