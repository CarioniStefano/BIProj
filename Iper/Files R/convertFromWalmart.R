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

if(!exists("Estrazione")){
  
  sheets <- readxl::excel_sheets("~/Downloads/Estrazione.xls")
  Estrazione <- list()
  for(sheet in sheets){
    Estrazione <- rbind(Estrazione, readxl::read_excel("~/Downloads/Estrazione.xls",col_types = c("text", "text", "text", "text",  "text", "text", "text", "text", "text",
                                                                                                               "numeric", "text", "text", "numeric",   "numeric", "numeric", "numeric",
                                                                                                               "numeric"), sheet = sheet))
  }
  
  #View(Estrazione)
  
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
from <- as.Date("2013-12-30")
to <- as.Date("2017-05-01")
months <- seq.Date(from=from,to=to,by="week")

values <- rep.int(0,length(months))

Zooserie <- zoo(values, months)

asaasas <- Zooserie


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
                                  &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ),c(4,5,6,7,8,9) ]  <-(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                                                                                                                                    as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                                                                                                                                  &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ]$`VALORE VENDUTO TOTALE`   )
        storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                    as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == currentWeek
                                  &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ),c(10,11,12,13,14,15) ]  <-(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                                                                                                                                          as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                                                                                                                                        &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ]$`VALORE VENDUTO PROMO`   )
        storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                    as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == currentWeek
                                  &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ),c(16,17,18,19,20,21) ]  <-(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                                                                                                                                          as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                                                                                                                                        &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ]$`QUANTITA' VENDUTA TOTALE`   )
        
        
        
      }
      
      
      
      
    }
  }
}

colnames(storeOrderedByWeek) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT1","VALORETOT2","VALORETOT3","VALORETOT4","VALORETOT5","VALORETOT6","VALOREPROMO1","VALOREPROMO2","VALOREPROMO3",
                                  "VALOREPROMO4","VALOREPROMO5","VALOREPROMO6","QUANTITA1","QUANTITA2","QUANTITA3","QUANTITA4","QUANTITA5","QUANTITA6")





# ##########################################################RIMOZIONE SETTIMANA 53

# I VALORI DELLA SETTIMANA 53 SONO MOLTO DIVERSI DALLA 52 E 1, MA NEL COMPLESSO DI AVVICINA MOLTO DI PI? ALLA SETTIMANA
# QUINDI MEDIO LA SETTIMANA 53 DEL 2015 CON LA SETTIMANA 52 DELLO STESSO ANNO


# TODO MEDIA SETTIMANA 52 - 53 E 1-53, VALORE MINIMO UNISCO

for (currentYear in unique(storeOrderedByWeek$ANNONO)){
  # print(currentYear)
  for(currentWeek in unique(storeOrderedByWeek$SETTIMANANO)){
    # print(currentWeek)
    for(currentDept in unique(storeOrderedByWeek$REPARTO)){
      
      if( nrow(selectedStoreDeptAggregated[which (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) == currentDept &
                                                  as.numeric(as.character(selectedStoreDeptAggregated$SETTIMANANO)) == currentWeek
                                                  &  as.numeric(as.character(selectedStoreDeptAggregated$ANNONO)) == currentYear ), ] ) >0  )
      {
        
        if(currentWeek == 53){
          print("week53")
          
          
          weekBefore <- 52
          weekAfter <- 1
          yearAfter <- currentYear+1
          valueCurrentweek <- storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                                          as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == currentWeek
                                                        &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ] 
          valueWeekBefore <- storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                                         as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == weekBefore
                                                       &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ]
          valueWeekAfter <- storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                                        as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == weekAfter
                                                      &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == yearAfter ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ]
          meanBefore <- rowMeans(valueWeekBefore - valueCurrentweek)
          meanAfter <- rowMeans(valueWeekAfter - valueCurrentweek)
          
          if(meanBefore < meanAfter){
            print("inside")
            print(currentWeek)
            
            valueWeek <- rbind(valueCurrentweek,valueWeekBefore)
            
            print(colMeans(valueWeek))
            storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                        as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == weekBefore
                                      &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ]<- colMeans(valueWeek)
            
            storeOrderedByWeek <- storeOrderedByWeek[ (which (!(as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                                                  as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == currentWeek
                                                                &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ))), ] 
            
          }else{
            print("inside2")
            
            print(currentWeek)
            
            valueWeek <- rbind(valueCurrentweek,valueWeekAfter)
            
            print(colMeans(valueWeek))
            storeOrderedByWeek[which (as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                        as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == weekAfter
                                      &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == yearAfter ),c(4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21) ]<- colMeans(valueWeek)
            storeOrderedByWeek <- storeOrderedByWeek[ (which (!(as.numeric(as.character(storeOrderedByWeek$REPARTO)) == currentDept &
                                                                  as.numeric(as.character(storeOrderedByWeek$SETTIMANANO)) == currentWeek
                                                                &  as.numeric(as.character(storeOrderedByWeek$ANNONO)) == currentYear ))), ] 
          }
          
        }
        
      }
      
      
      
      
    }
  }
}

yearList2 <- c(2014,2015,2016)
par(mfrow = c(3, 1))

# ###############################################.CLUSTERING





set.seed(4884)

listYears <- list() #CONTIENE LE LISTE DI TUTTI I DIPARTIMNTI DI QUELL'ANNO




for(year in yearList2){
  
  
  # print("year cycle")
  # print(year)
  
  
  
  listAllDeptYear <- list() #contiene tutte le liste dei dipartimenti per un anno
  
  
  for(deptSelected in deptNumbersSecond){
    
    
    
    
    
    # print("dept cycle")
    # print(deptSelected)
    reversedStoreOrderedByWeekIper <- t(storeOrderedByWeek[which(storeOrderedByWeek$ANNONO==year & storeOrderedByWeek$REPARTO==deptSelected),])
    
    reversedStoreForClusterIper <- reversedStoreOrderedByWeekIper
    
    for(storeNo in storesNumbersSecond){
      #print((match(storeNo,storesNumbersSecond)))
      reversedStoreForClusterIper<- rbind(reversedStoreForClusterIper , reversedStoreOrderedByWeekIper[(3+(match(storeNo,storesNumbersSecond))),])
      
    }
    
    reversedStoreForClusterIper <- reversedStoreForClusterIper[(nrow(reversedStoreOrderedByWeekIper)+1):nrow(reversedStoreForClusterIper),]
    cosineDistanceMatrix <- matrix(nrow=nrow(reversedStoreForClusterIper),ncol=nrow(reversedStoreForClusterIper))
    
    for (x in 1:nrow(reversedStoreForClusterIper)){
      
      for(y in 1: nrow(reversedStoreForClusterIper)){
        
        cosineDistanceMatrix[x ,y ] <- (1 - ( (reversedStoreForClusterIper[x,] %*% reversedStoreForClusterIper[y,]) / 
                                                (sqrt((reversedStoreForClusterIper[x,]%*%reversedStoreForClusterIper[x,]) * (reversedStoreForClusterIper[y,]%*%reversedStoreForClusterIper[y,]) ))))
        
      }
    }
    
    
    
    listSkMeans <- list() #CONTIENE I RISULTATI DELL'SKMEANS PER OGNI TENTATIVO DI CLUSTER
    listSilhouette <- list() #CONTIENE LA SILHOUETTE DI OGNI TENTATIVO DI CLUSTER
    listSilhouetteAvgWidth <- list() #CONTIENE LA MEDIA DELLA SILHOUETTE DI OGNI TENTATIVO DI CLUSTER
    listBestCluster <- list() #CONTIENE IL MIGLIOR CLUSTER BASATO SUI RISULTATI DELLA SILHOUETTE
    listMatrixDeptYears <- list() #CONTIENE LA MATRICE DEI CLUSTER PER OGNI REPARTO PER OGNI ANNO
    
    asw <- numeric(nrow(reversedStoreForClusterIper))
    for (k in 2:(nrow(reversedStoreForClusterIper) -1) ){
      # print("cluster")
      # print(k)
      listSkMeans[[k]] <- skmeans(x = reversedStoreForClusterIper, k=k ,control=list(verbose=FALSE))
      
      # plot(silhouette(listSkMeans[[k]]))
      
      listSilhouette[[k]] <- silhouette(x=listSkMeans[[k]]$cluster, dmatrix = t(cosineDistanceMatrix))
      
      # print((summary(listSilhouette[[k]])$avg.width))
      listSilhouetteAvgWidth[k] <- (summary(listSilhouette[[k]])$avg.width)
      
      # asw[[k]] <- pam(coredata(t(reversedStoreForCluster)), k) $ silinfo $ avg.width
      # clusterResult<-skmeans(x = reversedStoreForCluster, k=k.best,control=list(verbose=FALSE))
    }
    
    # print((which.max(unlist(listSilhouetteAvgWidth)))+1)   #se il miglior cluster ? da 2 sta nell index 1, quindi +1
    bestClusterNo = (which.max(unlist(listSilhouetteAvgWidth)))
    clusterMatrix = do.call(cbind, listSkMeans)
    # print(clusterMatrix[,bestClusterNo]$cluster)
    
    listMatrixDeptYears[[(match(deptSelected,deptNumbersSecond))]] <- clusterMatrix    
    
    listBestCluster[[match(deptSelected,deptNumbersSecond)]] <- clusterMatrix[,bestClusterNo]$cluster #CONTIENE IL MIGLIOR CLUSTER PER OGNI DEPT PER OGNI ANNO
    
    
    listSummaryDeptYears <-list(listSkMeans,listSilhouette,listSilhouetteAvgWidth, listMatrixDeptYears ,listBestCluster) #CONTIENE LE 5 LISTE PER DIPARTIMENTO PER ANNO
    
    listAllDeptYear[[match(deptSelected,deptNumbersSecond)]] <- listSummaryDeptYears
    
  }
  
  listYears[[match(year,yearList2)]] <- listAllDeptYear
  
}

# LISTE YEAR CONTIENE 
# LIVELLO 1 : I TRE ANNI 2014,2015,2016
# LIVELLO 2 : I TRE REPARTI 3 , 4, 6
# LIVELLO 3 : LE 5 LISTE CHE RIASSUMONO IL DIPARTIMENTO
# LIVELLO 4 : I SOTTOLIVELLI DI OGNI LISTA DESCRITTIVA



clusterDataframe <- as.data.frame( selectedStoreDeptAggregated$ANNONO)

clusterDataframe <- cbind(clusterDataframe,selectedStoreDeptAggregated$SETTIMANANO)

clusterDataframe <- cbind(clusterDataframe,selectedStoreDeptAggregated$REPARTO)






clusterDataframe <- unique(clusterDataframe)


colnames(clusterDataframe) <- c("ANNONO","SETTIMANANO","REPARTO")


clusterDataframe <- clusterDataframe[!clusterDataframe$SETTIMANANO==53,]

clusterDataframe <- cbind(clusterDataframe , storeOrderedByWeek[,4:9])




clusterDataframe <- clusterDataframe[!clusterDataframe$ANNONO == as.numeric("2017"), ]

clusterDataframe <- (clusterDataframe[with(clusterDataframe, order(ANNONO, REPARTO,SETTIMANANO)), ])

clusterVector <- c()

for(year in yearList2){
  
  for(deptSelected in deptNumbersSecond){
    
    clusterVector <- append(clusterVector,listYears[[match(year,yearList2)]][[match(deptSelected,deptNumbersSecond)]][[5]][[match(deptSelected,deptNumbersSecond)]])
    
  }
}

clusterDepts <- selectedStoreDeptAggregated[which( (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) %in% deptNumbersSecond)),]
clusterDepts <- clusterDepts [which( (selectedStoreDeptAggregated$ANNONO %in% yearList2)),]

clusterDepts <- clusterDepts[!duplicated(clusterDepts[,c('ENTE','REPARTO','ANNONO')]),c('ENTE','REPARTO','ANNONO')]
clusterDepts <- cbind(clusterDepts, clusterVector)
View(clusterDepts)
# View(clusterDataframe)

# PREDICTION BASE BEST 
# > min(rmse(error1),rmse(error2),rmse(error3),rmse(error4),rmse(error5),rmse(error6))
# [1] 13473.87
# > min(mae(error1),mae(error2),mae(error3),mae(error4),mae(error5),mae(error6))
# [1] 10040.44
# > 
#   > min(mean(abs((error1)/predictionMade$VALORETOT1) * 100), mean(abs((error2)/predictionMade$VALORETOT1) * 100), mean(abs((error3)/predictionMade$VALORETOT1) * 100),
#         +     mean(abs((error4)/predictionMade$VALORETOT1) * 100), mean(abs((error5)/predictionMade$VALORETOT1) * 100), mean(abs((error6)/predictionMade$VALORETOT1) * 100))
# [1] 7.180184
# > 
#   > getTrainPerf(svmRadialFit1)$TrainRMSE
# [1] 23278.83
# > getTrainPerf(svmRadialFit2)$TrainRMSE
# [1] 20229.93
# > getTrainPerf(svmRadialFit3)$TrainRMSE
# [1] 21354.98
# > 
#   > getTrainPerf(svmRadialSigmaFit1)$TrainRMSE
# [1] 21553.12
# > getTrainPerf(svmRadialSigmaFit2)$TrainRMSE
# [1] 20261.39
# > getTrainPerf(svmRadialSigmaFit3)$TrainRMSE
# [1] 19945.21
# > 
#   > min(getTrainPerf(svmRadialFit1)$TrainRMSE,getTrainPerf(svmRadialFit2)$TrainRMSE,getTrainPerf(svmRadialFit3)$TrainRMSE,
#         +     getTrainPerf(svmRadialSigmaFit1)$TrainRMSE,getTrainPerf(svmRadialSigmaFit2)$TrainRMSE,getTrainPerf(svmRadialSigmaFit3)$TrainRMSE)
# [1] 19945.21


# PREDICTION 2ND
# > min(rmse(error1),rmse(error2),rmse(error3),rmse(error4),rmse(error5),rmse(error6))
# [1] 11994.84
# > min(mae(error1),mae(error2),mae(error3),mae(error4),mae(error5),mae(error6))
# [1] 9314.431
# > 
#   > min(mean(abs((error1)/predictionMade$VALORETOT1) * 100), mean(abs((error2)/predictionMade$VALORETOT1) * 100), mean(abs((error3)/predictionMade$VALORETOT1) * 100),
#         +     mean(abs((error4)/predictionMade$VALORETOT1) * 100), mean(abs((error5)/predictionMade$VALORETOT1) * 100), mean(abs((error6)/predictionMade$VALORETOT1) * 100))
# [1] 8.387398
# > 
#   > 
#   > getTrainPerf(svmRadialFit1)$TrainRMSE
# [1] 25056.46
# > getTrainPerf(svmRadialFit2)$TrainRMSE
# [1] 22423.6
# > getTrainPerf(svmRadialFit3)$TrainRMSE
# [1] 22630.64
# > 
#   > getTrainPerf(svmRadialSigmaFit1)$TrainRMSE
# [1] 23311.44
# > getTrainPerf(svmRadialSigmaFit2)$TrainRMSE
# [1] 22321.29
# > getTrainPerf(svmRadialSigmaFit3)$TrainRMSE
# [1] 22131.48
# > 
#   > min(getTrainPerf(svmRadialFit1)$TrainRMSE,getTrainPerf(svmRadialFit2)$TrainRMSE,getTrainPerf(svmRadialFit3)$TrainRMSE,
#         +     getTrainPerf(svmRadialSigmaFit1)$TrainRMSE,getTrainPerf(svmRadialSigmaFit2)$TrainRMSE,getTrainPerf(svmRadialSigmaFit3)$TrainRMSE)
# [1] 22131.48
# 
# 8.387398
# 22131.48

# PREDICTION 3RD

# > min(rmse(error1),rmse(error2),rmse(error3),rmse(error4),rmse(error5),rmse(error6))
# [1] 12190.94
# > min(mae(error1),mae(error2),mae(error3),mae(error4),mae(error5),mae(error6))
# [1] 9436.837
# > 
#   > min(mean(abs((error1)/predictionMade$VALORETOT1) * 100), mean(abs((error2)/predictionMade$VALORETOT1) * 100), mean(abs((error3)/predictionMade$VALORETOT1) * 100),
#         +     mean(abs((error4)/predictionMade$VALORETOT1) * 100), mean(abs((error5)/predictionMade$VALORETOT1) * 100), mean(abs((error6)/predictionMade$VALORETOT1) * 100))
# [1] 8.452605
# > 
#   > 
#   > getTrainPerf(svmRadialFit1)$TrainRMSE
# [1] 24240.47
# > getTrainPerf(svmRadialFit2)$TrainRMSE
# [1] 21628.48
# > getTrainPerf(svmRadialFit3)$TrainRMSE
# [1] 20409.71
# > 
#   > getTrainPerf(svmRadialSigmaFit1)$TrainRMSE
# [1] 17651.93
# > getTrainPerf(svmRadialSigmaFit2)$TrainRMSE
# [1] 17432.55
# > getTrainPerf(svmRadialSigmaFit3)$TrainRMSE
# [1] 17538.37
# > 
#   > min(getTrainPerf(svmRadialFit1)$TrainRMSE,getTrainPerf(svmRadialFit2)$TrainRMSE,getTrainPerf(svmRadialFit3)$TrainRMSE,
#         +     getTrainPerf(svmRadialSigmaFit1)$TrainRMSE,getTrainPerf(svmRadialSigmaFit2)$TrainRMSE,getTrainPerf(svmRadialSigmaFit3)$TrainRMSE)
# [1] 17432.55

# PREDICTION3RDMINUS2ND
# > min(rmse(error1),rmse(error2),rmse(error3),rmse(error4),rmse(error5),rmse(error6))
# [1] 14430.88
# > min(mae(error1),mae(error2),mae(error3),mae(error4),mae(error5),mae(error6))
# [1] 10674.71
# > 
#   > min(mean(abs((error1)/predictionMade$VALORETOT1) * 100), mean(abs((error2)/predictionMade$VALORETOT1) * 100), mean(abs((error3)/predictionMade$VALORETOT1) * 100),
#         +     mean(abs((error4)/predictionMade$VALORETOT1) * 100), mean(abs((error5)/predictionMade$VALORETOT1) * 100), mean(abs((error6)/predictionMade$VALORETOT1) * 100))
# [1] 8.373621
# > 
#   > 
#   > getTrainPerf(svmRadialFit1)$TrainRMSE
# [1] 20642.8
# > getTrainPerf(svmRadialFit2)$TrainRMSE
# [1] 19709.92
# > getTrainPerf(svmRadialFit3)$TrainRMSE
# [1] 17583.68
# > 
#   > getTrainPerf(svmRadialSigmaFit1)$TrainRMSE
# [1] 17343.66
# > getTrainPerf(svmRadialSigmaFit2)$TrainRMSE
# [1] 16333.59
# > getTrainPerf(svmRadialSigmaFit3)$TrainRMSE
# [1] 16230.92
# > 
#   > min(getTrainPerf(svmRadialFit1)$TrainRMSE,getTrainPerf(svmRadialFit2)$TrainRMSE,getTrainPerf(svmRadialFit3)$TrainRMSE,
#         +     getTrainPerf(svmRadialSigmaFit1)$TrainRMSE,getTrainPerf(svmRadialSigmaFit2)$TrainRMSE,getTrainPerf(svmRadialSigmaFit3)$TrainRMSE)
# [1]  16230.92
# 8.373621
# 16230.92


# PREDITIONBASE 2 STORES

# > min(rmse(error1),rmse(error2),rmse(error3),rmse(error4),rmse(error5),rmse(error6))
# [1] 14304.05
# > min(mae(error1),mae(error2),mae(error3),mae(error4),mae(error5),mae(error6))
# [1] 10978.55
# > 
#   > min(mean(abs((error1)/predictionMade2stores$VALORETOT1) * 100), mean(abs((error2)/predictionMade2stores$VALORETOT1) * 100), mean(abs((error3)/predictionMade2stores$VALORETOT1) * 100),
#         +     mean(abs((error4)/predictionMade2stores$VALORETOT1) * 100), mean(abs((error5)/predictionMade2stores$VALORETOT1) * 100), mean(abs((error6)/predictionMade2stores$VALORETOT1) * 100))
# [1] 8.606016
# > 
#   > getTrainPerf(svmRadialFit1)$TrainRMSE
# [1] 22733.29
# > getTrainPerf(svmRadialFit2)$TrainRMSE
# [1] 22188.65
# > getTrainPerf(svmRadialFit3)$TrainRMSE
# [1] 22739.41
# > 
#   > getTrainPerf(svmRadialSigmaFit1)$TrainRMSE
# [1] 23840.35
# > getTrainPerf(svmRadialSigmaFit2)$TrainRMSE
# [1] 22272.2
# > getTrainPerf(svmRadialSigmaFit3)$TrainRMSE
# [1] 20406.15
# > 
#   > min(getTrainPerf(svmRadialFit1)$TrainRMSE,getTrainPerf(svmRadialFit2)$TrainRMSE,getTrainPerf(svmRadialFit3)$TrainRMSE,
#         +     getTrainPerf(svmRadialSigmaFit1)$TrainRMSE,getTrainPerf(svmRadialSigmaFit2)$TrainRMSE,getTrainPerf(svmRadialSigmaFit3)$TrainRMSE)
# [1] 20406.15
# > 
#   > 
#   > 
#   > 
#   > min(rmse(error12nd),rmse(error22nd),rmse(error32nd),rmse(error42nd),rmse(error52nd),rmse(error62nd))
# [1] 9588.197
# > min(mae(error12nd),mae(error22nd),mae(error32nd),mae(error42nd),mae(error52nd),mae(error62nd))
# [1] 7773.277
# > 
#   > min(mean(abs((error12nd)/predictionMade2stores$VALORETOT2) * 100), mean(abs((error22nd)/predictionMade2stores$VALORETOT2) * 100), mean(abs((error32nd)/predictionMade2stores$VALORETOT2) * 100),
#         +     mean(abs((error42nd)/predictionMade2stores$VALORETOT2) * 100), mean(abs((error52nd)/predictionMade2stores$VALORETOT2) * 100), mean(abs((error62nd)/predictionMade2stores$VALORETOT2) * 100))
# [1] 8.38124
# > 
#   > getTrainPerf(svmRadialFit12nd)$TrainRMSE
# [1] 15008.53
# > getTrainPerf(svmRadialFit22nd)$TrainRMSE
# [1] 13039.42
# > getTrainPerf(svmRadialFit32nd)$TrainRMSE
# [1] 13372.56
# > 
#   > getTrainPerf(svmRadialSigmaFit12nd)$TrainRMSE
# [1] 14471.86
# > getTrainPerf(svmRadialSigmaFit22nd)$TrainRMSE
# [1] 13560.59
# > getTrainPerf(svmRadialSigmaFit32nd)$TrainRMSE
# [1] 13286.22
# > 
#   > min(getTrainPerf(svmRadialFit12nd)$TrainRMSE,getTrainPerf(svmRadialFit22nd)$TrainRMSE,getTrainPerf(svmRadialFit32nd)$TrainRMSE,
#         +     getTrainPerf(svmRadialSigmaFit12nd)$TrainRMSE,getTrainPerf(svmRadialSigmaFit22nd)$TrainRMSE,getTrainPerf(svmRadialSigmaFit32nd)$TrainRMSE)
# [1] 13039.42
# > 


# PREDICTION3RDMINU2ND2STORES
# > min(rmse(error13rd),rmse(error23rd),rmse(error33rd),rmse(error43rd),rmse(error53rd),rmse(error63rd))
# [1] 14495.43
# > min(mae(error13rd),mae(error23rd),mae(error33rd),mae(error43rd),mae(error53rd),mae(error63rd))
# [1] 10685.27
# > 
#   > min(mean(abs((error13rd)/predictionMade$VALORETOT1) * 100), mean(abs((error23rd)/predictionMade$VALORETOT1) * 100), mean(abs((error33rd)/predictionMade$VALORETOT1) * 100),
#         +     mean(abs((error43rd)/predictionMade$VALORETOT1) * 100), mean(abs((error53rd)/predictionMade$VALORETOT1) * 100), mean(abs((error63rd)/predictionMade$VALORETOT1) * 100))
# [1] 8.383779
# > 
#   > 
#   > getTrainPerf(svmRadialFit13rd)$TrainRMSE
# [1] 19659.58
# > getTrainPerf(svmRadialFit23rd)$TrainRMSE
# [1] 18950.47
# > getTrainPerf(svmRadialFit33rd)$TrainRMSE
# [1] 18008.84
# > 
#   > getTrainPerf(svmRadialSigmaFit13rd)$TrainRMSE
# [1] 17893.79
# > getTrainPerf(svmRadialSigmaFit23rd)$TrainRMSE
# [1] 16278.53
# > getTrainPerf(svmRadialSigmaFit33rd)$TrainRMSE
# [1] 16375.4
# > 
#   > min(getTrainPerf(svmRadialFit13rd)$TrainRMSE,getTrainPerf(svmRadialFit23rd)$TrainRMSE,getTrainPerf(svmRadialFit33rd)$TrainRMSE,
#         +     getTrainPerf(svmRadialSigmaFit13rd)$TrainRMSE,getTrainPerf(svmRadialSigmaFit23rd)$TrainRMSE,getTrainPerf(svmRadialSigmaFit33rd)$TrainRMSE)
# [1] 16278.53
# > 
#   > error14th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction14th
# > error24th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction24th
# > error34th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction34th
# > error44th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction44th
# > error54th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction54th
# > error64th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction64th
# > 
#   > min(rmse(error14th),rmse(error24th),rmse(error34th),rmse(error44th),rmse(error54th),rmse(error64th))
# [1] 9966.681
# > min(mae(error14th),mae(error24th),mae(error3),mae(error44th),mae(error54th),mae(error64th))
# [1] 8104.499
# > 
#   > min(mean(abs((error14th)/predictionMade3rd$VALORETOT2) * 100), mean(abs((error24th)/predictionMade3rd$VALORETOT2) * 100), mean(abs((error34th)/predictionMade3rd$VALORETOT2) * 100),
#         +     mean(abs((error44th)/predictionMade3rd$VALORETOT2) * 100), mean(abs((error54th)/predictionMade3rd$VALORETOT2) * 100), mean(abs((error64th)/predictionMade3rd$VALORETOT2) * 100))
# [1] 8.668453
# > 
#   > 
#   > getTrainPerf(svmRadialFit14th)$TrainRMSE
# [1] 13131.39
# > getTrainPerf(svmRadialFit24th)$TrainRMSE
# [1] 11801.56
# > getTrainPerf(svmRadialFit34th)$TrainRMSE
# [1] 12134.52
# > 
#   > getTrainPerf(svmRadialSigmaFit14th)$TrainRMSE
# [1] 11596.08
# > getTrainPerf(svmRadialSigmaFit24th)$TrainRMSE
# [1] 10926.36
# > getTrainPerf(svmRadialSigmaFit34th)$TrainRMSE
# [1] 10974.14
# > 
#   > min(getTrainPerf(svmRadialFit14th)$TrainRMSE,getTrainPerf(svmRadialFit24th)$TrainRMSE,getTrainPerf(svmRadialFit34th)$TrainRMSE,
#         +     getTrainPerf(svmRadialSigmaFit14th)$TrainRMSE,getTrainPerf(svmRadialSigmaFit24th)$TrainRMSE,getTrainPerf(svmRadialSigmaFit34th)$TrainRMSE)
# [1] 10926.36