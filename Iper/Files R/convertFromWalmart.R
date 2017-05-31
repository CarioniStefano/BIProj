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
  
  sheets <- readxl::excel_sheets("~/Lavoro/Iper/Estrazione/Estrazione.xls")
  Estrazione <- list()
  for(sheet in sheets){
    Estrazione <- rbind(Estrazione, readxl::read_excel("~/Lavoro/Iper/Estrazione/Estrazione.xls",col_types = c("text", "text", "text",  "text", "text", "text", "text", "text",
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

storesNumbers <- c(8, 1 , 7 , 4 , 6 , 9)
storesNumbersSecond <- c(8, 1 , 7 , 4 , 6 , 9)
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

# I VALORI DELLA SETTIMANA 53 SONO MOLTO DIVERSI DALLA 52 E 1, MA NEL COMPLESSO DI AVVICINA MOLTO DI PIù ALLA SETTIMANA
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
    
    # print((which.max(unlist(listSilhouetteAvgWidth)))+1)   #se il miglior cluster è da 2 sta nell index 1, quindi +1
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


# per ogni cluster la time series  deve essere colorata per store, se ho 3 cluster lo store deve avere lo stesso colore