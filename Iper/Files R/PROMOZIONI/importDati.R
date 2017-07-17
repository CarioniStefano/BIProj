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
library(sqldf)
library(tcltk)
library(tcltk2)

# Estrazione_con_famiglia <- read_delim("~/Downloads/Estrazione con famiglia.csv", 
#                                       "|", escape_double = FALSE, trim_ws = TRUE)

if(!exists("Estrazione_con_famiglia")){
  
  Estrazione_con_famiglia <- read_delim("~/Downloads/Estrazione con famiglia.csv", 
                                        "|", escape_double = FALSE, trim_ws = TRUE, 
                                        col_types = list(col_number() , col_character() ,col_number() , col_character() ,col_number() , col_character() ,
                                                         col_number() , col_character() ,col_character(), col_character() , col_character(), col_character(),
                                                         col_character(), col_number(),col_character(), col_character(), col_character(),
                                                         col_number(), col_number(), col_number(), col_number()))
  
  
  

  View(Estrazione_con_famiglia)
  
  
  
  selectedStorePromoNoDuplicate<- Estrazione_con_famiglia[!(duplicated(Estrazione_con_famiglia[c("ENTE","SETTORE","REPARTO","SETTIMANA","GRUPPO","FAMIGLIA","QUANTITA' VENDUTA TOTALE",
                                                                                                 "QUANTITA' VENDUTA PROMO")])), ]
  View(selectedStorePromoNoDuplicate)
  Estrazione_con_famiglia$SETTIMANANO <- substr(Estrazione_con_famiglia$SETTIMANA,5,6)
  Estrazione_con_famiglia$ANNONO <- substr(Estrazione_con_famiglia$SETTIMANA,1,4)
  
  selectedStorePromoNoDuplicate <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE %in% c(9,8,31,21,4) ) ,]
  
  selectedStorePromoNoDuplicate$SETTIMANANO <- substr(selectedStorePromoNoDuplicate$SETTIMANA,5,6)
  selectedStorePromoNoDuplicate$ANNONO <- substr(selectedStorePromoNoDuplicate$SETTIMANA,1,4)
  
  selectedStorePromoNoDuplicate <- selectedStorePromoNoDuplicate[ , -c(2,3,4,6,8,11,15,17) ]
  
}

tabelladipartenza<- Estrazione_con_famiglia[,c(5,7,14,16)]
tabelladipartenza <- unique(tabelladipartenza)
tabelladate<- Estrazione_con_famiglia[,c(22,23)]
tabelladate <- unique(tabelladate)

storeOrderedByWeekPromo <- merge(x = tabelladate, y = tabelladipartenza, by = NULL)



selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 9),16] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 9),]$'QUANTITA\' VENDUTA PROMO'
selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 8),17] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 8),]$'QUANTITA\' VENDUTA PROMO'
selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 31),18] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 31),]$'QUANTITA\' VENDUTA PROMO'
selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 21),19] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 21),]$'QUANTITA\' VENDUTA PROMO'
selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),20] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),]$'QUANTITA\' VENDUTA PROMO'

store1 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 9),]
store2 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 8),]
store3 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 31),]
store4 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 21),]
store5 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),]



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

Estrazione_con_famiglia[is.na(Estrazione_con_famiglia),]



# ##########################################################RIMOZIONE SETTIMANA 53



# I VALORI DELLA SETTIMANA 53 SONO MOLTO DIVERSI DALLA 52 E 1, MA NEL COMPLESSO DI AVVICINA MOLTO DI PI? ALLA SETTIMANA
# QUINDI MEDIO LA SETTIMANA 53 DEL 2015 CON LA SETTIMANA 52 DELLO STESSO ANNO


# TODO MEDIA SETTIMANA 52 - 53 E 1-53, VALORE MINIMO UNISCO

annoFastidio <- 2015
settimanaFastidio <- 53
weekBefore <- 52
weekAfter <- 1
yearAfter <- 2016

if(53 %in% storeOrderedByWeekPromo$SETTIMANANO){
  
  
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
  
}





# count_raw<-sqldf("SELECT DISTINCT REPARTO,SETTORE,GRUPPO,FAMIGLIA FROM Estrazione_con_famiglia WHERE ENTE in (9,8,31,21,4)")
# 
# count_raw<-sqldf("SELECT DISTINCT REPARTO,SETTORE,GRUPPO,FAMIGLIA , COUNT(*) FROM storeOrderedByWeekPromo  GROUP BY REPARTO,SETTORE,GRUPPO,FAMIGLIA")
# 
# 
# 
# 
# count_raw<-sqldf("SELECT DISTINCT REPARTO,SETTORE,GRUPPO,FAMIGLIA  FROM storeOrderedByWeekPromo GROUP BY REPARTO,SETTORE,GRUPPO,FAMIGLIA ")
# 
# 
# count_raw<-sqldf("SELECT DISTINCT REPARTO,SETTORE,GRUPPO,FAMIGLIA  FROM storeOrderedByWeekPromo WHERE ENTE1 != 0 AND ENTE2 != 0 AND ENTE3 != 0 AND ENTE4 != 0 AND ENTE5 != 0
#                  GROUP BY REPARTO,SETTORE,GRUPPO,FAMIGLIA HAVING COUNT(*) = 181")


count_raw<-sqldf("SELECT DISTINCT REPARTO,SETTORE,GRUPPO,FAMIGLIA  FROM storeOrderedByWeekPromo WHERE ENTE1 != 0 OR ENTE2 != 0 OR ENTE3 != 0 OR ENTE4 != 0 OR ENTE5 != 0 
                 GROUP BY REPARTO,SETTORE,GRUPPO,FAMIGLIA HAVING COUNT(*) = 181")

count_raw<-sqldf("SELECT DISTINCT REPARTO,SETTORE,GRUPPO,FAMIGLIA , COUNT(*) FROM storeOrderedByWeekPromo WHERE ENTE1 = 0 AND ENTE2 = 0 AND ENTE3 = 0 AND ENTE4 = 0 AND ENTE5 = 0 
                 GROUP BY REPARTO,SETTORE,GRUPPO,FAMIGLIA HAVING COUNT(*) < 18")




asdasd <-merge(x = count_raw, y = storeOrderedByWeekPromo, by=c("REPARTO","SETTORE","GRUPPO","FAMIGLIA"))

count_raw1<-sqldf("SELECT DISTINCT  REPARTO,SETTORE,GRUPPO,FAMIGLIA FROM asdasd")

# for(currentReparto in unique(storeOrderedByWeekPromo$REPARTO)){
#   
#   for(currentSettore in unique(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$REPARTO == currentReparto),]$SETTORE ) ){
#     
#     for(currentGruppo in unique(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$SETTORE == currentSettore),]$GRUPPO)){
#       
#       
#       
#       
#       
#       for(currentFamiglia in unique(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$SETTORE == currentSettore & storeOrderedByWeekPromo$GRUPPO == currentGruppo),]$FAMIGLIA) ){
#         
#         print(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$REPARTO == currentReparto & storeOrderedByWeekPromo$SETTORE == currentSettore & storeOrderedByWeekPromo$GRUPPO == currentGruppo),])
#         
#         
#       }
#     }
#   }
# } 










listAllDeptYear2 <- list() #contiene tutte le liste dei dipartimenti per un anno
yearList2 <- c(2014,2015,2016)

for(currentSettore in unique(storeOrderedByWeekPromo$SETTORE)){
  
  for(currentGruppo in unique(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$SETTORE == currentSettore),]$GRUPPO)){
    
    for(currentFamiglia in unique(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$SETTORE == currentSettore & storeOrderedByWeekPromo$GRUPPO == currentGruppo),]$FAMIGLIA) ){
      
      # print("dept cycle")
      # print(deptSelected)
      reversedStoreOrderedByWeekIper2 <- t(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$SETTORE == currentSettore &
                                                                           storeOrderedByWeekPromo$GRUPPO == currentGruppo &
                                                                           storeOrderedByWeekPromo$FAMIGLIA == currentFamiglia &
                                                                           storeOrderedByWeek2$ANNONO!= 2017),])
      
      
      
      
      reversedStoreForClusterIper2 <- reversedStoreOrderedByWeekIper2
      
      for(storeNo in 1:5){
        #print((match(storeNo,storesNumbersSecond)))
        reversedStoreForClusterIper2<- rbind(reversedStoreForClusterIper2 , reversedStoreOrderedByWeekIper2[(6+storeNo),])
        
      }
      
      reversedStoreForClusterIper2 <- reversedStoreForClusterIper2[(nrow(reversedStoreOrderedByWeekIper2)+1):nrow(reversedStoreForClusterIper2),]
      print(nrow(reversedStoreForClusterIper2))
      print(ncol(reversedStoreForClusterIper2))
      
      
      reversedClusterYears2 <- matrix(ncol=52)
      
      for(year in yearList2){
        
        startIndex <- (52*(match(year,yearList2)-1) )
        
        endIndex <- (52*(match(year,yearList2)) )
        # print((startIndex+1) : endIndex)
        
        reversedClusterYears2 <- rbind(reversedClusterYears2,reversedStoreForClusterIper2[,( (startIndex+1) : endIndex)])
        
        
      }
      
      reversedClusterYears2 <- reversedClusterYears2[-1,]
      
      # reversedCluster contiene 6 righe per il primo anno, poi 6 righe per il secondo, e sei righe per il terzo
      
      cosineDistanceMatrix2 <- matrix(nrow=nrow(reversedClusterYears2),ncol=nrow(reversedClusterYears2))
      
      for (x in 1:nrow(reversedClusterYears2)){
        
        for(y in 1: nrow(reversedClusterYears2)){
          
          cosineDistanceMatrix2[x ,y ] <- (1 - ( (as.numeric(reversedClusterYears2[x,]) %*% as.numeric(reversedClusterYears2[y,])) / 
                                                  (sqrt((as.numeric(reversedClusterYears2[x,]) %*% as.numeric(reversedClusterYears2[x,])) * (as.numeric(reversedClusterYears2[y,])%*%as.numeric(reversedClusterYears2[y,]) ) ))))
          
        }
      }
      
      
      listSkMeans2 <- list() #CONTIENE I RISULTATI DELL'SKMEANS PER OGNI TENTATIVO DI CLUSTER
      listSilhouette2 <- list() #CONTIENE LA SILHOUETTE DI OGNI TENTATIVO DI CLUSTER
      listSilhouetteAvgWidth2 <- list() #CONTIENE LA MEDIA DELLA SILHOUETTE DI OGNI TENTATIVO DI CLUSTER
      listBestCluster2 <- list() #CONTIENE IL MIGLIOR CLUSTER BASATO SUI RISULTATI DELLA SILHOUETTE
      listMatrixDeptYears2 <- list() #CONTIENE LA MATRICE DEI CLUSTER PER OGNI REPARTO PER OGNI ANNO
      class(reversedClusterYears2) <- "numeric"
      
      for (k in 2:(nrow(reversedClusterYears2) -1) ){
        # print("cluster")
        # print(k)
        listSkMeans2[[k]] <- skmeans(x = reversedClusterYears2, k=k ,control=list(verbose=FALSE))
        
        # plot(silhouette(listSkMeans[[k]]))
        
        listSilhouette2[[k]] <- silhouette(x=listSkMeans2[[k]]$cluster, dmatrix = t(cosineDistanceMatrix2))
        
        # print((summary(listSilhouette[[k]])$avg.width))
        listSilhouetteAvgWidth2[k] <- (summary(listSilhouette2[[k]])$avg.width)
        
        
        # clusterResult<-skmeans(x = reversedStoreForCluster, k=k.best,control=list(verbose=FALSE))
      }
      
      # print((which.max(unlist(listSilhouetteAvgWidth)))+1)   #se il miglior cluster ? da 2 sta nell index 1, quindi +1
      bestClusterNo2 = (which.max(unlist(listSilhouetteAvgWidth2)))
      clusterMatrix2 = do.call(cbind, listSkMeans2)
      print(clusterMatrix2[,bestClusterNo2]$cluster)
      
      
      
      listMatrixDeptYears2[[(match(deptSelected,deptNumbersSecond))]] <- clusterMatrix    
      
      listBestCluster[[match(deptSelected,deptNumbersSecond)]] <- clusterMatrix[,bestClusterNo]$cluster #CONTIENE IL MIGLIOR CLUSTER PER OGNI DEPT PER OGNI ANNO
      
      
      listSummaryDeptYears <-list(listSkMeans,listSilhouette,listSilhouetteAvgWidth, listMatrixDeptYears ,listBestCluster) #CONTIENE LE 5 LISTE PER DIPARTIMENTO PER ANNO
      
      listAllDeptYear[[match(deptSelected,deptNumbersSecond)]] <- listSummaryDeptYears
      
      
    }
    
  }
}

par(mfrow = c(1, 1))
plot(reversedStoreForClusterIper2[1,],type="l",col="blue", ylim=c(0,max(as.numeric(reversedStoreForClusterIper2))))
lines(reversedStoreForClusterIper2[2,],type="l",col="red")
lines(reversedStoreForClusterIper2[3,],type="l",col="green")
lines(reversedStoreForClusterIper2[4,],type="l",col="black")
lines(reversedStoreForClusterIper2[5,],type="l",col="purple")

