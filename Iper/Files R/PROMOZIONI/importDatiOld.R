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


# Estrazione_con_famiglia <- read_delim("~/Downloads/Estrazione con famiglia.csv", 
#                                       "|", escape_double = FALSE, trim_ws = TRUE)


########################################LETTURA FILE#############################################

if(!exists("Estrazione_con_famiglia")){
  
  # lettura da file
  Estrazione_con_famiglia <- read_delim("~/Downloads/EstrazioneNew.csv", 
                                        "|", escape_double = FALSE, trim_ws = TRUE, 
                                        col_types = list(col_number() , col_character() ,col_number() , col_character() ,col_number() , col_character() ,
                                                         col_number() , col_character() ,col_character(), col_character() , col_character(), col_character(),
                                                         col_character(), col_number(),col_character(), col_character(), col_character(),
                                                         col_number(), col_number(), col_number(), col_number()))
  
  
  
  
  # View(Estrazione_con_famiglia)
  
  
  ################rimozioni duplicati per i campi selezionati
  
  selectedStorePromoNoDuplicate <- sqldf("SELECT ENTE,SETTORE,REPARTO,SETTIMANA,MAX(PROMOZIONE_CD) AS PROMOZIONE_CD,MAX(DATA_INIZIO) AS DATA_INIZIO,MAX(DATA_FINE) AS DATA_FINE,GRUPPO,FAMIGLIA,
                                         MAX([VALORE VENDUTO TOTALE]) AS [VALORE VENDUTO TOTALE],
                                         MAX([QUANTITA' VENDUTA TOTALE]) AS [QUANTITA' VENDUTA TOTALE],SUM([VALORE VENDUTO PROMO]) AS [VALORE VENDUTO PROMO],
                                         SUM([QUANTITA' VENDUTA PROMO]) AS [QUANTITA' VENDUTA PROMO]
                                         FROM Estrazione_con_famiglia GROUP BY ENTE,SETTORE,REPARTO,SETTIMANA,GRUPPO,FAMIGLIA")
  
  # selectedStorePromoNoDuplicate2<- Estrazione_con_famiglia[!(duplicated(Estrazione_con_famiglia[c("ENTE","SETTORE","REPARTO","SETTIMANA","GRUPPO","FAMIGLIA","QUANTITA' VENDUTA TOTALE",
  #                                                                                                 "QUANTITA' VENDUTA PROMO")])), ]
  
  View(selectedStorePromoNoDuplicate)
  
  # aggiunta nuova colonna contenente gli ultimi due charatteri della colonna settimana
  Estrazione_con_famiglia$SETTIMANANO <- substr(Estrazione_con_famiglia$SETTIMANA,5,6)
  # aggiunta nuova colonna contenente i primi quattro charatteri della colonna settimana
  Estrazione_con_famiglia$ANNONO <- substr(Estrazione_con_famiglia$SETTIMANA,1,4)
  
  # tengo solo gli enti selezionati 
  
  selectedStorePromoNoDuplicate <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE %in% c(9,8,31,21,4) ) ,]
  #selectedStorePromoNoDuplicate2 <- selectedStorePromoNoDuplicate2[which(selectedStorePromoNoDuplicate2$ENTE %in% c(9,8,31,21,4) ) ,]
  
  # aggiunta nuova colonna contenente gli ultimi due charatteri della colonna settimana
  selectedStorePromoNoDuplicate$SETTIMANANO <- substr(selectedStorePromoNoDuplicate$SETTIMANA,5,6)
  # aggiunta nuova colonna contenente i primi quattro charatteri della colonna settimana
  selectedStorePromoNoDuplicate$ANNONO <- substr(selectedStorePromoNoDuplicate$SETTIMANA,1,4)
  
  # selectedStorePromoNoDuplicate2$SETTIMANANO <- substr(selectedStorePromoNoDuplicate2$SETTIMANA,5,6)
  # selectedStorePromoNoDuplicate2$ANNONO <- substr(selectedStorePromoNoDuplicate2$SETTIMANA,1,4)
  
  # rimuovo colonne selezionate
  #selectedStorePromoNoDuplicate2 <- selectedStorePromoNoDuplicate2[ , -c(2,3,4,6,8,11,15,17) ]
  
}


################ #######

# creo la tabella con tutte le combinazioni necessarie prendendo dare e combinazioni di settore,reparto,gruppo,famiglia

tabelladipartenza<- Estrazione_con_famiglia[,c(5,7,14,16)]
tabelladipartenza <- unique(tabelladipartenza)
tabelladate<- Estrazione_con_famiglia[,c(22,23)]
tabelladate <- unique(tabelladate)

if(!exists("storeOrderedByWeekPromo")){
  
  storeOrderedByWeekPromo <- merge(x = tabelladate, y = tabelladipartenza, by = NULL)
  
  
  # inserisco in colonne distinte le quantit? promo dei vari enti
  # selectedStorePromoNoDuplicate2[which(selectedStorePromoNoDuplicate2$ENTE == 9),16] <- selectedStorePromoNoDuplicate2[which(selectedStorePromoNoDuplicate2$ENTE == 9),]$'QUANTITA\' VENDUTA PROMO'
  # selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 8),17] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 8),]$'QUANTITA\' VENDUTA PROMO'
  # selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 31),18] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 31),]$'QUANTITA\' VENDUTA PROMO'
  # selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 21),19] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 21),]$'QUANTITA\' VENDUTA PROMO'
  # selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),20] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),]$'QUANTITA\' VENDUTA PROMO'
  
  
  selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),16] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),]$'QUANTITA\' VENDUTA PROMO'
  selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 8),17] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 8),]$'QUANTITA\' VENDUTA PROMO'
  selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 9),18] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 9),]$'QUANTITA\' VENDUTA PROMO'
  selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 21),19] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 21),]$'QUANTITA\' VENDUTA PROMO'
  selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 31),20] <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 31),]$'QUANTITA\' VENDUTA PROMO'
  
  
  # prendo i dati dei solo enti che mi interessano
  store1 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 4),]
  store2 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 8),]
  store3 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 9),]
  store4 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 21),]
  store5 <- selectedStorePromoNoDuplicate[which(selectedStorePromoNoDuplicate$ENTE == 31),]
  
  # unisco tutta la tabella per ottenere quello che mi serve
  
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
  
  
  
  
  # NESSUNA PROMZIONE QUI
  # dove mancano valori metto 0
  storeOrderedByWeekPromo[is.na(storeOrderedByWeekPromo)] <- 0
  
  
  ###########################################################RIMOZIONE SETTIMANA 53 #############
  
  
  
  # I VALORI DELLA SETTIMANA 53 SONO MOLTO DIVERSI DALLA 52 E 1, MA NEL COMPLESSO DI AVVICINA MOLTO DI PI?? ALLA SETTIMANA
  # QUINDI MEDIO LA SETTIMANA 53 DEL 2015 CON LA SETTIMANA 52 DELLO STESSO ANNO
  
  
  # TODO MEDIA SETTIMANA 52 - 53 E 1-53, VALORE MINIMO UNISCO
  
  annoFastidio <- 2015
  settimanaFastidio <- 53
  weekBefore <- 52
  weekAfter <- 1
  yearAfter <- 2016
  
  if(53 %in% storeOrderedByWeekPromo$SETTIMANANO){
    
    for(currentDept in unique(storeOrderedByWeekPromo$REPARTO)){
      
      for(currentSettore in unique(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$REPARTO == currentDept),]$SETTORE) ){
        
        for(currentGruppo in unique(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$SETTORE == currentSettore),]$GRUPPO) ){
          
          for(currentFamiglia in unique(storeOrderedByWeekPromo[which(storeOrderedByWeekPromo$SETTORE == currentSettore & 
                                                                      storeOrderedByWeekPromo$GRUPPO == currentGruppo ),]$FAMIGLIA) ){
            
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
            
            # print("banana")
            
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
  
}
#########################################################################################


# Selezionare le famiglie che non contengono troppi 0 nella serie storica

# numeroCombinazioneAnnoSettimana <- nrow(unique(cbind(storeOrderedByWeekPromo$ANNONO,storeOrderedByWeekPromo$SETTIMANANO)))
# percentuale15 <- (numeroCombinazioneAnnoSettimana/100)*15


count_raw <- sqldf("SELECT DISTINCT REPARTO,SETTORE,GRUPPO,FAMIGLIA  FROM storeOrderedByWeekPromo WHERE ENTE1 != 0 AND ENTE2 != 0 AND ENTE3 != 0 AND ENTE4 != 0 AND ENTE5 != 0 
                   GROUP BY REPARTO,SETTORE,GRUPPO,FAMIGLIA HAVING COUNT(*) = (SELECT count(*)  FROM (select distinct ANNONO, SETTIMANANO FROM storeOrderedByWeekPromo))")

count_raw2<-sqldf("SELECT DISTINCT REPARTO,SETTORE,GRUPPO,FAMIGLIA FROM storeOrderedByWeekPromo WHERE ENTE1 = 0 AND ENTE2 = 0 AND ENTE3 = 0 AND ENTE4 = 0 AND ENTE5 = 0 
                  GROUP BY REPARTO,SETTORE,GRUPPO,FAMIGLIA HAVING COUNT(*) < (SELECT count(*)*15/100 FROM (select distinct ANNONO, SETTIMANANO FROM storeOrderedByWeekPromo)) ORDER BY REPARTO,SETTORE,GRUPPO,FAMIGLIA")

count_raw <- rbind(count_raw,count_raw2)



####################################CLUSTERING#########################################

if(!exists("tsPromo")){
  
  tsPromo <-merge(x = count_raw, y = storeOrderedByWeekPromo, by=c("REPARTO","SETTORE","GRUPPO","FAMIGLIA"))
  
  # cambio da character a numeric
  class(tsPromo$ANNONO) <- "numeric"
  class(tsPromo$SETTIMANANO) <- "numeric"
  
  tsPromo <- sqldf("SELECT * FROM tsPromo ORDER BY REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,SETTIMANANO")
  
  
  # familyList <- unique(tsPromo$FAMIGLIA)
  yearList2 <- head(unique(tsPromo$ANNONO),-1)
  
  listAllReparto <- list() #contiene tutte le liste dei dipartimenti per un anno
  
  for(currentReparto in unique(tsPromo$REPARTO) ){
    
    print("REPARTO")
    print(currentReparto)
    
    
    listAllSettore <- list()
    
    for(currentSettore in unique(tsPromo[which(tsPromo$REPARTO == currentReparto),]$SETTORE)){
      print("SETTORE")
      print(currentSettore)
      
      listAllGroupSettore <- list()
      
      for(currentGruppo in unique(tsPromo[which(tsPromo$SETTORE == currentSettore),]$GRUPPO)){
        
        print("GRUPPO")
        print(currentGruppo)
        
        reversedStoreOrderedByWeekIper2 <- t( tsPromo[which(tsPromo$SETTORE == currentSettore &
                                                              tsPromo$GRUPPO == currentGruppo &
                                                              tsPromo$ANNONO!= tail(unique(tsPromo$ANNONO) , 1 ) ) , ] )
        
        familyListClustering <- unique(reversedStoreOrderedByWeekIper2[4,])
        
        reversedStoreForClusterIper2 <- reversedStoreOrderedByWeekIper2
        
        # ogni riga è uno store diverso 5 store
        # attacco in riga le timeseries ripetute
        for(storeNo in 1:5){
          
          reversedStoreForClusterIper2<- rbind (reversedStoreForClusterIper2 , reversedStoreOrderedByWeekIper2 [ ( 6+storeNo ) , ] )
          
        }
        
        
        
        # Mantengo solo le righe contenenti le timeseries
        reversedStoreForClusterIper2 <- reversedStoreForClusterIper2 [ ( nrow ( reversedStoreOrderedByWeekIper2 ) +1 ) : nrow( reversedStoreForClusterIper2 ) , ]
        
        
        # SUDDIVIDO LA TS IN RIGHE DA 52 SETTIMANE
        reversedClusterYears2 <- matrix(ncol=52)
        
        appoggio <- reversedStoreForClusterIper2
        print("Famiglia")
        print(familyListClustering)
        
        for(currentFamily in familyListClustering){
          
          for(year in yearList2){
            
            reversedClusterYears2 <- rbind(reversedClusterYears2, appoggio[,1:52] )
            appoggio <- appoggio[,-(1:52)]
            
            
          }
        }
        
        reversedClusterYears2 <- reversedClusterYears2[-1,]
        
        # reversedCluster contiene 6 righe per il primo anno, poi 6 righe per il secondo, e sei righe per il terzo
        
        # CALCOLO LA DISTANZA COSENO DELLE TIME SERIES
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
        listMatrixGroup <- list() #CONTIENE LA MATRICE DEI CLUSTER 
        
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
        bestClusterNo2 = (which.max( unlist(listSilhouetteAvgWidth2) ) )
        clusterMatrix2 = do.call(cbind, listSkMeans2)
        
        print(clusterMatrix2[,bestClusterNo2]$cluster)
        
        
        # LIST MATRIX GROUP CONTIENE TUTTI I TENTIATIVI DA 2 A N-1 DI CLUTSERING (NESSUNA UTILITà AL MOMENTO, SALVATE PER NON SAPER NE LEGGERE NE SCRIVERE)
        listMatrixGroup[[ match(currentGruppo, unique(tsPromo[which(tsPromo$SETTORE == currentSettore),]$GRUPPO ) ) ]] <- clusterMatrix2  
        
        # CONTIENE TRA TUTTI I ITENTATIVI DI CLUSTER FATTI DA 2 A N-1, SOLO IL MIGLIORE
        # IN 
        listBestCluster2[[  match(currentGruppo, unique(tsPromo[which(tsPromo$SETTORE == currentSettore),]$GRUPPO ) ) ]] <- clusterMatrix2[,bestClusterNo2]$cluster 
        
        
        listSummaryGroup <-list(listSkMeans2,listSilhouette2,listSilhouetteAvgWidth2, listMatrixGroup ,listBestCluster2) #CONTIENE LE 5 LISTE PER DIPARTIMENTO PER ANNO
        
        listAllGroupSettore[[ match(currentGruppo, unique(tsPromo[which(tsPromo$SETTORE == currentSettore),]$GRUPPO ) ) ]] <- listSummaryGroup
        
        
      }
      
      # CON LA FUNZIONE MATCH ANDIAMO A POSIZIONARE NELLA PRIMA POSIZIONE IL SETTORE CHE VIENE PER PRIMO ALL'INTERNO DEL VETTORE CON CUI CONFRONTIAMO
      
      
      listAllSettore[[ match(currentSettore, unique(tsPromo[which(tsPromo$REPARTO == currentReparto),]$SETTORE))  ]] <- listAllGroupSettore
      
    }
    
    listAllReparto[[match(currentReparto,unique(tsPromo$REPARTO))]] <-  listAllSettore
    
  }
}


####################### GENERO TABELLA CON TUTTE COMBINAZIONI DI REPARTO,SETTORE,GRUPPO,FAMIGLIA + ANNO ED ENTE APPENDO VETTORE DI CLUSTER ##################

tabelladipartenzaCluster<- selectedStorePromoNoDuplicate[,c(3,2,8,9,1)]
tabelladipartenzaCluster <- unique(tabelladipartenzaCluster)
tabelladateCluster <- selectedStorePromoNoDuplicate[,15]
tabelladateCluster <- unique(tabelladateCluster)
clusterDataframe2 <- (unique(merge(x = tabelladateCluster, y = tabelladipartenzaCluster, by = NULL)))
clusterDataframe2 <- unique(clusterDataframe2)


colnames(clusterDataframe2) <- c("ANNONO","REPARTO","SETTORE","GRUPPO","FAMIGLIA","ENTE")



clusterDataframe2 <-merge(x = clusterDataframe2, y = tsPromo[,c(1:5)],  by=c("REPARTO","SETTORE","GRUPPO","FAMIGLIA","ANNONO"))

clusterDataframe2 <- sqldf("SELECT REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,ENTE  FROM clusterDataframe2 order by REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,ENTE")

clusterDataframe2 <- unique(clusterDataframe2)


clusterDataframe2 <- clusterDataframe2[!clusterDataframe2$ANNONO == as.numeric("2017"), ]


clusterVector2 <- c()

for(currentReparto in unique(tsPromo$REPARTO) ){
  
  for(currentSettore in unique(tsPromo[which(tsPromo$REPARTO == currentReparto),]$SETTORE)){
    
    for(currentGruppo in unique(tsPromo[which(tsPromo$SETTORE == currentSettore),]$GRUPPO)){
      
      clusterVector2 <- append(clusterVector2, listAllReparto[[match(currentReparto,unique(tsPromo$REPARTO))]][[match(currentSettore, unique(tsPromo[which(tsPromo$REPARTO == currentReparto),]$SETTORE))]][[ match(currentGruppo, unique(tsPromo[which(tsPromo$SETTORE == currentSettore),]$GRUPPO ) ) ]][[5]][[  match(currentGruppo, unique(tsPromo[which(tsPromo$SETTORE == currentSettore),]$GRUPPO ) ) ]] )
      
    }
  }
}


# clusterDepts <- selectedStoreDeptAggregated[which( (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) %in% deptNumbersSecond)),]
# clusterDepts <- clusterDepts [which( (selectedStoreDeptAggregated$ANNONO %in% yearList2)),]
# 
# clusterDepts <- clusterDepts[!duplicated(clusterDepts[,c('REPARTO','ENTE','ANNONO')]),c('REPARTO','ENTE','ANNONO')]
clusterDepts2 <- cbind(clusterDataframe2, clusterVector2)


################################################## APPENDO ALLA MATRICE DI CUI SOPRA LE TIMESERIES IN RIGA ############################################################################################ 
appoggioDepts2 <- data.frame()


# in base all'ordine dei cicli cambia l'ordine di apoggioDepts2
for(currentReparto in unique(tsPromo$REPARTO) ){
  
  for(currentSettore in unique(tsPromo[which(tsPromo$REPARTO == currentReparto),]$SETTORE)){
    
    for(currentGruppo in unique(tsPromo[which(tsPromo$SETTORE == currentSettore),]$GRUPPO)){
      
      for(currentFamiglia in unique(tsPromo[which(tsPromo$SETTORE == currentSettore & tsPromo$GRUPPO == currentGruppo),]$FAMIGLIA)){
        
        for(currentAnno in head(unique(tsPromo$ANNONO),-1) ) {
          
          for(currentEnte in unique(clusterDataframe2$ENTE)){
            
            
            
            
            # prendo una riga da clusterDepts2 per ogni combinazione di repoarto, settore, gruppo , famiglia anno ed ente (quindi una tupla sola)
            # ci attacco la ts presa da tsPromo per quella combinazione e la riporto in appoggioDepts2
            # Questo per tutte le combinazioni
            
            appoggioDepts2 <- rbind(appoggioDepts2 , cbind(clusterDepts2[which( clusterDepts2$REPARTO == currentReparto &
                                                                                  clusterDepts2$SETTORE == currentSettore &
                                                                                  clusterDepts2$GRUPPO == currentGruppo &
                                                                                  clusterDepts2$FAMIGLIA == currentFamiglia &
                                                                                  clusterDepts2$ANNONO == currentAnno &
                                                                                  clusterDepts2$ENTE == currentEnte),] ,
                                                           t(    tsPromo[which( tsPromo$REPARTO == currentReparto &
                                                                                  tsPromo$SETTORE == currentSettore &
                                                                                  tsPromo$GRUPPO == currentGruppo &
                                                                                  tsPromo$FAMIGLIA == currentFamiglia &
                                                                                  tsPromo$ANNONO == currentAnno),
                                                                         (match(currentEnte,unique(clusterDataframe2$ENTE)) +6)])      ))
            
          }
        }
        
      }
      
    }
    
  }
  
  
  
}

View(appoggioDepts2)

appoggioDepts2 <- sqldf("SELECT *  FROM appoggioDepts2 order by REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,ENTE")