# par(mfrow = c(1, 1))
# due possibilità
# quell'ente per questo reparto nel 2017 a che cluster assomoglia?
# dovresti porendere tutti i cluster che ho, tagliare le 52 colonne a 17, calcolo il prototipo (media, centroidi calcolati sui 17) quale centroide è più simile al 2017?
# confornto i prototype con distanza coseno
# prendo i prototipi dei cluster, calcolo la distanza coseno 

require(zoo)



shift <- function(x, n, invert=FALSE, default=NA){
  stopifnot(length(x)>=n)
  if(n==0){
    return(x)
  }
  n <- ifelse(invert, n*(-1), n)
  if(n<0){
    n <- abs(n)
    forward=FALSE
  }else{
    forward=TRUE
  }
  if(forward){
    return(c(rep(default, n), x[seq_len(length(x)-n)]))
  }
  if(!forward){
    return(c(x[seq_len(length(x)-n)+n], rep(default, n)))
  }
}


clusterVector2017 <- c()


# tempDept2 <- tsPromo[which(tsPromo$REPARTO == 1 & tsPromo$SETTORE == 10 & tsPromo$GRUPPO == 1),]


tempDept3 <- tsPromo[which(tsPromo$REPARTO == 1),]

for(cycleReparto in unique(tsPromo$REPARTO)){
  
  for(cycleSettore in unique(tsPromo[which(tsPromo$REPARTO == cycleReparto),]$SETTORE)){
    
    for(cycleGruppo in unique(tsPromo[which(tsPromo$REPARTO == cycleReparto & tsPromo$SETTORE == cycleSettore),]$GRUPPO) ){
      
      for (currentFamily in unique(tsPromo[which( tsPromo$REPARTO == cycleReparto & tsPromo$SETTORE == cycleSettore & tsPromo$GRUPPO == cycleGruppo),]$FAMIGLIA) ) {
        
        tempDept2 <- tsPromo[which(tsPromo$REPARTO == cycleReparto & tsPromo$SETTORE == cycleSettore &
                                     tsPromo$GRUPPO == cycleGruppo &
                                     tsPromo$FAMIGLIA == currentFamily),]
        
        reversedStoreOrderedByWeek2017 <- t( tempDept2[which(tempDept2$REPARTO == cycleReparto &
                                                               tempDept2$SETTORE == cycleSettore &
                                                               tempDept2$GRUPPO == cycleGruppo &
                                                               tempDept2$FAMIGLIA==currentFamily &
                                                               tempDept2$ANNONO== 2017),] )
        print(paste("REPARTO", cycleReparto))
        print(paste("SETTORE", cycleSettore))
        print(paste("GRUPPO", cycleGruppo))
        print(paste("FAMILY", currentFamily))
        # print(table(reversedStoreOrderedByWeek2017$FAMIGLIA))
        reversedStoreForCluster2017 <- reversedStoreOrderedByWeek2017
        
        for(currentEnte in unique(clusterDataframe2$ENTE)){
          #print((match(storeNo,storesNumbersSecond)))
          reversedStoreForCluster2017<- rbind(reversedStoreForCluster2017 , reversedStoreOrderedByWeek2017[(6+(match(currentEnte,unique(clusterDataframe2$ENTE)))),])
          
        }
        
        clusterVectorForGraph <- c()
        clusterCentroids <- data.frame()
        clusterMedoids <- data.frame()
        
        print( listAllReparto[[match(cycleReparto, unique(tsPromo$REPARTO))]][[ match( cycleSettore , unique(tsPromo[ which( tsPromo$REPARTO == cycleReparto ) , ]$SETTORE) )]][[ match(cycleGruppo,unique(tsPromo[which(tsPromo$REPARTO == cycleReparto & tsPromo$SETTORE == cycleSettore),]$GRUPPO)) ]][[5]][[ match(cycleGruppo,unique(tsPromo[which(tsPromo$REPARTO == cycleReparto & tsPromo$SETTORE == cycleSettore),]$GRUPPO)) ]] )
        
        clusterVectorForGraph <- append(clusterVectorForGraph, listAllReparto[[match(cycleReparto, unique(tsPromo$REPARTO))]][[ match( cycleSettore , unique(tsPromo[ which( tsPromo$REPARTO == cycleReparto ) , ]$SETTORE) )]][[ match(cycleGruppo,unique(tsPromo[which(tsPromo$REPARTO == cycleReparto & tsPromo$SETTORE == cycleSettore),]$GRUPPO)) ]][[5]][[ match(cycleGruppo,unique(tsPromo[which(tsPromo$REPARTO == cycleReparto & tsPromo$SETTORE == cycleSettore),]$GRUPPO)) ]]  )
        
        
        clusterNo<- unique(clusterVectorForGraph)
        clusterNo <- sort(clusterNo, decreasing = FALSE)
        
        clusterCentroids <- unname(clusterCentroids)
        clusterMedoids <- unname(clusterMedoids)
        
        
        
        # 
        # print(match(deptSelected, deptNumbersSecond))
        # print(max(clusterNo))
        # print(listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[1]][[max(clusterNo)]]$prototypes)
        
        for(clst in clusterNo){
          # print(clst)
          clusterMedoids <- rbind(clusterMedoids, (colMeans(appoggioDepts2[which(appoggioDepts2$REPARTO == cycleReparto & 
                                                                                   appoggioDepts2$SETTORE == cycleSettore &
                                                                                   appoggioDepts2$GRUPPO == cycleGruppo & 
                                                                                   appoggioDepts2$clusterVector2 == as.numeric(clst)) ,8:ncol(appoggioDepts2)])))
        }
        
        
        # clusterCentroids<- rbind(clusterCentroids, unname(listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[1]][[max(clusterNo)]]$prototypes ))
        
        # names(clusterMedoids)<- names(clusterCentroids)
        
        clusterMedoids <- clusterMedoids[,0:ncol(reversedStoreForCluster2017)]
        # print(clusterMedoids)
        
        # clusterCentroids <- clusterCentroids[,0:ncol(reversedStoreForCluster2017)]
        
        
        colnames(clusterMedoids) <- 1:ncol(clusterMedoids)
        names(reversedStoreForCluster2017) <-  names(clusterMedoids)
        colnames(reversedStoreForCluster2017) <- colnames(clusterMedoids)
        
        # reversedStoreForCluster2017 <- unname(reversedStoreForCluster2017)
        reversedStoreForCluster2017 <- reversedStoreForCluster2017[(nrow(reversedStoreOrderedByWeek2017)+1):nrow(reversedStoreForCluster2017),]
        class(reversedStoreForCluster2017) <- "numeric"
        # reversedStoreForCluster2017 <- rbind (reversedStoreForCluster2017,clusterCentroids)
        reversedStoreForCluster2017 <- rbind (reversedStoreForCluster2017,clusterMedoids)
        reversedStoreForCluster2017 <- as.matrix(reversedStoreForCluster2017)
        
        
        
        cosineDistanceMatrix2017 <- matrix(nrow=nrow(reversedStoreForCluster2017),ncol=nrow(reversedStoreForCluster2017))
        
        for (x in 1:(nrow(reversedStoreForCluster2017)-max(clusterNo))){
          
          for(y in 6: nrow(reversedStoreForCluster2017)){
            
            cosineDistanceMatrix2017[x ,y ] <- (1 - ( (reversedStoreForCluster2017[x,] %*% reversedStoreForCluster2017[y,]) /
                                                        (sqrt((reversedStoreForCluster2017[x,]%*%reversedStoreForCluster2017[x,]) * (reversedStoreForCluster2017[y,]%*%reversedStoreForCluster2017[y,]) ))))
            
          }
        }
        
        cosineDistanceMatrix2017 <- na.omit(cosineDistanceMatrix2017[,colSums(is.na(cosineDistanceMatrix2017))<nrow(cosineDistanceMatrix2017)])
        
        clusterVector2017 <- append(clusterVector2017, c(which.min(cosineDistanceMatrix2017[1,]) , which.min(cosineDistanceMatrix2017[2,]) , which.min(cosineDistanceMatrix2017[3,]) , 
                                                         which.min(cosineDistanceMatrix2017[4,]) , which.min(cosineDistanceMatrix2017[5,])) )
        print(clusterVector2017)
        
      }
    }
  }
}



tabelladipartenzaCluster<- selectedStorePromoNoDuplicate[,c(3,2,8,9,1)]
tabelladipartenzaCluster <- unique(tabelladipartenzaCluster)
tabelladateCluster <- selectedStorePromoNoDuplicate[,15]
tabelladateCluster <- unique(tabelladateCluster)
clusterDataframe2017 <- (unique(merge(x = tabelladateCluster, y = tabelladipartenzaCluster, by = NULL)))
clusterDataframe2017 <- unique(clusterDataframe2017)


colnames(clusterDataframe2017) <- c("ANNONO","REPARTO","SETTORE","GRUPPO","FAMIGLIA","ENTE")

clusterDataframe2017 <-merge(x = clusterDataframe2017, y = tsPromo[,c(1:5)],  by=c("REPARTO","SETTORE","GRUPPO","FAMIGLIA","ANNONO"))

clusterDataframe2017 <- sqldf("SELECT REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,ENTE  FROM clusterDataframe2017 order by REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,ENTE")

clusterDataframe2017 <- unique(clusterDataframe2017)


clusterDataframe2017 <- clusterDataframe2017[clusterDataframe2017$ANNONO == as.numeric("2017"), ]

clusterDataframe2017 <- cbind(clusterDataframe2017,clusterVector2017)

colnames(clusterDataframe2017) <- colnames(clusterDepts2)

clusterDataframeAllin <- rbind(clusterDepts2,clusterDataframe2017)


clusterDataframeAllin <- sqldf("SELECT *  FROM clusterDataframeAllin order by REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,ENTE")


# ################################################################################SIAMO ARRIVATI QUA##########

# View(clusterDeptsAllin)

appoggioDepts2017 <- data.frame()



for(currentReparto in unique(clusterDataframeAllin$REPARTO) ){
  
  for(currentSettore in unique(clusterDataframeAllin[which(clusterDataframeAllin$REPARTO == currentReparto),]$SETTORE)){
    
    for(currentGruppo in unique(clusterDataframeAllin[which(clusterDataframeAllin$SETTORE == currentSettore),]$GRUPPO)){
      
      for(currentFamiglia in unique(clusterDataframeAllin[which(clusterDataframeAllin$SETTORE == currentSettore & clusterDataframeAllin$GRUPPO == currentGruppo),]$FAMIGLIA)){
        
        for(currentAnno in unique(clusterDataframeAllin[which(clusterDataframeAllin$SETTORE == currentSettore & clusterDataframeAllin$GRUPPO == currentGruppo & clusterDataframeAllin$FAMIGLIA == currentFamiglia),]$ANNONO ) ) {
          # print(currentAnno)
          for(currentEnte in unique(clusterDataframeAllin$ENTE)){
            
            
            
            appoggioDepts2017 <- rbind.fill(appoggioDepts2017, cbind(clusterDataframeAllin[which(clusterDataframeAllin$REPARTO == currentReparto &
                                                                                                   clusterDataframeAllin$SETTORE == currentSettore &
                                                                                                   clusterDataframeAllin$GRUPPO == currentGruppo &
                                                                                                   clusterDataframeAllin$FAMIGLIA == currentFamiglia &
                                                                                                   clusterDataframeAllin$ANNONO == currentAnno &
                                                                                                   clusterDataframeAllin$ENTE == currentEnte),
                                                                                           ],
                                                                     t(tsPromo[which(tsPromo$REPARTO == currentReparto &
                                                                                       tsPromo$SETTORE == currentSettore &
                                                                                       tsPromo$GRUPPO == currentGruppo &
                                                                                       tsPromo$FAMIGLIA == currentFamiglia &
                                                                                       tsPromo$ANNONO == currentAnno) ,
                                                                               (match(currentEnte,unique(clusterDataframe2$ENTE)) + 6) ] ) ) )
            
          }
        }
      }
    }
  }
  
}



listAllCLustering <- list()

# clusterCurrent <- 6
repartoSelected <- 1
settoreSelected <- 10
gruppoSelected <- 5


for(gruppoSelected in unique(appoggioDepts2017[which(appoggioDepts2017$REPARTO == repartoSelected & appoggioDepts2017$SETTORE == settoreSelected),]$GRUPPO)){
  print(gruppoSelected)
  
  
  # View(appoggioDepts2017)
  # appoggioDepts2017 <- (appoggioDepts2017[with(appoggioDepts2017, order(REPARTO, ANNONO , ENTE)), ])
  # appoggioDepts2017 <- appoggioDepts2017[,0:22]
  
  clusterList <- unique(appoggioDepts2017[which(appoggioDepts2017$REPARTO == repartoSelected &
                                                  appoggioDepts2017$SETTORE == settoreSelected &
                                                  appoggioDepts2017$GRUPPO == gruppoSelected &
                                                  appoggioDepts2017$ANNONO == 2017) , ]$clusterVector2 )
  
  for(clusterCurrent in clusterList){
    
    dataModel <-(appoggioDepts2017[which(appoggioDepts2017$REPARTO == repartoSelected &
                                           appoggioDepts2017$SETTORE == settoreSelected &
                                           appoggioDepts2017$GRUPPO == gruppoSelected &
                                           appoggioDepts2017$clusterVector2 == clusterCurrent) , c(5,8:ncol(appoggioDepts2017))])
    
    
    dataModel <- t(sqldf("SELECT * FROM dataModel ORDER BY ANNONO"))
    
    class(dataModel) <- "numeric"
    dataModel <- dataModel[-1,]
    
    colnames(dataModel) <- 1:ncol(dataModel)
    
    if(length( colnames(dataModel)[colSums(is.na(dataModel)) > 0] ) != 0){
      
      training <- data.frame(1:23)
      
      # SELEZIONO COLONNE DI TRAINING
      for(colAssa in colnames(dataModel)[colSums(is.na(dataModel)) == 0]){
        
        
        training<- cbind(training, as.matrix(dataModel[1:23,colAssa])  )
        
      }
      
      training <- t(training[,-1])
      
      
      test <- data.frame(1:23)
      
      for(colAssa in colnames(dataModel)[colSums(is.na(dataModel)) > 0]){
        
        test<- cbind(test, as.matrix(dataModel[1:23,colAssa])  )
        
      }
      
      test <- t(test[,-1])
      
      rownames(training) <- NULL
      colnames(training) <- NULL
      
      
      rownames(test) <- NULL
      colnames(test) <- NULL
      #fine creazione del test set
      
      bootControl <- trainControl(number=15)    #definisce il comportamento di apprendimento (k-folds cross validation?)
      preProc <- c("center","scale")                #imposta i parametri per il preprocessing
      #setta uno scenario casuale
      # indexTrn <- ncol(training)                    # ???
      
      #creazione del modello di apprendimento
      #
      print("train")
      svmFit <- train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", tuneLength=100, trnControl=bootControl)
      # svmFit <- train(training[,-indexTrn],training[,indexTrn],method="svmRadial",tuneLength=15, trnControl=bootControl,preProcess = preProc ) 
      svmBest <-svmFit$finalModel    #modello migliore trovato con i parametri forniti
      
      
      if(nrow(test) == 1){
        
        test <- rbind(test,test)
      }
      
      predsvm <- predict(svmBest, test[,-ncol(test)])
      actualTS <- test[,ncol(test)]
      
      predictedTS <- predsvm
      print(predsvm)
      
      
      
      
      # plot(training[1,],type="b",col="blue", ylim=c(0,2000))
      # lines(training[2,],type="b",col="black")
      # lines(training[3,],type="b",col="green")
      # lines(training[4,],type="b",col="orange")
      # lines(training[5,],type="b",col="red")
      # 
      # 
      # 
      # plot(test[1,],type="b",col="blue", ylim=c(0,2000))
      # lines(test[2,],type="b",col="black")
      # lines(test[3,],type="b",col="green")
      # lines(test[4,],type="b",col="orange")
      # lines(test[5,],type="b",col="red")
      
      
      test[,ncol(test)] <- predictedTS
      test <- cbind(test,0)
      
      
      
      
      
      
      cbind(actualTS,predictedTS)
      par(mfrow = c(1, 1))
      
      
      
      training <- data.frame(1:24)
      
      
      
      # SELEZIONO COLONNE DI TRAINING
      for(colAssa in colnames(dataModel)[colSums(is.na(dataModel)) == 0]){
        
        
        training<- cbind(training, as.matrix(dataModel[1:24,colAssa])  )
        
      }
      
      training <- t(training[,-1])
      
      rownames(training) <- NULL
      colnames(training) <- NULL
      
      
      rownames(test) <- NULL
      colnames(test) <- NULL
      
      print("train 2")
      svmFit <- train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", tuneLength=100, trnControl=bootControl)
      svmBest <-svmFit$finalModel    #modello migliore trovato con i parametri forniti
      predsvm <- predict(svmBest, test[,-ncol(test)])
      
      predictedTS <- predsvm
      print(predsvm)
      
      
      
      test[,ncol(test)] <- predictedTS
      test <- cbind(test,0)
      
      
      training <- data.frame(1:25)
      
      # SELEZIONO COLONNE DI TRAINING
      for(colAssa in colnames(dataModel)[colSums(is.na(dataModel)) == 0]){
        
        training<- cbind(training, as.matrix(dataModel[1:25,colAssa])  )
        
      }
      
      training <- t(training[,-1])
      rownames(training) <- NULL
      colnames(training) <- NULL
      
      
      rownames(test) <- NULL
      colnames(test) <- NULL
      
      print("train 3")
      svmFit <- train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", tuneLength=100, trnControl=bootControl)
      svmBest <-svmFit$finalModel    #modello migliore trovato con i parametri forniti
      predsvm <- predict(svmBest, test[,-ncol(test)])
      predictedTS <- predsvm
      print(predsvm)
      
      
      test[,ncol(test)] <- predictedTS
      
      
      for(colAssa in colnames(dataModel)[colSums(is.na(dataModel)) > 0]){
        
        test <- rbind(test,t(na.omit(data.frame(dataModel[1:25,colAssa])) ) )
        
      }
      
      
      listAllCLustering[[gruppoSelected]] <- test
      
    }else{
      print("0 test")
    }
  }
}

plot(listAllCLustering[[1]][1,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[1]][1,],listAllCLustering[[1]][6,])))
lines(listAllCLustering[[1]][6,],type="b",col="red")

plot(listAllCLustering[[1]][2,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[1]][2,],listAllCLustering[[1]][7,])))
lines(listAllCLustering[[1]][7,],type="b",col="red")

plot(listAllCLustering[[1]][3,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[1]][3,],listAllCLustering[[1]][8,])))
lines(listAllCLustering[[1]][8,],type="b",col="red")

plot(listAllCLustering[[1]][4,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[1]][4,],listAllCLustering[[1]][9,])))
lines(listAllCLustering[[1]][9,],type="b",col="red")

plot(listAllCLustering[[1]][5,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[1]][5,],listAllCLustering[[1]][10,])))
lines(listAllCLustering[[1]][10,],type="b",col="red")




plot(listAllCLustering[[2]][1,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[2]][1,],listAllCLustering[[2]][13,])))
lines(listAllCLustering[[2]][13,],type="b",col="red")

plot(listAllCLustering[[2]][2,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[2]][2,],listAllCLustering[[2]][14,])))
lines(listAllCLustering[[2]][14,],type="b",col="red")

plot(listAllCLustering[[2]][3,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[2]][3,],listAllCLustering[[2]][15,])))
lines(listAllCLustering[[2]][15,],type="b",col="red")

plot(listAllCLustering[[2]][4,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[2]][4,],listAllCLustering[[2]][16,])))
lines(listAllCLustering[[2]][16,],type="b",col="red")



plot(listAllCLustering[[4]][1,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[4]][1,],listAllCLustering[[4]][13,])))
lines(listAllCLustering[[4]][13,],type="b",col="red")

plot(listAllCLustering[[4]][3,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[4]][3,],listAllCLustering[[4]][15,])))
lines(listAllCLustering[[4]][15,],type="b",col="red")

plot(listAllCLustering[[4]][5,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[4]][5,],listAllCLustering[[4]][17,])))
lines(listAllCLustering[[4]][17,],type="b",col="red")

plot(listAllCLustering[[4]][7,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[4]][7,],listAllCLustering[[4]][19,])))
lines(listAllCLustering[[4]][19,],type="b",col="red")

plot(listAllCLustering[[4]][9,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[4]][9,],listAllCLustering[[4]][21,])))
lines(listAllCLustering[[4]][21,],type="b",col="red")



plot(listAllCLustering[[5]][1,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[5]][1,],listAllCLustering[[5]][6,])))
lines(listAllCLustering[[5]][6,],type="b",col="red")

plot(listAllCLustering[[5]][2,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[5]][2,],listAllCLustering[[5]][7,])))
lines(listAllCLustering[[5]][7,],type="b",col="red")

plot(listAllCLustering[[5]][3,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[5]][3,],listAllCLustering[[5]][8,])))
lines(listAllCLustering[[5]][8,],type="b",col="red")

plot(listAllCLustering[[5]][4,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[5]][4,],listAllCLustering[[5]][9,])))
lines(listAllCLustering[[5]][9,],type="b",col="red")

plot(listAllCLustering[[5]][5,],type="b",col="blue", ylim=c(0,max(listAllCLustering[[5]][5,],listAllCLustering[[5]][10,])))
lines(listAllCLustering[[5]][10,],type="b",col="red")

#   
