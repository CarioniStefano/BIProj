# par(mfrow = c(1, 1))
# due possibilità
# quell'ente per questo reparto nel 2017 a che cluster assomoglia?
# dovresti porendere tutti i cluster che ho, tagliare le 52 colonne a 17, calcolo il prototipo (media, centroidi calcolati sui 17) quale centroide è più simile al 2017?
# confornto i prototype con distanza coseno
# prendo i prototipi dei cluster, calcolo la distanza coseno 

require(zoo)

set.seed(4844)

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




appoggioDeptsPred <- appoggioDepts2017[,c(1:4,6:7)]
appoggioDeptsPred <- unique(appoggioDeptsPred)


# clusterCurrent <- 6
# repartoSelected <- 1
# settoreSelected <- 10
# gruppoSelected <- 1

for(repartoSelected in unique(appoggioDepts2017$REPARTO)){
  print(paste("REPARTO",repartoSelected))
  
  
  for(settoreSelected in unique(appoggioDepts2017[which(appoggioDepts2017$REPARTO == repartoSelected),]$SETTORE)){
    
    print(paste("Settore",settoreSelected))
    
    for(gruppoSelected in unique(appoggioDepts2017[which(appoggioDepts2017$REPARTO == repartoSelected & appoggioDepts2017$SETTORE == settoreSelected),]$GRUPPO)){
      print(paste("gruppo",gruppoSelected))
      
      
      # View(appoggioDepts2017)
      # appoggioDepts2017 <- (appoggioDepts2017[with(appoggioDepts2017, order(REPARTO, ANNONO , ENTE)), ])
      # appoggioDepts2017 <- appoggioDepts2017[,0:22]
      
      
      
      clusterList <- unique(appoggioDepts2017[which(appoggioDepts2017$REPARTO == repartoSelected &
                                                      appoggioDepts2017$SETTORE == settoreSelected &
                                                      appoggioDepts2017$GRUPPO == gruppoSelected &
                                                      appoggioDepts2017$ANNONO == 2017) , ]$clusterVector2 )
      print(clusterList)
      for(clusterCurrent in clusterList){
        print(paste("cluster",clusterCurrent))
        
        
        
        dataModel <-(appoggioDepts2017[which(appoggioDepts2017$REPARTO == repartoSelected &
                                               appoggioDepts2017$SETTORE == settoreSelected &
                                               appoggioDepts2017$GRUPPO == gruppoSelected &
                                               appoggioDepts2017$clusterVector2 == clusterCurrent) , c(4:6,8:ncol(appoggioDepts2017))])
        
        
        
        
        
        
        
        
        dataModel <- t(sqldf("SELECT * FROM dataModel ORDER BY ANNONO"))
        
        dataModel <- dataModel[-2,]
        
        dataModelFamAndEnte <- dataModel[1:2,]
        dataModel <- dataModel [-c(1,2),]
        
        class(dataModel) <- "numeric"
        colnames(dataModel) <- 1:ncol(dataModel)
        
        
        # class(dataModel) <- "numeric"
        
        
        # colnames(dataModel) <- 1:ncol(dataModel)
        # C'è CORRISPONDENZA TRA LA COLONNA DEL DATA MODEL E LA RIGA DATAMODELFAMANENTE'
        if(length( colnames(dataModel)[colSums(is.na(dataModel)) > 0] ) != 0){
          
          training <- data.frame(1:23)
          
          # SELEZIONO COLONNE DI TRAINING
          for(colAssa in colnames(dataModel)[colSums(is.na(dataModel)) == 0]){
            
            
            training<- cbind(training, as.matrix(dataModel[1:23,colAssa])  )
            
          }
          
          training <- t(training[,-1])
          if(nrow(training) > 1){
            
            
            
            if( (sum(training[,ncol(training)]) == 0) ){
              training[,ncol(training)] <- index(training)*1 
            }
            
            
            test <- data.frame(1:23)
            testNames <- data.frame(1:2)
            # SELEZIONO COLONNE DI TEST
            for(colAssa in colnames(dataModel)[colSums(is.na(dataModel)) > 0]){
              
              test<- cbind(test, as.matrix(dataModel[1:23,colAssa])  )
              testNames <- cbind(testNames, dataModelFamAndEnte[,as.numeric(colAssa)])
              
            }
            
            test <- t(test[,-1])
            testNames <- testNames[,-1]
            
            rownames(training) <- NULL
            colnames(training) <- NULL
            
            
            rownames(test) <- NULL
            colnames(test) <- NULL
            #fine creazione del test set
            trControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10, savePredictions = T)
            # bootControl <- trainControl(number=15)    #definisce il comportamento di apprendimento (k-folds cross validation?)
            preProc <- c("center","scale")                #imposta i parametri per il preprocessing
            #setta uno scenario casuale
            # indexTrn <- ncol(training)                    # ???
            
            #creazione del modello di apprendimento
            #
            print("train")
            mode(training) = "numeric"
            
            
            
            svmFit <- 0
            try({svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", trControl=trControl,tuneLength = 10)})
            
            
            if (class(svmFit) == "numeric"){
              try({svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", trControl=trControl,tuneLength = 10)})
            }
            # svmFit <- train(training[,-indexTrn],training[,indexTrn],method="svmRadial",tuneLength=15, trnControl=bootControl,preProcess = preProc ) 
            svmBest <-svmFit$finalModel    #modello migliore trovato con i parametri forniti
            
            
            if(nrow(test) == 1){
              
              test <- rbind(test,test)
            }
            
            predsvm <- predict(svmBest, test[,-ncol(test)])
            actualTS <- test[,ncol(test)]
            
            predictedTS <- predsvm
            print(predsvm)
            
            
            
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
            
            if(( any(colSums(training) == 0) )){
              training[,ncol(training)] <- index(training)*1 
            }
            
            rownames(training) <- NULL
            colnames(training) <- NULL
            
            
            
            
            rownames(test) <- NULL
            colnames(test) <- NULL
            trControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10, savePredictions = T)
            print("train 2")
            mode(training) = "numeric"
            svmFit <- 0
            try({svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", trControl=trControl,tuneLength = 10)})
            
            
            if (class(svmFit) == "numeric"){
              try({svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", trControl=trControl,tuneLength = 10)})
            }
            # svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", trControl=trControl,tuneLength = 13)
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
            
            if(( any(colSums(training) == 0) )){
              training[,ncol(training)] <- index(training)*1 
            }
            
            
            rownames(training) <- NULL
            colnames(training) <- NULL
            
            
            rownames(test) <- NULL
            colnames(test) <- NULL
            
            print("train 3")
            trControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10, savePredictions = T)
            mode(training) = "numeric"
            svmFit <- 0
            try({svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", trControl=trControl,tuneLength = 10)})
            
            
            if (class(svmFit) == "numeric"){
              try({svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", trControl=trControl,tuneLength = 10)})
            }
            # svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", trControl=trControl,tuneLength = 13)
            svmBest <-svmFit$finalModel    #modello migliore trovato con i parametri forniti
            predsvm <- predict(svmBest, test[,-ncol(test)])
            predictedTS <- predsvm
            print(predsvm)
            
            
            test[,ncol(test)] <- predictedTS
            
            effectiveValue <- t(data.frame(1:25))
            
            for(colAssa in colnames(dataModel)[colSums(is.na(dataModel)) > 0]){
              
              effectiveValue <- rbind(effectiveValue,t(na.omit(data.frame(dataModel[1:25,colAssa])) ) )
              
            }
            effectiveValue <- effectiveValue[-1,]
            
            test <- cbind(test, t(dataModelFamAndEnte[,as.numeric(colnames(dataModel)[colSums(is.na(dataModel)) > 0] )]))
            
            test <- cbind(test, repartoSelected,settoreSelected,gruppoSelected,clusterCurrent)
            colnames(test) <- c(colnames(effectiveValue),"FAMIGLIA","ENTE","REPARTO","SETTORE","GRUPPO","clusterVector2")
            
            appoggioDeptsPred <- merge(x = appoggioDeptsPred, y = test, by=c("REPARTO","SETTORE","GRUPPO","FAMIGLIA","ENTE"), all.x = TRUE)
            
            
          }else{
            print("TRAINING INSUFFICIENTE")
          }
          
          
        }else{
          print("0 test")
        }
      }
    }
  }
}

