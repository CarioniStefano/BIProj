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


tempDept2 <- tsPromo[which(tsPromo$REPARTO == 1 & tsPromo$SETTORE == 10 & tsPromo$GRUPPO == 1),]


for (currentFamily in unique(tempDept2$FAMIGLIA) ) {
  
  
  
  reversedStoreOrderedByWeek2017 <- t( tempDept2[which(tempDept2$FAMIGLIA==currentFamily & tempDept2$ANNONO== 2017),] )
  
  reversedStoreForCluster2017 <- reversedStoreOrderedByWeek2017
  
  for(currentEnte in unique(clusterDataframe2$ENTE)){
    #print((match(storeNo,storesNumbersSecond)))
    reversedStoreForCluster2017<- rbind(reversedStoreForCluster2017 , reversedStoreOrderedByWeek2017[(6+(match(currentEnte,unique(clusterDataframe2$ENTE)))),])
    
  }
  
  clusterVectorForGraph <- c()
  clusterCentroids <- data.frame()
  clusterMedoids <- data.frame()
  
  
  clusterVectorForGraph <- append(clusterVectorForGraph, listAllReparto[[1]][[1]][[1]][[5]][[1]] )
  

  clusterNo<- unique(clusterVectorForGraph)
  clusterNo <- sort(clusterNo, decreasing = FALSE)
  
   clusterCentroids <- unname(clusterCentroids)
  clusterMedoids <- unname(clusterMedoids)
  
  
  
  # 
  # print(match(deptSelected, deptNumbersSecond))
  # print(max(clusterNo))
  # print(listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[1]][[max(clusterNo)]]$prototypes)
  
  for(clst in clusterNo){
    clusterMedoids <- rbind(clusterMedoids, (colMeans(appoggioDepts2[which(appoggioDepts2$REPARTO == 1 & 
                                                                             appoggioDepts2$SETTORE == 10 & appoggioDepts2$GRUPPO == 1 & 
                                                                             appoggioDepts2$clusterVector2 == as.numeric(clst)) ,8:ncol(appoggioDepts2)])))
  }
  
  
  # clusterCentroids<- rbind(clusterCentroids, unname(listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[1]][[max(clusterNo)]]$prototypes ))
  
  # names(clusterMedoids)<- names(clusterCentroids)
  
  clusterMedoids <- clusterMedoids[,0:ncol(reversedStoreForCluster2017)]
  
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
    
    for(y in 7: nrow(reversedStoreForCluster2017)){
      
      cosineDistanceMatrix2017[x ,y ] <- (1 - ( (reversedStoreForCluster2017[x,] %*% reversedStoreForCluster2017[y,]) /
                                                  (sqrt((reversedStoreForCluster2017[x,]%*%reversedStoreForCluster2017[x,]) * (reversedStoreForCluster2017[y,]%*%reversedStoreForCluster2017[y,]) ))))
      
    }
  }
  
  cosineDistanceMatrix2017 <- na.omit(cosineDistanceMatrix2017[,colSums(is.na(cosineDistanceMatrix2017))<nrow(cosineDistanceMatrix2017)])
  
  clusterVector2017 <- append(clusterVector2017, c(which.min(cosineDistanceMatrix2017[1,]) , which.min(cosineDistanceMatrix2017[2,]) , which.min(cosineDistanceMatrix2017[3,]) , 
                                                   which.min(cosineDistanceMatrix2017[4,]) , which.min(cosineDistanceMatrix2017[5,])) )
  print(clusterVector2017)
  
}



colors = c("red", "red", "red", "red", "red","cyan2","green","black","cyan2","yellow","darkgreen","lightpink")

xrange <- range(1, ncol(reversedStoreForCluster2017))
# yrange <- range(c(0, na.omit(max(tempDept[,7:ncol(tempDept)]))))
yrange <- range(c(0, 150000))

plot(xrange,
     yrange,
     type = "n",
     xlab = "WEEKNO",
     ylab = "WeeklySales",main="banana")

for(lineeeee in 1:nrow(reversedStoreForCluster2017)){
  
  lines(1:ncol(reversedStoreForCluster2017) ,reversedStoreForCluster2017[lineeeee,],
        lty = 1,
        lwd = 2,
        col = colors[lineeeee], #+1 perch? indice parte da 1
        pch = pchDot
  )
}















tabelladipartenzaCluster<- selectedStorePromoNoDuplicate[,c(3,2,8,9,1)]
tabelladipartenzaCluster <- unique(tabelladipartenzaCluster)
tabelladateCluster <- selectedStorePromoNoDuplicate[,15]
tabelladateCluster <- unique(tabelladateCluster)
clusterDataframe2017 <- (unique(merge(x = tabelladateCluster, y = tabelladipartenzaCluster, by = NULL)))
clusterDataframe2017 <- unique(clusterDataframe2017)


colnames(clusterDataframe2017) <- c("ANNONO","REPARTO","SETTORE","GRUPPO","FAMIGLIA","ENTE")



clusterDataframe2017 <-merge(x = clusterDataframe2017, y = tsPromo[,c(1:5)],  by=c("REPARTO","SETTORE","GRUPPO","FAMIGLIA","ANNONO"))

clusterDataframe2017 <- sqldf("SELECT REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,ENTE  FROM clusterDataframe2017 where REPARTO=1 and SETTORE=10 and GRUPPO=1 order by REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,ENTE")

clusterDataframe2017 <- unique(clusterDataframe2017)


clusterDataframe2017 <- clusterDataframe2017[clusterDataframe2017$ANNONO == as.numeric("2017"), ]

clusterDataframe2017 <- cbind(clusterDataframe2017,clusterVector2017)

colnames(clusterDataframe2017) <- colnames(clusterDepts2)

clusterDataframeAllin <- rbind(clusterDepts2,clusterDataframe2017)


clusterDataframeAllin <- sqldf("SELECT *  FROM clusterDataframeAllin order by REPARTO,SETTORE,GRUPPO,FAMIGLIA,ANNONO,ENTE")



# ################################################################################SIAMO ARRIVATI QUA##########

# View(clusterDeptsAllin)

appoggioDepts2017 <- data.frame()



for(repartoCurrent in unique(clusterDeptsAllin$REPARTO)){
  
  for(enteCurrent in storesNumbersSecond){
    
    for(annoCurrent in unique(clusterDeptsAllin$ANNONO)){   
      
      appoggioDepts2017 <- rbind.fill(appoggioDepts2017, cbind(clusterDeptsAllin[which( as.numeric(as.character(clusterDeptsAllin$REPARTO)) == as.numeric(repartoCurrent) & as.numeric(as.character(clusterDeptsAllin$ANNONO)) == as.numeric(annoCurrent) & as.numeric(as.character(clusterDeptsAllin$ENTE)) == as.numeric(enteCurrent)   ),
                                                                                 ],
                                                               t(storeOrderedByWeek2[which( as.numeric(storeOrderedByWeek2$REPARTO) == as.numeric(repartoCurrent) & as.numeric(storeOrderedByWeek2$ANNONO) == as.numeric(annoCurrent) ) ,
                                                                                     (match(enteCurrent,storesNumbersSecond) +3)])) )
      
    }
    
  }
  
  
  
}

clusterCurrent <- 2
deptSelected <- 6

# View(appoggioDepts2017)
appoggioDepts2017 <- (appoggioDepts2017[with(appoggioDepts2017, order(REPARTO, ANNONO , ENTE)), ])
# appoggioDepts2017 <- appoggioDepts2017[,0:22]


assa <-t(appoggioDepts2017[which(as.numeric(as.character(appoggioDepts2017$REPARTO)) == as.numeric(deptSelected) &
                                   as.numeric(as.character(appoggioDepts2017$clusterVector)) == as.numeric(clusterCurrent)),5:ncol(appoggioDepts2017)])


predictData2 <- data.frame(1:22)


for(colAssa in c(1:6)){
  
  predictData2<- cbind(predictData2, as.matrix(assa[1:22,colAssa])  )
  
}

predictData2 <- t(predictData2[,-1])

# colnames(predictData2) <- c("nolag","lag1","lag2")

test <- t(na.omit(data.frame(assa[1:22,7])))

test <- rbind(test,t(na.omit(data.frame(assa[1:22,8]))))


test <- rbind(test,t(na.omit(data.frame(assa[1:22,9]))))

test <- rbind(test,t(na.omit(data.frame(assa[1:22,10]))))

test <- rbind(test,t(na.omit(data.frame(assa[1:22,11]))))
test <- rbind(test,t(na.omit(data.frame(assa[1:22,12]))))


# predictFrame <- (c (as.vector(as.matrix(assa[,(1:6)])) , na.omit(as.vector(as.matrix(assa[,9])))))
# predictInput <- cbind(shift(predictFrame,-1),shift(predictFrame,-2),predictFrame)
# predictData <- predictInput[complete.cases(predictInput),]
# colnames(predictData2) <- c("nolag","lag1","lag2")

# trainIndex <- 1:(nrow(predictData2))
training <- predictData2
rownames(training) <- NULL
colnames(training) <- NULL
#fine creazione del training set

#inizio creazione del test set
# test <- as.data.frame(predictData[-trainIndex,])

rownames(test) <- NULL
colnames(test) <- NULL
#fine creazione del test set

bootControl <- trainControl(number=15)    #definisce il comportamento di apprendimento (k-folds cross validation?)
preProc <- c("center","scale")                #imposta i parametri per il preprocessing
set.seed(2)                                #setta uno scenario casuale
indexTrn <- ncol(training)                    # ???

#creazione del modello di apprendimento
#
svmFit <- train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", tuneLength=100, trnControl=bootControl)
# svmFit <- train(training[,-indexTrn],training[,indexTrn],method="svmRadial",tuneLength=15, trnControl=bootControl,preProcess = preProc ) 
svmBest <-svmFit$finalModel    #modello migliore trovato con i parametri forniti
predsvm <- predict(svmBest, test[,-ncol(test)])
actualTS <- test[,ncol(test)]
predictedTS <- predsvm


test[,ncol(test)] <- predictedTS
test <- cbind(test,0)

cbind(actualTS,predictedTS)
par(mfrow = c(1, 1))


predictData2 <- data.frame(1:23)


for(colAssa in c(1:6)){
  
  predictData2<- cbind(predictData2, as.matrix(assa[1:23,colAssa])  )
  
}

predictData2 <- t(predictData2[,-1])

training <- predictData2

svmFit <- train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", tuneLength=100, trnControl=bootControl)
svmBest <-svmFit$finalModel    #modello migliore trovato con i parametri forniti
predsvm <- predict(svmBest, test[,-ncol(test)])

predictedTS <- predsvm



test[,ncol(test)] <- predictedTS
test <- cbind(test,0)


predictData2 <- data.frame(1:24)


for(colAssa in c(1:6)){
  
  predictData2<- cbind(predictData2, as.matrix(assa[1:24,colAssa])  )
  
}

predictData2 <- t(predictData2[,-1])

training <- predictData2

svmFit <- train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial", tuneLength=100, trnControl=bootControl)
svmBest <-svmFit$finalModel    #modello migliore trovato con i parametri forniti
predsvm <- predict(svmBest, test[,-ncol(test)])
predictedTS <- predsvm

test[,ncol(test)] <- predictedTS
test <- rbind(test,t(na.omit(data.frame(assa[1:24,7])) ) ) 
test <- rbind(test,t(na.omit(data.frame(assa[1:24,8])) ) ) 
test <- rbind(test,t(na.omit(data.frame(assa[1:24,9])) ) ) 
test <- rbind(test,t(na.omit(data.frame(assa[1:24,10])) ) ) 
test <- rbind(test,t(na.omit(data.frame(assa[1:24,11])) ) ) 
test <- rbind(test,t(na.omit(data.frame(assa[1:24,12])) ) ) 


rmse(actual = test[7,22:(ncol(test)-1)], predicted = test[1,22:(ncol(test)-1)] )
rmse(actual = test[8,22:(ncol(test)-1)], predicted = test[2,22:(ncol(test)-1)] )
rmse(actual = test[9,22:(ncol(test)-1)], predicted = test[3,22:(ncol(test)-1)] )
rmse(actual = test[10,22:(ncol(test)-1)], predicted = test[4,22:(ncol(test)-1)] )
rmse(actual = test[11,22:(ncol(test)-1)], predicted = test[5,22:(ncol(test)-1)] )
rmse(actual = test[12,22:(ncol(test)-1)], predicted = test[6,22:(ncol(test)-1)] )

totBef <- mean(abs(test[7,22]-test[1,22])/(test[7,22]),abs(test[7,23]-test[1,23])/(test[7,23]))+mean(abs(test[8,22]-test[2,22])/(test[8,22]),abs(test[8,23]-test[2,23])/(test[8,23]))+mean(abs(test[9,22]-test[3,22])/(test[9,22]),abs(test[9,23]-test[3,23])/(test[9,23]))+mean(abs(test[10,22]-test[4,22])/(test[10,22]),abs(test[10,23]-test[4,23])/(test[10,23]))+mean(abs(test[11,22]-test[5,22])/(test[11,22]),abs(test[11,23]-test[5,23])/(test[11,23]))+mean(abs(test[12,22]-test[6,22])/(test[12,22]),abs(test[12,23]-test[6,23])/(test[12,23]))
totBef/6


mean(rowMeans(abs((test[7:12,22:23]-test[1:6,22:23])/test[7:12,22:23])*100))
plot(test[1,],type="b",col="blue", ylim=c(0,max(test[1,],test[7,])))
lines(test[7,],type="b",col="red")

plot(test[2,],type="b",col="blue", ylim=c(0,max(test[2,],test[8,])))
lines(test[8,],type="b",col="red")

plot(test[3,],type="b",col="blue", ylim=c(0,max(test[3,],test[9,])))
lines(test[9,],type="b",col="red")

plot(test[4,],type="b",col="blue", ylim=c(0,max(test[4,],test[10,])))
lines(test[10,],type="b",col="red")

plot(test[5,],type="b",col="blue", ylim=c(0,max(test[5,],test[11,])))
lines(test[11,],type="b",col="red")

plot(test[6,],type="b",col="blue", ylim=c(0,max(test[6,],test[12,])))
lines(test[12,],type="b",col="red")


