# par(mfrow = c(1, 1))
# due possibilità
# quell'ente per questo reparto nel 2017 a che cluster assomoglia?
# dovresti porendere tutti i cluster che ho, tagliare le 52 colonne a 17, calcolo il prototipo (media, centroidi calcolati sui 17) quale centroide è più simile al 2017?
# confornto i prototype con distanza coseno
# prendo i prototipi dei cluster, calcolo la distanza coseno 

clusterVector2017 <- c()
for (deptSelected in deptNumbersSecond) {
  
  
  tempDept <-
    appoggioDepts[(as.numeric(as.character(clusterDataframe$REPARTO)) == deptSelected),]
  
  reversedStoreOrderedByWeek2017 <- t(storeOrderedByWeek[which(storeOrderedByWeek$REPARTO==deptSelected & storeOrderedByWeek$ANNONO== 2017),])
  reversedStoreForCluster2017 <- reversedStoreOrderedByWeek2017
  
  for(storeNo in storesNumbersSecond){
    #print((match(storeNo,storesNumbersSecond)))
    reversedStoreForCluster2017<- rbind(reversedStoreForCluster2017 , reversedStoreOrderedByWeek2017[(3+(match(storeNo,storesNumbersSecond))),])
    
  }
  clusterVectorForGraph <- c()
  clusterCentroids <- data.frame()
  
  
  # 
  clusterVectorForGraph<- append(clusterVectorForGraph, listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[5]][[match(deptSelected, deptNumbersSecond)]])
  
  # clusterCentroids<- rbind(clusterCentroids, listYears[[match(yearSelected, yearList2)]][[match(deptSelected, deptNumbersSecond)]][[1]][[match(deptSelected, deptNumbersSecond)]]$prototypes)
  
  
  clusterNo<- unique(clusterVectorForGraph)
  clusterCentroids <- unname(clusterCentroids)
  
  
  
  # 
  # print(match(deptSelected, deptNumbersSecond))
  # print(max(clusterNo))
  # print(listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[1]][[max(clusterNo)]]$prototypes)
  clusterCentroids<- rbind(clusterCentroids, unname(listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[1]][[max(clusterNo)]]$prototypes ))
  
  clusterCentroids <- clusterCentroids[,0:ncol(reversedStoreForCluster2017)]
  
  names(reversedStoreForCluster2017) <- names(clusterCentroids) 
  
  reversedStoreForCluster2017 <- unname(reversedStoreForCluster2017)
  reversedStoreForCluster2017 <- reversedStoreForCluster2017[(nrow(reversedStoreOrderedByWeek2017)+1):nrow(reversedStoreForCluster2017),]
  reversedStoreForCluster2017 <- rbind (reversedStoreForCluster2017,clusterCentroids)
  reversedStoreForCluster2017 <- as.matrix(reversedStoreForCluster2017)
  
  
  
  cosineDistanceMatrix2017 <- matrix(nrow=nrow(reversedStoreForCluster2017),ncol=nrow(reversedStoreForCluster2017))
  
  for (x in 1:(nrow(reversedStoreForCluster2017)-max(clusterNo))){
    
    for(y in 7: nrow(reversedStoreForCluster2017)){
      
      cosineDistanceMatrix2017[x ,y ] <- (1 - ( (reversedStoreForCluster2017[x,] %*% reversedStoreForCluster2017[y,]) /
                                                  (sqrt((reversedStoreForCluster2017[x,]%*%reversedStoreForCluster2017[x,]) * (reversedStoreForCluster2017[y,]%*%reversedStoreForCluster2017[y,]) ))))
      
    }
  }
  
  cosineDistanceMatrix2017 <- na.omit(cosineDistanceMatrix2017[,colSums(is.na(cosineDistanceMatrix2017))<nrow(cosineDistanceMatrix2017)])
  
  clusterVector2017 <- append(clusterVector2017, c(which.min(cosineDistanceMatrix2017[1,]) , which.min(cosineDistanceMatrix2017[2,]) , which.min(cosineDistanceMatrix2017[3,]) , which.min(cosineDistanceMatrix2017[4,]) , which.min(cosineDistanceMatrix2017[5,]) , which.min(cosineDistanceMatrix2017[6,]) ))
  print(clusterVector2017)
  
}




clusterDataframe2017 <- as.data.frame( selectedStoreDeptAggregated$REPARTO)
clusterDataframe2017 <- cbind(clusterDataframe2017,selectedStoreDeptAggregated$ENTE)
clusterDataframe2017 <- cbind(clusterDataframe2017,selectedStoreDeptAggregated$ANNONO)



clusterDataframe2017 <- unique(clusterDataframe2017)


colnames(clusterDataframe2017) <- c("REPARTO","ENTE","ANNONO")



clusterDataframe2017 <- clusterDataframe2017[clusterDataframe2017$ANNONO == as.numeric("2017"), ]

clusterDataframe2017 <- (clusterDataframe2017[with(clusterDataframe2017, order(REPARTO, ANNONO , ENTE)), ])


clusterDataframeAllin <- rbind(clusterDataframe,clusterDataframe2017)
clusterDataframeAllin <- (clusterDataframeAllin[with(clusterDataframeAllin, order(REPARTO, ANNONO , ENTE)), ])






# clusterDepts <- selectedStoreDeptAggregated[which( (as.numeric(as.character(selectedStoreDeptAggregated$REPARTO)) %in% deptNumbersSecond)),]
# clusterDepts <- clusterDepts [which( (selectedStoreDeptAggregated$ANNONO %in% yearList2)),]
# 
# clusterDepts <- clusterDepts[!duplicated(clusterDepts[,c('REPARTO','ENTE','ANNONO')]),c('REPARTO','ENTE','ANNONO')]
clusterDepts2017 <- cbind(clusterDataframe2017, clusterVector2017)



colnames(clusterDepts2017) <- c("REPARTO","ENTE","ANNONO","clusterVector")
clusterDeptsAllin <- rbind(clusterDepts,clusterDepts2017)

View(clusterDeptsAllin)

appoggioDepts2017 <- data.frame()



for(repartoCurrent in unique(clusterDeptsAllin$REPARTO)){
  
  for(enteCurrent in storesNumbersSecond){
    
    for(annoCurrent in unique(clusterDeptsAllin$ANNONO)){   
      
      appoggioDepts2017 <- rbind.fill(appoggioDepts2017, cbind(clusterDeptsAllin[which( as.numeric(as.character(clusterDeptsAllin$REPARTO)) == as.numeric(repartoCurrent) & as.numeric(as.character(clusterDeptsAllin$ANNONO)) == as.numeric(annoCurrent) & as.numeric(as.character(clusterDeptsAllin$ENTE)) == as.numeric(enteCurrent)   ),
                                                               ],
                                                  t(storeOrderedByWeek[which( as.numeric(storeOrderedByWeek$REPARTO) == as.numeric(repartoCurrent) & as.numeric(storeOrderedByWeek$ANNONO) == as.numeric(annoCurrent) ) ,
                                                                       (match(enteCurrent,storesNumbersSecond) +3)])) )
      
    }
    
  }
  
  
  
}


appoggioDepts2017 <- (appoggioDepts2017[with(appoggioDepts2017, order(REPARTO, ANNONO , ENTE)), ])
appoggioDepts2017 <- appoggioDepts2017[,0:22]


