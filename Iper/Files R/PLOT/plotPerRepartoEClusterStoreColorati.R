

# per ogni cluster la time series  deve essere colorata per store, se ho 3 cluster lo store deve avere lo stesso colore

#numero di grafici per ogni reparto, uno per ogni cluster
#un grafico per ogni cluster, i colori identificano lo store





par(mfrow = c(1, 1))

for (deptSelected in deptNumbersSecond) {
  colors = c("red", "green", "blue", "black", "yellow","purple")
  
  
  # get the range for the x and y axis
  
  
  linetype <- 1
  pchDot <- 16
  
  tempDept <-
    appoggioDepts[(as.numeric(as.character(clusterDataframe$REPARTO)) == deptSelected),]
  
  
  
  
  
  tempDept2 <-tempDept[(as.numeric(as.character(tempDept$ANNONO)) %in% yearList2),]
  
  # prendere tutti gli anni cambiare match[yearSelecte,yearList due con un operazione vettoriale che prenda tutti gli year in yearlist]
  clusterVectorForGraph <- c()
  
  
  clusterVectorForGraph<- append(clusterVectorForGraph, listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[5]][[match(deptSelected, deptNumbersSecond)]])
  
  
  
  clusterNo<- unique(clusterVectorForGraph)
  
  
  
  for(clusterSelected in clusterNo){
    
    print("cluster: ")
    print(clusterSelected)
    
    xrange <- range(1, 53)
    yrange <-
      range(c(0, na.omit(max(
        appoggioDepts[which(as.numeric(as.character(appoggioDepts$REPARTO)) == deptSelected & as.numeric(appoggioDepts$clusterVector) == clusterSelected ) ,4:ncol(appoggioDepts)]
      ))))
    
    
    plot(xrange,
         yrange,
         type = "n",
         xlab = "WEEKNO",
         ylab = "WeeklySales",main=paste("valori per cluster",clusterSelected , "reparto",deptSelected,sep=" "))
    for(yearSelected2 in yearList2){
      
      for (storeSelected3 in storesNumbersSecond) {
        
        # color <- listAllDeptYear [[match(deptSelected, deptNumbersSecond)]] [[5]] [[match(deptSelected, deptNumbersSecond)]] [colCount - 3]
        
        color <- match(storeSelected3,storesNumbersSecond)
        
        tempDept3 <- tempDept2[(as.numeric(as.character(tempDept2$ANNONO))) == yearSelected2,]
        lines(
          (5:ncol(tempDept3)-4),
          tempDept3[which(as.numeric(as.character(tempDept3$REPARTO)) == deptSelected  & as.numeric(as.character(tempDept3$ANNONO)) == yearSelected2 & 
                            as.numeric(tempDept3$clusterVector) == clusterSelected &as.numeric(as.character(tempDept3$ENTE)) == storeSelected3), 5:ncol(tempDept3)],
          lty = 1,
          lwd = 2,
          col = colors[color], #+1 perch? indice parte da 1
          pch = pchDot
        )
        
        
      }
    }
    
  }
}

