# dividere in due grafifi, uno per cluster
# un altro grafico coi centroidi di ogni cluster
# per ogni cluster la time series  deve essere colorata per store, se ho 3 cluster lo store deve avere lo stesso colore

#numero di grafici per ogni reparto, uno per ogni cluster
#un grafico per ogni cluster, i colori identificano lo store






par(mfrow = c(1, 1))
for (deptSelected in deptNumbersSecond) {
  colors = c("red", "green", "blue", "black", "yellow")
  
  
  # get the range for the x and y axis
  xrange <- range(1, 53)
  yrange <- range(0,1)
  
  linetype <- 1
  pchDot <- 16
  
  tempDept <-
    clusterDataframe[(as.numeric(as.character(clusterDataframe$REPARTO)) == deptSelected),]
  
  
  
  
  
  tempDept2 <-tempDept[(as.numeric(as.character(tempDept$ANNONO)) %in% yearList2),]
  
  # prendere tutti gli anni cambiare match[yearSelecte,yearList due con un operazione vettoriale che prenda tutti gli year in yearlist]
  clusterVectorForGraph <- c()
  clusterCentroids <- data.frame()
  
  for (yearSelected in yearList2) {
    # 
    clusterVectorForGraph<- append(clusterVectorForGraph, listYears[[match(yearSelected, yearList2)]][[match(deptSelected, deptNumbersSecond)]][[5]][[match(deptSelected, deptNumbersSecond)]])
    
    # clusterCentroids<- rbind(clusterCentroids, listYears[[match(yearSelected, yearList2)]][[match(deptSelected, deptNumbersSecond)]][[1]][[match(deptSelected, deptNumbersSecond)]]$prototypes)
  }
  
  clusterNo<- unique(clusterVectorForGraph)
  clusterCentroids <- unname(clusterCentroids)
  
  for (yearSelected in yearList2) {
    
    print("ciclo")
    
    print(match(yearSelected, yearList2))
    print(match(deptSelected, deptNumbersSecond))
    print(max(clusterNo))
    print(listYears[[match(yearSelected, yearList2)]][[match(deptSelected, deptNumbersSecond)]][[1]][[max(clusterNo)]]$prototypes)
    clusterCentroids<- rbind(clusterCentroids, unname(listYears[[match(yearSelected, yearList2)]][[match(deptSelected, deptNumbersSecond)]][[1]][[max(clusterNo)]]$prototypes ))
  }
  
  clusterCentroids <- rbind (clusterCentroids, c(1:52))
  
  plot(xrange,
       yrange,
       type = "n",
       xlab = "WEEKNO",
       ylab = "WeeklySales", main=paste("Centroidi reparto", deptSelected,sep=" "))
  
  for(clusterSelected in clusterNo){
    print(clusterSelected)
    
    
    
    for(yearSelected2 in yearList2){
      
      
      
      color <- clusterSelected
      
      lines(
        clusterCentroids[nrow(clusterCentroids),],
        clusterCentroids[match(yearSelected, yearList2)*clusterSelected,],
        lty = 1,
        lwd = 2,
        col = colors[color], #+1 perch? indice parte da 1
        pch = pchDot
      )
      
      
      
    }
  }
  
  
  
}


# per ogni cluster la time series  deve essere colorata per store, se ho 3 cluster lo store deve avere lo stesso colore



# for (yearSelected in yearList2) {
#   clusterVectorForGraph<- append(clusterVectorForGraph, listYears[[match(yearSelected, yearList2)]][[match(deptSelected, deptNumbersSecond)]][[5]][[match(deptSelected, deptNumbersSecond)]])
#   print(dim(listYears[[match(yearSelected, yearList2)]][[match(deptSelected, deptNumbersSecond)]][[1]][[match(deptSelected, deptNumbersSecond)]]$prototypes))
# }
