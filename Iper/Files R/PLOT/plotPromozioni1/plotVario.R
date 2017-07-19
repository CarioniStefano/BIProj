

# per ogni cluster la time series  deve essere colorata per store, se ho 3 cluster lo store deve avere lo stesso colore

#numero di grafici per ogni reparto, uno per ogni cluster
#un grafico per ogni cluster, i colori identificano lo store




par(mfrow = c(1, 1))
linetype <- 1
pchDot <- 16


tempDept <-
  appoggioDepts2[which(appoggioDepts2$REPARTO == 2 & appoggioDepts2$SETTORE == 52 & appoggioDepts2$GRUPPO == 5),]

yearNo<- unique(tempDept$ANNONO)
colors = c("red", "green", "blue", "black", "darkgray","purple","orange","burlywood","cyan2","yellow","darkgreen","lightpink")
clusterNo<- unique(tempDept$clusterVector2)
xrange <- range(1, 53)
# yrange <- range(c(0, na.omit(max(tempDept[,7:ncol(tempDept)]))))
yrange <- range(c(0, 2000))

plot(xrange,
     yrange,
     type = "n",
     xlab = "WEEKNO",
     ylab = "WeeklySales",main="banana")


for (famSelected in unique(tempDept$FAMIGLIA)) {
  
  
  
  
  for(yearSelected in yearNo){
    
    for(enteSelected in unique(tempDept$ENTE)){
      
      for(clusterSelected in clusterNo){
        
        color <- match(clusterSelected,clusterNo)
        
        lines(1:52 ,tempDept[which(tempDept$ANNONO == yearSelected & tempDept$FAMIGLIA == famSelected & 
                            tempDept$ENTE == enteSelected & tempDept$clusterVector2 == clusterSelected), 8:ncol(tempDept)],
          lty = 1,
          lwd = 1,
          col = colors[color], #+1 perch? indice parte da 1
          pch = pchDot
        )
        
      }
      
      
    }
  }
}

  
  
  
  # get the range for the x and y axis
  
  

  
  # tempDept2 <-tempDept[(as.numeric(as.character(tempDept$ANNONO)) %in% yearList2),]
  
  # prendere tutti gli anni cambiare match[yearSelecte,yearList due con un operazione vettoriale che prenda tutti gli year in yearlist]
  # clusterVectorForGraph <- c()
  
  
  # clusterVectorForGraph<- append(clusterVectorForGraph, listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[5]][[match(deptSelected, deptNumbersSecond)]])
  
  
  

#   
#   
#   
#   
#     
#     
#     
#     
#    
#     
#         
#         
#         
#         tempDept3 <- tempDept2[(as.numeric(as.character(tempDept2$ANNONO))) == yearSelected2,]
#         
#         
#         
#     
#     
#   }
# }
# 
