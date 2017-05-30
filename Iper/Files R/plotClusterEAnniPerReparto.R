

par(mfrow = c(1, 1))

for(deptSelected in deptNumbersSecond){
  
  
  
  
  
  
  
  
  colors = c("red","green","blue","black","yellow")
  
  
  # get the range for the x and y axis
  xrange <- range(1,53)
  yrange <- range(c(0,na.omit(max(clusterDataframe[,4:ncol(clusterDataframe)]))))
  # yrange <- range(c(0,1)
  # print(yrange)
  # print(xrange)
  plot(xrange,
       yrange,
       type = "n",
       xlab = "WEEKNO",
       ylab = "WeeklySales",main=paste("Reparto", deptSelected , sep=" "))
  
  
  
  
  
  linetype <- 1
  pchDot <- 16
  
  
  
  
  print(deptSelected)
  
  tempDept <-  clusterDataframe[(as.numeric(as.character(clusterDataframe$REPARTO)) == deptSelected), ]
  
  # print(tempDept)
  
  for(yearSelected in yearList2){
    
    
    
    tempDept2 <-  tempDept[(as.numeric(as.character(tempDept$ANNONO)) == yearSelected), ]
    linetype=(as.numeric(as.character(head(tempDept[(as.numeric(as.character(tempDept$ANNONO)) == yearSelected & as.numeric(as.character(tempDept$REPARTO)) == deptSelected), ]$ANNONO,1) )))-2013
    
    for(colCount in 4:ncol(tempDept)){
      
      color = listYears[[match(yearSelected,yearList2)]][[match(deptSelected,deptNumbersSecond)]][[5]][[match(deptSelected,deptNumbersSecond)]][colCount-4]
      print(color)
      print(as.numeric((tempDept2$ANNONO))-2014)
      # if(tempDept[,colCount-1]==200){colors[colIndex]="black"}
      lines(tempDept2$SETTIMANANO,tempDept2[,colCount],
            lty = linetype,
            lwd = 2,
            col = colors[color], #+1 perchè indice parte da 1
            pch = pchDot
      )
      
      
    }
  }
  
  
  legend( x="topleft", 
          legend=c("2014","2015","2016"),
          col=c("black","black","black"), lwd=1, lty=c(1,2,3), 
  )
  
}

