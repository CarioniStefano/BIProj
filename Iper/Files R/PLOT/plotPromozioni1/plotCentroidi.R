

colors = c("red", "green", "blue", "black", "darkgray","purple","orange","burlywood","cyan2","yellow","darkgreen","lightpink")
listaProto <- listAllReparto[[1]][[1]][[1]][[1]][[3]]$prototypes

xrange <- range(0:53)
yrange <- range(0,1)
plot(xrange,
     yrange,
     type = "n",
     xlab = "WEEKNO",
     ylab = "WeeklySales", main=paste("Centroidi reparto"))

for(currentProto in 1:nrow(listaProto))
{
  color <- currentProto
  print(color)
  
  lines(
    1:52 ,
    listaProto[currentProto,],
    lty = 1,
    lwd = 2,
    col = colors[color], #+1 perch? indice parte da 1
    pch = pchDot
  )
  
}
# clusterCentroids<- rbind(clusterCentroids, unname(listAllDeptYear[[match(deptSelected, deptNumbersSecond)]][[1]][[max(clusterNo)]]$prototypes ))