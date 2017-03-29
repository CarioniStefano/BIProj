library(readr)

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
library(mclust)
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


options(scipen = 999)


# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# TODO LIST
#correlazione per reparto con vendite/holiday
#correlazione per store con sommatoria vendite / holiday + tipo store + dimensione store + temperatura
#livello store, livello reparto per singolo store, livello reparto per tutti store, livello tutti i reparti di singolo store

#aggiungere classificazione store per tipo
#multicollinearità (selezione variabili modello)

# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################

trainFile.z <-
  read.zoo(
    file = "~/Lavoro/InputFiles/train.csv",
    header = TRUE,
    index.column = 3,
    sep = ","
  )


stores <- read_csv("~/Lavoro/InputFiles/stores.csv")
# dataNumber <- table(trainFile.z$Store)



# 13 10474
# 10 10315
# 1 10244
# 2 10238
# 4 10272
# 24 10228


# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# STORE CON MAGGIORI DATI

store1.z = trainFile.z[which(trainFile.z$Store == 1), ]
store1.z = store1.z[which(store1.z$Dept == 7), ]

# 
#

windowed = store1.z

windowed = rollapply(store1.z, width = 4, FUN = mean, align = "left")
windowed$Dept=NULL
windowed$Store=NULL
# index(windowed) <- as.POSIXlt(index(windowed))
# index(windowed) <- strftime(index(windowed),format="%W")
# plot(windowed)
from <- as.Date("2010-01-01")
to <- as.Date("2012-12-31")
months <- seq.Date(from=from,to=to,by="week")

values <- rep.int(0,length(months))

Zooserie <- zoo(values, months)



asd <- merge(windowed,Zooserie)
asd <- merge(asd,Zooserie)
asd <-asd[!is.na(asd$Weekly_Sales),]


weekNo <- as.POSIXlt(index(asd))
asd[,3] <- as.numeric(strftime(weekNo,format="%m"))
asd[,4] <- as.numeric(strftime(weekNo,format="%Y"))



tes <- coredata(asd)

colnames(asd) <- c("Weekly_Sales","IsHoliday","WeekNo","Year")
asd2<-df[!duplicated(df[c("a", "b")]),]


#TODO CREEARE UN OGGETTO ZOO CHE ABBIA 52 SETTIMANE PER TUTTI I 3 ANNI (NA VALUES PER LE SETTIMANE CHE NON SONO NEI DATI EFFETTIVI )
# MERGE CON UN OGGETTO ZOO VUOTO?



years <- c(2010, 2011,2012)#numero di reparti selezionati
namedepts <-
  c("2010","2011","2012"  )
# get the range for the x and y axis
xrange <- range(0,53)
yrange <- range(asd[,1])
print(yrange)
plot(xrange,
     yrange,
     type = "n",
     xlab = "Date",
     ylab = "WeeklySales")
colIndex <- 1

for (j in years) {
  
  print(j)
  
  colors <-
    c("coral1",
      "chartreuse3",
      "chocolate4",
      "orange",
      "black",
      "purple")
  
  linetype <- 1
  pchDot <- 16
  
  tempDept <- subset(asd, Year == j)
  print(range(tempDept$Weekly_Sales))
  
  
  lines(
    tempDept$WeekNo,
    tempDept$Weekly_Sales ,
    lwd = 2,
    lty = linetype,
    col = colors[colIndex],
    pch = pchDot
  )
  colIndex <- colIndex + 1
  
}
legend(
  xrange[1],
  yrange[2],
  namedepts,
  cex = 1,
  col = colors,
  pch = pchDot,
  lty = linetype,
  title = "Dept"
)


