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


frequency(store1.z)
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

store1.z <- cbind(as.data.frame(store1.z),stores$Type[which(stores$Store==1)])
colnames(store1.z)<- c("Store","Dept","Weekly_Sales","IsHoliday","StoreType")
f <- decompose(store1.z, frequency=7)
store2.z = trainFile.z[which(trainFile.z$Store == 2), ]
store3.z = trainFile.z[which(trainFile.z$Store == 4), ]
store4.z = trainFile.z[which(trainFile.z$Store == 10), ]
store5.z = trainFile.z[which(trainFile.z$Store == 13), ]
store6.z = trainFile.z[which(trainFile.z$Store == 24), ]

# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# CREAZIONE MATRICE PER STORE E REPARTI SELEZIONATI
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################


HolidayDates <- store1.z[which(store1.z$IsHoliday == 1), 4]
HolidayDates <-
  data.frame(
    Date = time(HolidayDates),
    HolidayDates,
    check.names = FALSE,
    row.names = NULL
  )
HolidayDates[, 1] <- as.numeric(HolidayDates[, 1])
HolidayDates <- HolidayDates[!duplicated(HolidayDates),]

store1.mat = coredata(store1.z)
store2.mat = coredata(store2.z)
store3.mat = coredata(store3.z)
store4.mat = coredata(store4.z)
store5.mat = coredata(store5.z)
store6.mat = coredata(store6.z)

app <-
  data.frame(
    Date = time(store1.z),
    store1.z,
    check.names = FALSE,
    row.names = NULL
  )
app[, 1] <- as.numeric(app[, 1])

app2 <-
  data.frame(
    Date = time(store2.z),
    store2.z,
    check.names = FALSE,
    row.names = NULL
  )
app2[, 1] <- as.numeric(app2[, 1])

app3 <-
  data.frame(
    Date = time(store3.z),
    store3.z,
    check.names = FALSE,
    row.names = NULL
  )
app3[, 1] <- as.numeric(app3[, 1])

app4 <-
  data.frame(
    Date = time(store4.z),
    store4.z,
    check.names = FALSE,
    row.names = NULL
  )
app4[, 1] <- as.numeric(app4[, 1])

app5 <-
  data.frame(
    Date = time(store5.z),
    store5.z,
    check.names = FALSE,
    row.names = NULL
  )
app5[, 1] <- as.numeric(app5[, 1])

app6 <-
  data.frame(
    Date = time(store6.z),
    store6.z,
    check.names = FALSE,
    row.names = NULL
  )
app6[, 1] <- as.numeric(app6[, 1])

#TOGLO I DOPPIONI ISHOLIDAY E ORDINO LE COONNE PER COMODITA
app <- app[, c(1, 5, 2, 3, 4)]
app2$IsHoliday <- NULL
app3$IsHoliday <- NULL
app4$IsHoliday <- NULL
app5$IsHoliday <- NULL
app6$IsHoliday <- NULL

moreStores <- merge(app, app2 , by = c("Date", "Dept"), all = TRUE)
moreStores <- merge(moreStores,
                    app3 ,
                    by = c("Date", "Dept"),
                    all = TRUE)
moreStores <- merge(moreStores,
                    app4 ,
                    by = c("Date", "Dept"),
                    all = TRUE)
moreStores <- merge(moreStores,
                    app5 ,
                    by = c("Date", "Dept"),
                    all = TRUE)
moreStores <- merge(moreStores,
                    app6 ,
                    by = c("Date", "Dept"),
                    all = TRUE)
moreStores <- moreStores [order(moreStores[, 2]),]
colnames(moreStores) <-
  c(
    "Date",
    "Dept",
    "IsHoliday",
    "Store1",
    "WeeklySalesStore1",
    "Store2",
    "WeeklySalesStore2",
    "Store3",
    "WeeklySalesStore3",
    "Store4",
    "WeeklySalesStore4",
    "Store5",
    "WeeklySalesStore5",
    "Store6",
    "WeeklySalesStore6"
  )


# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# CREAZIONE MATRICE STATISTICA UNIVARIATA E SALVO IN UN FILE
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################



rowsNumber <-  length(sort(unique(moreStores[, 3]))) * 5

storeData <-
  data.frame(
    Store = rep(NA, rowsNumber),
    Department = rep(NA, rowsNumber),
    media = rep(NA, rowsNumber),
    varianza = rep(NA, rowsNumber),
    standardDeviation = rep(NA, rowsNumber),
    skewnesss = rep(NA, rowsNumber),
    curtosi = rep(NA, rowsNumber),
    quant1 = rep(NA, rowsNumber),
    quant5 = rep(NA, rowsNumber),
    # as many cols as you need
    stringsAsFactors = FALSE
  )


for (j in c(1:6)) {
  # print(j)
  
  
  for (i in sort(unique(moreStores[, 2]))) {
    print(((j - 1) * 99) + i)
    
    
    print(mean(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j *
                                                                      2)])))
    print(var(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j * 2)])))
    print(sd(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j * 2)])))
    print(skewness(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j *
                                                                          2)])))
    print(kurtosis(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j *
                                                                          2)])))
    print(quantile(
      moreStores[which(moreStores$Dept == i), 3 + (j * 2)],
      probs = c(0.01, 0.05),
      na.rm = TRUE
    ))
    
    
    mean <-
      mean(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j * 2)]))
    var <-
      var(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j * 2)]))
    sd <-
      (sd(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j * 2)])))
    skew <-
      skewness(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j * 2)]))
    kurt <-
      kurtosis(na.omit(moreStores[which(moreStores$Dept == i), 3 + (j * 2)]))
    quant <-
      quantile(moreStores[which(moreStores$Dept == i), 3 + (j * 2)],
               probs = c(0.01, 0.05),
               na.rm = TRUE)
    
    deptIndex <- c(i)
    deptIndexName <- "DEPT"
    
    storeData[((j - 1) * 99) + i,] <-
      c(j, i, mean, var, sd, skew, kurt, unname(quant))
    
  }
}


storeData <- na.omit(storeData)


# 1    2    3    4    7    8   10   13   14   16   21   38   40   46   67   79   81   82   90   91   92   95
# REPARTI CON 6435 ENTRY (MAX)
#
# 21  Books and Magazines  81  Commercial Bread
# REPARTI CON VARIANZA MINORE

# 91  Frozen Foods  40  Pharmacy OTC
# REPARTI CON VARIANZA MEDIA

# 7   Toys   92   Dry Grocery
# REPARTI CON VARIANZA MAGGIORE

deptSelected <-
  moreStores[which(
    moreStores$Dept == 21 |
      moreStores$Dept == 81 |
      moreStores$Dept == 91 |
      moreStores$Dept == 40 | moreStores$Dept == 7 |
      moreStores$Dept == 92
  ), ]

write.csv(deptSelected,
          file = "~/Lavoro/OutputFiles/deptSelected.csv",
          row.names = FALSE,
          append = FALSE)





# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# GRAFICO CON ASSE X DATA
# ASSE Y VENIDTE PER DIPARTIMENTO
# LINEE VERTICALI CON IsHoliday
#######################################################################################################################



ndepts <- c(21, 81, 91, 40, 7, 92)#numero di reparti selezionati
namedepts <-
  c(
    "Books an Magazine",
    "Commercial Bread",
    "Frozen Foods",
    "Pharmacy OTC",
    "Toys",
    "Dry Grocery"
  )
# get the range for the x and y axis
xrange <- range(deptSelected$Date)
deptSelectedtemp <- deptSelected


par(mfrow = c(3, 2))

for (j in c(1:6)) {
  yrange <- range(scale(deptSelected[, 3 + (j * 2)])[, 1])
  deptSelectedtemp [, (3 + (j * 2))] <-
    scale(deptSelectedtemp[, (3 + (j * 2))])[, 1]
  
  print(xrange)
  print("range plot")
  print(yrange)
  
  plot(xrange,
       yrange,
       type = "n",
       xlab = "Date",
       ylab = "WeeklySales")
  
  colors <-
    c("coral1",
      "chartreuse3",
      "chocolate4",
      "orange",
      "black",
      "purple")
  colIndex <- 1
  linetype <- 1
  pchDot <- 16
  
  for (i in ndepts) {
    tempDept <- subset(deptSelectedtemp, Dept == i)
    
    
    
    lines(
      tempDept$Date,
      tempDept[, (3 + (j * 2))] ,
      lwd = 2,
      lty = linetype,
      col = colors[colIndex],
      pch = pchDot
    )
    
    for (holiday in c(1:length(HolidayDates$Date))) {
      print(holiday)
      print(HolidayDates[holiday, 1])
      abline(v = HolidayDates[holiday, 1], col = "red")
    }
    
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
  
}






# #####################################################################################################################
# GRAFICO CON ASSE X DATA
# ASSE Y VENIDTE PER DIPARTIMENTO
# LINEE VERTICALI CON IsHoliday
#######################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# # PROSSIMA COSA DA FARE :
# CORRELAZIONE TRA VENDITE PER SINGOLO REPARTO E PRESENZA DI VACANZE
#######################################################################################################################


dept21 <- deptSelected[which(deptSelected$Dept == 21), ]
dept21$Store1 = NULL
dept21$Store2 = NULL
dept21$Store3 = NULL
dept21$Store4 = NULL
dept21$Store5 = NULL
dept21$Store6 = NULL
dept21$Dept = NULL
dept21 <- dept21[order(dept21[, 1]),]
dept21$Date = NULL

cor.mat = cor(cbind(dept21[, 1], dept21[, 2], dept21[, 3], dept21[, 4], dept21[, 5], dept21[, 6], dept21[, 7]))
colnames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
rownames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
print(cor.mat)

ScatterCC.ret.mat = cbind(dept21[, 1], dept21[, 2], dept21[, 3], dept21[, 4], dept21[, 5], dept21[, 6], dept21[, 7])
colnames(ScatterCC.ret.mat) = c("IsHoliday",
                                "Store1",
                                "Store2",
                                "Store3",
                                "Store4",
                                "Store5",
                                "Store6")
pairs(
  ScatterCC.ret.mat,
  pch = 18,
  col = "blue",
  cex = 1.5,
  cex.axis = 1.5
)


dept81 <- deptSelected[which(deptSelected$Dept == 81), ]
dept81$Store1 = NULL
dept81$Store2 = NULL
dept81$Store3 = NULL
dept81$Store4 = NULL
dept81$Store5 = NULL
dept81$Store6 = NULL
dept81$Dept = NULL
dept81 <- dept81[order(dept81[, 1]),]
dept81$Date = NULL

cor.mat = cor(cbind(dept81[, 1], dept81[, 2], dept81[, 3], dept81[, 4], dept81[, 5], dept81[, 6], dept81[, 7]))
colnames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
rownames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
print(cor.mat)

ScatterCC.ret.mat = cbind(dept81[, 1], dept81[, 2], dept81[, 3], dept81[, 4], dept81[, 5], dept81[, 6], dept81[, 7])
colnames(ScatterCC.ret.mat) = c("IsHoliday",
                                "Store1",
                                "Store2",
                                "Store3",
                                "Store4",
                                "Store5",
                                "Store6")
pairs(
  ScatterCC.ret.mat,
  pch = 18,
  col = "blue",
  cex = 1.5,
  cex.axis = 1.5
)


dept91 <- deptSelected[which(deptSelected$Dept == 91), ]
dept91$Store1 = NULL
dept91$Store2 = NULL
dept91$Store3 = NULL
dept91$Store4 = NULL
dept91$Store5 = NULL
dept91$Store6 = NULL
dept91$Dept = NULL
dept91 <- dept91[order(dept91[, 1]),]
dept91$Date = NULL

cor.mat = cor(cbind(dept91[, 1], dept91[, 2], dept91[, 3], dept91[, 4], dept91[, 5], dept91[, 6], dept91[, 7]))
colnames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
rownames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
print(cor.mat)

ScatterCC.ret.mat = cbind(dept91[, 1], dept91[, 2], dept91[, 3], dept91[, 4], dept91[, 5], dept91[, 6], dept91[, 7])
colnames(ScatterCC.ret.mat) = c("IsHoliday",
                                "Store1",
                                "Store2",
                                "Store3",
                                "Store4",
                                "Store5",
                                "Store6")
pairs(
  ScatterCC.ret.mat,
  pch = 18,
  col = "blue",
  cex = 1.5,
  cex.axis = 1.5
)


dept40 <- deptSelected[which(deptSelected$Dept == 40), ]
dept40$Store1 = NULL
dept40$Store2 = NULL
dept40$Store3 = NULL
dept40$Store4 = NULL
dept40$Store5 = NULL
dept40$Store6 = NULL
dept40$Dept = NULL
dept40 <- dept40[order(dept40[, 1]),]
dept40$Date = NULL

cor.mat = cor(cbind(dept40[, 1], dept40[, 2], dept40[, 3], dept40[, 4], dept40[, 5], dept40[, 6], dept40[, 7]))
colnames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
rownames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
print(cor.mat)

ScatterCC.ret.mat = cbind(dept40[, 1], dept40[, 2], dept40[, 3], dept40[, 4], dept40[, 5], dept40[, 6], dept40[, 7])
colnames(ScatterCC.ret.mat) = c("IsHoliday",
                                "Store1",
                                "Store2",
                                "Store3",
                                "Store4",
                                "Store5",
                                "Store6")
pairs(
  ScatterCC.ret.mat,
  pch = 18,
  col = "blue",
  cex = 1.5,
  cex.axis = 1.5
)


dept7 <- deptSelected[which(deptSelected$Dept == 7), ]
dept7$Store1 = NULL
dept7$Store2 = NULL
dept7$Store3 = NULL
dept7$Store4 = NULL
dept7$Store5 = NULL
dept7$Store6 = NULL
dept7$Dept = NULL
dept7 <- dept7[order(dept7[, 1]),]
dept7$Date = NULL

cor.mat = cor(cbind(dept7[, 1], dept7[, 2], dept7[, 3], dept7[, 4], dept7[, 5], dept7[, 6], dept7[, 7]))
colnames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
rownames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
print(cor.mat)

ScatterCC.ret.mat = cbind(dept7[, 1], dept7[, 2], dept7[, 3], dept7[, 4], dept7[, 5], dept7[, 6], dept7[, 7])
colnames(ScatterCC.ret.mat) = c("IsHoliday",
                                "Store1",
                                "Store2",
                                "Store3",
                                "Store4",
                                "Store5",
                                "Store6")
pairs(
  ScatterCC.ret.mat,
  pch = 18,
  col = "blue",
  cex = 1.5,
  cex.axis = 1.5
)


dept92 <- deptSelected[which(deptSelected$Dept == 92), ]
dept92$Store1 = NULL
dept92$Store2 = NULL
dept92$Store3 = NULL
dept92$Store4 = NULL
dept92$Store5 = NULL
dept92$Store6 = NULL
dept92$Dept = NULL
dept92 <- dept92[order(dept92[, 1]),]
dept92$Date = NULL

cor.mat = cor(cbind(dept92[, 1], dept92[, 2], dept92[, 3], dept92[, 4], dept92[, 5], dept92[, 6], dept92[, 7]))
colnames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
rownames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
print(cor.mat)

ScatterCC.ret.mat = cbind(dept92[, 1], dept92[, 2], dept92[, 3], dept92[, 4], dept92[, 5], dept92[, 6], dept92[, 7])
colnames(ScatterCC.ret.mat) = c("IsHoliday",
                                "Store1",
                                "Store2",
                                "Store3",
                                "Store4",
                                "Store5",
                                "Store6")
pairs(
  ScatterCC.ret.mat,
  pch = 18,
  col = "blue",
  cex = 1.5,
  cex.axis = 1.5
)






# #####################################################################################################################
# # PROSSIMA COSA DA FARE :
# CORRELAZIONE TRA VENDITE PER SINGOLO REPARTO E PRESENZA DI VACANZE
#######################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################


# # PROSSIMA COSA DA FARE :
# CORRELAZIONE TRA SOMMATORIE DI TUTTI I REPARTI TRA STORE E VACANZE
#######################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################


StoresCorrelation <- data.frame(unique(deptSelected[, c(1, 3)]))

StoresCorrelation <-
  cbind(StoresCorrelation,
        aggregate(
          moreStores$WeeklySalesStore1,
          by = list(Category1 = moreStores$Date, Store1 = moreStores$Store1),
          FUN = sum
        ))
StoresCorrelation <-
  cbind(StoresCorrelation,
        aggregate(
          moreStores$WeeklySalesStore2,
          by = list(Category2 = moreStores$Date, Store2 = moreStores$Store2),
          FUN = sum
        ))
StoresCorrelation <-
  cbind(StoresCorrelation,
        aggregate(
          moreStores$WeeklySalesStore3,
          by = list(Category3 = moreStores$Date, Store3 = moreStores$Store3),
          FUN = sum
        ))
StoresCorrelation <-
  cbind(StoresCorrelation,
        aggregate(
          moreStores$WeeklySalesStore4,
          by = list(Category4 = moreStores$Date, Store4 = moreStores$Store4),
          FUN = sum
        ))
StoresCorrelation <-
  cbind(StoresCorrelation,
        aggregate(
          moreStores$WeeklySalesStore5,
          by = list(Category5 = moreStores$Date, Store5 = moreStores$Store5),
          FUN = sum
        ))
StoresCorrelation <-
  cbind(StoresCorrelation,
        aggregate(
          moreStores$WeeklySalesStore6,
          by = list(Category6 = moreStores$Date, Store6 = moreStores$Store6),
          FUN = sum
        ))



StoresCorrelation$Store1 = NULL
StoresCorrelation$Store2 = NULL
StoresCorrelation$Store3 = NULL
StoresCorrelation$Store4 = NULL
StoresCorrelation$Store5 = NULL
StoresCorrelation$Store6 = NULL
StoresCorrelation$Category1 = NULL
StoresCorrelation$Category2 = NULL
StoresCorrelation$Category3 = NULL
StoresCorrelation$Category4 = NULL
StoresCorrelation$Category5 = NULL
StoresCorrelation$Category6 = NULL

colnames(StoresCorrelation) = c(
  "Date",
  "IsHoliday",
  "Store1Cumulative",
  "Store2Cumulative",
  "Store3Cumulative",
  "Store4Cumulative",
  "Store5Cumulative",
  "Store6Cumulative"
)

cor.mat = cor(
  cbind(
    StoresCorrelation[, 2],
    StoresCorrelation[, 3],
    StoresCorrelation[, 4],
    StoresCorrelation[, 5],
    StoresCorrelation[, 6],
    StoresCorrelation[, 7],
    StoresCorrelation[, 8]
  )
)
colnames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
rownames(cor.mat) = c("IsHoliday",
                      "Store1",
                      "Store2",
                      "Store3",
                      "Store4",
                      "Store5",
                      "Store6")
print(cor.mat)

ScatterCC.ret.mat = cbind(
  StoresCorrelation[, 2],
  StoresCorrelation[, 3],
  StoresCorrelation[, 4],
  StoresCorrelation[, 5],
  StoresCorrelation[, 6],
  StoresCorrelation[, 7],
  StoresCorrelation[, 8]
)
colnames(ScatterCC.ret.mat) = c("IsHoliday",
                                "Store1",
                                "Store2",
                                "Store3",
                                "Store4",
                                "Store5",
                                "Store6")
pairs(
  ScatterCC.ret.mat,
  pch = 18,
  col = "blue",
  cex = 1.5,
  cex.axis = 1.5
)







# # PROSSIMA COSA DA FARE :
# CORRELAZIONE TRA SOMMATORIE DI TUTTI I REPARTI TRA STORE E VACANZE
#######################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################


# # PROSSIMA COSA DA FARE :
# CLUSTERING STORES
#######################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################

# K-Means Cluster Analysis
# METODO 1 94.9%
storesCluster <- kmeans(stores[, 3], 3, iter.max = 10000)
storesCluster
table(storesCluster$cluster, stores$Type)

storesCluster$cluster <- as.factor(storesCluster$cluster)
ggplot(stores, aes( stores$Type,stores$Size , color = storesCluster$cluster)) + geom_point()








# # PROSSIMA COSA DA FARE :
# CLUSTERING STORES
#######################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################


# # PROSSIMA COSA DA FARE :
# DIVIDERE TIMESERIES IN WINDOWS
#######################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################



# # PROSSIMA COSA DA FARE :
# DIVIDEE TIMESERIES IN WINDOWS
#######################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################



# ORDINO IN BASE ALLA SECONDA COLONNA
# dataRow <- dataRow [ order(dataRow[,2]), ]
# par(mfrow=c(1,1))
#
#
# p1 <- ggplot()+geom_point(data=app , aes(x = app[,1] , y = app[,4] , color=app[,3]))+scale_y_continuous(labels = comma)+geom_vline(xintercept = HolidayDates[,1],color="red")
# p2 <- ggplot()+geom_point(data=app2 , aes(x = app2[,1] , y = app2[,4] , color=app2[,3]))+scale_y_continuous(labels = comma)+geom_vline(xintercept = HolidayDates[,1],color="red")
# p3 <- ggplot()+geom_point(data=app3 , aes(x = app3[,1] , y = app3[,4] , color=app3[,3]))+scale_y_continuous(labels = comma)+geom_vline(xintercept = HolidayDates[,1],color="red")
# p4 <- ggplot()+geom_point(data=app4 , aes(x = app4[,1] , y = app4[,4] , color=app4[,3]))+scale_y_continuous(labels = comma)+geom_vline(xintercept = HolidayDates[,1],color="red")
# p5 <- ggplot()+geom_point(data=app5 , aes(x = app5[,1] , y = app5[,4] , color=app5[,3]))+scale_y_continuous(labels = comma)+geom_vline(xintercept = HolidayDates[,1],color="red")
#
# grid.arrange(p1, p2, p3,p4,p5, ncol=1,nrow=5)
# #GRAFICO 3D
# # scatter3d(df2b[,1],df2b[,3],df2b[,4] , pch=16 ,  surface=FALSE ,xlab="DATE" ,ylab="DEPT" ,zlab="Sales", surface.col =c("green","red")  , groups=factor(df2b[,5]) ,bg.col=c("black"),axis.scale = FALSE, xlim = c(14000,16000))
#
# par(mfrow=c(2,2))
# histinfo = hist(store1.mat[,2],main="CPI", probability=TRUE, col="red", xlab="CPI")
#
# plot(density(na.omit(store1.mat[,2])),type="l",xlab="CPI", col="blue", lwd=2,ylab="density estimate",main="Smoothed CPI histogram")
# boxplot(store1.mat[,2], main="Boxplot CPI",ylab="CPI", col="red")
# qqnorm(store1.mat[,2],col="blue")
# qqline(store1.mat[,2])
