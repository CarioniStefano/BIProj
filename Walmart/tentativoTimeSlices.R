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
library(scales)
library(xts)


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
#multicollinearitÓ (selezione variabili modello)

# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# #####################################################################################################################
# 
# trainFile.z <-
#   read.zoo(
#     file = "~/Lavoro/InputFiles/train.csv",
#     header = TRUE,
#     index.column = 3,
#     sep = ","
#   )
stores <- read_csv("~/Lavoro/InputFiles/stores.csv")
stores$Type[stores$Type=="A"]  <- 100
stores$Type[stores$Type=="B"]  <- 200
stores$Type[stores$Type=="C"]  <- 300

# notZoo <- read.csv( file = "~/Lavoro/InputFiles/train.csv", header = TRUE, sep = ",")
# 
# notZoo <- merge.data.frame(notZoo,stores,all=TRUE)
# 
# 
# 
# notZoo <- xts(notZoo , order.by=make.time.unique(as.POSIXct(notZoo[,3],tz="UTC") ))


# notZoo <- cbind(as.data.frame(notZoo),stores$Type[which(stores$Store==notZoo$Store)])

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
storesNumbers <- c(1,2,4,10,13,24)
storesNumbersSecond <- c(1,2,4,10,13,24)
# deptNumbers <- c(21,81,91,40,7,92)
deptNumbers <- c(21,40,7)

deptDivisionList <- list()
par(mfrow = c(3, 1))
for(deptSelected in deptNumbers){
  
  # print(deptSelected)
  # deptDivisionList <- list(deptDivisionList,list(newObj))
  
  
  
  storesNumbers <- c(1,2,4,10,13,24)
  storesBinded.z = notZoo[which(as.numeric(notZoo$Store) == head(storesNumbers,1)), ]
  
  storesBinded.z <- storesBinded.z[, c(3, 5, 2, 1,6,4)]
  
  
  
  
  storesNumbers=tail(storesNumbers,-1)
  
  for(storeNo in storesNumbers){
    
    # print(storeNo)
    storesBinded.z = cbind(storesBinded.z , notZoo[which(as.numeric(notZoo$Store) == storeNo), c(1,6,4)])
    
  }
  
  storesBinded.z = storesBinded.z[which(as.numeric(storesBinded.z$Dept) == deptSelected), ]
  
  from <- as.Date("2010-01-01")
  to <- as.Date("2012-12-31")
  months <- seq.Date(from=from,to=to,by="week")
  
  values <- rep.int(-999,length(months))
  
  rangeZooPre <- zoo(values, months)
  
  rangeZoo <- xts(rangeZooPre , order.by=make.time.unique(as.POSIXct(index(rangeZooPre)), tz="UTC" ) )
  
  
  storesBinded.z <- merge(rangeZoo,storesBinded.z)
  
  
  from <- as.Date("2010-01-01")
  to <- as.Date("2012-12-31")
  months <- seq.Date(from=from,to=to,by="week")
  
  values <- rep.int(0,length(months))
  
  Zooserie <- zoo(values, months)
  
  storeWindowed <- Zooserie
  storeWindowed <- merge(storeWindowed,Zooserie)
  storeWindowed <- merge(storeWindowed,Zooserie)
  storeWindowed <- merge(storeWindowed,Zooserie)
  
  
  
  weekNo <- as.POSIXlt(index(storeWindowed))
  storeWindowed[,1] <- as.numeric((strftime(weekNo,format="%Y%m")), sep = "")
  storeWindowed[,2] <- as.numeric(strftime(weekNo,format="%m"))
  storeWindowed[,3] <- as.numeric(strftime(weekNo,format="%Y"))
  storeWindowed[,4] <- as.numeric(head(na.omit(storesBinded.z$Dept),1))
  
  
  colnames(storeWindowed) <- c("YearMonth","Month","Year","Dept")
  
  # 5 8 11 14
  # 2+3*1  2+3*2  2+3*3 2+3*4
  for(storeNo in storesNumbersSecond){
    
    # print(storeNo)
    # print(match(storeNo,storesNumbersSecond))
    # # rollapply(storesBinded.z[,(3+2*(match(storeNo,storesNumbersSecond)))], width = 4, FUN = mean, align = "left")
    # print(dim(storeWindowed))
    # print(dim( rollapply(storesBinded.z[,(3+2*(match(storeNo,storesNumbersSecond)))], width = 4, FUN = mean, align = "left")))
    storeWindowed <- cbind(storeWindowed , storesBinded.z[,(3+3*(match(storeNo,storesNumbersSecond)))])
    storeWindowed <- cbind(storeWindowed , rollapply(storesBinded.z[,(4+3*(match(storeNo,storesNumbersSecond)))], width = 4, FUN = mean, align = "left"))
    
    
  }
  
  storeWindowed<-storeWindowed[!duplicated(storeWindowed[,1]),]
  
  
  
  for(storeNo in storesNumbersSecond){
    print(median(na.omit(storeWindowed[,(4+2*(match(storeNo,storesNumbersSecond)))])))
    storeWindowed[rowSums(is.na(storeWindowed)) > 0,(3+2*(match(storeNo,storesNumbersSecond)))] <- median(na.omit(storeWindowed[,(3+2*(match(storeNo,storesNumbersSecond)))]))
    storeWindowed[rowSums(is.na(storeWindowed)) > 0,(4+2*(match(storeNo,storesNumbersSecond)))] <- median(na.omit(storeWindowed[,(4+2*(match(storeNo,storesNumbersSecond)))]))



  }
  
  
  #scaling valori di vndita
  # for(storeNo in storesNumbersSecond){
  #   
  #   storeWindowed[,(4+2*(match(storeNo,storesNumbersSecond)))] <- (rescale(storeWindowed[,(4+2*(match(storeNo,storesNumbersSecond)))],to=c(0,1)))
  #   
  # }
  
  
  
  deptDivisionList <- list(deptDivisionList,storeWindowed)
  
  
  
  years <- c(2010,2011,2012)#numero di reparti selezionati
  namedepts <-
    c("2010","2011","2012"  )
  
  
  colIndex <- 1
  
  colors = rainbow(ncol(storeWindowed))
  # colors <-
  #   c("coral1",
  #     "chartreuse3",
  #     "chocolate4",
  #     "orange",
  #     "black",
  #     "purple","royalblue","seagreen4","violetred1")
  
  # get the range for the x and y axis
  xrange <- range(0,12)
  yrange <- range(c(0,na.omit(max(storeWindowed[,4:ncol(storeWindowed)]))))
  # yrange <- range(c(0,1)
  # print(yrange)
  # print(xrange)
  plot(xrange,
       yrange,
       type = "n",
       xlab = "Date",
       ylab = "WeeklySales")
  
  
  
  
  
  linetype <- 1
  pchDot <- 16
  
  
  for (j in years) {
    
    print(j)
    
    tempDept <- data.frame( subset(storeWindowed, Year == j))
    
    for(colCount in 6:ncol(tempDept)){
      
      
      # print(colCount)
      if(colCount%%2==0){
        print(colCount)
         if(tempDept[,colCount-1]==200){colors[colIndex]="black"}
        
      lines(tempDept$Month,tempDept[,colCount],
            lty = linetype,
            lwd = 2,
            col = colors[colIndex],
            pch = pchDot
      )
      colIndex <- colIndex + 1
      }
    }
    # legend(
    #   xrange[1],
    #   yrange[2],
    #   namedepts,
    #   cex = 1,
    #   col = colors,
    #   pch = pchDot,
    #   lty = linetype,
    #   title = "Dept"
    # )
    
    
  }
}


