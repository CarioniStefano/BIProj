library(mlbench)
library(rpart)
library(zoo)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error) 
{
  mean(abs(error))
}


storesNumbersSecond <- c(1 , 4 , 6 , 7 , 8 , 9)
repartoSelected <- 3
clusterSelected <- 2


entiList <-clusterDepts[which(as.numeric(clusterDepts$REPARTO) == repartoSelected &  as.numeric(clusterDepts$clusterVector) == clusterSelected ),]
unique(entiList$ENTE)
svmInput <- storeOrderedByWeek[which(as.numeric(storeOrderedByWeek$REPARTO) == repartoSelected ), c(1,2,3 , 3 + match(as.numeric(unique(entiList$ENTE)),storesNumbersSecond))]

# svmInput <- svmInput[!svmInput$ANNONO==2017,]
svmInput <- cbind(svmInput,paste(svmInput[,1],paste( "W",formatC(format="d",svmInput[,2] ,flag="0",width=ceiling(log10(max(svmInput[,2])))), sep="" )   ,1,sep="-"))
testRegex <- svmInput[,ncol(svmInput)]
svmInput[,ncol(svmInput)]<-ISOweek2date(testRegex)
colnames(svmInput) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT1","VALORETOT2","VALORETOT3","VALORETOT4","VALORETOTO5","DATE")
z <- read.zoo(svmInput, index = "DATE")
z$REPARTO <- NULL
z$ANNONO <- NULL

# svmInput[,5] <- ISOweek2date( paste(testRegex[-grep("\\b\\d{4}-W53", ISOweek(as.Date(ISOweek2date (paste(week_iso,1,sep="-"))) + (0:(length(svmInput[,5])))*7) , perl=TRUE, value=FALSE)], 1, sep="-"))

as.Date(paste("1", 1,  2014, sep = "-"), format = "%w-%W-%Y")
as.Date(paste("1", 53,  2015, sep = "-"), format = "%w-%W-%Y")

as.Date(paste("1", storeOrderedByWeek$SETTIMANANO,  storeOrderedByWeek$ANNONO, sep = "-"), format = "%w-%W-%Y")


svmInput <- storeOrderedByWeek[,c(1,2,3,4) ]
svmInput <- svmInput[!svmInput$ANNONO==2017,]


svmInput <- cbind(svmInput,NA)
svmInput <- cbind(svmInput,NA)





newCol <- data.frame()
newCol2 <- data.frame()

for(week in unique(svmInput$SETTIMANANO)) {
  # print("week")
  # print(week)
  svmInput[which(as.numeric( svmInput$SETTIMANANO ) == week), 5] <- colMeans(svmInput[which(as.numeric( svmInput$SETTIMANANO ) == week),])[4] 
  for(deptCycle in unique(svmInput$REPARTO)){
  
    # print("dept")
    # print(deptCycle)
    # print(colMeans(svmInput[which(as.numeric( svmInput$SETTIMANANO ) == week  &  as.numeric(svmInput$REPARTO) == deptCycle  ), ]))
    # 
    svmInput[which(as.numeric( svmInput$SETTIMANANO ) == week  &  as.numeric(svmInput$REPARTO) == deptCycle  ), 6] <- colMeans(svmInput[which(as.numeric( svmInput$SETTIMANANO ) == week  &  as.numeric(svmInput$REPARTO) == deptCycle  ), ])[4]
    }
}

colnames(svmInput) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT1","MEDIASETTIMANA","MEDIAREPARTO")
svmInput <- svmInput[,2:ncol(svmInput)]
svmInput <- svmInput[,c(1,2,4,5,3)]

# svmInput <- cbind(svmInput,paste(svmInput[,1],paste( "W",formatC(format="d",svmInput[,2] ,flag="0",width=ceiling(log10(max(svmInput[,2])))), sep="" )   ,1,sep="-"))
# 
# 
# testRegex <- svmInput[,7]
# svmInput[,7]<-ISOweek2date(testRegex)
# colnames(svmInput) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT","VALOREPROMO","QUANTITA","DATAINTERA")
# NON SERVE PI? MA CI HO MESSO TROPPO A FARLO PER CANCELLARLo
# svmInput[,7] <- ISOweek2date( paste(testRegex[-grep("\\b\\d{4}-W53", ISOweek(as.Date(ISOweek2date (paste(week_iso,1,sep="-"))) + (0:(length(svmInput[,7])))*7) , perl=TRUE, value=FALSE)], 1, sep="-"))
#  




predictDataFrame <- svmInput[1:9,]
predictDataFrame[,5] <- 0








train_controlcv <- trainControl(method="cv", number=10)
train_controlrcv <- trainControl(method="repeatedcv", number=10, repeats=3)


# svmPolyFit <- train(VALORETOT1 ~ .,
#                     data = svmInput,
#                     method = "svmPoly",
#                     tuneLength = 12)

svmRadialFit1 <- train(VALORETOT1 ~ .,
                    data = svmInput,
                    method = "svmRadial",
                    tuneLength = 15 )

svmRadialFit2 <- train(VALORETOT1 ~ .,
                      data = svmInput,
                      method = "svmRadial",
                      tuneLength = 15 , trControl=train_controlcv)
svmRadialFit3 <- train(VALORETOT1 ~ .,
                       data = svmInput,
                       method = "svmRadial",
                       tuneLength = 15 , trControl=train_controlrcv)

svmRadialSigmaFit1 <- train(VALORETOT1 ~ .,
                      data = svmInput,
                      method = "svmRadialSigma",
                      tuneLength = 15)
svmRadialSigmaFit2 <- train(VALORETOT1 ~ .,
                           data = svmInput,
                           method = "svmRadialSigma",
                           trControl=train_controlcv,
                           tuneLength = 15)
svmRadialSigmaFit3 <- train(VALORETOT1 ~ .,
                           data = svmInput,
                           method = "svmRadialSigma",
                           trControl=train_controlrcv,
                           tuneLength = 15)



prediction1 <- predict(svmRadialFit1$finalModel,predictDataFrame[,-5])
prediction2 <- predict(svmRadialFit2$finalModel,predictDataFrame[,-5])
prediction3 <- predict(svmRadialFit3$finalModel,predictDataFrame[,-5])
prediction4 <- predict(svmRadialSigmaFit1$finalModel,predictDataFrame[,-5])
prediction5 <- predict(svmRadialSigmaFit2$finalModel,predictDataFrame[,-5])
prediction6 <- predict(svmRadialSigmaFit3$finalModel,predictDataFrame[,-5])




predictionMade <- storeOrderedByWeek[c(469,470,471,472,473,474,475,476,477),c(1,2,3,4) ]
predictionMade <- storeOrderedByWeek[ c(469,470,471,472,473,474,475,476,477) , c(1,2,3,4) ]
predictionMade <- cbind(predictionMade,prediction1,prediction2,prediction3,prediction4,prediction5,prediction6)
View(predictionMade)

error1 <- predictionMade$VALORETOT1 - predictionMade$prediction1
error2 <- predictionMade$VALORETOT1 - predictionMade$prediction2
error3 <- predictionMade$VALORETOT1 - predictionMade$prediction3
error4 <- predictionMade$VALORETOT1 - predictionMade$prediction4
error5 <- predictionMade$VALORETOT1 - predictionMade$prediction5
error6 <- predictionMade$VALORETOT1 - predictionMade$prediction6


min(rmse(error1),rmse(error2),rmse(error3),rmse(error4),rmse(error5),rmse(error6))
min(mae(error1),mae(error2),mae(error3),mae(error4),mae(error5),mae(error6))

min(mean(abs((error1)/predictionMade$VALORETOT1) * 100), mean(abs((error2)/predictionMade$VALORETOT1) * 100), mean(abs((error3)/predictionMade$VALORETOT1) * 100),
    mean(abs((error4)/predictionMade$VALORETOT1) * 100), mean(abs((error5)/predictionMade$VALORETOT1) * 100), mean(abs((error6)/predictionMade$VALORETOT1) * 100))


getTrainPerf(svmRadialFit1)$TrainRMSE
getTrainPerf(svmRadialFit2)$TrainRMSE
getTrainPerf(svmRadialFit3)$TrainRMSE

getTrainPerf(svmRadialSigmaFit1)$TrainRMSE
getTrainPerf(svmRadialSigmaFit2)$TrainRMSE
getTrainPerf(svmRadialSigmaFit3)$TrainRMSE

min(getTrainPerf(svmRadialFit1)$TrainRMSE,getTrainPerf(svmRadialFit2)$TrainRMSE,getTrainPerf(svmRadialFit3)$TrainRMSE,
    getTrainPerf(svmRadialSigmaFit1)$TrainRMSE,getTrainPerf(svmRadialSigmaFit2)$TrainRMSE,getTrainPerf(svmRadialSigmaFit3)$TrainRMSE)

