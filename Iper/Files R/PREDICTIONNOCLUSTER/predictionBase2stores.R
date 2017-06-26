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

library(mlbench)
library(rpart)



# svmInput2 <- storeOrderedByWeek[,c(1,2,3,5) ]
# svmInput2 <- svmInput2[!svmInput2$ANNONO==2017,]
# svmInput2 <- svmInput2[,c(2,3,4)]
# 
# 
# predictDataFrame2 <- svmInput2[1:18,]
# predictDataFrame2[,3] <- 0



svmInput1 <- storeOrderedByWeek[,c(1,2,3,4) ]
svmInput1 <- svmInput1[!svmInput1$ANNONO==2017,]
svmInput1 <- svmInput1[,c(2,3,4)]


predictDataFrame1 <- svmInput1[1:18,]
predictDataFrame1[,3] <- 0

svmInput2 <- storeOrderedByWeek[,c(1,2,3,5) ]
svmInput2 <- svmInput2[!svmInput2$ANNONO==2017,]
svmInput2 <- svmInput2[,c(2,3,4)]


predictDataFrame2 <- svmInput2[1:18,]
predictDataFrame2[,3] <- 0





train_controlcv <- trainControl(method="cv", number=10)
train_controlrcv <- trainControl(method="repeatedcv", number=10, repeats=3)

svmRadialFit1 <- train(VALORETOT1 ~ .,
                       data = svmInput1,
                       method = "svmRadial",
                       tuneLength = 12 )

svmRadialFit2 <- train(VALORETOT1 ~ .,
                       data = svmInput1,
                       method = "svmRadial",
                       tuneLength = 12 , trControl=train_controlcv)
svmRadialFit3 <- train(VALORETOT1 ~ .,
                       data = svmInput1,
                       method = "svmRadial",
                       tuneLength = 12 , trControl=train_controlrcv)

svmRadialSigmaFit1 <- train(VALORETOT1 ~ .,
                            data = svmInput1,
                            method = "svmRadialSigma",
                            tuneLength = 12)
svmRadialSigmaFit2 <- train(VALORETOT1 ~ .,
                            data = svmInput1,
                            method = "svmRadialSigma",
                            trControl=train_controlcv,
                            tuneLength = 12)
svmRadialSigmaFit3 <- train(VALORETOT1 ~ .,
                            data = svmInput1,
                            method = "svmRadialSigma",
                            trControl=train_controlrcv,
                            tuneLength = 12)


svmRadialFit12nd <- train(VALORETOT2 ~ .,
                       data = svmInput2,
                       method = "svmRadial",
                       tuneLength = 12 )

svmRadialFit22nd <- train(VALORETOT2 ~ .,
                       data = svmInput2,
                       method = "svmRadial",
                       tuneLength = 12 , trControl=train_controlcv)
svmRadialFit32nd <- train(VALORETOT2 ~ .,
                       data = svmInput2,
                       method = "svmRadial",
                       tuneLength = 12 , trControl=train_controlrcv)

svmRadialSigmaFit12nd <- train(VALORETOT2 ~ .,
                            data = svmInput2,
                            method = "svmRadialSigma",
                            tuneLength = 12)
svmRadialSigmaFit22nd <- train(VALORETOT2 ~ .,
                            data = svmInput2,
                            method = "svmRadialSigma",
                            trControl=train_controlcv,
                            tuneLength = 12)
svmRadialSigmaFit32nd <- train(VALORETOT2 ~ .,
                            data = svmInput2,
                            method = "svmRadialSigma",
                            trControl=train_controlrcv,
                            tuneLength = 12)


prediction1 <- predict(svmRadialFit1$finalModel,predictDataFrame1[,-3])
prediction2 <- predict(svmRadialFit2$finalModel,predictDataFrame1[,-3])
prediction3 <- predict(svmRadialFit3$finalModel,predictDataFrame1[,-3])
prediction4 <- predict(svmRadialSigmaFit1$finalModel,predictDataFrame1[,-3])
prediction5 <- predict(svmRadialSigmaFit2$finalModel,predictDataFrame1[,-3])
prediction6 <- predict(svmRadialSigmaFit3$finalModel,predictDataFrame1[,-3])


prediction12nd <- predict(svmRadialFit12nd$finalModel,predictDataFrame2[,-3])
prediction22nd <- predict(svmRadialFit22nd$finalModel,predictDataFrame2[,-3])
prediction32nd <- predict(svmRadialFit32nd$finalModel,predictDataFrame2[,-3])
prediction42nd <- predict(svmRadialSigmaFit12nd$finalModel,predictDataFrame2[,-3])
prediction52nd <- predict(svmRadialSigmaFit22nd$finalModel,predictDataFrame2[,-3])
prediction62nd <- predict(svmRadialSigmaFit32nd$finalModel,predictDataFrame2[,-3])





predictionMade2stores <- storeOrderedByWeek[c(469,470,471 ,472,473,474 ,475,476,477 ,478,479,480 ,481,482,483 ,484,485,486), c(1,2,3,4) ]
predictionMade2stores <- cbind(predictionMade2stores,prediction1,prediction2,prediction3,prediction4,prediction5,prediction6)

predictionMade2stores <- cbind(predictionMade2stores,storeOrderedByWeek[c(469,470,471 ,472,473,474 ,475,476,477 ,478,479,480 ,481,482,483 ,484,485,486),5 ])
predictionMade2stores <- cbind(predictionMade2stores,prediction12nd,prediction22nd,prediction32nd,prediction42nd,prediction52nd,prediction62nd)

View(predictionMade2stores)
colnames(predictionMade2stores)[11]<- "VALORETOT2"

error1 <-  predictionMade2stores$prediction1 - predictionMade2stores$VALORETOT1
error2 <- predictionMade2stores$prediction2 - predictionMade2stores$VALORETOT1
error3 <-  predictionMade2stores$prediction3 - predictionMade2stores$VALORETOT1
error4 <-  predictionMade2stores$prediction4 - predictionMade2stores$VALORETOT1
error5 <-  predictionMade2stores$prediction5 - predictionMade2stores$VALORETOT1
error6 <-  predictionMade2stores$prediction6 -predictionMade2stores$VALORETOT1

error12nd <-   predictionMade2stores$prediction12nd - predictionMade2stores$VALORETOT2
error22nd <-  predictionMade2stores$prediction22nd- predictionMade2stores$VALORETOT2
error32nd <-  predictionMade2stores$prediction32nd- predictionMade2stores$VALORETOT2
error42nd <- predictionMade2stores$prediction42nd- predictionMade2stores$VALORETOT2
error52nd <-  predictionMade2stores$prediction52nd- predictionMade2stores$VALORETOT2
error62nd <-  predictionMade2stores$prediction62nd- predictionMade2stores$VALORETOT2


min(rmse(error1),rmse(error2),rmse(error3),rmse(error4),rmse(error5),rmse(error6))
min(mae(error1),mae(error2),mae(error3),mae(error4),mae(error5),mae(error6))

min(mean(abs((error1)/predictionMade2stores$VALORETOT1) * 100), mean(abs((error2)/predictionMade2stores$VALORETOT1) * 100), mean(abs((error3)/predictionMade2stores$VALORETOT1) * 100),
    mean(abs((error4)/predictionMade2stores$VALORETOT1) * 100), mean(abs((error5)/predictionMade2stores$VALORETOT1) * 100), mean(abs((error6)/predictionMade2stores$VALORETOT1) * 100))

getTrainPerf(svmRadialFit1)$TrainRMSE
getTrainPerf(svmRadialFit2)$TrainRMSE
getTrainPerf(svmRadialFit3)$TrainRMSE

getTrainPerf(svmRadialSigmaFit1)$TrainRMSE
getTrainPerf(svmRadialSigmaFit2)$TrainRMSE
getTrainPerf(svmRadialSigmaFit3)$TrainRMSE

min(getTrainPerf(svmRadialFit1)$TrainRMSE,getTrainPerf(svmRadialFit2)$TrainRMSE,getTrainPerf(svmRadialFit3)$TrainRMSE,
    getTrainPerf(svmRadialSigmaFit1)$TrainRMSE,getTrainPerf(svmRadialSigmaFit2)$TrainRMSE,getTrainPerf(svmRadialSigmaFit3)$TrainRMSE)




min(rmse(error12nd),rmse(error22nd),rmse(error32nd),rmse(error42nd),rmse(error52nd),rmse(error62nd))
min(mae(error12nd),mae(error22nd),mae(error32nd),mae(error42nd),mae(error52nd),mae(error62nd))

min(mean(abs((error12nd)/predictionMade2stores$VALORETOT2) * 100), mean(abs((error22nd)/predictionMade2stores$VALORETOT2) * 100), mean(abs((error32nd)/predictionMade2stores$VALORETOT2) * 100),
    mean(abs((error42nd)/predictionMade2stores$VALORETOT2) * 100), mean(abs((error52nd)/predictionMade2stores$VALORETOT2) * 100), mean(abs((error62nd)/predictionMade2stores$VALORETOT2) * 100))

getTrainPerf(svmRadialFit12nd)$TrainRMSE
getTrainPerf(svmRadialFit22nd)$TrainRMSE
getTrainPerf(svmRadialFit32nd)$TrainRMSE

getTrainPerf(svmRadialSigmaFit12nd)$TrainRMSE
getTrainPerf(svmRadialSigmaFit22nd)$TrainRMSE
getTrainPerf(svmRadialSigmaFit32nd)$TrainRMSE

min(getTrainPerf(svmRadialFit12nd)$TrainRMSE,getTrainPerf(svmRadialFit22nd)$TrainRMSE,getTrainPerf(svmRadialFit32nd)$TrainRMSE,
    getTrainPerf(svmRadialSigmaFit12nd)$TrainRMSE,getTrainPerf(svmRadialSigmaFit22nd)$TrainRMSE,getTrainPerf(svmRadialSigmaFit32nd)$TrainRMSE)



# svmInput <- cbind(svmInput,paste(svmInput[,1],paste( "W",formatC(format="d",svmInput[,2] ,flag="0",width=ceiling(log10(max(svmInput[,2])))), sep="" )   ,1,sep="-"))
# 
# 
# testRegex <- svmInput[,7]
# svmInput[,7]<-ISOweek2date(testRegex)
# svmInput[,7] <- ISOweek2date( paste(testRegex[-grep("\\b\\d{4}-W53", ISOweek(as.Date(ISOweek2date (paste(week_iso,1,sep="-"))) + (0:(length(svmInput[,7])))*7) , perl=TRUE, value=FALSE)], 1, sep="-"))
# colnames(svmInput) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT","VALOREPROMO","QUANTITA","DATAINTERA")
# NON SERVE PI? MA CI HO MESSO TROPPO A FARLO PER CANCELLARLo
