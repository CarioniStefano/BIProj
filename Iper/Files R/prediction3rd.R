
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




svmInput <- storeOrderedByWeek[,c(1,2,3,4) ]
svmInput <- svmInput[!svmInput$ANNONO==2017,]


svmInput <- cbind(svmInput,NA)
svmInput <- cbind(svmInput,NA)


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

predictDataFrame <- svmInput[1:18,]
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




predictionMade <- storeOrderedByWeek[c(469,470,471 ,472,473,474 ,475,476,477 ,478,479,480 ,481,482,483 ,484,485,486),c(1,2,3,4) ]

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

