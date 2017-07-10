
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




svmInput3 <- storeOrderedByWeek[,c(1,2,3,4) ]
# svmInput3 <- svmInput3[!svmInput3$ANNONO==2017,]
svmInput3 <- cbind(svmInput3,NA)

svmInput4 <- storeOrderedByWeek[,c(1,2,3,5) ]
svmInput4 <- svmInput4[!svmInput4$ANNONO==2017,]
svmInput4 <- cbind(svmInput4,NA)



for(week in unique(svmInput3$SETTIMANANO)) {
  # print("week")
  # print(week)
  
  for(deptCycle in unique(svmInput3$REPARTO)){
    
    # print("dept")
    # print(deptCycle)
    # print(colMeans(svmInput3[which(as.numeric( svmInput3$SETTIMANANO ) == week  &  as.numeric(svmInput3$REPARTO) == deptCycle  ), ]))
    # 
    svmInput3[which(as.numeric( svmInput3$SETTIMANANO ) == week  &  as.numeric(svmInput3$REPARTO) == deptCycle  ), 5] <- colMeans(svmInput3[which(as.numeric( svmInput3$SETTIMANANO ) == week  &  as.numeric(svmInput3$REPARTO) == deptCycle  ), ])[4]
  }
}
# View(svmInput3)
colnames(svmInput3) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT1","MEDIAREPARTO")
svmInput3 <- svmInput3[,2:ncol(svmInput3)]
svmInput3 <- svmInput3[,c(1,2,4,3)]

predictDataFrame3 <- svmInput3[c(57,60,63),]
predictDataFrame3[,4] <- 0




for(week in unique(svmInput4$SETTIMANANO)) {
  # print("week")
  # print(week)
  
  for(deptCycle in unique(svmInput4$REPARTO)){
    
    # print("dept")
    # print(deptCycle)
    # print(colMeans(svmInput3[which(as.numeric( svmInput3$SETTIMANANO ) == week  &  as.numeric(svmInput3$REPARTO) == deptCycle  ), ]))
    # 
    svmInput4[which(as.numeric( svmInput4$SETTIMANANO ) == week  &  as.numeric(svmInput4$REPARTO) == deptCycle  ), 5] <- colMeans(svmInput4[which(as.numeric( svmInput4$SETTIMANANO ) == week  &  as.numeric(svmInput4$REPARTO) == deptCycle  ), ])[4]
  }
}
# View(svmInput3)
colnames(svmInput4) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT2","MEDIAREPARTO")
svmInput4 <- svmInput4[,2:ncol(svmInput4)]
svmInput4 <- svmInput4[,c(1,2,4,3)]

predictDataFrame4 <- svmInput4[1:45,]
predictDataFrame4[,4] <- 0


train_controlcv <- trainControl(method="cv", number=15)
train_controlrcv <- trainControl(method="repeatedcv", number=15, repeats=4)




svmRadialFit13rd <- train(VALORETOT1 ~ .,
                       data = svmInput3,
                       method = "svmRadial",
                       tuneLength = 15 )

svmRadialFit23rd <- train(VALORETOT1 ~ .,
                       data = svmInput3,
                       method = "svmRadial",
                       tuneLength = 15 , trControl=train_controlcv)
svmRadialFit33rd <- train(VALORETOT1 ~ .,
                       data = svmInput3,
                       method = "svmRadial",
                       tuneLength = 15 , trControl=train_controlrcv)

# svmRadialSigmaFit13rd <- train(VALORETOT1 ~ .,
#                             data = svmInput3,
#                             method = "svmRadialSigma",
#                             tuneLength = 15)
# svmRadialSigmaFit23rd <- train(VALORETOT1 ~ .,
#                             data = svmInput3,
#                             method = "svmRadialSigma",
#                             trControl=train_controlcv,
#                             tuneLength = 15)
# svmRadialSigmaFit33rd <- train(VALORETOT1 ~ .,
#                             data = svmInput3,
#                             method = "svmRadialSigma",
#                             trControl=train_controlrcv,
#                             tuneLength = 15)



# svmRadialFit14th <- train(VALORETOT2 ~ .,
#                        data = svmInput4,
#                        method = "svmRadial",
#                        tuneLength = 15 )
# 
# svmRadialFit24th <- train(VALORETOT2 ~ .,
#                        data = svmInput4,
#                        method = "svmRadial",
#                        tuneLength = 15 , trControl=train_controlcv)
# svmRadialFit34th <- train(VALORETOT2 ~ .,
#                        data = svmInput4,
#                        method = "svmRadial",
#                        tuneLength = 15 , trControl=train_controlrcv)
# 
# svmRadialSigmaFit14th <- train(VALORETOT2 ~ .,
#                             data = svmInput4,
#                             method = "svmRadialSigma",
#                             tuneLength = 15)
# svmRadialSigmaFit24th <- train(VALORETOT2 ~ .,
#                             data = svmInput4,
#                             method = "svmRadialSigma",
#                             trControl=train_controlcv,
#                             tuneLength = 15)
# svmRadialSigmaFit34th <- train(VALORETOT2 ~ .,
#                             data = svmInput4,
#                             method = "svmRadialSigma",
#                             trControl=train_controlrcv,
#                             tuneLength = 15)


prediction13rd <- predict(svmRadialFit13rd$finalModel,predictDataFrame3[,-4])
prediction23rd <- predict(svmRadialFit23rd$finalModel,predictDataFrame3[,-4])
prediction33rd <- predict(svmRadialFit33rd$finalModel,predictDataFrame3[,-4])
# prediction43rd <- predict(svmRadialSigmaFit13rd$finalModel,predictDataFrame3[,-4])
# prediction53rd <- predict(svmRadialSigmaFit23rd$finalModel,predictDataFrame3[,-4])
# prediction63rd <- predict(svmRadialSigmaFit33rd$finalModel,predictDataFrame3[,-4])
# 
# prediction14th <- predict(svmRadialFit14th$finalModel,predictDataFrame4[,-4])
# prediction24th <- predict(svmRadialFit24th$finalModel,predictDataFrame4[,-4])
# prediction34th <- predict(svmRadialFit34th$finalModel,predictDataFrame4[,-4])
# prediction44th <- predict(svmRadialSigmaFit14th$finalModel,predictDataFrame4[,-4])
# prediction54th <- predict(svmRadialSigmaFit24th$finalModel,predictDataFrame4[,-4])
# prediction64th <- predict(svmRadialSigmaFit34th$finalModel,predictDataFrame4[,-4])





predictionMade3rd <- storeOrderedByWeek[469:513,c(1,2,3,4) ]

predictionMade3rd <- cbind(predictionMade3rd,prediction13rd,prediction23rd,prediction33rd)

# predictionMade3rd <- cbind(predictionMade3rd, storeOrderedByWeek[469:513, 5 ])
# 
# predictionMade3rd <- cbind(predictionMade3rd,prediction14th,prediction24th,prediction34th,prediction44th,prediction54th,prediction64th)

View(predictionMade3rd)

colnames(predictionMade3rd)[11]<- "VALORETOT2"

error13rd <- predictionMade3rd$VALORETOT1 - predictionMade3rd$prediction13rd
error23rd <- predictionMade3rd$VALORETOT1 - predictionMade3rd$prediction23rd
error33rd <- predictionMade3rd$VALORETOT1 - predictionMade3rd$prediction33rd
error43rd <- predictionMade3rd$VALORETOT1 - predictionMade3rd$prediction43rd
error53rd <- predictionMade3rd$VALORETOT1 - predictionMade3rd$prediction53rd
error63rd <- predictionMade3rd$VALORETOT1 - predictionMade3rd$prediction63rd




min(rmse(error13rd),rmse(error23rd),rmse(error33rd),rmse(error43rd),rmse(error53rd),rmse(error63rd))
min(mae(error13rd),mae(error23rd),mae(error33rd),mae(error43rd),mae(error53rd),mae(error63rd))

min(mean(abs((error13rd)/predictionMade3rd$VALORETOT1) * 100), mean(abs((error23rd)/predictionMade3rd$VALORETOT1) * 100), mean(abs((error33rd)/predictionMade3rd$VALORETOT1) * 100),
    mean(abs((error43rd)/predictionMade3rd$VALORETOT1) * 100), mean(abs((error53rd)/predictionMade3rd$VALORETOT1) * 100), mean(abs((error63rd)/predictionMade3rd$VALORETOT1) * 100))


getTrainPerf(svmRadialFit13rd)$TrainRMSE
getTrainPerf(svmRadialFit23rd)$TrainRMSE
getTrainPerf(svmRadialFit33rd)$TrainRMSE

getTrainPerf(svmRadialSigmaFit13rd)$TrainRMSE
getTrainPerf(svmRadialSigmaFit23rd)$TrainRMSE
getTrainPerf(svmRadialSigmaFit33rd)$TrainRMSE

min(getTrainPerf(svmRadialFit13rd)$TrainRMSE,getTrainPerf(svmRadialFit23rd)$TrainRMSE,getTrainPerf(svmRadialFit33rd)$TrainRMSE,
    getTrainPerf(svmRadialSigmaFit13rd)$TrainRMSE,getTrainPerf(svmRadialSigmaFit23rd)$TrainRMSE,getTrainPerf(svmRadialSigmaFit33rd)$TrainRMSE)

error14th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction14th
error24th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction24th
error34th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction34th
error44th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction44th
error54th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction54th
error64th <- predictionMade3rd$VALORETOT2 - predictionMade3rd$prediction64th

min(rmse(error14th),rmse(error24th),rmse(error34th),rmse(error44th),rmse(error54th),rmse(error64th))
min(mae(error14th),mae(error24th),mae(error3),mae(error44th),mae(error54th),mae(error64th))

min(mean(abs((error14th)/predictionMade3rd$VALORETOT2) * 100), mean(abs((error24th)/predictionMade3rd$VALORETOT2) * 100), mean(abs((error34th)/predictionMade3rd$VALORETOT2) * 100),
    mean(abs((error44th)/predictionMade3rd$VALORETOT2) * 100), mean(abs((error54th)/predictionMade3rd$VALORETOT2) * 100), mean(abs((error64th)/predictionMade3rd$VALORETOT2) * 100))


getTrainPerf(svmRadialFit14th)$TrainRMSE
getTrainPerf(svmRadialFit24th)$TrainRMSE
getTrainPerf(svmRadialFit34th)$TrainRMSE

getTrainPerf(svmRadialSigmaFit14th)$TrainRMSE
getTrainPerf(svmRadialSigmaFit24th)$TrainRMSE
getTrainPerf(svmRadialSigmaFit34th)$TrainRMSE

min(getTrainPerf(svmRadialFit14th)$TrainRMSE,getTrainPerf(svmRadialFit24th)$TrainRMSE,getTrainPerf(svmRadialFit34th)$TrainRMSE,
    getTrainPerf(svmRadialSigmaFit14th)$TrainRMSE,getTrainPerf(svmRadialSigmaFit24th)$TrainRMSE,getTrainPerf(svmRadialSigmaFit34th)$TrainRMSE)

