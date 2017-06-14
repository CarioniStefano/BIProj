options(scipen = 999)
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

svmInput <- storeOrderedByWeek[,c(1,2,3,4) ]
svmInput <- svmInput[!svmInput$ANNONO==2017,]

predictionMade <- storeOrderedByWeek[c(469,470,471,472,473,474,475,476,477),c(1,2,3,4) ]

newCol <- data.frame()

for(week in unique(svmInput$SETTIMANANO)) {
   

  newCol<-rbind(newCol, colMeans(svmInput[which(as.numeric( svmInput$SETTIMANANO ) == week),]) )

}

newCol <- newCol[rep(1:nrow(newCol),each=3),] 
rowsToDuplicate <- nrow( newCol )



for(year in ( tail(unique(svmInput$ANNONO) ,-1) )){
  newCol <- rbind(newCol,newCol[1:rowsToDuplicate,])
}



svmInput <- cbind(svmInput,newCol[,4])
  
svmInput <- svmInput[,c(2:3,5,4)]
colnames(svmInput) <-c("SETTIMANANO","REPARTO","MEDIASETTIMANA","VALORETOT1")





predictDataFrame <- svmInput[1:9,]
predictDataFrame[,4] <- 0






library(mlbench)
library(rpart)

train_controlcv <- trainControl(method="cv", number=15)
train_controlrcv <- trainControl(method="repeatedcv", number=15 , repeats=5)
# 
# svmPolyFit <- train(VALORETOT1 ~ .,
#                     data = svmInput,
#                     method = "svmPoly",
#                     tuneLength = 12)

svmRadialFit1 <- train(VALORETOT1 ~ .,
                       data = svmInput,
                       method = "svmRadial",
                       tuneLength = 20 )

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
                            tuneLength = 12)



prediction1 <- predict(svmRadialFit1$finalModel,predictDataFrame[,-4])
prediction2 <- predict(svmRadialFit2$finalModel,predictDataFrame[,-4])
prediction3 <- predict(svmRadialFit3$finalModel,predictDataFrame[,-4])
prediction4 <- predict(svmRadialSigmaFit1$finalModel,predictDataFrame[,-4])
prediction5 <- predict(svmRadialSigmaFit2$finalModel,predictDataFrame[,-4])
prediction6 <- predict(svmRadialSigmaFit3$finalModel,predictDataFrame[,-4])




predictionMade <- storeOrderedByWeek[c(469,470,471,472,473,474,475,476,477),c(1,2,3,4) ]
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


