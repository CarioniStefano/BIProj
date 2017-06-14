library(mlbench)
library(rpart)



svmInput <- storeOrderedByWeek[,c(1,2,3,4) ]

predictionMade <- storeOrderedByWeek[ c(469,470,471,472,473,474,475,476,477) , c(1,2,3,4) ]


for(week in unique(predictionMade)){
  
  
  
}

# svmInput <- cbind(svmInput,paste(svmInput[,1],paste( "W",formatC(format="d",svmInput[,2] ,flag="0",width=ceiling(log10(max(svmInput[,2])))), sep="" )   ,1,sep="-"))
# 
# 
# testRegex <- svmInput[,7]
# svmInput[,7]<-ISOweek2date(testRegex)
# colnames(svmInput) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT","VALOREPROMO","QUANTITA","DATAINTERA")
# NON SERVE PI? MA CI HO MESSO TROPPO A FARLO PER CANCELLARLo
# svmInput[,7] <- ISOweek2date( paste(testRegex[-grep("\\b\\d{4}-W53", ISOweek(as.Date(ISOweek2date (paste(week_iso,1,sep="-"))) + (0:(length(svmInput[,7])))*7) , perl=TRUE, value=FALSE)], 1, sep="-"))
#  


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

svmInput <- svmInput[,c(2,3,4)]


predictDataFrame <- svmInput[1:9,]
predictDataFrame[,3] <- 0








train_controlcv <- trainControl(method="cv", number=10)
train_controlrcv <- trainControl(method="repeatedcv", number=10, repeats=3)


# svmPolyFit <- train(VALORETOT1 ~ .,
#                     data = svmInput,
#                     method = "svmPoly",
#                     tuneLength = 12)

svmRadialFit1 <- train(VALORETOT1 ~ .,
                    data = svmInput,
                    method = "svmRadial",
                    tuneLength = 12 )

svmRadialFit2 <- train(VALORETOT1 ~ .,
                      data = svmInput,
                      method = "svmRadial",
                      tuneLength = 12 , trControl=train_controlcv)
svmRadialFit3 <- train(VALORETOT1 ~ .,
                       data = svmInput,
                       method = "svmRadial",
                       tuneLength = 12 , trControl=train_controlrcv)

svmRadialSigmaFit1 <- train(VALORETOT1 ~ .,
                      data = svmInput,
                      method = "svmRadialSigma",
                      tuneLength = 12)
svmRadialSigmaFit2 <- train(VALORETOT1 ~ .,
                           data = svmInput,
                           method = "svmRadialSigma",
                           trControl=train_controlcv,
                           tuneLength = 12)
svmRadialSigmaFit3 <- train(VALORETOT1 ~ .,
                           data = svmInput,
                           method = "svmRadialSigma",
                           trControl=train_controlrcv,
                           tuneLength = 12)



prediction1 <- predict(svmRadialFit1$finalModel,predictDataFrame[,-3])
prediction2 <- predict(svmRadialFit2$finalModel,predictDataFrame[,-3])
prediction3 <- predict(svmRadialFit3$finalModel,predictDataFrame[,-3])
prediction4 <- predict(svmRadialSigmaFit1$finalModel,predictDataFrame[,-3])
prediction5 <- predict(svmRadialSigmaFit2$finalModel,predictDataFrame[,-3])
prediction6 <- predict(svmRadialSigmaFit3$finalModel,predictDataFrame[,-3])




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
