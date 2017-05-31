svmInput <- storeOrderedByWeek[which(as.numeric(storeOrderedByWeek$REPARTO) == 3), ]
svmInput <- svmInput[,c(1,2,3,4,10,16)]



svmInput <- cbind(svmInput,paste(svmInput[,1],paste( "W",formatC(format="d",svmInput[,2] ,flag="0",width=ceiling(log10(max(svmInput[,2])))), sep="" )   ,1,sep="-"))


testRegex <- svmInput[,7]
svmInput[,7]<-ISOweek2date(testRegex)
colnames(svmInput) <- c("ANNONO","SETTIMANANO","REPARTO","VALORETOT","VALOREPROMO","QUANTITA","DATAINTERA")
# NON SERVE PIù MA CI HO MESSO TROPPO A FARLO PER CANCELLARLo
# svmInput[,7] <- ISOweek2date( paste(testRegex[-grep("\\b\\d{4}-W53", ISOweek(as.Date(ISOweek2date (paste(week_iso,1,sep="-"))) + (0:(length(svmInput[,7])))*7) , perl=TRUE, value=FALSE)], 1, sep="-"))
#  

appoggio <- svmInput[,c(1,2,4)]


# zooObj <- read.zoo(appoggio,index = "DATAINTERA" )

# View(zooObj)

# names(zooObj)<- c("VALORE")

# svmApp <- merge(lag(zooObj,1),lag(zooObj,2),all=FALSE)


# svmData <- merge(zooObj,svmApp)


# ###############################################TUTORIAL



# Plot the data
plot(appoggio, pch=16)

# Create a linear regression model


# display the predictions

rmse <- function(error)
{
  sqrt(mean(error^2))
}





model <- svm(VALORETOT ~ ANNONO + SETTIMANANO , appoggio)

predictedY <- predict(model, appoggio)

error <- appoggio$VALORETOT - predictedY
svrPredictionRMSE <- rmse(error)

tuneResult <- tune(svm, VALORETOT ~ ANNONO + SETTIMANANO,  data = appoggio,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
# best performance: MSE = 8.371412, RMSE = 2.89 epsilon 1e-04 cost 4
# Draw the tuning graph
plot(tuneResult)


tuneResult <- tune(svm, VALORETOT ~ ANNONO + SETTIMANANO,  data = appoggio,
                   ranges = list(epsilon = seq(0.1,0.2,0.005), cost = 2^(2:9))
) 

print(tuneResult)
plot(tuneResult)


tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, appoggio) 

error <- appoggio$VALORETOT - tunedModelY  

# this value can be different on your computer
# because the tune method  randomly shuffles the data
tunedModelRMSE <- rmse(error)  # 2.219642  
# ###############################################TUTORIAL


trainIndex <- 1:(nrow(svmData)*0.75)
training <- as.data.frame(svmData[trainIndex])
rownames(training) <- NULL

test <- as.data.frame(svmData[-trainIndex])
rownames(test) <- NULL



bootControl <- trainControl(number=100)    #definisce il comportamento di apprendimento (k-folds cross validation?)
preProc <- c("center","scale")                #imposta i parametri per il preprocessing
set.seed(2)                                #setta uno scenario casuale
indexTrn <- ncol(training)                    # ???

#creazione del modello di apprendimento
#svmFit=train(training[,-indexTrn],training[,indexTrn],method="svmRadial",tuneLength=25, trnControl=bootControl)

svmFit <- train(training[,-indexTrn],training[,indexTrn],method="svmRadial",tuneLength=25, trnControl=bootControl)

svmBest <- svmFit$finalModel    #modello migliore trovato con i parametri forniti
predsvm <- predict(svmBest, test[,-ncol(test)])
actualTS <- test[,ncol(test)]
predictedTS <- predsvm

mse(actual=actualTS, predicted=predictedTS)
mae(actual=actualTS, predicted=predictedTS )








