dataModel <-(appoggioDepts2017[which(appoggioDepts2017$REPARTO == 1 &
                                       appoggioDepts2017$SETTORE == 10 &
                                       appoggioDepts2017$GRUPPO == 1 &
                                       appoggioDepts2017$ENTE == 4 &
                                       appoggioDepts2017$FAMIGLIA == "1_") , c(4:6,8:ncol(appoggioDepts2017))])

dataModel <- dataModel[,-c(1,2,3)]



training <- data.frame(as.matrix(dataModel[1:15,1:26]))


test <- data.frame(as.matrix(dataModel[16:nrow(dataModel),1:26]))



# rownames(training) <- NULL
# colnames(training) <- NULL
# rownames(test) <- NULL
# colnames(test) <- NULL

#CROSS VALIDATION
# trControl <- trainControl(method = 'repeatedcv', number = 10, repeats = 10, savePredictions = T)
# print("train")
# mode(training) = "numeric"
# mode(test) = "numeric"
#   ALLENO MODELLO
# svmFit <- 0
try({svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial",tuneLength = 40)})
# if (class(svmFit) == "numeric"){
#   try({svmFit <- caret::train( training[,-ncol(training)], training[,ncol(training)] , method="svmRadial",tuneLength = 40)})
# }
# svmFit <- train(training[,-indexTrn],training[,indexTrn],method="svmRadial",tuneLength=15, trnControl=bootControl,preProcess = preProc ) 

# SELEZIONO IL MODELLO MIGLIORE TRA QUELLI GENERATI
svmBest <-svmFit$finalModel    #modello migliore trovato con i parametri forniti

# ESEGUO PREVISIONE SUI DATI DI TEST
predsvm <- predict(svmBest, test[,-ncol(test)])
# INCOLLO IN FONDO AI DATI DI TEST IL VALORE DELLA PREVISIONE, PER UTILIZZARLA NELLA PROSSIMA PREVISIONE
actualTS <- test[,ncol(test)]

predictedTS <- predsvm
print(predsvm)

plot(y = training[1,], x= 1:26, type="l" , col="blue", ylim=c(min(training),max(training))) 
lines(y = training[2,], x= 1:26,type="b",col="red")
lines(y = training[3,], x= 1:26,type="b",col="red")
lines(y = training[4,], x= 1:26,type="b",col="red")
lines(y = training[5,], x= 1:26,type="b",col="red")
lines(y = training[6,], x= 1:26,type="b",col="red")
lines(y = training[7,], x= 1:26,type="b",col="red")
lines(y = training[8,], x= 1:26,type="b",col="red")
lines(y = training[9,], x= 1:26,type="b",col="red")
lines(y = training[10,], x= 1:26,type="b",col="red")
lines(y = training[11,], x= 1:26,type="b",col="red")
lines(y = training[12,], x= 1:26,type="b",col="red")
lines(y = training[13,], x= 1:26,type="b",col="red")
lines(y = training[14,], x= 1:26,type="b",col="red")
lines(y = training[15,], x= 1:26,type="b",col="red")
lines(y = training[16,], x= 1:26,type="b",col="red")
lines(y = training[17,], x= 1:26,type="b",col="red")