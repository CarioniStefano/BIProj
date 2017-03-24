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


inputFile.z <- read.zoo(file = "~/Lavoro/InputFiles/features.csv", header = TRUE, index.column=2, sep = ",")
trainFile.z <- read.zoo(file = "~/Lavoro/InputFiles/train.csv", header = TRUE, index.column=3, sep = ",")

#View(trainFile.z)
#inputFile.z[i,1]

store1.z = inputFile.z[which(inputFile.z$Store == 1),]
#colnames(store1.z)[1] <- 1
store1.z$MarkDown1=NULL
store1.z$MarkDown2=NULL
store1.z$MarkDown3=NULL
store1.z$MarkDown4=NULL
store1.z$MarkDown5=NULL
store1.z$Temperature = NULL
store1.z$Fuel_Price = NULL
store1.z$Unemployment = NULL
store1.z$IsHoliday = NULL
store1.z$Store=NULL
store1.z=head(store1.z,-13)


for (i in 2:45) {
  storetemp.z = inputFile.z[which(inputFile.z$Store == i),]
  storetemp.z$MarkDown1=NULL
  storetemp.z$MarkDown2=NULL
  storetemp.z$MarkDown3=NULL
  storetemp.z$MarkDown4=NULL
  storetemp.z$MarkDown5=NULL
  storetemp.z$Temperature = NULL
  storetemp.z$Fuel_Price = NULL
  storetemp.z$Unemployment = NULL
  storetemp.z$IsHoliday = NULL
  storetemp.z$Store=NULL
  storetemp.z=head(storetemp.z,-13)
  store1.z = cbind(store1.z,storetemp.z)
  colnames(store1.z)[i] <- i
  
}





store1.mat=coredata(store1.z)





#
#par(mfrow=c(2,2))
#histinfo = hist(store1.mat[,3],main="CPI", probability=TRUE, col="red", xlab="CPI")
#histinfo
#plot(density(na.omit(store1.mat[,3])),type="l",xlab="CPI", col="blue", lwd=2,ylab="density estimate",main="Smoothed CPI histogram")
#boxplot(store1.mat[,3], main="Boxplot CPI",ylab="CPI", col="red")
#qqnorm(store1.mat[,3],col="blue")
#qqline(store1.mat[,3])

for (i in 1:ncol(store1.mat)) {
  
  print("////////////////////////////////////////////////////////////////")
  
  print(colnames(store1.mat)[i])
  
  print("MEDIA :  measures the centre of mass of the pdf")
  print(mean(na.omit(store1.mat[,i])))
  print("Varianza : measures the spread of the distribution about the mean ")
  print(var(na.omit(store1.mat[,i])))
  
  #print((1) / (var(na.omit(store1.mat[,i]))))
  
  print("deviazione standard")
  print(sd(na.omit(store1.mat[,i])))
  
  print("skewness: if  = 0 symmetric (Normal distribution) , if > 0  pdf long right tail (log normal), if < 0 pdf long left tail.")
  print(skewness(na.omit(store1.mat[,i])))
  
  print("kurtosi: > 0 fatter tail than normal , < 0 thinner tails than normal, =0 same tails as normal distr.")
  print(kurtosis(na.omit(store1.mat[,i])))
  
  print("quantili")
  print(quantile(store1.mat[,i],probs=c(0.01,0.05), na.rm=TRUE))
  #print(qnorm(p=c(0.01,0.05),mean=mean(na.omit(store1.mat[,i])),sd=sd(na.omit(store1.mat[,i]))))
  
}





par(mfrow=c(4,2))

############PLOTTARE TUTTO
#plot(store1.z, plot.type="single", col=c("blue","red","green","orange","black","chocolate3","cornflowerblue","cyan2","darkgoldenrod","darkorchid1","lightpink","brown4"), lty=1, lwd=2)
#legend(x="topleft", legend=c("Store","Temp","Fuel Price","Mark1","Mark2","Mark3","Mark4","Mark5","CPI","Uneloyment","Holiday"),col=c("blue","red","green","orange","black","chocolate3","cornflowerblue","cyan2","darkgoldenrod","darkorchid1","lightpink","brown4"), lty=1,lwd=3)
#############

#storeCut.z = head(store1.z,-13)

############PLOTTO SENZA OFFERTE E STORE ID
plot(store1.z, plot.type="single", col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1, lwd=2, xaxt = "n")
tt=time(store1.z)


ix <- seq(1, length(tt), by=7) #every 60 days
fmt <- "%b-%d" # format for axis labels
labs <- format(tt[ix], fmt)
axis(side = 1, at = tt[ix], labels = labs,  cex.axis = 0.7)
legend(x="topleft", legend=c("Temp","Fuel Price","CPI","Uneloyment","Holiday"),col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1,lwd=3)
###################################

############PLOTTO SENZA OFFERTE E STORE ID
plot(store2.z, plot.type="single", col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1, lwd=2, xaxt = "n")
tt=time(store2.z)


ix <- seq(1, length(tt), by=7) #every 60 days
fmt <- "%b-%d" # format for axis labels
labs <- format(tt[ix], fmt)
axis(side = 1, at = tt[ix], labels = labs,  cex.axis = 0.7)
legend(x="topleft", legend=c("Temp","Fuel Price","CPI","Uneloyment","Holiday"),col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1,lwd=3)
###################################

############PLOTTO SENZA OFFERTE E STORE ID
plot(store3.z, plot.type="single", col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1, lwd=2, xaxt = "n")
tt=time(store3.z)


ix <- seq(1, length(tt), by=7) #every 60 days
fmt <- "%b-%d" # format for axis labels
labs <- format(tt[ix], fmt)
axis(side = 1, at = tt[ix], labels = labs,  cex.axis = 0.7)
legend(x="topleft", legend=c("Temp","Fuel Price","CPI","Uneloyment","Holiday"),col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1,lwd=3)
###################################

############PLOTTO SENZA OFFERTE E STORE ID
plot(store4.z, plot.type="single", col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1, lwd=2, xaxt = "n")
tt=time(store4.z)


ix <- seq(1, length(tt), by=7) #every 60 days
fmt <- "%b-%d" # format for axis labels
labs <- format(tt[ix], fmt)
axis(side = 1, at = tt[ix], labels = labs,  cex.axis = 0.7)
legend(x="topleft", legend=c("Temp","Fuel Price","CPI","Uneloyment","Holiday"),col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1,lwd=3)
###################################


############PLOTTO SENZA OFFERTE E STORE ID
plot(store5.z, plot.type="single", col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1, lwd=2, xaxt = "n")
tt=time(store5.z)


ix <- seq(1, length(tt), by=7) #every 60 days
fmt <- "%b-%d" # format for axis labels
labs <- format(tt[ix], fmt)
axis(side = 1, at = tt[ix], labels = labs,  cex.axis = 0.7)
legend(x="topleft", legend=c("Temp","Fuel Price","CPI","Uneloyment","Holiday"),col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1,lwd=3)
###################################



############PLOTTO SENZA OFFERTE E STORE ID
plot(store6.z, plot.type="single", col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1, lwd=2, xaxt = "n")
tt=time(store6.z)


ix <- seq(1, length(tt), by=7) #every 60 days
fmt <- "%b-%d" # format for axis labels
labs <- format(tt[ix], fmt)
axis(side = 1, at = tt[ix], labels = labs,  cex.axis = 0.7)
legend(x="topleft", legend=c("Temp","Fuel Price","CPI","Uneloyment","Holiday"),col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1,lwd=3)
###################################



############PLOTTO SENZA OFFERTE E STORE ID
plot(store7.z, plot.type="single", col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1, lwd=2, xaxt = "n")
tt=time(store7.z)


ix <- seq(1, length(tt), by=7) #every 70 days
fmt <- "%b-%d" # format for axis labels
labs <- format(tt[ix], fmt)
axis(side = 1, at = tt[ix], labels = labs,  cex.axis = 0.7)
legend(x="topleft", legend=c("Temp","Fuel Price","CPI","Uneloyment","Holiday"),col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1,lwd=3)
###################################


############PLOTTO SENZA OFFERTE E STORE ID
plot(store8.z, plot.type="single", col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1, lwd=2, xaxt = "n")
tt=time(store8.z)


ix <- seq(1, length(tt), by=7) #every 70 days
fmt <- "%b-%d" # format for axis labels
labs <- format(tt[ix], fmt)
axis(side = 1, at = tt[ix], labels = labs,  cex.axis = 0.7)
legend(x="topleft", legend=c("Temp","Fuel Price","CPI","Uneloyment","Holiday"),col=c("blue","red","green","orange","black","chocolate3","cornflowerblue"), lty=1,lwd=3)
###################################



cov.mat=cov(cbind(store1.mat[,1] , store2.mat[,1] , store3.mat[,1] , store4.mat[,1] , store5.mat[,1] , store6.mat[,1] , store7.mat[,1] , store8.mat[,1]))
colnames(cov.mat)=c("ADS.F","NKE","NESN.VX","DNN.MI","MS.MI")
rownames(cov.mat)=c("ADS.F","NKE","NESN.VX","DNN.MI","MS.MI")
print(cov.mat)

cor.mat=cor(cbind(store1.mat[,1] , store2.mat[,1] , store3.mat[,1] , store4.mat[,1] , store5.mat[,1] , store6.mat[,1] , store7.mat[,1] , store8.mat[,1]))
colnames(cor.mat)=c("ADS.F","NKE","NESN.VX","DNN.MI","MS.MI")
rownames(cor.mat)=c("ADS.F","NKE","NESN.VX","DNN.MI","MS.MI")
print(cor.mat)

ScatterCC.ret.mat=cbind(store1.mat[,1] , store2.mat[,1] , store3.mat[,1] , store4.mat[,1] , store5.mat[,1] , store6.mat[,1] , store7.mat[,1] , store8.mat[,1])
colnames(ScatterCC.ret.mat)=c("ADS.F","NKE","NESN.VX","DNN.MI","MS.MI")
pairs(ScatterCC.ret.mat,pch=10,col="blue",cex=1.5,cex.axis=1.5)



