rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')
library('RSNNS')
library('mclust')

data(BreastCancer)
summary(BreastCancer)
X <- data.matrix(BreastCancer[,2:10])
X[is.na(X)] <- 0
Y <- matrix(0, nrow = dim(X)[1], ncol = 2)
Y[which(BreastCancer$Class == 'benign'),1] <- 1
Y[which(BreastCancer$Class == 'malignant'),2] <- 1

N <- 10
MSE <- rep(0,N)
SD <- rep(0,N)
percentage <- rep(0,N)

for(k in 1:N)
{
  
  xc1 <- X[which(BreastCancer$Class == 'benign'),]
  xc2 <- X[which(BreastCancer$Class == 'malignant'),]
  
  index <- sample(dim(xc1)[1])
  
  xc1Test <- xc1[index[1:137],]
  xc1Train <- xc1[index[(137 + 1):dim(xc1)[1]],]
  
  index <- sample(dim(xc2)[1])
  
  xc2Test <- xc2[index[1:72],]
  xc2Train <- xc2[index[(72 + 1):dim(xc2)[1]],]
  
  #Modelos
  modelxc1<-densityMclust(xc1Train)
  modelxc2<-densityMclust(xc2Train)
  
  dataTest <- as.matrix(rbind(xc1Test,xc2Test))
  
  PxC1 <- as.matrix(dens(modelName=modelxc1$modelName, data = dataTest, parameters = modelxc1$parameters))
  PxC2 <- as.matrix(dens(modelName=modelxc2$modelName, data = dataTest, parameters = modelxc2$parameters))
  
  prediction <- rep(0, dim(dataTest)[1])
  
  prioriC1 <- dim(xc1Test)[1]/dim(dataTest)[1]
  prioriC2 <- dim(xc2Test)[1]/dim(dataTest)[1]
  
  for(i in 1: dim(dataTest)[1]){
    if(PxC1[i,1]*prioriC1>= PxC2[i,1]*prioriC2)
      prediction[i] <- 1
    else
      prediction[i] <- 2
  }
  
  xc1Y <- rep(1,dim(xc1Test)[1])
  xc2Y <- rep(2,dim(xc2Test)[1])
  
  realOut <- c(xc1Y,xc2Y)
  
  MSE[k] <- mean((prediction-realOut)^2)
  SD[k] <- sd(prediction-realOut)
  percentage[k] <- sum(1*(realOut == prediction))/length(realOut)
}
  
plot(MSE, xlab='', ylab='',main='Erro Quadrático Médio (MSE)')
plot(SD, xlab='', ylab='',main='Desvio Padrão (SD)')
plot(percentage, xlab='', ylab='',main='Acerto Percentual')
mean(percentage)
  
  
