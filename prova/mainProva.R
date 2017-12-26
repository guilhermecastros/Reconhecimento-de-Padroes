rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')
library('RSNNS')
library('mclust')
library('plot3D')
library('e1071')

data(Sonar) # L^e os dados
xall<-Sonar[,(1:60)]
yall<-1*(Sonar[,61] == "M") # transforma as classes em valores binários

xall[is.na(xall)] <- 0

yall[which(yall == 0)] <- -1

xc1 <- xall[which(yall == -1),]
xc2 <- xall[which(yall == 1),]

meanx <- colMeans(xall)
Xs<- xall - t(replicate(dim(xall)[1],meanx))
S<-cov(Xs)
eigS<-eigen(S)
plot(c(1 : ncol(xall)),eigS$values,type='b',xlab='Eixo PCA',ylab='Autovalor')

################################## Escolha parametros ######################################## 

index <- sample(dim(xall)[1])

dimXTrain <- round(dim(xall)[1]*0.6)
dimXTest <- round(dim(xall)[1]*0.2)
dimXValidation <- round(dim(xall)[1]*0.2)

XTest <- xall[index[1:dimXTest],]
DTest <- yall[index[1:dimXTest]]

XTrain <- xall[index[(dimXTest + 1): (dimXTrain + dimXTest)],]
DTrain <- yall[index[(dimXTest + 1): (dimXTrain + dimXTest)]]

XValidation <- xall[index[((dimXTest + dimXTrain) + 1): dim(xall)[1]],1:ncol(xall)]
DValidation <- yall[index[((dimXTest + dimXTrain) + 1): dim(xall)[1]]]

DTest <- as.factor(DTest)
DTrain <- as.factor(DTrain)
DValidation <- as.factor(DValidation)

cost <- c(2^-5,2^-4,2^-3,2^-2,2^-1,2^0,2^1,2^2,2^3,2^4,2^5,2^6,2^7,2^8,2^9,2^10,2^11,2^12,2^13,2^14,2^15)
gamma <- c(2^-15,2^-14,2^-13,2^-12,2^-11,2^-10,2^-9,2^-8,2^-7,2^-6,2^-5,2^-4,2^-3,2^-2,2^-1,2^0,2^1,2^2,2^3)

percentageParam <- matrix(0, nrow=length(cost), ncol=length(gamma))

for(i in 1:length(cost))
{
  for(j in 1:length(gamma))
  {
    svm.model <- svm(DTrain ~ ., data = XTrain , cost = cost[i], gamma = gamma[j])
    svm.pred <- predict(svm.model, XValidation)
    
    Pxc <- as.numeric(as.character(svm.pred))
    
    percentageParam[i,j] <- (sum(1*(DValidation == Pxc))/length(Pxc))
  }
}

indexParam <- which(percentageParam == max(percentageParam), arr.ind = TRUE)
costParam <- cost[indexParam[1,1]]
gammaParam <- gamma[indexParam[1,2]]

################################## MÉTODO SVM ######################################## 
N <- 10
MSE <- rep(0,N)
SD <- rep(0,N)
percentage <- rep(0,N)
for(i in 1:N){
  
  cost <- seq(1,1000,10)
  gamma <- seq(0.1, 10, 0.1)
  X <- data.matrix(Sonar[,(1:60)])
  X[is.na(X)] <- 0
  Y <- matrix(0, nrow = dim(X)[1], ncol = 2)
  Y[which(Sonar$Class == 'R'),1] <- -1
  Y[which(Sonar$Class == 'M'),1] <- 1
  
  xc1 <- X[which(Sonar$Class == 'R'),]
  xc2 <- X[which(Sonar$Class == 'M'),]
  
  index <- sample(dim(xc1)[1])
  
  numOfTrainXC1 <- round(dim(xc1)[1]*0.7)
  numOfTestXC1 <- round(dim(xc1)[1]*0.3)
  
  xc1Test <- xc1[index[1:numOfTestXC1],]
  xc1Train <- xc1[index[(numOfTestXC1 + 1): dim(xc1)[1]],]
  
  index <- sample(dim(xc2)[1])
  
  numOfTrainXC2 <- round(dim(xc2)[1]*0.7)
  numOfTestXC2 <- round(dim(xc2)[1]*0.3)
  
  xc2Test <- xc2[index[1:numOfTestXC2],]
  xc2Train <- xc2[index[(numOfTestXC2 + 1):dim(xc2)[1]],]
  
  # formatando base de dados
  
  DTrain <- c(rep(-1,nrow(xc1Train)),rep(1,nrow(xc2Train)))
  DTrain <- as.factor(DTrain)
  XTrain <- rbind(xc1Train,xc2Train)
  
  
  DTest <- c(rep(-1,nrow(xc1Test)),rep(1,nrow(xc2Test)))
  #DTest <- as.factor(DTest)
  XTest <- rbind(xc1Test,xc2Test)  
  
  svm.model <- svm(DTrain ~ ., data = XTrain, cost = costParam, gamma = gammaParam)
  svm.pred <- predict(svm.model, XTest)
  
  Pxc <- as.numeric(as.character(svm.pred))
  
  percentage[i] <- (sum(1*(DTest == Pxc))/length(Pxc))
  MSE[i] <- mean((Pxc-DTest)^2)
  SD[i] <- sd(Pxc-DTest)
}

plot(MSE, xlab='', ylab='',main='Erro Quadrático Médio (MSE)')
plot(SD, xlab='', ylab='',main='Desvio Padrão (SD)')
plot(percentage, xlab='', ylab='',main='Acerto Percentual')
mean(percentage)


################################## MÉTODO SVM + PCA ######################################## 

# best cost -> cost[1]
# best gamma -> gamma[1]

N <- 10
MSE <- rep(0,N)
SD <- rep(0,N)
percentage <- rep(0,N)
for(i in 1:N){
  
  indexesPCA <- order(eigS$values, decreasing=T)
  X <- data.matrix(Sonar[,(indexesPCA[1:15])])
  X[is.na(X)] <- 0
  Y <- matrix(0, nrow = dim(X)[1], ncol = 2)
  Y[which(Sonar$Class == 'R'),1] <- -1
  Y[which(Sonar$Class == 'M'),1] <- 1
  
  xc1 <- X[which(Sonar$Class == 'R'),]
  xc2 <- X[which(Sonar$Class == 'M'),]
  
  index <- sample(dim(xc1)[1])
  
  numOfTrainXC1 <- round(dim(xc1)[1]*0.7)
  numOfTestXC1 <- round(dim(xc1)[1]*0.3)
  
  xc1Test <- xc1[index[1:numOfTestXC1],]
  xc1Train <- xc1[index[(numOfTestXC1 + 1): dim(xc1)[1]],]
  
  index <- sample(dim(xc2)[1])
  
  numOfTrainXC2 <- round(dim(xc2)[1]*0.7)
  numOfTestXC2 <- round(dim(xc2)[1]*0.3)
  
  xc2Test <- xc2[index[1:numOfTestXC2],]
  xc2Train <- xc2[index[(numOfTestXC2 + 1):dim(xc2)[1]],]
  
  # formatando base de dados
  
  DTrain <- c(rep(-1,nrow(xc1Train)),rep(1,nrow(xc2Train)))
  DTrain <- as.factor(DTrain)
  XTrain <- rbind(xc1Train,xc2Train)
  
  
  DTest <- c(rep(-1,nrow(xc1Test)),rep(1,nrow(xc2Test)))
  #DTest <- as.factor(DTest)
  XTest <- rbind(xc1Test,xc2Test)  
  
  svm.model <- svm(DTrain ~ ., data = XTrain, cost = costParam, gamma = gammaParam)
  svm.pred <- predict(svm.model, XTest)
  
  Pxc <- as.numeric(as.character(svm.pred))
  
  percentage[i] <- (sum(1*(DTest == Pxc))/length(Pxc))
  MSE[i] <- mean((Pxc-DTest)^2)
  SD[i] <- sd(Pxc-DTest)
}

plot(MSE, xlab='', ylab='',main='Erro Quadrático Médio (MSE)')
plot(SD, xlab='', ylab='',main='Desvio Padrão (SD)')
plot(percentage, xlab='', ylab='',main='Acerto Percentual')
mean(percentage)





