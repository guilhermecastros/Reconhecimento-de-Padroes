rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')
library('RSNNS')
library('mclust')
library('plot3D')
library('e1071')
library(simone)

rm(list=ls())

## load the data set
data(cancer)
summary(cancer$expr)
X <- data.matrix(cancer$expr)
X[is.na(X)] <- 0

data.matrix(cancer$status)

Y <- matrix(0, nrow = dim(X)[1], ncol = 2)
Y[which(cancer$status == 'not'),1] <- 1
Y[which(cancer$status == 'pcr'),2] <- 1

xc1 <- X[which(cancer$status == 'not'),]
xc2 <- X[which(cancer$status == 'pcr'),]

meanx <- colMeans(X)
Xs<- X - t(replicate(dim(X)[1],meanx))
S<-cov(Xs)
eigS<-eigen(S)
plot(c(1 : ncol(X)),eigS$values,type='b',xlab='Eixo PCA',ylab='Autovalor')

projX<-Xs %*% eigS$vectors

iC1 <- which(cancer$status == 'not')
iC2 <- which(cancer$status == 'pcr')

maxX <- max(max(projX[iC1,1]),max(projX[iC2,1]))
minX <- min(min(projX[iC1,1]),min(projX[iC2,1]))
maxY <- max(max(projX[iC1,2]),max(projX[iC2,2]))
minY <- min(min(projX[iC1,2]),min(projX[iC2,2]))


plot(projX[iC1,1],projX[iC1,2], col='blue', xlim=c(-1 + minX,maxX), ylim=c(-1 + minY,maxY), xlab ='', ylab='')
par(new=TRUE)
plot(projX[iC2,1], projX[iC2,2], col='red', xlim=c( -1 + minX,maxX), ylim=c(-1 + minY,maxY), xlab ='', ylab='')

points3d(projX[iC1,1], projX[iC1,2], projX[iC1,3], col = "blue", size = 6, xlab="x", ylab="Y", zlab = "", add=T)
points3d(projX[iC2,1], projX[iC2,2], projX[iC2,3], col = "red", size = 6, xlab="x", ylab="Y", zlab = "", add=T)
axes3d()



######################################## MÉTODO SVM ######################################## 

# best cost -> cost[1]
# best gamma -> gamma[1]

N <- 10
MSE <- rep(0,N)
SD <- rep(0,N)
percentage <- rep(0,N)
for(i in 1:N){
  
  cost <- seq(1,1000,10)
  gamma <- seq(0.1, 10, 0.1)
  data(cancer)
  summary(cancer$expr)
  X <- data.matrix(cancer$expr)
  X[is.na(X)] <- 0
  
  data.matrix(cancer$status)
  
  Y <- matrix(0, nrow = dim(X)[1], ncol = 2)
  Y[which(cancer$status == 'not'),1] <- 1
  Y[which(cancer$status == 'pcr'),2] <- 1
  
  xc1 <- X[which(cancer$status == 'not'),]
  xc2 <- X[which(cancer$status == 'pcr'),]
  
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
  
  svm.model <- svm(DTrain ~ ., data = XTrain, cost = cost[1], gamma = gamma[1])
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

# best cost -> cost[1]
# best gamma -> gamma[1]

N <- 10
MSE <- rep(0,N)
SD <- rep(0,N)
percentage <- rep(0,N)
for(i in 1:N){
  
  cost <- seq(1,1000,10)
  gamma <- seq(0.1, 10, 0.1)
  data(cancer)
  summary(cancer$expr)
  X <- data.matrix(cancer$expr)
  X[is.na(X)] <- 0
  
  data.matrix(cancer$status)
  
  Y <- matrix(0, nrow = dim(X)[1], ncol = 2)
  Y[which(cancer$status == 'not'),1] <- 1
  Y[which(cancer$status == 'pcr'),2] <- 1
  
  xc1 <- projX[which(cancer$status == 'not'),1:3]
  xc2 <- projX[which(cancer$status == 'pcr'),1:3]
  
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
  
  svm.model <- svm(DTrain ~ ., data = XTrain, cost = cost[1], gamma = gamma[1])
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





