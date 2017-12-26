rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')
library('RSNNS')
library('mclust')
library('plot3D')
library('e1071')
library('class')
source('evalK.r')

data(BreastCancer)
summary(BreastCancer)
X <- data.matrix(BreastCancer[,2:10])
X[is.na(X)] <- 0
Y <- matrix(0, nrow = dim(X)[1], ncol = 2)
Y[which(BreastCancer$Class == 'benign'),1] <- 1
Y[which(BreastCancer$Class == 'malignant'),2] <- 1

N <- 100
MSE <- rep(0,N)
SD <- rep(0,N)
percentage <- rep(0,N)

xc1 <- X[which(BreastCancer$Class == 'benign'),]
xc2 <- X[which(BreastCancer$Class == 'malignant'),]

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

XTest <-  rbind(xc1Test,xc2Test)
DTest <- c(rep(1,nrow(xc1Test)),rep(2,nrow(xc2Test)))

DTrain <- c(rep(1,nrow(xc1Train)),rep(2,nrow(xc2Train)))
#DTrain <- as.factor(DTrain)
XTrain <- rbind(xc1Train,xc2Train)

foldList <- list()
foldList <- createFolds(1:nrow(XTrain), 10)

kResult <- rep(0,round(N/3))

for(k in c(1:round(N/3)))
{
  #REALIZAR 30 VEZES PARA O DATA DE TESTE E FAZER A MÉDIA PARA SABER QUAL O MELHOR
  kResult[k] <- evalK(k, XTrain, DTrain, foldList)
}

K <- which(kResult == max(kResult))

XResult <- rep(0,nrow(XTest))

for(j in c(1:nrow(XTest))){
  #XResult[j] <- KNN(XTest[j,],XTrain,K, DTrain)
  XResult[j] <- knn(XTrain,XTest[j,],DTrain,K)
}

percentage <- (sum(1*(XResult == DTest))/length(DTest))
print(percentage)
print(K)