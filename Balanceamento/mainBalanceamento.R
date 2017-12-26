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
library('caTools')
source('evalByAcc.R')
source('evalByAuc.R')
library('readr')
library('DMwR')

dataset <- read.csv("BaseCar", skip=1, header = FALSE)
data <- dataset[,2:8]

# Remove '#' in the next lines to use SMOTE
#data$V8 <- as.factor(data$V8)
#data <- SMOTE(V8 ~ ., data, perc.over = 2000, perc.under=100)

table(data$V8)

X <- data.matrix(data[,1:6])
X[is.na(X)] <- 0
Y <-  data.matrix(data[,7])

#N <- 100
#MSE <- rep(0,N)
#SD <- rep(0,N)
#percentage <- rep(0,N)

xc1 <- X[which(Y == 0),]
xc2 <- X[which(Y == 1),]

#Usar SVM

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
DTest <- c(rep(1,nrow(xc1Test)),rep(-1,nrow(xc2Test)))
DTest <- as.factor(DTest)

DTrain <- c(rep(1,nrow(xc1Train)),rep(-1,nrow(xc2Train)))
DTrain <- as.factor(DTrain)
#DTrain <- as.factor(DTrain)
XTrain <- rbind(xc1Train,xc2Train)


foldList <- list()
foldList <- createFolds(1:nrow(XTrain), 10)
N <- 100

cost <- c(2^-5,2^-4,2^-3,2^-2,2^-1,2^0,2^1,2^2,2^3,2^4,2^5,2^6,2^7,2^8,2^9,2^10,2^11,2^12,2^13,2^14,2^15)
gamma <- c(2^-15,2^-14,2^-13,2^-12,2^-11,2^-10,2^-9,2^-8,2^-7,2^-6,2^-5,2^-4,2^-3,2^-2,2^-1,2^0,2^1,2^2,2^3)

accResult <- matrix(0, nrow = length(cost), ncol = length(gamma))
aucResult <- matrix(0, nrow = length(cost), ncol = length(gamma))

for(i in c(1:length(cost))) #c(1:length(cost)
{
  for(j in c(1:length(gamma))) #length(gamma)
  {
    #REALIZAR 30 VEZES PARA O DATA DE TESTE E FAZER A MÉDIA PARA SABER QUAL O MELHOR
    accResult[i,j] <- evalByAcc(XTrain, DTrain, foldList, cost[i], gamma[j])
    aucResult[i,j] <- evalByAuc(XTrain, DTrain, foldList, cost[i], gamma[j])
  }
}

svm.model <- svm(DTrain ~ ., data = XTrain, cost = 0.5, gamma = 1)
svm.pred <- predict(svm.model, XTest)

Pxc <- as.numeric(as.character(svm.pred))

#results[i] <- colAUC(svm.pred,DTest, plotROC=FALSE)
print((sum(1*(DTest == Pxc))/length(Pxc)))

k <- 1;
for(i in c(1:length(cost))) #c(1:length(cost)
{
  for(j in c(1:length(gamma))) #length(gamma)
  {
    plot(k, aucResult[i,j],xlim = c(0,400), ylim=c(0,1), col='red')
    par(new=T)
    k <- k + 1
  }
}