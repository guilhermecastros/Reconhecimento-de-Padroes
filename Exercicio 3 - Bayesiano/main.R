rm(list=ls())
library('bmp')
library('rgl')
library('caret')

pdfnvar <- function(x,m,K,n)((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*((x-m)%*%(solve(K))%*%t(x-m))))
# pdfnvar retorna -> P(x|ci)
#P(ci) é a probabilidade de estar no conjunto, 2/3 e 1/3 para este exercício
#P(ci|x) = P(x|ci)*P(ci)

data(iris)
plot(iris)

seqj <- seq(1,30)

storage <- rep(0,30)

for(j in seqj)
{
  
  setosa <- iris[1:50,]
  others <- iris[51:dim(iris)[1],]
  
  index <- sample(dim(setosa)[1])
  
  setosaTest <- setosa[index[1:(dim(setosa)[1]*0.3)],]
  setosaTrain <- setosa[index[(dim(setosa)[1]*0.3 + 1):dim(setosa)[1]],]
  
  index <- sample(dim(others)[1])
  
  othersTest <- others[index[1:(dim(others)[1]*0.3)],]
  othersTrain <- others[index[(dim(others)[1]*0.3 + 1):dim(others)[1]],]
  
  sepalLengthSetosaTrain <- mean(setosaTrain[,1])
  sepalWidthSetosaTrain <- mean(setosaTrain[,2])
  petalLengthSetosaTrain <- mean(setosaTrain[,3])
  petalWidthSetosaTrain <- mean(setosaTrain[,4])
  
  mSetosa <- c(sepalLengthSetosaTrain,sepalWidthSetosaTrain,petalLengthSetosaTrain,petalWidthSetosaTrain)
  kSetosa <- cov(cbind(setosaTrain[,1],setosaTrain[,2],setosaTrain[,3],setosaTrain[,4]))
  
  sepalLengthOthersTrain <- mean(othersTrain[,1])
  sepalWidthOthersTrain <- mean(othersTrain[,2])
  petalLengthOthersTrain <- mean(othersTrain[,3])
  petalWidthOthersTrain <- mean(othersTrain[,4])
  
  mOthers <- c(sepalLengthOthersTrain,sepalWidthOthersTrain,petalLengthOthersTrain,petalWidthOthersTrain)
  kOthers <- cov(cbind(othersTrain[,1],othersTrain[,2],othersTrain[,3],othersTrain[,4]))
  
  allTestData <- rbind(setosaTest, othersTest)
  allTrainData <- rbind(setosaTrain, othersTrain)
  
  #
  np <- dim(allTrainData)[1]
  
  seqi <- seq(1,np)
  
  M1 <- rep(0,dim(allTrainData)[1])
  M2 <- rep(0,dim(allTrainData)[1])
  Mpre <- rep(0,dim(allTrainData)[1])
  
  Mres <- c(rep(1,dim(setosaTrain)[1]),rep(0,dim(othersTrain)[1]))
  
  for(i in seqi){
    M1[i] <- pdfnvar(as.matrix(allTrainData[i,1:4]),t(mSetosa),kSetosa,4)*2/3
    M2[i] <- pdfnvar(as.matrix(allTrainData[i,1:4]),mOthers,kOthers,4)*1/3
    Mpre[i] <- 1*(M1[i]>M2[i])
  }
  
  table(Mpre, Mres)
  
  #
  
  np <- dim(allTestData)[1]
  
  seqi <- seq(1,np)
  
  M1 <- rep(0,dim(allTestData)[1])
  M2 <- rep(0,dim(allTestData)[1])
  Mpre <- rep(0,dim(allTestData)[1])
  
  Mres <- c(rep(1,dim(setosaTest)[1]),rep(0,dim(othersTest)[1]))
  
  for(i in seqi){
    M1[i] <- pdfnvar(as.matrix(allTestData[i,1:4]),t(mSetosa),kSetosa,4)*2/3
    M2[i] <- pdfnvar(as.matrix(allTestData[i,1:4]),mOthers,kOthers,4)*1/3
    Mpre[i] <- 1*(M1[i]>M2[i])
  }
  
  matrixData <- table(Mpre, Mres)
  
  storage[j] <- matrixData[1,2] + matrixData[2,1] 
  sd(storage)
  mean(storage)
  #cMatrix <- confusionMatrix(Mpre, Mres)
  #print(cMatrix)
}


