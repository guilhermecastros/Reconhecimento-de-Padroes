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
source('createFirstCostGenearation.R')
source('createFirstGammaGenearation.R')
source('calcFitness.R')
source('getDataset.R')
library('readr')
library('DMwR')

# 1 - diabetes
# 2 - BreastCancer
# 3 - iris
# 4 - Ionosphere
# 5 - Sonar
# 6 - voting
# 7 - liver disosters
# 8 - PimaIndiansDiabetes
# 9 - wine
# 10 - fertility_Diagnosis
# 11 - heart
# 12- hepatitis
# 13 - planning_relax
# 14 - tic-tac-toe
# 15 - haberman
data <- getDataset("haberman")
xc <- data[[1]]
Y <- data[[2]]
resultAcc <- rep(0,30)
iterations <- 0
while(iterations <= 30)
{
  message(iterations)
  iterations <- iterations + 1 
  index <- sample(dim(xc)[1])
  
  dimXTrain <- round(dim(xc)[1]*0.6)
  dimXTest <- round(dim(xc)[1]*0.2)
  dimXValidation <- round(dim(xc)[1]*0.2)
  
  XTest <- xc[index[1:dimXTest],]
  DTest <- Y[index[1:dimXTest]]
  
  XTrain <- xc[index[(dimXTest + 1): (dimXTrain + dimXTest)],]
  DTrain <- Y[index[(dimXTest + 1): (dimXTrain + dimXTest)]]
  
  XValidation <- xc[index[((dimXTest + dimXTrain) + 1): dim(xc)[1]],1:ncol(xc)]
  DValidation <- Y[index[((dimXTest + dimXTrain) + 1): dim(xc)[1]]]
  
  DTest <- as.factor(DTest)
  DTrain <- as.factor(DTrain)
  DValidation <- as.factor(DValidation)
  
  cost <- c(2^-5,2^-4,2^-3,2^-2,2^-1,2^0,2^1,2^2,2^3,2^4,2^5,2^6,2^7,2^8,2^9,2^10,2^11,2^12,2^13,2^14,2^15)
  gamma <- c(2^-15,2^-14,2^-13,2^-12,2^-11,2^-10,2^-9,2^-8,2^-7,2^-6,2^-5,2^-4,2^-3,2^-2,2^-1,2^0,2^1,2^2,2^3)
  
  costBin <- rep(0, length(cost))
  gammaBin <- rep(0, length(gamma))
  
  costBin[sample(1:length(cost), 1)] <- 1
  gammaBin[sample(1:length(gamma), 1)] <- 1
  
  
  #print("Best Accuracy Validation")
  #print(bestAcc)
  resultTest <- calcFitness(costBin, gammaBin, DTrain, XTrain, XTest, DTest)
  #print("Best Accuracy Test")
  #print(resultTest)
  resultAcc[iterations] <- resultTest
}
mean(resultAcc)