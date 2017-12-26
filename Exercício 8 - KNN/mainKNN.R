rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')
library('RSNNS')
library('mclust')


N <-100

xc1 <- matrix(c(rnorm(N/2)*0.5+3.5, rnorm(N/2)*0.5+3.5), N/2, 2)
xc2 <- matrix(c(rnorm(N/2)*0.5+2, rnorm(N/2)*0.5+2), N/2, 2)

plot(xc1[,1], xc1[,2], col='blue', xlim=c(0.5,5.5), ylim=c(0,6), xlab ='', ylab='')
par(new=T)
plot(xc2[,1], xc2[,2], col='red', xlim=c(0.5,5.5), ylim=c(0,6), xlab ='', ylab='')
par(new=T)
## Pode ser utilizado não só para classsificação, mas tbem para remoção de ruídos quando todos os pontos
## ligados aquela nova amostra são de uma classe
index <- sample(dim(xc1)[1])

numOfTrainXC1 <- round(dim(xc1)[1]*0.7)
numOfTestXC1 <- round(dim(xc1)[1]*0.3)

xc1Test <- xc1[index[1:numOfTestXC1],]
xc1Train <- xc1[index[(numOfTestXC1 + 1): dim(xc1)[1]],]


index <- sample(dim(xc2)[1])

numOfTrainXC2 <- round(dim(xc2)[1]*0.7)
numOfTestXC2 <- round(dim(xc2)[1]*0.3)

xc2Test <- xc2[index[1:numOfTestXC2],]
xc2Train <- xc2[index[(numOfTestXC2 + 1): dim(xc2)[1]],]

DTrain <- c(rep(-1,nrow(xc1Train)), rep(1, nrow(xc2Train)))
dataTrain <- rbind(xc1Train,xc2Train)

DTest <- c(rep(-1,nrow(xc1Test)), rep(1, nrow(xc2Test)))
dataTest <- rbind(xc1Test,xc2Test)

plot(dataTest[,1], dataTest[,2], col='green', xlim=c(0.5,5.5), ylim=c(0,6), xlab ='', ylab='')

K <- 10

plot(xc1[,1], xc1[,2], col='blue', xlim=c(0.5,5.5), ylim=c(0,6), xlab ='', ylab='')
par(new=T)
plot(xc2[,1], xc2[,2], col='red', xlim=c(0.5,5.5), ylim=c(0,6), xlab ='', ylab='')
par(new=T)

D_amostra <- rep(0, dim(dataTest)[1])
for(i in 1:dim(dataTest)[1])
{
  
  X <- rbind(dataTest[i,],dataTrain)
  
  distancias <- dist(X, method = "euclidean", diag = FALSE, upper = TRUE)
  distancias <- as.matrix(distancias)
  distancias <- distancias[,-1]
  
  # i indica qual dado esta sendo testado
  vizinhos <- order(distancias[1,])[1:K]
  #classificacao
  resultado <- sum(DTrain[vizinhos])
  Sys.sleep(2)
  if(resultado > 0)
  {
    par(new=T)
    plot(dataTest[i,1], dataTest[i,2], col='red', xlim=c(0.5,5.5), ylim=c(0,6), xlab ='', ylab='')
    D_amostra[i] <- -1
  }
  if(resultado < 0)
  {
    par(new=T)
    plot(dataTest[i,1], dataTest[i,2], col='blue', xlim=c(0.5,5.5), ylim=c(0,6), xlab ='', ylab='')
    D_amostra[i] <- 1
  }
}

##Criação do grid


KNN <- function(point,data,K,D){

  X <- rbind(point,data)
  
  distancias <- dist(X, method = "euclidean", diag = FALSE, upper = TRUE)
  distancias <- as.matrix(distancias)
  distancias <- distancias[,-1]
  
  vizinhos <- order(distancias[1,])[1:K]
  #classificacao
  resultado <- sum(D[vizinhos])
  
  if(resultado >= 0)
  {
    return (-1)
  }
  if(resultado < 0)
  {
    return (1)
  }
}

gridx <- seq(0,5,0.2)
gridy <- seq(0,5,0.2)
gridz <- matrix(0, length(gridx), length(gridx))
amostra <- matrix(nrow = 1, ncol = 2)

for(i in c(1:length(gridx))){
  for(j in c(1:length(gridy))){
    
    amostra[1,1] <- gridx[i]
    amostra[1,2] <- gridy[j]
    
    aux <- KNN(amostra,dataTrain,K, DTrain)
    
    gridz[i,j] <- aux
  }
}

persp3d(gridx, gridy, gridz, alpha = 0.5, col = "lightblue", xlab="x", ylab="Y", zlab = "")
points3d(xc1Train[,1], xc1Train[,2], 0.3, col = "blue", size = 6)
points3d(xc2Train[,1], xc2Train[,2], 0.3, col = "red", size = 6)

