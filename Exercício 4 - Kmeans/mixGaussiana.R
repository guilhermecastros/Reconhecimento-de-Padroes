rm(list=ls())
library('bmp')
library('rgl')
library('caret')
source('Kmedias.r')
source('pdfnvar.r')

mixGaussiana <- function(data,numberOfClusters,sequei, sequej){
  
  X <- data
  U <- Kmedias(X,numberOfClusters)
  
  K <- list()
  dataByCluster <- list()
  
  countCluster <- 0
  for(i in 1:numberOfClusters){
    iGroup <- which(U[,i] == 1)
    if(length(iGroup) > 0)
    {
      countCluster <- countCluster + 1
      dataByCluster[[countCluster]] <- X[iGroup,]
    }
  }
  
  for(i in 1:countCluster)
  {
    K[[i]] <- cov(dataByCluster[[i]])  
  }
  
  m <- list()
  
  for(i in 1:countCluster)
  {
    m[[i]] <- as.matrix(colMeans(dataByCluster[[i]]))  
  }
  
  seqi <- sequei
  seqj <- sequej
  
  M <- matrix(0, nrow = length(seqi), ncol = length(seqj))
  
  ci <- 0
  p <- 0
  for(i in seqi){
    ci <- ci + 1
    cj <- 0
    for(j in seqj){
      cj <- cj + 1
      x <- as.matrix(c(i,j))
      for(k in 1:countCluster){
        p <- dim(dataByCluster[[k]])[1]/dim(data)[1]
        M[ci,cj] <- M[ci,cj] + p*pdfnvar(x,m[[k]],K[[k]],2)
      }
    }
  }
  
  return (M[,])
}


