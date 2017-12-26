library('R.matlab')
source('plotCluster.r')
#------------------------------------------------------------------------
# Algoritmo K-Means

Kmedias <- function(data,numberOfClusters){
  # define o numero de grupos
  K <- numberOfClusters #4
  
  X <- data#readMat("SyntheticDataset.mat")$x
  
  #X <- matrix(0, ncol=2, nrow=dim(X)[1])
  
  # step 1: randomly assign a cluster to each one of the patterns
  n <- dim(X)[1]
  U <- matrix(0,n,K)    # partition matrix
  idx <- matrix(0,n,1)
  
  for (i in 1:n){
    rnd <- sample(1:K, 1)
    U[i,rnd] <- 1  
    idx[i] <- rnd
  }
  
  ##J[1] <- sum(W)
  changes <- TRUE;
  oldIdx <- idx;
  iter <- 1;
  
  while (changes){    # iterate until the cluster assignments stop changing
    
    # computing the initial centroids
    centroids <- matrix(0,K,2)
    
    for (j in 1:K){
      Gj <- U[,j]
      onesIndexes <- which(Gj == 1)
      if(length(onesIndexes) > 0)
      {
        Xj <- X[onesIndexes,]
        
        if(length(Xj) == 2){
          centroids[j,1] <- mean(Xj[1])
          centroids[j,2] <- mean(Xj[2])
        }
        else
        {
          centroids[j,1] <- mean(Xj[,1])
          centroids[j,2] <- mean(Xj[,2])
        }
      }
      else
      {
        centroids[j,] <- 9999999
      }
    }
    
    xdata <- centroids[,1]
    ydata <- centroids[,2]
    
    #plot(xdata, ydata)
    
    
    # assign each pattern to the cluster whose centroid is closest 
    U <- matrix(0,n,K)
    for (i in 1:n){
      pattern <- X[i,]
      smallDistance <- 9999999
      for (j in 1:K){
        gc <- centroids[j,]
        distance <- sum((pattern-gc)^2);  # squared Euclidian distance from pattern to each centroid  
        if (distance < smallDistance){
          smallDistance <- distance
          smallIndex <- j
        }
      }
      U[i,smallIndex] <- 1;
      idx[i] <- smallIndex;
    }
    
    iter <- iter + 1
    ##J[iter] <- sum(W)
    
    # verifying the stop criteria 
    if (length(which(idx[] == oldIdx[])) == length(oldIdx)){
      changes <- FALSE
    }
    else
    {
      oldIdx <- idx
    }
  }
  
  plotCluster(X,U,K)
  
  plot(centroids, xlim=c(min(X[,1]) - 0.25,max(X[,1]) + 0.25), ylim=c(min(X[,2]) - 0.25,max(X[,2]) + 0.25),pch=21,xlab='',ylab='', col="red", bg="red")

  return (U)
}