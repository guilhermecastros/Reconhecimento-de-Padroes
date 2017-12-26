
plotCluster <- function(X, U, K){
  #Ploting Clusters
  colors <- c("blue", "yellow", "green", "purple", "black")
  for(i in 1:K){
    iGroup <- which(U[,i] == 1)
    if(length(iGroup) > 0)
    {
      plot(X[iGroup,], xlim=c(min(X[,1]) - 0.25,max(X[,1]) + 0.25), ylim=c(min(X[,2]) - 0.25,max(X[,2]) + 0.25),xlab='',ylab='', col=colors[i])
      par(new=T) 
    }
  }
}