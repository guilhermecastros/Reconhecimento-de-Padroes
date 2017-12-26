rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')
source('Kmedias.r')
source('mixGaussiana.r')

#List of data classes
G <- list()

data <- mlbench.spirals(1000,3,0.9)
plot(data)

N <- 30

g1 <- matrix(rnorm(N), ncol=2)*0.8+ matrix(c(2,2),15,2, byrow=T)
g2 <- matrix(rnorm(N), ncol=2)*0.8 + matrix(c(4,4),15,2, byrow=T)
g3 <- matrix(rnorm(N), ncol=2)*0.8 + matrix(c(4,2),15,2, byrow=T)
g4 <- matrix(rnorm(N), ncol=2)*0.8 + matrix(c(2,4),15,2, byrow=T)

numberOfClasses <- 2

for(i in 1:numberOfClasses)
{
  iClass <- which(data$classes == i)
  G[[i]] <- data$x[iClass,] #rbind(g1,g2,g3,g4)
}

numberOfClusters <- 30

seqi <- seq(-2,2,0.1)
seqj <- seq(-2,2,0.1)

gridResult <- list()
for(i in 1:numberOfClasses)
{
  gridResult[[i]] <- mixGaussiana(G[[i]],numberOfClusters, seqi, seqj)
}


colors <- rainbow(3)
persp3d(seqi,seqj,gridResult[[1]], col=colors[1])
persp3d(seqi,seqj,gridResult[[2]], col=colors[2], add = TRUE)

contour(seqi,seqj,gridResult[[1]], col='black', drawlabels = F)
contour(seqi,seqj,gridResult[[2]], col='black', drawlabels = F)



