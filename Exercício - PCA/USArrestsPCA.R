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
data(USArrests)
summary(USArrests)
X <- data.matrix(USArrests)
X[is.na(X)] <- 0

meanx <- colMeans(X)
Xs<- X - t(replicate(dim(X)[1],meanx))
S<-cov(Xs)
eigS<-eigen(S)
plot(c(1 : ncol(X)),eigS$values,type='b',xlab='Eixo PCA',ylab='Autovalor')

projX<-Xs %*% eigS$vectors

maxX <- max(max(projX[,1]),max(projX[,1]))
minX <- min(min(projX[,1]),min(projX[,1]))
maxY <- max(max(projX[,2]),max(projX[,2]))
minY <- min(min(projX[,2]),min(projX[,2]))

plot(projX[,1],projX[,2], col='blue', xlim=c(-1 + minX,maxX), ylim=c(-1 + minY,maxY), xlab ='', ylab='')

points3d(projX[,1], projX[,2], projX[,3], col = "blue", size = 6, xlab="x", ylab="Y", zlab = "", add=T)
axes3d()

calcTotalVariance <- function(X) {
  sum(apply(projX, 2, function(x) x^2)) / nrow(X)
}

calcCumulativeProportionVarianceExplained <- function(X, nComponents) {
  cumVarExp <- 0
  for (component in 1:nComponents) {
    cumVarExp <- cumVarExp + calcVarianceExplained(X, component)
  }
  cumVarExp / calcTotalVariance(X)
}
calcVarianceExplained <- function(X, component) {
  sum((projX[, component]) ^ 2) / nrow(X)
}

explainedVariances <- c()
for (nComponents in 1:ncol(X)) {
  expVar <- calcCumulativeProportionVarianceExplained(X, nComponents)
  explainedVariances <- c(explainedVariances, expVar)
}
xPos <- barplot(height = explainedVariances, ylim = c(0, 1.1),
                xlab = 'Componentes', ylab = 'Explained Variance')
text(x = xPos, y = explainedVariances, label = explainedVariances, pos = 3, cex =
       0.8, col = "blue")

