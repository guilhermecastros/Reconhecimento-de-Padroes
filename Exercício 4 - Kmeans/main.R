rm(list=ls())
library('bmp')
library('rgl')
library('caret')

pdfnvar <- function(x,m,K,n)((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))

N <- 30

g1 <- matrix(rnorm(N), ncol=2)*0.8+ matrix(c(2,2),15,2, byrow=T)
g2 <- matrix(rnorm(N), ncol=2)*0.8 + matrix(c(4,4),15,2, byrow=T)
g3 <- matrix(rnorm(N), ncol=2)*0.8 + matrix(c(4,2),15,2, byrow=T)
g4 <- matrix(rnorm(N), ncol=2)*0.8 + matrix(c(2,4),15,2, byrow=T)

plot(g1[,1],g1[,2], col='red', xlim=c(0,6), ylim=c(0,6))
par(new=T)

plot(g2[,1],g2[,2], col='blue', xlim=c(0,6), ylim=c(0,6))
par(new=T)

plot(g3[,1],g3[,2], col='black', xlim=c(0,6), ylim=c(0,6))
par(new=T)

plot(g4[,1],g4[,2], col='green', xlim=c(0,6), ylim=c(0,6))


K1 <- cov(g1)
K2 <- cov(g2)
K3 <- cov(g3)
K4 <- cov(g4)

p1 <- 0.25
p2 <- 0.25
p3 <- 0.25
p4 <- 0.25

m1 <- as.matrix(colMeans(g1))
m2 <- as.matrix(colMeans(g2))
m3 <- as.matrix(colMeans(g3))
m4 <- as.matrix(colMeans(g4))

seqi <- seq(0,6, 0.1)

seqj <- seqi

M <- matrix(nrow = length(seqi), ncol = length(seqj))

ci<- 0
for(i in seqi){
  ci <- ci + 1
  cj <- 0
  for(j in seqj){
    cj <- cj + 1
    x <- as.matrix(c(i,j))
    M[ci,cj] <- p1*pdfnvar(x,m1,K1,2) + p2*pdfnvar(x,m2,K2,2) + p3*pdfnvar(x,m3,K3,2) + p4*pdfnvar(x,m4,K4,2)
  }
}

persp3d(seqi,seqj,M, col = 'red')
contour(seqi,seqj,M, col = 'black',add = T)



