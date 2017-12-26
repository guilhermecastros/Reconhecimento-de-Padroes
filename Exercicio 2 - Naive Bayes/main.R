rm(list=ls())
library('bmp')
library('rgl')

seqi <- seq(1,5)
seqj <- seq(1,5)
seqindex <- seq(1,10,2)

class1 <- list()
class2 <- list()
class3 <- list()
class4 <- list()
class5 <- list()

allclass <- list(class1,class2,class3,class4,class5)
# c(matriz) -> transforma em vetor

for(i in seqi){
  for(j in seqj){
    class1 <- read.bmp(paste("f",j,"teste", i, ".bmp", sep=""))
    class2 <-read.bmp(paste("F",j, "TESTE", i, "R.bmp", sep=""))
    allclass[[i]][[seqindex[j]]] <- class1
    allclass[[i]][[seqindex[j] + 1]] <- class2
  }
}

average <- matrix(0,nrow = 5, ncol = 10)
sd <- matrix(0,nrow = 5, ncol = 10)

for(k in 1:5){
  for(l in 1:10){
    average[k,l] <- mean(c(allclass[[k]][[l]]))
    sd[k,l] <- sd(c(allclass[[k]][[l]]))
  }
}

plot(average[1,],sd[1,],col='red',xlim=c(70,160),ylim=c(0,100), ylab='', xlab='')
par(new=T)
plot(average[2,],sd[2,],col='blue',xlim=c(70,160),ylim=c(0,100), ylab='', xlab='')
par(new=T)
plot(average[3,],sd[3,],col='purple',xlim=c(70,160),ylim=c(0,100), ylab='', xlab='')
par(new=T)
plot(average[4,],sd[4,],col='green',xlim=c(70,160),ylim=c(0,100), ylab='', xlab='')
par(new=T)
plot(average[5,],sd[5,],col='black',xlim=c(70,160),ylim=c(0,100), ylab='Desvio Padrão', xlab='Média')

avg <- rep(0,1,5)
desvio <- rep(0,1,5)

feature <- matrix(0,nrow = 5, ncol = 2)

for(i in 1:5){
  avg[i] = mean(average[i,])
  desvio[i] = mean(sd[i,])
  feature[i,] <- c(mean(average[i,]),mean(sd[i,]))
}

K1 <- cov(cbind((average[1,]),sd[1,]))
K2 <- cov(cbind((average[2,]),sd[2,]))
K3 <- cov(cbind((average[3,]),sd[3,]))
K4 <- cov(cbind((average[4,]),sd[4,]))
K5 <- cov(cbind((average[5,]),sd[5,]))

m1 <- feature[1,]
m2 <- feature[2,]
m3 <- feature[3,]
m4 <- feature[4,]
m5 <- feature[5,]

pdfnvar <- function(x,m,K,n)((1/(sqrt((2*pi)^n*(det(K)))))*exp(-0.5*(t(x-m)%*%(solve(K))%*%(x-m))))

xrange<-seq(1,200,.1)
np <- length(seq(1,200,.1))
seqi <- seq(1,np)
seqj <- seq(1,np)

M1 <- matrix (0, np,np)
M2 <- matrix (0, np,np)
M3 <- matrix (0, np,np)
M4 <- matrix (0, np,np)
M5 <- matrix (0, np,np)
Mres <- matrix (0, np,np)

for(i in seqi){
  for(j in seqj){
    
    x <- c(xrange[i],xrange[j])
    
    M1[i,j] <- pdfnvar(x,m1,K1,2)
    M2[i,j] <- pdfnvar(x,m2,K2,2)
    M3[i,j] <- pdfnvar(x,m3,K3,2)
    M4[i,j] <- pdfnvar(x,m4,K4,2)
    M5[i,j] <- pdfnvar(x,m5,K5,2)
    
    max <- max(M1[i,j],M2[i,j],M3[i,j],M4[i,j],M5[i,j])
    
    if(max == M1[i,j])
    {
      Mres[i,j] <- 1
    }
    else if(max == M2[i,j])
    {
      Mres[i,j] <- 2
    }
    else if(max == M3[i,j])
    {
      Mres[i,j] <- 3
    }
    else if(max == M4[i,j])
    {
      Mres[i,j] <- 4
    }
    else if(max == M5[i,j])
    {
      Mres[i,j] <- 5
    }
  }
}

max <- max(max(M1),max(M2),max(M3),max(M4), max(M5))

persp3d(xrange,xrange,M1,xlim=c(70,160), ylim = c(20,80), zlim= c(0,max), col='red', xlab='', ylab='', lab='')
persp3d(xrange,xrange,M2,xlim=c(70,160), ylim = c(20,80), zlim= c(0,max), col='blue', add = T, xlab='', ylab='', lab='')
persp3d(xrange,xrange,M3,xlim=c(70,160), ylim = c(20,80), zlim= c(0,max), col='purple', add = T, xlab='', ylab='', lab='')
persp3d(xrange,xrange,M4,xlim=c(70,160), ylim = c(20,80), zlim= c(0,max), col='green', add = T, xlab='', ylab='', lab='')
persp3d(xrange,xrange,M5,xlim=c(70,160), ylim = c(20,80), zlim= c(0,max), col='black', add = T, xlab='', ylab='', lab='')

contour(xrange,xrange,M1,xlim=c(70,160), ylim = c(20,80), col='red', drawlabels = F, nlevels = 5)
contour(xrange,xrange,M2,xlim=c(70,160), ylim = c(20,80), col='blue', drawlabels = F, nlevels = 5, add = T)
contour(xrange,xrange,M3,xlim=c(70,160), ylim = c(20,80), col='purple', drawlabels = F, nlevels = 5, add = T)
contour(xrange,xrange,M4,xlim=c(70,160), ylim = c(20,80), col='green', drawlabels = F, nlevels = 5, add = T)
contour(xrange,xrange,M5,xlim=c(70,160), ylim = c(20,80), col='black', drawlabels = F, nlevels = 5, add = T)
contour(xrange,xrange,Mres,xlim=c(70,160), ylim = c(20,80), col='black', drawlabels = F, add = T)

contour(xrange,xrange,Mres,xlim=c(70,160), ylim = c(20,80), col='black', drawlabels = F)

persp3d(xrange,xrange,Mres,xlim=c(70,160), ylim = c(20,80), col='black', drawlabels = F)


