rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')
library('RSNNS')
library('mclust')

N <-200

xc1 <- matrix(c(rnorm(N/2)*0.5+4, rnorm(N/2)*0.5+4), N/2, 2)
xc2 <- matrix(c(rnorm(N/2)*0.5+2, rnorm(N/2)*0.5+2), N/2, 2)

plot(xc1[,1], xc1[,2], col='blue', xlim=c(0.5,5.5), ylim=c(0,6), xlab ='', ylab='')
par(new=T)
plot(xc2[,1], xc2[,2], col='red', xlim=c(0.5,5.5), ylim=c(0,6), xlab ='', ylab='')

X<-rbind(xc1,xc2)
xm<-colMeans(X)
X_xm<-X-matrix(xm,nrow = nrow(X),ncol = ncol(X),byrow = T)

S<-cov(X_xm)
eigS<-eigen(S)
u1<-eigS$vectors[,1]
u2<-eigS$vectors[,2]

u100<-xm+u1
u200<-xm+u2

nc <- N/2

par(col.lab='blue')
plot(X[(1:nc),1],X[(1:nc),2],type="p", col = 'red',xlim=c(min(X[,1]),max(X[,1])),ylim=c(min(X[,2]),max(X[,2])), xlab=expression(x[1]), ylab=expression(x[2]))
par(new=T)
plot(X[((nc+1):(nc+nc)),1],X[((nc+1):(nc+nc)),2],type="p", col = 'blue',xlim=c(min(X[,1]),max(X[,1])),ylim=c(min(X[,2]),max(X[,2])), xlab=' ', ylab=' ')
par(new=T)
plot(xm[1],xm[2],type="p", pch=15, col = 'blue',xlim=c(min(X[,1]),max(X[,1])),ylim=c(min(X[,2]),max(X[,2])), xlab=' ', ylab=' ')
arrows(x0 = xm[1],y0 = xm[2],x1 = u100[1],y1 = u100[2])
arrows(x0 = xm[1],y0 = xm[2],x1 = u200[1],y1 = u200[2])
