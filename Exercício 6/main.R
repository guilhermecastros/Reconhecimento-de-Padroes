rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')

KDE <- function(xi, x){
  # h é o raio
  # n é a dimensão
  # N é o número de amostras
  n <- dim(x)[2]
  N <- dim(x)[1]
  H <- 1.06*sd(x)*(N^(-1/5))
  p <- 0
  for(xj in x){
    p <- p + exp(-(xi-xj)^2/(2*(H)^2))
  }
  return (1/(N*(sqrt(2*pi)*H)^n) * p)
}


N <- 30

xc1 <- rnorm(N)*0.5+2 #rnorm <- Média 0 e Desvio padrão 1
xc2 <- rnorm(N)*0.5+4
vec0 <- matrix(0,N)

plot(xc1, vec0, col='red',xlim=c(0,6),ylim=c(0,1),xlab='x',ylab='')
par(new=T)
plot(xc2, vec0, col='blue',xlim=c(0,6),ylim=c(0,1),xlab='x',ylab='')


fnormal1var <- function(x,m,r) ((1/(sqrt(2*pi*r*r)))*exp(-0.5 * ((x-m)/(r))^2))

m1 <- mean(xc1)
m2 <- mean(xc2)

s1 <- sd(xc1)
s2 <- sd(xc2)

xrange <- seq(0,6,0.1)

f1 <- fnormal1var(xrange,m1,s1) #aplicando os pontos na função
f2 <- fnormal1var(xrange,m2,s2) #aplicando os pontos na função

yrange2 <- 1*(f2>f1) #resposta do classificador

par(new=T)

plot(xrange,f1,type = 'l', col ='red', xlim=c(0,6), ylim=c(0,1), xlab='x', ylab='')
par(new=T)
plot(xrange,f2,type = 'l', col ='blue', xlim=c(0,6), ylim=c(0,1), xlab='x', ylab='')
par(new=T)
plot(xrange,yrange2,type = 'l', col ='green', xlim=c(0,6), ylim=c(0,1), xlab='x', ylab='P(x|C1), P(x,C2), Classe (x)')


f3 <- rep(0,length(xrange))

for(i in 1: length(xrange)){
  f3[i] <- KDE(xrange[i],as.matrix(c(xc1,xc2)))
}

par(new=T)
plot(xrange,f3,type = 'l', col ='purple', xlim=c(0,6), ylim=c(0,1), xlab='x', ylab='')


