rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')
library('RSNNS')
library('mclust')
library('e1071')

################# Máquinas de Vetor de Suporte - Superficie #################

rm(list=ls())
N <-200
groups <- 4
dim <- 2

xc <- array(0,dim=c(N/2, dim,groups))
xc1 <- matrix(c(rnorm(N/2)*0.5+7, rnorm(N/2)*0.5+3), N/2, 2)
xc2 <- matrix(c(rnorm(N/2)*0.5+2, rnorm(N/2)*0.5+3), N/2, 2)

plot(xc1[,1], xc1[,2], col='blue', xlim=c(0,9), ylim=c(0,7), xlab ='', ylab='')
par(new=T)
plot(xc2[,1], xc2[,2], col='red', xlim=c(0,9), ylim=c(0,7), xlab ='', ylab='')

intervalo <- seq(from = 0, to = 10, by= 0.2)
grid <- meshgrid(intervalo,intervalo)
grid$z <- matrix(nrow = nrow(grid$x), ncol = ncol(grid$x))
amostra <- matrix(nrow = 1, ncol = 2)

D <- c(rep(-1,nrow(xc1)),rep(1,nrow(xc2)))

D <- as.factor(D)

svm.modelT <- svm(D ~ ., data = rbind(xc1,xc2), cost = 100, gamma = 0.01)

gridx <- seq(0,10,0.2)
gridy <- seq(0,10,0.2)
gridz <- matrix(0, length(gridx), length(gridx))

for(i in c(1:ncol(grid$x))){
  for(j in c(1:ncol(grid$y))){
    
    amostra[1,1] <- gridx[i]
    amostra[1,2] <- gridy[j]
    
    svm.predT <- predict(svm.modelT, amostra)
    aux <- as.numeric(svm.predT)
    
    if(aux == 2){
      aux <- -1
    }
    
    gridz[i,j] <- aux
  }
}


persp3d(intervalo, intervalo, gridz, alpha = 0.5, col = "lightblue", xlab="x", ylab="Y", zlab = "")
points3d(xc1[,1], xc1[,2], 0.3, col = "red", size = 6)
points3d(xc2[,1], xc2[,2], 0.3, col = "blue", size = 6)


################# Mistura de Gaussianas - Superficie #################

rm(list=ls())
N <-200
groups <- 4
dim <- 2

xc <- array(0,dim=c(N/2, dim,groups))
xc1 <- matrix(c(rnorm(N/2)*0.5+7, rnorm(N/2)*0.5+3), N/2, 2)
xc2 <- matrix(c(rnorm(N/2)*0.5+2, rnorm(N/2)*0.5+3), N/2, 2)

plot(xc1[,1], xc1[,2], col='blue', xlim=c(0,9), ylim=c(0,7), xlab ='', ylab='')
par(new=T)
plot(xc2[,1], xc2[,2], col='red', xlim=c(0,9), ylim=c(0,7), xlab ='', ylab='')

#Modelos
modelxc1<-densityMclust(xc1)
modelxc2<-densityMclust(xc2)

dataTest <- as.matrix(rbind(xc1,xc2))

prioriC1 <- dim(xc1)[1]/dim(dataTest)[1]
prioriC2 <- dim(xc2)[1]/dim(dataTest)[1]

amostra <- matrix(nrow = 1, ncol = 2)

gridx <- seq(0,10,0.2)
gridy <- seq(0,10,0.2)
gridz <- matrix(0, length(gridx), length(gridx))

for(i in c(1:(length(gridx)))){
  for(j in c(1:(length(gridx)))){
    
    amostra[1,1] <- gridx[i] #grid2$x[i,j]
    amostra[1,2] <- gridy[j] #grid2$y[i,j]
    
    PxC1 <- dens(modelName=modelxc1$modelName, data = amostra, parameters = modelxc1$parameters)
    PxC2 <- dens(modelName=modelxc2$modelName, data = amostra, parameters = modelxc2$parameters)
    
    if(PxC1 >= PxC2)
      aux <- 1
    else
      aux <- -1
    
    gridz[i,j] <- aux  
  }
}

persp3d(gridx, gridy, gridz, alpha = 0.5, col = "lightblue", xlab="x", ylab="Y", zlab = "")
points3d(xc1[,1], xc1[,2], 0.3, col = "red", size = 6)
points3d(xc2[,1], xc2[,2], 0.3, col = "blue", size = 6)



################# Clssificação - Breast Cancer #################

rm(list=ls())

data(BreastCancer)
summary(BreastCancer)
X <- data.matrix(BreastCancer[,2:10])
X[is.na(X)] <- 0
Y <- matrix(0, nrow = dim(X)[1], ncol = 2)
Y[which(BreastCancer$Class == 'benign'),1] <- 1
Y[which(BreastCancer$Class == 'malignant'),2] <- 1

N <- 10
MSE <- rep(0,N)
SD <- rep(0,N)
percentage <- rep(0,N)

xc1 <- X[which(BreastCancer$Class == 'benign'),]
xc2 <- X[which(BreastCancer$Class == 'malignant'),]

index <- sample(dim(xc1)[1])

numOfTrainXC1 <- round(dim(xc1)[1]*0.6)
numOfTestXC1 <- round(dim(xc1)[1]*0.2)
numOfValidationXC1 <- round(dim(xc1)[1]*0.2)

xc1Test <- xc1[index[1:numOfTestXC1],]
xc1Train <- xc1[index[(numOfTestXC1 + 1): (numOfTestXC1 + numOfTrainXC1)],]
xc1Validation <- xc1[index[((numOfTestXC1 + numOfTrainXC1) + 1): dim(xc1)[1]],]

index <- sample(dim(xc2)[1])

numOfTrainXC2 <- round(dim(xc2)[1]*0.6)
numOfTestXC2 <- round(dim(xc2)[1]*0.2)
numOfValidationXC2 <- round(dim(xc2)[1]*0.2)

xc2Test <- xc2[index[1:numOfTestXC2],]
xc2Train <- xc2[index[(numOfTestXC2 + 1): (numOfTestXC2 + numOfTrainXC2)],]
xc2Validation <- xc2[index[((numOfTestXC2 + numOfTrainXC2) + 1): dim(xc2)[1]],]

# formatando base de dados

DTrain <- c(rep(-1,nrow(xc1Train)),rep(1,nrow(xc2Train)))

DTrain <- as.factor(DTrain)
XTrain <- rbind(xc1Train,xc2Train)

# Validação

DValidation <- c(rep(-1,nrow(xc1Validation)),rep(1,nrow(xc2Validation)))

#DTest <- as.factor(DTest)
XValidation <- rbind(xc1Validation,xc2Validation)


#Modelos
#gamma = raio da gaussiana
#cost = parâmetro C

result <- matrix(0, 100,100)

cost <- seq(1,1000,10)
gamma <- seq(0.1, 10, 0.1)

for(i in 1:length(cost)){
  for(j in 1:length(gamma)){
    
    svm.model <- svm(DTrain ~ ., data = XTrain, cost = cost[i], gamma = gamma[j])
    svm.pred <- predict(svm.model, XValidation)
    
    # compute vsm confusion matrix
    confusion <- table(DValidation, pred = as.numeric(as.character(svm.pred)))
    
    Pxc <- as.numeric(as.character(svm.pred))
    
    result[i,j] <- sum(1*(DValidation == Pxc))/length(Pxc)
    
  }
}

plot(seq(1,10000,1),rbind(c(result)), xlab='', ylab='', main='Evolução percentual da classificação')

# best cost -> cost[1]
# best gamma -> gamma[1]

#########################################################################################3333
MSE <- rep(0,N)
SD <- rep(0,N)
percentage <- rep(0,N)
N <- 10
for(i in 1:N){
    
    cost <- seq(1,1000,10)
    gamma <- seq(0.1, 10, 0.1)
    data(BreastCancer)
    summary(BreastCancer)
    X <- data.matrix(BreastCancer[,2:10])
    X[is.na(X)] <- 0
    Y <- matrix(0, nrow = dim(X)[1], ncol = 2)
    Y[which(BreastCancer$Class == 'benign'),1] <- 1
    Y[which(BreastCancer$Class == 'malignant'),2] <- 1
    
    xc1 <- X[which(BreastCancer$Class == 'benign'),]
    xc2 <- X[which(BreastCancer$Class == 'malignant'),]
    
    index <- sample(dim(xc1)[1])
    
    numOfTrainXC1 <- round(dim(xc1)[1]*0.6)
    numOfTestXC1 <- round(dim(xc1)[1]*0.2)
    numOfValidationXC1 <- round(dim(xc1)[1]*0.2)
    
    xc1Test <- xc1[index[1:numOfTestXC1],]
    xc1Train <- xc1[index[(numOfTestXC1 + 1): (numOfTestXC1 + numOfTrainXC1)],]
    xc1Validation <- xc1[index[((numOfTestXC1 + numOfTrainXC1) + 1): dim(xc1)[1]],]
    
    index <- sample(dim(xc2)[1])
    
    numOfTrainXC2 <- round(dim(xc2)[1]*0.6)
    numOfTestXC2 <- round(dim(xc2)[1]*0.2)
    numOfValidationXC2 <- round(dim(xc2)[1]*0.2)
    
    xc2Test <- xc2[index[1:numOfTestXC2],]
    xc2Train <- xc2[index[(numOfTestXC2 + 1): (numOfTestXC2 + numOfTrainXC2)],]
    xc2Validation <- xc2[index[((numOfTestXC2 + numOfTrainXC2) + 1): dim(xc2)[1]],]
    
    # formatando base de dados
    
    DTrain <- c(rep(-1,nrow(xc1Train)),rep(1,nrow(xc2Train)))
    
    DTrain <- as.factor(DTrain)
    XTrain <- rbind(xc1Train,xc2Train)
    
    # Validação
    
    DValidation <- c(rep(-1,nrow(xc1Validation)),rep(1,nrow(xc2Validation)))
    
    #DTest <- as.factor(DTest)
    XValidation <- rbind(xc1Validation,xc2Validation)  
  
    svm.model <- svm(DTrain ~ ., data = XTrain, cost = cost[1], gamma = gamma[1])
    svm.pred <- predict(svm.model, XValidation)
    
    # compute vsm confusion matrix
    confusion <- table(DValidation, pred = as.numeric(as.character(svm.pred)))
    
    Pxc <- as.numeric(as.character(svm.pred))
    
    percentage[i] <- (sum(1*(DValidation == Pxc))/length(Pxc))
    MSE[i] <- mean((Pxc-DValidation)^2)
    SD[i] <- sd(Pxc-DValidation)
}



plot(MSE, xlab='', ylab='',main='Erro Quadrático Médio (MSE)')
plot(SD, xlab='', ylab='',main='Desvio Padrão (SD)')
plot(percentage, xlab='', ylab='',main='Acerto Percentual')
mean(percentage)