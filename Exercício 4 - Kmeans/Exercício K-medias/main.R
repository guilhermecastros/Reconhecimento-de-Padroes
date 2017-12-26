rm(list=ls())
library('bmp')
library('rgl')
library('caret')
source('Kmedias.r')

N <- 30

numberOfClusters <- 4
X <- readMat("SyntheticDataset.mat")$x

Kmedias(X,numberOfClusters)
