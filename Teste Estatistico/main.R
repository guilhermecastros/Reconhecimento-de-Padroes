rm(list=ls())
library('bmp')
library('rgl')
library('caret')
library('mlbench')
library('RSNNS')
library('mclust')
library('plot3D')
library('e1071')
library('class')
library('readr')

dados <- c(0.763, 0.768, 0.771, 0.798,
0.599, 0.591, 0.590, 0.569,
0.954, 0.971, 0.968, 0.967,
0.628, 0.661, 0.654, 0.657,
0.882, 0.888, 0.886, 0.898,
0.936, 0.931, 0.916, 0.931,
0.661, 0.668, 0.609, 0.685,
0.583, 0.583, 0.563, 0.625,
0.775, 0.838, 0.866, 0.875,
1.000, 1.000, 1.000, 1.000,
0.940, 0.962, 0.965, 0.962,
0.619, 0.666, 0.614, 0.669,
0.972, 0.981, 0.975, 0.975,
0.957, 0.978, 0.946, 0.970)

dados <- matrix(dados, nrow=14, ncol=4, byrow=TRUE)

dados2 <- dados

friedman.test(dados)
for(i in c(1:14))
{
  dados2[i,] <- rank(dados[i,],ties.method = "first")
}








