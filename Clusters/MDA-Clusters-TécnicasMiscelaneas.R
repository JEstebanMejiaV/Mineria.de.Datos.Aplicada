'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Técnicas miscelaneas de clusterización

Juan Esteban Mejía Velásquez

'''

## Fuzzy C-Means

if(!require(e1071, quietly=TRUE))install.packages("e1071")
if(!require(scatterplot3d, quietly=TRUE))install.packages("scatterplot3d")

library(e1071)
library(scatterplot3d)


CFC <- cmeans(iris[,-5], 3, 100, m=2, method="cmeans")

plot(iris[,1], iris[,2], col=CFC$cluster)

points(CFC$centers[,c(1,2)], col=1:3, pch=8, cex=2)

CFC$membership[1:3,]

table(iris$Species, result$cluster)

s3d <- scatterplot3d(CFC$membership, color=result$cluster, type="h", 
                     angle=240, scale.y=0.7, pch=16)

plot(iris, col=CFC$cluster)



## Multi-guasiana con EM (expectation-Maxiization)

if(!require(psych, quietly=TRUE))install.packages("psych")

library(mclust)

MGC <- Mclust(iris[,1:4], 3)

summary(MGC)

plot(MGC, what=c("classification"), dimens=c(1,2))

plot(MGC, what=c("classification"), dimens=c(3,4))


## ROCK (RObust Clustering using linKs)

if(!require(cba, quietly=TRUE))install.packages("cba")

library("cba")

data(Votes)
shw
x <- as.dummy(Votes[-17])
RC<- rockCluster(x, n=2, theta=0.73, debug=TRUE)
print(RC)
RCF <- fitted(RC)
table(Votes$Class, RCF$cl)

## Mapas auto-organizados (Sefl-Organization Maps)

if(!require(SOMbrero, quietly=TRUE))install.packages("SOMbrero")

library(SOMbrero)

iris.som <- trainSOM(x.data = iris[, 1:4], verbose = TRUE, nb.save = 5)

iris.som

plot(iris.som, what = "energy")

iris.som$clustering

plot(iris.som, what = "obs", type = "hitmap")

summary(iris.som)

par(mfrow = c(2, 2))

plot(iris.som, what = "prototypes", var = "Sepal.Length", main = "Sepal length")
plot(iris.som, what = "prototypes", var = "Sepal.Width", main = "Sepal width")
plot(iris.som, what = "prototypes", var = "Petal.Length", main = "Petal length")
plot(iris.som, what = "prototypes", var = "Petal.Width", main = "Petal width")
