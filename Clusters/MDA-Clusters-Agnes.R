
'''
Minería de datos apicada
K Means
Juan Esteban Mejía Velásquez

'''



animals <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/zoo/zoo.data", 
             sep = ",", header = F, col.names = c("animal", 
            "hair", "feathers", "eggs", "milk", "airbone", 
            "aquatic", "predator", "toothed", "backbone", 
            "breathes", "venomous", "fins", "legs", "tail", 
            "domestic", "catsize", "type"), fill = FALSE, 
                      strip.white = T)

##

animals <- animals[, -18]
animals <- animals[-27, ]
animals <- animals[-29, ]
animal.names <- animals[, 1]
animals <- data.frame(row.names = animal.names, animals[2:17])



suppressWarnings(suppressMessages(if (!require(NbClust, quietly = TRUE)) install.packages("NbClust")))



ind <- c("kl", "ch", "hartigan", "cindex", "db", "silhouette", 
         "duda", "pseudot2", "ratkowsky", "ball", "ptbiserial", 
         "gap", "frey", "gamma", "gplus", "dunn", "sdindex", 
         "sdbw")
clusters <- 0


library(NbClust)

for (i in 1:length(ind)) {
  Best <- NbClust(data, diss = NULL, distance = "binary", 
                  min.nc = 2, max.nc = 5, method = "complete", 
                  index = ind[i])
  
  clusters[i] <- Best$Best.nc[1]
}


table(clusters)



suppressWarnings(suppressMessages(if (!require(amap, 
                   quietly = TRUE)) install.packages("amap")))

library(amap)

#HC

hclust <- hclusterpar(na.omit(dist(animals), method = "euclidean", link = "average", nbproc = 3))


plot(hclust)

suppressWarnings(suppressMessages(if (!require(cba)) install.packages("cba")))

library(cba)


plot(hclust, main = "", sub = "", xlab = "")
title(main = "Dendrograma de animals")

rect.hclust(hclust, k = 5, border="blue")	

#Cortar el dondograma en 5 clústers
group<-cutree(hclust, k = 5)


clusters<-(cbind(animals,group))


table(clusters[17])


suppressWarnings(
  suppressMessages(if
                   (!require(rattle, quietly=TRUE))
    install.packages("rattle")))

library(rattle)

suppressWarnings(
  suppressMessages(if
                   (!require(fpc, quietly=TRUE))
    install.packages("fpc")))
cluster.stats(dist(animals), cutree(hclust, 5))
library("fpc")


### Categoricas

suppressWarnings(
  suppressMessages(if
                   (!require(ElemStatLearn))
    install.packages("ElemStatLearn")))
library("ElemStatLearn")
data(SAheart)
str(SAheart)


suppressWarnings(
  suppressMessages(if
                   (!require(cluster))
    install.packages("cluster")))
library("cluster")
diss<-suppressWarnings(daisy(SAheart, metric = "gower"))


suppressWarnings(h.factor <- hclusterpar(na.omit(diss,method="complete",
                                                 nbproc=3)))
plot(h.factor, hang = -1,main="Cluster Dendrogram")
rect.hclust(h.factor, k=4,border="blue")


group<-cutree(h.factor, k = 4)
Cluster<-cbind(SAheart,group)
table(Cluster$group)



suppressWarnings(suppressMessages(if (!require(dendextend, quietly
                                               =TRUE))install.packages("dendextend")))
library("dendextend")

data(iris)
Iris <- iris


irisdendlist <- dendlist()


methods <- c(
  "ward.D", "single", "complete", "average",
  "mcquitty", "median","centroid","ward.D2")


for (i in seq_along(methods)) {
  hciris <- hclust(dist(Iris[1:4]), method = methods[i])
  irisdendlist <-
    dendlist(irisdendlist, as.dendrogram(hciris))
} 

names(irisdendlist) <- methods

irisdendlist


cor <- round(cor.dendlist(irisdendlist), 2)

suppressWarnings(suppressMessages(if (!require(corrplot,
                                              quietly = TRUE)) install.packages("corrplot")))
library(corrplot)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))
corrplot(cor, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, col = col(200), addCoef.col = "black",
         order = "AOE")

##

dismatrix <- dist(Iris[1:4])

library("fpc")

methods <- c("ward.D", "single", "complete", "average",
             "mcquitty", "median", "centroid", "ward.D2")

within.cluster <- 0
average.between <- 0
average.within <- 0

for (i in 1:length(methods)) {
  hciris <- hclust(dismatrix, method = methods[i])
  group <- cutree(hciris, k = 3)
  stats <- cluster.stats(dismatrix, group)
  within.cluster[i] <- stats$within.cluster.ss
  average.between[i] <- stats$average.between
  average.within[i] <- stats$average.within}

# Resultados

data.frame(methods, within.cluster, average.between,
           average.within)
