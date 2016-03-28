
'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ilustración del AGNES

Juan Esteban Mejía Velásquez

'''


## Leer los datos de un url. Del repositorio de la UCI
## En este caso es nesesario indicarle los nombres de 
## los campos

animals <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/zoo/zoo.data", 
             sep = ",", header = F, col.names = c("animal", 
            "hair", "feathers", "eggs", "milk", "airbone", 
            "aquatic", "predator", "toothed", "backbone", 
            "breathes", "venomous", "fins", "legs", "tail", 
            "domestic", "catsize", "type"), fill = FALSE, 
                      strip.white = T)

## Algo de preprocesameinto

## Los nombres de los animeles se asignan a nombres de final
## Elo con aras de que al realizae el dendograma en las raices 
## se exprese en nombre de estos

animals <- animals[, -18]
animals <- animals[-27, ]
animals <- animals[-29, ]
animal.names <- animals[, 1]
animals <- data.frame(row.names = animal.names, animals[2:17])



## Se quiere hallar el # de cluster optimo utilizando la ibreria
## NbClust la cual tiene incoposado 30 indices para ello

if (!require(NbClust, quietly = TRUE)) install.packages("NbClust")

## Definicion de indices

ind <- c("kl", "ch", "hartigan", "cindex", "db", "silhouette", 
         "duda", "pseudot2", "ratkowsky", "ball", "ptbiserial", 
         "gap", "frey", "gamma", "gplus", "dunn", "sdindex", 
         "sdbw")
clusters <- 0


library(NbClust)

for (i in 1:length(ind)) {
  Best <- NbClust(animals, diss = NULL, distance = "binary", 
                  min.nc = 2, max.nc = 5, method = "complete", 
                  index = ind[i])
  
  clusters[i] <- Best$Best.nc[1]
}


table(clusters)

## Observese que los resultados son muy sensibles a la eleccion del
## numero maximo de cluster a evaluar ¿ Por que razón?

## Produciremos el dendograma para ver si en realidad es adecuado usar 
## 5 Cluster

(if (!require(amap, quietly = TRUE)) install.packages("amap"))
                   

library(amap)

## Creación de cluster jeranquico

hclust <- hclusterpar(na.omit(dist(animals), method = "euclidean", link = "average", nbproc = 3))

## Creacion del dendrograma

plot(hclust)


## Creacion del dendrograma con la elección de 5 clusters

if (!require(cba)) install.packages("cba")

library(cba)


plot(hclust, main = "", sub = "", xlab = "")
title(main = "Dendrograma de animals")

rect.hclust(hclust, k = 5, border="blue")	

## Cortar el dondograma en 5 clÃºsters para su análisis mas detallado 

group<-cutree(hclust, k = 5)

## Crear un nuvo dataframe con la infomacion de los 5 cluster

clusters<-(cbind(animals,group))




if (!require(rattle, quietly=TRUE)) install.packages("rattle")
 
## Podemos invocar la ibreria de la GUI Rattle para usar a función
## centers.hclust que nos sugiere cuales deberian ser los centoides
## para cada grupo y variable

library(rattle)

centers<-as.data.frame(centers.hclust(animals, hclust, 5))
centers[1:5]

## Ya para sacar multiples estadisticas podemos usar la función cluster.stats
## que se aloja en la libreria fpc

if (!require(fpc, quietly=TRUE))install.packages("fpc")

library("fpc")

cluster.stats(dist(animals), cutree(hclust, 5))




### Clusters jerárquicos con variables Categoricas

if(!require(ElemStatLearn))install.packages("ElemStatLearn")

library("ElemStatLearn")

data(SAheart)
str(SAheart)


if (!require(cluster))install.packages("cluster")

library("cluster")

diss<-suppressWarnings(daisy(SAheart, metric = "gower"))


suppressWarnings(h.factor <- hclusterpar(na.omit(diss,method="complete",
                                                 nbproc=3)))
plot(h.factor, hang = -1,main="Cluster Dendrogram")
rect.hclust(h.factor, k=4,border="blue")


group<-cutree(h.factor, k = 4)
Cluster<-cbind(SAheart,group)
table(Cluster$group)



if (!require(dendextend, quietly=TRUE))install.packages("dendextend")
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

## Cálculo de la correlación cofenetica

cor <- round(cor.dendlist(irisdendlist), 2)

if (!require(corrplot,quietly = TRUE)) install.packages("corrplot")

## Graficacion de la correlacion cofenetico cambiando el metodo de 
## fusión de clusters

library(corrplot)

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF",
                          "#77AADD", "#4477AA"))
corrplot(cor, method = "shade", shade.col = NA, tl.col = "black",
         tl.srt = 45, col = col(200), addCoef.col = "black",
         order = "AOE")

## Estadisticas de clusters

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
