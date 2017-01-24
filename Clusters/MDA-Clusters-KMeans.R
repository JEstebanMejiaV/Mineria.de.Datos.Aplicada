
####

# Miner{ia de Datos Aplicada
# Universidad Nacional de Colombia
# 
# Ilustración K-means
# 
# Juan Esteban Mejía Velásquez

####



## Mostrar a estructura de conjunto de datos}

str(iris)
attach(iris) # Separar as variabes de data.frame original 
             # (poner iris en el search path)

## Un poco de exploracion : 
  
 # Graficos de dispersión 

plot(Sepal.Width ~ Sepal.Length, col = Species)

plot(Sepal.Length, Sepal.Width,col = Species, pch = as.numeric(Species))

   # Le agregamso una leyenda

    legend("topright", levels(Species) , lty = 1, col = 1:3, bty = "n", cex = .75)

   # Le agregamso una leyenda
    
    pairs(iris[1:4], pch = as.numeric(iris$Species),col = iris$Species)
    
   # Graficar coordenadas paralelas

    library(MASS)
    parcoord(iris[1:4], col = iris$Species,var.label = T)


## Contruccon de un modeo estandar de k-means
    
set.seed(123) # la semilla inicial es para que nos de los mismo resultados
              # Recurde que los clÃºsters son sensibles a los valores iniciales

KM.Iris <- kmeans(iris[1:4], 3,iter.max = 1000, algorithm = c("Lloyd") )

## Obtencion de mas informacion acerca de modelo
 # Tamaño de los custers

KM.Iris$size

 # Centro de los cluster por variable

KM.Iris$centers

#  tabla con el numero de record por especie

table(iris$Species,KM.Iris$cluster)

# Ahora resumiremos cada grupo con los valores promedio de las variables 
# númericas, Ello medinate la función 'aggregate'

aggregate(x = iris[,-5], by = list(KM.Iris$cluster), FUN = mean)

# Para efectos de facilidad crearemso un funcion mas sensilla para aplicar 
# lo qeu hicimos anteriormente con 'aggregate'

seg.summ <- function(data, groups) {
  aggregate(data, list(groups), function(x) mean(as.numeric(x)))
}

seg.summ(iris, KM.Iris$cluster)

#### --------------- Escalamiento Multidimencional -----------------


## Traducir a dos dimensiones usando escalamiento mutidimensional

Iris.dist <- dist(iris[1:4])

Iris.mds <- cmdscale(Iris.dist)

# Graficar los punto en el espacio de dos dimensiones

par(mfrow = c(1, 2))


# Cargar e instalar el paquete de  scatterplot3d para graficos 3D

if(!require(scatterplot3d, quietly = TRUE))install.packages("scatterplot3d")

library("scatterplot3d")

# Decarar los puntos como caracteres 1, 2, 3

chars <- c("1", "2", "3")[as.integer(iris$Species)]

# Graficar en 3d

g3d <- scatterplot3d(Iris.mds,pch = chars)

g3d$points3d(Iris.mds,col = KM.Iris$cluster,pch = chars)

# Graficar en 2d

plot(Iris.mds, col = KM.Iris$cluster, pch = chars, xlab = "indice", ylab = "Y")

# Adiccionar la variabe "cluster" a el conjunto de datos original

Iris.Cluster <- cbind(iris,KM.Iris[1])

head(Iris.Cluster[3:6])



### -------------- Determinacion de # de Clusters ---------------


# Un  acercamiento a la definición
# de numero de custers

# Geraración de loop para asignar 



wssplot <- function(data, nc=15, seed=1234){
            SSE = c()
            for (k in 1:nc) {
              set.seed(seed)
              grupos = kmeans(data, k)
              SSE[k] = grupos$tot.withinss
            }
plot(SSE, col = "navy", type = "b",xlab = '# de Clusters', 
     ylab = 'Suma cuadratica dentro de los grupos')
}

par(mfrow = c(1, 1))

wssplot(iris[1:4], nc = 15) 


# Mejorar grafica

abline(v = 3, col = "black", lty = 14)
text(4, 60, "3 Clusters", col = "black", adj = c(0, -0.1), cex = 0.7)


#### Calculo de coeficiente de silueta


# Calculo de la matriz e disimilariedad

diss <- daisy( iris[1:4] )
diss2 <- diss^2


# Calculo de silueta



Sil <- silhouette(KM.Iris$cl, diss2)

Sil

# Grarfico

par(mfrow = c(1, 1))

plot(Sil, col = c("red", "green", "blue"))



### Grafico de nemero de clusters según silueta

asw <- numeric(nrow(iris[,-5]))
for (k in 1:(nrow(iris[,-5]) - 1)) {
  sil <- silhouette(KM.Iris$cl, diss2)
  asw[k] <- summary(sil)$avg.width
}

k.best <- which.max(asw)

plot(1:nrow(iris[,-5]), asw, type = "h", 
     main = "Número de Clústers basados en silueta", 
     xlab = "k (Numero de Grupos)", ylab = "Amplitud de la silueta promedio")
axis(1, k.best, paste("optimum",k.best,sep = "\n"), col = "red", font = 2,
     col.axis = "red")
points(k.best, max(asw), pch = 16, col = "red", cex = 1.5)


cat("", "Número de Clústers basados en silueta k =", k.best, "\n", 
    "Con una amplitud de silueta promedio de ", max(asw), "\n")





#### Multiples Criterios para bondad de Clusters


## Usas la libreria NbClust para determinar e # de cluster mediante 30
## indices

if (!require(NbClust, quietly = TRUE)) install.packages("NbClust")


library(NbClust)

nb <- NbClust(scale(iris[, 1:4]), distance = "euclidean", min.nc = 2,
               max.nc = 10, method = "complete", index = "all")


table(names(nb$Best.nc[1,]), nb$Best.nc[1,])

par(mfrow = c(1, 1))

barplot(table(nb$Best.nc[1,]), border = "blue", 
        density = c(90, 70, 50, 40, 30, 20, 10))



#### ------ Elección de algoritmo ----------------

# setiar los vectores para almacenar os resutados

Lloyd <- 0
MacQueen <- 0
Hartigan <- 0
Forgy <- 0

set.seed(123)

for (i in 1:500) {
  KM <- kmeans(iris[1:4], 3, iter.max = 1000, algorithm = "Hartigan-Wong")
  Hartigan <- Hartigan + KM$betweenss
  KM <- kmeans(iris[1:4], 3, iter.max = 1000, algorithm = "Lloyd")
  Lloyd <- Lloyd + KM$betweenss
  KM <- kmeans(iris[1:4], 3, iter.max = 1000, algorithm = "Forgy")
  Forgy <- Forgy + KM$betweenss
  KM <- kmeans(iris[1:4], 3, iter.max = 1000, algorithm = "MacQueen")
  MacQueen <- MacQueen + KM$betweenss
}


Metodo <- c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
Resultados <- as.data.frame(round(c(Hartigan, Lloyd, Forgy, 
                                 MacQueen)/5000, 2))
Resultados <- cbind(Metodo, Resultados)
names(Resultados) <- c("Metodo", "Intermediación")

Resultados

### Función


KmAlgo <- function(data, k=3, nc=500 ) {
  
  Lloyd <- 0
  MacQueen <- 0
  Hartigan <- 0
  Forgy <- 0
  
  set.seed(123)
  
  for (i in 1:nc) {
    KM <- kmeans(data, k, iter.max = 1000, algorithm = "Hartigan-Wong")
    Hartigan <- Hartigan + KM$betweenss
    KM <- kmeans(data, k, iter.max = 1000, algorithm = "Lloyd")
    Lloyd <- Lloyd + KM$betweenss
    KM <- kmeans(data, k, iter.max = 1000, algorithm = "Forgy")
    Forgy <- Forgy + KM$betweenss
    KM <- kmeans(data, k, iter.max = 1000, algorithm = "MacQueen")
    MacQueen <- MacQueen + KM$betweenss
  }
  
  
  Metodo <- c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
  Resultados <- as.data.frame(round(c(Hartigan, Lloyd, Forgy, 
                                      MacQueen)/nc, 2))
  Resultados <- cbind(Metodo, Resultados)
  names(Resultados) <- c("Metodo", "Intermediación")
  
  Resultados
  
}

KmAlgo(iris[1:4])

#### ---- Otros graficos para la validación del clúster --------

## La tÃ©cnica PCA (anÃ¡lisis de componentes principales) lo que 
## hace es resumir la informaciÃ³n de todas las variables en unas 
## pocas nuevas llamadas componentes. Cada componente explica cierto 
## porcentaje de la variabilidad total. La funciÃ³n cusplot es una
## manera de identificar la efectividad de los cluster. ClÃºsteres 
## bien formados estÃ¡n separados en el plano principal formado por 
## los componentes 1 y 2. Por el contrario si se ven solapamientos en 
## el plano principal es porque los clÃºsteres no estÃ¡n bien formados.

if (!require(cluster, quietly = TRUE)) install.packages("cluster")


clusplot(iris[1:4], KM$cluster, color = TRUE, shade = TRUE,
         labels = 2, lines = 1,main = 'Análisis de clúster para Iris')



## Coeficiente de silueta

if (!require(HSAUR,quietly = TRUE)) install.packages("HSAUR")
if (!require(cluster,quietly = TRUE)) install.packages("cluster")

library("HSAUR")
library("cluster")



KM <- kmeans(iris[1:4], 3, iter.max = 1000, algorithm = "Hartigan-Wong")

# Calculo de la matriz de disimilariedad

diss <- daisy(iris[1:4])
dE2 <- diss^2

# Creacion de mapa de calor para intuir validez

if (!require(d3heatmap, quietly = TRUE)) install.packages("d3heatmap")


library(d3heatmap) # Libreria para heat map interactivos

heatmap(as.matrix(diss))


## Grafico interactivo basado en javascrip

d3heatmap(diss,  dendrogram = "none")



