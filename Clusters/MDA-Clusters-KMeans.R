
'''
Minería de datos apicada
K Means
Juan Esteban Mejía Velásquez

'''
# Cargar el conjunto de datos de iris 

Iris <- iris

# Mostrar a estructura de conjunto de datos}

str(Iris)
attach(Iris) # SEparar as variabes de data.frame original

# Un poco de exploracion : 
  
 # Graficos de dispersión 

plot(Sepal.Width~Sepal.Length, col=Species)
plot(Sepal.Length, Sepal.Width,col=Species,pch=as.numeric(Species))

   # Le agregamso una leyenda

    legend("topright", levels(Species) , lty=1, col=1:3, bty="n", cex=.75)

   # Le agregamso una leyenda
    
    pairs(iris[1:4],pch=as.numeric(iris$Species),col=iris$Species)
    
   # Graficar coordenadas paralelar

    library(MASS)
    parcoord(Iris[1:4], col=iris$Species,var.label=T)


# Contruccon de un modeo estandar de k-meands
set.seed(123)
KM.Iris<-kmeans(Iris[1:4], 3,iter.max=1000,
                algorithm = c("Lloyd") )

# Obtencion de mas informacion acerca de modelo
# Tamaño de los custers

KM.Iris$size

# Centro de los cluster por variable

KM.Iris$centers

# Table tabla con el numero de record por especie

table(Iris$Species,KM.Iris$cluster)

# Traducir a dos dimensiones usando
# escaamiento mutidimensional

Iris.dist <- dist(Iris[1:4])
Iris.mds <- cmdscale(Iris.dist)

# Graficar los punto en el espacio de dos dimensiones
# Abrir array para la graficacion

par(mfrow = c(1, 2))

# Cargar e instar el paquete de  scatterplot3d

suppressWarnings(
  suppressMessages(if
                   (!require(scatterplot3d, quietly=TRUE))
    install.packages("scatterplot3d")))

library("scatterplot3d")

# Decarar los puntos como caracteres 1, 2, 3

chars <- c("1", "2", "3")[as.integer(iris$Species)]

# Graficar en 3d

g3d=scatterplot3d(Iris.mds,pch=chars)
g3d$points3d(Iris.mds,col=KM.Iris$cluster,pch=chars)

# Graficar en 2d

plot(Iris.mds, col = KM.Iris$cluster, pch = chars, xlab = "Indeice", ylab = "Y")

# Adiccionar la variabe "custer" a
# el conjunto de datos original

Iris.Cluster<-cbind(Iris,KM.Iris[1])

head(Iris.Cluster[3:6])


# Un  acercamiento a la definición
# de numero de custers

# Geraración de loop para asignar 
#  de 1 a 30 clusters

SSE = rep(0, 30)
for (k in 1:30) {
  set.seed(42)
  grupos = kmeans(Iris[1:4], k)
  SSE[k] = grupos$tot.withinss
}

#

plot(SSE, col = "red", type = "b")

# Mejorar grafica

abline(v = 3, col = "black", lty = 14)
text(4, 60, "3 Clusters", col = "black", adj = c(0, -0.1), cex = 0.7)


## Eección de algoritmo

# setiatr los vectores para almacenar os resutados

Lloyd <- 0
MacQueen <- 0
Hartigan <- 0
Forgy <- 0

set.seed(123)

for (i in 1:5000) {
  KM <- kmeans(Iris[1:4], 3, iter.max = 1000, algorithm = "Hartigan-Wong")
  Hartigan <- Hartigan + KM$betweenss
  KM <- kmeans(Iris[1:4], 3, iter.max = 1000, algorithm = "Lloyd")
  Lloyd <- Lloyd + KM$betweenss
  KM <- kmeans(Iris[1:4], 3, iter.max = 1000, algorithm = "Forgy")
  Forgy <- Forgy + KM$betweenss
  KM <- kmeans(Iris[1:4], 3, iter.max = 1000, algorithm = "MacQueen")
  MacQueen <- MacQueen + KM$betweenss
}


Metodo <- c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")
Resultados <- as.data.frame(round(c(Hartigan, Lloyd, Forgy, 
                                 MacQueen)/5000, 2))
Resultados <- cbind(Metodo, Resultados)
names(Results) <- c("Metodo", "Intermediación")

Resultados

##Otros graficos


suppressWarnings(
  suppressMessages(if
                   (!require(cluster, quietly=TRUE))
    install.packages("cluster")))


clusplot(Iris[1:4], KM$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=1,main='Anaisis de clúster para Iris')



#Coeficiente de silueta

suppressWarnings(suppressMessages(if (!require(HSAUR,
                                               quietly = TRUE)) install.packages("HSAUR")))
suppressWarnings(suppressMessages(if (!require(cluster,
                                               quietly = TRUE)) install.packages("cluster")))
library("HSAUR")
library("cluster")



KM <- kmeans(Iris[1:4], 3, iter.max = 1000, algorithm = "Hartigan-Wong")

# Calculo de la matriz de disimilariedad

diss <- daisy(Iris[1:4])
dE2 <- diss^2
heatmap(as.matrix(diss))

# Calculo de silueta

Sil <- silhouette(KM$cl, diss)
silh

# Grarfico

par(mfrow=c(1, 1))

plot(Sil, col = c("red", "green", "blue"))


library(NbClust)

nb <- NbClust(scale(Iris[, 1:4]), distance = "euclidean", min.nc = 2,
              max.nc = 10, method = "complete", index ="all")


table(names(nb$Best.nc[1,]), nb$Best.nc[1,])

barplot(table(nb$Best.nc[1,]), border="blue", 
        density=c(90, 70, 50, 40, 30, 20, 10))

