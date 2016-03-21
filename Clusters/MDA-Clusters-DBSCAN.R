'''
Minería de datos apicada
K Means
Juan Esteban Mejía Velásquez

'''
##

   
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/factoextra")


#

library(factocextra)

data("multishapes")
df <- multishapes[, 1:2]
plot(df)
set.seed(123)
km.res <- kmeans(df, 5, nstart = 25)
fviz_cluster(km.res, df, frame = FALSE, geom = "point")
plot(km.res)
#

install.packages("fpc")
install.packages("dbscan")
library(fpc)
library(dbscan)


#

db <- fpc::dbscan(df, eps = 0.15, MinPts = 5)
plot(db, df, main = "DBSCAN", frame = FALSE)
fviz_cluster(db, df, stand = FALSE, frame = FALSE, geom = "point")


#

dbscan::kNNdistplot(df, k =  5)
abline(h = 0.15, lty = 2)

##

Iris <- iris
Iris <- as.matrix(Iris[, 1:4])


#

dbscan::kNNdistplot(Iris, k =  4)
abline(h = 0.4, lty = 2)


#
set.seed(123)

# Paquete fpc 

res.fpc <- fpc::dbscan(Iris, eps = 0.4, MinPts = 4)

# Paquete dbscan 

res.db <- dbscan::dbscan(Iris, 0.4, 4)

#

all(res.fpc$cluster == res.db)

#

fviz_cluster(res.fpc, Iris, geom = "point")


