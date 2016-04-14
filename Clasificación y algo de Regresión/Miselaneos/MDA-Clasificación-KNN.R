'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos KNN

Juan Esteban Mejía Velásquez

'''
### implementación Manual de KNN

if(!require(class, quietly=TRUE))install.packages("class")
library(class)


## Caundo se nesesite Normalizar

Normalizar <- function(x) {
  y <- (x - min(x))/(max(x) - min(x))
  y
}

Iris.N <- as.data.frame(lapply(iris[, -5], Normalizar))

summary(Iris.N)

Iris.N$Species <- iris[, 5]

ind <- sample(2, nrow(Iris.N), replace = TRUE, prob=c(0.7, 0.3))

IrisE = Iris.N[ind == 1,]
IrisT = Iris.N[ind == 2,]

Iris <- iris

Class.KNN = knn(IrisE[, -5],IrisT[, -5], IrisE[, 5], 5)

summary(Class.KNN)

table(IrisT[, 5], Class.KNN)

## Como elejir el # de K

Exactitud = rep(0,10)

 for (i in 1:10) {
   Class.KNN = knn(IrisE[, -5],IrisT[, -5], IrisE[, 5], i)
   Exactitud[i] = sum(Class.KNN == IrisT[, 5])/nrow(IrisT)
   }

which.max(Exactitud)

## Ahora,  ¿ podemos sacar una marco común para la evaluación de los
## los clasificadores?

### Otro ejemplo

if(!require(C50, quietly=TRUE))install.packages("C50")


library(C50)


data(churn)

churnTrain = churnTrain[,! names(churnTrain)
                        %in% c("state","area_code", "account_length") ]

ind <- sample(2, nrow(churnTrain), replace = TRUE, prob=c(0.7, 0.3))




Trainset = churnTrain[ind == 1,]
Testset = churnTrain[ind == 2,]

levels(Trainset$international_plan) = list("0"="no", "1"="yes")
levels(Trainset$voice_mail_plan) = list("0"="no", "1"="yes")
levels(Testset$international_plan) = list("0"="no", "1"="yes")
levels(Testset$voice_mail_plan) = list("0"="no", "1"="yes")

Churn.KNN  = knn(Trainset[,! names(Trainset) %in% c("churn")],
                 Testset[,! names(Testset) %in% c("churn")], Trainset$churn, k=3)


summary(Churn.KNN)

table(testset$churn, Churn.KNN )
confusionMatrix(table(testset$churn, Churn.KNN ))
