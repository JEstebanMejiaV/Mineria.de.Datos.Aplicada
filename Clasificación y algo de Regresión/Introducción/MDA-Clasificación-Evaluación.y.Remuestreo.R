
'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Remuestreo y Evaluación

Juan Esteban Mejía Velásquez

'''

library(caTools)

set.seed(123)

split <-  sample.split(iris$Species, SplitRatio = 0.7)
Trainset <-  subset(iris, split == TRUE)
Testset <-  subset(iris, split == FALSE)

##

set.seed(123)

ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.7,0.3))
Trainset <-  iris[ind == 1,]
Testset <-  iris[ind == 2,]

##

library(e1071)

ind <-  cut(1:nrow(iris), breaks=10, labels=F)

Exactitudes = c()

for (i in 1:10) {
  fit <-  svm(Species ~., iris[ind != i,])
  Predicciones <-  predict(fit, iris[ind == i, -5])
  Exactitudesk <-  sum(Predicciones == iris[ind == i,c("Species")])
  Exactitudes <-  append(Exactitudesk  / nrow(iris[ind == i,]), Exactitudes)
}

Exactitudes

mean(Exactitudes)


### Validuación Cruzada con el paquete e1071

library(e1071)

Ajuste <-  tune.svm(Species~., data = iris, gamma = 10^-2, cost =
                   10^2, tunecontrol=tune.control(cross=10))

summary(Ajuste)

Ajuste$performances

Ajuste$best.model

svmAjuste <-  Ajuste$best.model

table(iris[,c("Species")], predict(svmAjuste))


##### Ranquiar por importancia

if(!require(rminer, quietly=TRUE))install.packages("rminer")

library(rminer)

Modelo <- fit(Species~.,iris,model="svm")

Importancia <- Importance(Modelo,iris,method="sensv")

L <- list(runs=1,sen=t(Importancia$imp),sresponses=Importancia$sresponses)

mgraph(L,graph="IMP",leg=names(iris),col="gray",Grid=10)

?rminer::fit

#####



library(caret)

Trainrows <- createDataPartition(iris, p = .80, list= FALSE)

trainPredictors <- iris[Trainrows, -5]
trainClasses <- iris[Trainrows,5]




#### Caso de estudio : Scoring Áleman







