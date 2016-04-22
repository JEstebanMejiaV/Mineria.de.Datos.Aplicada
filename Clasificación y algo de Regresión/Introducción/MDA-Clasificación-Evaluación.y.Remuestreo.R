
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

## Particion básica

library (ISLR)

set.seed(123)

Train <-  sample (392 ,196)


lm.fit <- lm(mpg ~ horsepower ,data=Auto, subset = Train )

mean((Auto$mpg - predict(lm.fit ,Auto))[-Train]^2)

lm.fit2 <- lm(mpg ~ poly( horsepower ,2) ,data=Auto , subset =Train )
mean((Auto$mpg - predict(lm.fit2 ,Auto))[-Train]^2)

lm.fit3 <- lm(mpg ~ poly( horsepower ,3) ,data=Auto , subset =Train )
mean((Auto$mpg - predict(lm.fit3 ,Auto))[-Train]^2)

# Cambine la semilla inicial (set.seed()) y repita estos ajustes

## LOOCV

# Usaremos la funcion glm (de modelos lineales generalizados) para
# Ajustar el modelo de Regresión linesal. Con el objeto de usar al funcion
# cv.glm usada para la validación creuzada. 

glm.fit =glm (mpg ~ horsepower , data= Auto)


if(!require(boot, quietly=TRUE))install.packages("boot")

library(boot)

CV.Error <- cv.glm(Auto ,glm.fit )

CV.Error$delta

####

CV.Error <- c()

for (i in 1:5){
   glm.fit = glm (mpg~poly( horsepower ,i),data=Auto )
   CV.Error = append(cv.glm (Auto ,glm.fit )$delta[1], CV.Error) 
}


CV.Error


## k-Fold Cross-Validation

set.seed (12)

CV.Error.10= c()

for (i in 1:10) {
  glm.fit =glm (mpg~poly( horsepower ,i),data=Auto )
  CV.Error.10[ i]= append(cv.glm (Auto ,glm.fit ,K =10)$delta[1],CV.Error.10)
}


CV.Error.10


## k-Fold Cross-Validation para otros métodos de clasificación

if(!require(e1071, quietly=TRUE))install.packages("e1071")


library(e1071) # Para la función svm

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


### Bootstrap

Alpha <- function(data , index ){
   X= data$X [ index ]
   Y= data$Y [ index ]
   return (( var (Y)-cov (X,Y))/( var (X)+ var (Y) -2* cov (X,Y)))
   }

Alpha(Portfolio ,1:100)

set.seed (1)

Alpha (Portfolio ,sample (100 ,100 , replace =T))

boot(Portfolio , Alpha ,R =1000)

## Bootsrap en modelos de regresión


boot.fn= function (data ,index )
         return (coef(lm(mpg~horsepower , data=data , subset = index )))


boot.fn(Auto ,1:nrow(Auto))

boot.fn(Auto ,sample (nrow(Auto) ,nrow(Auto) , replace =T))

# Repitamos la funcion para mostrar la variación por otra muestra
# Con remplazo

boot.fn(Auto ,sample (nrow(Auto) ,nrow(Auto) , replace =T))

# Estimación de 1.000 Bootstraps

boot(Auto ,boot.fn ,1000)

# Comparación. Importancia del bootstrap

summary (lm(mpg~horsepower ,data =Auto))$coef

# Boostrap a la funcion polinomica

boot.Poly <- function (data ,index )
   coefficients(lm(mpg~horsepower +I(horsepower^2) ,data=data ,
                    subset = index ))

boot(Auto,boot.Poly ,1000)

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


### Matriz de Error

library(C50)

library(caret)


data(churn)

churnTrain = churnTrain[,! names(churnTrain) %in% c("state","area_code", "account_length") ]

set.seed(123)

ind = sample(2, nrow(churnTrain), replace = TRUE, prob=c(0.7,0.3))
trainset = churnTrain[ind == 1,]
testset = churnTrain[ind == 2,]

svm.Modelo = train(churn ~ ., data = trainset,method = "svmRadial")

svm.Pred = predict(svm.Modelo, testset[,! names(testset) %in%c("churn")])

table(testset[,c("churn")], svm.Pred)

confusionMatrix(svm.Pred, testset[,c("churn")])


### Ánalsiis ROC


install.packages("ROCR")


library(ROCR)

library(e1071)


svmfit=svm(churn~ ., data=trainset, prob=TRUE)


pred=predict(svmfit,testset[, !names(testset) %in% c("churn")],probability=TRUE)

pred.prob = attr(pred, "probabilities")

pred.ROC = pred.prob[, 2]

pred.rocr = prediction(pred.ROC, testset$churn)

perf.rocr = performance(pred.rocr, measure = "auc", x.measure ="cutoff")

perf.tpr.rocr = performance(pred.rocr, "tpr","fpr")

plot(perf.tpr.rocr, colorize=T,main=paste("AUC:",(perf.rocr@y.values)))

as.numeric(performance(pred.rocr, "auc")@y.values)

## Comparación de ROC´s

install.packages("pROC")

library("pROC")

control = trainControl(method = "repeatedcv",
                       number = 10,
                       repeats = 3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)

glm.model= train(churn ~ .,
                 data = trainset,
                 method = "glm",
                 metric = "ROC",
                 trControl = control)

svm.model= train(churn ~ .,
                 data = trainset,
                 method = "svmRadial",
                 metric = "ROC",
                 trControl = control)

rpart.model= train(churn ~ .,
                   data = trainset,
                   method = "rpart",
                   metric = "ROC",
                   trControl = control)

glm.probs = predict(glm.model, testset[,! names(testset) %in% c("churn")], type = "prob")
svm.probs = predict(svm.model, testset[,! names(testset) %in% c("churn")], type = "prob")
rpart.probs = predict(rpart.model, testset[,! names(testset)%in% c("churn")], type = "prob")


glm.ROC = roc(response = testset[,c("churn")],
              predictor =glm.probs$yes,
              levels = levels(testset[,c("churn")]))

plot(glm.ROC, type="S", col="red")


svm.ROC = roc(response = testset[,c("churn")],
              predictor =svm.probs$yes,
              levels = levels(testset[,c("churn")]))

plot(svm.ROC, add=TRUE, col="navy")

rpart.ROC = roc(response = testset[,c("churn")],
                predictor =rpart.probs$yes,
                levels = levels(testset[,c("churn")]))

plot(rpart.ROC, add=TRUE, col="orange")


CV = resamples(list(glm = glm.model, svm=svm.model, rpart
                           = rpart.model))

summary(CV)

dotplot(CV, metric = "ROC")

bwplot(CV, layout = c(3, 1))

#### Caso de estudio : Scoring Áleman







