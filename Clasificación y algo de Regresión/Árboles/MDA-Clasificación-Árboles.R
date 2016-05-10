'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos Árboles de Clasificación

Juan Esteban Mejía Velásquez

'''

if(!require(RWeka, quietly=TRUE))install.packages("RWeka")
if(!require(C50, quietly=TRUE))install.packages("C50")
if(!require(rpart, quietly=TRUE))install.packages("rpart")
if(!require(rpart.plot, quietly=TRUE))install.packages("rpart.plot")
if(!require(randomForest, quietly=TRUE))install.packages("randomForest")
if(!require(c("Formula","partykit"), quietly=TRUE))install.packages(c("Formula","partykit"))
if(!require(arules, quietly=TRUE))install.packages("arules")

library("rJava")
library("RWeka")
library("C50")
library("rpart")
library("rpart.plot")
library("randomForest")
library("Formula")
library("partykit")
library("arules")


data(AdultUCI)

summary(AdultUCI)

Adultos = na.omit(AdultUCI)[,-c(3,5)]


if(!require(caTools, quietly=TRUE))install.packages("caTools")

library(caTools)

set.seed(1234)

spl = sample.split(Adultos$income, SplitRatio=0.7)

EntrenamientoTemp = subset(Adultos, spl==TRUE)
Test = subset(Adultos, spl==FALSE)


AdultosTrainIngresoAlto = EntrenamientoTemp[EntrenamientoTemp$income == "small",]
AdultosTrainIngresoBajo = EntrenamientoTemp[EntrenamientoTemp$income == "large",]

Sobremuestreo = sample(nrow(AdultosTrainIngresoAlto),
                       nrow(AdultosTrainIngresoBajo), replace = TRUE)

Entrenamiento = rbind(AdultosTrainIngresoBajo,AdultosTrainIngresoAlto[Sobremuestreo,])


### Clasificación con C 4.5

C45tree = J48(income ~ . , data= Entrenamiento, control= Weka_control(U=TRUE))

summary(C45tree)

## Predicciones

Predicciones = data.frame(matrix(nrow = nrow(Test), ncol=0))
Predicciones$C45 = predict(C45tree, Test)

## Podar árbol

C45treePodado = J48(income ~ . , data= Entrenamiento, control= Weka_control(U=FALSE))
C45treePodado
summary(C45treePodado)

Predicciones$C45Podado = predict(C45treePodado, Test)

summary(Predicciones)

### Clasificación con  C 50

C50tree = C5.0(y = Entrenamiento$income, x =Entrenamiento[,-13], Trials = 10)
  
summary(C50tree)


ContTC5.0 <-  as.table(matrix(c(4434,822,532,4724),byrow = T, nrow = 2))

ContTC5.0

if(!require(psych, quietly=TRUE))install.packages("psych")

library(psych)

cohen.kappa(ContTC5.0)

Predicciones$C5.0 = predict(C50tree, Test)

summary(Predicciones)

### Clasificación con  C 50

CARTtree = rpart(income ~. , data= Entrenamiento)

rpart.plot(CARTtree, extra = 1)

ProbsCART = predict(CARTtree, Entrenamiento)

PredictCART = rep(0, nrow(ProbsCART))

PredictCART[ProbsCART[,1] <=.5] = "small"
PredictCART[ProbsCART[,1] >.5] = "large"

ConfCART = table(Entrenamiento$income, PredictCART)

ConfCART

cohen.kappa(ConfCART)

CARTtreePodado = prune(CARTtree, cp=0.03)

rpart.plot(CARTtreePodado, extra = 1)

levels(Entrenamiento$relationship)

ProbsCARTtest = predict(CARTtreePodado, Test)
Predicciones$CART[ProbsCARTtest[,1] <=.5] = "small"
Predicciones$CART[ProbsCARTtest[,1] >.5] = "large"

summary(Predicciones)

### Bosques Aleatorios

library(randomForest)

BosquesA = randomForest(income  ~ ., data = Entrenamiento, importance =T)

BosquesA

BosquesA.Pred = predict(BosquesA, Test)



