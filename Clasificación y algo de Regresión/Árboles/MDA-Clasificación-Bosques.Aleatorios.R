# '''
# Minería de Datos Aplicada
# Universidad Nacional de Colombia
# 
# Ejemplos Bosques Aleatorios
# 
# Juan Esteban Mejía Velásquez
# 
# '''

url="http://freakonometrics.free.fr/german_credit.csv"

credit=read.csv(url, header = TRUE, sep = ",")

Calitativas=c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20)



#convertir a variables  cualitaticas a Factores

for(i in Calitativas) credit[,i]=as.factor(credit[,i]) 


# Particion de la base

library(caTools)

set.seed(2000)
spl = sample.split(credit$Creditability, SplitRatio = 0.7)
Entrenamiento = subset(credit, spl==TRUE)
Testeo = subset(credit, spl==FALSE)


library(randomForest)

BosquesA = randomForest(Creditability  ~ ., data = Entrenamiento, 
                        importance =T,# Evalución de la importancia de los predictores
                        ntree = 500 , 
                        proximity=TRUE # Número de arboles en el bosque
                        )


BosquesA # visualizar las caracteristicas de mi bosque. Se halla támbien, 
         # a matriz de confución in-sample

# Reglas del árbol
getTree(BosquesA, 1)

# Grafica la dependencia parcial de la variable Account.Balance 
# sobre una de las clases que se desea predecir

partialPlot(BosquesA, Entrenamiento, Account.Balance, "1")  



# Grafíca los PCA de la matriz de proximidad
MDSplot(BosquesA, Entrenamiento$Creditability  )
# Prediciones

BosquesA.Pred = predict(BosquesA, Testeo)

table(BosquesA.Pred, Testeo$Creditability)

plot(BosquesA) #Graficacion de Error para cada árbol 

importance(BosquesA) # Importancia de los predictores medante diferentes criterios
                     # Recuesde: que sucederia con la exactitud o el coeficionte de 
                     # GINI promedio al quirar dicho predictor

varImpPlot(BosquesA) #Graficación de los resultados anteriores


## Trate de ver el margen, positivo o negativo, si es positivo significa 
## clasificación correcta

plot(margin(BosquesA,Testeo$Creditability))


## Validación Cruzada

library(ipred)

set.seed(1234)

error.RF <- c()
 
for(i in 1:10) error.RF[i] <- errorest(Creditability ~ ., data = Entrenamiento,
                                        model = randomForest, mtry = 2)$error
 
summary(error.RF)


par(mfrow = c(2, 2))

par(mfrow = c(1, 1))

for (i in 1:4)
  plot(sort(BosquesA$importance[,i], dec = TRUE),
  type = "h", main = paste("Measure", i))





# Graficar LA dependencia parcial de cada predictor
par(mfrow = c(4, 5), mar = c(2, 2, 2, 2), pty = "s");
for (i in 1:(ncol(Entrenamiento) - 1))
{
  partialPlot(BosquesA, Entrenamiento, names(Entrenamiento)[i], xlab = names(Entrenamiento)[i],
              main = NULL);
}

## Matrix de Confución


library(e1071)
library(caret)

confusionMatrix(data=BosquesA.Pred,
                reference=Testeo$Creditability,
                positive='1')



### ROC

library(ROCR)

#Evaluate the performance of the random forest for classification.
pred2=predict(BosquesA,type = "prob")


perf = prediction(pred2[,2], Entrenamiento$Creditability)


auc = performance(perf, "auc")

pred3 = performance(perf, "tpr","fpr")


plot(pred3,main="ROC Curve for Random Forest",col=2,lwd=2)
abline(a=0,b=1,lwd=2,lty=2,col="gray")


library(party)

# Graficar un árbol de ejemplo

cforest(Creditability~., data=Entrenamiento, controls=cforest_control(mtry=4, mincriterion=0))

## Tunear


TuneRF <- tuneRF(Entrenamiento[,-1], Entrenamiento[,'Creditability'],
                 stepFactor=0.5, ntreeTry = 500)

Mejormtry  <- TuneRF[TuneRF[, 2] == min(TuneRF[, 2]), 1]

Mejormtry

print(TuneRF)



### No Superpervisado
library(MASS)

library(mva)
set.seed(131)
crabs.prox <- randomForest(dslcrabs, ntree = 1000, proximity = TRUE)$proximity

crabs.mds <- cmdscale(1 - crabs.prox)
plot(crabs.mds, col = c("blue", "orange")[codes(crabs$sp)],
     pch = c(1,16)[codes(crabs$sex)], xlab="", ylab="")

## Forma Alternativa de computar los Bosques Aleatorios 



## Procesamiento en paralelo de bosques aleatorios



l