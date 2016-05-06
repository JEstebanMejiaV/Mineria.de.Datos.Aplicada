'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos Bosques Aleatorios

Juan Esteban Mejía Velásquez

'''

url="http://freakonometrics.free.fr/german_credit.csv"

credit=read.csv(url, header = TRUE, sep = ",")

Calitativas=c(1,2,4,5,7,8,9,10,11,12,13,15,16,17,18,19,20)
for(i in Calitativas) credit[,i]=as.factor(credit[,i]) #convertir a variables que son cualitaticas


library(caTools)

set.seed(2000)
spl = sample.split(credit$Creditability, SplitRatio = 0.7)
Entrenamiento = subset(credit, spl==TRUE)
Testeo = subset(credit, spl==FALSE)


library(randomForest)

BosquesA = randomForest(Creditability  ~ ., data = Entrenamiento, 
                        importance =T,# Evalución de la importancia de los predictores
                        ntree = 500   # Número de arboles en el bosque
                        )


BosquesA # visualizar las caracteristicas de mi bosque. Se halla támbien, 
         # a matriz de confución in-sample

BosquesA.Pred = predict(BosquesA, Testeo)

table(BosquesA.Pred, Testeo$Creditability)

plot(BosquesA) #Graficacion de Error para cada árbol 

importance(BosquesA)# Importancia de los predictores medante diferentes criterios
                    # Recuesde: que sucederia con la exactitud o el coeficionte de 
                    # GINI promedio al quirar dicho predictor

varImpPlot(BosquesA) #Graficación de los resultados anteriores



## Forma Alternativa de computar los Bosques Aleatorios 



## Procesamiento en paralelo de bosques aleatorios



l