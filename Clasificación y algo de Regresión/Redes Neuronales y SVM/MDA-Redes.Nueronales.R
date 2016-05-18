'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos ANN

Juan Esteban Mejía Velásquez

'''


## Modelamiento de la fuerza de concreto

'''
https://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength

Cement (component 1) -- quantitative -- kg in a m3 mixture -- Input Variable 
Blast Furnace Slag (component 2) -- quantitative -- kg in a m3 mixture -- Input Variable 
Fly Ash (component 3) -- quantitative -- kg in a m3 mixture -- Input Variable 
Water (component 4) -- quantitative -- kg in a m3 mixture -- Input Variable 
Superplasticizer (component 5) -- quantitative -- kg in a m3 mixture -- Input Variable 
Coarse Aggregate (component 6) -- quantitative -- kg in a m3 mixture -- Input Variable 
Fine Aggregate (component 7)	-- quantitative -- kg in a m3 mixture -- Input Variable 
Age -- quantitative -- Day (1~365) -- Input Variable 
Concrete compressive strength -- quantitative -- MPa -- Output Variable 

'''

## Exploración de la base de datos

Concreto <- read.csv("concrete.csv")


# Función para normalizar

Normalizar <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# Aplicar normalización

ConcretoNorm<- as.data.frame(lapply(concrete, Normalizar))

# Confirmar que el rango este entre 0 y 1

summary(ConcretoNorm$strength)

# Comparación

summary(Concreto$strength)

# Particion de la base de datos simple 

Entrenamiento  <- ConcretoNorm[1:773, ]
Testeo <- ConcretoNorm[774:1030, ]

# Modelo de Red Neuronal artificial(ANN en sus siglas en inglés) 
#  mediante ' neuronalnet

if(!require(neuralnet, quietly=TRUE))install.packages('neuralnet')

library(neuralnet)

# Simple ANN con una neurona olcuta

set.seed(123) 

Concreto.M <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = Entrenamiento)

# Vizualizar la topologia de la Red neuronal
plot(Concreto.M)


## Evaluación de Rendimiento

# Optención de resutados. La funcion 'compute' calcula los vectores
# de covarianza

Concreto.M.Resul <- compute(Concreto.M, Testeo[1:8])

# Optencion de las predicciones 

Pred <- Concreto.M.Resul$net.result

# examine the correlation between predicted and actual values

cor(Pred, Testeo$strength)

## Mejorar el modelo

# Red neuronal con 5 neuronas ocultas

set.seed(123) 

Concreto.M2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 5)

# Graficas

plot(Concreto.M2)

# Evaluación

Concreto.M.Resul2 <- compute(Concreto.M2, Testeo[1:8])
Pred2 <- Concreto.M.Resul2$net.result
cor(Pred2, Testeo$strength)
