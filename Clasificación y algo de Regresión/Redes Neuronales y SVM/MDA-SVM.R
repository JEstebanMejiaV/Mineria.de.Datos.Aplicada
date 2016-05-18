'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos SVM

Juan Esteban Mejía Velásquez

'''

https://archive.ics.uci.edu/ml/datasets/Letter+Recognition

'''
Attribute Information:

1.	lettr	capital letter	(26 values from A to Z) 
2.	x-box	horizontal position of box	(integer) 
3.	y-box	vertical position of box	(integer) 
4.	width	width of box	(integer) 
5.	high height of box	(integer) 
6.	onpix	total # on pixels	(integer) 
7.	x-bar	mean x of on pixels in box	(integer) 
8.	y-bar	mean y of on pixels in box	(integer) 
9.	x2bar	mean x variance	(integer) 
10.	y2bar	mean y variance	(integer) 
11.	xybar	mean x y correlation	(integer) 
12.	x2ybr	mean of x * x * y	(integer) 
13.	xy2br	mean of x * y * y	(integer) 
14.	x-ege	mean edge count left to right	(integer) 
15.	xegvy	correlation of x-ege with y	(integer) 
16.	y-ege	mean edge count bottom to top	(integer) 
17.	yegvx	correlation of y-ege with x	(integer)

'''
# Leer el conjunto de datos
Letras <- read.csv("letterdata.csv")

# divide into training and test data

library(caTools)

spl <- sample.split(Letras, SplitRatio = 0.7)

Entrenamiento <- subset(Letras, spl==TRUE)
  
Testeo <- subset(Letras, spl==FALSE)

# Entrenar e modelo con SVM ineal
# El parametro vanilladot apica un kerner lineal

library(kernlab)

Letras.M <- ksvm(letter ~ ., data = Entrenamiento,
                          kernel = "vanilladot")


# Ver las caracteristicas del modelo

Letras.M

# Predicciones

Pred <- predict(Letras.M, Testeo)


table(Pred, Testeo$letter)

# VEr en total contos me clasificaron bien. 
# Cálculo de exactitud
# Cálculo porcentaje de error

BienClasificados <- Pred == Testeo$letter
table(BienClasificados)
table(BienClasificados)/nrow(Testeo)

## Mejora del modelo
# El parametro rbfdot apica un kerner radial 'Gausiano'

set.seed(123)
Letras.M2 <- ksvm(letter ~ ., data = Entrenamiento, kernel = "rbfdot")
Pred2 <- predict(Letras.M2, Testeo)

BienClasificados2 <- Pred2 == Testeo$letter
table(BienClasificados2)
table(BienClasificados2)/nrow(Testeo)
