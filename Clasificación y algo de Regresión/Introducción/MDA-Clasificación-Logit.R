
'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos Regresión logistica

Juan Esteban Mejía Velásquez

'''
## Graficar un modeo de regresion logistica

beta0 <- 1
beta1 <- 0.5
curve( expr = exp( beta0 + beta1 *x) / (1+ exp( beta0 + beta1 *x)), xlim
          = c(-15, 15) , col = " black ", 
       main = expression(pi == frac (e ^{1+0.5* x[1]} , 1+e ^{1+0.5* x [1]}) ),
       xlab =expression (x [1]) , ylab = expression (pi))

## Ejmeplo de scoring

if(!require(ISLR, quietly=TRUE))install.packages("ISLR")

library(ISLR)

summary(Default)

plot(Default[3:4], col=Default$default)

xtabs(~ default + student, data= Default)

Mlogit1 <- glm(default ~ ., data = Default, family = "binomial")

summary(Mlogit1)


Mlogit2 <- glm(default ~ .-income, data = Default, family = "binomial")

summary(Mlogit2)



## Intervalos de confianza usando Log-verosimilitud

confint(Mlogit2)

## Intervalos de confianza usando errores estandar

confint.default(Mlogit2)

if(!require(aod, quietly=TRUE))install.packages("aod")

library(aod)

wald.test(b = coef(Mlogit2), Sigma = vcov(Mlogit2), Terms = 2:3)

wald.test(b = coef(Mlogit2), Sigma = vcov(Mlogit2), Terms = 3)

## Ratios Odd

exp(coef(Mlogit2))

## Ratios Odd a un 95% de confianza

exp(cbind(OR = coef(Mlogit2), confint(Mlogit2)))

## R^2 de McFadden y otros Pseudo R^2

if(!require(pscl, quietly=TRUE))install.packages("pscl")

library(pscl)

pR2(Mlogit2)


## importancia mediante el valor absoluto de el t-estadistico

if(!require(caret, quietly=TRUE))install.packages("caret")

library(caret)

varImp(Mlogit2)


## importancia mediante medainte análisis de sensivilidad de varianza

if(!require(rminer, quietly=TRUE))install.packages("rminer")

library(rminer)

Modelo <- fit(default~.,Default,model="lr")

Importancia <- Importance(Modelo,Default,method="sensv")

# Para otros metodos de evaluar importancia :
# http://www.inside-r.org/packages/cran/rminer/docs/Importance

L <- list(runs=1,sen=t(Importancia$imp),sresponses=Importancia$sresponses)

mgraph(L,graph="IMP",leg=names(Default),col="gray",Grid=10)

############# Alemán

Credito <- read.csv("german_credit.csv")

summary(Credito)

str(Credito)

