
'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ejemplos Regresión logistica

Juan Esteban Mejía Velásquez

'''

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


############# Alemán

Credito <- read.csv("german_credit.csv")

summary(Credito)

str(Credito)

