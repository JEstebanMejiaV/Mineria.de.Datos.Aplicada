
'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ilustración Reglas de Asociación

Juan Esteban Mejía Velásquez

'''

if(!require(arules, quietly=TRUE))install.packages("arules")
if(!require(arulesViz, quietly=TRUE))install.packages("arulesViz")
if(!require(RColorBrewer, quietly=TRUE))install.packages("RColorBrewer")
if(!require(scatterplot3d, quietly=TRUE))install.packages("scatterplot3d")


library(arules)  # Reglas de Asociacion
library(arulesViz)  # Vizuaización de Reglas de Asociacion
library(RColorBrewer)  # Paletas de colores para las graficas
library(cluster)  # Analissi de cluster

data(Groceries)  # Trasnsacciones de tienda desde e paquete arules 



Groceries

class(Groceries)

## Crear como data.frame

items<-as.data.frame(itemLabels(Groceries))
colnames(items) <- "Item"
head(items,10)

## Calcular las cuentas de soporte (Suoporte absoluto)

itemFrequency(Groceries,type = "absolute")

## Calcular el soporte (Suoporte relativo)

round(itemFrequency(Groceries,type = "relative")*100,2)

## Graficas el soporte

plot(round(itemFrequency(Groceries,type = "relative")*100,2), col="navy")

### Graficar la frecuencia por item

par(mfrow = c(1, 2))

itemFrequencyPlot(Groceries, topN = 10,col="darkmagenta")
itemFrequencyPlot(Groceries, support = 0.1,col="deepskyblue4")

par(mfrow = c(1, 1))

itemFrequencyPlot(Groceries, support = 0.025, cex.names=0.8, xlim = c(0,0.3),
                  type = "relative", horiz = TRUE, col = "royalblue4", las = 1,
                  xlab = paste("Proporción de la canasta de bienes que contiene el item",
                               "\n(Frecuencia relativa o Soporte)"))

## Creacion de clúster jerásquico para seguir explorando

x <- dissimilarity(Groceries, which = "items")
x[is.na(x)] <- 1 

hcd<-hclust(x)
plot(hcd, cex=.6) # cex controla e tamaño de las etiquetas

## Ahora sí, vamso a aplicar el algorítmo Apriori

rules <- apriori (Groceries, parameter = list(supp = 0.001, conf = 0.8))

rules2 <- apriori (Groceries, parameter = list(supp = 0.002, conf = 0.8))

rules3 <- apriori (Groceries,parameter = list(supp = 0.001, conf = 0.6))

rules

rules2

rules3

## Organizar por confianza de manera decreciente

rules<-sort(rules, by="confidence", decreasing=TRUE)

summary(rules)

## Ver las primeras 10 reglas

inspect(rules[1:10])


## Subcunjunto de reglas
## Encontrar so cunjuntos de regas donde precedente sea compras de yogurt

yogurt <- subset(rules, subset = rhs %pin% "yogurt")

# Ordenar por confianza

yogurt<-sort(yogurt, by="confidence", decreasing=TRUE)

# Inspecionar el top 10

inspect(yogurt[1:5])

plot(rules, control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),
     shading = "lift")  

plot(second.rules, method="grouped",   
     control=list(col = rev(brewer.pal(9, "Greens")[4:9])))

plot(rules[1:10],method="graph",interactive=TRUE)

# Borrar redundantes

subset <- is.subset(rules, rules)
subset[lower.tri(subset, diag=T)] <- NA
Redundantes <- colSums(subset, na.rm=T) >= 1
Podado <- rules[!Redundantes]
ReglasPodadas<-Podado
plot(ReglasPodadas[1:10],method="graph",interactive=TRUE)


