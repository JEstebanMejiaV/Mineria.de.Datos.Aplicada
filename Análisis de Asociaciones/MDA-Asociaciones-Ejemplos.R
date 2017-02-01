

# Minería de Datos Aplicada
# Universidad Nacional de Colombia
# 
# Ilustración Reglas de Asociación
# 
# Juan Esteban Mejía Velásquez



## Crear como data.frame

if(!require(arules, quietly=TRUE))install.packages("arules")
if(!require(arulesViz, quietly=TRUE))install.packages("arulesViz")
if(!require(RColorBrewer, quietly=TRUE))install.packages("RColorBrewer")
if(!require(scatterplot3d, quietly=TRUE))install.packages("scatterplot3d")


library(arules)  # Reglas de Asociacion
library(arulesViz)  # Vizuaización de Reglas de Asociacion
library(RColorBrewer)  # Paletas de colores para las graficas
library(cluster)  # Analissi de cluster

data(Groceries)  # Trasnsacciones de tienda desde el paquete arules 


Groceries
class(Groceries)

## Extraer el númeor de items en la la estructura transaciones 
## y convertirla en un dataframe

items<-as.data.frame(itemLabels(Groceries))
colnames(items) <- "Item"
head(items,10)
nrow(items) # Número de items (Articulos)

## Calcular las cuentas de soporte (Suoporte absoluto)

itemFrequency(Groceries,type = "absolute")

head(itemFrequency(Groceries,type = "absolute"), 5)

tail(itemFrequency(Groceries,type = "absolute"))


head(sort(itemFrequency(Groceries,type = "absolute"), decreasing = T), 5)

## Calcular el soporte (Suoporte relativo)


head(itemFrequency(Groceries,type = "relative"), 10)

round(itemFrequency(Groceries,type = "relative")*100,2) # En porcentaje

head(sort(round(itemFrequency(Groceries,type = "relative")*100,2), decreasing = T), 5)


## Graficas el soporte simple

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

## Creacion de cluster jerásquico para seguir explorando

x <- dissimilarity(Groceries, which = "items")
x[is.na(x)] <- 1 

HC<-hclust(x)
plot(HC, cex=.65) # cex controla e tamaño de las etiquetas

## Ahora , vamos a aplicar el algorótmo Apriori

# Dada la grna variedad de articulos los soportes son muy bajos, por ello
# en este caso particular nuestros maores son del orden de 0.001.

# Crearemso 3 variaciones:

# - supp = 0.001, conf = 0.8 la estandar
# - Aumentaremso legeramente el soporte a 0.002
# - Disminuiremos legeramente la confianza de 0.8 a 0.6

rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

rules2 <- apriori(Groceries, parameter = list(supp = 0.002, conf = 0.8))

rules3 <- apriori(Groceries,parameter = list(supp = 0.001, conf = 0.6))

rules
# 410 Reglas

rules2
# 11 Reglas


rules3
# 2918 Reglas

## Depurar un poco a 'rules'

# Pro ahora no dedicaremos a eliminar las reglas repetidas

subset.matrix <- is.subset(rules, rules)
subset.matrix[lower.tri(subset.matrix, diag=T)] <- NA
Redundantes <- colSums(subset.matrix, na.rm=T) >= 1
rules.podadas <- rules[!Redundantes]
rules <- rules.podadas



## Organizar por confianza de manera decreciente

rules<-sort(rules, by="confidence", decreasing=TRUE)

summary(rules)

## Ver las primeras 10 reglas

inspect(rules[1:10])


## Subcunjunto de reglas
## Encontrar so cunjuntos de regas donde precedente sea compras de yogurt

yogurt <- subset(rules, subset = rhs %pin% "yogurt")

# Ordenar por confianza

yogurt <- sort(yogurt, by="confidence", decreasing=TRUE)

# Inspecionar el top 10

inspect(yogurt[1:5])


# Otra forma. desde la aplicacion de apriori

OtherVegetables <- apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.08), 
                   appearance = list(default="lhs",rhs="other vegetables"),
                   control = list(verbose=F))

OtherVegetables <- sort(OtherVegetables, decreasing=TRUE,by="confidence")

inspect(OtherVegetables[1:5])



## Datos que contengan un concecuente espesifico

WholeMilk <- apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15,minlen=2), 
               appearance = list(default="rhs",lhs="whole milk"),
               control = list(verbose=F))

WholeMilk <- sort(WholeMilk, decreasing=TRUE,by="confidence")

inspect(WholeMilk[1:5])


### Otros Graficos

plot(rules, control=list(jitter=2, col = rev(brewer.pal(9, "Greens")[4:9])),
     shading = "lift")  

plot(rules, method="grouped",   
     control=list(col = rev(brewer.pal(9, "Greens")[4:9])))

plot(rules[1:10],method="graph",interactive=TRUE)

plot(rules[1:10],method="graph",interactive=TRUE,shading=NA)


plot( head(sort(rules, by="lift"), 10), method="paracoord")

plot( head(sort(rules, by="lift"), 10), method="paracoord", 
      control=list(reorder=TRUE))

# Grafics Doubledecker  para 1 regla

Regla <- head(sort(rules, by="lift"), 1)

inspect(Regla)

plot(Regla, method="doubledecker", data = Groceries)


