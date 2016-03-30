'''
Minería de Datos Aplicada
Universidad Nacional de Colombia

Ilustración PCA

Juan Esteban Mejía Velásquez

'''

## Cargaremos el cunjunto de datos con el cuestionario de estados emocionales
## de 3.896 personas con 72 estados posibles

if(!require(psych, quietly=TRUE))install.packages("psych")

library(psych)

data(msq)

Motiv = msq[,1:72]

## Los valores faltantes son un problema recurrente en so dataset. mieremos 
## por estados emocionales cuantos valores fatante tenemso

apply(is.na(Motiv),2,sum)


na.omit(Motiv)

head(cbind(names(motiv)),72)

Borrar = c(5, 15, 37, 38, 66)

names(Motiv[Borrar])

PCA = princomp(na.omit(Motiv[,-Borrar]))

PCA

summary(PCA)

plot(PCA$sdev^2)


if(!require(FactoMineR, quietly=TRUE))install.packages("FactoMineR")


library(FactoMineR)

# Veáse problema de imputación por la media del documento de la CEPAL

PCA2<-PCA(Motiv , scale.unit=TRUE, ncp=5, graph = FALSE)

PCA2

PCA3<-PCA(na.omit(Motiv[,-Borrar]) , scale.unit=TRUE, ncp=5, graph = FALSE)

PCA3

PCA3$eig

## Graficar los eigenvalues

barplot(PCA3$eig[, 2], names.arg=1:nrow(PCA3$eig),
        main = "Varianzas por Componente",
        xlab = "Componentes Principales",
        ylab = "% de Varianza",
        col ="navy")

## Adicionar linea que conecta los segmentos

lines(x = 1:nrow(PCA3$eig), PCA3$eig[, 2],
      type="b", pch=19, col = "red")

## Contribución de cada variable

round(PCA3$var$contrib,2)


## el Cos2 nos ayuda a determinar la calidad de la representación en 
## el mapa de factores

cos2<-PCA3$var$cos2[,1:3]
Total<-apply(cos2, 1, sum)
cos2<-cbind(cos2,Total)
x<- c("Comp.1","Comp.2","Comp.3", "Total")
cos2<- as.data.frame(round(cos2*100,2))
colnames(cos2) <- c("Comp.1","Comp.2","Comp.3","Total")

cos2

## Ayudas adicionales visuales para PCA


plot.PCA(PCA3, axes=c(1, 2), choix="var", new.plot=TRUE, col.var="#ff0000",
         col.quanti.sup="blue", label=c("var", "quanti.sup"), lim.cos2.var=0,
         title="Mapa de Factor de Variabes")


plot.PCA(PCA3, axes=c(1, 2), choix="ind", habillage="none",
         col.ind="#0000ff", col.ind.sup="blue", col.quali="magenta",
         label=c("ind","ind.sup", "quali"),new.plot=TRUE, 
         title=" Mapar de Factor de Individuos")

