
## Aplicación de el filtro Hodrick- Prescott


# Libreria con algunos fitros de series de tiempo
if(!require(mFilter, quietly=TRUE))install.packages('mFilter')
library(mFilter)

# Libreria para el modelaje financiero
# Aqui bajaremso os datos de prueba que seran de 
# Productor interno bruto (GDP en sus siglas en ingés)
if(!require(quantmod, quietly=TRUE))install.packages('quantmod')
library(quantmod)

# El siguinte comando pertenese a la libreria 'quantmod' 
# el cual carga los datos GDP ya en obketo de series de tiempo

getSymbols('GDP',src='FRED')

# hpfilter comando para aplicar el filtro Hodrick- Prescott

Fhp <- hpfilter(log(GDP),freq = 1600)

# Cramos un objeto series de tiempo con os componentes
# ciclico, tendencia, y dato real (en este caso espesifico log(GDP))

GDP2 <- xts(cbind(Fhp$x, Fhp$trend, Fhp$cycle), index(GDP))

# Dar nombres a als variabes (Campos)

colnames(GDP2) <- c("x", "Tendencia", "Ciclo")

## Graficación

par(mfrow = c(2, 1), mar = c(3, 2, 2, 1))
plot(GDP2[,"x"], t= "n", main = paste(Fhp$title, "de", Fhp$xname))
lines(GDP2[,"x"], col = "steelblue")
lines(GDP2[,"Tendencia"], col = "red")
legend("topleft", legend = c(Fhp$xname, "Tendencia"), col = c("steelblue", "red"), lty = rep(1, 2), ncol = 2)
plot(GDP2[,"Ciclo"], t = "n", main = "CComponente Ciclico (Desviación de la Tendencia)")
lines(GDP2[,"Ciclo"], col = "steelblue")