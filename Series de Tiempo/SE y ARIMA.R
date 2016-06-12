
## Pronosticos SE y ARIMA

# Instalar librerias
if(!require(forecast, quietly=TRUE))install.packages('forecast')
library("forecast")

Datos <- read.table("http://training-course-material.com/images/1/19/Sales-time-series.txt",h=T)
Datos$Date <- as.Date(rawdata$Date)
head(Datos)
plot(Datos)

# Contruir objeto de series de tiempo
Datos2 <- ts(Datos$Sales,start=2001,frequency=12)
plot(Datos2)

par(mfrow = c(2, 2))  

# Pronostico usando suavizamieto exponencial
SE = forecast(Datos2)
plot(SE)
plot(SE$residuals)
plot(SE$fitted)


# Pronostico usando metodo ARIMA

# auto.arima es una funcion que automaticamente determina
# los rezagos correspondintes.No recomendada

ARIMA= auto.arima(Datos2)
ARIMA
fc.arima = forecast(ARIMA)
fc.arima
plot(fc.arima)

# Evalaución de la exactitud de los modelos
accuracy(SE)
accuracy(ARIMA)