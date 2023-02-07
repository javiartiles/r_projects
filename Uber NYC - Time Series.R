
rm(list = ls())

getwd() 
library(readxl)
datos <- read_excel("uber.xlsx")
View(datos)

#buscamos si hay alg?n NA para calcularlos con la funci?n na.approx()
sum(is.na(datos))

#creamos una representaci?n gr?fica de los Viajes en Uber en NYC desde enero del 2015 hasta febrero del 2020
uber = ts(datos$`Trips per Day`, start =2015, frequency = 12) 
uber
plot(uber, sub="Figura 1: Viajes en Uber en NYC", xlab = "Tiempo", ylab = "N?mero de Viajes")

#descomponemos la serie para adem?s obtener los gr?ficos de la tendencia, la estacionalidad y el ruido blanco o componente aleatorio
componentes.ST = decompose(uber) 
plot(componentes.ST)
#con estos gr?ficos podremos estudiar el modelo de suavizado que debemos utilizar

#creamos un boxplot con 12 observaciones por a?o para visualizar la media de cada mes de a?o en toda la serie
cycle(uber)
boxplot(uber~ cycle(uber))

#------------------------------- MODELO DE SUAVIZADO ----------------------------------

#existe tendencia y estacionalidad por lo que utilizaremos el modelo de Holt Winters
Holt = HoltWinters(uber)
Holt
plot(Holt)
#hacemos una predicci?n de 30 periodos
Prediccion = predict(Holt, n.ahead = 30, prediction.interval = T, level = 0.95) 
plot(Holt, Prediccion, sub="Figura 2: Predicci?n de 30 Periodos Modelo Holt-Winters")

#----------------------------------- MODELO ARIMA ---------------------------------------

par(mfrow=c(1,3)) #indica el numero de filas (1) y de columnas (3), en las que queremos que se divida la pantalla
plot(uber, sub="Figura 3: Viajes en Uber en NYC", xlab = "Tiempo", ylab = "N?mero de Viajes", col="blue")
#calculamos las FAS y las FAP 
acf(uber, sub="Figura 4: Funci?n de Autocorrelaciones")
pacf(uber, sub="Figura 5: Funci?n de Autocorrelaciones Parciales") #estas son las importantes para determinar el valor de p
#no obstante, aunque representemos las FAP es muy d?ficil sacar conclusiones claras, por lo que utilizaremos auto.arima

#tambi?n podemos utilizar la funci?n ggtsdisplay() para obtener la serie temporal, las FAS y las FAP
library(forecast) #visualizaci?n de datos y ajustar los datos al modelo que tenemos
ggtsdisplay(uber, lag.max = 100) 
#gr?fica de los residuos
library(tsoutliers) #detectar outliers en la serie
outliers <- tso(uber)
outliers #nos encontramos con outliers de muchos tipos: additive outliers (AO), temporary changes (TC) y level shift(LS)
#los dos primeros afectan al componente aleatorio, mientras que los LS est?n asociados a la tendencia c?clica de la serie
#AO = un pico aislado (at?pico aditivo); TC = cambio de nivel; LS = un pico que tarda varios periodos en desaparecer (at?pico innovador)
#visualizamos los at?picos para poder interpretar mejor los resultados seg?n la fecha en la que ocurren
plot(outliers) #comprobamos que es as?

#si quisieramos eliminar los outliers hacemos lo siguiente, no obstante consideramos que aportan informaci?n importante, sobre todo en los ?ltimos periodos
#uber_nuevo <-outliers$yadj
#outliers <-tso(uber_nuevo)
#outliers 
#en efecto nos aparece que no tenemos ning?n at?pico, en el caso de que saliese deber?amos dejarlo por si es relevante para la serie

#creamos un conjunto de datos de entrenamiento
validacion=13
train=length(uber)-validacion
train_data=window(uber, start=2015,end=2019)
train_data

library(MASS) #hacer una transformaci?n Box-Cox
#comprobamos que no tendrmos problemas de heterocedasticidad
#transformaci?n BoxCox
Lambda=BoxCox.lambda(train_data)
Lambda
#Lambda = 0.91
uber_1 = BoxCox(train_data,Lambda) #estabilizar la varianza
#transformaci?n BoxCox que con un par?metro Lambda para que no tenga problemas de heterocedasticidad

par(mfrow=c(1,2))
acf(uber_1, sub="Figura 6: Funci?n de Autocorrelaciones sin Heterocedasticidad") #observamos un decrecimiento lento de las FAS
pacf(uber_1, sub="Figura 7: Funci?n de Autocorrelaciones Parciales sin Heterocedasticidad")
#observamos que existe un decrecimiento lento de las FAS

library("fUnitRoots")
urkpssTest(uber_1, type=c("tau"), lags=c("short"), use.lag = NULL, doplot = TRUE) #KPSS test con estacionalidad como hip?tesis nula
#eliminamos la no estacionariedad
#calculamos la diferenciaci?n regular
Bz=diff(uber_1,differences = 1) #diferencia de orden 1
acf(Bz, sub="Figura 8: Funci?n de Autocorrelaciones sin Heterocedasticidad y con Diferenciaci?n Regular")
pacf(Bz, sub="Figura 9: Funci?n de Autocorrelaciones Parciales sin Heterocedasticidad y con Diferenciaci?n Regular")
#ha habido variaci?n, no obstante hacemos una diferenciaci?n estacional
B12Bz=diff(Bz, lag = 12, differences = 1)
acf(B12Bz, sub="Figura 10: Funci?n de Autocorrelaciones sin Heterocedasticidad y con Diferenciaci?n Estacional")
pacf(B12Bz, sub="Figura 11: Funci?n de Autocorrelaciones Parciales sin Heterocedasticidad y con Diferenciaci?n Estacional")
#dif?cil decir el orden que tiene, as? que utilizaremos la funci?n auto.arima

par(mfrow=c(1,1))
#nos devolver? el mejor modelo de ARIMA seg?n el AIC
auto.arima(B12Bz, stepwise=FALSE,approx=FALSE) 
#el mejor modelo es un ARIMA(1,0,0)12(0,0,1) con media 0
#AIC=1091.15
#la parte regular es un AR(1), y la parte estacional es un MA(1)
#podemos comprobarlo nosotros

#calculamos el modelo ARIMA completo
modelo = Arima(train_data, order=c(1,1,0), 
                  #es (0,1,1) porque hemos calculado que tenemos que diferenciar
                  seasonal = list(order=c(0,1,1), period=12),
                  lambda = Lambda,
                  #para hacer la transformaci?n BoxCox
                  include.constant = TRUE)
modelo #AIC=717.74

library(lmtest)
coeftest(modelo) #ver si los coeficientes son o no significativos
#si no lo fueran tendr?amos que plantear un modelo diferente
confint(modelo) #para eliminar los coeficientes insignificantes

tsdiag(modelo) #un primer diagn?stico de los residuos
#el test de Ljung-Box nos gusta, no presenta nada de lo que preocuparnos
#estudio de una posible variabilidad de los residuos del modelo
par(mfrow=c(1,3))
plot.ts(modelo$residuals, sub="Figura 12: Representaci?n de los Residuos del Modelo ARIMA", 
                xlab="Tiempo", ylab="Residuos")
acf(modelo$residuals, sub="Figura 13: Funci?n de Autocorrelaciones de los Residuos del Modelo ARIMA",
                xlab="Tiempo", ylab="Residuos")
#no muestra correlaciones significativas
pacf(modelo$residuals, sub="Figura 14: Funci?n de Autocorrelaciones Parciales de los Residuos del Modelo ARIMA",
                xlab="Tiempo", ylab="Residuos")

par(mfrow=c(1,2))
library(FitAR)
boxresult=LjungBoxTest (modelo$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")
#los p-valores est?n pr?cticamente todos por encima del 0.5, indicando que no son significativos
#adem?s siguen una l?nea curva, y no est?n distribuidos de forma aleatoria
qqnorm(modelo$residuals) #gr?fico de cuantiles que compara los cuantiles de nuestros datos con los de una distribuci?n normal N(0,1)
qqline(modelo$residuals)
#observamos que existe asimetr?a positiva
#si los datos se alejasen mucho de la l?nea, entonces no seguir?an una distribuci?n normal
#en nuestro caso, a pesar de que no siguen completamente la l?nea, no deber?amos rechazar que sigan una distribuci? normal
#todos los gr?ficos respaldan que no existe ning?n patr?n en el comportamiento de los residuos por lo que podemos calcular la predicci?n

par(mfrow=c(1,1))
#dibujamos la serie temporal y el ajuste
autoplot(uber, series= "Real")+forecast::autolayer(modelo$fitted, series= "Serie Ajustada")
prediccion = forecast(modelo, h=13)
prediccion
plot(prediccion, sub="Figura 15: Predicci?n de 13 Periodos Modelo ARIMA")
#dos formas de comparar la predicci?n de datos del validation set con la serie original
autoplot(uber, series= "Real")+forecast::autolayer(prediccion$fitted, series= "Serie Ajustada")
modelo %>%
  forecast(h=13) %>%
  autoplot()+autolayer(uber)
#comparamos con los datos originales
#observamos que la predicci?n no es muy buena
