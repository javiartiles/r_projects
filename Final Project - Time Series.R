### ---- MACHINE LEARNING II ---- ###

# Limpiamos el entorno para no tener posibles datos de otros Scripts

rm(list = ls())

# Establecemos el Directorio dse Trabajo con los datos los cuales vamos a trabajar

getwd()
library(readr)
Datos <- read_csv("Artiles-Gonzalez-Porto.csv")
View(Datos) # Como podemos ver estamos ante un Dataset con observaciones mensuales durante 20 años. 
            # Debido a que nos encontramos con valores postivios y negativos vamos a entender que los valores son de beneficios o perdidas mensuales.

#Para evitar posibles problemas en los datos, buscamos si hay alguna celda sin datos (NA)

sum(is.na(Datos)) #Observamos que no existen celdas sin datos (NA = 0)

# Vamos a visualziar los datos en una gráfica 

resultado = ts(Datos$x, start = 2001, frequency = 12) 
resultado
plot(resultado, sub = "Gráfica 1: Resultado", xlab = "Tiempo", ylab = "Beneficios y Perdidas")

# Para poder obtener la TENDENCIA, ESTACIONALIDAD y COMPONENTES ALEATORIOS (ruido blanco) descomponemos la serie

componentes.ST = decompose(resultado) 
plot(componentes.ST, xlab = "Tiempo") # En base a estos graficos determinaremos el modelo de suavizado

cycle(resultado)
boxplot(resultado~ cycle(resultado), xlab = "Media Meses") # Visualizamos en 12 observaciones la media total de cada mes

# ------------------------------- MODELO DE SUAVIZADO (HOLT-WINTERS) ----------------------------------

# Utilizamos el Modelo de Suvizado HOLT-WINTERS ya que existe tendencia y estacionalidad

Holt = HoltWinters(resultado)
Holt
plot(Holt)

Prediccion = predict(Holt, n.ahead = 30, prediction.interval = T, level = 0.95) # Predecimos a 30 periodos
plot(Holt, Prediccion, sub="Gráfica 2: Prediccion 30 Periodos Holt-Winters")

# ----------------------------------- MODELO ARIMA ---------------------------------------

par(mfrow=c(1,3))

plot(resultado, sub="Gráfica 3: Resultado", xlab = "Tiempo", ylab = "Beneficios y Perdidas", col="blue")
acf(resultado, sub="Gráfica: FAS")
pacf(resultado, sub="Gráfica: FAP") # De esta gráfica determinaremos el valor de "p"

# Observamos la gráfica de FAP y vamos a utilizar AUTO.ARIMA para sacar mejores conclusiones

library(forecast)
ggtsdisplay(resultado, lag.max = 100) # Esta seria otra forma de obtener la serie temporal, FAS y FAP

# Pasamos a buscar los residuos de los datos (Outliers)

library(tsoutliers)
outliers <- tso(resultado)
outliers

# Solo hay outliers de un tipo: additive outliers (AO) = pico aislado (atipico aditivo). Este outlier afecta al componente aleatorio
plot(outliers) # Observamos los puntos que serian outliers

# Procedemos a eliminar los outliers ya que no nos ofrecen ninguna informacion

sin_outliers <- outliers$yadj
outliers <- tso(sin_outliers)
outliers 
plot(outliers)

# ------------------------------------- TRAIN-SET -------------------------------------

# En primer lugar, creamos el conjunto TRAIN, el de entrenamiento

validacion = 24
train=length(resultado)-validacion # Tal y como dice el enunciado eliminamos los dos últimos años

train_data=window(resultado, start = c(2001, 1), end = c(2001, train))
train_data

library(MASS)
# Nos disponemos a hacer una transformacion Box-Cox por lo que comprobamos que no hay problemas de heterocedasticidad

Lambda=BoxCox.lambda(train_data) # Transformacion Box-Cox
Lambda # Lambda = 1.83
resultado_1 = BoxCox(train_data,Lambda) # Estabilizamos la varianza:
                                        # Para elo transformamos a Box-Cox  con un parametro Lambda para que no tenga problemas de heterocedasticidad

par(mfrow=c(1,2))
acf(resultado_1, sub="Grafico 6: Funcion de Autocorrelaciones sin Heterocedasticidad") # Claramente hay un decrecimiento lento de las FAS
pacf(resultado_1, sub="Grafico 7: Funcion de Autocorrelaciones Parciales sin Heterocedasticidad")

library("fUnitRoots")
urkpssTest(resultado_1, type=c("tau"), lags=c("short"), use.lag = NULL, doplot = TRUE) # KPSS:
                                                                                       # Test con estacionalidad como hipotesis nula

# En primer lugar, vamos a eliminar la no estacionariedad y calcular la diferenciación regular
Bz=diff(resultado_1,differences = 1) #diferencia de orden 1
acf(Bz, sub="Grafico 8: Funcion de Autocorrelaciones sin Heterocedasticidad y con Diferenciacion Regular")
pacf(Bz, sub="Grafico 9: Funcion de Autocorrelaciones Parciales sin Heterocedasticidad y con Diferenciacion Regular")

auto.arima(Bz) # Calculamos el modelo ARIMA
# Se nos recomienda el modelo ARIMA(0,1,2)12(0,0,1) con media 0
# AIC = 2993.75

# Ha habido variacion, pero aun asi calculamos con diferenciacion estacional

# En segundo lugar, vamos a eliminar la no estacionariedad y calcular la diferenciación estacional
B12Bz=diff(Bz, lag = 12, differences = 1)
acf(B12Bz, sub="Grafico 10: Funcion de Autocorrelaciones sin Heterocedasticidad y con Diferenciacion Estacional")
pacf(B12Bz, sub="Grafico 11: Funcion de Autocorrelaciones Parciales sin Heterocedasticidad y con Diferenciacion Estacional")

# Utilizaremos AUTO.ARIMA ya que es dificil visualmente decir el orden que tiene

auto.arima(B12Bz) # Calculamos el modelo ARIMA 
# Se nos recomienda el modelo ARIMA(4,0,1)12(2,0,0) con media 0
# AIC = 2886.03

# La parte regular es un AR(4), y la parte estacional es un MA(1)
# Podemos comprobarlo nosotros

per=12 #sabemos que la estacionalidad es de periodo 12
ordenregular = 4 #orden regular queremos probar
ordenestacional = 2 #orden estacional queremos probar
difregular = 1 # orden de diferenciaci�n regular queremos probar
difestacional = 1 #ºorden de diferenciaci�n estacional queremos probar

lista<-c()
lista2<-c()
for (p in 0:ordenregular){
  for (d in 0:difregular){
    for (q in 0:ordenregular){
      for (P in 0:ordenestacional){
        for (D in 0:difestacional){
          for (Q in 0:ordenestacional){
            prueba = try(Arima(train_data, order=c(p,d,q),
                               seasonal = list(order=c(P,D,Q), period=per),
                               lambda = Lambda,
                               include.constant = TRUE),silent = TRUE)
            
            p1<-try(prueba$aic, silent = TRUE)
            lista<-append(lista,p1,after = length(lista))
            ORDEN<-paste(p,d,q,P,D,Q)
            lista2<-append(lista2,ORDEN,after = length(lista2))
            
          }}}}}}

valores<-na.omit(data.frame(as.numeric(lista),lista2))
valores

dimension=dim(valores)
dimension[1] #700 filas en este caso

#Partimos de un modelo naive

Modelo.naive = Arima(train_data, order=c(0,0,0),
                     seasonal = list(order=c(0,0,0), period=12),
                     lambda = Lambda,
                     include.constant = TRUE)

mejor.aic = Modelo.naive$aic
mejor.aic

for (i in 1:dimension[1])
{
  if(valores[i,1] < mejor.aic)
  {
    mejor.aic=valores[i,1]
    mejor.modelo=valores[i,2]
  }
  
}

mejor.modelo

# Debido a que, según el bucle, el AIC del modelo ARIMA (3,1,4)(0,1,1) es igual a 3900.508 no haremos uso de este modelo

# Obtengo los valores p,d,q de un modelo ajustado por auto.arima (nos quedamos con el modelo B12Bz)

modelo=auto.arima(B12Bz)
orden=arimaorder(modelo)
orden

library(lmtest)
coeftest(modelo) # Ver si los coeficientes son o no significativos ya que si no lo fueran habria que plantear un modelo distinto
confint(modelo) # Eliminamos los coeficientes insignificantes

tsdiag(modelo) # Diagnostio de los residuos
#el test de Ljung-Box nos gusta, no presenta nada de lo que preocuparnos

# Estudiamos la poisbilidad de una variabilidad en los residuos del modelo
par(mfrow=c(1,3))
plot.ts(modelo$residuals, sub="Grafico 12: Representacion de los Residuos del Modelo ARIMA", 
                xlab="Tiempo", ylab="Residuos")
acf(modelo$residuals, sub="Grafico 13: Funcion de Autocorrelaciones de los Residuos del Modelo ARIMA",
                xlab="Tiempo", ylab="Residuos")
# No muestra correlaciones significativas
pacf(modelo$residuals, sub="Grafico 14: Funcion de Autocorrelaciones Parciales de los Residuos del Modelo ARIMA",
                xlab="Tiempo", ylab="Residuos")

par(mfrow=c(1,2))
library(FitAR)
boxresult=LjungBoxTest (modelo$residuals,k=2,StartLag=1)
plot(boxresult[,3],main= "Ljung-Box Q Test", ylab= "P-values", xlab= "Lag")

# Los p-valores estan practicamente todos por encima del 0.5 = NO son significativos
# Se puede observar la linea curva que siguen y la NO aleatoriedad (exeptuando algunos valores) de los valores

qqnorm(modelo$residuals) # Comparacion de los cuantiles de nuestro dataset con uno de distribución normal (N(0,1))
qqline(modelo$residuals)

# Hay asimetria positiva
# En caso de alejarse los datos de la linea, no habria una distribucion normal
# Los graficos defienden que el comportamiento de los residuos en la predicción no sigue un patron definido

par(mfrow=c(1,1))
# Serie temporal + ajuste
autoplot(resultado, series= "Real")+forecast::autolayer(modelo$fitted, series= "Serie Ajustada")
prediccion = forecast(modelo, h=36)
prediccion
plot(prediccion, sub="Grafico 15: Prediccion Modelo ARIMA (36 Periodos)")

# Comparamos la prediccion de datos del validation set con la serie original
autoplot(resultado, series= "Real")+forecast::autolayer(prediccion$fitted, series= "Serie Ajustada")

modelo %>%
  forecast(h=36) %>%
  autoplot()+autolayer(resultado)

# Una vez comparado con los datos originales concluimos que la predicción es buena
