install.packages("caret")
install.packages("nnet")
install.packages("NeuralNetTools")

library(caret)
library(NeuralNetTools)
library(nnet)

library(readxl)
datos <- read_excel("viviendasChamberi_limpio.xlsx")




#particion
RNGkind("Super", "Inversion", "Rounding")     # para tener todos el mismo generador de numeros aleatorios
# RNGkind("Super", "Inversion")  #si a alguien le sale error en la anterior
set.seed(123)
index<-createDataPartition(datos$Precio, p=0.75, list=FALSE)
train<-datos[index,]
test<-datos[-index,]

#procesamos los datos para que esten en rango 0-1

preProcessRangeModel<-preProcess(train, method=c("range")) 

trainproc<-predict(preProcessRangeModel, train)

summary(trainproc)

#parametros de cross validacion

control<-trainControl(method="repeatedcv", number=10, repeats=3)

#hiperparametros a optimizar con nnet

modelLookup("nnet")


#creamos grid de combinaciones de hiperparametros
grid<-expand.grid(size=c(2:10), decay=c(0.01))

#entrenamiento de la red
RNGkind("Super", "Inversion", "Rounding")     # para tener todos el mismo generador de numeros aleatorios
# RNGkind("Super", "Inversion")  #si a alguien le sale error en la anterior

set.seed(123)
net<-train(Precio~., data=trainproc, method="nnet", trControl=control, tuneGrid=grid)


net
plot(net)

RMSEcv<-min(net$results[,3])

#grafico de la red
plotnet(net, pos_col="green", neg_col="blue")

# importancia de las variables

garson(net)

varImp(net)

#predicciones en training set

predtrainproc<-predict(net, newdata=trainproc)
RMSEtr<-RMSE(predtrainproc, trainproc$Precio)


#predicciones en testset

#primero hay que preprocesar el test set

minprec<-min(test$Precio)
maxprec<-max(test$Precio)

testproc<-predict(preProcessRangeModel, test)

predtestproc<-predict(net, newdata=testproc)

RMSEtest<-RMSE(predtestproc, testproc$Precio)


#deshacemos la transformacion de range para obtener predicciones en escala original

predtest<-predtestproc*(maxprec-minprec)+minprec

head(predtest)
head(test$Precio)


# comparamos metrica de ajuste en cv, train y test

RMSEcv
RMSEtr
RMSEtest






# COMPARACION CON REGRESION LINEAL

#entrenamiento de la regresion
RNGkind("Super", "Inversion", "Rounding")     # para tener todos el mismo generador de numeros aleatorios
# RNGkind("Super", "Inversion")  #si a alguien le sale error en la anterior
set.seed(123)
reg<-train(Precio~., data=trainproc, method="lm", trControl=control)

reg


# obtenemos RMSE de cv
RMSEcvREG<-reg$results[,2]


# importancia de las variables


varImp(reg)

#predicciones en training set

predtrainprocREG<-predict(reg, newdata=trainproc)
RMSEtrREG<-RMSE(predtrainprocREG, trainproc$Precio)


#predicciones en testset


predtestprocREG<-predict(reg, newdata=testproc)

RMSEtestREG<-RMSE(predtestprocREG, testproc$Precio)


#deshacemos la transformacion de range para obtener predicciones en escala original

predtestREG<-predtestprocREG*(maxprec-minprec)+minprec

head(predtestREG)
head(test$Precio)


# comparamos metrica de ajuste en cv, train y test

RMSEcvREG
RMSEtrREG
RMSEtestREG
