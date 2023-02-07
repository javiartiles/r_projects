#PROYECTO FINAL MACHINE LEARNING I | ÁLVARO PORTO DE TOMÁS Y JAVIER ARTILES MANRIQUE DE LARA

# ---------- USO DE LAS NUEVAS TECNOLOGÍAS ----------

# El primer paso es la descarga y apertura de los paquetes necesarios para nuestra predicción.

library (lattice)
library(ggplot2)
library(caret)

install.packages("corrplot")
library(corrplot)
library(class)
library(caret)

install.packages("NeuralNetTools")
library(NeuralNetTools)
library(nnet)
library(caret)
library(e1071)

#Deberiamos ver si hay algúna variable con observación Na para eliminarla o modificarla, pero debido a que hemos creado el DataSet por medio de una encusta hemos tenido la suerte de encontranos con que no hay ningun valor Na. 

dataframe <- NUEVAS_TECNOLOGI_AS_respuestas_

#CONVERTIMOS EN FACTOR NUESTRA VARIABLE TARGET (Y) = HORAS

dataframe$HORAS<-factor(dataframe$HORAS)

#ELIMINAMOS NUESTRA VARIABLE 'ID' DEBIDO A QUE NO NOS DA NINGUNA INFORMACIÓN RELEVANTE

dataframe<-dataframe[,-1]

summary(dataframe)


#MATRIZ DE CORRELACIÓN

round(cor(dataframe), 1)

corrplot(corr = cor(x = dataframe, method = "pearson"), method = "number")


RNGkind("Super", "Inversion",)
set.seed(1245)

#PARTICIÓN TRAIN/SET

trainDatos <- sample(1:nrow(dataframe), nrow(dataframe)*0.7, replace=FALSE)

train<-dataframe[trainDatos,]
test<-dataframe[-trainDatos,]

summary(train)
summary(test)

modelo_logit <- glm(HORAS ~ EDAD + CONVIVENCIA + CODIGO_POSTAL + SITUACION_LABORAL + INGRESOS + SMARTPHONE + PRENSA + 
                      COMPRAS + SERIES_PELICULAS + TRANSPORTE + COMIDA, train, family = binomial (logit))

summary(modelo_logit)


datosreales <- test$HORAS

head(datosreales)

prediccion <- ifelse(predict(modelo_logit, newdata = test, type = "response") >= 0.5, 1, 0)

head(prediccion)

matriz_confusion <- table(datosreales, prediccion)

accuracy_modelo <- sum(diag(matriz_confusion)) / sum(matriz_confusion) *100

matriz_confusion
print(paste(" AUC:", accuracy_modelo))

#Analizada la predicción, analizamos los errores

errores<-test$HORAS-prediccion
h<-nrow(test)
ME<-sum(errores)/h
RMSE<-sqrt(sum(errores^2)/h)
MAE<-sum(abs(errores))/h
#MAPE<-sum(abs(errores)/test$HORAS)/h*100

"ME";ME;"RMSE";RMSE;"MAE";MAE;"MAPE";MAPE

##ALGORITMO KNN

modelobasicoknn<-train(data=train, method="knn", HORAS~.) 
modelobasicoknn                                       
#coincidencia por azar k=[p(a)-p(e)]/(1-p(e))

# "K" = valor del hiperparámetro a considerar
repeats = 3   #veces que se repite el procedimiento de remuestro
numbers = 10  #numero de folds en la cross-validation
tunel = 30      #número de valores del hiperparámetro que se van a probar

RNGkind("Super", "Inversion", "Rounding")
set.seed(4321)

train$HORAS = ifelse(train$HORAS==1,"Si","No")
train$HORAS<-as.factor(train$HORAS)
x = trainControl(method = "repeatedcv",		# método de remuestreo, en este caso cv repetida; en este caso se va a hacer cross validacion de 10 folds y se va a hacer 3 veces
                 number = numbers,				# número de folds
                 repeats = repeats,				# número de repeticiones de la cross validación
                 classProbs = TRUE,				# se obtienen las probabilidades de clasificacion
                 summaryFunction = twoClassSummary) #el resumen es el adecuado para dos clases

#ESTIMACIÓN (TRAIN)
modeloknn <- train(HORAS ~ EDAD + CONVIVENCIA + CODIGO_POSTAL + SITUACION_LABORAL + INGRESOS + SMARTPHONE + PRENSA + 
                     COMPRAS + SERIES_PELICULAS + TRANSPORTE + COMIDA, data = train, 
                method = "knn",					              #algoritmo a usar
                preProcess = c("center","scale"),			#transformaciones de los datos, aqu� se tipifican, es necesario para poder usar bien knn, sobre todo si hay features en escalas muy diversas
                trControl = x,				              	#c�mo se realiza el proceso de training
                metric = "ROC",				            	  #m�trica usada para seleccionar el mejor modelo
                tuneLength = tunel)				          	#cu�ntos valores de K se prueban	          	


modeloknn                                               

plot(modeloknn)    # Valores del Hiperparámetro "K" y su AUC

#Predicciones en el test set

test_class <- predict(modeloknn, newdata=test)             #prediccion clasificada
test_pred <- predict(modeloknn, newdata=test, type="prob") #prediccion probabilidades numericas

datosreales1<- test$HORAS
prediccion1<- predict(modeloknn, newdata=test)

#MATRIZ DE CONFUSIÓN

matriz_confusion1 <- table(datosreales1, prediccion1)
matriz_confusion1
accuracy_modelo1 <- sum(diag(matriz_confusion1)) / sum(matriz_confusion1) *100
print(paste("AUC:", accuracy_modelo1))

#AREA POR DEAJO DE LA CURVA ROC
library(gplots)

#Storing Model Performance Scores
library(ROCR)
pred_test <-prediction(test_pred[,1],test$HORAS)

#AUC

perf_test <- performance(pred_test,"auc")
auc=paste("AUC=",round(perf_test@y.values[[1]],3))
auc

#AUC (GRAFICA)
perf_test <- performance(pred_test, "tpr", "fpr")
plot(perf_test, col = "green", lwd = 1.5)
text(auc, x=.1, y=.8)


#ALGORTIMO ESEMBLE

####ENSEMBLES DE MODELOS

install.packages("caret")
install.packages("caretEnsemble")
install.packages("tictoc")


library(caret)
library(caretEnsemble)
library(tictoc) #para medir tiempo

#STACKING CREANDO MODELO SUPERVISOR DE SEGUNDO NIVEL

tic()

control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE, search="random")
algorithmList <- c('rpart', "rf",'glm', 'knn', 'svmRadial', 'nnet')

RNGkind("Super", "Inversion", "Rounding")
#RNGkind("Super", "Inversion")
set.seed(123)

models <- caretList(HORAS~., data=train, trControl=control, methodList=algorithmList, tuneLength=10)
results <- resamples(models)
summary(results)
dotplot(results)

toc()

# CORRELACIÓN
modelCor(results)
splom(results)


# STACK (glm)
tic()
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE, search="random")


RNGkind("Super", "Inversion", "Rounding")
#RNGkind("Super", "Inversion")
set.seed(123)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
toc()

# STACK (random forest)
tic()
set.seed(123)
stack.rf <- caretStack(models, method="rf", metric="ROC", trControl=stackControl,tuneLength=5)
print(stack.rf)
toc()


#PREDICCIÓN
pred.glm<-predict(stack.glm, newdata=test)
confusionMatrix(pred.glm, test$Outcome, positive="yes")


pred.rf<-predict(stack.rf, newdata=test)

confusionMatrix(pred.rf, test$Outcome, positive = "yes")
