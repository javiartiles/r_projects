##### SVM Support Vector Machines

library(caret)


# cargamos datos desde el directorio de trabajo de cada uno
library(readxl)
datos<- read_excel("hombre o mujer.xlsx")

#grafico

datos$sexo<-factor(datos$sexo, levels=c(1,0), labels=c("Hombre", "Mujer"))
datos$E6B<-factor(datos$E6B, levels=c(1,0), labels=c("E6B", "E3An"))

ggplot()+aes(x=datos$moda, y=datos$futbol, col=datos$sexo)+geom_point()+geom_jitter()




#particion 70/30

RNGkind("Super", "Inversion","Rounding")
#RNGkind("Super", "Inversion")
set.seed(4)

index<-createDataPartition(datos$sexo, p=0.7, list=FALSE)
train<-datos[index,]
test<-datos[-index,]


#parametros de cross validacion

control<-trainControl(method="repeatedcv", number=10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)

#hiperparametros a optimizar con svm radial: sigma y C

modelLookup("svmRadial")
modelLookup("svmLinear")
modelLookup("svmPoly")

# fijamos el grid

grid<-expand.grid(sigma=c(0.1,0.2,0.4,0.6,0.8), C=c(0.25,0.5,1,2,4,8,16,32,64,128))

#entrenamiento del SVM

RNGkind("Super", "Inversion","Rounding")
#RNGkind("Super", "Inversion")
set.seed(123)

svm1<-train(sexo~., data=train, method="svmRadial", trControl=control, metric="ROC", tuneGrid=grid)

svm2<-train(sexo~., data=train, method="svmLinear", trControl=control, metric="ROC", tuneLength=10)

svm3<-train(sexo~., data=train, method="svmPoly", trControl=control, metric="ROC")

svm1

plot(svm1)

svm2

svm3

#predicciones

predsvm1<-predict(svm1, newdata=test, type="raw")

confusionMatrix(predsvm1,test$sexo)


predsvm2<-predict(svm2, newdata=test, type="raw")

confusionMatrix(predsvm2,test$sexo)


predsvm3<-predict(svm3, newdata=test, type="raw")

confusionMatrix(predsvm3,test$sexo)

# COMPARACION CON LOGIT

#entrenamiento del logit

RNGkind("Super", "Inversion","Rounding")
#RNGkind("Super", "Inversion")
set.seed(123)

logit<-train(sexo~., data=train, method="glm", trControl=control, metric="ROC")

logit

summary(logit)

predlogit<-predict(logit, newdata=test, type="raw")

confusionMatrix(predlogit, test$sexo)




# COMPARACION CON knn

#entrenamiento del knn

RNGkind("Super", "Inversion","Rounding")
#RNGkind("Super", "Inversion")
set.seed(123)

knn<-train(sexo~., data=train, method="glm", trControl=control, metric="ROC", tuneLength=10)

knn

predknn<-predict(knn, newdata=test, type="raw")

confusionMatrix(predknn, test$sexo)