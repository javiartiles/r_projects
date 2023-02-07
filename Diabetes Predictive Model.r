####ENSEMBLES DE MODELOS
#cargamos paquetes
install.packages("caret")
install.packages("caretEnsemble")
install.packages("tictoc")  #para contar tiempo entre tic() y toc()

# cargamos librerias
library(caret)
library(caretEnsemble)
library(tictoc) #para medir tiempos

# Cargar los datos desde el directorio de trabajo
library(readr)
diabetes <- read_csv("diabetes.csv")
head(diabetes)
summary(diabetes)


# Transformamos el target (Y) (Outcome) en factor y damos nombres validos a los niveles
diabetes$Outcome<-factor(diabetes$Outcome, levels=c(1,0), labels=c("yes", "no"))



#Partitioning the data into training and test data
#la funci�n createDataPartition respecto a sample 
#asegura que hay un % de "1" y "0" igual en el training set y en el test set
RNGkind("Super", "Inversion", "Rounding")
#RNGkind("Super", "Inversion")
set.seed(3)

index = createDataPartition(diabetes$Outcome, p = 0.75, list = F ) 
train = diabetes[index,]
test = diabetes[-index,]


###### STACKING CREANDO MODELO SUPERVISOR DE SEGUNDO NIVEL

# Example of Stacking algorithms
# create submodels
tic()

control <- trainControl(method="repeatedcv", number=10, repeats=3, classProbs=TRUE, search="random")
algorithmList <- c('rpart', "rf",'glm', 'knn', 'svmRadial', 'nnet')

RNGkind("Super", "Inversion", "Rounding")
#RNGkind("Super", "Inversion")
set.seed(123)

models <- caretList(Outcome~., data=train, trControl=control, methodList=algorithmList, tuneLength=10)
results <- resamples(models)
summary(results)
dotplot(results)

toc()



# correlation between results
modelCor(results)
splom(results)





# stack using glm
tic()
stackControl <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE, search="random")

RNGkind("Super", "Inversion", "Rounding")
#RNGkind("Super", "Inversion")
set.seed(123)
stack.glm <- caretStack(models, method="glm", metric="Accuracy", trControl=stackControl)
print(stack.glm)
toc()



# stack using random forest
tic()
set.seed(123)
stack.rf <- caretStack(models, method="rf", metric="ROC", trControl=stackControl,tuneLength=5)
print(stack.rf)
toc()




#prediccion
pred.glm<-predict(stack.glm, newdata=test)
confusionMatrix(pred.glm, test$Outcome, positive="yes")


pred.rf<-predict(stack.rf, newdata=test)

confusionMatrix(pred.rf, test$Outcome, positive = "yes")