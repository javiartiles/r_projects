### TRABAJO FINAL CLUSTERING - MACHINE LEARNING III - 4º E3 ANALYTICS

### El dataset a analizar recoge informacion del comportamiento de las tarjetas de creditos de determinados clientes durante seis meses

rm(list = ls())

library(zoo)
library(readr)
library(dplyr)
library(NbClust)

dataset <- read_csv("dataset.csv")
View(dataset)

### --------------------------------  EXPLORACION Y LIMPIEZA -------------------------------- ###
summary(dataset)
head(dataset)
dataset <- select(dataset,-CUST_ID) # En primer lugar eliminamos la primera variable debido a que no nos aporta informacion
str(dataset)
summary(dataset) # Debido a la existencia de valores nulos (NA's) vamos a eliminarlos para que no nos den problemas
dataset <- dataset[!is.na(dataset$MINIMUM_PAYMENTS),]
dataset <- dataset[!is.na(dataset$CREDIT_LIMIT),]
summary(dataset)

# Como nos encontramos con un dataset de 17 variables, cabe la posibilidad de que no consigamos con exactitud nuestro objetivo
# Para solucionar esto vamos a quedarnos con las variables que mas pese, por lo que veremos la correlacón que tienen

sapply(dataset, var)
pairs(dataset)
cor(dataset)
var(dataset)

# Podemos observar como las variables con mas peso y correlacion son:

dataset <- select(dataset, -4, -6, -8, -9, -10, -11, -16)
View(dataset)
summary(dataset)

# Paquetes necesarios

library(dendextend)
library(ggplot2)  
library(factoextra)   
library(NbClust) 

### -------------------------------- CLUSTERING JERARQUICO (TANTEO) -------------------------------- ###

# Estandarizamos las variables (media = 0 y varianza = 1), las cuales son todas numericas

datasetnorm <- scale(dataset) 

# Debido a que todas las variables son numericas, optamos por hacer uso de la distancia euclidea y asi crear la matriz de distancias
# Ademas, visualizamos la misma con la funcion "fviz_dist()"

matrizdist<- dist(datasetnorm, method = "euclidean")
fviz_dist(matrizdist)

# Probamos con un modelo jerarquico
hc_complete<-hclust(matrizdist, method= "single")
plot(hc_complete, hang=-1, main="dendograma con criterio de enlace simple")

hc_complete<-hclust(matrizdist, method= "average")
plot(hc_complete, hang=-1, main="dendograma con criterio de enlace average")

hc_complete<-hclust(matrizdist, method= "complete")
plot(hc_complete, hang=-1, main="dendograma con criterio de enlace completo")


# Visualizamos el Heatmap

library(gplots)
heatmap.2(x = datasetnorm, scale = "none",
          distfun = function(x){dist(x, method = "euclidean")},
          hclustfun = function(x){hclust(x, method = "ward.D2")},
          density.info = "none",
          trace = "none",
          col = bluered(200),
          cexCol=0.8)

### -------------------------------- DETERMINAR NUMERO OPTIMO DE CLUSTERS -------------------------------- ###

fviz_nbclust(datasetnorm, kmeans, method = "wss")+
  labs(subtitle = "Elbow method")

# wss = Metodo Elbow

# Metodo silhouette = saber de manera mas exacta cuantos clusters usar
fviz_nbclust(datasetnorm, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# A pesar de que por medio de este metodo nos recomienden 2 clusters haremos uso de 3 y 4

# Otra alternativa mas avanzada: usar el paquete NbClust

nb <- NbClust(datasetnorm, distance="euclidean", min.nc = 2,
              max.nc = 9, method = "kmeans")

### -------------------------------- K-MEANS -------------------------------- ###

# Nos aconseja hacer 2 clusters. Pero como dijimos anteriormente, aplicamos K-means con 3 y 4 clusters y hacemos 40 asignaciones aleatorias

set.seed(123)
km <- kmeans(datasetnorm, centers = 3, nstart=40)

# Tamaño de los clusters
km$size
prop<-km$size/nrow(datasetnorm)
prop

# Visualizamos los clusters usando PCA
fviz_cluster(km,data=datasetnorm, geom="point", show.clust.cent=TRUE)
#Tenemos que tener en cuenta que esto no nos da la imagen mas fiel ya que faltarian mas datos

# Probamos tambien con 4 clusters, ya que el anterior no deja muy claro si es el optimo o no

km1 <- kmeans(datasetnorm, centers = 4, nstart=40)
km1$size
prop<-km1$size/nrow(datasetnorm)
prop 
fviz_cluster(km1,data=datasetnorm, geom="point", show.clust.cent=TRUE)

# En base a la prueba de usar 3 o 4 clusters, concluimos que es mejor dividir los datos en 4

# ---------------------------------  Interpretacion/validacion de los clusters ------------------------------------------ #

# Hacemos resumen estadistico por cluster. Lo hacemos sobre una copia de los datos originales (tras haber quitado los NA)

dataset1<-dataset

dataset1$cluster<-km1$cluster

head(dataset1)

resumen1<-dataset1 %>% group_by(cluster)%>% summarise_all(mean) 
resumen1$prop<-km1$size/nrow(dataset1)
print(resumen1, width=Inf)

# Veamos mejor el profile plot de los centroides
# Lo hacemos con dplyr
require(dplyr)
require(reshape)
centprofile<-melt(km1$centers) # Centers = centroides (media del cluster en forma de vector)
ggplot(centprofile, aes(x=X2, y=value, group=X1, colour=as.factor(X1)) )+
  geom_line()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(colour = "Cluster")
# Dibujamos los centroides

# Utilicemos el silhouette width para validar los clusters
set.seed(123)
km.eclust <- eclust(datasetnorm, "kmeans", k = 4, nstart = 40, graph=FALSE)  

fviz_silhouette(km.eclust, palette = "jco", ggtheme = theme_classic())

# Hay algunas observaciones mal asignadas  

# En este siguiente paso veremos donde asignar aquellas observaciones mal colcoadas

sil <- km.eclust$silinfo$widths #Silouethe de cada punto
head(sil) # para cada observacion tenemos su sil_width, el cluster al que pertenece y el cluster m?s cercano
s<-sil[sil$sil_width<0, ] 
s

# cuales son las variables con un silhouette negativo
dataset1[row.names(s),]

# Con esta info podriamos hacer re-asignaciones
