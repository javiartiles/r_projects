### TRABAJO FINAL TEXT MINING - MACHINE LEARNING III - 4º E3 ANALYTICS

rm(list = ls())
getwd()

library(stringr)
library(dplyr)
library(rvest)
library(lubridate)
library(quanteda)   
library(ggplot2)

install.packages("RColorBrewer")
library("RColorBrewer")
display.brewer.all()


### ------------------------------ LECTURA DE DATOS CON WEB SCRAPING ---------------------------------  ###

# En el fichero WebScraping_Amazon.R se encuentra el código de la función scrape_amazon
# Esta función recibe como primer argumento el código ASIN del producto del que queremos hacer las reviews
# El segundo argumento es el numero de páginas que queremos scrapear
# Para el producto con ASIN B00A0VCJPI vamos a sacar las reviews que aparecen en https://www.amazon.es/product-reviews/B00A0VCJPI/?pageNumber=1

options(encoding="windows-1252")
source("WebScraping_Amazon.R")

# Una vez hemos ejecutado esa función con el source, ya podemos llamar a la función
# Nuestro producto es el "B00A0VCJPI" y vamos a hacer scraping de las 20 primeras páginas

url<-"https://www.amazon.es/product-reviews/B00A0VCJPI/?pageNumber=1"

reviews<-scrape_amazon("B00A0VCJPI", 20) 
copia<-reviews

# Analizamos las reseñas de nuestro producto

str(reviews)
View(reviews)

reviews$titulo
reviews$fecha
reviews$texto[1:4]
reviews$rating


### ---------------------------------  INSPECCIONAMOS UN POCO EL DATA FRAME Y LIMPIAMOS ALGUNAS COSAS DEL TEXTO --------------------------------- ###

View(reviews)
summary(reviews)
anyNA(reviews)

# Ya que tenemos la fecha, vamos a ver como evoluciona el numero de reviews a lo largo del tiempo
# Tener en cuenta que la primera review es del 4 de enero de 2018
# Vamos a calcular el numero de reviews por dia y lo vemos en un grafico

reviews %>%
  dplyr::count(fdate=floor_date(fecha, "month")) %>%
  ggplot(aes(as.Date(fdate), n))+
  geom_bar(stat="identity")+
  scale_x_date(date_labels = "%b-%Y")+
  ggtitle ("Número de reviews por mes")


grep("[A-z]\\.[A-z]",reviews$text) # Existen documentos con estructuras tales como "palabra.palabra"
reviews$texto<-gsub("\\.(?=[A-z])", " ",reviews$texto, perl = TRUE) # Quitamos ese punto y sustituimos por espacio  
reviews$texto<-gsub('[^[:alnum:][:blank:]¿?&/!¡\\.\\-]', "",reviews$texto) # Quitamos los caracteres especiales ademas de emoticonos

### --------------------------------------------------------- CORPUS Y MATRIZ --------------------------------------------------------- ###

micorpus<-corpus(reviews$texto)
summary(micorpus) # Nuestro corpus esta formado por 200 documentos

docvars(micorpus, c("Date", "Rating"))<-reviews[,c(2,4)] # Añadimos las variables Fecha y Rating en nuestro corpus
docvars(micorpus, "posneg")<-ifelse(micorpus$Rating<=3, "NEGATIVO", "POSITIVO") # Decimos si la revisión es positiva (4 estrella o más) o negativa

summary(micorpus, n=200)


midfm<-dfm(micorpus) # Construcción de la matriz document-term
dim(midfm) # Se han detectado 803 terminos
featnames(midfm)  

topfeatures(midfm,40)  

midfm<- dfm(micorpus,
            remove_numbers = TRUE,  
            remove_punct = TRUE,
            remove= c(stopwords("spanish"),"si")) 
  

dim(midfm) # Nueva dimension de 673 terminos
featnames(midfm)
topfeatures(midfm,40)

# Stemming
midfm <- dfm_wordstem(midfm, language = "spa")
dim(midfm)
featnames(midfm)

### -------------------------------------- TABLAS Y GRÁFICOS DE FRECUENCIAS DE PALABRAS -------------------------------------- ###


# Un gráfico de la frecuencia con la que aparece cada término
# Para ello hacemos uso de la función textstat_frequency()

features_freq<-textstat_frequency(midfm, n = 20)  # frecuencia de cada término y el número de documentos en los que aparece
View(features_freq)
ggplot(features_freq, aes(x = reorder(feature,-frequency), y = frequency)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

set.seed(123)
textplot_wordcloud(midfm, min_count=200, min_size=1.5, col=brewer.pal(n = 9, name = "OrRd"))
# Observamos por medio de una nube de palabras aquellas que se repitan mas de 25 veces

# Graficamos la frecuencia de cada término dentro dentro de cada grupo (reviews positivas y negtivas)

freq_posneg <- textstat_frequency(midfm, n = 10, groups = "posneg")

ggplot(data = freq_posneg, aes(x = nrow(freq_posneg):1, y = frequency, fill=group)) +
  geom_bar(stat="identity") +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(freq_posneg):1,
                     labels = freq_posneg$feature) +
  labs(x = NULL, y = "Frecuencia de cada Término")


### ---------------------------- CONTEXTO KEY WORDS ---------------------------- ###

# Tanto en las reviews POSITIVAS como en las NEGATIVAS se habla de la "SEÑAL" y del "WIFI"
# Vamos a ver, dentro de las negativas, lo que se dicen de estas dos palabras

corpus_neg<-corpus_subset(micorpus, posneg=="NEGATIVO")
kw_neg <- kwic(corpus_neg, pattern =  c("señal", "wifi"), window = 7)
head(kw_neg, 50)

### ----------------------------------  BIGRAMAS ---------------------------------- ###

# Bigramas MAS frecuentes

bigrams<-quanteda::tokens(micorpus,remove_punct = TRUE)%>%
  tokens_tolower()%>%
  tokens_remove(stopwords("spanish"))%>%
  tokens_ngrams(n=2) %>% 
  dfm()

topFeatures <- topfeatures(bigrams, 20)
topFeatures

bigram_posneg <- textstat_frequency(bigrams, n = 20, groups = "posneg") # Visualizamo sen un grafico las frecuencias de dichos bigramas

ggplot(data = bigram_posneg, aes(x = nrow(bigram_posneg):1, y = frequency, fill=group)) +
  geom_bar(stat="identity") +
  facet_wrap(~ group, scales = "free") +
  coord_flip() +
  scale_x_continuous(breaks = nrow(bigram_posneg):1,
                     labels = bigram_posneg$feature) +
  labs(x = NULL, y = "Frecuencia de los Bigramas")



### ------------------------------------------------- LEMATIZACIÓN ------------------------------------------------- ###

library(utf8)
install.packages("udpipe")
library(udpipe)
udmodel <- udpipe_download_model(language = "spanish")  
udmodel<-udpipe_load_model(file = udmodel$file_model) 

# Cogemos reviews, en las que manteníamos puntuación, stopwords, etc.
texto<-as_utf8(reviews$texto)
anot_texto<- udpipe_annotate(udmodel, texto,  tagger = "default", parser = "none")
anot_texto <- as.data.frame(anot_texto, detailed = TRUE)
View(anot_texto)

# Hagamos una nube de palabras en las que sólo utilicemos nombres y adjetivos

anot_plot <- anot_texto %>%
  filter(upos %in% c("ADJ")) %>%
  group_by(upos) %>%
  count(lemma, sort=T)

library(ggwordcloud)
ggplot(anot_plot %>% filter(n>25), aes(label = lemma, size = n, color=n)) +
  geom_text_wordcloud(eccentricity = 1) +
  scale_size_area(max_size = 12) +
  theme_minimal() +
  scale_color_gradient(low="#fb6a4a", high="#67000d")

# Algoritmo Google's Textrank 

library(textrank)
rank <- textrank_keywords(anot_texto$lemma, 
                          relevant = anot_texto$upos %in% c("ADJ"), 
                          ngram_max = 8, sep = "_")
rank <- subset(rank$keywords, ngram >= 1 & freq >= 3)
ggplot(rank, aes(label=keyword, size=freq, color=freq)) +
  geom_text_wordcloud(eccentricity = 1) +
  scale_size_area(max_size = 12) +
  theme_minimal() +
  scale_color_gradient(low="#fb6a4a", high="#67000d")
