##SEteamos información
setwd("~/Ministerio de competitividad/curso")
##cargamos librerias
library("openxlsx")
library(dplyr)
library(openxlsx)
library(plotly)
###leCTURA DE DATA
dir()
data<-read.xlsx("bank-additional-full.xlsx",sheet = 1)
# Descripción de variables
# Age: Edad del cliente (Numérica)
# Job: Ocupación del cliente (Categórica)
# Marital: Estado civil (Categórica)
# Education: Nivel de eduación del cliente (Categórica) 
# Default: Indicador de si un cliente tiene un crédito por defecto (categórica)
# Housing: Indicador de si un cliente tiene un crédito hipotecario (Categórica)
# Loan: Indicador de si un cliente tiene un crédito personal(Categórica)
# Contact: Medio por el que se le contacta a un cliente (Categórica)
# Month: Mes del último contacto (Categórica)
# Day_of_week: Día de la semana del último contacto (Categórica)
# Duration: Duración del último contacto en segundos (Numérica) [Tiene alto impacto sobre la variable de decisión]
# Campaign: Número de contactos hechos durante la campaña para este cliente (Numérica)
# Pdays: Número de días desde que se contacto por última vez en la campaña anterior (Numérica)
# Previous: Número de contactos hechas antes de esta campaña (Numérica)
# Poutcome: Resultado de de la campaña de marketing previa (Categórica)
# Empvarrate: Variación de la tasa de empleo [Indicador de cuartiles] (Numérica)
# Conspriceidx: Indice de precios al consumidor [Indicador mensual] (Numérica) (3616 valores perdidos)
# Consconfidx: Indice de confianza del consumidor [Indicador mensual] (Numérica)
# Euribor3m: Tasa de interés bancos europeos [Indicador diario] (Numérica) (8041 valores perdidos)
# Nremployed: Número de empleados[Indicador cuartiles] (Numérica) (33425 valores perdidos)
#####Variables desconocido 
def_desconocida<-sum(as.numeric(data$default=="unknown"))/nrow(data)*100
# Convertimos los datos desconocidos en "yes"
data$default[which(data$default=="unknown")]<-"yes"

# Transformación de la naturaleza de las variables
data$cons.price.idx<-as.numeric(data$cons.price.idx)
data$euribor3m<-as.numeric(data$euribor3m)
data$nr.employed<-as.numeric(data$nr.employed)
data$emp.var.rate<-as.numeric(data$emp.var.rate)
data$cons.conf.idx<-as.numeric(data$cons.conf.idx)
data$cons.price.idx[which(data$cons.price.idx==min(data$cons.price.idx))]<-min(data$cons.price.idx)*100
#####Valores atípicos
plot_ly(ggplot2::diamonds, y = data$age,name = "Edad", type = "box",line = list(color = 'rgba(113, 108, 235, 0.7)'))
plot_ly(ggplot2::diamonds, y = data$cons.conf.idx,name = "Cons.conf", type = "box",line = list(color = 'rgba(113, 108, 235, 0.7)'))
q<-quantile(data$age,0.75)-quantile(data$age,0.25)
data$age[which(data$age>quantile(data$age,0.75)+1.5*q)]<-median(data$age)
q<-quantile(data$cons.conf.idx,0.75)-quantile(data$cons.conf.idx,0.25)
data$cons.conf.idx[which(data$cons.conf.idx>quantile(data$cons.conf.idx,0.75)+1.5*q)]<-median(data$cons.conf.idx)
####Concatenar
data$fecha<-do.call(paste, c(, sep = "/"))
data$fecha<-as.Date(data$fecha,format = '%m/%d/%Y')
#####Ejemplo Valores perdidos
na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
na_count

dir()
###Leyendo la data para valores perdidos
data<-read.csv("adult.csv",sep = ",")
###IDENTIFICAMOS LOS NA
is.na(data) <- data=='?'
is.na(data) <- data==' ?'
#######UNA TABLA DE LOS NA EXISTENTES
na_count <- sapply(data, function(y) sum(length(which(is.na(y)))))
na_count
###vISUALIZACIÓN DE DATOS PERDIDOS
library(VIM)
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, 
                  sortVars=TRUE, labels=names(data), 
                  cex.axis=.7, gap=3,
                  ylab=c("Histograma de datos perdidos ","Patrones"))
####cAMBIAMOS POR LA MEDIA
m <- floor(mean(na.omit(data$age)))
data$age[which(is.na(data$age))]<-m
###Metodos del paquete mice
impu.data<-mice(data,m=5,maxit=50,meth='pmm')
write.csv(Base,"Finiquito_Contractuales.csv")



#######Nube de palabras
####Instalando y cargando librerias necesarias
install.packages('nutshell')
install.packages('tm')
install.packages('SnowballC')
install.packages('slam')
install.packages('NLP')
install.packages('Matrix')
install.packages('wordcloud')
install.packages('RColorBrewer')
install.packages('ggplot2')
install.packages('XLConnect')
install.packages('XLConnectJars')
install.packages('WriteXLS')
install.packages('readxl')
library(WriteXLS)

library(XLConnectJars)
library(XLConnect)
library(NLP)
library(tm) # para Text Mining
library(SnowballC) # para Stemming
library(slam) # estructuras y algoritmos para arrays y matrices
library(Matrix) # para trabajar con matrice
library(readxl)
library(RColorBrewer)
library(wordcloud)
library(ggplot2)
library(nutshell)
dir()
pob1<-read.csv("final.csv")
View(pob1)
View(calificacion)
names(pob1)
str(pob1)
attributes(pob1)
summary(pob1)
#####Escoger solo los que tienen Otros de todas las calificaciones
#########Nivel 1
txtdataset1<- subset(pob1, pob1[[37]]=="Otros")
txtdataset2<- subset(pob1, pob1[[39]]=="Otros")
txtdataset3<- subset(pob1, pob1[[41]]=="Otros")

#############
#txtdataset2#
#############

########Creando un corpus esta es la que uso 
micorpus1 <- Corpus(VectorSource(txtdataset2$Experiencia),
                    readerControl = list(reader = readPlain,
                                         language = "es",
                                         load = TRUE))

################################

######Eliminando palabras extras
micorpus1 <- tm_map(micorpus1, removeNumbers)
micorpus1 <- tm_map(micorpus1,PlainTextDocument)
micorpus1 <- tm_map(micorpus1, removePunctuation)
#summary(micorpus)
micorpus1 <- tm_map(micorpus1, tolower)
micorpus1 <- tm_map(micorpus1, removeWords, stopwords("spanish"))
micorpus1 <- tm_map(micorpus1, stemDocument, language="spanish")
micorpus1 <- tm_map(micorpus1, stripWhitespace)
####Creando la Matriz
tdm1 <- TermDocumentMatrix(micorpus1,
                           control = list(removePunctuation = TRUE,
                                          stopwords = TRUE))
#####Creando Matriz
m1<-as.matrix(tdm1)
v1<-sort(rowSums(m1),decreasing = TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
View(d1)
######Construyendo la Nube
wordcloud(d1$word,d1$freq)
###Exportando
write.table(d1,file="Ex2.cvs",eol="\n",row.names = FALSE,col.names = TRUE)
###############################

