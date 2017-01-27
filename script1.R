setwd("C://Users//Ana Julia//Documents//Ministerio de competitividad//curso")
getwd() 
dir()
#Ayuda sobre funci?n o capacidad
help(solve)
help(mean)
?solve
help("[[") 
help.start()
help(rnorm)
####Asiganar
a<-2
set.seed(12456)
b<-rnorm(5,25,2)
a
b
##Creando vectores
a<-c(1,5,6,8,4);a
b<-runif(n = 5,min = 0,max = 100);b
b1<-round(b,4);b
##vector caracter
c<-c("Quito","Ambato","Guayaquil","Cuenca");c
d<-seq(1,100,0.5);d
e<-rep("No Informa",10);e
###Extraer una o m?s componentes del vector
x = c(18,11,12,10,7,6,17)
x[c(1,3,6)]
x[-3]
x[-c(1,2)]
#Especificar una condici?n l?gica. En el caso del vector x creado arriba:
x>10
x[x>10]
###Uso de variables de tipo caracter
nim1<-numeric(456)
s <-"Hola mundo";s
num<-165465
num<-as.character(num)
class(num)
##Retorna la clase de s
class(s)
###Tama?o de s
length(s)
##N?mero de caracteres que tiene s
nchar(s)
#####Seleciona desde la posici?n 5 a la 10
substr(s, 5, 10)
substring(s, 1, 5)
substr(s, 5, rep(8, 3))
substring(s, 5, rep(8, 3))
substr(s, 5, 8)<-"-";s

a<-"Ecuador"
b<-"Quito"
#Une las dos variables
paste(a,b,collapse="-")
c
##b?squeda de caracteres iguales
##Exactamente igual
match("Cuenca",c)
##parcialmente
pmatch("Cue",c)
####Funci?n grep
texto <- c("Handel", "Haendel","H?andel","Handemore","Mendel","Handle")
patron <- "H[a|?a](e)?ndel"
musicos <- grep(patron, texto)
texto[musicos]
## Factores
estudiantes.origen<-c("getafe","mostoles","madrid","madrid","mostoles", 
                      "leganes","getafe","leganes","madrid","mostoles",
                      "parla","alcorcon","mostoles","getafe","leganes")
festudiantes<-as.factor(estudiantes.origen) 
levels(festudiantes) 
summary(festudiantes) 
estudiantes.estaturas <- c(1.83, 1.71, 1.79, 1.64, 1.74,
                           1.81, 1.62, 1.84, 1.68, 1.81, 
                           1.82, 1.74, 1.84, 1.61, 1.84) 
tapply(estudiantes.estaturas,festudiantes,mean) 

t(matrix(1:6))
matrix(1:6,nrow=2) 
matrix(1:6,nrow=2,byrow=T) 

data<-data.frame(iris)
View(data)
summary(iris)
str(iris)
data[1,]
data[2, 5]
data$Sepal.Length
data$media<-mean(data$Sepal.Length)
x <- cbind(iris[1:5,], nuevaCol=1:5)
boxplot(iris$Sepal.Length)
head(iris,10)
tabla<-table(iris$Species)

f<- function(v){
v <- v[!is.na(v)]
p <- sum(v) / length(v)
p
}
x<-c(5,12,35,489,132,NA,132,12,NA)
f(x)
#####Ejemplo
attributes(iris)
summary(iris)
names(iris)
table(iris$Species)
setosa<-subset(iris,iris$Species=="setosa")
largo<-iris[which(iris$Sepal.Length==min(iris$Sepal.Length)),]
asd<-iris[1:5,]
asd
asd[-1,]
apply(iris[,1:4],2,mean)
apply(iris[,1:4],2,sum)
by(iris$Sepal.Length,iris$Species,summary)

