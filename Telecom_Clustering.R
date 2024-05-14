if (!require('readxl')) install.packages('readxl')
if (!require('psych')) install.packages('psych')
if (!require('cluster')) install.packages('cluster')
if (!require('fpc')) install.packages('fpc')
if (!require('vcd')) install.packages('vcd')

#autor: Yesin Camarena
######k-means###################

# Lectura de los datos que deben estar en la ruta que genera el siguiente código
getwd()
library(readxl)
cell<-read_excel("cellsegmentation.xlsx")
str(cell)
#####################################################
### 2.analisis descriptivo de las variables escalares
#####################################################
library(psych)
describe(cell[,6:10])
# para graficar varios plot en una misma ventana
par(mfrow=c(2,3))
hist(cell$minutos_preferido)
hist(cell$adicionales)
hist(cell$equipo_dolares)
hist(cell$minutos_no_preferido)
hist(cell$internet_gigas)
# parar el attach de las graficas
dev.off()

#################################
### 3.Preparación de los datos
##################################

#variable de servicios adicionales
cell$servicios<-cell$fijo+cell$largadistancia+cell$internetcasa+cell$numoculto
#reducción de at??picos
##obtener las variables a reducir
cell1<-cell[,6:7]
cell2<-cell[,9:10]
cell3<-as.data.frame(cbind(cell1,cell2))
#obtener el logaritmo de 1+x #el log0 es indefinido, y cuando x sea 0 no quede indefinido
cell4<-apply(cell3,2,log1p)
#ensamble final
cellfin<-as.data.frame(cbind(cell4,cell$servicios, cell$equipo_dolares))
colnames(cellfin)[5:6]<-c("servicios","equipo_dolares")
colnames(cellfin)
#estandarizar
cellfins<-as.data.frame(scale(cellfin))
#revisar descriptivos y ya no tengo datos atipicos
describe(cellfins)
par(mfrow=c(1,3))
hist(cellfins$minutos_preferido)
hist(cellfins$adicionales)
hist(cellfins$minutos_no_preferido)

dev.off()

##############################################################
### 4. Escoger el número de clusters y por desicion de negocio
##############################################################

#utilizo una semilla para replicar resultados
set.seed(5935)
#calculo la suma de cuadrados total es para calcular la varianza
wss <- (nrow(cellfins)-1)*sum(apply(cellfins,2,var))
#calculo para cada solución de clustering 
for (i in 2:15) wss[i] <- sum(kmeans(cellfins,
                                     centers=i, nstart=10)$withinss)#la suma de cuadrados dentro de los grupos
#10 veces cada una, es decir 140 veces kmeas
plot(1:15, wss, type="b", xlab="Número de Clusters",
     ylab="Suma de cuadrados within") 
# mientras mas grupos creo mas complejidad se tiene
#regularización: penalizacion por la cantidfad de grupos que quiero penalizo visualmente
#es decir en el momento la información disminuye
#es decir entre 4 o 5 una primera aproximacion me dice entre 4 o 5.

# otras formas de ver el numero de clusters
#asw
set.seed(2)
clustering.asw <- kmeansruns(cellfins,krange=2:15,criterion = "asw", nstar=10)
clustering.asw$bestk



#evaluar con gap statistic
#mira el minimo k tal que el gap sea mayor que el gap de k+1
gscar <- clusGap(cellfins,FUNcluster = kmeans,K.max = 10)
# Plot the results (optional)
plot(gscar)


getwd()
### Ejecución
#ejecución de k-means
cellcluster<-kmeans(cellfins,centers=5,nstart=10,iter.max=20)
#tamaño de grupos
cellcluster$size
#numero de iteraciones
cellcluster$iter
#centros de grupos
cellcluster$centers


#guardar el cluster de pertenencia
cell$grupo<-cellcluster$cluster
library("vcd")
mosaic(~grupo + jubilado ,data=cell, 
       legend=TRUE, shade=TRUE)
mosaic(~grupo + genero ,data=cell, 
       legend=TRUE, shade=TRUE)
boxplot(log(ingreso_miles)~grupo, data=cell)
boxplot(Edad~grupo, data=cell)

clusplot(cellfins,cellcluster$cluster, color=TRUE)



#validar resultados- consistencia
kclusters <- clusterboot(cellfins,B=10,clustermethod=kmeansCBI,k=5,seed=5)
#la validacion del resultado. >0.75 o .85 muy bueno; <.6 malo
kclusters$bootmean
str(cell)
nueva<-cell%>%group_by(grupo)%>%summarise(minutos=mean(adicionales))




source("D:/Mineria de datos/Proyecto 8 Clusterin/funcion.R")
# Example: Assuming 4 clusters based on previous selection
kmeans_fit <- kmeans(cellfins, centers = 4, nstart = 10)
cluster_labels <- kmeans_fit$cluster
# Calculate the distance matrix using Euclidean distance
dist_matrix <- dist(cellfins)


# Aplicar la función para calcular las distancias promedio
dist_promedio <- calcular_distancias_promedio(cellfins, cluster_labels)

# Generar el mapa de calor
library(pheatmap)
pheatmap(dist_promedio, main = "Average Distances Between Clusters", show_rownames = FALSE, show_colnames = FALSE)

