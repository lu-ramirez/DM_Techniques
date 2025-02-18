#Clustering jer�rquico

#Importamos nuestra base de datos de la encuesta y la etiquetamos como "myData" 
setwd("C:/Users/luzy3/OneDrive/IM�GENES/IMAGENES/Documents/INVESTIGACI�N/ENCUENTRO GARZA/Hierarchical clustering")

myData <- read.csv(file="data.csv", header= TRUE)
Surveyed <- myData$ENCUESTADO

#Instalamos y cargamos la librer�a "cluster", "ggplot2", "factoextra" y "openxlsx".
library (cluster)
library(ggplot2)
library (factoextra)
library(openxlsx)

#La primera variable del conjunto de datos contiene los n�meros de identificaci�n de los encuestados y no se utilizar� en el an�lisis de agrupaciones. 
#Usamos la funci�n de "daisy" para determinar la similitud entre las observaciones y etiquetar los resultados como d. 
#Para las opciones dentro de la funci�n "daisy", usamos m�trica para especificar el c�lculo de la distancia. 
#Las opciones para el c�lculo de la distancia incluyen "euclidiana", "manhattan" y "gower". Especificamos "gower"
d <- daisy(myData[,2:29], metric="gower")
d

#Usamos la funci�n agnes con el m�todo de Ward para realizar agrupaciones aglomerativas y etiquetar los resultados como mResult.
mResult <- agnes(d, method="ward")
mResult

#R informa un coeficiente de aglomeraci�n de 0.94

#Usamos la funci�n cutree para obtener los cl�steres y luego agregamos la informaci�n de pertenencia al cl�ster al marco de datos myData. 
mClusters <- cutree(mResult, k=2)
myData <- data.frame(myData, mClusters)

#Usamos la funci�n plot para obtener el gr�fico del dendrograma.
plot(mResult, which.plots =2) 
fviz_dend(mResult, cex = 0, k = 2, main="Dendograma aglomeramiento jer�rquico",
          color_labels_by_k = FALSE, k_colors=c("#2A2640","#17A67D"))


#Usamos la funci�n de resumen para obtener estad�sticas de resumen para cada grupo.
summary(subset(myData, mClusters==1))
summary(subset(myData, mClusters==2))
#summary(subset(myData, mClusters==3))


#Para averiguar el n�mero de observaciones en cada grupo, usamos la funci�n as.factor para convertir mCluster en datos categ�ricos 
#y luego usamos la funci�n de resumen para averiguar el n�mero de observaciones en cada grupo. 
summary(as.factor(mClusters))

#Usamos la funci�n cbind para conocer qu� individuos pertenecen a cada cluster.
Cluster <- cbind(Surveyed, mClusters)

#Generamos un data frame con la tabla de datos. 
Cluster_f <- data.frame(Cluster)
Cluster_f

#Exportamos el data frame a un archivo xlsx.
write.xlsx(Cluster_f,"Cluster_f.xlsx", asTable = TRUE) 
