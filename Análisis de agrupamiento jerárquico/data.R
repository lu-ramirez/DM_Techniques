#Clustering jerárquico

#Importamos nuestra base de datos de la encuesta y la etiquetamos como "myData" 
setwd("C:/Users/luzy3/OneDrive/IMÁGENES/IMAGENES/Documents/INVESTIGACIÓN/ENCUENTRO GARZA/Hierarchical clustering")

myData <- read.csv(file="data.csv", header= TRUE)
Surveyed <- myData$ENCUESTADO

#Instalamos y cargamos la librería "cluster", "ggplot2", "factoextra" y "openxlsx".
library (cluster)
library(ggplot2)
library (factoextra)
library(openxlsx)

#La primera variable del conjunto de datos contiene los números de identificación de los encuestados y no se utilizará en el análisis de agrupaciones. 
#Usamos la función de "daisy" para determinar la similitud entre las observaciones y etiquetar los resultados como d. 
#Para las opciones dentro de la función "daisy", usamos métrica para especificar el cálculo de la distancia. 
#Las opciones para el cálculo de la distancia incluyen "euclidiana", "manhattan" y "gower". Especificamos "gower"
d <- daisy(myData[,2:29], metric="gower")
d

#Usamos la función agnes con el método de Ward para realizar agrupaciones aglomerativas y etiquetar los resultados como mResult.
mResult <- agnes(d, method="ward")
mResult

#R informa un coeficiente de aglomeración de 0.94

#Usamos la función cutree para obtener los clústeres y luego agregamos la información de pertenencia al clúster al marco de datos myData. 
mClusters <- cutree(mResult, k=2)
myData <- data.frame(myData, mClusters)

#Usamos la función plot para obtener el gráfico del dendrograma.
plot(mResult, which.plots =2) 
fviz_dend(mResult, cex = 0, k = 2, main="Dendograma aglomeramiento jerárquico",
          color_labels_by_k = FALSE, k_colors=c("#2A2640","#17A67D"))


#Usamos la función de resumen para obtener estadísticas de resumen para cada grupo.
summary(subset(myData, mClusters==1))
summary(subset(myData, mClusters==2))
#summary(subset(myData, mClusters==3))


#Para averiguar el número de observaciones en cada grupo, usamos la función as.factor para convertir mCluster en datos categóricos 
#y luego usamos la función de resumen para averiguar el número de observaciones en cada grupo. 
summary(as.factor(mClusters))

#Usamos la función cbind para conocer qué individuos pertenecen a cada cluster.
Cluster <- cbind(Surveyed, mClusters)

#Generamos un data frame con la tabla de datos. 
Cluster_f <- data.frame(Cluster)
Cluster_f

#Exportamos el data frame a un archivo xlsx.
write.xlsx(Cluster_f,"Cluster_f.xlsx", asTable = TRUE) 
