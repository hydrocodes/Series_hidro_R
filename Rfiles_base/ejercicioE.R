library(easypackages)
library(xts)
library(lattice)
library(ggplot2)
pdiaria <- read.csv(file.choose(),header=TRUE, check.names = F, stringsAsFactors = F)
str(pdiaria)
idx <- as.Date(pdiaria[,1])
data.matrix <- pdiaria[,-1]
data.xts <- xts(data.matrix, order.by = idx )
str(data.xts)
plot(data.xts)
plot(data.matrix[,1], type="l")
plot(data.xts[,3])
#Ploteando la ubicacion de los valores vacios
library(lubridate)
library(imputeTS)
library(fracdiff)
library(hydroGOF)
library(plyr)
plotNA.distribution(data.xts[,1], ylab = "P",xlab = "Nro datos")
plotNA.distribution(data.xts[,2], ylab = "P",xlab = "Nro datos")
plotNA.distribution(data.xts[,3], ylab = "P",xlab = "Nro datos")
plotNA.distribution(data.xts[,4], ylab = "P",xlab = "Nro datos")

#Completacion forzada de datos
p_int<-na.interpolation(data.xts) #interpolation
p_kal<-na.kalman(data.xts) #Filtro de Kalman
p_mn<-na.mean(data.xts) #promedio total 
plot(p_int[,1])
plot(p_kal[,1])
plot(p_mn[,1])

#Convirtiendo a mensuales la estacion 103
data.monthly <- apply.monthly(data.xts[,3], FUN = sum)
plot(data.monthly)
#Todas las estaciones a mensuales
data.monthly <- apply.monthly(data.xts, FUN=apply, MARGIN = 2, sum)
xyplot(data.monthly, ylim = c(0,600))
#Completacion de datos mediante correlaciones cruzadas
library(corrplot)
library(cutoffR) 
#Correlacion cruzada 
cor.cruzada.mensual <- cor(as.matrix(data.monthly),use="complete")
cor.test(data.monthly[,1],data.monthly[,2])
summary(cor.cruzada.mensual)
min(cor.cruzada.mensual)
#Grafico de matriz de correlaciones entre estaciones
corrplot(cor.cruzada.mensual, method="number",type = c("lower"),mar = c(4, 2, 3, 4)) 
data.cutoff <- data.frame(data.monthly,date=index(data.monthly),check.names = FALSE)

data.cutoff.comp <- cutoff(data = data.cutoff,method = c("correlation"),corr = "spearman",cutoff = 0.7) #para tmin:cutoff = 0.8, tmax:cutoff = 0.7 
data.mensual.comp <- as.xts(data.cutoff.comp,order.by = index(data.monthly)) 
xyplot(data.monthly,ylim=c(0,600)) # mensual 
xyplot(data.mensual.comp,ylim=c(0,600)) 
library(latticeExtra)
st.sin <- xyplot(data.monthly,ylim=c(0,600),col="red", lwd=2)
st.com <- xyplot(data.mensual.comp,ylim=c(0,600),col="blue", lwd=2) 
st.com + st.sin

library(hydroTSM)
hydroplot(as.zoo(data.xts[,3]), var.type="Precipitation", pfreq = "dma", ylab = "Prec")
hydroplot(as.zoo(data.monthly[,3]), var.type="Precipitation", pfreq = "ma", ylab = "Prec")

