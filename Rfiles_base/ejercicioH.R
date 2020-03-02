library(rgdal) 
library(gstat)
#definir directorio de trabajo
setwd("D:/2_Courses/R_Hidrologia")
# Cargar los datos de countries.shp a una variable llamada limite
# Cargar los datos de GSOD_2008_bilogora.csv a una variable llamada PREC.2008
limite <- readOGR(dsn="files", layer="countries")
PREC.2008<-read.csv(file.choose(),header=TRUE, sep=";")
names(PREC.2008)
#STN: nombre de estación
#WBAN: identificador del weather bureau air force navy 
#TEMPC: temperatura diaria acumulada (celcius) 
#TEMP.count: número de observaciones usadas para calcular la temperatura media 
#PREC: precipitación total diaria acumulada (mm) 
#LAT y LON: coordenadas en latitud longitud, wgs84 
#DATE: fecha de observación
str(PREC.2008)

# Formatear datos de fechas 
PREC.2008$DATE <- as.Date(PREC.2008$DATE)
# Seleccionar datos de un día en específico (1 de mayo 2008 - 01/05/2008) y cuyos valores no sean vacios
PREC.20080501 <- subset(PREC.2008, PREC.2008$DATE==as.Date("2008-05-01")&!is.na(PREC.2008$PREC)) 
str(PREC.20080501)

head(PREC.20080501)
#Asignar coordenadas para convertir en Spatial.Points.Data.Frame 
coordinates(PREC.20080501) <- ~LON+LAT # recordar que otra opción es c(LON,LAT) 
#Asignar sistema de referencia 
proj4string(PREC.20080501) <- CRS("+proj=longlat +datum=WGS84") 
#Definir sistema de referencia UTM y convertir 
utm33 <- "+proj=utm +zone=33 +ellps=WGS84 +datum=WGS84 +units=m +no_defs" 
PREC.20080501.XY <- spTransform(PREC.20080501, CRS(utm33)) 
#En los datos hay una estación duplicada, de manera que se eliminará
table(duplicated(PREC.20080501.XY$STATION.NAME))
PREC.20080501.XY <- remove.duplicates(PREC.20080501.XY) 
hist(PREC.20080501.XY$PREC)

# Se aplicará log para mejorar la normalización de los datos.
#log1p = log(x+1) se utiliza para evitar tener valores negativos resultado de log
hist(log1p(PREC.20080501.XY$PREC))
#Visualizar
PREC.plt1<- bubble(PREC.20080501.XY, "PREC", col="black", pch=21, main="PREC 2008-05-01", sp.layout=list(list("sp.polygons", col="grey", fill="transparent", limite), list("sp.points", col="black", pch="+", cex=1.2, subset(PREC.20080501.XY, PREC.20080501.XY$PREC==0))))
PREC.plt1

#Variograma empírico 
PREC.ve.d <- variogram(log1p(PREC)~1, PREC.20080501.XY) 
plot(PREC.ve.d, pl = T)

PREC.ve.d
# variograma inicial 
PREC.vi.d <- vgm(nugget=0, model="Exp", range=sqrt(diff(PREC.20080501.XY@bbox["LON",])^2 + diff(PREC.20080501.XY@bbox["LAT",])^2)/4, psill=var(log1p(PREC.20080501.XY$PREC))) 
# Regla general para evitar determinar parámetros visualmente 
# Nugget = 0 
# partial-sill = varianza total de los datos 
# Range = 1/4 de la distancia maxima, diagonal del bounding box 
PREC.vi.d

#Ajuste del variograma (teórico)
PREC.vt.d <- fit.variogram(PREC.ve.d, model=PREC.vi.d)
PREC.vt.d
plot(PREC.ve.d, pl = T, model = PREC.vt.d)

# Preparar grilla para predicción 
PREC.20080501.XY_grid = spsample(PREC.20080501.XY, type = "regular", cellsize = c(1000,1000)) 
#spsample selecciona una muestra regular de puntos en la extensión geográfica de PREC.20080501.XY.
#Los puntos estarán espaciados uno de otro 1000m. 
class(PREC.20080501.XY_grid)

gridded(PREC.20080501.XY_grid) = TRUE 
class(PREC.20080501.XY_grid)

#Thiessen 
thiessen.d = krige(PREC ~ 1, PREC.20080501.XY, PREC.20080501.XY_grid, nmax = 1)
pts.s <- list("sp.points", PREC.20080501.XY, col="white",pch=20)
spplot(thiessen.d, "var1.pred", asp=1, at= seq(0,16,1), zlim=c(0,16), col.regions=gray(seq(0.9,0.1,l=30)),sp.layout = list(pts.s),main="Thiessen")

#Inverso de la distancia (idw) 
idw.d = idw(PREC ~ 1, PREC.20080501.XY, PREC.20080501.XY_grid)
spplot(idw.d, "var1.pred", asp=1, at= seq(0,16,1), zlim=c(0,16), col.regions=gray(seq(1,0.1,l=30)),sp.layout = list(pts.s),main="IDW")

# Ordinary kriging
ok.d <- krige(log1p(PREC) ~ 1, locations = PREC.20080501.XY, newdata = PREC.20080501.XY_grid, model = PREC.vt.d)
ok.d$PREC.pred <- expm1(ok.d$var1.pred)# Antilog para volver a valores de precipitación
par(mfrow=c(2,1))
print(spplot(ok.d, "PREC.pred", asp=1, at= seq(0,16,1), zlim=c(0,16), col.regions=gray(seq(1,0.1,l=30)),main="OK, diaria 01/05/2008", sp.layout = list(pts.s), zlim=c(0,16)), split=c(1,1,2,1), more=TRUE) 
print(spplot(ok.d, "PREC.pred", asp=1, col.regions=gray(seq(1,0.1,l=30)), main="Varianza OK, diaria 01/05/2008", sp.layout = list(pts.s)), split=c(2,1,2,1), more=FALSE)
