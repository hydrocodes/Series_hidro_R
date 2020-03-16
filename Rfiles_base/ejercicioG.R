##Lectura de una cuenca hidrográfica en formato shape polígono
library(easypackages) 
library(sp)
library(rgdal)
library(raster)
library(lattice)
library(latticeExtra)
library(ncdf4)
# Definiendo un directorio de trabajo donde se encuentren los archivos input descargados
setwd("D:/2_Courses/R_Hidrologia/Tutorial_files") 
# Leyendo el polígono chillon.shp (layer: chillon) desde la carpeta "files" con data source name (dsn) 
cuenca.shape <- readOGR(dsn="shapes", layer="chillon")
# Conociendo el tipo de objeto después de la importación
class(cuenca.shape)
# Visualizando la cuenca con color cyan, se aprecia que se encuentra en coordenadas geograficas
plot(cuenca.shape, axes=T, col=c("red"))
# Conociendo el contenido del polígono importado
head(cuenca.shape@data)

######Proyección y reproyección de coordenadas
# Transformando a UTM zona 18, WGS84 y visualizando la conversión
cuenca.utm <- spTransform(cuenca.shape, CRS("+proj=utm +zone=18 +ellps=WGS84 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(cuenca.utm, axes=T, asp=1)
# Reproyectando a Geograficas WGS84 y visualizando la reconversión
cuenca.wgs <- spTransform(cuenca.utm, CRS("+proj=longlat +ellps=WGS84"))
plot(cuenca.wgs, axes=T, asp=1)


#####Lectura de la precipitación espacializada desde un ráster - Método 1
# Extrayendo datos y visualizando el producto ráster PISCO 
r <- stack("PISCOV3-MONTHLY.nc") # El archivo *.nc debe estar en la carpeta lab2, no en subcarpetas
# Visualizando la precipitación espacial del primer mes de 1981.
plot(r[[1]])
# Ploteando la cuenca del rio Chillón dentro del mapa de precipitaciones
plot(cuenca.wgs, add=T)
# Delimitando el área de estudio al cuadrante que ocupa el rio Chillón
r.chillon <- crop(r, cuenca.wgs, snap="out")
# Ploteando el primer mes de 1981 en el cuadrante que ocupa el rio Chillón
plot(r.chillon[[1]])
# Delimitando el área de estudio a la cuenca del rio Chillón
r.chillon <- mask(r.chillon, cuenca.wgs)
# Ploteando los meses de enero a diciembre de 1981
plot(r.chillon[[1:12]])
# Ploteando todos los meses del ráster (431 datos) delimitado por la cuenca del rio Chillón
spplot(r.chillon, col.regions = rev(terrain.colors(100)))

###Lectura de la precipitación espacializada desde un ráster - Método 2
# Extrayendo datos y visualizando
Pisco.prec.brick <- brick("PISCOV3-MONTHLY.nc") # El archivo *.nc debe estar en la carpeta lab2, no en subcarpetas
Pisco.prec.brick
nlayers(Pisco.prec.brick)
# Ploteando los primeros 12 meses de 1981 de la precipitación para todo el Perú
plot(Pisco.prec.brick[[1:12]]) 
# Extrayendo los datos y promediando todas las grillas de la cuenca del Chillón
pp.cuenca.mensual <- extract(Pisco.prec.brick, cuenca.wgs, fun=mean)
colnames(pp.cuenca.mensual) <- 1:ncol(pp.cuenca.mensual)
# Visualizando los 431 datos promediados 
View(pp.cuenca.mensual)
range(pp.cuenca.mensual)
# Ploteando la serie de los 431 valores de precipitación mensual promedio areal
plot(pp.cuenca.mensual[1,], type="o", col="1", ylim=c(0,200), ylab="P (mm)", xlab = "Meses", main="Precipitacion promedio areal - Chillon (mm)")
