library(easypackages) 
library(sp)
library(rgdal)
library(raster)
library(lattice)
library(latticeExtra)
library(ncdf4)

setwd("D:/2_Courses/R_Hidrologia/Tutorial_files") 
cuenca.shape <- readOGR(dsn="shapes", layer="chillon")
class(cuenca.shape)
plot(cuenca.shape, axes=T, col=c("red"))
head(cuenca.shape@data)
cuenca.utm <- spTransform(cuenca.shape, CRS("+proj=utm +zone=18 +ellps=WGS84 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
plot(cuenca.utm, axes=T, asp=1)
cuenca.wgs <- spTransform(cuenca.utm, CRS("+proj=longlat +ellps=WGS84"))
plot(cuenca.wgs, axes=T, asp=1)


r <- stack("PISCOV3-MONTHLY.nc") # El archivo *.nc debe estar en la carpeta lab2, no en subcarpetas
plot(r[[1]])
plot(cuenca.wgs, add=T)
r.chillon <- crop(r, cuenca.wgs, snap="out")
plot(r.chillon[[1]])
r.chillon <- mask(r.chillon, cuenca.wgs)
plot(r.chillon[[1:12]])
spplot(r.chillon, col.regions = rev(terrain.colors(100)))


Pisco.prec.brick <- brick("PISCOV3-MONTHLY.nc") # El archivo *.nc debe estar en la carpeta lab2, no en subcarpetas
Pisco.prec.brick
nlayers(Pisco.prec.brick)
plot(Pisco.prec.brick[[1:12]]) 
pp.cuenca.mensual <- extract(Pisco.prec.brick, cuenca.wgs, fun=mean)
colnames(pp.cuenca.mensual) <- 1:ncol(pp.cuenca.mensual)
View(pp.cuenca.mensual)
range(pp.cuenca.mensual)
plot(pp.cuenca.mensual[1,], type="o", col="1", ylim=c(0,200), ylab="P (mm)", xlab = "Meses", main="Precipitacion promedio areal - Chillon (mm)")
