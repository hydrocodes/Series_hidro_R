library(raster)
# Comando brick lee as capas contenidas en el archivo ncdf *.nc, (i.e. pr: precipitación mensual). 
# Archivo que contiene parte del registro histórico y proyección de precipitación a corto plazo (2005 - 2030) del modelo HadGEM2-ES para el escenario RCP4.5
b <- brick("D:/2_Courses/R_Hidrologia/Tutorial_files/cmip5/pr_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-203011.nc", varname = "pr")
b # Para ver especificaciones del raster obtenido. Ejm su resolución es: 1.875° x 1.25° ! 
plot (b) # Visualización de la precipitación global
# Asignando índices
idx <- getZ(b)
# Indicar coordenadas y extraer los valores y convertirlos de kg/m2/s a mm/mes
# En general formato de longitud GCM =  longitud WGS84 (ej. Google Earth) + 360°
# Ejemplo para la cuenca media del Rimac.
coords <- matrix(c(283.6, -11.8), ncol = 2) # 283.6 es la longitud (-76.4° en WGS84) y -11.8 la latitud
vals <- extract(b, coords, df=T)*86400*30.5 # Extrayendo las coordenadas y conversion
# Fijar fechas y datos en un solo archivo dataframe
df <- data.frame(idx, t(vals)[-1,])
rownames(df) <- NULL
names(df) <- c('date','value')
# Verificar el archivo
head(df) 
# Plotear la serie
plot(df, type="l", xlab="Años",  ylab="P (mm/mes)")

# Hacer los siguientes cambios al código anterior
# Lectura de la temperatura mínima: "tasmin" (e.g. tasmax para la temperatura maxima)
b <- brick("D:/2_Courses/R_Hidrologia/Tutorial_files/cmip5/tasmin_Amon_HadGEM2-ES_rcp45_r1i1p1_200512-203011.nc", varname = "tasmin")
# Conversión de datos K/d a °C/d
vals <- extract(b, coords, df=T)-273.15
# Plotear con las etiquetas correctas
df <- data.frame(idx, t(vals)[-1,])
rownames(df) <- NULL
names(df) <- c('date','value')
# Verificar el archivo
head(df) 
plot(df, type="l", xlab="Años",  ylab="Tmin (°C/d)")

