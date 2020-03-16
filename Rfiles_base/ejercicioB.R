library("lattice")
library("hydroTSM")
pmensual<-read.table("D:/2_Courses/R_Hidrologia/pmensual.txt", header=TRUE)
pmensual
typeof(pmensual)
class(pmensual)
names(pmensual)
prec_ENE<-pmensual$ENE
yr<-pmensual$Año
plot(yr, prec_ENE, type = "b")

pmensual<-read.table(file.choose(), header = F) 
datos<-pmensual[2:45,2:13]
datos_vector<-as.vector(t(datos))
datos_ts<-stats::ts(datos_vector, start=c(1964, 1), end=c(2010, 12), frequency=12)
plot.ts(datos_ts, col="black", main="Monthly Rainfall time series", ylab="P [mm]", xlab="Year")
lines(lowess(time(datos_ts), datos_ts), col="blue", lwd=2) 
lluvia<-pmensual[2:45,2:13] 
meses<-pmensual[1:1,2:13] 
colnames(lluvia)<-unlist(meses)
rownames(lluvia)<-pmensual[2:45,1:1] 
matrixplot(lluvia, ColorRamp="Precipitation",main="Monthly Rainfall 1964-2011 (mm/month)")