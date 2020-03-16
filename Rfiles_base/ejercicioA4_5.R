array2 =matrix(1:12, 3, 4)*2
array2

ndim <-  dim(array2)
nrows <- ndim[1]
ncols <- ndim[2]
nrows
ncols

typeof(ndim)
class(ndim)



##datos de indices de aridez
ia <- c(0.5, 0.45, 0.74, 0.51, 0.78, 0.66, 0.63, 0.82, 0.67, 0.41)
n_elem <- length(ia)

media <- mean(ia)
cat("los datos son de tipo:", class(ia))
if (media >=0.65){
  cat("\n\nRegion Humeda")
}else{cat("\n\nRegion Arida")}


for(i in 1:n_elem){
  cat("\n", "Año", i+1979, "--> ", ia[i])
}

i <- 1
while (i <= n_elem) {
  cat("\n", i+1979, "--> ", ia[i])
  i = i+1
}
#Uso del break
cat(ia,"\n\n")

for (ind in ia) {
  if (ia[ind]==0.66){
    break
  }
  print(ia[ind])
}

###LECTURA DE ARCHIVOS ASCII
pmensual<-read.table("D:/2_Courses/R_Hidrologia/pmensual.txt", header=TRUE)
pmensual
typeof(pmensual)
class(pmensual)
names(pmensual)
prec_ENE<-pmensual$ENE
yr<-pmensual$Año
plot(yr, prec_ENE, type = "b")
