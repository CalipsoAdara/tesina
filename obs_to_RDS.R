# Observations dataset to compare to SubX data (1999-2015)
#
# M. Alvarez 2020
# Modified by Lucia M Castro 
#-----------------------------------------------------------------------------
rm(list=ls())


# Call libraries to be used
library("ncdf4")
library("metR")
library("abind")
library("fields")

# Cargo mis funciones
source("funciones.R")

# Path a donde guardar los archivos
savepath = "/pikachu/datos4/Obs/t2m_cpc_daily"
#savepath = "/datos/SubX/hindcast/tassfc/daily/ensmean/"  # NO Pongas /vegeta porque ya estas parada ahi

# Seteo el directorio
setwd(savepath)

#----------------------------------------------------------------------------------
# LECTURA DE LOS DATOS TMAX Y TMIN Y CALCULO DE TPROMEDIO A 2M PARA TODO 
# EL PERIODO SIN RESTRINGIR MESES
  
# Abro un archivo netcdf cualquiera para obtener las dimensiones y poder crear
# un array con dichas dimensiones (NOTAR: tmax y tmin == dimensiones)
  
nc_tmax <- nc_open("/pikachu/datos4/Obs/t2m_cpc_daily/tmax.2000.nc")
#t2m_world = array(0,c(DimNc(nc_tmax),17))

# Hago una variable para llenar con todas las fechas diarias desde 1999 hasta 2015
# 5 anios bisiestos, 12 anios no bisiestos 
tiempos_total <- array()
max_years <- array()

t2m_sa_years <- array(NA, c(66,76))
for (i in 1:16) {
  
  year <- 1999:2015
  
  #-----------------------------------------
  # ABRIR DATOS 
  
  # Abro los archivos para T maxima y T minima
  inFname_max <- paste("./tmax.",year[i],".nc",sep = "")
  inFname_min <- paste("./tmin.",year[i],".nc",sep = "")
  nc_tmax <- nc_open(paste("/pikachu/datos4/Obs/t2m_cpc_daily/tmax.",year[i],".nc",sep = ""))
  nc_tmin <- nc_open(paste("/pikachu/datos4/Obs/t2m_cpc_daily/tmin.",year[i],".nc",sep = ""))
  
  # Leo dimensiones (mismas para ambas variables)
  lon <- ncvar_get(nc_tmax, "lon")
  lat <- ncvar_get(nc_tmax, "lat", verbose = F)
  t <- ncvar_get(nc_tmax, "time")
  
  # Convertir las fechas
  tiempos = as.character(as.Date(t/24, origin = "1900-01-01"))
  
  # Me quedo con los datos de mi region 
  lat_sa = which(lat<=15 & lat>=-60)
  lon_sa = which(lon<=330 & lon>=265)
  
  # Consigo los datos 
  tmax = ncvar_get(nc_tmax, names(nc_tmax$var))
  tmin = ncvar_get(nc_tmin, names(nc_tmin$var))
  
  # Restringo datos a los limites
  tmax_sa = tmax[lon_sa,lat_sa,]
  tmin_sa = tmin[lon_sa,lat_sa,]
  
  # Variable temperatura a 2 metros se obtiene como el promedio de Tmax y Tmin
  t2m = (tmax_sa + tmin_sa)/2
  
  #------------------------------------------------------------------
  # INTERPOLACION
  # lograr que los datos esten en una grilla 1x1 con numeros enteros
  
  # Creo array vacio para llenar con los datos de todo un anio
  # las dimensiones son las restrigidas para sudamerica con grilla de 1x1 (no 0.5)
  t2m_dias <- array(NA, dim = c(66,76)) 
  lista <- list()
  # Necesita un loop alrededor de todos los dias del anio
  
  for (dias in 1:length(tiempos)) {
    # Defino un objeto con coordenadas y datos de mi campo
    obj <- list(x = lon[lon_sa], y = lat[lat_sa],
                z = t2m[,,dias])
    
    # Defino un grid con la dimensión la resolución que deseo,
    grid.list <- list(x =seq(265,330, 1), y = rev(seq(-60,15, 1)))
    # Funcion para realizar una interpolación
    # bilineal a la nueva reticula
    obj2 <- interp.surface.grid(obj, grid.list)
    
    lista[[dias]] <- obj2 
    # Convierto la lista en array con el paquete abind
    # en el tercer elemento se encuntran los datos de T2m
    obj2 <- abind(obj2[[3]],along = 3)
    
    # Guardo en la variable todos los dias de un solo anio
    t2m_dias <- abind(t2m_dias, obj2)
  
  } # Termina loop sobre los dias del anio

  
  # Guardo los datos en un solo array con las dimensiones de lon,lat de Sudamerica
  # y todos los dias del periodo 
  t2m_sa_years <- abind(t2m_sa_years,t2m_dias,along = 3)
  
  # Rename times dimension      
  #dimnames(data_max)$time=tiempos
  #dimnames(data_min)$time=tiempos
  
  
  # Sigo trabajando con el array
  # bisis = c("2000-02-29","2004-02-29","2008-02-29","2012-02-29","2016-02-29") 
  # nb=setdiff(tiempos,bisis) # me quedo solo con los dias que no son 29/2 en el período
  # 
  # data.nobis_max = data_max[,,nb]
  # data.nobis_min = data_min[,,nb]
  
  
  # Lleno la variable con las fechas
  tiempos_total <- append(tiempos_total,tiempos)
  
  
}

dimnames(olr1) = list(lon = dimnames(data.nobis)$lon , lat = dimnames(data.nobis)$lat, monday = substr(tiempos_total[1:365],6,10), year=1998:2017)
# Se quita el primer valor que es NA
t2m_sa_years <- t2m_sa_years[,,-1]
tiempos_total <- tiempos_total[-1]

# Le doy nombre a las dimensiones del nuevo array
dimnames(t2m_sa_years) <- list(lon = lon[lon_sa],lat = lat[lat_sa],monday = substr(tiempos_total,6,10)) 

#------------------------------------------------------------------
# INTERPOLACION
# lograr que los datos esten en una grilla 1x1 con numeros enteros


# COSAS QUE PRUEBO -----------------------------------------------------------------------
tulio = array()

for (i in 1:10) {
  tutu = array(c(1:6),dim = c(2,3,10))
  piola = sqrt(tutu[,,i])
  
  tulio<- tutu
  tulio[,,]
  
  
}

f = array()
for (i  in 1:4) {
  x=c("pepe","cock")
  f = append(f,x)
  
}
f[-1]

u <- matrix(1:10,ncol = 2,byrow = T)

# Five different ways of binding together two matrices
x <- array(1:12,c(3,4,2))
y <- x+100
dim(abind(x,y,along=0)) # binds on new dimension before first
dim(abind(x,y,along=1)) # binds on first dimension
dim(abind(x,y,along=1.5))
dim(abind(x,y,along=2))
dim(abind(x,y,along=3))
dim(abind(x,y,rev.along=1)) # binds on last dimension
dim(abind(x,y,rev.along=0)) # binds on new dimension after last

xu<- array(NA, dim = c(2,3))
for (n in 1:5) {
  yu<-array(1:6, dim = c(2,3,n))
  xu<-abind(xu,yu, along = 3)
}

xu[,,-1]
# Generate data in the form of a sine wave
set.seed(1)
n <- 1e3
dat <- data.frame(
  x = 1:n,
  y = sin(seq(0, 5*pi, length.out = n)) + rnorm(n=n, mean = 0, sd=0.1)
)

approxData <- data.frame(
  with(dat, 
       approx(x, y, xout = seq(1, n, by = 10), method = "linear")
  ),
  method = "approx()"
)
approx(dat$x,dat$y,xout =seq(1, n, by = 10), method = "linear" )



# NOT RUN {
#
# evaluate an image at a finer grid
# 

data( lennon)
lennon <- matrix(1:256, nrow = 256, ncol = 256)
# create an example in the right list format like image or contour
obj<- list( x= 1:20, y=1:20, z= lennon[ 201:220, 201:220])

set.seed( 123)
# lots of random points
N<- 500
loc<- cbind( runif(N)*20, runif(N)*20)
z.new<- interp.surface( obj, loc)
# compare the image with bilinear interpolation at scattered points
set.panel(2,2)
image.plot( obj)
quilt.plot( loc, z.new) 






# Defino un objeto con coordenadas y
# datos de mi campo
obj <- list(x = lon[lon_sa], y = lat[lat_sa],
            z = t2m_sa_years[,,1])
# Defino un grid con la dimensión y/o
# resolución que deseo,
grid.list <- list(x =seq(265,330, 1), y = rev(seq(-60,15, 1)))
# Funcion para realizar una interpolación
# bilineal a la nueva reticula
obj2 <- interp.surface.grid(obj, grid.list)
obj2 <- abind(obj2[[3]],along = 3)

test_mat <- lapply(1:3, function(x) matrix(runif(12), nrow=3, ncol=4))

test_mat_2<-abind(test_mat, along=3)

