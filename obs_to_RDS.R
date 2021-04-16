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
library("data.table")
library("plyr")

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/pikachu/datos4/Obs/t2m_cpc_daily"
#savepath = "/datos/SubX/hindcast/tassfc/daily/ensmean/"  # NO Pongas /vegeta porque ya estas parada ahi

# Seteo el directorio
setwd(savepath)

#----------------------------------------------------------------------------------
# LECTURA DE LOS DATOS TMAX Y TMIN Y CALCULO DE TPROMEDIO A 2M PARA TODO 
# EL PERIODO SIN RESTRINGIR MESES


# Hago una variable para llenar con todas las fechas diarias desde 1999 hasta 2015
# 5 anios bisiestos, 12 anios no bisiestos 
tiempos_total <- array()
t2m_sa_years <- array(NA, c(66,76))

for (i in 1:17) {
  
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
  
  # Creo lista vacia para llenar con los datos de todo un anio
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
    
    # Guardo en la variable todos los dias de un solo anio
    lista[[dias]] <- obj2[[3]] 
  
  
  } # Termina loop sobre los dias del anio

  
  #-----------------------------------------------------------------------------
  # GUARDADO DE DATOS
  
  # Convierto la lista en array con el paquete abind
  # en el tercer elemento se encuntran los datos de T2m
  array_2 <- abind(lista,along = 3)
  
  # Guardo los datos en un solo array con las dimensiones de lon,lat de Sudamerica
  # y todos los dias del periodo 
  t2m_sa_years <- abind(t2m_sa_years,array_2,along = 3)
  
  # Lleno la variable con las fechas
  tiempos_total <- append(tiempos_total,tiempos)
  
  
}


# Se quita el primer valor que es NA
t2m_sa_years <- t2m_sa_years[,,-1]
tiempos_total <- tiempos_total[-1]

#-----------------------------------------------------------------------------------------
# ANIOS BISIESTOS

# Renombrar la dimension de tiempo

dimnames(t2m_sa_years)[[3]] <- tiempos_total

# Sigo trabajando con el array
bisis=c("2000-02-29","2004-02-29","2008-02-29","2012-02-29") 
nb=setdiff(tiempos_total,bisis) # me quedo solo con los dias que no son 29/2 en el período

data.nobis=t2m_sa_years[,,nb]

# ----------------------------------------------------------------------------------------
# CLIMATOLOGIA
# Reshape en los años
obs = array(data.nobis,c(66,76,365,17))
# Le doy nombre a las dimensiones del nuevo array
dimnames(obs) <- list(lon = seq(265,330, 1),lat = rev(seq(-60,15, 1)),monday = substr(tiempos_total[1:365],6,10),year=1999:2015)

dimnames(t2m_sa_years) <- list(lon = dimnames(obs)$lon , lat = dimnames(obs)$lat, dia = tiempos_total)
# Calculo la media en la dimensión de los años (tomo 1999-2015)
climday = apply(obs, c(1,2,3), mean, na.rm=TRUE)

climdayperiodic=array(data=NA_real_,dim=c(66,76,(31+365+31)))
climdayperiodic[,,1:31]=climday[,,335:365]
climdayperiodic[,,32:(365+31)]=climday[,,]
climdayperiodic[,,(365+31+1):427]=climday[,,1:31]


smoothclim=array(data=NA_real_,dim=c(66,76,(31+365+31)))

for(i in 1:66){
  for(j in 1:76){
    smoothclim[i,j,]=frollapply(climdayperiodic[i,j,],31,mean,align = "center")
    smoothclim[i,j,]=frollapply(smoothclim[i,j,],31,mean,align = "center")
  }
}

smoothclim=smoothclim[,,32:(365+31)] # Mejor =)

# Agrego 29/2=(28/2+1/3)/2

clim=array(data=NA_real_,dim=c(66,76,366))
clim[,,1:(31+28)]=smoothclim[,,1:(31+28)]
clim[,,60]=(smoothclim[,,59]+smoothclim[,,60])/2 #28/2+1/3
clim[,,61:366]=smoothclim[,,60:365]

rm("climday","climdayperiodic","smoothclim")
dimnames(clim) = list(lon = dimnames(obs)$lon , lat = dimnames(obs)$lat, monday = substr(tiempos_total[(1+365*2):(365*3+1)],6,10))


# Listo, ahora seguiría hacer las data tables, darle merge usando "monday,lat y lon" y después hacer la resta
# Aca es necesario que se mantengan los NaN para conservar las dimensiones
dt.clim=as.data.table(clim, na.rm = F)
dt.obs=as.data.table(t2m_sa_years, na.rm = F)

# renombro variable
setnames(dt.clim, "value", "clim")

# Agrego la dimension "monday" a dt.obs
dt.obs$monday=substr(as.character(dt.obs$dia),6,10)
setnames(dt.obs, "value", "t2m")

dt.anom=merge(dt.obs,dt.clim,by=c("lon","lat","monday"))
dt.anom$anom=dt.anom$t2m-dt.anom$clim

# Elimino las columnas de obs y climatología y esas data tables
dt.anom$t2m=NULL
dt.anom$clim=NULL
dt.anom$monday=NULL
rm("dt.obs","dt.clim")

# renombro la fila de las fechas para luego combinar
setnames(dt.anom, "dia", "targetdate")

# Ajusto los tipos de las variables para luego acoplar a los datos de SubX
dt.anom$lon=as.numeric(dt.anom$lon)
dt.anom$lat=as.numeric(dt.anom$lat)
dt.anom$targetdate=as.Date(dt.anom$targetdate)

# Guardo la data table de las observaciones
saveRDS(dt.anom, file = "t2manom_NOAA.rds")

#end






















