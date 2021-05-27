# Archivo para chequear observaciones con informacion del IRI

# ---------------------------------------------------------------------------------

# cargar paquetes
library(reshape2)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Limpiar el espacio
rm(list=ls())

savepath = "/pikachu/datos4/Obs/t2m_cpc_daily"

# Seteo el directorio
setwd(savepath)

# Variable a descagar
# URL = "http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/.tmin/X/%28265W%29%28330W%29RANGEEDGES/T/%281%20Jan%201999%29%2831%20Dec%202015%29RANGEEDGES/Y/%2860S%29%2815N%29RANGEEDGES/data.nc")
# ncid=nc_open(URL, write=FALSE, readunlim=TRUE, verbose=FALSE,auto_GMT=TRUE, suppress_dimvals=FALSE )
# data = metR::ReadNetCDF(URL, out='array')
# data = data[[1]]

nc_tmax <- nc_open(paste(savepath,"/data.tmax.nc",sep = ""))
nc_tmin <- nc_open(paste(savepath,"/data.tmin.nc",sep = ""))

datamax = metR::ReadNetCDF(paste0(savepath,"/data.tmax.nc"), out='array')
datamin = metR::ReadNetCDF(paste0(savepath,"/data.tmin.nc"), out='array')
datamax = datamax[[1]]
datamin = datamin[[1]]

# t2m = promedio de tmax y tmin
t2m = (datamax + datamin)/2

# Leo dimensiones (mismas para ambas variables)
lon <- ncvar_get(nc_tmax, "X")
lat <- ncvar_get(nc_tmax, "Y", verbose = F)
t <- ncvar_get(nc_tmax, "T")

# Convertir las fechas
tiempos = as.character(as.Date(t, origin = "-4713-01-01"))
fechas = seq.Date(from = as.Date("1999-01-01"),to =as.Date("2015-12-31"),by=1)

# No hay valores faltantes 
#missingvalue = nc_tmax$var$tmax$missval
#datamax[which(datamin==missingvalue)]

# cambiar nombre de las dimensions (didt work)
dimnames(t2m)[[3]] <- fechas

# ----------------------------------------------------------------------------------------------
# Funcion que toma los campos de reanalisis para cada semana respecto al startdate y calcula
# la media 
ObsMediaSemanal <- function(PronoDate,TargetDate,Anomalia){
  
  # PronoDate: numero de la cantidad de startdates se tiene para el periodo. Por ej: 612
  # TargetDate: Matriz donde los elementos son las fechas del targetdate para cada startdate y cada lead
  # Anomalia: Array con las anomalias. Debe tener dimensiones iguales al modelo de lon y lat y una dimension
  #           temporal con nombre "days" y fechas en formato character
  
  # Crea las variables a llenar
  anom_media_semanal <- array(NA, dim = c(66,76,PronoDate,4))
  days = dimnames(Anomalia)$day 
  
  for (w in 1:4) {
    
    for (startdate in 1:PronoDate) {
      
      lead = c(1,8,15,22) # Es el lead inicial para cada semana
      
      week = as.vector(TargetDate[lead[w]:(lead[w]+6) , startdate])
      targetweek = Anomalia[,,which(days %in% week)]
      media_semanal = apply(targetweek, c(1,2), FUN = mean, na.rm = FALSE)
      anom_media_semanal[,,startdate,w] <- media_semanal 
      
    } #End loop pronodate
  }# End loop week
  
  return(anom_media_semanal)
}

# ----------------------------------------------------------------------------------------------


# Lo que voy a hacer es comparar las medias (clim) de dos bases de datos distintas y comparando con
# el psl

# ----------------------------------------------------------------------------------------
# CLIMATOLOGIA

## Climatologia del archivo "obs_to_RDS.R" que es la climatologia del dato original
clim=apply(clim, c(1,2), FUN = mean)

df.clim=reshape2::melt(clim)
dimnames(df.clim)[[2]] <- list("x","y","z")
breaks = seq(-5,35,5)
GraphDiscrete(df.clim,Breaks =breaks ,Titulo = "Climatologia \nCPC",Label = "°C",Paleta = "RdBu")

## Climatologia de la serie bajada entera
clim2 = apply(t2m, c(1,2),FUN = mean)
df.clim2=reshape2::melt(clim2)
dimnames(df.clim2)[[2]] <- list("x","y","z")
breaks = seq(-5,35,5)
GraphDiscrete(df.clim2,Breaks =breaks ,Titulo = "Climatologia \nNOAA",Label = "°C",Paleta = "RdBu")


prueba <- reshape2::melt(datamin[,,1])
dimnames(prueba)[[2]] <- list("x","y","z")
breaks = seq(-5,35,5)
GraphDiscrete(prueba,Breaks =breaks ,Titulo = "Prueba",Label = "°C",Paleta = "RdBu")

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
dimnames(clim) = list(lon = dimnames(obs)$lon , lat = dimnames(obs)$lat, monday = substr(tiempos_total[(1+365):(365*2+1)],6,10))
