# Archivo para chequear observaciones con informacion del IRI

# ---------------------------------------------------------------------------------
# Limpiar el espacio
rm(list=ls())

# cargar paquetes
library(reshape2)
library(metR)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

savepath = "/pikachu/datos4/Obs/t2m_cpc_daily"

# Seteo el directorio
setwd(savepath)

# Variable a descagar
URL_max = "https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/.tmax/Y/%2860S%29%2815N%29RANGEEDGES/T/%281%20Jan%201999%29%2831%20Dec%202015%29RANGEEDGES/X/%2895W%29%2830W%29RANGEEDGES/dods"
URL_min = "https://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.temperature/.daily/.tmin/Y/%2860S%29%2815N%29RANGEEDGES/T/%281%20Jan%201999%29%2831%20Dec%202015%29RANGEEDGES/X/%2895W%29%2830W%29RANGEEDGES/dods"

# ncid_max=nc_open(URL_max, write=FALSE, readunlim=TRUE, verbose=FALSE,auto_GMT=TRUE, suppress_dimvals=FALSE )
# ncid_min=nc_open(URL_min, write=FALSE, readunlim=TRUE, verbose=FALSE,auto_GMT=TRUE, suppress_dimvals=FALSE )
data_max = metR::ReadNetCDF(URL_max, out='array')
data_min = metR::ReadNetCDF(URL_min, out='array')
data_max = data_max[[1]]
data_min = data_min[[1]]


# t2m = promedio de tmax y tmin
t2m = (data_max + data_min)/2

# Leo dimensiones (mismas para ambas variables)
lon <- as.numeric(dimnames(t2m)$X)
lat <- as.numeric(dimnames(t2m)$Y)

# Convertir las fechas
fechas = as.character(seq.Date(from = as.Date("1999-01-01"),to =as.Date("2015-12-31"),by=1))
dimnames(t2m)$T <- fechas
# No hay valores faltantes 
#missingvalue = nc_tmax$var$tmax$missval
#datamax[which(datamin==missingvalue)]

# Semana de 12 al 18 Febrero 2000
full = t2m[,,408:414]
dia_mes = substr(fechas,6,10)
week = c("02-12","02-13","02-14","02-15","02-16","02-17","02-18")
day =c("2000-02-12","2000-02-13","2000-02-14","2000-02-15","2000-02-16","2000-02-17","2000-02-18")
day2 =c("0212","0213","0214","0215","0216","0217","0218")
Feb1218 = dia_mes %in% week
semana_full = t2m[,,Feb1218]

# clim como el promedio de 12 a 18 de febrero
semana_full2 = array(t2m[,,Feb1218], dim = c(130,150,7,17))
clim = apply(semana_full2, c(1,2,3), mean)

anom = full - clim

for (i in 1:7) {
  
  title = paste0("t2m Anom IRI \n",day[i])
  valores = as.vector(anom[,,i])
  # Creo los data frames necesarios
  # En la primer columna tiene todas las long repetidas la cantidad de veces de las latitudes
  # En la segunda columna tiene todas las lat repetidas la cantidad de veces de las longitudes
  # En la tercera columna tiene los valores de media mensual para 1 mes en particular
  df <-data.frame(x=rep(lon,length(lat)),
                  y=rep(lat,each=length(lon)),
                  z=valores)
  
  GraphDiscreteIRI(df, Titulo = title, Paleta = "RdBu", Direccion = -1,
                Label = "°C", Breaks = seq(-6,6,1))
  ggsave(paste0("/home/lucia.castro/SubX_processed_Rdata/IRIanom_",day2[i],".png"),width = 10, height = 11)
}


# Promedio semanal para esa semana
media = as.numeric(apply(anom, c(1,2), mean))
df.media <-data.frame(x=rep(lon,length(lat)),
                      y=rep(lat,each=length(lon)),
                      z=media) 
GraphDiscreteIRI(df.media, Titulo = "T2M ANOM IRI \nmedia 12-18 Febrero 2000", Paleta = "RdBu", Direccion = -1,
                 Label = "°C", Breaks = seq(-6,6,1))
ggsave(paste0("/home/lucia.castro/SubX_processed_Rdata/IRImedia.png"),width = 10, height = 11)


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




