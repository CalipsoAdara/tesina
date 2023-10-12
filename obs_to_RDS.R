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
#library("data.cube")

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
# 4 anios bisiestos, 13 anios no bisiestos 
t2m_sa_years <- array(NA, c(66,76))

for (i in 1:18) {
  
  year <- 1999:2016
  
  #-----------------------------------------
  # ABRIR DATOS 
  
  # Abro los archivos para T maxima y T minima
  nc_path_max=paste0("/pikachu/datos4/Obs/t2m_cpc_daily/tmax.",year[i],".nc")
  nc_path_min=paste0("/pikachu/datos4/Obs/t2m_cpc_daily/tmin.",year[i],".nc")
  
  nc_tmax <- ReadNetCDF(nc_path_max,subset = list(lat = -60:15, lon = 265:330),out = "array")
  nc_tmin <- ReadNetCDF(nc_path_min,subset = list(lat = -60:15, lon = 265:330),out = "array")
  #nc_tmin <- nc_open(paste0("/pikachu/datos4/Obs/t2m_cpc_daily/tmin.",year[i],".nc"))
  
  # Consigo los datos 
  tmax = nc_tmax[[1]]
  tmin = nc_tmin[[1]]
  
  # Leo dimensiones (mismas para ambas variables)
  lon <- dimnames(tmax)$lon
  lat <- dimnames(tmax)$lat
  tiempos <- dimnames(tmax)$time
  
  # missig value -9.96921e+36  which(tmin==-9.96921e+36)
  
  # Variable temperatura a 2 metros se obtiene como el promedio de Tmax y Tmin
  t2m = (tmax + tmin)/2
  
  #------------------------------------------------------------------
  # INTERPOLACION
  # lograr que los datos esten en una grilla 1x1 con numeros enteros
  
  # Creo lista vacia para llenar con los datos de todo un anio
  lista <- list()
  
  # Necesita un loop alrededor de todos los dias del año
  
  for (dias in 1:length(tiempos)) {
    # Defino un objeto con coordenadas y datos de mi campo
    obj <- list(x = lon, y = lat,
                z = t2m[,,dias])
    
    # Defino un grid con la dimensión la resolución que deseo,
    grid.list <- list(x =seq(265,330, 1), y = rev(seq(-60,15, 1)))
    # Funcion para realizar una interpolación
    # bilineal a la nueva reticula
    obj2 <- interp.surface.grid(obj, grid.list)
    
    # Guardo en la variable todos los dias de un solo año
    lista[[dias]] <- obj2[[3]] 
  
  
  } # Termina loop sobre los dias del año

  
  #-----------------------------------------------------------------------------
  # GUARDADO DE DATOS
  
  # Convierto la lista en array con el paquete abind
  # en el tercer elemento se encuntran los datos de T2m
  array_2 <- abind(lista,along = 3)
  
  # Guardo los datos en un solo array con las dimensiones de lon,lat de Sudamerica
  # y todos los dias del periodo 
  t2m_sa_years <- abind(t2m_sa_years,array_2,along = 3)
  
}  # End loop year


# Se quita el primer valor que es NA
t2m_sa_years <- t2m_sa_years[,,-1]
# Lleno la variable con las fechas
tiempos_total <- as.character(seq.Date(as.Date("1999-01-01"),as.Date("2016-12-31"),by=1))

#-----------------------------------------------------------------------------------------
# ANIOS BISIESTOS

# Renombrar la dimension de tiempo
dimnames(t2m_sa_years)[[3]] <- tiempos_total

# Sigo trabajando con el array
bisis=c("2000-02-29","2004-02-29","2008-02-29","2012-02-29","2016-02-29") 
nb=setdiff(tiempos_total,bisis) # me quedo solo con los dias que no son 29/2 en el período

data.nobis=t2m_sa_years[,,nb]

# ---------------------------------------------------------------------------------------
# CLIMATOLOGIA
# Reshape en los años
obs = array(data.nobis,c(66,76,365,18))
# Le doy nombre a las dimensiones del nuevo array
dimnames(obs) <- list(lon = seq(265,330, 1),lat = rev(seq(-60,15, 1)),monday = substr(tiempos_total[1:365],6,10),year=1999:2016)

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

# -------------------------------------------------------------------------------------------------------------
# CALCULO ANOMALIA CON ARRAY
# primero agrego el 29 de febrero en todos los años como NA para restarle la climatologia 

# array a completar 
more.bis = array(NA, dim = c(66,76,366*18))

for (s in 1:18) {
  
  # Busca posiciones donde esta el 28 de febrero
  mes_dia = substr(nb,6,10)
  dia28_nb = which(mes_dia == "02-28")
  
  # dias 29/02 en el objeto final
  dia29_falso = seq(60,366*17,366)
  
  # completo con t2m y dejo un campo vacio para todos los 29 de febrero (bisiesto o no bisiesto)
  
  if (s==1) { # primer año distinto
    more.bis[,,1:59] <- data.nobis[,,1:59]
  } else if(s<17) {
    inicio = dia29_falso[s-1]+1   # salta un dia para dejar el 29 vacio
  more.bis[,,inicio:(inicio+364)] <- data.nobis[,,(dia28_nb[s-1]+1):(dia28_nb[s])]
  } else { # ultimo año distinto
    more.bis[,,(5916+1):(366*17)] <- data.nobis[,,(dia28_nb[s]+1):6205]
  }
}

# ahora lleno los campos bisiestos reales 
bis = tiempos_total %in% bisis
bisiesto = c(426,1890,3354,4818) # posiciones de 29/02 reales 
for (b in 1:4) {
  more.bis[,,bisiesto[b]] <- t2m_sa_years[,,bis][b]
}

## Calculo la anomalia = full - clim
# Repito 17 veces la climatologia 
arr = rep(clim,17)
ar.clim = array(arr, dim = c(66,76,6222))
anom = more.bis-ar.clim

# Quito los 29/02 falsos
quitar29 = -(setdiff(dia29_falso,bisiesto))
array.anom = anom[,,quitar29]

# Guardo el array
dimnames(ar.anom) = list("lon" = lon , "lat" = lat , "day" = as.character(tiempos_total))
saveRDS(array.anom, file = "anomarrayNOAA.rds")

#------------------------------------------------------------------------------------------------------------
# CALCULO ANOMALIA CON DATA TABLE 

# Listo, ahora seguiría hacer las data tables, darle merge usando "monday,lat y lon" y después hacer la resta
# Aca es necesario que se mantengan los NaN para conservar las dimensiones
dt.clim=as.data.table(clim, na.rm = F)
dt.obs=as.data.table(t2m_sa_years, na.rm = F)


# Esta sentencia ordena de manera correcta las latitudes que generaba problemas al incluir negativos
dt.clim = dt.clim[order(match(lat, as.numeric(dimnames(obs)$lat)))]
dt.obs = dt.obs[order(match(lat, as.numeric(dimnames(obs)$lat)))]

# renombro variable
setnames(dt.clim, "value", "clim")
# Guardo la data table de la climatologia
saveRDS(dt.clim, file = "t2mclim_NOAA.rds")

# Agrego la dimension "monday" a dt.obs
dt.obs$monday=substr(as.character(dt.obs$dia),6,10)
setnames(dt.obs, "value", "t2m")

dt.anom=merge(dt.obs,dt.clim,by=c("lon","lat","monday"))
dt.anom$anom=dt.anom$t2m-dt.anom$clim

# Esta sentencia ordena de manera correcta las latitudes que generaba problemas al incluir negativos
dt.anom = dt.anom[order(match(lat, as.numeric(dimnames(obs)$lat)))]
dt.anom = dt.anom[order(match(dia, dt.obs$dia))]

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

####
lon = unique(dt.anom$lon)
lat = unique(dt.anom$lat)

# Este array es de 237 Mb, la data table es de 1 GB
array.anom <- array(dt.anom$anom, dim = c(length(lon), length(lat), length(tiempos_total)))
dimnames(array.anom) <- list("lon" = lon , "lat" = lat , "day" = as.character(tiempos_total))


# Guardo la data table de las observaciones
saveRDS(array.anom, file = "t2manom_NOAA.rds")
saveRDS(dt.anom ,file = "t2manom_data.table_NOAA.rds")
#end


