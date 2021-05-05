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
# 4 anios bisiestos, 13 anios no bisiestos 
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
  
  # missig value -9.96921e+36
  
  # Variable temperatura a 2 metros se obtiene como el promedio de Tmax y Tmin
  t2m = (tmax_sa + tmin_sa)/2
  
  # Cierro los archivos nc
  nc_close(nc_tmax)
  nc_close(nc_tmin)
  
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


# Esta sentencia ordena de manera correcta las latitudes que generaba problemas al incluir negativos
dt.clim = dt.clim[order(match(lat, as.numeric(dimnames(obs)$lat)))]
dt.obs = dt.obs[order(match(lat, as.numeric(dimnames(obs)$lat)))]

# renombro variable
setnames(dt.clim, "value", "clim")

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


# ---------------------------------------------------------------------------------------------
# Graficos

# Reordeno los datos para graficar solo las medias mensuales desde Octubre a Marzo
## IGNORAR ESTO, AUN LO ESTOY TRABAJANDO
# dt.anom$month <- month(dt.anom$targetdate)
# dt.anom$year <- format(dt.anom$targetdate,format="%Y")
# 
# aggregate(data = dt.anom,anom~month  ,FUN = mean)
# DF[  , .(sum_no = sum(no), unq_age = unique(age)), by = id]
# media_month <- dt.anom[  , .(MeanMonth = mean(anom,na.rm = T), lon,lat) , by = c("month","year")]

# Aca lei los datos por no correrlo todo seguido (ignorar la sentencia)
dt.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")



setDT(tmeasmax)[, .(MontlyMeans = mean(MEAN.C.)), by = .(year(TIMESTEP), month(TIMESTEP))]
dt.anom[,.(MonthMean = mean(anom), by = .(format(targetdate,"%Y"), months(targetdate)))]

dt.anom$mes <- month(dt.anom$targetdate)

# Creo estas columnas para calcular la media mensual
dt.anom$mesdia = substr(as.character(dt.anom$targetdate),6,10)
dt.anom$year = substr(as.character(dt.anom$targetdate),1,4)

dt.anom[,.(mediamensual= mean(anom), by = .(mesdia,year))]

# Seteo una key para realizar los procesos mas rapidos
setkey(dt.anom, mesdia)
# Ahora cuando aplico el filtro no es necesario indicar la columna en la que aplicar el filtro
enero <- dt.anom[J(1)]
dia <- dt.anom[c("01-01")]

# Uso el reordenamiento que usaba hasta ahora

for (s in 1:6) {
  meses <- c(1, 2, 3, 10, 11, 12)                   # Meses Octubre a Marzo
  month <- month(tiempos_total)                     # Vector con "01","02" repetidos segun la cant de dias
  
  # Tomo los valores de t2m para un solo mes por vez
  mes <- which(month == meses[s])                   # Vector con Indices del mes en cuestion
  t2m_mes <- t2m_sa_years[,,mes]                    # Evaluo en todas lat y lon para ese mes
  media_mensual <- apply(t2m_mes, c(1,2), mean)     # Calculo Media Mensual
  
  # Uso esta variable para graficar
  
  nombre_mes <- c("Enero", "Febrero", "Marzo", "Octubre", "Noviembre", "Diciembre")
  valores <- as.vector(media_mensual)
  
  # Obtengo las dimensiones necesarias
  # Convertirlo a numeric porque sino se generan problemas
  x <- as.numeric(dimnames(obs)$lon)
  y <- as.numeric(dimnames(obs)$lat)
  
  
  # Creo los data frames necesarios
  # En la primer columna tiene todas las long repetidas la cantidad de veces de las latitudes
  # En la segunda columna tiene todas las lat repetidas la cantidad de veces de las longitudes
  # En la tercera columna tiene los valores de media mensual para 1 mes en particular
  data_temp<-data.frame(x=rep(x,length(y)),
                        y=rep(y,each=length(x)),
                        z=valores)

  
  # Grafico 
  GraphDiscrete(Data = data_temp,
                Breaks = seq(-5,35,5),
                Titulo = p("T2M Media", "\n",nombre_mes[s]), 
                Label = "  C", 
                Paleta = "RdBu")
  
  ggsave(filename = p("t2mMedia_",nombre_mes[s],".png"), path = "/home/lucia.castro/tesina/imagenes/")
  
  GraphDiscrete2(Data = data_temp,
                Breaks = seq(-5,35,5),
                Titulo = p("T2M Media", "\n",nombre_mes[s]), 
                Label = "  C", 
                Paleta = "RdBu")
  
  ggsave(filename = p("t2mMedia_",nombre_mes[s],"_2",".png"), path = "/home/lucia.castro/tesina/imagenes/")
}
 

















