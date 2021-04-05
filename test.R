# Descargar archivos de Temperatura Maxima y Minima y obtener la promedio

# ---------------------------------------------------------------------------------

# Limpiar el espacio
rm(list=ls())

# Cargo paquetes
library("ncdf4")
library("ggplot2")
library("lubridate")  #caoaz no lo uso
library("dplyr")      #capaz no lo uso
library("maps")
library("RColorBrewer")
# Cargo mis funciones
source("funciones.R")

# Path a donde guardar los archivos
savepath = "/pikachu/datos4/Obs/t2m_cpc_daily"

# Seteo el directorio
setwd(savepath)

# # Variable a descagar
# var = "tmin"    # tmax, tmin, otros
# for (i in 1999:2014) {
#   
#   URL = paste("ftp://ftp2.psl.noaa.gov/Datasets/cpc_global_temp/",var,".",i,".nc",sep = "")
#   destfile = paste(var,".",i,".nc", sep = "")
#   
#   download.file(url = URL, destfile = destfile)
# }
# 
# 


# Calculos de Temperatura promedio

# Array vacio para llenar con medias mensuales (6 meses x 15 anios)
# 120 y 150 son la lon y lat luego de restringir a una region particular
t2m_media_mensual = array(0,c(120,150,6,15)) 


for (i in 1:15) {
  
  year <- 1999:2014
  
  #-----------------------------------------
  # ABRIR DATOS Y RESTRINGIR REGION
  
  # Abro los archivos para T maxima y T minima
  nc_tmax <- nc_open(paste("/pikachu/datos4/Obs/t2m_cpc_daily/tmax.",year[i],".nc",sep = ""))
  nc_tmin <- nc_open(paste("/pikachu/datos4/Obs/t2m_cpc_daily/tmin.",year[i],".nc",sep = ""))
  
  # Leo dimensiones (mismas para ambas variables)
  lon <- ncvar_get(nc_tmax, "lon")
  lat <- ncvar_get(nc_tmax, "lat", verbose = F)
  t <- ncvar_get(nc_tmax, "time")
  
  # Convertir las fechas
  tiempos = as.Date(t/24, origin = "1900-01-01")
  
  # Consigo los datos 
  tmax = ncvar_get(nc_tmax, names(nc_tmax$var))
  tmin = ncvar_get(nc_tmin, names(nc_tmin$var))
  
  # Defino los limites espaciales y temporales 
  # SUDAMERICA Y DESDE OCTUBRE A MARZO
  oct_mar = which(format(tiempos,"%m")>="10" | format(tiempos, "%m")<="03")
  lat_sa = which(lat<=15 & lat>=-60)
  lon_sa = which(lon<=330 & lon>=270)
  
  # Restringo datos a los limites
  tmax_sa = tmax[lon_sa,lat_sa,oct_mar]
  tmin_sa = tmin[lon_sa,lat_sa,oct_mar]
  
  
  #------------------------------------------
  # CALCULO DE PROMEDIOS
  
  # Variable temperatura a 2 metros se obtiene como el promedio de Tmax y Tmin
  t2m_sa = (tmax_sa + tmin_sa)/2
  
  
  # Loop para calcular promedios mensuales (MEJORAR)
  
  
  
  for (s in 1:6) {
    
    meses <- c("01", "02", "03","10", "11", "12")     # Meses Octubre a Marzo
    month <- format(tiempos,"%m")[oct_mar]            # Vector con "01","02" repetidos segun la cant de dias
    
    # Tomo los valores de t2m para un solo mes por vez
    mes <- which(month == meses[s])                   # Vector con Indices del mes en cuestion
    t2m_mes <- t2m_sa[,,mes]                          # Evaluo en todas lat y lon para ese mes
    media_mensual <- apply(t2m_mes, c(1,2), mean)     # Calculo Media Mensual
    
    # Guardo esta informacion en una variable
    
    t2m_media_mensual[,,s,i] <-media_mensual  
    
  }
  
  # Ahora completo el array de t2m_media_mensual con los distintos anios en la ultima dimension
  
  
}


# Hecho este calculo enorme, ahora se calcula el promedio de cada mes usando todos los anios
# Sera un array del mapa de la region para los meses propuestos

t2m_media_mensual_anual <- apply(t2m_media_mensual, c(1,2,3), mean)




#---------------------------------------------------------------------------------------
# G R A F I C O S 

# Seteo los parametros de mapa y gradiente 
mapa<-map_data("world2") 
min <- min(t2m_media_mensual_anual, na.rm = T)
max <- max(t2m_media_mensual_anual, na.rm = T)
my_fill<-scale_fill_gradient(name=expression(" C"),
                             high  = "firebrick2", 
                             low = "royalblue", 
                             limits=c(min,max),
                             na.value = "transparent")

# Obtengo las dimensiones necesarias
x <- lon[lon_sa]
y <- lat[lat_sa]

for (meses in 1:6) {
  nombre_mes <- c("Enero", "Febrero", "Marzo", "Octubre", "Noviembre", "Diciembre")
  valores <- as.vector(t2m_media_mensual_anual[,,meses])
  
  # Creo los data frames necesarios
  # En la primer columna tiene todas las long repetidas la cantidad de veces de las latitudes
  # En la segunda columna tiene todas las lat repetidas la cantidad de veces de las longitudes
  # En la tercera columna tiene los valores de media mensual para 1 mes en particular
  data_temp<-data.frame(x=rep(x,length(y)),
                        y=rep(y,each=length(x)),
                        z=valores)
  # Grafico un mapa para cada mes desde Octubre a Marzo
  
  ggplot(data=data_temp,aes(x=x,y=y)) +
    geom_tile(aes(fill=z)) +
    my_fill +
    geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
    coord_fixed(1) +
    ggtitle(paste("Temperatura Media", "\n", nombre_mes[meses],sep = ""))  +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()+
    xlim(270,330) + ylim(-60,15) +
    xlab("Longitud") + ylab("Latitud") +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("TempMedia_",nombre_mes[meses],".png",sep=""))
  
}




# Grafico un mapa para cada mes desde Octubre a Marzo

ggplot(data=data_temp,aes(x=x,y=y)) +
  geom_tile(aes(fill=z)) +
  scale_fill_gradientn(name= expression("C"),colours = brewer.pal(9,"YlOrRd"),limits = c(min,max),na.value = "transparent") +
  #scale_fill_gradient(name=expression(" C"),
  #                    high  = "firebrick2", 
  #                    low = "royalblue", 
  #                    limits=c(min,max),
  #                    na.value = "transparent")+
  geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
  coord_fixed(1) +
  ggtitle(paste("Temperatura Media", "\n", nombre_mes[meses],sep=""))  +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw()+
  xlim(270,330) + ylim(-60,15) +
  xlab("Longitud") + ylab("Latitud") +
  theme(plot.title = element_text(hjust = 0.5))
#
barplot(rep(1, 20), axes = FALSE, space = 0, col = rainbow(20))

ncfile = nc_open("/pikachu/datos4/Obs/t2m_cpc_daily/tmax.1999.nc")

lon <- ncvar_get(ncfile, "lon")
lat <- ncvar_get(ncfile, "lat", verbose = F)
t <- ncvar_get(ncfile, "time")

names_var = names(ncfile$var)
tmax = ncvar_get(ncfile, "tmax")
names_dim = names(ncfile$dim)

# Convertir las fechas
tiempos = as.Date(t/24, origin = "1900-01-01")
format(tiempos,"%m")


oct_mar = which(format(tiempos,"%m")>="10" | format(tiempos, "%m")<="03")
lat_sa = which(lat<=15 & lat>=-60)
lon_sa = which(lon<=330 & lon>=270)