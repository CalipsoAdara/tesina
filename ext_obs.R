# Analisis extremos y fechas extremas con las observaciones (t2m de cpc)
# 
#
# by Lucia M Castro
#-----------------------------------------------------------------------------------------------------------------------

# Limpio enviroment
rm(list=ls())

# Seteo directorio
setwd("/home/lucia.castro/")

# Cargo paquetes
library(secr)
library(reshape2)
library(csv)
library(dplyr)


# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Percentiles. Observaciones
# cargo datos
ar.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")
lon = dimnames(ar.anom)$lon
lat = dimnames(ar.anom)$lat

# Acomodo datos para hacer medias semanales
# En este caso voy a hacer medias semanales segun las semanas del mme
dias = dimnames(ar.anom)$day
targetdateMME= readRDS("./SubX_processed_Rdata/model/viernes/targetdate_MME_OA.rds")
inisemOM <- as.Date(dimnames(targetdateMME)$startdate)

# Poligonos. Lon de menor a mayor, el primer punto se repite para cerrar el poligono

SP <- readRDS("./SubX_processed_Rdata/model/poligonos/SP.rds")
SACZ <- readRDS("./SubX_processed_Rdata/model/poligonos/SACZ.rds")
SESA <- readRDS("./SubX_processed_Rdata/model/poligonos/SESA.rds")

# Variable a guardar
anom.sem = array(NA, dim = c(66,76,7,length(inisemOM)))

# OJO: la ultima semana no tiene 7 dias 
for (sem in 1:length(inisemOM)) {
  # Busco las fechas
  semana = as.character(seq.Date(inisemOM[sem],inisemOM[sem]+6,by=1))
  posicion = which(dias %in% semana)
  
  # Guardo
  anom.sem[,,,sem] = ar.anom[,,posicion]
  
}
# Promedio en la semana
anom.media = apply(anom.sem, c(1,2,4), mean)

# Convierto en Data frame 
dimnames(anom.media) <- list("lon" = lon, "lat" = lat, "semana" = as.character(inisemOM))
df.anom = reshape2::melt(anom.media)

# Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
obssacz=pointsInPolygon(df.anom[,1:2],SACZ) 
obssp=pointsInPolygon(df.anom[,1:2],SP) 
obssesa=pointsInPolygon(df.anom[,1:2],SESA) 
obs_sacz = df.anom[obssacz,]
obs_sp = df.anom[obssp,]
obs_sesa = df.anom[obssesa,]

# Promedio en lat y long
med_obs_sacz = DTPromEspacPesado(obs_sacz, "value", "semana", Lat = "lat")
med_obs_sp = DTPromEspacPesado(obs_sp, "value", "semana", Lat = "lat")
med_obs_sesa = DTPromEspacPesado(obs_sesa, "value", "semana", Lat = "lat")
#med_obs = DTPromEspacPesado(df.anom, "value", "semana", Lat = "lat")

# Encontrar semanas debajo del p10 y por encima del p90
fecha_sacz = FechasPercentiles(med_obs_sacz,med_obs_sacz$media)
fecha_sp = FechasPercentiles(med_obs_sp,med_obs_sp$media)
fecha_sesa = FechasPercentiles(med_obs_sesa,med_obs_sesa$media)
#fecha = FechasPercentiles(med_obs,med_obs$media)



list(fecha_sacz,fecha_sp, fecha_sesa)

# Generar data frame con toda la info
#colname = c("TOTAL10","TOTAL90","SACZ10","SACZ90","SP10","SP90")
colname = c("SACZ10","SACZ90","SP10","SP90","SESA10","SESA90")
extremo <- as.data.frame(list(fecha_sacz,fecha_sp,fecha_sesa))
colnames(extremo)<- colname

# Busca coincidencia entre regiones
p10 <- list(fecha[[1]],fecha_sacz[[1]], fecha_sp[[1]])
p90 <- list(fecha[[2]],fecha_sacz[[2]], fecha_sp[[2]])
Reduce(intersect, p10)
Reduce(intersect, p90)

#,secr
# Creo archivo csv 
write.csv(extremo, "./SubX_processed_Rdata/model/viernes/ext/extMME_reg.csv")
