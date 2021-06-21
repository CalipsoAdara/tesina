# SubX data - olr anomaly verification for GMAO model (SubX database)
#
# M. Alvarez - 2020
# Modified by Lucia M Castro
#-----------------------------------------------------------------------------------------------------------------------
rm(list=ls())

setwd("/home/lucia.castro/")

# Call libraries to be used
library("ncdf4")
library("metR")
library('pracma')
library('lubridate')
library('reshape2')
library('data.table')
library("s2dverification") # No se si lo voy a usar al final
library("RColorBrewer")
library("maps")
library("ggplot2")
library("gridExtra")
library("grid")

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")
#---------------------------------------------------------------------------------------
#  functions of verification metrics 
#---------------------------------------------------------------------------------------

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
      media_semanal = apply(targetweek, c(1,2), FUN = mean, na.rm = T)
      anom_media_semanal[,,startdate,w] <- media_semanal 
      
    } #End loop pronodate
  }# End loop week
  
  return(anom_media_semanal)
}

# ----------------------------------------------------------------------------------------------
# Funcion que calcula la media semanal para cada fecha de pronostico del modelo
ModelMediaSemanal <- function(Modelo, PronoDate){
  # Modelo: Array del modelo con dimension de lon, lat, leads y cada fecha pronostico
  # PronoDate: numero de la cantidad de startdates se tiene para el periodo. Por ej: 612
  # Creo variable a llenar
  model_media_semanal <- array(NA, dim = c(66,76,PronoDate,4))
  
  for (w in 1:4) {
    
    lead = c(1,8,15,22) # Es el lead inicial para cada semana
    ar.model.w = Modelo[,,lead[w]:(lead[w]+6),]  # Toma cada semana 
    media_semanal = apply(ar.model.w, c(1,2,4), FUN = mean, na.rm= TRUE)
    model_media_semanal[,,,w]<- media_semanal
  } # End loop week
  
  return(model_media_semanal)
}

#---------------------------------------------------------------------------------------
#  Main Program  
#---------------------------------------------------------------------------------------
ar.model = readRDS("/home/lucia.castro/SubX_processed_Rdata/model_GMAO_ONDEFM.rds")
targetdate = readRDS("/home/lucia.castro/SubX_processed_Rdata/targetdate_GMAO_ONDEFM.rds")
ar.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")

ar.model = ar.model[,seq(76,1,-1),,]
# La cantidad de fechas de pronosticos desde Oct a Mar en el periodo del modelo
fechas_pronosticos <- dim(ar.model)[4]

# Realizo una media semanal para el reanalisis
anom_media_semanal <- ObsMediaSemanal(PronoDate = fechas_pronosticos, TargetDate = targetdate,
                                      Anomalia = ar.anom)
model_media_semanal <- ModelMediaSemanal(Modelo = ar.model, PronoDate = fechas_pronosticos)


# Calculo las distintas métricas por cada lon/lat/targetweek
dif = (anom_media_semanal - model_media_semanal)
me = apply(dif, c(1,2,4),FUN = mean, na.rm = TRUE)
mae = apply(abs(dif), c(1,2,4), FUN = mean, na.rm = TRUE) 
rmse = sqrt(apply(dif^2,c(1,2,4), FUN = mean, na.rm = TRUE))

# Para el calculo de ACC hago una vuelta mas, para recorrer todos los puntos y obtener un valor de correlacion
acc <- array(NA, dim = c(66,76,4))

for (week in 1:4) {
  for (lon in 1:66) {
    for (lat in 1:76) {
      
      # Me quedo solo con una semana a analizar y todos las fechas de pronostico
      anom.week <- anom_media_semanal[,,,week]
      model.week <- model_media_semanal[,,,week]
      # Me quedo solo con un punto particular y todas las fechas de pronostico
      observ <- anom.week[lon,lat,]
      modelo <- model.week[lon,lat,]
      
      coef_corr <- cor(observ,modelo,use="pairwise.complete.obs",method = "pearson")
      
      acc[lon,lat,week] <- coef_corr
      
    } # End loop lat
    
  }  # End loop lon
  
} # End loop week

# Otra forma de calcular la correlacion 
# r = covariaza/desvio x * desvio y
acc2 <- array(NA, dim = c(66,76,4))

for (week in 1:4) {
  for (lon in 1:66) {
    for (lat in 1:76) {
    
      # Me quedo solo con una semana a analizar y todos las fechas de pronostico
      anom.week <- anom_media_semanal[,,,week]
      model.week <- model_media_semanal[,,,week]
      # Me quedo solo con un punto particular y todas las fechas de pronostico
      observ <- anom.week[lon,lat,]
      modelo <- model.week[lon,lat,]
      
      # Covarianza
      cova <- cov(observ,modelo,use="pairwise.complete.obs",method = "pearson")
      # medias
      media_obs <- sd(observ,na.rm = T)
      media_mod <- sd(modelo,na.rm = T)
      
      # Correlacion
      coefCorr <- cova/(media_mod*media_obs)
      
      acc2[lon,lat,week] <- coefCorr
      
    } # End loop lat
    
  }  # End loop lon
  
} # End loop week

# Renombro dimensiones 
dimnames(me) <- list(x = dimnames(ar.anom)$lon, 
                     y = dimnames(ar.anom)$lat, 
                     week = c("Week 1", "Week 2","Week 3", "Week 4"))
dimnames(mae) <- list(x = dimnames(ar.anom)$lon, 
                     y = dimnames(ar.anom)$lat, 
                     week = c("Week 1", "Week 2","Week 3", "Week 4"))
dimnames(rmse) <- list(x = dimnames(ar.anom)$lon, 
                     y = dimnames(ar.anom)$lat, 
                     week = c("Week 1", "Week 2","Week 3", "Week 4"))
dimnames(acc) <- list(x = dimnames(ar.anom)$lon, 
                      y = dimnames(ar.anom)$lat, 
                      week = c("Week 1", "Week 2","Week 3", "Week 4"))

# Armo data.frames para graficar
dt.me <- reshape2::melt(me, value.name = "z")
dt.mae <- reshape2::melt(mae, value.name = "z")
dt.rmse <- reshape2::melt(rmse, value.name = "z")
dt.acc <- reshape2::melt(acc, value.name = "z")
dt.acc2 <- reshape2::melt(acc2, value.name = "z")


x <- as.numeric(dimnames(ar.anom)$lon)
y <- as.numeric(dimnames(ar.anom)$lat)
valores <- as.vector(rmse[,,4])

# Creo los data frames necesarios
# En la primer columna tiene todas las long repetidas la cantidad de veces de las latitudes
# En la segunda columna tiene todas las lat repetidas la cantidad de veces de las longitudes
# En la tercera columna tiene los valores de media mensual para 1 mes en particular
data_temp<-data.frame(x=rep(x,length(y)),
                      y=rep(y,each=length(x)),
                      z=valores)
plot(rmse[,,1])
GraphDiscrete(data_temp,Titulo = "averr", Label = "rmse",Breaks = seq(0,3,0.5), Paleta = "YlOrRd", Direccion = 1)
#---------------------------------------------------------------------------------------
#  Gráficos  
#---------------------------------------------------------------------------------------
g1 <- GraphDiscreteMultiple(Data = dt.rmse, Breaks = seq(0,3,0.5),Label = "rsme",Paleta = "YlOrRd", Direccion = "1")
g2 <- GraphDiscreteMultiple(Data = dt.me, Breaks = seq(-0.1,0.1,0.025), Label = "me",Paleta = "RdBu",Direccion = "-1")
g3 <- GraphDiscreteMultiple(Data = dt.mae, Breaks = seq(0,2,0.25), Label = "ACC",Paleta = "YlOrRd",Direccion = "1")


fig <- grid.arrange(g1,g2,g3, ncol = 1,top = textGrob("SubX GMAO-GEOS_V2p1 tasa (99-15, Oct-Mar)",gp=gpar(fontsize=13,font=3)))
ggsave(filename="/home/lucia.castro/SubX_processed_Rdata/scores_map_GMAO.png",plot=fig,width = 10, height = 11)



# Graficos para comparar con los de PSL

x <-as.numeric( dimnames(ar.anom)$lon)
y <- as.numeric(dimnames(ar.anom)$lat)
breaks = seq(-6,6,1)
valores <- as.vector(anom_media_semanal[,,77,1]) #45  77   1   458
targetdate[,77]
data_temp <- data.frame(x=rep(x,length(y)),
                        y=rep(y,each=length(x)),
                        z=valores)

GraphDiscrete(data_temp, Breaks = breaks, Titulo = "T2M week mean \n 22-28 Ene 2001", Paleta = "RdBu",Label = "°C") 
ggsave(filename="/home/lucia.castro/SubX_processed_Rdata/meanweak20010122_28.png",)

# Graficar rmse de la semana dos nomas
dt.rmse[week=="Week 2"]
GraphDiscrete(dt.acc,Titulo = "aver", Paleta = "YlOrRd", Label = "rmse",Breaks = seq(0,3,0.5))




# GRAFICO UNO POR UNO A VER QUE PASA 

# 11 de enero 
semobs = ar.anom[,,12:18]
semmod = ar.model[,seq(76,1,-1),1:7,3]  

ggDataFrame(apply(semobs,c(1,2), mean))

GraphDiscrete(ggDataFrame(apply(semobs,c(1,2), mean)),Titulo = "OBS", Paleta = "RdBu", Label = "C",Breaks = seq(-6,6,1), Direccion = -1)
GraphDiscrete(ggDataFrame(apply(semmod,c(1,2), mean)),Titulo = "MODE", Paleta = "RdBu", Label = "C",Breaks = seq(-6,6,1), Direccion = -1)


# LUEGO DE LAS FUNCIONES

anom_media_semanal[,,3,1]

GraphDiscrete(ggDataFrame(anom_media_semanal[,,3,1]),Titulo = "OBS", Paleta = "RdBu", Label = "C",Breaks = seq(-6,6,1), Direccion = -1)
GraphDiscrete(ggDataFrame(model_media_semanal[,,3,1]),Titulo = "MODE", Paleta = "RdBu", Label = "C",Breaks = seq(-6,6,1), Direccion = -1)

# LA DIFERENCIA
GraphDiscrete(ggDataFrame(mae[,,1]),Titulo = "diferencia", Paleta = "RdBu", Label = "C",Breaks = seq(0,2,0.2), Direccion = -1)
GraphDiscrete(ggDataFrame(dif[,,3,1]),Titulo = "diferencia", Paleta = "RdBu", Label = "C",Breaks = seq(-6,6,1), Direccion = -1)
Breaks = seq(-6,6,1)

ggplot() +
  geom_contour_fill(data=ggDataFrame(mae[,,1]),aes(x, y, z = z))+
  scale_fill_distiller(name="Label",palette="RdBu",direction= -1,
                       na.value = "transparent",
                       breaks = Breaks,
                       limits = c(min(Breaks), max(Breaks)),
                       guide = guide_colorstrip(),
                       oob  = scales::squish)
# BAJANDO COSAS DEL IRI

URLSEM = "https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.GMAO/.GEOS_V2p1/.hindcast/.tas/X/%2895W%29%2830W%29RANGEEDGES/Y/%2860S%29%2815N%29RANGEEDGES/S/%280000%2011%20Jan%201999%29%280000%2018%20Jan%201999%29RANGEEDGES/dods"
data_SUBX = metR::ReadNetCDF(URLSEM, out='array')
data_SUBX=data_SUBX[[1]]


# abrir el dato full
inPath='/datos/SubX/hindcast'
path = paste0(inPath,"/tassfc/daily/full/GMAO-GEOS_V2p1/")

file = "tas_sfc_GMAO-GEOS_V2p1_19990111.e1.SouthAmerica.daily.nc"
file = "tas_sfc_GMAO-GEOS_V2p1_19990111.e2.SouthAmerica.daily.nc"
file = "tas_sfc_GMAO-GEOS_V2p1_19990111.e3.SouthAmerica.daily.nc"
file = "tas_sfc_GMAO-GEOS_V2p1_19990111.e4.SouthAmerica.daily.nc"
data = metR::ReadNetCDF(paste0(path,file), out='array')

data = data [[2]]
week = data[,,2:8]

semana = c("11","12","13","14","15","16","17")
anio = 1999:2015
t2m = array(NA, dim= c(66,76,7,17,4))

for (i in 1:17) {
  
  for (j in 1:4) {
    
    file = paste0("tas_sfc_GMAO-GEOS_V2p1_",anio[i],"0111.e",j,".SouthAmerica.daily.nc")
  
    data = metR::ReadNetCDF(paste0(path,file), out='array')
    
    data = data[[2]]
    week = data[,,2:8]
    
    t2m[,,,i,j] <- week
    
  }
  
  
}

# media

t2m_ens = apply(t2m, c(1,2,3,4), mean)

# promedio semanal 
t2m_sem = apply(t2m_ens,c(1,2,4),mean)

var = t2m_sem[,,1] - apply(t2m_sem, c(1,2),mean)
image.plot(var)
