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
      media_semanal = apply(targetweek, c(1,2), FUN = mean, na.rm = FALSE)
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
dt.me <- melt(me, value.name = "z")
dt.mae <- melt(mae, value.name = "z")
dt.rmse <- melt(rmse, value.name = "z")
dt.acc <- melt(acc, value.name = "z")

#---------------------------------------------------------------------------------------
#  Gráficos  
#---------------------------------------------------------------------------------------
g1 <- GraphDiscreteMultiple(Data = dt.rmse, Breaks = seq(0,3,0.5),Label = "rsme",Paleta = "YlOrRd", Direccion = "1")
g2 <- GraphDiscreteMultiple(Data = dt.me, Breaks = seq(-0.5,0.7,0.2), Label = "me",Paleta = "RdBu",Direccion = "-1")
g3 <- GraphDiscreteMultiple(Data = dt.mae, Breaks = seq(0,3,0.5), Label = "mae",Paleta = "YlOrRd",Direccion = "1")
g4 <- GraphDiscreteMultiple(Data = dt.acc, Breaks = seq(-1,1,0.2), Label = "ACC",Paleta = "RdBu",Direccion = "-1")



fig <- grid.arrange(g1,g2,g3,g4, ncol = 1,top = textGrob("SubX GMAO-GEOS_V2p1 tasa (99-15, Oct-Mar)",gp=gpar(fontsize=13,font=3)))
ggsave(filename="/home/lucia.castro/SubX_processed_Rdata/scores_map_GMAO.png",plot=fig,width = 10, height = 11)

