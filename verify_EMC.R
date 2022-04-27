# SubX data - t2m anomaly verification for EMC model (SubX database)
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
ar.model = readRDS("/home/lucia.castro/SubX_processed_Rdata/model_EMC_ONDEFM.rds")
targetdate = readRDS("/home/lucia.castro/SubX_processed_Rdata/targetdate_EMC_ONDEFM.rds")
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
desvio = apply(dif,c(1,2,4),FUN = sd, na.rm = TRUE)
var = 1-(rmse/desvio)

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

#-------------------------------------------------
# Busco el rho1 para calcular el tamaño de muestra efectivo y obtener significancia
# Autocorrelacion con lag 1 en las observaciones
rho1 = readRDS(file = "./SubX_processed_Rdata/rho1.rds")

# Calculo estadistico de prueba

# RHO1 sirve para todas las weeks (1,2.3 y 4). Repito el tamaño de muestra para cada semana
n_eff = fechas_pronosticos*((1 - rho1)/(1 + rho1))
n_eff = array(n_eff, dim = c(66,76,4))
t = (acc * sqrt(n_eff - 2)) / sqrt(1-acc^2)

# Significancia de 0.05 y grados de libertad 
critc = qt(p=0.95, df = trunc(n_eff))
test = t < critc


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
dimnames(var) <- list(x = dimnames(ar.anom)$lon, 
                      y = dimnames(ar.anom)$lat, 
                      week = c("Week 1", "Week 2","Week 3", "Week 4"))

# Armo data.frames para graficar
dt.me <- reshape2::melt(me, value.name = "z")
dt.mae <- reshape2::melt(mae, value.name = "z")
dt.rmse <- reshape2::melt(rmse, value.name = "z")
dt.acc <- reshape2::melt(acc, value.name = "z")
dt.var <- reshape2::melt(var, value.name = "z")


#---------------------------------------------------------------------------------------
#  Gráficos  
#---------------------------------------------------------------------------------------
g1 <- GraphDiscreteMultiple(Data = dt.rmse, Breaks = seq(0,3,0.25),Label = "RMSE",Paleta = "YlOrRd", Direccion = "1")
g2 <- GraphDiscreteMultiple(Data = dt.me, Breaks = seq(-0.1,0.1,0.025), Label = "ME",Paleta = "RdBu",Direccion = "-1")
g3 <- GraphMultiplePuntos(Data = dt.acc, ArLogic = test, Breaks = seq(0,1,0.20), Label = "ACC",Paleta = "YlGn",Direccion = "1")
g4 <- GraphDiscreteMultiple(Data = dt.var, Breaks = seq(-0.5,0.5,0.10), Label = "NRMSE",Paleta = "RdBu",Direccion = "-1")


fig <- grid.arrange(g1,g2,g3,g4, ncol = 1,top = textGrob("SubX EMC-GEFS tasa (99-14, Oct-Mar)",gp=gpar(fontsize=13,font=3)))
ggsave(filename="/home/lucia.castro/SubX_processed_Rdata/scores_map_EMC.png",plot=fig,width = 10, height = 11)

#---------------------------------------------------------------------------------------
#  Guardado de datos
#---------------------------------------------------------------------------------------
lon = dimnames(ar.model)$X
lat = dimnames(ar.model)$Y
fechas = dimnames(ar.model)$startdate
dimnames(model_media_semanal) <- list("lon" = lon,"lat" = lat, "start" = fechas, 
                                      "week" = c("Week 1","Week 2","Week 3","Week 4"))

dimnames(anom_media_semanal) <- list("lon" = lon,"lat" = lat, "start" = fechas, 
                                     "week" = c("Week 1","Week 2","Week 3","Week 4"))
metrics <- list(rmse,me,acc,var)
saveRDS(model_media_semanal, paste0("./SubX_processed_Rdata/modelweek_EMC.rds"))
saveRDS(anom_media_semanal, paste0("./SubX_processed_Rdata/obsweek_EMC.rds"))
saveRDS(metrics, "./SubX_processed_Rdata/metrics_EMC.rds")