# SubX data - t2m anomaly verification for extreme dates
# Region and extreme analysis
# M. Alvarez - 2020
# Modified by Lucia M Castro
#-----------------------------------------------------------------------------------------------------------------------
rm(list=ls())

setwd("/home/lucia.castro/")

# Call libraries to be used
library("metR")
library('pracma')
library('lubridate')
library('reshape2')
library('data.table')
library("RColorBrewer")
library("maps")
library("ggplot2")
library("gridExtra")
library("grid")
library(csv)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

#--------------------------------------------------------------------------------------------------
BuscarFechaExtrema <- function(Ext, Region, Startdate){
  ## Ext : Data frame. Tabla con las fechas extremas separadas las regiones en columnas
  ## Region: Character. Alguna de las regiones del datatable ext. Ej: "SACZ", "SP"
  ## Startdate: Vector con las fechas de inicializacion del modelo
  
  # Fuerzo a data table
  ext = as.data.table(Ext)
  
  # Nombre de las columnas para llamar
  columna = c(paste0(Region,10), paste0(Region,90))
  
  # Crea data table con las fechas extremas de la region y otro con
  # las fechas de inicio totales
  fecha_region = data.table("region" = as.Date(c(ext[,get(columna[1])],ext[,get(columna[2])])))
  inicios = data.table("ini" = as.Date(Startdate)) 
  
  # Encontrar las posiciones de las fechas extremas mas cercanas, ya que 
  # no soy exactamente iguales
  pos_extrema = inicios[fecha_region, on = .(ini = region),roll = "nearest", which = T]
  
  return(pos_extrema)
  
}
#--------------------------------------------------------------------------------------
#  Main Program  
#---------------------------------------------------------------------------------------
ext <- read.csv("./SubX_processed_Rdata/ext.csv",stringsAsFactors = F)

# Los nombres 
grupos = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
modelname = c('FIMr1p1','GEM','GEFS','GEOS_V2p1','CCSM4','NESM',"SAT")
nmodels = length(grupos) 
regiones = c("SACZ","SP")

# Variable a llenar. 4 semanas y 4 scores para guardar
tabla = array(NA, dim = c(4,4,length(regiones), length(grupos)))

for (mod in 1:nmodels) {
  
  # Leer datos
  model = grupos[mod]
  ar.model = readRDS(paste0("./SubX_processed_Rdata/modelweek_",model, ".rds"))
  ar.anom = readRDS(paste0("./SubX_processed_Rdata/obsweek_",model, ".rds"))
  
  # La cantidad de fechas de pronosticos desde Oct a Mar en el periodo del modelo
  fechas_pronosticos <- dim(ar.model)[3]
  
  # Buscar posiciones de las fechas extremas para c/region
  for (zona in 1:length(regiones)) {
    
    inicios = dimnames(ar.model)$start
    extrema = BuscarFechaExtrema(Ext = ext, Region = regiones[zona], Startdate = inicios)
    
    model_media_semanal = ar.model[,,extrema,]
    anom_media_semanal = ar.anom[,,extrema,]
    
    # Calculo las distintas métricas por cada lon/lat/targetweek
    dif = (anom_media_semanal - model_media_semanal)
    me = apply(dif, c(1,2,4),FUN = mean, na.rm = TRUE)
    mae = apply(abs(dif), c(1,2,4), FUN = mean, na.rm = TRUE) 
    rmse = sqrt(apply(dif^2,c(1,2,4), FUN = mean, na.rm = TRUE))
    desvio = apply(dif,c(1,2,4),FUN = sd, na.rm = TRUE)
    var = (1-sqrt(rmse))/desvio
    
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
    
    # Ahora promedio espacialmente en longitud y latitud
    me_prom <- apply(me, 3, FUN = mean, na.rm = T)
    rmse_prom <- apply(rmse, 3, FUN = mean, na.rm = T)
    var_prom <- apply(var, 3, FUN = mean, na.rm = T)
    acc_prom <- apply(acc, 3, FUN = mean, na.rm = T)
    
    
    scores <- array(c(me_prom, rmse_prom, var_prom, acc_prom), dim =c(4,4))
    
    # Guardo
    tabla[,,zona,mod] <- scores
    
    
    
  } # End region
  
} # End loop modelos

# Transformo el array en data table 
tabla
dimnames(tabla) <- list(week = c("w1","w2","w3","w4"),
                       score =c("BIAS","RMSE", "NRMSE","ACC"),
                       region = regiones,
                       modelo = grupos)

# Tomar la primera semana, una region y todos los modelos
regionw1 <- cbind.data.frame(t(tabla[1,,1,]),t(tabla[1,,2,]))
regionw2 <- cbind.data.frame(t(tabla[2,,1,]),t(tabla[2,,2,]))
regionw3 <- cbind.data.frame(t(tabla[3,,1,]),t(tabla[3,,2,]))
regionw4 <- cbind.data.frame(t(tabla[4,,1,]),t(tabla[4,,2,]))


# Creo archivo csv 
write.csv(rbind(regionw1,regionw2, regionw3, regionw4), "./SubX_processed_Rdata/ext_mod.csv")
