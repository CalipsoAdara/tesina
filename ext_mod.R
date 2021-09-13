# SubX data - t2m anomaly verification for extreme dates
# Region and extreme analysis
# M. Alvarez - 2020
# Modified by Lucia M Castro
#-----------------------------------------------------------------------------------------------------------------------
rm(list=ls())

setwd("/home/lucia.castro/")

# Call libraries to be used
library("metR")
#library('pracma')
#library('lubridate')
#library('reshape2')
library('data.table')
#library("RColorBrewer")
library("maps")
library("ggplot2")
library("gridExtra")
library("grid")
library(csv)
library(dplyr)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

#--------------------------------------------------------------------------------------------------
BuscarFechaExtrema <- function(Ext, Startdate, Columna){
  ## Ext : Data frame. Tabla con las fechas extremas separadas las regiones en columnas
  ## Startdate: Vector con las fechas de inicializacion del modelo
  ## Columna: Character vector. Columna o Columnas deseadas del data frame EXT
  
  # Fuerzo a data table
  ext = as.data.table(Ext)
  
  # Crea data table con las fechas de inicio totales
  inicios = data.table("ini" = as.Date(Startdate)) 
  
  # Crea data table con las fechas extremas de la region
  vecfechas <- c()
  for (n in 1:length(Columna)) {
    fecha_region = ext[,get(Columna[n])]
    vecfechas = append(vecfechas,fecha_region)
  }
  dt.fechas = data.table("region" = as.Date(vecfechas))
  
  # Encontrar las posiciones de las fechas extremas mas cercanas, ya que 
  # no son exactamente iguales
  pos_extrema = inicios[dt.fechas, on = .(ini = region),roll = "nearest", which = T]
  
  return(pos_extrema)
  
}
#------------------------------------------------------------------------------------------
# Funcion que calcula el coeficiente de correlacion entre dos arrays de mismas dimensiones 
# punto a punto 
ACC <- function(Lon,Lat,Model,Anom){
  ## Lon: Numeric. Cantidad de puntos longitudinales
  ## Lat: Numeric. Cantidad de puntos latitudinales
  ## Model: Array de cuatro dimensiones (lon,lat,startdate,week)
  ## Anom: Array de cuatro dimensiones (lon,lat,startdate,week)
  # Para el calculo de ACC hago una vuelta mas, para recorrer todos los puntos y obtener un valor de correlacion
  acc = array(NA, dim = c(Lon,Lat,4))
  for (week in 1:4) {
    for (lon in 1:Lon) {
      for (lat in 1:Lat) {
        
        # Me quedo solo con una semana a analizar y todos las fechas de pronostico
        anom.week <- Anom[,,,week]
        model.week <- Model[,,,week]
        # Me quedo solo con un punto particular y todas las fechas de pronostico
        observ <- anom.week[lon,lat,]
        modelo <- model.week[lon,lat,]
        
        coef_corr <- cor(observ,modelo,use="pairwise.complete.obs",method = "pearson")
        
        acc[lon,lat,week] <- coef_corr
        
      } # End loop lat
      
    }  # End loop lon
    
  } # End loop week
  return(acc)
}
#---------------------------------------------------------------------------------------
#  functions of verification metrics 
#---------------------------------------------------------------------------------------

domae <- function(obs, fcst){
  mae=mean(abs(obs-fcst),na.rm = T)
  return(mae)
}

dome <- function(obs, fcst){
  me=mean(obs-fcst,na.rm = T)
  return(me)
}

dormse <- function(obs, fcst){
  rmse=sqrt(mean((obs-fcst)^2,na.rm = T))
  return(rmse)
}

doacc <- function(obs, fcst){
  acc=cor(obs, fcst, use = "pairwise.complete.obs",method = "pearson")
  return(acc)
}

donrmse <- function(obs, fcst){
  rmse=sqrt(mean((obs-fcst)^2,na.rm = T))
  desvio=sd((obs-fcst),na.rm = T) 
  nrmse=(1-sqrt(rmse))/desvio
  return(nrmse)
}
#----------------------------------------------------------------------------------------
# Funcion que encuentra los puntos dentro de un poligono delimitado en un data frame
# Funciona todo con df, devuelve uno
PuntoDentroPoligono <- function(Poli,Data) {
  ## Poli: Data frame del poligono
  ## Data: Array con la informacion 
  library(secr)
  
  # Convierto en Data frame 
  df = reshape2::melt(Data)
  
  # Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
  puntospoli=pointsInPolygon(df[,1:2],Poli) 
  df_poli = df[puntospoli,]
  
  return(df_poli)
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

# Poligonos. Lon de menor a mayor, el primer punto se repite para cerrar el poligono
SP <- data.frame(x_coords = c(291,288,291,298,291),
                 y_coords = c(-30,-40,-53,-40,-30))

SACZ <- data.frame(x_coords = c(305,305,310,321,305),
                   y_coords = c(-10,-25,-30,-10,-10))

poli = list(SACZ, SP)

# Variable a llenar. 4 semanas y 4 scores para guardar
tabla = array(NA, dim = c(4,4,length(regiones), nmodels))

for (mod in 1:nmodels) {
  
  # Leer datos
  model = grupos[mod]
  ar.model = readRDS(paste0("./SubX_processed_Rdata/modelweek_",model, ".rds"))
  ar.anom = readRDS(paste0("./SubX_processed_Rdata/obsweek_",model, ".rds"))
  
  # La cantidad de fechas de pronosticos desde Oct a Mar en el periodo del modelo
  fechas_pronosticos <- dim(ar.model)[3]
  inicios = as.Date( dimnames(ar.model)$start )
  
  # Buscar posiciones de las fechas extremas para c/region
  for (zona in 1:length(regiones)) {
    
    # RESTRIGIR SEMANAS INDICADAS ------------------------------------
    colname = c(paste0(regiones[zona],"10"))
    #colname = c(paste0(regiones[zona],"10"))
    extrema = BuscarFechaExtrema(Ext = ext, Columna = colname, Startdate = inicios)
    
    # Remuevo posiciones repetidas y ordeno de menor a mayor
    extrema = sort(unique(extrema))
    # ar.model.ext = ar.model[,,-extrema,]
    # ar.anom.ext = ar.anom[,,-extrema,]
    ar.model.ext = ar.model[,,extrema,]
    ar.anom.ext = ar.anom[,,extrema,]
    #model_media_semanal = ar.model
    #anom_media_semanal = ar.anom
    
    # RESTRIGIR ZONA INDICADA ----------------------------------------
    df.model = PuntoDentroPoligono(Poli = poli[[zona]], Data = ar.model.ext)
    df.anom = PuntoDentroPoligono(Poli = poli[[zona]], Data = ar.anom.ext)
    
    # Junto obs y model en un solo dataframe
    df.model$obs = df.anom$value
    setnames(df.model,"value","model")
    
    # Calculo las distintas métricas por cada lon/lat/targetweek
    dt = as.data.table(df.model)

    ## Promedia todas las fechas de pronosticos
    dt.me=dt[,list(me.w=dome(obs,model)),by=.(lat,lon,week)]
    dt.rmse=dt[,list(rmse.w=dormse(obs,model)),by=.(lat,lon,week)]
    dt.nrmse = dt[,list(nrmse.w=donrmse(obs,model)),by=.(lat,lon,week)]
    dt.acc = dt[,list(acc.w=doacc(obs,model)),by=.(lat,lon,week)]
    
    MergedDT = Reduce(function(x,y) merge(x = x, y = y, by = c("lat","lon","week")), 
           list(dt.me,dt.nrmse,dt.acc,dt.rmse))
    
    # Ahora promedio espacialmente en longitud y latitud
    cols = c("me.w","nrmse.w","acc.w","rmse.w")
    score = MergedDT[, sapply(.SD, function(x) list(mean = mean(x,na.rm=T)))
                     , .SDcols = cols, by=week]
    

    

    
    # Vuelvo a array
    region.lon = unique(df.model$lon)
    region.lat = unique(df.model$lat)
    model_media_semanal = array(data = df.model$value, dim = c(length(region.lon),
                                                             length(region.lat),
                                                             dim(ar.model.ext)[3],
                                                             4))
    anom_media_semanal = array(data = df.anom$value, dim = c(length(region.lon),
                                                               length(region.lat),
                                                               dim(ar.anom.ext)[3],
                                                               4))
    
    # Calculo las distintas métricas por cada lon/lat/targetweek
    dif = (anom_media_semanal - model_media_semanal)
    me = apply(dif, c(1,2,4),FUN = mean, na.rm = TRUE)
    mae = apply(abs(dif), c(1,2,4), FUN = mean, na.rm = TRUE) 
    rmse = sqrt(apply(dif^2,c(1,2,4), FUN = mean, na.rm = TRUE))
    desvio = apply(dif,c(1,2,4),FUN = sd, na.rm = TRUE)
    var = (1-sqrt(rmse))/desvio
    
    acc = ACC(Lon = length(region.lon),
              Lat = length(region.lat), 
              Model = model_media_semanal, 
              Anom = anom_media_semanal)
 
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
write.csv(rbind(regionw1,regionw2, regionw3, regionw4), "./SubX_processed_Rdata/ext_10.csv")





# Funcion que encuentra los puntos dentro de un poligono delimitado en un data frame
# Funciona todo con df, devuelve uno
PuntoDentroPoligono <- function(Poli,Data) {
  ## Poli: Data frame del poligono
  ## Data: Array con la informacion 
  
  # Convierto en Data frame 
  df = melt(Data)
  
  # Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
  puntospoli=pointsInPolygon(df[,1:2],Poli) 
  df_poli = df[puntospoli,]
  
  return(df_poli)
}


ACC <- function(Lon,Lat,Model,Anom){
  ## Lon: Numeric. Cantidad de puntos longitudinales
  ## Lat: Numeric. Cantidad de puntos latitudinales
  ## Model: Array de cuatro dimensiones (lon,lat,startdate,week)
  ## Anom: Array de cuatro dimensiones (lon,lat,startdate,week)
  # Para el calculo de ACC hago una vuelta mas, para recorrer todos los puntos y obtener un valor de correlacion
  acc <- array(NA, dim = c(Lon,Lat,4))
  for (week in 1:4) {
    for (lon in 1:Lon) {
      for (lat in 1:Lat) {
        
        # Me quedo solo con una semana a analizar y todos las fechas de pronostico
        anom.week <- Anom[,,,week]
        model.week <- Model[,,,week]
        # Me quedo solo con un punto particular y todas las fechas de pronostico
        observ <- anom.week[lon,lat,]
        modelo <- model.week[lon,lat,]
        
        coef_corr <- cor(observ,modelo,use="pairwise.complete.obs",method = "pearson")
        
        acc[lon,lat,week] <- coef_corr
        
      } # End loop lat
      
    }  # End loop lon
    
  } # End loop week
  return(acc)
}
