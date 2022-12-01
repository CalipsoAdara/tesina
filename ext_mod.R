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
#------------------------------------------------------------------------------------------
# Funcion que pega un asterisco al lado del numero cuando se cumple una condicion logica
EsSignificativo <- function(Cor,Test) {
  ## Cor: Numeric. Vector con los valores a testear
  ## Test: Logic. Vector del mismo largo que Cor que indique TRUE donde se quiera el asterisco
  
  Cor_as = ifelse(test, yes = paste0(Cor,"*"), no = Cor)
  return(Cor_as)
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
# ext <- read.csv("./SubX_processed_Rdata/modeL/extMME/extMME.csv",stringsAsFactors = F)

# Los nombres 
grupos = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
modelname = c('FIMr1p1','GEM','GEFS','GEOS_V2p1','CCSM4','NESM',"")
nmodels = length(grupos) 
regiones = c("SACZ","SP")

# Poligonos. Lon de menor a mayor, el primer punto se repite para cerrar el poligono
SP <- data.frame(x_coords = c(291,288,291,298,291),
                 y_coords = c(-30,-40,-53,-40,-30))

SACZ <- data.frame(x_coords = c(305,305,310,321,305),
                   y_coords = c(-10,-25,-30,-10,-10))

poli = list(SACZ, SP)

# Rho1 para calcular la muestra efectiva y la significancia de ACC
rho1 <- readRDS(file = "./SubX_processed_Rdata/rho1.rds")

# Variable a llenar. 4 semanas y 4 scores para guardar
tabla = array(NA, dim = c(4,4,length(regiones), nmodels))
tablan = array(NA, dim = c(length(regiones),nmodels))

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
    #colname = c(paste0(regiones[zona],"10"),paste0(regiones[zona],"90"))
    colname = c(paste0(regiones[zona],"10"))
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
    
    df.rho1 = PuntoDentroPoligono(Poli = poli[[zona]], Data = rho1)
    
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
    
    # Uno todos los scores en un solo data table
    MergedDT = Reduce(function(x,y) merge(x = x, y = y, by = c("lat","lon","week")), 
           list(dt.me,dt.rmse,dt.nrmse,dt.acc))
    
    # Peso los scores por el coseno de la latitud
    MergedDT[, c("me.lat","rmse.lat","nrmse.lat","acc.lat") := lapply(.SD, function(x) x*cos(lat*pi/180)), 
          .SDcols=c("me.w","rmse.w","nrmse.w","acc.w")]
    MergedDT[, c("me.w","rmse.w","nrmse.w","acc.w") := NULL]
    
    # Lo mismo para el rho1
    df.rho1$varlat = df.rho1$value * cos(df.rho1$lat*pi/180)
  
    # Ahora promedio espacialmente en longitud y latitud
    cols = c("me.lat","rmse.lat","nrmse.lat","acc.lat")
    scores = MergedDT[, sapply(.SD, function(x) list(mean = mean(x,na.rm=T)))
                     , .SDcols = cols, by=week]
    
    # -------- ESTADISTICO DE PRUEBA
    # Lo mismo para el rho1
    df.rho1.mean = mean(df.rho1$varlat,na.rm=T)
    
    # RHO1 sirve para todas las weeks (1,2.3 y 4). Repito el tamaño de muestra para cada semana
    n = dim(ar.model.ext)[3]
    acc = scores$acc.lat.mean
    n_eff = n*((1 - df.rho1.mean)/(1 + df.rho1.mean))
    t = (acc * sqrt(n_eff - 2)) / sqrt(1-acc^2)
    
    # Significancia de 0.05 y grados de libertad 
    critc = qt(p=0.95, df = trunc(n_eff))
    test = t > critc
    
    scores$acc.lat.mean <- EsSignificativo(Cor = acc, Test = test)
    
    # -------- ESTADISTICO DE PRUEBA

    # Convierto a matrix (sin la primer columna de week)
    scores.ar <-as.matrix(scores[,-1])
    
    # Guardo
    tabla[,,zona,mod] <- scores.ar
    tablan[zona,mod] <- n
    
    
    
  } # End region
  
} # End loop modelos


# Transformo el array en data table 

dimnames(tabla) <- list(week = c("w1","w2","w3","w4"),
                       score =c("BIAS","RMSE", "NRMSE","ACC"),
                       region = regiones,
                       modelo = grupos)
dimnames(tablan) <- list(region = regiones,
                         modelo = grupos)

# Tomar la primera semana, una region y todos los modelos
regionw1 <- cbind.data.frame(t(tabla[1,,1,]),t(tabla[1,,2,]))
regionw2 <- cbind.data.frame(t(tabla[2,,1,]),t(tabla[2,,2,]))
regionw3 <- cbind.data.frame(t(tabla[3,,1,]),t(tabla[3,,2,]))
regionw4 <- cbind.data.frame(t(tabla[4,,1,]),t(tabla[4,,2,]))


# Creo archivo csv 
write.csv(rbind(regionw1,regionw2, regionw3, regionw4), "./SubX_processed_Rdata/ext_9010.csv")
write.csv(tablan,"./SubX_processed_Rdata/tablan_10.csv")




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

#------------------------------------------------------------------------------------------------
# D I F E R E N C I A  EN  S C O R E S 
#
# aqui no promedio espacialmente

# Path a donde guardar los archivos
ext <- read.csv("./extMME/extMME.csv",stringsAsFactors = F)
savepath = "/home/lucia.castro/SubX_processed_Rdata/model"

# Seteo el directorio
setwd(savepath)

# Calculo rmse y acc para el caso no extremo y extremo por separado, luego resto

colname = c("TOTAL10","TOTAL90")
label = c("10","90")

for (mod in 1:nmodels) { # por cada modelo
  
  # Leer datos
  model = grupos[mod]
  ar.model = readRDS(paste0("./modelweek_",model, ".rds"))
  ar.anom = readRDS(paste0("./obsweek_",model, ".rds"))
  inicios = as.Date(dimnames(ar.model)$start)
  
  for (p in 1:length(colname)) { # por cada percentil
    
    # Busco posiciones de fechas extrema
    extrema = BuscarFechaExtrema(Ext = ext, Columna = colname[p], Startdate = inicios)
    
    # Remuevo posiciones repetidas y ordeno de menor a mayor
    extrema = sort(unique(extrema))
    
    ar.model.noext = ar.model[,,-extrema,]
    ar.anom.noext = ar.anom[,,-extrema,]
    ar.model.ext = ar.model[,,extrema,]
    ar.anom.ext = ar.anom[,,extrema,]
    
    #--- METRICAS---------
    # obs - pro
    metext = Metrics(ar.anom.ext,ar.model.ext)
    metnoext = Metrics(ar.anom.noext,ar.model.noext)
    
    # guardo
    saveRDS(metext,paste0("./extMME/scoresext_",model,label[p]))
    saveRDS(metnoext,paste0("./extMME/scoresnoext_",model,label[p]))
    
  } # end loop percentil
  
} # end loop models
