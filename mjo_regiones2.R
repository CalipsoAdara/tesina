# MJO con regiones
# Analizar si las inicializaciones con MJO activos afecta a los scores segun la$
# Analisis zona SACZ y SUR PATAGONIA
# # Si resto activo - inactivo
# donde rmse sea positivo siginifica que fue mayor en activo ----> MAL
# donde acc sea positivo significa que fue mayor en activo -----> BIEN
# 
#
# by Lucia M Castro
#------------------------------------------------------------------------------

# Limpio enviroment
rm(list=ls())


# Cargo paquetes
library(secr)
library(reshape2)
library(csv)
library(dplyr)
library(data.table)
library(ggplot2)
library(metR)



# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata/MJO")

groups=c('GMAO','RSMAS','ESRL','ECCC','NRL','EMC','MME')                       
models=c('GEOS_V2p1','CCSM4','FIMr1p1','GEM','NESM','GEFS','SAT')   

# Cargo los datos de eventos
df_rmm <- readRDS("df_rmm.rds")
df_eventos <- readRDS("df_eventos.rds")
fechas_act <- as.character(df_rmm$DATE)

# Poligonos. Lon de menor a mayor, el primer punto se repite para cerrar el poligono
SP <- data.frame(x_coords = c(293,289,290,293,298,293),
                 y_coords = c(-28,-35,-50,-50,-40,-28))

SACZ <- data.frame(x_coords = c(305,305,312,319,323,305),
                   y_coords = c(-10,-20,-24,-25,-10,-10))


# Separar segun la fase inicial
# Bins = [8,1] [2,3] [4,5] [6,7]

# Debo calcular las metricas (RMSE, ACC) con solo los datos que caen en el bin de fase inicial. A estos luego se restan
# la metrica calculada para las fechas inactivas de MJO. En mjo_fases.R ya se calculo y guardaron los datos.

Bins = levels(df_eventos$Bin)
score = c("RMSE","ACC")
binsweeksacz<-data.frame()
binsweeksp <-data.frame()


for (g in 1:length(groups)) {
  grupo = groups[g]
  
  for (b in Bins) { # por cada Bin
    # Cargo los datos
    metric <- readRDS(paste0("./ScoresBins/diff",grupo,b))
   
    
    for(m in 1:length(metric)) { # Por cada metrica ( rmse y acc)
      # Convierto a data frame
      df_resta <- reshape2::melt(metric[[m]])
      
      # Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
      puntossacz=pointsInPolygon(df_resta[,1:2],SACZ) 
      puntossp=pointsInPolygon(df_resta[,1:2],SP) 
      obs_sacz = df_resta[puntossacz,]
      obs_sp = df_resta[puntossp,]
      
      # Promedio en lat y long
      med_obs_sacz = DTPromEspacPesado(obs_sacz, "value", "week")
      med_obs_sp = DTPromEspacPesado(obs_sp, "value", "week")
      
      # Agrego una columna con el nombre del modelo, con la metrica y el bin
      med_obs_sacz$model <- rep(grupo,nrow(med_obs_sacz))
      med_obs_sp$model <- rep(grupo,nrow(med_obs_sp))
      
      med_obs_sacz$metric <- rep(score[m],nrow(med_obs_sacz))
      med_obs_sp$metric <- rep(score[m],nrow(med_obs_sp))
      
      med_obs_sacz$bin <- rep(b,nrow(med_obs_sacz))
      med_obs_sp$bin <- rep(b,nrow(med_obs_sp))
      
      # Guardar info para hacer tabla (una para cada region)
      binsweeksacz<-rbind(binsweeksacz,med_obs_sacz)
      binsweeksp<-rbind(binsweeksp,med_obs_sacz)
    }
    
    
    
  }# End Bin
}# End model


# SOlo guardo semana 2 y 3 porque son las unicas que analizamos y separo lo scores
setDT(binsweeksacz)
setDT(binsweeksp)

sacz=binsweeksacz[binsweeksacz$metric %in% c("ACC","RMSE") & binsweeksacz$week %in% c('Week 2','Week 3'),]
sp=binsweeksacz[binsweeksp$metric %in% c("ACC","RMSE") & binsweeksp$week %in% c('Week 2','Week 3'),]

# ordeno para tener semana 2 primero, luego semana 3
setorder(sacz, "week","metric")  
setorder(sp, "week","metric")  
# GUARDO
saveRDS(sacz,"./ScoresBins/score_sacz.rds")
saveRDS(sp,"./ScoresBins/score_sp.rds")

