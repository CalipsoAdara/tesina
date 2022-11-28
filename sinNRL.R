# Predictibilidad SIN EL MODELO NRL
# La idea es correlacionar un modelo con la media del ensamble formada por el resto de los modelos
# y luego esto repetirlo con cada modelo y sacar el promedio de esa correlación

# by Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------

# Limpiar enviroment 
rm(list=ls())

# Llamar paquetes
library(abind)
library(lubridate)
library(data.table)
library(scales)
library(ggpubr)
library("grid")

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
direc = "/home/lucia.castro/SubX_processed_Rdata/model"
svpath = "/home/lucia.castro/SubX_processed_Rdata/noNRL"

# Seteo el directorio
setwd(direc)

# Cargo los datos 
models = c("ESRL","ECCC","EMC","GMAO","RSMAS")
nmodels = length(models)
# Cargo la media del ensamble aparte
# MME
targetdateMME <- readRDS(paste0("./targetdate_MME_OA.rds"))
sabadosMME <- as.Date(dimnames(targetdateMME)$startdate)

#
MODELOS <- list()
targetdateMODELOS <- list()
startdateMODELOS <- list()

for (m in 1:nmodels) {
  MODELOS[[m]] <- readRDS(paste0("./model_",models[m],"_OA.rds"))
  targetdateMODELOS[[m]] <- readRDS(paste0("./targetdate_",models[m],"_OA.rds"))
  startdateMODELOS[[m]] <- dimnames(targetdateMODELOS[[m]])$startdate
}
# MJO
# Cargo los datos de eventos
df_rmm <- readRDS("./MJO/df_rmmOA.rds")
df_eventos <- readRDS("./MJO/df_eventosOA.rds")
fechas_act <- df_rmm$DATE

# Busco que sabados coinciden con las fechas de MJO activos
sabMJO <- as.Date(sabadosMME[sabadosMME %in% fechas_act])

# Restringo tambien el targetdate del multimodelo para fechas Activas
targetdateMMEMJO <- targetdateMME[,sabadosMME %in% fechas_act]

#-------------------------------------------------------------------------------------
# Predictibilidad MJO
#-------------------------------------------------------------------------------------------------
# Predictibilidad
# correlacionar un modelo contra la media del ensamble formada por el resto de los modelos, 
# esto repetirlo con cada modelo y sacar el promedio de esa correlación


#----------- VERSION NUEVA
predicmjo <- EnsamblesPredictiblidad(Modelos=MODELOS,
                                     TgdtMod=targetdateMODELOS, 
                                     StdtMod=startdateMODELOS, 
                                     FechEnsam = sabMJO,
                                     TgdtEnsam = targetdateMMEMJO,
                                     FilePath = paste0(svpath,"/MJO/predic/act"))
saveRDS(predicmjo , paste0(svpath,"/MJO/predic/predictmjo.rds"))
rm(predicmjo)

##-------------------------------------------------------------------------------------------------
# Predictibilidad de NO MJO
#-------------------------------------------------------------------------------------------------
# Predictibilidad
# correlacionar un modelo contra la media del ensamble formada por el resto de los modelos, 
# esto repetirlo con cada modelo y sacar el promedio de esa correlación
sabNOMJO <- as.Date(sabadosMME[!sabadosMME %in% fechas_act])


#Restringo tambien el targetdate del multimodelo para fechas Inactivas
targetdateMMENOMJO <- targetdateMME[,!sabadosMME %in% fechas_act]

predicnomjo <- EnsamblesPredictiblidad(Modelos=MODELOS,
                                     TgdtMod=targetdateMODELOS, 
                                     StdtMod=startdateMODELOS, 
                                     FechEnsam = sabNOMJO,
                                     TgdtEnsam = targetdateMMENOMJO,
                                     FilePath = paste0(svpath,"/MJO/predic/inact"))
saveRDS(predicnomjo , paste0(svpath,"/MJO/predic/predictnomjo.rds"))
rm(predicnomjo)



for (model in 1:nmodels) {
  
  # Tomo el modelo a comparar contra la EM del resto de los mod
  mod_restante_stdate = startdateMODELOS[[model]]
  mod_restante_tgdate = targetdateMODELOS[[model]]
  MODELO_restante = MODELOS[[model]]
  
  mod_ensamble_stdate = startdateMODELOS[-model]
  mod_ensamble_tgdate = targetdateMODELOS[-model]
  MODELOS_ensamble = MODELOS[-model]
  nombres_ensamble = models[-model]
  
  # array a completar 
  MME_nmenos1 <- array(NA, dim = c(66,76,28,(nmodels-1),length(sabNOMJO)))
  mod_aparte <- array(NA, dim = c(66,76,28,length(sabNOMJO)))
  
  for (i in 1:length(sabNOMJO)) { # Por cada sabado
    
    # Semana y lead en cuestion del MME
    startweek = as.character(seq.Date(sabNOMJO[i]-7,sabNOMJO[i]-1,by=1)) #desde el sabado anterior al viernes
    leadMME = targetdateMMENOMJO[,i]
    
    # MODELO RESTANTE ---------------------------------------------
    # Que startdate cae en la semana del MME para el modelo restante
    stdt_restante = mod_restante_stdate %in% startweek
    # Tomar la inicializacion mas cercana al sabado de pronostico
    if (sum(stdt_restante)>1) {stdt_restante = last(which(stdt_restante))}
    
    # Evaluo el modelo restante en las fechas que coincide con MME
    target_restante = mod_restante_tgdate[,stdt_restante] %in% leadMME
    modelo_objetivo_rest = MODELO_restante[,,target_restante,stdt_restante]
    
    # Llenar con NA si faltan dias 
    modelo_objetivo_rest = CompletarFaltante(Target = target_restante, 
                                             Stdt = stdt_restante, 
                                             ModeloObjetivo = modelo_objetivo_rest,
                                             Startweek = startweek,
                                             ModelNombre = models[model])
    
    # MEDIA ENSAMBLE N-1 MODELOS ----------------------------------
    for (mod in 1:(nmodels-1)) { # por cada modelo
      # Que startdate cae en la semana del MME para cada modelo
      stdt = mod_ensamble_stdate[[mod]] %in% startweek
      
      # Tomar la inicializacion mas cercana al sabado de pronostico
      if (sum(stdt)>1) {    # Hay mas de un inicio en la semana
        stdt = last(which(stdt))}
      
      # Quiero saber que lead hace que coincida el targetdate del modelo con el targetdate del MME 
      leadMODELO = mod_ensamble_tgdate[[mod]]
      target = leadMODELO[,stdt] %in% leadMME
      
      # Evaluo el modelo en esas fechas
      modelo = MODELOS_ensamble[[mod]]
      modelo_objetivo = modelo[,,target,stdt]
      
      # Si el modelo no alcanza a llenar los 28 dias del MME, llenar el resto con NA
      modelo_objetivo = CompletarFaltante(target, stdt, modelo_objetivo,Startweek = startweek,
                                          ModelNombre = nombres_ensamble[mod])
      
      
      # Guardo
      MME_nmenos1[,,,mod,i] <- modelo_objetivo
      mod_aparte[,,,i] <- modelo_objetivo_rest
      
    } # End loop media ensamble
    
  } # End loop sabados  
  # Promedio sobre los modelos (cuarta dimension)
  MME_pro = apply(MME_nmenos1 , c(1,2,3,5), mean, na.rm = T)
  print(paste("termino el promedio sabados del modelo",models[model]))
  
  cor_mod <- Predictibilidad(mod_aparte, MME_pro)
  saveRDS(cor_mod, paste0(svpath,"/MJO/predic/predicNOMJO_",models[model]))
  print(paste("termino la correlacion del modelo",models[model]))
  
  # ELimino para hacer espacio
  rm(cor_mod)
  
} # End loop models


# Ahora promedio todas las correlaciones obtenidad de cada modelo (cuarta dimension)
# Guarde todas las correlaciones por separado para que sea menos pesado y ahora promedio

cor_mod <- array(NA, dim = c(66,76,28,nmodels))
for (m in nmodels) {
  cor_mod[,,,model] <- readRDS(paste0(svpath,"/MJO/predic/predicNOMJO_",models[m]))
}

predictibilidad = apply(cor_mod , c(1,2,3), mean, na.rm = T)

# Guardo
saveRDS(predictibilidad, paste0(svpath,"/MJO/predic/predictnomjo.rds"))




# G R A F I C O S ------------------------------------
#----------------------------------------------------------------------------------------
# Ahora grafico la diferencia entre la predictibilidad total y la predictiblidad 
# de solo los eventos activos de MJO
##
# SI ES TOTAL - ACT DE MJO DONDE SEA NEGATIVO ------> APORTA MJO
# SI ES ACT - TOTAL DONDE SEA POSITIVO ----------> APORTA MJO

# cargo predictibilidad total
predtotal <- readRDS("../noNRL/MJO/predic/predictnomjo.rds")
predMJO <- readRDS("../noNRL/MJO/predic/predictmjo.rds")

pred_diff <- predMJO - predtotal

# convierto a data frame para las 4 weeks
dimnames(pred_diff) <- list("x" = seq(265,330,1), "y" = rev(seq(-60,15,1)),
                            "week" = c(rep("Week 1",7),
                                       rep("Week 2",7),
                                       rep("Week 3",7),
                                       rep("Week 4",7)))
df <- reshape2::melt(pred_diff)
colnames(df) <- c("x","y","week","z")

g<-GraphDiscreteMultiple(Data=df,Breaks = seq(-0.2,0.2,0.05),Label = "ACC",Paleta = "RdBu",Direccion = -1)
g <- g + ggtitle(paste0("Predictibilidad MJO ACT - INACT \nT2MA (99-14, Oct-Apr)"))

ggsave(filename = "../noNRL/MJO/predic/predic_resta_actinactNRL.png",plot=g,width = 10, height = 4)

#-------------------------------------------------------------------------------------------
# PREDICTIBILILDAD TOTAL
#-------------------------------------------------------------------------------------------------
# Predictibilidad
# correlacionar un modelo contra la media del ensamble formada por el resto de los modelos, 
# esto repetirlo con cada modelo y sacar el promedio de esa correlación

predictibilidad = EnsamblesPredictiblidad(Modelos=MODELOS,
                                       TgdtMod=targetdateMODELOS, 
                                       StdtMod=startdateMODELOS, 
                                       FechEnsam = sabadosMME,
                                       TgdtEnsam = targetdateMME,
                                       FilePath = "../noNRL/predic")


# Guardo
saveRDS(predictibilidad, paste0(svpath,"/predic/predict.rds"))
#------------------------------ ----------------------------------------------------------
# Ahora con fechas extremas yaay
# Lo importante es que las fechas del ensamble (MME) sean lo mas cercanas a las fechas extremas


# Cargo datos de fechas extremas
ext <- read.csv("./ext.csv",stringsAsFactors = F)
#ext <- read.csv("./extMME.csv",stringsAsFactors = F)
colnombre <- c("TOTAL90","TOTAL10")

# Discrimino fechas no extremas (ni p10 ni p90)
#Busco las fehcas extremas
extrema = BuscarFechaExtrema(Ext = ext, Columna = colnombre,Startdate = sabadosMME)
# Remuevo posiciones repetidas y ordeno de menor a mayor
extrema = sort(unique(extrema))

# Eligo solo las fechas no extremas y hago su predictibilidad, solo una vez
stdtMME_noext = sabadosMME[-extrema]
tgdtMME_noext = targetdateMME[,-extrema]

predic_noext = EnsamblesPredictiblidad(Modelos=MODELOS,
                                       TgdtMod=targetdateMODELOS, 
                                       StdtMod=startdateMODELOS, 
                                       FechEnsam = stdtMME_noext,
                                       TgdtEnsam = tgdtMME_noext,
                                       FilePath = "./noNRL/ext")
#saveRDS(predic_noext,"./ext/predic_noext.rds")
saveRDS(predic_noext,paste0(svpath,"/ext/predic_noext.rds"))

for (p in colnombre) {
  
  # Busco las fehcas extremas
  extrema = BuscarFechaExtrema(Ext = ext, Columna = p,Startdate = sabadosMME)
  # Remuevo posiciones repetidas y ordeno de menor a mayor
  extrema = sort(unique(extrema))
  
  # Evaluo en mme para separar en fechas extrema y no extremas
  stdtMME_ext = sabadosMME[extrema]
  tgdtMME_ext = targetdateMME[,extrema]
  
  #Predictibilidad
  predic_ext = EnsamblesPredictiblidad(Modelos=MODELOS,
                                       TgdtMod=targetdateMODELOS, 
                                       StdtMod=startdateMODELOS, 
                                       FechEnsam = stdtMME_ext,
                                       TgdtEnsam = tgdtMME_ext,
                                       FilePath = "./noNRL/ext")
  
  #predic_noext = readRDS("./ext/predic_noext.rds")
  
  # Guardo
  indice = substr(p,6,7)
  saveRDS(predic_ext, paste0(svpath,"/ext/predict_ext",indice,".rds"))
  
}
# G R A F I C O S ------------------------------------

# si restas
# ext - no ext ------> positivo aporta mas las fechas ext

for (i in 1:length(colnombre)) {
  
  indice = colnombre[i]
  p = substr(indice,6,7)
  # Cargo los datos para la resta
  pred_ext <- readRDS(paste0("./noNRL/ext/predict_ext",p,".rds"))
  pred_noext <- readRDS("./noNRL/ext/predic_noext.rds")
  
  # Resta
  resta <- pred_ext - pred_noext
  
  # convierto a data frame para las 4 weeks
  dimnames(resta) <- list("x" = seq(265,330,1), "y" = rev(seq(-60,15,1)),
                          "week" = c(rep("Week 1",7),
                                     rep("Week 2",7),
                                     rep("Week 3",7),
                                     rep("Week 4",7)))
  df <- reshape2::melt(resta)
  colnames(df) <- c("x","y","week","z")
  g<-GraphDiscreteMultiple(Data=df,Breaks = seq(-0.4,0.4,0.1),Label = "ACC",Paleta = "RdBu",Direccion = -1)
  g<-g + ggtitle(paste0("Predictibilidad \n P",p," EXT-NO EXT \nT2MA (99-14, Oct-Mar)"))
  
  ggsave(filename = paste0("./noNRL/ext/predic_resta_p",p,".png"),plot=g,width = 10, height = 4)
  
}

# G R A F I C O S ------------------------------------
# total
predic <- readRDS("./noNRL/predic/predict.rds")

# convierto a data frame para las 4 weeks
dimnames(predic) <- list("x" = seq(265,330,1), "y" = rev(seq(-60,15,1)),
                         "week" = c(rep("Week 1",7),
                                    rep("Week 2",7),
                                    rep("Week 3",7),
                                    rep("Week 4",7)))
df <- reshape2::melt(predic)
colnames(df) <- c("x","y","week","z")
g<-GraphDiscreteMultiple(Data=df,Breaks = seq(0,1,0.2),Label = "ACC",Paleta = "Greens",Direccion = 1)
g <- g + ggtitle(paste0("Predictibilidad  \nT2MA (99-14, Oct-Mar)"))

ggsave(filename = "./noNRL/predic/predic.png",plot=g,width = 10, height = 4)
