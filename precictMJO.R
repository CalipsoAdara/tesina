# Predictibilidad para MJO activos o inactivos
#
# by Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------

# Limpiar enviroment 
rm(list=ls())

# Call libraries to be used
library(abind)
library(lubridate)
library(data.table)
library(scales)
library(ggpubr)
library("grid")

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/model"

# Seteo el directorio
setwd(savepath)

# Cargo los datos 
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL")
nmodels = length(models)

MODELOS <- list()
targetdateMODELOS <- list()
startdateMODELOS <- list()

for (m in 1:nmodels) {
  MODELOS[[m]] <- readRDS(paste0("./model_",models[m],"_OA.rds"))
  targetdateMODELOS[[m]] <- readRDS(paste0("./targetdate_",models[m],"_OA.rds"))
  startdateMODELOS[[m]] <- dimnames(targetdateMODELOS[[m]])$startdate
}

# MME
targetdateMME <- readRDS(paste0("./targetdate_MME_OA.rds"))
sabadosMME <- dimnames(targetdateMME)$startdate

# MJO
# Cargo los datos de eventos
df_rmm <- readRDS("./MJO/df_rmmOA.rds")

df_eventos <- readRDS("./MJO/df_eventosOA.rds")
fechas_act <- as.character(df_rmm$DATE)

# Busco que sabados coinciden con las fechas de MJO activos
sabMJO <- as.Date(sabadosMME[sabadosMME %in% fechas_act])

# Restringo tambien el targetdate del multimodelo para fechas Activas
targetdateMMEMJO <- targetdateMME[,sabadosMME %in% fechas_act]

#-------------------------------------------------------------------------------------------------
# Predictibilidad
# correlacionar un modelo contra la media del ensamble formada por el resto de los modelos, 
# esto repetirlo con cada modelo y sacar el promedio de esa correlación


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
  MME_nmenos1 <- array(NA, dim = c(66,76,28,(nmodels-1),length(sabMJO)))
  mod_aparte <- array(NA, dim = c(66,76,28,length(sabMJO)))
  
  for (i in 1:length(sabMJO)) { # Por cada sabado
    
    # Semana y lead en cuestion del MME
    startweek = as.character(seq.Date(sabMJO[i]-7,sabMJO[i]-1,by=1)) #desde el sabado anterior al viernes
    leadMME = targetdateMMEMJO[,i]
    
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
  saveRDS(cor_mod, paste0("./MJO/predic/predic_",models[model]))
  print(paste("termino la correlacion del modelo",models[model]))
  
  # ELimino para hacer espacio
  rm(cor_mod)
  
} # End loop models

# Ahora promedio todas las correlaciones obtenidad de cada modelo (cuarta dimension)
# Guarde todas las correlaciones por separado para que sea menos pesado y ahora promedio

cor_mod <- array(NA, dim = c(66,76,28,nmodels))
for (m in nmodels) {
  cor_mod[,,,model] <- readRDS(paste0("./MJO/predic/predic_",models[m]))
}

predictibilidad = apply(cor_mod , c(1,2,3), mean, na.rm = T)

# Guardo
saveRDS(predictibilidad, "./MJO/predic/predictmjo.rds")

rm(predictibilidad, cor_mod)
##-------------------------------------------------------------------------------------------------
# Predictibilidad de NO MJO
#-------------------------------------------------------------------------------------------------
# Predictibilidad
# correlacionar un modelo contra la media del ensamble formada por el resto de los modelos, 
# esto repetirlo con cada modelo y sacar el promedio de esa correlación
sabNOMJO <- as.Date(sabadosMME[!sabadosMME %in% fechas_act])


#Restringo tambien el targetdate del multimodelo para fechas Inactivas
targetdateMMENOMJO <- targetdateMME[,!sabadosMME %in% fechas_act]


#----------- VERSION NUEVA
predicnomjo <- EnsamblesPredictiblidad(Modelos=MODELOS,
                                       TgdtMod=targetdateMODELOS, 
                                       StdtMod=startdateMODELOS, 
                                       FechEnsam = sabNOMJO,
                                       TgdtEnsam = targetdateMMENOMJO,
                                       FilePath = "./MJO/predic")

# Guardo
saveRDS(predicnomjo, "./MJO/predic/predictnomjo.rds")
#----------- VERSION ANTERIOR
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
    startweek = seq.Date(sabNOMJO[i]-7,sabNOMJO[i]-1,by=1) #desde el sabado anterior al viernes
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
  saveRDS(cor_mod, paste0("./MJO/predic/predicNOMJO_",models[model]))
  print(paste("termino la correlacion del modelo",models[model]))
  
  # ELimino para hacer espacio
  rm(cor_mod)
  
} # End loop models


# Ahora promedio todas las correlaciones obtenidad de cada modelo (cuarta dimension)
# Guarde todas las correlaciones por separado para que sea menos pesado y ahora promedio

cor_mod <- array(NA, dim = c(66,76,28,nmodels))
for (m in nmodels) {
  cor_mod[,,,model] <- readRDS(paste0("./MJO/predic/predicNOMJO_",models[m]))
}

predictibilidad = apply(cor_mod , c(1,2,3), mean, na.rm = T)

# Guardo
saveRDS(predictibilidad, "./MJO/predic/predictnomjo.rds")


#------------------------------------------------------------------------------------------------
# Predictibilidad segun fase (los bins bah)

# MJO
# Cargo los datos de eventos
df_rmm <- readRDS("./MJO/df_rmm.rds")
df_rmm <- data.table(df_rmm)

# Vector Bin
bin = levels(df_rmm$Bin)

for (b in bin) {
  
  # Fechas de ese bin
  fechas_bin <- df_rmm[Bin == b,.(DATE)]
  fechas_bin <- as.character(fechas_bin$DATE)
  
  # Busco que sabados coinciden con las fechas del bin
  sabBin <- as.Date(sabadosMME[sabadosMME %in% fechas_bin])
  
  # Restringo tambien el targetdate del multimodelo para fechas del bin
  targetdateMMEBIN<- targetdateMME[,sabadosMME %in% fechas_bin]
  
  predic_bin= EnsamblesPredictiblidad(Modelos=MODELOS,
                          TgdtMod=targetdateMODELOS, 
                          StdtMod=startdateMODELOS, 
                          FechEnsam = sabBin,
                          TgdtEnsam = targetdateMMEBIN,
                          FilePath = paste0("./MJO/predic/bin",b))
  
  # Guardo
  saveRDS(predic_bin, paste0("./MJO/predic/predic_bin",b,".rds"))
  
  
}


# Resta de predictibilidad Inactiva vs Predictibilidad en una fase particular

# SI ES TOTAL - ACT DE MJO DONDE SEA NEGATIVO ------> APORTA MJO
# SI ES ACT - TOTAL DONDE SEA POSITIVO ----------> APORTA MJO

# cargo predictibilidad no activa
predtotal <- readRDS("./MJO/predic/predictnomjo.rds")

for (b in bin) {
  # Cargo datos de predictibilidad del bin
  predbin <- readRDS(paste0("./MJO/predic/predic_bin",b,".rds"))
  
  # Resto
  pred_diff <- predbin - predtotal
  
  # convierto a data frame para las 4 weeks
  dimnames(pred_diff) <- list("x" = seq(265,330,1), "y" = rev(seq(-60,15,1)),
                              "week" = c(rep("Week 1",7),
                                         rep("Week 2",7),
                                         rep("Week 3",7),
                                         rep("Week 4",7)))
  df <- reshape2::melt(pred_diff)
  colnames(df) <- c("x","y","week","z")
  
  # grafico 
  fase = paste("Fases",substr(b,5,5),"y",substr(b,6,6))
  g<-GraphDiscreteMultiple(Data=df,Breaks = seq(-0.4,0.4,0.1),Label = "ACC",Paleta = "RdBu",Direccion = -1)
  g <- g + ggtitle(paste0("Predictibilidad MJO ACT - INACT \n",fase," tasa (99-14, Oct-Mar)"))
  
  ggsave(filename = paste0("./MJO/predic/predic_diff",b,".png"),plot=g,width = 10, height = 4)
}
