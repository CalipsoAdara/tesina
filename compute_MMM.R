# Multi-model mean SubX 1999-2015
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
library(ggplot2)
library(metR)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/"

# Seteo el directorio
setwd(savepath)

# FUNCIONES ----------------------------------------------------------------------------------
Predictibilidad <- function(Modelo, Ensamble) {
  ## Modelo: array de 4 dimensiones (lon,lat,lead,startdate)
  ## Ensamble: array de 4 dimensiones (lon,lat,lead,startdate)
  ##  Ambos array deben tener las mismas dimensiones
  
  # Tomo las dimensiones de los array (para ambas son las mismas)
  lon = dim(Modelo)[1]
  lat = dim(Modelo)[2]
  lead = dim(Modelo)[3]
  starts = dim(Modelo)[4]
  
  # array a llenar
  acc = array(NA, dim = c(lon,lat,lead))
  
  # TIENE QUE HABER UNA MEJOR FORMA DE CORRELACIONAR 2 MATRICES EN LA TERCERA DIMENSION
  # Y OBTENER UN VALOR POR PUNTO

    for (l in 1:lead) { # por cada lead
      for (x in 1:lon) { # por cada longitud
        for (y in 1:lat) { # por cada latitud
          
          mod_punto = Modelo[x,y,l,]
          ens_punto = Ensamble[x,y,l,]
          acc[x,y,l] <- cor(mod_punto,ens_punto,use="pairwise.complete.obs",method = "pearson")
        }
        
      }
      
    }
    return(acc)
}
#------------------------------------------------------------------------------------------------
CompletarFaltante <- function(Target, Stdt, ModeloObjetivo) {
  ## Target: Vector logico
  ## Stdt: Vector logico
  ## ModeloObjetico: array de 3 dimensiones que deberia completarse con 66,76 y 28 lead
  
  # Si el modelo no alcanza a llenar los 28 dias del MME, llenar el resto con NA
  if (sum(Target)<28) {
    faltante = 28 - sum(Target)
    mod.faltante = array(NA, dim = c(66,76,faltante))
    
    # PRUEBA: QUE PASA CUANDO NO HAY MODELO ESA SEMANA
    if (sum(Stdt)==0){
      #print(paste("La semana ",startweek[1]," no tiene el modelo",models[mod]))
      
      modelo_objetivo = mod.faltante
    }else{
      #print(paste("La semana ",startweek[1]," le faltan dias del modelo",models[mod]))
      
      modelo_objetivo = abind(ModeloObjetivo,mod.faltante)
    }
  }
  
  # Si el modelo esta completo no hacer nada
  if (sum(Target)==28) {
     modelo_objetivo = ModeloObjetivo
  }
  return(modelo_objetivo)
}
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Cargo los datos 
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL")
nmodels = length(models)

ESRL <- readRDS("model_ESRL_ONDEFM.rds")
ECCC <- readRDS("model_ECCC_ONDEFM.rds")
EMC <- readRDS("model_EMC_ONDEFM.rds")
GMAO <- readRDS("model_GMAO_ONDEFM.rds")
RSMAS <- readRDS("model_RSMAS_ONDEFM.rds")
NRL <- readRDS("model_NRL_ONDEFM.rds")
targetdate_ESRL <- readRDS("targetdate_ESRL_ONDEFM.rds")
targetdate_ECCC <- readRDS("targetdate_ECCC_ONDEFM.rds")
targetdate_EMC <- readRDS("targetdate_EMC_ONDEFM.rds")
targetdate_GMAO <- readRDS("targetdate_GMAO_ONDEFM.rds")
targetdate_RSMAS <- readRDS("targetdate_RSMAS_ONDEFM.rds")
targetdate_NRL <- readRDS("targetdate_NRL_ONDEFM.rds")

# startdate de los modelos 
startdateESRL = as.Date(dimnames(targetdate_ESRL)$startdate)
startdateECCC = as.Date(dimnames(targetdate_ECCC)$startdate)
startdateEMC = as.Date(dimnames(targetdate_EMC)$startdate)
startdateGMAO = as.Date(dimnames(targetdate_GMAO)$startdate)
startdateRSMAS = as.Date(dimnames(targetdate_RSMAS)$startdate)
startdateNRL = as.Date(dimnames(targetdate_NRL)$startdate)

#
# Listas de todos los modelos 
MODELOS <- list(ESRL, ECCC, EMC, GMAO, RSMAS, NRL)

targetdateMODELOS <- list(targetdate_ESRL, targetdate_ECCC, targetdate_EMC, 
                          targetdate_GMAO, targetdate_RSMAS,targetdate_NRL)
startdateMODELOS <- list(startdateESRL, startdateECCC, startdateEMC,
                     startdateGMAO, startdateRSMAS, startdateNRL)

# Obtengo el periodo de 1999 a 2014 con solo octubre a marzo
periodo = seq.Date(as.Date("1999-01-01"), as.Date("2014-12-31"), by = 1)
OM = c(1,2,3,10,11,12)
oct_mar <- which(month(periodo) %in% OM) # posiciones donde el mes cae entre Octubre a Marzo
periodo_OM <- periodo[oct_mar]

# Obtengo sabados y sus respectivos viernes siguientes para el periodo
sabado = periodo_OM[weekdays(periodo_OM)=="Saturday"][-1] # Borra el primer sabado (1999-01-02) que no tiene semana anterior 

# Los sabados son la fecha de inicio del multimodelo. En la semana anterior a ese sabado busco los
# modelos inicializados. La semana objetivo empieza el sabado y termina el viernes siguiente.

# Quito los sabados que caigan entre 1 y 7 de octubre ya que no tendran modelos inicializados
# la semana anterior (septiembre)
oct1_7 = c("10-01","10-02","10-03","10-04","10-05","10-06","10-07")
sabadoMME = sabado[!substr(sabado,6,10) %in% oct1_7]  # Asigna TRUE donde se cumple la condicion y "!" lo revierte

# Array de targetday del multimodelo
targetdateMME = array(NA,dim = c(28,length(sabadoMME)), dimnames = list("lead"=1:28,"startdate" = as.character(sabadoMME)))
# Recorre todas las fechas de pronosticos
for (sab in 1:length(sabadoMME)) {
  targetdateMME[,sab] <- as.character(sabadoMME[sab] +(0:27))
}



#---------------------------------------------
# Promediar modelos

# array a completar 
MME <- array(NA, dim = c(66,76,28,nmodels,length(sabadoMME)))


for (i in 1:length(sabadoMME)) { # Por cada sabado
  
  # Semana en cuestion del MME
  startweek = seq.Date(sabadoMME[i]-7,sabadoMME[i]-1,by=1) #desde el sabado anterior al viernes
  
  for (mod in 1:length(MODELOS)) { # por cada modelo
    
    # Que startdate cae en la semana del MME para cada modelo
    stdt = startdateMODELOS[[mod]] %in% startweek
    
    # Tomar la inicializacion mas cercana al sabado de pronostico
    if (sum(stdt)>1) {    # Hay mas de un inicio en la semana
      stdt = last(which(stdt))
    }
    
    # Quiero saber que lead hace que coincida el targetdate del modelo con el targetdate del MME 
    leadMODELO = targetdateMODELOS[[mod]]
    leadMME = targetdateMME[,i]
    
    target = leadMODELO[,stdt] %in% leadMME
  
    # Evaluo el modelo en esas fechas
    model = MODELOS[[mod]]
    modelo_objetivo = model[,,target,stdt]
    
    
  
    
    # Si el modelo no alcanza a llenar los 28 dias del MME, llenar el resto con NA
    if (sum(target)<28) {
      
      
      faltante = 28 - sum(target)
      mod.faltante = array(NA, dim = c(66,76,faltante))
      
      # PRUEBA: QUE PASA CUANDO NO HAY MODELO ESA SEMANA
      if (sum(stdt)==0){
        print(paste("La semana ",startweek[1]," no tiene el modelo",models[mod]))
        
        modelo_objetivo = mod.faltante
      }else{
        print(paste("La semana ",startweek[1]," le faltan dias del modelo",models[mod]))
        
        modelo_objetivo = abind(modelo_objetivo,mod.faltante)
      }
    }
    
    # Guardo
    MME[,,,mod,i] <- modelo_objetivo
  
  } # End loop modelos
  
} # End loop sabados  


# Promedio sobre los modelos (cuarta dimension)
MME_pro = apply(MME, c(1,2,3,5), mean, na.rm = T)

# Nombro dimensiones
dimnames(MME_pro) <- list("lon" = seq(265,330,1), "lat" = rev(seq(-60,15,1)),
                          "lead" = 1:28, "startdate"  = as.character(sabadoMME))

# Todo listo para empezar la verificación octubre-marzo. Guardo para limpiar y comenzar la verificación.
saveRDS(MME_pro,paste0("./MME_OM.rds"))
saveRDS(targetdateMME,paste0("./targetdate_MME_ONDEFM.rds"))

#-------------------------------------------------------------------------------------------------
# Predictibilidad
# correlacionar un modelo contra la media del ensamble formada por el resto de los modelos, 
# esto repetirlo con cada modelo y sacar el promedio de esa correlación
cor_mod <- array(NA, dim = c(66,76,28,nmodels))

for (model in 1:nmodels) {
  
  # Tomo el modelo a comparar contra la EM del resto de los mod
  mod_restante_stdate = startdateMODELOS[[model]]
  mod_restante_tgdate = targetdateMODELOS[[model]]
  MODELO_restante = MODELOS[[model]]
  
  mod_ensamble_stdate = startdateMODELOS[-model]
  mod_ensamble_tgdate = targetdateMODELOS[-model]
  MODELOS_ensamble = MODELOS[-model]
  
  # array a completar 
  MME_nmenos1 <- array(NA, dim = c(66,76,28,(nmodels-1),length(sabadoMME)))
  mod_aparte <- array(NA, dim = c(66,76,28,length(sabadoMME)))
  
  for (i in 1:length(sabadoMME)) { # Por cada sabado
    
    # Semana y lead en cuestion del MME
    startweek = seq.Date(sabadoMME[i]-7,sabadoMME[i]-1,by=1) #desde el sabado anterior al viernes
    leadMME = targetdateMME[,i]
    
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
                                             ModeloObjetivo = modelo_objetivo_rest)
    
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
      modelo_objetivo = CompletarFaltante(target, stdt, modelo_objetivo)
      
      
      # Guardo
      MME_nmenos1[,,,mod,i] <- modelo_objetivo
      mod_aparte[,,,i] <- modelo_objetivo_rest
      
    } # End loop media ensamble
    
  } # End loop sabados  
  # Promedio sobre los modelos (cuarta dimension)
  MME_pro = apply(MME_nmenos1 , c(1,2,3,5), mean, na.rm = T)  # Creado la media del ensamble n-1

  # Correlacion entre mod y ens n-1
  cor_mod[,,,model] <- Predictibilidad(mod_aparte, MME_pro)
  
} # End loop models
  
# Ahora promedio todas las correlaciones obtenidad de cada modelo (cuarta dimension)
predictibilidad = apply(cor_mod , c(1,2,3), mean, na.rm = T)

# Guardo
saveRDS(predictibilidad, "./predict.rds")

# G R A F I C O S ------------------------------------

predic <- readRDS("predict.rds")

# convierto a data frame para las 4 weeks
dimnames(predic) <- list("x" = seq(265,330,1), "y" = rev(seq(-60,15,1)),
                         "week" = c(rep("Week 1",7),
                                    rep("Week 2",7),
                                    rep("Week 3",7),
                                    rep("Week 4",7)))
df <- reshape2::melt(predic)
colnames(df) <- c("x","y","week","z")
g<-GraphDiscreteMultiple(Data=df,Breaks = seq(0,1,0.2),Label = "ACC",Paleta = "Greens",Direccion = 1)
g <- g + ggtitle(paste0("Predictibilidad  \ntasa (99-14, Oct-Mar)"))

ggsave(filename = "./predic.png",plot=g,width = 10, height = 4)

for (m in 1:nmodels) {
  
  # Renombro dimensiones 
  data = cor_mod[,,,m]
  dimnames(data) <- list(x = seq(265,330,1), 
                         y = rev(seq(-60,15,1)), 
                         week = c("Week 1", "Week 2","Week 3", "Week 4"))
  
  # Armo data.frames para graficar
  df <- reshape2::melt(data, value.name = "z")
  
  titl = paste0("Predictibilidad ", models[m], " tasa (99-14, Oct-Mar)")
  g <- GraphDiscreteMultiple(df, Breaks = seq(0,1,0.2) , Label = "ACC", Paleta = "YlOrRd",Direccion = "1")
  fig <- grid.arrange(g, ncol = 1,top = textGrob(titl,gp=gpar(fontsize=13,font=3)))
  fn <- paste0("/home/lucia.castro/SubX_processed_Rdata/predic_",models[m],".png")
  ggsave(filename=fn,plot=fig,width = 10, height = 4)
}

cor(matrix1, matrix2)
matrix1 = matrix(data = 1:12, nrow=3, ncol = 4)

matrix2 = matrix(data = 2:13, nrow=3, ncol = 4)

