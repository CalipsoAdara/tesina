# Multi-model mean SubX 1999-2015
#
# by Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------

# Limpiar enviroment 
rm(list=ls())

# Call libraries to be used
library(abind)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/"

# Seteo el directorio
setwd(savepath)

# Cargo los datos 
models = c("ESRL","ECCC","EMC","GMAO","RSMAS")#,"NRL")
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
MODELOS <- list(ESRL, ECCC, EMC, GMAO, RSMAS)

targetdateMODELOS <- list(targetdate_ESRL, targetdate_ECCC, targetdate_EMC, 
                          targetdate_GMAO, targetdate_RSMAS)
startdateMODELOS <- list(startdateESRL, startdateECCC, startdateEMC,
                     startdateGMAO, startdateRSMAS)

# Obtengo el periodo de 1999 a 2015 con solo octubre a marzo
periodo = seq.Date(as.Date("1999-01-01"), as.Date("2015-12-31"), by = 1)
OM = c(1,2,3,10,11,12)
oct_mar <- which(month(periodo) %in% OM) # posiciones donde el mes cae entre Octubre a Marzo
periodo_OM <- periodo[oct_mar]

# Obtengo sabados y sus respectivos viernes siguientes para el periodo
sabado = periodo_OM[weekdays(periodo_OM)=="Saturday"][-1] # Borra el primer sabado (1999-01-02) que no tiene semana anterior 
vier_sig = periodo_OM[weekdays(periodo_OM)=="Friday"][-(1:2)]  # Borra el primer viernes (1999-01-01) y segundo (1999-01-08) que no tiene sabado asociado

# Los sabados son la fecha de inicio del multimodelo. En la semana anterior a ese sabado busco los
# modelos inicializados. La semana objetivo empieza el sabado y termina el viernes siguiente.

# Quito los sabados que caigan entre 1 y 7 de octubre ya que no tendran modelos inicializados
# la semana anterior (septiembre)
oct1_7 = c("10-01","10-02","10-03","10-04","10-05","10-06","10-07")
sabadoMME = sabado[!substr(sabado,6,10) %in% oct1_7]  # Asigna TRUE donde se cumple la condicion y "!" lo revierte

# Array de targetday del multimodelo
targetdateMME = array(NA,dim = c(28,length(sabadoMME)), dimnames = list(1:28,as.character(sabadoMME)))
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
  
  for (mod in 1:5) { # por cada modelo
    
    # Que startdate cae en la semana del MME para cada modelo
    stdt = startdateMODELOS[[mod]] %in% startweek
    
   
    # Quiero saber que lead hace que coincida el targetdate del modelo con el targetdate del MME 
    leadMODELO = targetdateMODELOS[[mod]]
    leadMME = targetdateMME[,i]
    
    target = leadMODELO[,stdt] %in% leadMME
  
    # Evaluo el modelo en esas fechas
    model = MODELOS[[mod]]
    modelo_objetivo = model[,,target,stdt]
    

    
    
    # Tomar la inicializacion mas cercana al sabado de pronostico
    if (sum(stdt)>1) {    # Hay mas de un inicio en la semana
      stdt = last(which(stdt))
    }
    # Si el modelo no alcanza a llenar los 28 dias del MME, llenar el resto con NA
    if (sum(target)<28) {
      
      
      faltante = 28 - sum(target)
      mod.faltante = array(NA, dim = c(66,76,faltante))
      
      # PRUEBA: QUE PASA CUANDO NO HAY MODELO ESA SEMANA
      if (sum(stdt)==0){
        print(paste("La semana ",startweek[1]," no tiene el modelo",models[mod]))
        
        modelo_objetivo = mod.faltante
      }else{
        modelo_objetivo = abind(modelo_objetivo,mod.faltante)
      }

    }
    
    # Guardo
    MME[,,,mod,i] <- modelo_objetivo
  
  } # End loop modelos
  
} # End loop sabados  

