# SOLUCION 
# En los archivos de los modelos Subx se da vuelta las latitudes y los vuelvo a guardar 

#-----------------------------------------------------------------------------------------------------------------------

# LIMPIO ENVIROMENT
rm(list=ls())

# SETEO DIRECTORIO
setwd("/home/lucia.castro/")

groups = c('GMAO','RSMAS','ESRL','ECCC','EMC',"NRL")  

for (grupo in groups) {
  
  # Leo los datos del modelo
  ar.model = readRDS(paste0("./SubX_processed_Rdata/model_",grupo,"_ONDEFM.rds"))
  
  # Da vuelta las latitudes (60S a 15N en todos los casos)
  ar.model = ar.model[,seq(76,1,-1),,]
  
  # Guardo los datos
  saveRDS(ar.model,paste0("./SubX_processed_Rdata/model_",grupo,"_ONDEFM.rds"))
  
}

