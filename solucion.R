# SOLUCION 
# En los archivos de los modelos Subx se da vuelta las latitudes y los vuelvo a guardar 

#-----------------------------------------------------------------------------------------------------------------------

# LIMPIO ENVIROMENT
rm(list=ls())

# SETEO DIRECTORIO
setwd("/home/lucia.castro/")


ar.model.GMAO = readRDS("./SubX_processed_Rdata/model_GMAO_ONDEFM.rds")
