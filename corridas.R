# Script para correr varios codigos de una vez 
#
# By Lucia M. Castro

# Limpiar enviroment
rm(list=ls())

# Seteo directorio
setwd("/home/lucia.castro/tesina")

# Cargo funciones
source("/home/lucia.castro/tesina/funciones.R")

# Los nombres 
grupos = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
nmodels = length(grupos)

# Generar imagenes de scores de los modelos
for (mod in 1:nmodels) {
  # Seteo directorio
  setwd("/home/lucia.castro/tesina")
  
  grupos = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
  fname = paste0("./verify_",grupos[mod],".R")
  source(fname)
}
