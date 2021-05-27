# Multi-model mean SubX 1999-2015
#
# by Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------

# Limpiar enviroment 
rm(list=ls())

# Call libraries to be used

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/"

# Seteo el directorio
setwd(savepath)

# Cargo los datos 
ESRL <- readRDS("model_ESRL_ONDEFM.rds")
GMAO <- readRDS("model_GMAO_ONDEFM.rds")
RSMAS <- readRDS("model_RSMAS_ONDEFM.rds")
targetdate_ESRL <- readRDS("targetdate_ESRL_ONDEFM.rds")
targetdate_GMAO <- readRDS("targetdate_GMAO_ONDEFM.rds")
targetdate_RSMAS <- readRDS("targetdate_RSMAS_ONDEFM.rds")


weekdays(as.Date(targetdate_ESRL))
# Veo cual es el primer sabado del periodo Ene 1999- Dic 2015 (es el 2/01/1999 y el que sigue es 9/02/1999)
periodo = seq.Date(as.Date("1999-01-01"), as.Date("2015-12-31"), by = 1)
weekday = weekdays(periodo)

for (i in 9:length(periodo)) {    # arranca el 9/02/1999
  
  # Dia de la seamana
  dia = weekday[i]
  
  if(dia == "Saturday"){
    
    # Obtengo la semana anterior al sabado en cuestion
    semana_anterior <-  seq.Date(periodo[i-6],periodo[i-1],by=1)
    
    # 
    startdate = dimnames(targetdate_ESRL)$stardate
    setdiff(startdate,periodo)
  }
  
}
