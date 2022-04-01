# Analisis de la irrupcion fria 2007 6-10 Julio 2007
#https://es.wikipedia.org/wiki/Nevadas_en_Argentina_del_9_de_julio_de_2007  
#
# Basado en el paper de los dioses
#Subseasonal prediction of the heat wave of December 2013
#in Southern South America by the POAMA and BCC�^`^pCPS models
#
#
# LA DIFERENCIA CON EL CASO DE OLA DE CALOR ES QUE SON MENOS DIAS, NO HAY PROMEDIO SEMANAL
# SE MIRAN DIAS ESPECIFICOS
#
# By lucia m. castro
# ---------------------------------------------------------------------------------




# Limpiar el espacio
rm(list=ls())

# cargar paquetes
library(ggplot2)
library(metR)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")
svpath = "/home/lucia.castro/SubX_processed_Rdata"


# ------------------------------------------------------------------------------

# O B S E R V A C I O N E S

# Cargo datos
ar.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")
caso = as.character(seq.Date(as.Date("2007-07-05"),as.Date("2007-07-10"),1))

for(d in caso) {
  
  
  # Graficar
  go <- GraphDiscrete(Data = ggDataFrame(prom),Breaks = seq(-3,3,1),Label = "°C",
                      Paleta = "RdBu", Direccion = -1,
                      Titulo = paste0("T2M ANOM CPC \n",sem[1],"-",sem[length(sem)]))
  caso <- caso +1
  ggsave(paste0(svpath,"/obsola_",caso,".png"),go)
  
}

GraphDiscrete(Data = ggDataFrame(ar.anom[,,"2007-07-09"]),Breaks = seq(-5,5,1),Label = "°C",
              Paleta = "RdBu", Direccion = -1,
              Titulo = paste0("T2M ANOM CPC \n"))
