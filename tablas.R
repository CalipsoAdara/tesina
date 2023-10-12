# Script de para hacer tablas de inicios segun fases o extremos
#
#
# By Lucia M Castro
#---------------------------------------------------------------------------------------
# Limpiar el espacio
rm(list=ls())

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata/model/viernes/")
#--------------------------------------------------------------------------------------------------------
# Tabla de inicios activos e inactivos segun el modelo

# Cargo los datos de eventos
df_rmm <- readRDS("/home/lucia.castro/SubX_processed_Rdata/model/MJO/df_rmmOA.rds")
df_eventos <- readRDS("/home/lucia.castro/SubX_processed_Rdata/model/MJO/df_eventosOA.rds")
fechas_act <- as.character(df_rmm$DATE)
groups=c('GMAO','RSMAS','ESRL','ECCC','NRL','EMC','MME')     
Bins = levels(df_eventos$Bin)

# Vectors a llenar con la cantitda de start de cada modelo
stdt_totales <-c()  # totales
stdt_MJO <-c()      # de MJO activos
stdt_fases <-c()    # de cada fase MJO

for (g in 1:length(groups)) {
  grupo = groups[g]
  # Busco que startdates coinciden con los eventos activos
  TargetDate <- readRDS(paste0("./targetdate_",grupo,"_OA.rds"))
  startdate = dimnames(TargetDate)$startdate
  posMJO = startdate %in% fechas_act
  

  
  
  # Cantidad de inicios antes y despues de restringir en los eventos
  stdt_totales <-c(stdt_totales,length(startdate))
  stdt_MJO <-c(stdt_MJO,sum(posMJO))
  
  print(paste("Los inicios totales del modelo",grupo,"son:",length(startdate),"\nLos inicios MJO son:",sum(posMJO)))
  
  for (b in Bins) {
    f1 = as.numeric(substr(b,5,5))
    f2 = as.numeric(substr(b,6,6))
    # Busco que startdates coinciden con los eventos activos
    fechas_act_bin <- as.character(df_rmm[FASE==f1|FASE==f2,(DATE)])
    posMJOBIN = startdate %in% fechas_act_bin
    stdt_fases <-c(stdt_fases,sum(posMJOBIN))
    print(paste("Los inicios del modelo",grupo,"en las fases:",f1,"o",f2,"son:",sum(posMJOBIN)))
  }
  
}


# Busqueda de fechas extremas para tener inicios
ext <- read.csv("./ext_newpoli.csv",stringsAsFactors = F)

# Los nombres 
nmodels = length(grupos) 
Col = c("TOTAL10","TOTAL90")

for (mod in 1:nmodels) {
  
  # Leer datos
  grupo = groups[mod]
  TargetDate <- readRDS(paste0("./targetdate_",grupo,"_ONDEFM.rds"))
  startdate = dimnames(TargetDate)$startdate
  
  # La cantidad de fechas de pronosticos desde Oct a Mar en el periodo del modelo
  fechas_pronosticos <- dim(ar.model)[3]
  inicios = as.Date( dimnames(ar.model)$start )
  
  for (c in 1:2) {
    colname = Col[c]
    extrema = BuscarFechaExtrema(Ext = ext, Columna = colname, Startdate = startdate)
    # Remuevo posiciones repetidas y ordeno de menor a mayor
    extrema = sort(unique(extrema))
  
    stdtext = startdate[extrema]
    ar.anom.ext = ar.anom[,,extrema,]
  }
  

      
      
}

BuscarFechaExtrema <- function(Ext, Startdate, Columna){
  ## Ext : Data frame. Tabla con las fechas extremas separadas las regiones en columnas
  ## Startdate: Vector con las fechas de inicializacion del modelo
  ## Columna: Character vector. Columna o Columnas deseadas del data frame EXT
  
  # Fuerzo a data table
  ext = as.data.table(Ext)
  
  # Crea data table con las fechas de inicio totales
  inicios = data.table("ini" = as.Date(Startdate)) 
  
  # Crea data table con las fechas extremas de la region
  vecfechas <- c()
  for (n in 1:length(Columna)) {
    fecha_region = ext[,get(Columna[n])]
    vecfechas = append(vecfechas,fecha_region)
  }
  dt.fechas = data.table("region" = as.Date(vecfechas))
  
  # Encontrar las posiciones de las fechas extremas mas cercanas, ya que 
  # no son exactamente iguales
  pos_extrema = inicios[dt.fechas, on = .(ini = region),roll = "nearest", which = T]
  
  return(pos_extrema)
  
}