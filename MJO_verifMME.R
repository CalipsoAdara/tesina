# -------------------------------------------------------------------------------------------------------
# Verificacion MJO vs NOMJO solo para MME
# ----------------------------------------------------------------------------------------------------

# por Lucia M Castro
# ----------------------------------------------------------------------------------------

# Limpiar el espacio
rm(list=ls())

# cargar paquetes
library(metR)
library(data.table)
library("ggplot2")
library(ggpubr)


# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata/model")
svpath = "/home/lucia.castro/SubX_processed_Rdata/model/viernes/MJO"

# Cargo los datos de eventos
df_rmm <- readRDS("./MJO/df_rmmOA.rds")
df_eventos <- readRDS("./MJO/df_eventosOA.rds")
fechas_act <- as.character(df_rmm$DATE)

# Cargo datos MME
tgdtMME <- readRDS("./viernes/targetdate_MME_OA.rds")
stdtMME <- dimnames(tgdtMME)$startdate
modelweek <- readRDS("./viernes/modelweek_MME.rds")
obsweek <- readRDS("./viernes/obsweek_MME.rds")

# Busco que startdates coinciden con los eventos activos
posMJO = stdtMME %in% fechas_act





#-------------------------------------------------------------------------------------------------------
# I N A C T I V O 
met_ina <- Metrics(obsweek[,,!posMJO,],modelweek[,,!posMJO,])
saveRDS(met_ina,paste0(svpath,"/metric_inact.rds"))


# ------------------------------------------------------------------------------------------------------
# A C T I V O S -- BINS


# Separar segun la fase inicial
# Bins = [8,1] [2,3] [4,5] [6,7]
# Quiero hacer la diferencia entre activos - inactivos para C/bin

Bins = levels(df_eventos$Bin)
nstartdateMJOBIN <- list() # lista para llenar

metric_ina <- readRDS(paste0(svpath,"/metric_inact.rds"))

for (b in Bins) {  # por cada Bin
  # Busco que startdates coinciden con los eventos activos
  fechas_act_bin <- as.character(df_rmm[Bin==b,DATE])
  posMJOBIN = stdtMME %in% fechas_act_bin
  
  # Cantidad de inicios antes y despues de restringir en los eventos
  # informacion para luego crear tabla
  nstartdate = length(stdtMME)
  nstartdateMJOBIN <- c(nstartdateMJOBIN,sum(posMJOBIN))
  
  # Calculo las distintas métricas por cada lon/lat/targetweek
  sco_bin<-Metrics(obsweek[,,posMJOBIN,],modelweek[,,posMJOBIN,])
  saveRDS(sco_bin,paste0(svpath,"/scorebinMME_",b))
  
  
  # RESTO 
  resta_bin <- Map('-', sco_bin, metric_ina) #Resto ambas listas 
  # Guardar la diferencia
  saveRDS(resta_bin,paste0(svpath,"/diff",b))
}


#---------------------------------------------------------------------------------------
# R E S T A 
# DATA FRAME a unir todo: modelos, bins, week, metric
df <- data.frame()

for (b in Bins) {
    df_mod <- reshape2::melt(readRDS(paste0(svpath,"/diff",b)))
    # agrego columnas
    df_mod$bin <- rep(b, nrow(df_mod))
    df <- rbind(df,df_mod)
}

df = rename(df, "metric" ="L1")
dt<-as.data.table(df)
dt <- dt[ week == "Week 2" | week == "Week 3"]

# cambiar la etiquetas de las fases
dt = FactorsFases(dt,"bin")

# Guardo o cargo segun el caso
saveRDS(dt, file = "./viernes/dtMMEverif.rds")
dt = readRDS("./viernes/dtMMEverif.rds")

# Graficado
gacc <- GraphMet(Data= dt[metric == "acc"],
             Breaks = seq(-0.5,0.5,0.10),
             Paleta = "RdBu",
             Direccion = 1 ,
             Titulo = " Coeficiente de Correlación de Anomalías",
             Row = "week",
             Col = "FASE")
grmse <- GraphMet(Data= dt[metric == "rmse"],
                 Breaks = seq(-0.5,0.5,0.10),
                 Paleta = "RdBu",
                 Direccion = 1 ,
                 Titulo = "Error Cuadrático Medio",
                 Row = "week",
                 Col = "FASE")


# Ahora agrego leyenda
legend <- LeyendaCondicional(Breaks = seq(-0.5,0.5,0.10), 
                             Paleta = "RdBu", 
                             Labels = c("MJO APORTA", "MJO NO APORTA"))


g <-grid.arrange(gacc,grmse,                                
             legend,                               
             ncol = 2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(7,7),
             heights = c(5,0.25))

ggsave("./viernes/verifMME_fases.png",g,width = 10, height = 4)


