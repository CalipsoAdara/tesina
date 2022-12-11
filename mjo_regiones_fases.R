## Script para ordenar los datos del reanalisis del CPC  segun las dimensiones de los datos SubX
# tiene en cuenta los leads. Segun las fases de MJO

# MJO con regiones
# Analizar si las inicializaciones con MJO activos afecta a los scores segun la$
# Analisis zona SACZ y SUR PATAGONIA
# # Si resto activo - inactivo
# donde rmse sea positivo siginifica que fue mayor en activo ----> MAL
# donde acc sea positivo significa que fue mayor en activo -----> BIEN
# 
#


# By Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------------
# Limpiar enviroment 
rm(list=ls())

# Llamar paquetes
library(secr)
library(ggplot2)
library(data.table)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/model"

# Seteo el directorio
setwd(savepath)

# --------------------------------------------------------------------------------------------------------
# Cargo datos modelos y MJO
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
nmodels = length(models)
df_rmm <- readRDS("./MJO/df_rmmOA.rds")
Bins = levels(df_rmm$Bin)

# ESTO LO ESTOY HACIENDO POR FASE Y NO POR FASE INICIAL OJO!!
for (m in models) {  # por cada modelo
  
  # cargo datos
  model <- readRDS(paste0("./model_",m,"_OA.rds"))
  target <- readRDS(paste0("./targetdate_",m,"_OA.rds"))
  startdate <- as.Date(dimnames(target)$startdate)
  obs <- readRDS(paste0("./obs_",m,".rds"))
  
  for (b in Bins) {  # por cada bin
    # f1 = substr(b,5,5)
    # f2 = substr(b,6,6)
    # 
    # # Restringo en dias con esa fases
    # fase_obs = df_rmm[FASE == f1 | FASE == f2, DATE]
    fase_obs = df_rmm[Bin==b, DATE]
    
    fase_stdt = startdate %in% fase_obs
    
    # Evaluo en esas startdates
    model_fase = model[,,,fase_stdt]
    obs_fase = obs[,,,fase_stdt]
    
    # Metricas -------------------------
    # hago los calculos
    acc = ACC2(obs_fase,model_fase, Dim=4)
    
    # RMSE
    dif = (obs_fase-model_fase)**2
    rmse = sqrt(apply(dif,c(1,2,3),mean))
    
    # ponerle nombres 
    # Guardo
    metric <- list( "acc" = acc,"rmse" = rmse)
    saveRDS(metric,paste0("./MJO/fase/metricINI_",m,"_",b,".rds"))
    
  }  # End loop bins
} # End loop models

# --------------------------------------------------------------------------------------------
# PROMEDIOS EN REGIONES

# Poligonos. Lon de menor a mayor, el primer punto se repite para cerrar el poligono
SP <- readRDS("./poligonos/SP.rds")

SACZ <- readRDS("./poligonos/SACZ.rds")

# cargo datos
df = data.frame()
for (m in models) { # por cada modelo
  for (b in Bins) { # por cada bin
    mod <- readRDS(paste0("./model_",m,"_OA.rds"))
    metric=readRDS(paste0("./MJO/fase/metricINI_",m,"_",b,".rds"))
    dimnames(metric$acc) <- dimnames(mod)[1:3]
    dimnames(metric$rmse) <- dimnames(mod)[1:3]
    
    df.model = reshape2::melt(metric)
    # agrego una columna del modelo y otra de fase
    df.model$MODEL = rep(m,nrow(df.model))
    df.model$BIN = rep(b,nrow(df.model))
    df = rbind(df,df.model)
  } # End loop bind
}

# Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
puntossacz=pointsInPolygon(df[,1:2],SACZ) 
puntossp=pointsInPolygon(df[,1:2],SP) 
df_sacz = df[puntossacz,]
df_sp = df[puntossp,]

# Promedio pesado
prom_sacz = DTPromEspacPesado(df_sacz, Variable = "value", Lat= "Y",Grupo=c("L","MODEL","L1","BIN"))
prom_sp = DTPromEspacPesado(df_sp, Variable = "value", Lat= "Y",Grupo=c("L","MODEL","L1","BIN"))

# Cambio nombre
prom_sacz = as.data.table(prom_sacz)
prom_sp = as.data.table(prom_sp)
setnames(prom_sacz,"L","LEADS")
setnames(prom_sp,"L","LEADS")


# GUARDO
saveRDS(prom_sacz, "./poligonos/prom_sacz_faseINI.rds")
saveRDS(prom_sp, "./poligonos/prom_sp_faseINI.rds")

# --------------------------------------------------------------------------------------------
# G R A F I C O S 


# cargar de ser necesario
prom_sacz <- readRDS("./poligonos/prom_sacz_faseINI.rds")
prom_sp <- readRDS("./poligonos/prom_sp_faseINI.rds")
ref <- readRDS("./metric/dt_regions.rds")

# JUnto en un solo data frame 
prom_sacz$REGION = rep("SACZ",nrow(prom_sacz))
prom_sp$REGION = rep("SEPG",nrow(prom_sp))
prom = rbind(prom_sacz,prom_sp)
prom = FactorsModels(prom, Col = "MODEL")  # convierto los models en factors
ref$BIN = rep("ALL",nrow(ref))  # agrego columna con nombre de bBins
dt = rbind(prom,ref)

# Hago primero con ACC Primero
dt_acc = dt[L1 == "acc" ]
dt_rmse = dt[L1 == "rmse" ]

# Grafico
gacc = GraphLineFases(Data = dt_acc, X = "LEADS", Y = "media", ALPHA = "MODEL",
               SIZE = "BIN",COLOR = "BIN", Facet = "REGION", EjeX = "LEADS" , EjeY = "ACC" )
grmse = GraphLineFases(Data = dt_rmse, X = "LEADS", Y = "media", ALPHA = "MODEL",
               SIZE = "BIN",COLOR = "BIN", Facet = "REGION", EjeX = "LEADS" , EjeY = "RMSE" )

# Voy a limitar el eje y a solo 28 leads
gacc <- gacc + xlim(c(0,28))
grmse <- grmse + xlim(c(0,28))

# GUARDO
ggsave(filename="./MJO/fase/acc_Region_fase.png",plot=gacc,width = 15, height = 7)
ggsave(filename="./MJO/fase/rmse_Region_fase.png",plot=grmse,width = 15, height = 7)






p = c("black","darkolivegreen4","indianred2","steelblue1","darkorange")

tube = c("#00782A","#0098D4","#F3A9BB","#E32017","#003688","#FFD300","#000000")

# La diferencia con usar fase sin que sea inicial tiene peores scores. ACC bajo y rmse altos. 

