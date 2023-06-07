# Predictibilidad
# La idea es correlacionar un modelo con la media del ensamble formada por el resto de los modelos
# y luego esto repetirlo con cada modelo y sacar el promedio de esa correlación

# by Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------

# Limpiar enviroment 
rm(list=ls())

# Llamar paquetes
library(ggplot2)
library(data.table)
library(abind)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/model"

# Seteo el directorio
setwd(savepath)

# Cargo los datos 
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL")
nmodels = length(models)

MODELOS <- list()
targetdateMODELOS <- list()
startdateMODELOS <- list()

for (m in 1:nmodels) {
  MODELOS[[m]] <- readRDS(paste0("./model_",models[m],"_OA.rds"))
  targetdateMODELOS[[m]] <- readRDS(paste0("./targetdate_",models[m],"_OA.rds"))
  startdateMODELOS[[m]] <- dimnames(targetdateMODELOS[[m]])$startdate
}

# G R A F I C O S ------------------------------------
predic <- readRDS("./viernes/MJO/predic/predictmjo.rds")
predic <- predic*100

# convierto a data frame para las 4 weeks
dimnames(predic) <- list("x" = seq(265,330,1), "y" = rev(seq(-60,15,1)),
                         "week" = c(rep("Week 1",7),
                                    rep("Week 2",7),
                                    rep("Week 3",7),
                                    rep("Week 4",7)))
df <- reshape2::melt(predic)
colnames(df) <- c("x","y","week","z")
g<-GraphDiscreteMultiple(Data=df,Breaks = seq(0,100,10),Label = "(%)",Paleta = "Greens",Direccion = 1)
g <- g + ggtitle(paste0("Predictibilidad  \nT2M (99-14, Oct-Apr)"))

ggsave(filename = "./viernes/MJO/predic/predic_porcentaje.png",plot=g,width = 10, height = 4)



#----------------------------------------------------------------------------------------
# Ahora grafico la diferencia entre la predictibilidad total y la predictiblidad 
# de solo los eventos activos de MJO
##
# SI ES TOTAL - ACT DE MJO DONDE SEA NEGATIVO ------> APORTA MJO
# SI ES ACT - TOTAL DONDE SEA POSITIVO ----------> APORTA MJO

# cargo predictibilidad total
predtotal <- readRDS("./viernes/MJO/predic/predictnomjo.rds")
predMJO <- readRDS("./viernes/MJO/predic/predictmjo.rds")

pred_diff <- predMJO*100 - predtotal*100

# convierto a data frame para las 4 weeks
dimnames(pred_diff) <- list("x" = seq(265,330,1), "y" = rev(seq(-60,15,1)),
                           "week" = c(rep("Week 1",7),
                                      rep("Week 2",7),
                                      rep("Week 3",7),
                                      rep("Week 4",7)))
df <- reshape2::melt(pred_diff)
colnames(df) <- c("x","y","week","z")

# Si se desea cambiar week por semanas
df = WeeksToSemanas(DF = df, Col = "week")

g<-GraphDiscreteMultiple(Data=df,Breaks = seq(-20,20,5),Label = "p.p.",Paleta = "RdBu",Direccion = -1)
g <- g + ggtitle(paste0("Predictibilidad MJO ACT - INACT \nT2MA (99-14, Oct-Abr)"))
#g <- g + ggtitle(paste0("Predictability  MJO ACT - INACT \nT2MA (99-14, Oct-Apr)"))


ggsave(filename = "./viernes/MJO/predic/predic_es_resta_actinact.png",plot=g,width = 10, height = 4)


#------------------------------ ----------------------------------------------------------
# Ahora con fechas extremas yaay
# Lo importante es que las fechas del ensamble (MME) sean lo mas cercanas a las fechas extremas

tgdtMME <- readRDS("./viernes/targetdate_MME_OA.rds")
stdtMME <- dimnames(tgdtMME)$startdate

# Cargo datos de fechas extremas
ext <- read.csv("./viernes/ext/extMME.csv",stringsAsFactors = F)
#ext <- read.csv("./extMME.csv",stringsAsFactors = F)
colnombre <- c("TOTAL90","TOTAL10")

# Discrimino fechas no extremas (ni p10 ni p90)
#Busco las fehcas extremas
extrema = BuscarFechaExtrema(Ext = ext, Columna = colnombre,Startdate = stdtMME)
# Remuevo posiciones repetidas y ordeno de menor a mayor
extrema = sort(unique(extrema))

# Eligo solo las fechas no extremas y hago su predictibilidad, solo una vez
stdtMME_noext = stdtMME[-extrema]
tgdtMME_noext = tgdtMME[,-extrema]

predic_noext = EnsamblesPredictiblidad(Modelos=MODELOS,
                                       TgdtMod=targetdateMODELOS, 
                                       StdtMod=startdateMODELOS, 
                                       FechEnsam = stdtMME_noext,
                                       TgdtEnsam = tgdtMME_noext,
                                       FilePath = "./viernes/ext")
#saveRDS(predic_noext,"./ext/predic_noext.rds")
saveRDS(predic_noext,"./viernes/ext/predic_noext.rds")

for (p in colnombre) {
  
  # Busco las fehcas extremas
  extrema = BuscarFechaExtrema(Ext = ext, Columna = p,Startdate = stdtMME)
  # Remuevo posiciones repetidas y ordeno de menor a mayor
  extrema = sort(unique(extrema))
  
  # Evaluo en mme para separar en fechas extrema y no extremas
  stdtMME_ext = stdtMME[extrema]
  tgdtMME_ext = tgdtMME[,extrema]
  
  #Predictibilidad
  predic_ext = EnsamblesPredictiblidad(Modelos=MODELOS,
                          TgdtMod=targetdateMODELOS, 
                          StdtMod=startdateMODELOS, 
                          FechEnsam = stdtMME_ext,
                          TgdtEnsam = tgdtMME_ext,
                          FilePath = "./viernes/ext/")
  
  #predic_noext = readRDS("./ext/predic_noext.rds")
  
  # Guardo
  indice = substr(p,6,7)
  saveRDS(predic_ext, paste0("./viernes/ext/predict_ext",indice,".rds"))
  
}
# G R A F I C O S ------------------------------------

# si restas
# ext - no ext ------> positivo aporta mas las fechas ext

for (i in 1:length(colnombre)) {
  
  indice = colnombre[i]
  p = substr(indice,6,7)
  # Cargo los datos para la resta
  pred_ext <- readRDS(paste0("./viernes/ext/predict_ext",p,".rds"))
  pred_noext <- readRDS("./viernes/ext/predic_noext.rds")
  
  # Resta
  resta <- pred_ext - pred_noext
  
  # convierto a data frame para las 4 weeks
  dimnames(resta) <- list("x" = seq(265,330,1), "y" = rev(seq(-60,15,1)),
                             "week" = c(rep("Week 1",7),
                                        rep("Week 2",7),
                                        rep("Week 3",7),
                                        rep("Week 4",7)))
  df <- reshape2::melt(resta)
  colnames(df) <- c("x","y","week","z")
  g<-GraphDiscreteMultiple(Data=df,Breaks = seq(-0.4,0.4,0.1),Label = "ACC",Paleta = "RdBu",Direccion = -1)
  g<-g + ggtitle(paste0("Predictibilidad \n P",p," EXT-NO EXT \nT2MA (99-14, Oct-Apr)"))
  
  ggsave(filename = paste0("./viernes/ext/predic_restaMME_p",p,".png"),plot=g,width = 10, height = 4)
  
}





# ----------------------------------------------------------------------------
# Tabla con las fechas sin modelo o que se llena de vacio
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL")

Modelos = MODELOS
TgdtMod = targetdateMODELOS
FechEnsam = stdtMME
TgdtEnsam= tgdtMME
FilePath = "./predic"
StdtMod = startdateMODELOS

fecha_sin = c()
fecha_fal = c()

for (model in 1:nmodels) {
  FechEnsam = as.Date(FechEnsam)
  
  # Tomo el modelo a comparar contra la EM del resto de los mod
  mod_restante_stdate = StdtMod[[model]]
  mod_restante_tgdate = TgdtMod[[model]]
  MODELO_restante = Modelos[[model]]
  
  mod_ensamble_stdate = StdtMod[-model]
  mod_ensamble_tgdate = TgdtMod[-model]
  MODELOS_ensamble = Modelos[-model]
  nombres_ensamble = models[-model]
  for (i in 1:length(FechEnsam)) {
    
    # Semana y lead en cuestion del MME
    startweek = as.character(seq.Date(FechEnsam[i]-7,FechEnsam[i]-1,by=1)) #desde el sabado anterior al viernes
    leadMME = TgdtEnsam[,i]
    
    # MODELO RESTANTE ---------------------------------------------
    # Que startdate cae en la semana del MME para el modelo restante
    stdt_restante = mod_restante_stdate %in% startweek
    # Tomar la inicializacion mas cercana al sabado de pronostico
    if (sum(stdt_restante)>1) {stdt_restante = last(which(stdt_restante))}
    
    # Evaluo el modelo restante en las fechas que coincide con MME
    target_restante = mod_restante_tgdate[,stdt_restante] %in% leadMME
    modelo_objetivo_rest = MODELO_restante[,,target_restante,stdt_restante]
    
    # Llenar con NA si faltan dias 
    fechas = CompletarFaltanteStr(Target = target_restante, 
                                             Stdt = stdt_restante, 
                                             ModeloObjetivo = modelo_objetivo_rest,
                                             Startweek = startweek,
                                             ModelNombre = models[model])
    fecha_sin=append(fecha_sin,fechas[[1]])
    fecha_fal=append(fecha_fal,fechas[[2]])
    
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
      fechas = CompletarFaltanteStr(Target = target_restante, 
                                    Stdt = stdt_restante, 
                                    ModeloObjetivo = modelo_objetivo_rest,
                                    Startweek = startweek,
                                    ModelNombre = models[model])
      fecha_sin=append(fecha_sin,fechas[[1]])
      fecha_fal=append(fecha_fal,fechas[[2]])
      modelo_objetivo = CompletarFaltante(Target= target, 
                                          Stdt = stdt,
                                          ModeloObjetivo = modelo_objetivo,
                                          Startweek = startweek,
                                          ModelNombre = nombres_ensamble[mod])
      
      }
  }
  
}
