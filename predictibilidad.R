# Predictibilidad
# La idea es correlacionar un modelo con la media del ensamble formada por el resto de los modelos
# y luego esto repetirlo con cada modelo y sacar el promedio de esa correlación

# by Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------

# Limpiar enviroment 
rm(list=ls())

# Llamar paquetes

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/"

# Seteo el directorio
setwd(savepath)

# FUNCIONES ----------------------------------------------------------------------------------
Predictibilidad <- function(Modelo, Ensamble) {
  ## Modelo: array de 4 dimensiones (lon,lat,lead,startdate)
  ## Ensamble: array de 4 dimensiones (lon,lat,lead,startdate)
  ##  Ambos array deben tener las mismas dimensiones
  
  # Tomo las dimensiones de los array (para ambas son las mismas)
  lon = dim(Modelo)[1]
  lat = dim(Modelo)[2]
  lead = dim(Modelo)[3]
  starts = dim(Modelo)[4]
  
  # array a llenar
  acc = array(NA, dim = c(lon,lat,lead))
  
  # TIENE QUE HABER UNA MEJOR FORMA DE CORRELACIONAR 2 MATRICES EN LA TERCERA DIMENSION
  # Y OBTENER UN VALOR POR PUNTO
  
  for (l in 1:lead) { # por cada lead
    for (x in 1:lon) { # por cada longitud
      for (y in 1:lat) { # por cada latitud
        
        mod_punto = Modelo[x,y,l,]
        ens_punto = Ensamble[x,y,l,]
        acc[x,y,l] <- cor(mod_punto,ens_punto,use="pairwise.complete.obs",method = "pearson")
      }
      
    }
    
  }
  return(acc)
}
#------------------------------------------------------------------------------------------------
CompletarFaltante <- function(Target, Stdt, ModeloObjetivo) {
  ## Target: Vector logico
  ## Stdt: Vector logico
  ## ModeloObjetico: array de 3 dimensiones que deberia completarse con 66,76 y 28 lead
  
  # Si el modelo no alcanza a llenar los 28 dias del MME, llenar el resto con NA
  if (sum(Target)<28) {
    faltante = 28 - sum(Target)
    mod.faltante = array(NA, dim = c(66,76,faltante))
    
    # PRUEBA: QUE PASA CUANDO NO HAY MODELO ESA SEMANA
    if (sum(Stdt)==0){
      #print(paste("La semana ",startweek[1]," no tiene el modelo",models[mod]))
      
      modelo_objetivo = mod.faltante
    }else{
      #print(paste("La semana ",startweek[1]," le faltan dias del modelo",models[mod]))
      
      modelo_objetivo = abind(ModeloObjetivo,mod.faltante)
    }
  }
  
  # Si el modelo esta completo no hacer nada
  if (sum(Target)==28) {
    modelo_objetivo = ModeloObjetivo
  }
  return(modelo_objetivo)
}
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
# Cargo los datos 
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL")
nmodels = length(models)
# Cargo la media del ensamble aparte
MME = readRDS("./MME_OM.rds")
tgdtMME = readRDS("./targetdate_MME_ONDEFM.rds")
stdtMME = dimnames(tgdtMME)$startdate

MODELOS <- list()
targetdateMODELOS <- list()
startdateMODELOS <- list()

for (m in 1:nmodels) {
  MODELOS[[m]] <- readRDS(paste0("./model_",models[m],"_ONDEFM.rds"))
  targetdateMODELOS[[m]] <- readRDS(paste0("./targetdate_",models[m],"_ONDEFM.rds"))
  startdateMODELOS[[m]] <- dimnames(targetdateMODELOS[[m]])$startdate
}


# ------------------------------------------------------------------------------------
# Predictibilidad solo para eventos MJO activos

# Cargo los datos de eventos
df_rmm <- readRDS("./MJO/df_rmm.rds")
fechas_act <- as.character(df_rmm$DATE)

# Restrinjo los datos solo a las fechas donde hubo eventos activos de MJO
MODELOSMJO <- list()
tgdtMJO <- list()
stdtMJO <- list()
for (m in 1:nmodels){
  mod <- MODELOS[[m]]
  tgdt <- targetdateMODELOS[[m]]
  stdt <- startdateMODELOS[[m]]
  
  # Busco que startdates coinciden con los eventos activos
  posMJO = stdt %in% fechas_act
  MODELOSMJO[[m]] <- mod[,,,posMJO]
  tgdtMJO[[m]] <- tgdt[,posMJO]
  stdtMJO[[m]] <- stdt[posMJO]
}
# Restrinjo las fechas del Ensamble tmb
posMJOmme <- stdtMME %in% fechas_act
MMEMJO <- MME[,,,posMJOmme]
tgdtMMEMJO <- tgdtMME[,posMJOmme]
stdtMMEMJO <- as.Date(stdtMME[posMJOmme])

# Predictibilidad ahora solo con los startdates durante eventos activos MJO
predicti <- array(NA, dim = c(66,76,4,nmodels))

for (model in 1:nmodels) {
  
  # Tomo el modelo a comparar contra la EM del resto de los mod
  mod_restante_stdate = stdtMJO[[model]]
  mod_restante_tgdate = tgdtMJO[[model]]
  MODELO_restante = MODELOSMJO[[model]]
  
  mod_ensamble_stdate = stdtMJO[-model]
  mod_ensamble_tgdate = tgdtMJO[-model]
  MODELOS_ensamble = MODELOSMJO[-model]
  
  # array a completar 
  MME_nmenos1 <- array(NA, dim = c(66,76,28,(nmodels-1),length(stdtMMEMJO)))
  mod_aparte <- array(NA, dim = c(66,76,28,length(stdtMMEMJO)))
  
  for (i in 1:length(stdtMMEMJO)) { # Por cada sabado
    
    # Semana y lead en cuestion del MME
    startweek = as.character(seq.Date(stdtMMEMJO[i]-7,stdtMMEMJO[i]-1,by=1)) #desde el sabado anterior al viernes
    leadMME = tgdtMMEMJO[,i]
    
    # MODELO RESTANTE ---------------------------------------------
    # Que startdate cae en la semana del MME para el modelo restante
    stdt_restante = mod_restante_stdate %in% startweek
    # Tomar la inicializacion mas cercana al sabado de pronostico
    if (sum(stdt_restante)>1) {stdt_restante = last(which(stdt_restante))}
    
    # Evaluo el modelo restante en las fechas que coincide con MME
    target_restante = mod_restante_tgdate[,stdt_restante] %in% leadMME
    modelo_objetivo_rest = MODELO_restante[,,target_restante,stdt_restante]
    
    # Llenar con NA si faltan dias 
    modelo_objetivo_rest = CompletarFaltante(Target = target_restante, 
                                             Stdt = stdt_restante, 
                                             ModeloObjetivo = modelo_objetivo_rest)
    
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
      modelo_objetivo = CompletarFaltante(target, stdt, modelo_objetivo)
      
      
      # Guardo
      MME_nmenos1[,,,mod,i] <- modelo_objetivo
      mod_aparte[,,,i] <- modelo_objetivo_rest
      
    } # End loop media ensamble
    
  } # End loop sabados  
  # Promedio sobre los modelos (cuarta dimension)
  MME_pro = apply(MME_nmenos1 , c(1,2,3,5), mean, na.rm = T)
  
  predicti[,,,model] <- Predictibilidad(mod_aparte, MME_pro, length(stdtMMEMJO))
  
} # End loop models

# GUARDO
saveRDS(predicti, "./MJO/predict_MJO.rds")

# G R A F I C O S ------------------------------------

for (m in 1:nmodels) {
  
  # Renombro dimensiones 
  data = predicti[,,,m]
  dimnames(data) <- list(x = seq(265,330,1), 
                         y = rev(seq(-60,15,1)), 
                         week = c("Week 1", "Week 2","Week 3", "Week 4"))
  
  # Armo data.frames para graficar
  df <- reshape2::melt(data, value.name = "z")
  
  titl = paste0("Predictibilidad MJO ", models[m], "\ntasa (99-15, Oct-Mar)")
  g <- GraphDiscreteMultiple(df, Breaks = seq(0,1,0.2) , Label = "ACC", Paleta = "YlOrRd",Direccion = "1")
  fig <- grid.arrange(g, ncol = 1,top = textGrob(titl,gp=gpar(fontsize=13,font=3)))
  fn <- paste0("/home/lucia.castro/SubX_processed_Rdata/MJO/predic_MJO_",models[m],".png")
  ggsave(filename=fn,plot=fig,width = 10, height = 4)
}

#----------------------------------------------------------------------------------------
# Ahora grafico la diferencia entre la predictibilidad total y la predictiblidad 
# de solo los eventos activos de MJO

# cargo predictibilidad total
predtotal <- readRDS("./predict.rds")
predMJO <- readRDS("./MJO/predict_MJO.rds")

pred_diff <- predtotal - predMJO

for (g in 1:nmodels) {
  df = ggScoreSemanal(pred_diff[,,,g])
  
  title = paste0("Predictibilidad TOTAL - MJO ", models[g], "\ntasa (99-15, Oct-Mar)")
  gr <- GraphDiscreteMultiple(df, Breaks = seq(-0.1,0.1,0.02) , Label = "ACC", Paleta = "RdBu",Direccion = "-1")
  fig <- grid.arrange(gr, ncol = 1,top = textGrob(title,gp=gpar(fontsize=13,font=3)))
  fn <- paste0("/home/lucia.castro/SubX_processed_Rdata/MJO/predicdiff_MJO_",models[g],".png")
  ggsave(filename=fn,plot=fig,width = 10, height = 4)
}
