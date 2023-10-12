# MJO analisis con los modelos. 

# ---------------------------------------------------------------------------------
# Limpiar el espacio
rm(list=ls())

# cargar paquetes
library(metR)
library("grid")
library(RNetCDF)
library(raster)
library('pracma')
library("gridExtra")
library(data.table)
library("ggplot2")
library(dplyr)


# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata/model/viernes")

groups=c('GMAO','RSMAS','ESRL','ECCC','NRL','EMC','MME')                       
models=c('GEOS_V2p1','CCSM4','FIMr1p1','GEM','NESM','GEFS','SAT')   

var_mjo = c("RMM1","RMM2","amplitude","phase")


# # DATOS DE RMM DE LOS MODELOS 
# # Directorio donde guardar
# outPath = '/datos/SubX/hindcast/rmm/'
# 
# inFname = "/datos/SubX/hindcast/rmm/Amplitude/NRL_amp.nc"
# data = metR::ReadNetCDF(inFname, out='array')
# data = data[[1]]
# 
# dimnames(data)$S <- as.character(seq.Date(as.Date("1999-01-01"),as.Date("2015-12-30"),1))


# Supongo que donde hubo MJO activo para las observaciones, el modelo tambien tiene evento activo
# Me interesa solo donde se inicio el modelo con evento activo
# Podria separar por fases al iniciar 

# Cargo los datos de eventos
df_rmm <- readRDS("/home/lucia.castro/SubX_processed_Rdata/model/MJO/df_rmmOA.rds")
df_eventos <- readRDS("/home/lucia.castro/SubX_processed_Rdata/model/MJO/df_eventosOA.rds")
fechas_act <- as.character(df_rmm$DATE)
# Cargo datos del rho1 para la significancia
rho1 <- readRDS("../rho1.rds")

for (g in 1:length(groups)) {
  grupo = groups[g]
  model = models[g]
  # Busco que startdates coinciden con los eventos activos
  TargetDate <- readRDS(paste0("./targetdate_",grupo,"_OA.rds"))
  startdate = dimnames(TargetDate)$startdate
  posMJO = startdate %in% fechas_act
  
  modelweek<-readRDS(paste0("./modelweek_",grupo,".rds"))
  obsweek<-readRDS(paste0("./obsweek_",grupo,".rds"))
  
  modMJO = modelweek[,,!posMJO,] #para inactive !posMJO
  obsMJO = obsweek[,,!posMJO,]
  
  # Cantidad de inicios antes y despues de restringir en los eventos
  nstartdate = length(startdate)
  nstartdateMJO = sum(posMJO)
  
  # Calculo las distintas métricas por cada lon/lat/targetweek
  dif = (obsMJO - modMJO)
  me = apply(dif, c(1,2,4),FUN = mean, na.rm = TRUE)
  mae = apply(abs(dif), c(1,2,4), FUN = mean, na.rm = TRUE) 
  rmse = sqrt(apply(dif^2,c(1,2,4), FUN = mean, na.rm = TRUE))
  desvio = apply(dif,c(1,2,4),FUN = sd, na.rm = TRUE)
  var = (1-(sqrt(rmse))/desvio)
  acc = ACC(Lon=66, Lat = 76, Model = modMJO, Anom = obsMJO) # tarda un cacho
  
  # Calculo estadistico de prueba
  # RHO1 sirve para todas las weeks (1,2.3 y 4). Repito el tamaño de muestra para cada semana
  n_eff = nstartdate*((1 - rho1)/(1 + rho1))
  n_eff = array(n_eff, dim = c(66,76,4))
  t = (acc * sqrt(n_eff - 2)) / sqrt(1-acc^2)
  
  # Significancia de 0.05 y grados de libertad 
  critc = qt(p=0.95, df = trunc(n_eff))
  test = t < critc
  
  # Renombro dimensiones 
  dt.me = ggScoreSemanal(me)
  dt.mae = ggScoreSemanal(mae)
  dt.rmse = ggScoreSemanal(rmse)
  dt.var = ggScoreSemanal(var)
  dt.acc = ggScoreSemanal(acc)
  
  metrics <- list(rmse,me,acc,var,test)
  saveRDS(metrics, paste0("./MJO/metricsMJO_inact_",grupo,".rds"))
  #---------------------------------------------------------------------------------------
  #  Gráficos  
  #---------------------------------------------------------------------------------------
  
  #g1 <- GraphDiscreteMultiple(Data = dt.rmse, Breaks = seq(0,3,0.25),Label = "RMSE",Paleta = "YlOrRd", Direccion = "1")
  #g2 <- GraphDiscreteMultiple(Data = dt.me, Breaks = seq(-0.1,0.1,0.025), Label = "ME",Paleta = "RdBu",Direccion = "-1")
  #g3 <- GraphMultiplePuntos(Data = dt.acc, ArLogic = test, Breaks = seq(0,1,0.20), Label = "ACC",Paleta = "YlGn",Direccion = "1")
  #g4 <- GraphDiscreteMultiple(Data = dt.var, Breaks = seq(-0.5,0.5,0.10), Label = "NRMSE",Paleta = "RdBu",Direccion = "-1")
  
  
  
  #title = title <- paste("SubX ",grupo,"-",model," Inicios",nstartdateMJO,"/",nstartdate,
  #                       "\nMJO inactive events T2MA(99-14, Oct-Abr) ")
  #fig <- grid.arrange(g1,g2,g3,g4, ncol = 1,top = textGrob(title,gp=gpar(fontsize=13,font=3)))
  #ggsave(filename=paste0("./MJO/scores_map_MJO_inact_",grupo,".png"),plot=fig,width = 10, height = 15)
}


# Hacer la resta entre los scores inactivos y cuando hay evento activo
# TOTAL

for (g in 1:length(groups)) {
  grupo = groups[g]
  model = models[g]
  # Busco que startdates coinciden con los eventos activos
  TargetDate <- readRDS(paste0("./targetdate_",grupo,"_ONDEFM.rds"))
  startdate = dimnames(TargetDate)$startdate
  posMJO = startdate %in% fechas_act
  # Cantidad de inicios antes y despues de restringir en los eventos
  nstartdate = length(startdate)
  nstartdateMJO = sum(posMJO)
  
  # Cargo los scores
  metricINA <- readRDS(paste0("./metricsMJO_inact_",grupo,".rds"))
  metricACT <- readRDS(paste0("./metricsMJO_",grupo,".rds"))
  
  # Hago la resta de cada metrica
  metricANOM <- list()
  for (m in 1:length(metricMOD)) {
    metric <- metricACT[[m]] - metricINA[[m]]
    df <- ggScoreSemanal(metric)
    metricANOM[[m]] <- df
  }
  #---------------------------------------------------------------------------------------
  #  Gráficos  
  #---------------------------------------------------------------------------------------

  test = array(NA, c(66,76,4))
  # Como es (todo - mjo) significa que cuando es negativo el mjo tiene valor mayor
  
  g1 <- GraphDiscreteMultiple(Data = metricANOM[[1]], Breaks = seq(-0.2,0.2,0.05),Label = "RMSE",Paleta = "RdBu", Direccion = "-1")
  g3 <- GraphMultiplePuntos(Data = metricANOM[[3]], ArLogic = test, Breaks = seq(-0.2,0.2,0.05), Label = "ACC",Paleta = "RdBu",Direccion = "-1")
  
  

  title = paste0("SubX ",grupo,"-",model,"\nMJO activo - inactivo (99-15, Oct-Mar)")
  fig <- grid.arrange(g1,g3, ncol = 1,top = textGrob(title,gp=gpar(fontsize=13,font=3)))
  ggsave(filename=paste0("/home/lucia.castro/SubX_processed_Rdata/MJO/scores_map_MJO_dff",grupo,".png"),plot=fig,width = 10, height = 15)
  
}

# Si resto activo - inactivo
# donde rmse sea positivo siginifica que fue mayor en activo ----> MAL
# donde acc sea positivo significa que fue mayor en activo -----> BIEN
# Seteo los parametros de mapa y gradiente 

# Separar segun la fase inicial
# Bins = [8,1] [2,3] [4,5] [6,7]
# Quiero hacer la diferencia entre activos - inactivos para C/bin

Bins = levels(df_eventos$Bin)
nstartdateMJOBIN <- list()
listagraficosRMSE <- list()
listagraficosACC <- list()


for (g in 1:length(groups)) {
  grupo = groups[g]
  model = models[g]
  
  # Cargo las fechas del modelo
  TargetDate <- readRDS(paste0("./targetdate_",grupo,"_OA.rds"))
  startdate = dimnames(TargetDate)$startdate
  # Cargo datos de modelo y observaciones
  modelweek<-readRDS(paste0("./modelweek_",grupo,".rds"))
  obsweek<-readRDS(paste0("./obsweek_",grupo,".rds"))
  metricINA <- readRDS(paste0("./MJO/metricsMJO_inact_",grupo,".rds"))[c(1,3)] #Leo rmse y acc solo
  
  for (b in Bins) { # por cada Bin
    # Extraigo las fases de ese bin
    f1 = as.numeric(substr(b,5,5))
    f2 = as.numeric(substr(b,6,6))
    # Busco que startdates coinciden con los eventos activos
    fechas_act_bin <- as.character(df_rmm[FASE==f1 |FASE==f2 ,DATE])
    posMJOBIN = startdate %in% fechas_act_bin

    # Cantidad de inicios antes y despues de restringir en los eventos
    # informacion para luego crear tabla
    nstartdate = length(startdate)
    nstartdateMJOBIN <- c(nstartdateMJOBIN,sum(posMJOBIN))
    
    # Calculo las distintas métricas por cada lon/lat/targetweek
    sco_bin<-Metrics(obsweek[,,posMJOBIN,],modelweek[,,posMJOBIN,])
    saveRDS(sco_bin,paste0("./MJO/ScoresBins/",grupo,b))
    resta_bin <- Map('-', sco_bin, metricINA) #Resto ambas listas 
    
    # Guardar la diferencia
    saveRDS(resta_bin,paste0("./MJO/ScoresBins/diff_FI_",grupo,b))
    
    # Graficos
    g1 <- GraphDiscreteMultiple(Data = ggScoreSemanal(resta_bin[[1]]), Breaks = seq(-0.2,0.2,0.05),Label = "RMSE",Paleta = "RdBu", Direccion = "1") 
    g2 <- GraphDiscreteMultiple(Data = ggScoreSemanal(resta_bin[[2]]), Breaks = seq(-0.2,0.2,0.05), Label = "ACC",Paleta = "RdBu",Direccion = "-1")

    # Lo completo asi en vez de append porque sino guarda listas en vez de ggplots
    len <- length(listagraficosRMSE)
    listagraficosRMSE[[len+1]] <- g1 + theme(legend.position = "none") 
    listagraficosACC[[len+1]] <- g2 + theme(legend.position = "none") 
    
 
  }# End Bin
}# End model


# PRUEBA DE FACET GRID 
# Si resto activo - inactivo
# donde rmse sea positivo siginifica que fue mayor en activo ----> MAL
# donde acc sea positivo significa que fue mayor en activo -----> BIEN
# leer datos pasar la info a un dataframe
Bins = levels(df_eventos$Bin)

# DATA FRAME a unir todo: modelos, bins, week, metric
df <- data.frame()

for (m in groups){
  for (b in Bins) {
    df_mod <- reshape2::melt(readRDS(paste0("./MJO/ScoresBins/diff_FI_",m,b)))
    # agrego columnas
    df_mod$mod <- rep(m, nrow(df_mod))
    df_mod$bin <- rep(b, nrow(df_mod))
    df <- rbind(df,df_mod)
  }
}
df = rename(df, "metric" ="L1")
dt<-as.data.table(df)

# Uso factors para cambiar el orden de los models
library(stringr)

dt = FactorsModelsPretty(DF = dt, Col ="mod")


# Le cambio el nombre de las metricas
met_str = c('rmse'='RMSE','acc'='ACC')
dt$metric <- str_replace_all(dt$metric, met_str)

# Le cambio el nombre a las semanas
dt = WeeksToSemanas(DF = dt, Col = "week")

# Le cambio el nombre a las fases
fases = c('Fase81'='*Fases* **8** *y* **1**',
          'Fase23'='*Fases* **2** *y* **3**',
          'Fase45'='*Fases* **4** *y* **5**',
          'Fase67'='*Fases* **6** *y* **7**')

dt$bin <- str_replace_all(dt$bin, fases)



# Graficado y guardado

for (w in c( "Semana 2", "Semana 3")) { #por cada semana
  for (mt in unique(dt$metric)) { # para rmse y acc
    
    # Restrinjo y grafico
    data = dt[week == w & metric == mt]
    
    if (mt == "RMSE") {
      titulo = paste0("<span style = 'font-size:25pt; font-family:Helvetica;'>RMSE de la **",w,"** Proyecto SubX 
    </span><br>*Segun región y fase inicial de Madden-Julian (99-14, Oct-Abr)* ")
    } else if (mt == "ACC") {
      titulo = paste0("<span style = 'font-size:25pt; font-family:Helvetica;'>ACC de la **",w,"** Proyecto SubX 
    </span><br>*Segun región y fase inicial de Madden-Julian (99-14, Oct-Abr)* ")
    }
    
    if (mt == "ACC") {
      data$value=data$value*100
      breks = seq(-50,50,10)
      leg.title = "p.p."
    } else if (mt == "RMSE") {
      breks = seq(-0.5,0.5,0.10)
      leg.title = "°C"
    }
    
    
    # si haces el rmse tenes que poner direccion 1
    # si haces acc tenes que poner direccion -1
    if (mt=="RMSE") {direccion = 1
    } else {direccion = -1}
    
    fig<-GraphMJOCondLegend(Data=data, 
                            Breaks = breks, 
                            Paleta = "RdBu",
                            Direccion = direccion,
                            Row = "MODEL",
                            Col = "bin" ,
                            Titulo = titulo,
                            Label = leg.title)


      
    # Guardado
    sem = substr(w,8,8)
    ggsave(filename=paste0("./MJO/ScoresBins/scoresFIv3_MJODIFF_",sem,"_",mt,".png"),
           plot=fig,width = 10, height = 17)
  }
}

# Grafico de solo MME para S2S Summit
dt_mme=dt[MODEL=="**MME**"]

for (w in c( "Semana 2", "Semana 3")) { #por cada semana
  for (mt in unique(dt$metric)) { # para rmse y acc
    
    # Restrinjo y grafico
    data = dt_mme[week == w & metric == mt]
    
    if (mt == "RMSE") {
      titulo = paste0("<span style = 'font-size:14pt; font-family:Helvetica;'>RMSE de la **",w,"** Proyecto SubX 
    </span><br>*Segun región y fase inicial de Madden-Julian (99-14, Oct-Abr)* ")
    } else if (mt == "ACC") {
      titulo = paste0("<span style = 'font-size:14pt; font-family:Helvetica;'>ACC de la **",w,"** Proyecto SubX 
    </span><br>*Segun región y fase inicial de Madden-Julian (99-14, Oct-Abr)* ")
    }
    
    if (mt == "ACC") {
      data$value=data$value*100
      breks = seq(-50,50,10)
    } else if (mt == "RMSE") {
      breks = seq(-0.5,0.5,0.10)
    }
    
    
    # si haces el rmse tenes que poner direccion 1
    # si haces acc tenes que poner direccion -1
    if (mt=="RMSE") {direccion = 1
    } else {direccion = -1}
    
    fig<-GraphMJOCondLegend(Data=data, 
                            Breaks = breks, 
                            Paleta = "RdBu",
                            Direccion = direccion,
                            Row = "MODEL",
                            Col = "bin" ,
                            Titulo = titulo)
    
    
    # Guardado
    sem = substr(w,8,8)
    ggsave(filename=paste0("./MJO/ScoresBins/MME_mjodiff_",sem,"_",mt,".png"),
           plot=fig,width = 7, height = 4.5)
  }
}
# Funcion para graficar las metricas de MJO en mjo_mod y mjo_fases con una leyenda condicional

  ## Data: un data frame de al menos 3 dimensiones para realizar el mapa. Primer dim son las long repetidas la cantidad
  # de veces de las latitudes, Segunda dim son las lat repetidas la cantidad de veces de las longitudes y Tercera dim 
  # son los valores
  ## Breaks: un vector con los numeros para discretizar la barra de colores. Ej c(0,5,10,20)
  ## Titulo: character vector con el titulo del grafico
  ## Paleta: character vector que indica una paleta existente. Ej "RdBu"
  ## Direccion : numero 1 o -1 para indicar si se revierte la paleta. 
  ## Row, Col: string con las columnas segun hacer facet grid
  
  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
library(ggtext)
data = dt[week == "Semana 2" & metric == "RMSE"]
data$MODEL<- str_replace_all(data$MODEL, rep_str2)

Data=data 
Breaks = seq(-0.5,0.5,0.10)
Paleta = "RdBu"
Direccion = 1
Titulo = ""
Row = "MODEL"
Col = "bin"
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$value, na.rm = T)
  max <- max(Data$value, na.rm = T)
  Data$value=oob_squish(Data$value,range = c(min(Breaks),max(Breaks)))
  
  # Aqui extiendo un poco la escala para que cubra todo
  fillbreaks = Breaks
  fillbreaks[length(Breaks)] <- max(Breaks)*1.1
  fillbreaks[1] <- min(Breaks)*1.1
  
  # Grafico en si 
  g<-  ggplot() +                                          # o Breaks   
    geom_contour_fill(data=Data,aes(lon, lat, z = value),breaks = fillbreaks) +
    scale_x_longitude(breaks = c(280,300, 320),expand = c(0.09, 0.09)) +
    scale_y_latitude(breaks = c(-40,-20,0),expand = c(0.09, 0.09)) +
    scale_fill_distiller(palette=Paleta,direction= Direccion,
                         na.value = "transparent",
                         breaks = Breaks,
                         limits = c(min(Breaks), max(Breaks)),
                         guide = guide_colorstrip(),
                         oob  = scales::squish) +
    geom_map(dat=mapa, map = mapa, aes(map_id=region), fill="NA", color="black", inherit.aes = F)+
    MarianoTheme + 
    # Ejes del Facet grid
    theme(strip.text.x = element_markdown(size=18),
          strip.text.y = element_markdown())+
    facet_grid( get(Row) ~ get(Col))  +   # esto es nuevo row ~ col
    coord_cartesian()  +
    # Leyenda
    theme(legend.position  = "bottom",
          legend.title = element_blank(),
          legend.key.size = unit(1.3,"cm"),
          legend.text = element_text(size = 10))+
    
    # Titulo
    labs(title = Titulo)+
    theme(plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0.5))
  
  g
  # Ahora agrego leyenda
  legend <- LeyendaCondicional(Breaks, Paleta, Labels = c("MJO APORTA", "MJO NO APORTA"))
  grid.arrange(g, legend,
               ncol=1, nrow = 2, 
               widths = c(3), heights = c( 2.5,0.2))

