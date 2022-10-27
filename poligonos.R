# Analisis zona SACZ y SUR PATAGONIA
# 
#
# by Lucia M Castro
#-----------------------------------------------------------------------------------------------------------------------

# Limpio enviroment
rm(list=ls())

# Seteo directorio
setwd("/home/lucia.castro/")

# Cargo paquetes
library(secr)
library(reshape2)
library(csv)
library(dplyr)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# -------------------------------------------------------------------------------------
# Funcion que devuelve las fechas donde se alcanzaron ciertos percentiles
FechasPercentiles <- function(DF,Variable) {
  ## DF: Dataframe con la primera columna de fechas y alguna columna con valores
  ## Variable: columna de dataframe con los valores 
 
  
  percentil = quantile(Variable, c(0.1,0.9))
  
  # Busco solo valores mayor al P90 y menores al P10
  p10 = Variable < percentil[1]
  p90 = Variable > percentil[2]
  
  # Evalua el dataframe en las filas TRUE y tomo la primer columna (fechas)
  fechas10 <- DF[p10,][,1]
  fechas90 <- DF[p90,][,1]
  
  
  return(list(fechas10,fechas90))
}
# -----------------------------------------------------------------------------------
# Funcion que dado un data frame realiza un promedio espacial pesado por la latitud
# Devuelve data table con los valores unicos 
DTPromEspacPesado <- function(DF,Variable,Grupo) {
  ## DF: Data frame 
  ## Variable: Columna del data frame con el valor a promediar
  ## Grupo: Una o mas columnas del data frame que tener en cuenta para hacer 
  ## el promedio. Ej: c(Semanas, Region)
  
  # Promedio en lat y long. Para eso paso a data table
  dt = as.data.table(DF)
  
  # Peso por la latitud 
  dt = dt[,varlat := get(Variable)*cos(lat*pi/180)]
  
  # Hago el promedio y elimina filas repetidas. Luego le cambio el nombre
  # a la columna porque queda "get"
  promedio = dt[,list(media=mean(varlat,na.rm=TRUE)),by=.(get(Grupo))]
  setnames(promedio, "get", Grupo)
  
  return(promedio)
}
#-----------------------------------------------------------------------------------------
GraphLineMultiple <- function(Data, Varx, Vary , Region) {
  ## Data: Data frame con los datos. Debe tener una columna llamanda "week" para hacer las separaciones
  ## VarX: La columna del data frame con los valores numericos
  ## VarY: La columna del data frame con los valores del eje y
  ## Region: character diciendo que region es. "SACZ" o "PATAGONIA"
  
  if (Region == "SACZ") {
    p10 = -0.9387607
    p90 = 1.1146134}
  if (Region == "PATAGONIA") {
    p10 = -1.466749
    p90 = 1.562939}
  
  
  # Cargo paquetes
  library(ggplot2)
  
  
  ggplot(data = Data, aes(x = get(Varx), y = get(Vary))) +
    geom_line(color = "darkolivegreen3", size = 0.7) +
    geom_hline(yintercept=p10, linetype="dashed", color = "firebrick") +
    geom_hline(yintercept=p90, linetype="dashed", color = "firebrick") +
    ylim(c(-5.,5.)) +
    
    theme(axis.text=element_text(size=12))+
    theme(strip.text.x = element_text(size = 12, colour = "black"))+
    facet_wrap( .~ week) +
    
    theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
    theme(panel.background = element_rect(fill = "white",colour = "grey70")) +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.ticks.x=element_blank())
}
#-----------------------------------------------------------------------------------------

# Cargo datos obs y modelo
grupos = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
models = c('FIMr1p1','GEM','GEFS','GEOS_V2p1','CCSM4','NESM',"")
nmodels = length(models)

# Lista con los datos a llenar
modelweek <- list()
obsweek <- list()

for (i in 1:nmodels) {
  inpath = paste0("./SubX_processed_Rdata/modelweek_",grupos[i],".rds")
  modelweek[[i]] <- readRDS(inpath)
}

for (i in 1:nmodels) {
  inpath = paste0("./SubX_processed_Rdata/obsweek_",grupos[i],".rds")
  obsweek[[i]] <- readRDS(inpath)
}

# Poligonos. Lon de menor a mayor, el primer punto se repite para cerrar el poligono
SP <- data.frame(x_coords = c(293,289,290,293,298,293),
                 y_coords = c(-28,-35,-50,-50,-40,-28))

SACZ <- data.frame(x_coords = c(305,305,312,319,323,305),
                   y_coords = c(-10,-20,-24,-25,-10,-10))




# Listas de percentiles a llenar
lista.modelos.sacz <- list()
lista.modelos.sp <- list()

for (mod in 1:nmodels) {
  
  # Cantidad de fechas de inicializacion del modelo
  modelo = modelweek[[mod]]
  obs = obsweek[[mod]]
  inicios <- dim(modelo)[3]
  
  # Convierto en Data frame 
  df.mod = melt(modelo)
  df.obs = melt(obs)
  
  # Junto obs y modelo en mismo df
  df <- rbind(df.mod, df.obs )
  df$fuente = c(rep("Modelo",nrow(df.mod)),
                rep("CPC",nrow(df.obs)))
  rm(df.obs,df.mod,modelo,obs)
  
  # Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
  puntossacz=pointsInPolygon(df[,1:2],SACZ) 
  puntossp=pointsInPolygon(df[,1:2],SP) 
  df_sacz = df[puntossacz,]
  df_sp = df[puntossp,]
  
  # Promedio en lat y long. Para eso paso a data table
  dt_sacz = as.data.table(df_sacz)
  dt_sp = as.data.table(df_sp)
  dt = as.data.table(df)
  
  # Peso por la latitud 
  dt_sacz = dt_sacz[,varlat := value*cos(lat*pi/180)]
  dt_sp = dt_sp[,varlat := value*cos(lat*pi/180)]
  dt = dt[,varlat := value*cos(lat*pi/180)]
  med_sacz = dt_sacz[,media:=mean(varlat,na.rm=TRUE),by=.(start,week,fuente)]
  med_sp = dt_sp[,media:=mean(varlat,na.rm=TRUE),by=.(start,week,fuente)]
  med = dt[,media:=mean(varlat,na.rm=TRUE),by=.(start,week,fuente)]
  med_sacz$lon = NULL
  med_sacz$lat = NULL
  med_sacz$value = NULL
  med_sacz$varlat= NULL
  med_sp$lon = NULL
  med_sp$lat = NULL
  med_sp$value = NULL
  med_sp$varlat= NULL
  med$lon = NULL
  med$lat = NULL
  med$value = NULL
  med$varlat= NULL
  
  # Ahora elimino las filas duplicadas 
  med_sacz = unique(med_sacz, incomparables=FALSE, fromLast=FALSE)
  med_sp = unique(med_sp, incomparables=FALSE, fromLast=FALSE)
  med = unique(med, incomparables=FALSE, fromLast=FALSE)
  
  # Guardo datos en lista
  lista.modelos.sacz[[mod]] <- med_sacz[fuente == "Modelo"]
  lista.modelos.sp[[mod]] <- med_sp[fuente == "Modelo"]
  
  # #------------------------------- G R A F I C O S -----------------------------------
  # #Graficar histogramas
  # intervalos = seq(-5.0,5.0,2.5)
  # g1 <- GraphHistMultiple(Data = med_sacz, Var = "media", Breaks = intervalos, 
  #                         N = inicios, LabelY = "SACZ", LimY = c(0,0.6))
  # g2 <- GraphHistMultiple(Data = med_sp, Var = "media",Breaks = intervalos, 
  #                         N = inicios, LabelY = "PATAGONIA",LimY = c(0,0.6))
  # 
  # title = paste0("SubX ", grupos[mod],"-",models[mod], " T2MA (99-15, Oct-Mar)")
  # fig <- grid.arrange(g1,g2, ncol = 1,top = textGrob(title,gp=gpar(fontsize=13,font=3)))
  # fname <- paste0("./SubX_processed_Rdata/hist2_",grupos[mod],".png")
  # ggsave(filename=fname,plot=fig,width = 15, height = 10)
  # 
  # # Para todo el terreno
  # g3 <- GraphHistMultiple(Data = med, Var = "media" ,Breaks = intervalos, 
  #                         N = inicios, LabelY = "TOTAL", LimY = c(0,1))
  # fig2 <- grid.arrange(g3, ncol = 1,top = textGrob(title,gp=gpar(fontsize=13,font=3)))
  # fname2 <- paste0("./SubX_processed_Rdata/histtotal_",grupos[mod],".png")
  # ggsave(filename=fname2,plot=fig2,width = 15, height = 10)
  # 
  # # Graficos de linea 
  # # conviero a Date primero
  # med_sacz$start <- as.Date(med_sacz$start)
  # med_sp$start <- as.Date(med_sp$start)
  # l1 <- GraphLineMultiple(Data = med_sacz, Varx = "start", Vary = "media", Region = "SACZ" )
  # l2 <- GraphLineMultiple(Data = med_sp, Varx = "start", Vary = "media", Region = "PATAGONIA" )
  # 
  # fig3 <- grid.arrange(l1,l2, ncol = 1,top = textGrob(title,gp=gpar(fontsize=13,font=3)))
  # fname <- paste0("./SubX_processed_Rdata/line_",grupos[mod],".png")
  # ggsave(filename=fname,plot=fig3,width = 15, height = 10)
  
 
}

saveRDS(lista.modelos.sacz,"./SubX_processed_Rdata/poligonos/poli.sacz.rds")
saveRDS(lista.modelos.sp,"./SubX_processed_Rdata/poligonos/poli.sp.rds")



# Percentiles. Observaciones
# cargo datos
ar.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")
lon = dimnames(ar.anom)$lon
lat = dimnames(ar.anom)$lat

# Acomodo datos para hacer medias semanales
dias = dimnames(ar.anom)$day
inisem = seq.Date(as.Date("1999-01-01"),as.Date("2015-12-31"),by=7)

# Extraigo solo las fechas de startdate desde Octubre a Marzo
OM = c(1,2,3,10,11,12)
#oct_mar <- which(month(dias) %in% OM) # posiciones donde el mes cae entre Octubre a Marzo
inisemOM <- inisem[which(month(inisem) %in% OM)]
#ar.anom.OM <- ar.anom[,,oct_mar]

# Variable a guardar
anom.sem = array(NA, dim = c(66,76,7,length(inisemOM)))

# OJO: la ultima semana no tiene 7 dias 
for (sem in 1:length(inisemOM)) {
  # Busco las fechas
  semana = as.character(seq.Date(inisemOM[sem],inisemOM[sem]+6,by=1))
  posicion = which(dias %in% semana)
  
  # Guardo
 anom.sem[,,,sem] = ar.anom[,,posicion]
  
}
# Promedio en la semana
anom.media = apply(anom.sem, c(1,2,4), mean)

# Convierto en Data frame 
dimnames(anom.media) <- list("lon" = lon, "lat" = lat, "semana" = as.character(inisemOM))
df.anom = melt(anom.media)

# Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
obssacz=pointsInPolygon(df.anom[,1:2],SACZ) 
obssp=pointsInPolygon(df.anom[,1:2],SP) 
obs_sacz = df.anom[obssacz,]
obs_sp = df.anom[obssp,]

# Promedio en lat y long
med_obs_sacz = DTPromEspacPesado(obs_sacz, "value", "semana")
med_obs_sp = DTPromEspacPesado(obs_sp, "value", "semana")
med_obs = DTPromEspacPesado(df.anom, "value", "semana")

# guardo
saveRDS(med_obs_sacz,"./SubX_processed_Rdata/poligonos/obs_sacz.rds")
saveRDS(med_obs_sp,"./SubX_processed_Rdata/poligonos/obs_sp.rds")
saveRDS(med_obs,"./SubX_processed_Rdata/poligonos/obs.rds")

# Encontrar semanas debajo del p10 y por encima del p90
fecha_sacz = FechasPercentiles(med_obs_sacz,med_obs_sacz$media)
fecha_sp = FechasPercentiles(med_obs_sp,med_obs_sp$media)
fecha = FechasPercentiles(med_obs,med_obs$media)

list(fecha,fecha_sacz,fecha_sp)

# Generar data frame con toda la info
colname = c("TOTAL10","TOTAL90","SACZ10","SACZ90","SP10","SP90")
extremo <- as.data.frame(list(fecha,fecha_sacz,fecha_sp))
colnames(extremo)<- colname

# Busca coincidencia entre regiones
p10 <- list(fecha[[1]],fecha_sacz[[1]], fecha_sp[[1]])
p90 <- list(fecha[[2]],fecha_sacz[[2]], fecha_sp[[2]])
Reduce(intersect, p10)
Reduce(intersect, p90)


# Creo archivo csv 
write.csv(extremo, "./SubX_processed_Rdata/ext_newpoli.csv")

# Grafico de la ubicacion de los poligonos en un dia particular
SP <- data.frame(x_coords = c(293,289,290,293,298,293),
                 y_coords = c(-28,-35,-50,-50,-40,-28))

SACZ <- data.frame(x_coords = c(305,305,312,319,323,305),
                   y_coords = c(-10,-20,-24,-25,-10,-10))



mapa<-map_data("world2")
g=ggplot() +
  scale_x_longitude(breaks = c(280,300, 320),expand = c(0.09, 0.09)) +
  scale_y_latitude(breaks = c(-40,-20,0),expand = c(0.09, 0.09)) +

  geom_map(dat=mapa, map = mapa, aes(map_id=region), fill="NA", color="black", inherit.aes = F)+
  geom_polygon(data= SP, aes(x=x_coords, y=y_coords),color= 'slateblue',fill= NA,size=1)+
  geom_polygon(data= SACZ, aes(x=x_coords, y=y_coords),color= 'seagreen',fill= NA,size=1) +

theme(axis.text=element_text(size=12))+
  theme(strip.text.x = element_text(size = 12, colour = "black"))+
  
  theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
  theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey86"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey86")) +
  coord_cartesian(xlim = c(270,325), ylim = c(-60,15)) +

    # agrego nombres de poligonos
    annotate("text", x = 297, y = -32, label = "SEP",
             col = "slateblue", size = 5)  +     #angle=-60 si queres
  annotate("text", x = 312, y = -9, label = "SACZ",
            col = "seagreen", size = 5)
g
# guardo
ggsave(plot=g,filename = "./SubX_processed_Rdata/poligonos/poli.png",width = 7, height = 9)


#---------------------------------------------------------------------------------------------
#-----------------------------------------B O X P L O T S------------------------------------- 

#modelos
lista.modelos.sacz <- readRDS("./SubX_processed_Rdata/poligonos/poli.sacz.rds")
lista.modelos.sp <- readRDS("./SubX_processed_Rdata/poligonos/poli.sp.rds")
#reanalisis
med_obs_sacz <- readRDS("./SubX_processed_Rdata/poligonos/obs_sacz.rds")
med_obs_sp <- readRDS("./SubX_processed_Rdata/poligonos/obs_sp.rds")


# Armar datos para boxplots
# Junto todos los modelos en mismo df
grupos = c("ESRL-FIMr1p1","ECCC-GEM","EMC-GEFS",
           "GMAO-GEOS_V2p1","RSMAS-CCSM4","NRL-NESM","MME")
names(lista.modelos.sacz) <- grupos
names(lista.modelos.sp) <- grupos

dt.modelos.sacz = bind_rows(lista.modelos.sacz, .id = "modelo")
dt.modelos.sp = bind_rows(lista.modelos.sp, .id = "modelo")



# g1<-GraphBoxplotMultiple(Data = dt.modelos.sacz, Var = "media", Grupo = "modelo",
#                      Week = "week", DataObs = med_obs_sacz, VarObs = "media",
#                      Titulo = "SACZ")
# 
# g2<-GraphBoxplotMultiple(Data = dt.modelos.sp, Var = "media", Grupo = "modelo",
#                          Week = "week", DataObs = med_obs_sp, VarObs = "media",
#                          Titulo = "PATAGONIA")
# 
# 
# fig <- grid.arrange(g1,g2, ncol = 2,top = textGrob("SubX tasa (99-15, Oct-Mar)",gp=gpar(fontsize=13,font=3)))
# ggsave(filename="./SubX_processed_Rdata/boxplot.png",plot=fig,width = 17, height = 11)

g1<-GraphBoxplotMultiple2(Data = dt.modelos.sacz, Var = "media", Grupo = "modelo",
                         Week = "week", DataObs = med_obs_sacz, VarObs = "media",
                         Titulo = "SACZ")

g2<-GraphBoxplotMultiple2(Data = dt.modelos.sp, Var = "media", Grupo = "modelo",
                         Week = "week", DataObs = med_obs_sp, VarObs = "media",
                         Titulo = "SEPG")

fig2 <- grid.arrange(g1,g2, ncol = 2,top = textGrob("SubX tasa (99-14, Oct-Mar)",gp=gpar(fontsize=13,font=3)))
ggsave(filename="./SubX_processed_Rdata/boxplot2.png",plot=fig2,width = 17, height = 11)


v1 <- GraphBoxplotViolin(Data = dt.modelos.sacz, Var = "media", Grupo = "modelo",
                         Week = "week", DataObs = med_obs_sacz, VarObs = "media",
                         Titulo = "SACZ")

v2 <- GraphBoxplotViolin(Data = dt.modelos.sp, Var = "media", Grupo = "modelo",
                          Week = "week", DataObs = med_obs_sp, VarObs = "media",
                          Titulo = "SEPG")
                         
fig3 <- grid.arrange(v1,v2, ncol = 2,top = textGrob("SubX tasa (99-14, Oct-Mar)",gp=gpar(fontsize=13,font=3)))
ggsave(filename="./SubX_processed_Rdata/violin.png",plot=fig3,width = 17, height = 11)
