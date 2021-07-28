# Analisis zona SACZ y SUR PATAGONIA
# GMAO
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
  fechas10 <- DF[p10,][1,]
  fechas90 <- DF[p90,][1,]
  
  return(fechas10,fechas10)
}
# -----------------------------------------------------------------------------------

# Cargo datos obs y modelo
grupos = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
models = c('FIMr1p1','GEM','GEFS','GEOS_V2p1','CCSM4','NESM',"SAT")
nmodels = length(models)

# Lista con los datos a llenar
modelweek <- list()

for (i in 1:nmodels) {
  inpath = paste0("./SubX_processed_Rdata/modelweek_",grupos[i],".rds")
  modelweek[[i]] <- readRDS(inpath)
}


# Poligonos. Lon de menor a mayor, el primer punto se repite para cerrar el poligono
SP <- data.frame(x_coords = c(291,288,291,298,291),
                 y_coords = c(-30,-40,-53,-40,-30))

SACZ <- data.frame(x_coords = c(305,305,310,321,305),
                   y_coords = c(-10,-25,-30,-10,-10))

for (mod in 1:nmodels) {
  
  # Cantidad de fechas de inicializacion del modelo
  modelo = modelweek[[mod]]
  inicios <- dim(modelo)[3]
  
  # Convierto en Data frame 
  df = melt(modelo)
  
  # Restringir el data table al area del poligono (primeras 2 col son lat y lon)
  puntossacz=pointsInPolygon(df[,1:2],SACZ) 
  puntossp=pointsInPolygon(df[,1:2],SP) 
  df_sacz = df[puntossacz,]
  df_sp = df[puntossp,]
  
  # Promedio en lat y long
  dt_sacz = as.data.table(df_sacz)
  dt_sp = as.data.table(df_sp)
  
  # Peso por la latitud 
  dt_sacz = dt_sacz[,varlat := value*cos(lat*pi/180)]
  dt_sp = dt_sp[,varlat := value*cos(lat*pi/180)]
  med_sacz = dt_sacz[,media:=mean(varlat,na.rm=TRUE),by=.(start,week)]
  med_sp = dt_sp[,media:=mean(varlat,na.rm=TRUE),by=.(start,week)]
  med_sacz$lon = NULL
  med_sacz$lat = NULL
  med_sacz$value = NULL
  med_sacz$varlat= NULL
  med_sp$lon = NULL
  med_sp$lat = NULL
  med_sp$value = NULL
  med_sp$varlat= NULL
  
  # Ahora elimino las filas duplicadas 
  med_sacz = unique(med_sacz, incomparables=FALSE, fromLast=FALSE)
  med_sp = unique(med_sp, incomparables=FALSE, fromLast=FALSE)
  
  # Graficar
  intervalos = seq(-5.0,5.0,2.5)
  g1 <- GraphHistMultiple(Data = med_sacz, Breaks = intervalos, N = inicios, LabelY = "SACZ")
  g2 <- GraphHistMultiple(Data = med_sp, Breaks = intervalos, N = inicios, LabelY = "PATAGONIA")
  
  title = paste0("SubX ", grupos[mod],"-",models[mod], " T2MA (99-15, Oct-Mar)")
  fig <- grid.arrange(g1,g2, ncol = 1,top = textGrob(title,gp=gpar(fontsize=13,font=3)))
  fname <- paste0("./SubX_processed_Rdata/hist_",grupos[mod],".png")
  ggsave(filename=fname,plot=fig,width = 15, height = 10)
  
  # Percentiles
  # quiero el p90 y el p10
  
  percentil_sacz = quantile(med_sacz$media, c(0.1,0.9))
  percentil_sp = quantile(med_sp$media, c(0.1,0.9))
  
  l = sem1$media > percentil[3]
  
  sem1[l,][,1]
  lista <- list()
  

}

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
  fechas10 <- DF[p10,][1,]
  fechas90 <- DF[p90,][1,]
  
  return(fechas10,fechas10)
}


# Convierto en Data frame 
df = melt(anom_media_semanal)

# Restringir el data table al area del poligono
puntospoli=pointsInPolygon(df[,1:2],SACZ) 
df_poli = df[puntospoli,]

# Promedio en lat y long
dt = as.data.table(df_poli)
med = dt[,media:=mean(value,na.rm=TRUE),by=.(start,week)]
med$lon = NULL
med$lat = NULL
med$value = NULL

# Ahora elimino las filas duplicadas 
medi = unique(med, incomparables=FALSE, fromLast=FALSE)

titulo = paste0("GMAO"," (oct-mar) ", "Week 1")
data = medi[Week =="week1"]
ggplot(data = medi, aes(x = media)) +
  geom_histogram(aes(y = stat(count) / fechas_pronosticos),
                 binwidth = 0.5, color ="white" , fill = "indianred1") +
   scale_x_continuous(breaks = seq(-5.0, 5.0, 2.5), limits = c(-5.0,5.0)) +
   
   scale_y_continuous(labels = scales::percent) +

   xlab("Anomalia T2M") +
   ylab("Frecuencia") +
  theme(axis.text=element_text(size=12))+
  theme(strip.text.x = element_text(size = 12, colour = "black"))+
  facet_grid( .~ week) +
  
  theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
  theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey86"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey86")) +
  theme(plot.title = element_text(hjust = 0.5))


# Percentil 
sem1 = med_sacz[week=="Week 1"]
percentil = quantile(sem1$media, c(0.9,0.95,0.99))

l = sem1$media > percentil[3]

sem1[l,][,1]
lista <- list()
lista[[2]] <-sem1[l,][,1]
lista[[3]] <- f
f=sem1[l,][,1][-1]

a <- c(1,3,5,7,9)
b <- c(3,6,8,9,10)
d <- c(2,3,4,5,7,9)
Reduce(intersect, lista)

DatoExtremo <- function() {
  
  # 
  

}

## funcion para lineas

GraphHistMultiple <- function(Data, Breaks, N, LabelY) {
  ## Data: Data frame con los datos. Debe tener una columna llamanda "week" para hacer las separaciones
  # y una columna llamada "media" con los numeros
  ## Breaks: Vector con los ticks del eje x
  ## N: Cantidad de datos. Sirve para hacer la frecuencia relativa
  ## LabelY: Character. Titulo en el eje y
  
  
  # Cargo paquetes
  library(ggplot2)
  
  Max = max(Breaks)
  Min = min(Breaks)
  
  Data$start <- as.Date(med_sacz$start)
  
  ggplot(data = Data, aes(x = start, y = media)) +
    geom_line(color = "paleturquoise3", size = 0.7) +
    theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
    theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey86"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey86")) +
    theme(plot.title = element_text(hjust = 0.5))
    
    
    #xlab("Anomalia T2M") +
    ylab(LabelY) +
    theme(axis.text=element_text(size=12))+
    theme(strip.text.x = element_text(size = 12, colour = "black"))+
    facet_grid( .~ week) +
    
    theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
    theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey86"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey86")) +
    theme(plot.title = element_text(hjust = 0.5))
}