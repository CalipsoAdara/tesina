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

# Cargo datos obs y modelo
grupos = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL")
models = c('FIMr1p1','GEM','GEFS','GEOS_V2p1','CCSM4','NESM')
nmodels = length(models)

anom_media_semanal <- readRDS("./SubX_processed_Rdata/modelweek_GMAO.rds")

# Lista con los datos a llenar
modelweek <- list()

for (i in 1:nmodels) {
  inpath = paste0("./SubX_processed_Rdata/modelweek_",grupos[i],".rds")
  modelweek[i] <- readRDS(inpath)
}


# Poligonos. Lon de menor a mayor, el primer punto se repite para cerrar el poligono
SP <- data.frame(x_coords = c(291,288,291,298,291),
                 y_coords = c(-30,-40,-53,-40,-30))

SACZ <- data.frame(x_coords = c(305,305,310,321,305),
                   y_coords = c(-10,-25,-30,-10,-10))

for (mod in 1:nmodels) {
  
  # Cantidad de fechas de inicializacion del modelo
  inicios <- dim(model_media_semanal)[3]
  
  # Convierto en Data frame 
  df = melt(model_media_semanal)
  
  # Restringir el data table al area del poligono
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
  ggsave(filename=fname,plot=fig,width = 10, height = 11)
  

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
sem1 = medi[week=="Week 1"]
percentil = quantile(sem1$media, c(0.9,0.95,0.99))

l = sem1$media > percentil[3]

sem1[l,]

DatoExtremo <- function() {
  
  # 
}
