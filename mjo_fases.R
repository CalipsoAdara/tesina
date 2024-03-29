# MJO analisis con los modelos separando segun la fase inicial
# Si resto activo - inactivo
# donde rmse sea positivo siginifica que fue mayor en activo ----> MAL
# donde acc sea positivo significa que fue mayor en activo -----> BIEN
# 
#
#
# By lucia m. castro
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


# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata")

groups=c('GMAO','RSMAS','ESRL','ECCC','NRL','EMC','MME')                       
models=c('GEOS_V2p1','CCSM4','FIMr1p1','GEM','NESM','GEFS','SAT')   
glabel=paste0(groups,"-",models)

# Cargo los datos de eventos
df_rmm <- readRDS("./MJO/df_rmm.rds")
df_eventos <- readRDS("./MJO/df_eventos.rds")
fechas_act <- as.character(df_rmm$DATE)

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
  TargetDate <- readRDS(paste0("./targetdate_",grupo,"_ONDEFM.rds"))
  startdate = dimnames(TargetDate)$startdate
  # Cargo datos de modelo y observaciones
  modelweek<-readRDS(paste0("./modelweek_",grupo,".rds"))
  obsweek<-readRDS(paste0("./obsweek_",grupo,".rds"))
  metricINA <- readRDS(paste0("./metricsMJO_inact_",grupo,".rds"))[c(1,3)] #Leo rmse y acc solo
  
 
  for (b in Bins) { # por cada Bin
    # Busco que startdates coinciden con los eventos activos
    fechas_act_bin <- as.character(df_rmm[Bin==b,DATE])
    posMJOBIN = startdate %in% fechas_act_bin
    
    # Cantidad de inicios antes y despues de restringir en los eventos
    # informacion para luego crear tabla
    nstartdate = length(startdate)
    nstartdateMJOBIN <- c(nstartdateMJOBIN,sum(posMJOBIN))
    
    # Calculo las distintas métricas por cada lon/lat/targetweek
    sco_bin<-Metrics(obsweek[,,posMJOBIN,],modelweek[,,posMJOBIN,])
    saveRDS(sco_bin,paste0("./MJO/ScoresBins/",grupo,b))
    resta_bin <- Map('-', sco_bin, metricINA) #Resto ambas listas 
    
    # Solo miro una semana
    drmse <- ggDataFrame(resta_bin[[1]][,,w])
    
    # Graficos
    g1 <- GraphDiscreteMultiple(Data = ggScoreSemanal(resta_bin[[1]]), Breaks = seq(-0.2,0.2,0.05),Label = "RMSE",Paleta = "RdBu", Direccion = "1") 
    g2 <- GraphDiscreteMultiple(Data = ggScoreSemanal(resta_bin[[2]]), Breaks = seq(-0.2,0.2,0.05), Label = "ACC",Paleta = "RdBu",Direccion = "-1")
    
    # Lo completo asi en vez de append porque sino guarda listas en vez de ggplots
    len <- length(listagraficosRMSE)
    listagraficosRMSE[[len+1]] <- g1 + theme(legend.position = "none") 
    listagraficosACC[[len+1]] <- g2 + theme(legend.position = "none") 
    
    
  }# End Bin
}# End model
##############################################################################################################3
####################### esto es para correr en el sv pikachu
listagraficosRMSE <- list()
listagraficosACC <- list()

for (g in 1:length(groups)) {
  grupo = groups[g]
  model = models[g]
  
  metricINA <- readRDS(paste0("./metricsMJO_inact_",grupo,".rds"))[c(1,3)] #Leo rmse y acc solo
  
  
  
  for (b in Bins) { # por cada Bin
    # Cargo los datos
    metricACT <- readRDS(paste0("./MJO/ScoresBins/",grupo,b))
    # Resto
    resta_bin <- Map('-', metricACT, metricINA) #Resto ambas listas 
    # Graficos
    g1 <- GraphDiscreteMultiple(Data = ggScoreSemanal(resta_bin[[1]]), Breaks = seq(-0.2,0.2,0.05),Label = "RMSE",Paleta = "RdBu", Direccion = "1") 
    g2 <- GraphDiscreteMultiple(Data = ggScoreSemanal(resta_bin[[2]]), Breaks = seq(-0.2,0.2,0.05), Label = "ACC",Paleta = "RdBu",Direccion = "-1")
    
    # Lo completo asi en vez de append porque sino guarda listas en vez de ggplots
    len <- length(listagraficosRMSE)
    listagraficosRMSE[[len+1]] <- g1 + theme(legend.position = "none") 
    listagraficosACC[[len+1]] <- g2 + theme(legend.position = "none") 
    #listagg <- c(listagraficosRMSE,listagraficosACC)
    }# End Bin
  
  # Graficos
  # nbins <- length(Bins)
  # nmet <- 1 # Dos metricas, ACC y RMSE
  # 
  # lg <- tableGrob(paste0(grupo,"-",model), theme= ttheme_minimal())
  # matrixgraficos <- rbind(matrix(1:(nmet*nbins),ncol = nmet, nrow = nbins),
  #                         rep(nbins+1,nmet))
  # rg <- grid.arrange(grobs = listagraficosACC,legend, ncol = nmet, nrow = nbins+1,  #Una fila mas para leyenda
  #                    layout_matrix = matrixgraficos,
  #                    widths = rep(2.7,nmet),
  #                    heights = c(rep(2.5,nbins),0.2),
  #                    top = textGrob(t,gp=gpar(fontsize=13,font=3))) 
  # ggsave(paste0(svpath,"/mjobin_",grupo,b,".png"),rg)
  
}# End model

##############################################################################################################
# Hago el grafico
# la matrix del grafico indica donde esta cada una de las figuras. Si hay numeros repetidos significa que 
# un objeto ocupa varios espacios
# La lista es bin
legend <- LeyendaCondicional(Breaks = seq(-0.2,0.2,0.05), Paleta = "RdBu", Labels = c("MJO APORTA","MJO NO APORTA"))
t <- paste("pepe")
lg <- tableGrob(paste0(grupo,"-",model), theme= ttheme_minimal())
matrixgraficos <- rbind(matrix(1:(4*7),ncol = 4, nrow = 7, byrow = T),
                        c(8,8,8,8))
rg <- grid.arrange(grobs = listagraficosRMSE,legend, ncol = 4, nrow = 8,
                   layout_matrix = matrixgraficos,
                   widths = rep(2.7,4),
                   heights = c(rep(2.5,7),0.2),
                   top = textGrob(t,gp=gpar(fontsize=13,font=3))) 

grid.arrange(bp, vp, legend, ncol=2, nrow = 2, 
             layout_matrix = rbind(c(1,2), c(3,3)),
             widths = c(2.7, 2.7), heights = c(2.5, 0.2))
grid.newpage()
grid.draw(cbind(lg, rg, size = "last"))

ggsave(file = OutFileName, arrangeGrob(grobs = plist, ncol = 2)) 
# Tabla de inicios activos e inactivos segun el modelo

#---------------------------------------------------------------------------------------

# Mentira. voy a a hacer otro grafico. Un grafico por cada week, sino, se rompe todo
svpath = "/home/lucia.castro/tesina2/resultados"
lead = c("1-7","8-14","15-21","22-28")

for(w in 1:4) { # Por cada week 
  listagraficosRMSE <- list()
  listagraficosACC <- list()
  for(g in 1:length(groups)){ # Por cada modelo
    
    grupo = groups[g]
    model = models[g]
    
    metricINA <- readRDS(paste0("./metricsMJO_inact_",grupo,".rds"))[c(1,3)] #Leo rmse y acc solo

    
    for( b in Bins) {
      # Cargo los datos
      metricACT <- readRDS(paste0("./ScoresBins/",grupo,b))
      # Resto
      resta_bin <- Map('-', metricACT, metricINA) #Resto ambas listas 
      
      # Por cada semana
      drmse <- ggDataFrame(resta_bin[[1]][,,w])
      dacc <- ggDataFrame(resta_bin[[2]][,,w])
      
      # Graficos
      g1 <- GraphDiscrete(Data = drmse, Breaks = seq(-0.2,0.2,0.05),Label = "RMSE",
                          Paleta = "RdBu", Direccion = "1", Titulo = "")
      g2 <- GraphDiscrete(Data = dacc, Breaks = seq(-0.2,0.2,0.05),Label = "ACC",
                          Paleta = "RdBu", Direccion = "-1", Titulo = "")
     
      # Lo completo asi en vez de append porque sino guarda listas en vez de ggplots
      len <- length(listagraficosRMSE)
      listagraficosRMSE[[len+1]] <- g1 + theme(legend.position = "none") 
      listagraficosACC[[len+1]] <- g2 + theme(legend.position = "none") 
      #listagg <- c(listagraficosRMSE,listagraficosACC)
      
    }# end loop bins
    
  } #End loop modelo

  # Termina todos los plots y los pongo en una imagen 
  #---------------------------
  # De esta forma puedo poner los nombres de los modelos en las filas
  png(paste0(svpath,"/fases_acc_w",w,".png"),width = 20, height = 35, units = "cm",res = 1080) # creo el objeto
  legend <-  LeyendaCondicional(Breaks = seq(-0.2,0.2,0.05), Paleta = "RdBu", Labels = c("MJO APORTA","MJO NO APORTA"))
  
  lg <- tableGrob(c(glabel), theme= ttheme_minimal(base_size = 10)) # filas
  rname = c("Bin [8,1]","Bin [2,3]","Bin [4,5]","Bin [6,7]")
  cg <- tableGrob(t(c(rname)), theme= ttheme_minimal(base_size = 15)) # columna
  
  rg <- arrangeGrob(grobs = listagraficosACC, ncol=4)

  gr<-grid.arrange(cg,cbind(lg, rg, size = "last"), legend, ncol= 1, nrow = 3,
                   layout_matrix = matrix(1:3, ncol = 1),
                   widths = c(2.7), heights = c(0.3,3, 0.3))

  grid.newpage()
  grid.draw(gr) # lo dibuja

  dev.off() # borra
  
}# End loop semana



#------------------------------
for(w in 1:4) {
  for (m in 1:7) {
    for( b in Bins){
      
      print(paste0(b,"Mod",m,"seman",w))
    }
  }
}
