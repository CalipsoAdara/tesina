# Analisis de la ola de calor 2013. 11/12 al 31/12
#
# Basado en el paper de los dioses
#Subseasonal prediction of the heat wave of December 2013
#in Southern South America by the POAMA and BCC�^`^pCPS models
#
# By lucia m. castro
# ---------------------------------------------------------------------------------

# Limpiar el espacio
rm(list=ls())

# cargar paquetes
library(ggplot2)
library(metR)
library(data.table)

# ----- EN PIKACHU-------
# source("/vegeta/datos/SubX/mjo.lucia.cas/funciones.R")
# svpath = "/home/lucia.castro/tesina2/resultados"
# setwd("/vegeta/datos/SubX/mjo.lucia.cas")
# ----- EN PIKACHU-------

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")
svpath = "/home/lucia.castro/SubX_processed_Rdata/model/viernes/"
setwd(svpath)

# Modelos
groups=c('GMAO','RSMAS','ESRL','ECCC','NRL','EMC','MME')
models=c('GEOS_V2p1','CCSM4','FIMr1p1','GEM','NESM','GEFS','SAT')
glabel=paste0(groups,"-",models)

# Dias de la ola de calor
diasola = seq.Date(as.Date("2013-12-13"),as.Date("2013-12-31"),1)
olaw1 = diasola[1:7]  # 13 a 19
olaw2 = diasola[8:14] # 20 a 26
olaw3 = diasola[15:19]  # 5 dias
olaw <- list(olaw1,olaw2,olaw3)
leadbin <- c(7,14,21)
weeklabel <- c("olaw1","olaw2","olaw3")

# Cargo datos OBSERVACIONES
ar.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")
# Primera forma: Analizar como los modelos pronosticaro cada semana de la ola.
# todos los modelos tienen inicializaciones en fechas diferentes, por lo tanto el lead a las
# semanas de la ola seran distintos para c/u. VOy a considerar grupos de leads por ej (leads 2-5,lead 5-8)

df <- data.frame()

for(we in 1:length(olaw)) { # por cada semana del evento
  for(m in groups) { # por cada modelo
    
    w = olaw[[we]]
    
    # Cargar datos del modelo (solo el subset nov-dic de 2013)
    targetdate <- readRDS(paste0("targetdate_",m,"_OA.rds"))
    startdate <- dimnames(targetdate)$startdate
    mod <- readRDS(paste0("model_",m,"_OA.rds"))
    
    for(l in leadbin) { # por cada lead
      # Buscar la inicializacion anterior a la semana del evento
      sem_anterior = as.character(w-l)
      
      # CONDICIONAL: en la semana 3 solo hay 5 dias, extiendo "sem_anterior" para tener en cuenta los 7 lead posibles
      if(length(w) == 5 ) {
        date_lead_mayor = (w-l)[1]
        date_lead_menor = (w-l)[1] +6
        sem_anterior = as.character(seq.Date(date_lead_mayor,date_lead_menor,by = 1))}
      
      stdate_ola <- last(intersect(startdate, sem_anterior))
      
      # Osea que el lead al evento es:
      lead =  as.numeric(w[1] - as.Date(stdate_ola))
      
      # Buscar las fechas de la semana hindcasteadas
      hind_ola <- targetdate[,stdate_ola] %in% as.character(w)
      
      # Evaluo en el stdate de la ola y los dias de la semana. luego promedio
      mod_ola <- apply(mod[,,hind_ola,stdate_ola], c(1,2), FUN = mean)
      
      # Convierto a dataframe y los voy combinando
      df.ola <- reshape2::melt(mod_ola)
      colnames(df.ola)<- c("lon","lat","value")
      df.ola$model <- m
      df.ola$lead <- paste0(l-6,"-",l)
      df.ola$week <- weeklabel[we]
      df <- rbind(df,df.ola)
      
     
      

    }# End loop lead
    # ------------------------------------------------------------------------------
    # O B S E R V A C I O N E S
    # Para agregar las observaciones las voy a tratar como un lead diferente para el grafico final. 
    # Todas las otras columnas son iguales excepto esa y la junto al df
    
    # Promedio semanal
    prom <- apply(ar.anom[,,as.character(w)],c(1,2),FUN = mean)
    df.obs <- reshape2::melt(prom)
    df.obs$model <- m
    df.obs$lead <- "CPC"
    df.obs$week <- weeklabel[we]
    df <- rbind(df,df.obs)
    
  } # End loop modelo
} # End loop semana

# Guardado

saveRDS(df,"./ola/dfola.rds")

# ------------------------------------------------------------------------------
# O B S E R V A C I O N E S

# Cargo datos
ar.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")
caso = 0
for(w in olaw) {
  
  sem <- format(w, "%d/%m")
  # Promedio semanal
  prom <- apply(ar.anom[,,as.character(w)],c(1,2),FUN = mean)
  
  # Graficar
  go <- GraphDiscrete(Data = ggDataFrame(prom),Breaks = seq(-3,3,1),Label = "°C",
                      Paleta = "RdBu", Direccion = -1,
                      Titulo = paste0("T2M ANOM CPC \n",sem[1],"-",sem[length(sem)]))
  caso <- caso +1
  ggsave(paste0(svpath,"./ola/obsola_",caso,".png"),go)
  
}


# G R A F I C O S ---------------------------------------------------------
# uno por cada semana

df <- readRDS("./ola/dfola.rds")

dt <- as.data.table(df)
fechaola <- c("13 a 19 de Dic", "20 a 26 de Dic","27 a 31 de Dic")
fechaolain = c("13 to 19 of Dec", "20 to 26 of Dec","27 to 31 of Dec")
library(stringr)

#rep_str = c('GMAO'='GMAO-GEOS_V2p1','RSMAS'='RSMAS-CCSM4','ESRL'='ESRL-FIMr1p1',
#            'ECCC'='ECCC-GEM','NRL'='NRL-NESM','EMC'='EMC-GEFS','MME'='MME')
#dt$model <- str_replace_all(dt$model, rep_str)
#dt$model =  factor(dt$model, levels=c('GMAO-GEOS_V2p1','RSMAS-CCSM4','ESRL-FIMr1p1','ECCC-GEM','NRL-NESM','EMC-GEFS','MME'))
dt = FactorsModelsPretty(DF=dt,Col="model")
setnames(dt,old="MODEL",new="model")


# Para que plotee los leads de forma correcta los convierto en factors
dt$lead =  factor(dt$lead, levels=c('15-21','8-14','1-7','CPC'))
GraphGrupos <- function(Data, Breaks, Titulo, Paleta, Direccion,Label){
  ## Data: un data frame de al menos 3 dimensiones para realizar el mapa. Primer dim son las long repetidas la cantidad
  # de veces de las latitudes, Segunda dim son las lat repetidas la cantidad de veces de las longitudes y Tercera dim 
  # son los valores
  ## Breaks: un vector con los numeros para discretizar la barra de colores. Ej c(0,5,10,20)
  ## Titulo: character vector con el titulo del grafico
  ## Paleta: character vector que indica una paleta existente. Ej "RdBu"
  ## Direccion : numero 1 o -1 para indicar si se revierte la paleta. 
  
  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$value, na.rm = T)
  max <- max(Data$value, na.rm = T)
  Data$z=oob_squish(Data$value,range = c(min(Breaks),max(Breaks)))
  
  # Aqui extiendo un poco la escala para que cubra todo
  fillbreaks = Breaks
  fillbreaks[length(Breaks)] <- max(Breaks)*1.1
  fillbreaks[1] <- min(Breaks)*1.1
  
  # Grafico en si 
  ggplot() +                                          # o Breaks   
    geom_contour_fill(data=Data,aes(lon, lat, z = z),breaks = fillbreaks) +
    scale_x_longitude(breaks = c(280,300, 320),expand = c(0.09, 0.09)) +
    scale_y_latitude(breaks = c(-40,-20,0),expand = c(0.09, 0.09)) +
    scale_fill_distiller(name=Label,palette=Paleta,direction= Direccion,
                         na.value = "transparent",
                         breaks = Breaks,
                         limits = c(min(Breaks), max(Breaks)),
                         guide = guide_colorstrip(),
                         oob  = scales::squish) +
    ggtitle(Titulo)  +
    geom_map(dat=mapa, map = mapa, aes(map_id=region), fill="NA", color="black", inherit.aes = F)+
    theme(strip.text.x = element_markdown(size=20),
          strip.text.y = element_markdown())+
    #theme(axis.text=element_text(size=12))+
    
    facet_grid( model~ lead )  +   # esto es nuevo row ~ col
    
    theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
    theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey86"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey86")) +
    coord_cartesian()  +
    theme(plot.title = element_text(hjust = 0.5))
  
  
}

for (w in 1:3) {
  
  
  titulo = paste("T2MA Ola de Calor\n",fechaola[w])
  #titulo = paste("T2MA HEAT WAVE\n",fechaolain[w])
  fig = GraphGrupos(Data = dt[week == weeklabel[w]], Paleta = "RdBu", Direccion = -1,
              Breaks = seq(-3,3,1), Label = "°C", Titulo = titulo)
  fig <- fig+theme(#legend.position  = "top",
                   plot.title = element_text(size=25),
                   legend.text = element_text(size=20),
                   legend.key.size = unit(1.5, 'cm'),
                   strip.text = element_text(size = 20),
                   legend.title = element_text(size=20))
  fig
  ggsave(filename=paste0("./ola/temp2_",weeklabel[w],"_v3.png"),
         plot=fig,width = 13, height = 20)
}

# Grafico de forma horizontal
GraphGruposHorizontal<-function(Data, Breaks, Titulo, Paleta, Direccion,Label){
  ## Data: un data frame de al menos 3 dimensiones para realizar el mapa. Primer dim son las long repetidas la cantidad
  # de veces de las latitudes, Segunda dim son las lat repetidas la cantidad de veces de las longitudes y Tercera dim 
  # son los valores
  ## Breaks: un vector con los numeros para discretizar la barra de colores. Ej c(0,5,10,20)
  ## Titulo: character vector con el titulo del grafico
  ## Paleta: character vector que indica una paleta existente. Ej "RdBu"
  ## Direccion : numero 1 o -1 para indicar si se revierte la paleta. 
  
  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$value, na.rm = T)
  max <- max(Data$value, na.rm = T)
  Data$z=oob_squish(Data$value,range = c(min(Breaks),max(Breaks)))
  
  # Aqui extiendo un poco la escala para que cubra todo
  fillbreaks = Breaks
  fillbreaks[length(Breaks)] <- max(Breaks)*1.1
  fillbreaks[1] <- min(Breaks)*1.1
  
  # Grafico en si 
  ggplot() +                                          # o Breaks   
    geom_contour_fill(data=Data,aes(lon, lat, z = z),breaks = fillbreaks) +
    scale_x_longitude(breaks = c(280,300, 320),expand = c(0.09, 0.09)) +
    scale_y_latitude(breaks = c(-40,-20,0),expand = c(0.09, 0.09)) +
    scale_fill_distiller(name=Label,palette=Paleta,direction= Direccion,
                         na.value = "transparent",
                         breaks = Breaks,
                         limits = c(min(Breaks), max(Breaks)),
                         guide = guide_colorstrip(),
                         oob  = scales::squish) +
    ggtitle(Titulo)  +
    geom_map(dat=mapa, map = mapa, aes(map_id=region), fill="NA", color="black", inherit.aes = F)+
    theme(strip.text.x = element_markdown(size=12),
          strip.text.y = element_markdown())+
    #theme(axis.text=element_text(size=12))+
    
    facet_grid( lead~ model )  +   # esto es nuevo row ~ col
    
    theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
    theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey86"), 
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey86")) +
    coord_cartesian()  +
    theme(plot.title = element_text(hjust = 0.5))
  
  
}


for (w in 1:3) {
  
  
  #titulo = paste("T2MA OLA DE CALOR\n",fechaola[w])
  titulo = paste("T2MA HEAT WAVE\n",fechaolain[w])
  fig = GraphGruposHorizontal(Data = dt[week == weeklabel[w]], Paleta = "RdBu", Direccion = -1,
                    Breaks = seq(-3,3,1), Label = "°C", Titulo = titulo)
  fig <- fig+theme(legend.position  = "top")
  ggsave(filename=paste0("./ola/temp2_",weeklabel[w],"_vhor.png"),
         plot=fig,width = 15, height = 8)
}


# Segunda forma: Analizar las medias semanales ya calculadas de los mod vs las obs
#

listagraficos2 <- list()
for(mod in groups) { # por cada modelo
  #cargo el targatdate y el startdate
  targetdate <- readRDS(paste0("targetdate_",mod,"_ONDEFM.rds"))
  startdate <- dimnames(targetdate)$startdate
  
  # Busco que dias de la ola fueron modeladas.
  # Tomo la el startdate que caiga en la semana anterior a la ola (si hay dos eligo el ultimo)
  stdate_ola <- last(intersect(startdate, sem_anterior))
  
  # Cargo los datos semanales de los modelos
  modweek <- readRDS(paste0("modelweek_",mod,".rds"))
  obsweek <- readRDS(paste0("obsweek_",mod,".rds"))
  
  # Evaluo en la fecha y resto
  resta = obsweek[,,stdate_ola,] - modweek[,,stdate_ola,]
  # Grafico
  g <- GraphDiscreteMultiple(Data = ggScoreSemanal(resta),Breaks = seq(-5,5,1),
                             Label = "°C",Paleta = "RdBu", Direccion = -1)
  # Lo completo asi en vez de append porque sino guarda listas en vez de ggplots
  len <- length(listagraficos2)
  listagraficos2[[len+1]] <- g
  
} # End model loop
# De esta forma puedo poner los nombres de los modelos en las filas
png(paste0(svpath,"/ola.png"),width = 20, height = 35, units = "cm",res = 1080) # creo el objeto

lg <- tableGrob(c("", glabel), theme= ttheme_minimal(base_size = 10))
rg <- arrangeGrob(grobs = listagraficos2, ncol=1,
                  top = textGrob("Ola de calor Dic 2013 \n OBS-MOD"
                                 ,gp=gpar(fontsize=18)))

grid.newpage()
grid.draw(cbind(lg, rg, size = "last")) # lo dibuja

dev.off() # borra

# ------------------------------------------------------------------------------

# O B S E R V A C I O N E S

# Cargo datos
ar.anom = readRDS("/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")
caso = 0
for(w in olaw) {
  
  sem <- format(w, "%d/%m")
  # Promedio semanal
  prom <- apply(ar.anom[,,as.character(w)],c(1,2),FUN = mean)
  # Graficar
  go <- GraphDiscrete(Data = ggDataFrame(prom),Breaks = seq(-3,3,1),Label = "°C",
                      Paleta = "RdBu", Direccion = -1,
                      Titulo = paste0("T2M ANOM CPC \n",sem[1],"-",sem[length(sem)]))
  caso <- caso +1
  ggsave(paste0(svpath,"/obsola_",caso,".png"),go)
  
}
