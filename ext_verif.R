# Verificacion de Extremos

# by Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------

# Limpiar enviroment 
rm(list=ls())

# Llamar paquetes

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/model/viernes"

# Seteo el directorio
setwd(savepath)
#-------------------------------------------------------------------------------------------------------------

# Cargo datos de extremos
ext <- read.csv("./ext/extMME.csv",stringsAsFactors = F)
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL")  # sin MME
nmodels = length(models)



# Voy a calcular RMSE y ACC segun las fechas de los percentiles 90 y 10.
colname = c("TOTAL10","TOTAL90")

for (c in colname) { # por cada percentil
  
  #obtengo las semanas extremas
  p = substr(c,6,7)
  
  for (mod in models) { # por cada modelo
    
    modweek <- readRDS(paste0("/home/lucia.castro/SubX_processed_Rdata/model/modelweek_",mod,".rds"))
    obsweek <- readRDS(paste0("/home/lucia.castro/SubX_processed_Rdata/model/obsweek_",mod,".rds"))
    
    
    
    # Busco que fechas coinciden, o se acercan lo mas posible a las extrema
    # No coinciden exactamente
    startdate = dimnames(modweek)$start
    pos_ext= BuscarFechaExtrema(Ext=ext, Startdate = startdate, Columna = c)
    pos_ext = unique(pos_ext)  # saco los repetidos
    
  
    
    
    #--------- EXTREMOS ------------
    # restringo a esa fechas
    mod_ext = modweek[,,pos_ext,]
    obs_ext = obsweek[,,pos_ext,]
    
    # Hago la metrica 
    l = Metrics(obs_ext,mod_ext)
    
    # Guardo
    saveRDS(l, paste0("./ext/metricext_p",p,"_",mod,".rds"))
    
    
  }
  
}

#---------------------------------------------------------------------------
#-------- VERIFICACION NO EXTRMOS ------------------------------------------

for (mod in models) {  # por cada modelo
  
  modweek <- readRDS(paste0("/home/lucia.castro/SubX_processed_Rdata/model/modelweek_",mod,".rds"))
  obsweek <- readRDS(paste0("/home/lucia.castro/SubX_processed_Rdata/model/obsweek_",mod,".rds"))
  
  
  # Busco que fechas coinciden, o se acercan lo mas posible a las extrema
  # No coinciden exactamente
  startdate = dimnames(modweek)$start
  pos_ext= BuscarFechaExtrema(Ext=ext, Startdate = startdate, Columna = colname)
  pos_ext = unique(pos_ext)  # saco los repetidos
  

  
  
  #--------- EXTREMOS ------------
  # restringo a esa fechas
  mod_noext = modweek[,,-pos_ext,]
  obs_noext = obsweek[,,-pos_ext,]
  
  # Hago la metrica 
  l = Metrics(obs_noext,mod_noext)
  
  # Guardo
  saveRDS(l, paste0("./ext/metricnoext_",mod,".rds"))
}


#---------------------------------------------------------------------------
# R E S T A 
for (m in models) { # por cada modelo
  
  # Cargo metricas no ext
  m_noext <- readRDS(paste0("./ext/metricnoext_",m,".rds"))
  for (c in colname) { # por cada percentil
    
    p = substr(c,6,7)
    # Cargo metricas ext
    mext <- readRDS(paste0("./ext/metricext_p",p,"_",m,".rds"))
    
    # RESTO 
    resta <- Map('-', mext, m_noext) #Resto ambas listas 
    # guardo
    saveRDS(resta,paste0("./ext/restaEXT_p",p,"_",m,".rds"))
    
    
  }
 
  
}
#----------------------------------------------------------------------------
# G R A F I C O S 

# Junto todo en un dataframe

df <- data.frame()
for (mod in models) { # por cada modelo
  df_mod10 <- reshape2::melt(readRDS(paste0("./ext/restaEXT_p10_",mod,".rds")))
  df_mod90 <- reshape2::melt(readRDS(paste0("./ext/restaEXT_p90_",mod,".rds")))
  
  # agrego columa de percentil y otra de modelo
  df_mod10$per <- rep("P10",nrow(df_mod10))
  df_mod90$per <- rep("P90",nrow(df_mod90))
  df_mod <- rbind(df_mod90,df_mod10)
  df_mod$model <- rep(mod,nrow(df_mod))
  
  # guardo 
  df <- rbind(df,df_mod)
  
}


df = rename(df, "metric" ="L1")
dt<-as.data.table(df)
dt <- dt[ week == "Week 2" | week == "Week 3"]

# guardo
saveRDS(dt,"./ext/df_metricEXT.rds")

#------ Grafico de mapas ------------

# resto ext - NO ext
# acc mayor en ext ----> resta pos
# rmse mayor en 

# una imagen acc y otra rmse
# poner '10 y p90 uno al lado de otro
df <- readRDS("./ext/df_metricEXT.rds")
setDT(df)


g10 <- GraphMet(Data = df[per == "P10" & metric == "acc"],
             Breaks = seq(-0.5,0.5,0.1),
             Paleta = "RdBu",
             Direccion = -1,
             Row = "model",
             Col = "week",
             Titulo = "Percentil 10")

g90 <-GraphMet(Data = df[per == "P90" & metric == "acc"],
         Breaks = seq(-0.5,0.5,0.1),
         Paleta = "RdBu",
         Direccion = -1,
         Row = "model",
         Col = "week",
         Titulo = "Percentil 90")

g <-grid.arrange(g10,g90,                                
                 ncol = 2, 
                 widths = c(5,5),
                 heights = c(7))

ggsave("./ext/verifresta_acc.png",g,width = 10, height = 4)




# ----- Grafico de lineas -----------

for (s in 1:2) { # por cada metrica, RMSE Y ACC respectivamente
  for (m in models) {  # por cada modelo
    
    # cargo metrica
    metrics10 = readRDS(paste0("./metric/metricext_p10_",mod,".rds"))
    metrics90 = readRDS(paste0("./extMME/metricext_p90_",mod,".rds"))
    metric10 = metrics10[[s]]
    metric90 = metrics90[[s]]
    
    # Convierto a dataframe
    df10=melt(metric10)
    
    # Promedio en la region pesando por la latitud
    prom10= DTPromEspacPesado(DF =df10,
                      Variable = "value" ,
                      Grupo = "week")
  }
  
}


#------------------------------------------------------------------------------------



# gRfico dde prueba 
colnames(df10) <- c("x","y","week","z")
GraphDiscreteMultiple(Data= df10,Label = "",Breaks = seq(0,3,0.5), Paleta = "YlOrRd",Direccion = 1)


GraphDiscreteMultiple <- function(Data, Breaks, Label, Paleta, Direccion){
  
  ## Data: un data frame de al menos 3 dimensiones para realizar el mapa. Primer dim son las long repetidas la cantidad
  # de veces de las latitudes, Segunda dim son las lat repetidas la cantidad de veces de las longitudes y Tercera dim 
  # son los valores
  ## Breaks: un vector con los numeros para discretizar la barra de colores. Ej c(0,5,10,20)
  ## Titulo: character vector con el titulo del grafico
  ## Label: character vector con el titulo para la barra de colores. Ej "Kelvin"
  ## Paleta: character vector que indica una paleta existente. Ej "RdBu"
  ## Direccion: Numero 1 o -1 indicando si la paleta se revierte 
  
  
  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
  library(scales)
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$z, na.rm = T)
  max <- max(Data$z, na.rm = T)
  Data$z <- oob_squish(Data$z,range = c(min(Breaks),max(Breaks)))
  
  # Aqui extiendo un poco la escala para que cubra todo
  fillbreaks = Breaks
  fillbreaks[length(Breaks)] <- max(Breaks)*1.1
  fillbreaks[1] <- min(Breaks)*1.1
  
  # Grafico en si 
  ggplot() +                                          # fILLBREAKS
    geom_contour_fill(data=Data,aes(x, y, z = z),breaks = fillbreaks) +
    geom_contour(color = c())
    scale_x_longitude(breaks = c(280,300, 320),expand = c(0.09, 0.09)) +
    scale_y_latitude(breaks = c(-40,-20,0),expand = c(0.09, 0.09)) +
    scale_fill_distiller(name=Label,palette=Paleta,direction=as.numeric(Direccion),
                         na.value = "transparent",
                         breaks = Breaks,
                         limits = c(min(Breaks), max(Breaks)),
                         guide = guide_colorstrip(),
                         oob  = scales::squish) +
    
    geom_map(dat=mapa, map = mapa, aes(map_id=region), fill="NA", color="black", inherit.aes = F)+
    theme(axis.text=element_text(size=12))+
    theme(strip.text.x = element_text(size = 12, colour = "black"))+
    facet_grid( .~ week) +
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

#----------------------------------------------------------------------------------------------
# ------------ Verif MME ----------------------- 
# (no se si me conviente, ya que el MME no representa bien los extremos )
# Voy a calcular RMSE y ACC segun las fechas de los percentiles 90 y 10.

colname = c("TOTAL10","TOTAL90")

for (c in colname) { # por cada percentil
  
  #obtengo las semanas extremas
  sem_ext = ext[,c]
  p = substr(c,6,7)
  
  for (mod in models) { # por cada modelo
    
    modweek <- readRDS(paste0("/home/lucia.castro/SubX_processed_Rdata/model/viernes/modelweek_MME.rds"))
    target <- readRDS(paste0("/home/lucia.castro/SubX_processed_Rdata/model/viernes/targetdate_MME_OA.rds"))
    obsweek <- readRDS(paste0("/home/lucia.castro/SubX_processed_Rdata/model/viernes/obsweek_MME.rds"))


    # Busco que fechas coinciden, o se acercan lo mas posible a las extrema
    # No coinciden exactamente
    startdate = dimnames(modweek)$start
    pos_ext= BuscarFechaExtrema(Ext=ext, Startdate = startdate, Columna = c)
    pos_ext = unique(pos_ext)  # saco los repetidos
    
    # Chequeo que la diferencia entre dias sea menor o igual a 3
    pos_ext_bien = EsFechaCercana(startdate[pos_ext], sem_ext,3)
    
    # restringo a esa fechas
    mod_ext = modweek[,,pos_ext_bien,]
    obs_ext = obsweek[,,pos_ext_bien,]
    
    # Hago la metrica 
    l = Metrics(obs_ext,mod_ext)
    
    # Guardo
    saveRDS(l, paste0("./ext/metricext_p",p,"_MME.rds"))
  }
  
}

# ------------------------------------------
#  G r a f i c o s

# cargo datos

m90 <- readRDS("./ext/metricext_p90_MME.rds")
m10 <- readRDS("./ext/metricext_p10_MME.rds")

# Convierto en data frame y agrego columna
m90 <- reshape2::melt(m90)
m10 <- reshape2::melt(m10)
m90$per <- rep("90", nrow(m90))
m10$per <- rep("10", nrow(m10))


# Junto
m <- rbind(m90,m10)
m = rename(m, "metric" ="L1")
dt<-as.data.table(m)
dt <- dt[ week == "Week 2" | week == "Week 3"]



  # Graficado
gacc <- GraphMet(Data= dt[metric == "acc"],
                 Breaks = seq(-0.5,0.5,0.10),
                 Paleta = "RdBu",
                 Direccion = -1 ,
                 Titulo = " Coeficiente de Correlación de Anomalías",
                 Row = "week",
                 Col = "per")
grmse <- GraphMet(Data= dt[metric == "rmse"],
                  Breaks = seq(-0.5,0.5,0.10),
                  Paleta = "RdBu",
                  Direccion = 1 ,
                  Titulo = "Error Cuadrático Medio",
                  Row = "week",
                  Col = "per")
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
