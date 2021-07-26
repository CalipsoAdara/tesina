# F U N C I O N E S 

#-----------------------------------------------------------------------------------------
p <- function(..., sep='') {
  paste(..., sep=sep, collapse=sep)
}

# -----------------------------------------------------------------------------------------
# Funcion que obtiene las dimensiones de un archivo netcdf y devuelve un vector con dichas
# dimensiones
DimNc <- function(nc) {
  
  # Obtener los nombres de dichas dimensiones (probablemente lon,lat,time)
  name_dim <- attributes(nc$dim)$names
  
  # Creo vector vacio donde guardar estos datos
  dimension <- vector()
  
  for (i in 1:nc$ndims) {
    nc_dim <- ncvar_get( nc, attributes(nc$dim)$names[i])
    
    dimension[i] <- length(nc_dim)
  
  }  
  print(dimension)
}

# -----------------------------------------------------------------------------------------
# Funcion para graficar con escalas discretas de colores

GraphDiscrete <- function(Data, Breaks, Titulo, Label, Paleta, Direccion){
  
  ## Data: un data frame de al menos 3 dimensiones para realizar el mapa. Primer dim son las long repetidas la cantidad
  # de veces de las latitudes, Segunda dim son las lat repetidas la cantidad de veces de las longitudes y Tercera dim 
  # son los valores
  ## Breaks: un vector con los numeros para discretizar la barra de colores. Ej c(0,5,10,20)
  ## Titulo: character vector con el titulo del grafico
  ## Label: character vector con el titulo para la barra de colores. Ej "Kelvin"
  ## Paleta: character vector que indica una paleta existente. Ej "RdBu"
  ## Direccion : numero 1 o -1 para indicar si se revierte la paleta. 
  ## LabelBreaks: un vector con las
  
  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$z, na.rm = T)
  max <- max(Data$z, na.rm = T)
  Data$z=oob_squish(Data$z,range = c(min(Breaks),max(Breaks)))
  
  # Grafico en si 
  ggplot() +
    geom_contour_fill(data=Data,aes(x, y, z = z),breaks = Breaks) +
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
    theme(axis.text=element_text(size=12))+
    theme(strip.text.x = element_text(size = 12, colour = "black"))+

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



#---------------------------------------------------------------------------------------------
# Otra forma de graficar discreta 

GraphDiscrete2 <- function(Data, Breaks, Titulo, Label, Paleta){
  
  ## Data: un data frame de al menos 3 dimensiones para realizar el mapa. Primer dim son las long repetidas la cantidad
  # de veces de las latitudes, Segunda dim son las lat repetidas la cantidad de veces de las longitudes y Tercera dim 
  # son los valores
  ## Breaks: un vector con los numeros para discretizar la barra de colores. Ej c(0,5,10,20)
  ## Titulo: character vector con el titulo del grafico
  ## Label: character vector con el titulo para la barra de colores. Ej "Kelvin"
  ## Paleta: character vector que indica una paleta existente. Ej "RdBu"
  
  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$z, na.rm = T)
  max <- max(Data$z, na.rm = T)

  ggplot() +
    geom_contour_fill(data = Data, aes(x=x,y=y,z=z),breaks = Breaks) +
  
    scale_fill_distiller(name=Label,palette=Paleta,direction=-1,
                       breaks = Breaks,
                       limits = c(min(Breaks), max(Breaks)),
                       guide = guide_colorstrip()) +
    geom_polygon(data=mapa,aes(x=long ,y=lat, group=group),fill=NA,color="black",size=0.2) +
    #scale_x_longitude(breaks = c(280,300, 320)) +
    #scale_y_latitude(breaks = c(-40,-20,0)) +
    ggtitle(Titulo)  +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme_bw()+
    xlim(265,330) + ylim(-60,15) +
    xlab("Longitud") + ylab("Latitud") +
    theme(plot.title = element_text(hjust = 0.5)) +
    coord_cartesian()


}


# -----------------------------------------------------------------------------------------
# Funcion para graficar con escalas discretas de colores

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
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$z, na.rm = T)
  max <- max(Data$z, na.rm = T)
  Data$z=oob_squish(Data$z,range = c(min(Breaks),max(Breaks)))
  
  # Grafico en si 
  ggplot() +
    geom_contour_fill(data=Data,aes(x, y, z = z),breaks = Breaks) +
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

# Funcion para graficos del IRI que no estan interpolados y las longitudes no esta en 360
GraphDiscreteIRI <- function(Data, Breaks, Titulo, Label, Paleta, Direccion){
  
  ## Data: un data frame de al menos 3 dimensiones para realizar el mapa. Primer dim son las long repetidas la cantidad
  # de veces de las latitudes, Segunda dim son las lat repetidas la cantidad de veces de las longitudes y Tercera dim 
  # son los valores
  ## Breaks: un vector con los numeros para discretizar la barra de colores. Ej c(0,5,10,20)
  ## Titulo: character vector con el titulo del grafico
  ## Label: character vector con el titulo para la barra de colores. Ej "Kelvin"
  ## Paleta: character vector que indica una paleta existente. Ej "RdBu"
  ## Direccion : numero 1 o -1 para indicar si se revierte la paleta. 
  
  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world") 
  min <- min(Data$z, na.rm = T)
  max <- max(Data$z, na.rm = T)
  
  # Grafico en si 
  ggplot() +
    geom_contour_fill(data=Data,aes(x, y, z = z),breaks = Breaks) +
    scale_x_longitude(breaks = c(-80,-60, -40),expand = c(0.09, 0.09)) +
    scale_y_latitude(breaks = c(-40,-20,0),expand = c(0.09, 0.09)) +
    scale_fill_distiller(name=Label,palette=Paleta,direction= Direccion,
                         na.value = "transparent",
                         breaks = Breaks,
                         limits = c(min(Breaks), max(Breaks)),
                         guide = guide_colorstrip(),
                         oob  = scales::squish) +
    ggtitle(Titulo)  +
    geom_map(dat=mapa, map = mapa, aes(map_id=region), fill="NA", color="black", inherit.aes = F)+
    theme(axis.text=element_text(size=12))+
    theme(strip.text.x = element_text(size = 12, colour = "black"))+
    
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

#---------------------------------------------------------------------------------------------
# Funcion que realiza dataframes de longitud, latitud y variable para colocar en la funcion GraphDiscrete

ggDataFrame <- function(Array){
  ## Array: array con los valores de la variable a dibujar, por ejemplo t2m a dos metros 
  
  valores = as.vector(Array)
  
  lon = seq(265,330,1)
  lat = rev(seq(-60,15, 1))
  
  # Creo los data frames necesarios
  # En la primer columna tiene todas las long repetidas la cantidad de veces de las latitudes
  # En la segunda columna tiene todas las lat repetidas la cantidad de veces de las longitudes
  # En la tercera columna tiene los valores de media mensual para 1 mes en particular
  df <-data.frame(x=rep(lon,length(lat)),
                        y=rep(lat,each=length(lon)),
                        z=valores)
  return(df)
  
}

#--------------------------------------------------------------------------------------------------
# Funcion para graficar teniendo en cuenta la significancia de correlacion
# se cubre con puntos la parte no significativa

GraphDiscretePuntos <- function(Data, ArLogic, Breaks, Titulo, Label, Paleta, Direccion){
  
  ## Data: un data frame de al menos 3 dimensiones para realizar el mapa. Primer dim son las long repetidas la cantidad
  # de veces de las latitudes, Segunda dim son las lat repetidas la cantidad de veces de las longitudes y Tercera dim 
  # son los valores
  ## Arlogic: array logico (T o F) de las mismas dimensiones que para Data 
  ## Breaks: un vector con los numeros para discretizar la barra de colores. Ej c(0,5,10,20)
  ## Titulo: character vector con el titulo del grafico
  ## Label: character vector con el titulo para la barra de colores. Ej "Kelvin"
  ## Paleta: character vector que indica una paleta existente. Ej "RdBu"
  ## Direccion : numero 1 o -1 para indicar si se revierte la paleta. 

  
  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
  library("scales")
  library("metR")
  library(grid)
  library("gridExtra")
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  Data$z=oob_squish(Data$z,range = c(min(Breaks),max(Breaks)))
  
  # Crear data frame para los puntos
  lon = seq(265,330,1)
  lat = rev(seq(-60,15,1))
  
  # Busca los indices donde hay TRUE y los evalua en las lon y lat. Asi recibe la informacion GEOM_POINT
  mtx.posicion = which(ArLogic, arr.ind = T)
  punto_lon = lon[mtx.posicion[,1]]
  punto_lat = lat[mtx.posicion[,2]]  
  df.puntos = data.frame(lon = punto_lon, lat = punto_lat)
  
  # Grafico en si 
  ggplot() +
    geom_contour_fill(data=Data,aes(x, y, z = z),breaks = Breaks) +
    geom_point(data=df.puntos, aes(x=lon, y=lat), col="black",size=0.5, na.rm = T) +
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
    theme(axis.text=element_text(size=12))+
    theme(strip.text.x = element_text(size = 12, colour = "black"))+
    
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

#--------------------------------------------------------------------------------------------------
# Funcion para graficar multiple teniendo en cuenta la significancia de correlacion
# se cubre con puntos la parte no significativa

GraphMultiplePuntos <- function(Data, ArLogic, Breaks, Titulo, Label, Paleta, Direccion){
  
  ## Data: un data frame de al menos 3 dimensiones para realizar el mapa. Primer dim son las long repetidas la cantidad
  # de veces de las latitudes, Segunda dim son las lat repetidas la cantidad de veces de las longitudes y Tercera dim 
  # son los valores
  ## Arlogic: array logico (T o F) de las mismas dimensiones que para Data 
  ## Breaks: un vector con los numeros para discretizar la barra de colores. Ej c(0,5,10,20)
  ## Titulo: character vector con el titulo del grafico
  ## Label: character vector con el titulo para la barra de colores. Ej "Kelvin"
  ## Paleta: character vector que indica una paleta existente. Ej "RdBu"
  ## Direccion : numero 1 o -1 para indicar si se revierte la paleta. 

  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$z, na.rm = T)
  max <- max(Data$z, na.rm = T)
  Data$z=oob_squish(Data$z,range = c(min(Breaks),max(Breaks)))
  
  # Crear data frame para los puntos
  lon = seq(265,330,1)
  lat = rev(seq(-60,15,1))
  sem = c("Week 1", "Week 2", "Week 3", "Week 4")
  
  # Busca los indices donde hay TRUE y los evalua en las lon y lat. Asi recibe la informacion GEOM_POINT
  mtx.posicion = which(ArLogic, arr.ind = T)
  punto_lon = lon[mtx.posicion[,1]]
  punto_lat = lat[mtx.posicion[,2]]  
  semana = sem[mtx.posicion[,3]]
  df.puntos = data.frame(lon = punto_lon, lat = punto_lat, week = semana)
  
  # Grafico en si 
  ggplot() +
    geom_contour_fill(data=Data,aes(x, y, z = z),breaks = Breaks) +
    geom_point(data=df.puntos, aes(x=lon, y=lat), col="black",size=0.5, na.rm = T) +
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
#------------------------------------------------------------------------------------------------
# Funcion que grafica histogramas multiples

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
  
  ggplot(data = Data, aes(x = media)) +
    geom_histogram(aes(y = stat(count) / N),
                   binwidth = 0.5, color ="white" , fill = "indianred1") +
    scale_x_continuous(breaks = Breaks, limits = c(Min,Max)) +
    
    scale_y_continuous(labels = scales::percent) +
    
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
#------------------------------------------------------------------------------------------------
# Funcion que toma dos arrays de tres dimensiones y los correlaciona punto a punto en la tercera
# dimension
CorrWeek <- function(Var1, Var2){
  ## Var1: un array de tres dimensiones 
  ## Var2: un array de tres dimensiones
  
  acc <- array(NA, dim = c(66,76))
  
  for (lon in 1:66) {
    for (lat in 1:76) {
      
      # Me quedo solo con un punto particular y todas las fechas de pronostico
      var1.punto <- Var1[lon,lat,]
      var2.punto <- Var2[lon,lat,]
      
      coef_corr <- cor(var1.punto,var2.punto,use="pairwise.complete.obs",method = "pearson")
      
      acc[lon,lat] <- coef_corr
      
    } # End loop lat
    
  }  # End loop lon
  return(acc)}


#---------------------------------------------------------------------------------------
#  functions of verification metrics 
#---------------------------------------------------------------------------------------

# ----------------------------------------------------------------------------------------------
# Funcion que toma los campos de reanalisis para cada semana respecto al startdate y calcula
# la media 
ObsMediaSemanal <- function(PronoDate,TargetDate,Anomalia){
  
  # PronoDate: numero de la cantidad de startdates se tiene para el periodo. Por ej: 612
  # TargetDate: Matriz donde los elementos son las fechas del targetdate para cada startdate y cada lead
  # Anomalia: Array con las anomalias. Debe tener dimensiones iguales al modelo de lon y lat y una dimension
  #           temporal con nombre "days" y fechas en formato character
  
  # Crea las variables a llenar
  anom_media_semanal <- array(NA, dim = c(66,76,PronoDate,4))
  days = dimnames(Anomalia)$day 
  
  for (w in 1:4) {
    
    for (startdate in 1:PronoDate) {
      
      lead = c(1,8,15,22) # Es el lead inicial para cada semana
      
      week = as.vector(TargetDate[lead[w]:(lead[w]+6) , startdate])
      targetweek = Anomalia[,,which(days %in% week)]
      media_semanal = apply(targetweek, c(1,2), FUN = mean, na.rm = T)
      anom_media_semanal[,,startdate,w] <- media_semanal 
      
    } #End loop pronodate
  }# End loop week
  
  return(anom_media_semanal)
}

# ----------------------------------------------------------------------------------------------
# Funcion que calcula la media semanal para cada fecha de pronostico del modelo
ModelMediaSemanal <- function(Modelo, PronoDate){
  # Modelo: Array del modelo con dimension de lon, lat, leads y cada fecha pronostico
  # PronoDate: numero de la cantidad de startdates se tiene para el periodo. Por ej: 612
  # Creo variable a llenar
  model_media_semanal <- array(NA, dim = c(66,76,PronoDate,4))
  
  for (w in 1:4) {
    
    lead = c(1,8,15,22) # Es el lead inicial para cada semana
    ar.model.w = Modelo[,,lead[w]:(lead[w]+6),]  # Toma cada semana 
    media_semanal = apply(ar.model.w, c(1,2,4), FUN = mean, na.rm= TRUE)
    model_media_semanal[,,,w]<- media_semanal
  } # End loop week
  
  return(model_media_semanal)
}

