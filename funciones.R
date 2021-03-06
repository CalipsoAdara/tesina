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
  ggplot() +
    geom_contour_fill(data=Data,aes(x, y, z = z),breaks = fillbreaks) +
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
    geom_point(data=df.puntos, aes(x=lon, y=lat), col="black",size=0.5,alpha = 0.5, na.rm = T) +
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
    geom_point(data=df.puntos, aes(x=lon, y=lat), col="black",alpha = 0.5,size=0.1, na.rm = T) +
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

GraphHistMultiple <- function(Data, Var, Breaks, N, LabelY, LimY) {
  ## Data: Data frame con los datos. Debe tener una columna llamanda "week" para hacer las separaciones
  ## Var: Character. La columna del data frame con los valores numericos
  ## Breaks: Vector con los ticks del eje x
  ## N: Cantidad de datos. Sirve para hacer la frecuencia relativa
  ## LabelY: Character. Titulo en el eje y
  ## LimY: Vector de dos elementos indicando el max y min del eje y. EJ c(0,0.40)
  
  
  # Cargo paquetes
  library(ggplot2)
  
  Max = max(Breaks)
  Min = min(Breaks)
  
  ggplot(data = Data, aes(x = get(Var),fill = fuente)) +
    geom_histogram(aes(y = (stat(count) / N)), position = "identity",
                   binwidth = 0.5, color ="white" ) +
    # grafica el histograma de nuevo con transparecia para que se diferencie
    # donde se superponen los histogramas
    geom_histogram(aes(y = (stat(count) / N)), position = "identity",
                   binwidth = 0.5, alpha = 0.5 ) +
    scale_x_continuous(breaks = Breaks, limits = c(Min,Max)) +
    
    scale_y_continuous(labels = scales::percent, limits = LimY) +
    scale_fill_manual(values=c("#b4e956", "#e956b4", "#56B4E9")) +
    
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
# Funcion para graficar multiples boxplots

GraphBoxplotMultiple <- function(Data,Var,Grupo,Week, DataObs, VarObs, Titulo){
  # https://waterdata.usgs.gov/blog/boxplots/
  ## Data: Data frame con los datos. 
  ## Var: Character. La columna del data frame con los valores numericos
  ## Grupo: Character. La columna del data frame que indica los grupos de boxplots. Ej: "modelo"
  ## Week: Character. La columna del data frame que indica cuantos graficos hay. Normalmente es por week
  ## DataObs: Data frame con los datos de observaciones 
  ## VarObs: Character. La columna del data frame obs con la variable
  ## Titulo: Character. Titulo del grafico
  
  g <- ggplot() +
    geom_boxplot(data = Data, aes(y = get(Var), color = get(Grupo)),
                 notch=TRUE, outlier.shape =  NA) +
    geom_boxplot(data = DataObs, aes(y=get(VarObs),x= -0.5),
                 color= "black", width = 0.1,outlier.shape =  NA) +
    #, varwidth = TRUE
    
    ylim(-3,3) +
    facet_wrap( .~ get(Week)) +
    
    
    theme(axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.title.x = element_blank(),
          legend.title = element_blank()) +
    theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
    theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey86"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey86")) +
    
    annotate(geom="text", x=-0.5, y=0.2, label="CPC",
             color="black", size = 3) +
    ggtitle(Titulo) +
    theme(plot.title = element_text(hjust = 0.5))
  
  return(g)
}
#------------------------------------------------------------------------------------------------
# 
# Funcion para graficar multiples boxplots donde el final de las lineas son el maximo y minimo
GraphBoxplotMultiple2 <- function(Data,Var,Grupo,Week, DataObs, VarObs, Titulo){
  # https://waterdata.usgs.gov/blog/boxplots/
  ## Data: Data frame con los datos. 
  ## Var: Character. La columna del data frame con los valores numericos
  ## Grupo: Character. La columna del data frame que indica los grupos de boxplots. Ej: "modelo"
  ## Week: Character. La columna del data frame que indica cuantos graficos hay. Normalmente es por week
  ## DataObs: Data frame con los datos de observaciones 
  ## VarObs: Character. La columna del data frame obs con la variable
  ## Titulo: Character. Titulo del grafico
  
  # Acomodo los datos para poder hacer los graficos
  dt = Data[,list(mediana=median(get(Var),na.rm=TRUE),
                  min = quantile(get(Var),0.10),
                  max = quantile(get(Var),0.90),
                  p25 = quantile(get(Var),0.25),
                  p75 = quantile(get(Var),0.75)),by=.(get(Week),get(Grupo))]
  
  dt.obs = DataObs[,list(mediana=median(get(VarObs),na.rm=TRUE),
                         min = quantile(get(Var),0.10),
                         max = quantile(get(Var),0.90),
                         p25 = quantile(get(VarObs),0.25),
                         p75 = quantile(get(VarObs),0.75))]
  
  # Data frame con los maximos y minimos para agregar 
  dt.minmax <- Data[,list(min = min(get(Var),na.rm=TRUE),
                          max = max(get(Var),na.rm=TRUE)),
                          by=.(get(Week),get(Grupo))]
  
  dt.minmax.obs <- DataObs[,list(min = min(get(VarObs),na.rm=TRUE),
                                 max = max(get(VarObs),na.rm=TRUE))]


  
  # Agrego columna con las posiciones en el eje x. 7 modelos y 4 week
  dt = dt[,x:= rep(1:7,each=4)]
  dt.minmax = dt.minmax[,x:= rep(1:7,each=4)]

  
  # Cambio nombre de la columna
  setnames(dt,"get",Week)
  setnames(dt,"get.1",Grupo)
  setnames(dt.minmax,"get",Week)
  setnames(dt.minmax,"get.1",Grupo)
  
  
g <-  ggplot() +
  
    geom_boxplot(data = dt, aes(x=x,ymin = min, lower = p25, middle = mediana,
                                  upper = p75, ymax = max, color = modelo, 
                                  fill = modelo), alpha = 0.3,stat = "identity")+
    geom_boxplot(data = dt.obs,aes(x=-0.5,ymin = min, lower = p25, middle = mediana,
                                  upper = p75, ymax = max),stat = "identity",
                                  color= "black", width = 1)+
  
    geom_point(data = dt.minmax, aes(x=x, y=min,color = modelo)) +
    geom_point(data = dt.minmax, aes(x=x, y=max,color = modelo)) +
    geom_point(data = dt.minmax.obs, aes(x=-0.5, y=min)) +
    geom_point(data = dt.minmax.obs, aes(x=-0.5, y=max)) +
  
    facet_wrap( .~ get(Week)) +
    
    ylim(-5,5) +
    
     theme(axis.ticks.x = element_blank(),
           axis.title.y = element_blank(),
           axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           legend.title = element_blank(),
           text = element_text(size=15)) +
     theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
     theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                           size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey86"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey86")) +
    
    annotate(geom="text", x=-0.5, y=(dt.obs$mediana +0.3), label="CPC",
              color="black", size = 3) +
    ggtitle(Titulo) +
    theme(plot.title = element_text(hjust = 0.5))
  
   return(g)

}
#------------------------------------------------------------------------------------------------
# 
# Funcion para graficar multiples boxplots VIOLIN donde el final de las lineas son lo elegis 
GraphBoxplotViolin <- function(Data,Var,Grupo,Week, DataObs, VarObs, Titulo){
  # http://www.sthda.com/english/wiki/ggplot2-violin-plot-quick-start-guide-r-software-and-data-visualization
  ## Data: Data frame con los datos. 
  ## Var: Character. La columna del data frame con los valores numericos
  ## Grupo: Character. La columna del data frame que indica los grupos de boxplots. Ej: "modelo"
  ## Week: Character. La columna del data frame que indica cuantos graficos hay. Normalmente es por week
  ## DataObs: Data frame con los datos de observaciones 
  ## VarObs: Character. La columna del data frame obs con la variable
  ## Titulo: Character. Titulo del grafico


  # Repito 4 veces las observaciones y le agrego una columna de Week y modelo
  DataObs2 <- DataObs[rep(seq_len(nrow(DataObs)), times = 4), ]
  DataObs2$week <- rep(c("Week 1", "Week 2", "Week 3", "Week 4"),
                     each = nrow(DataObs))
  DataObs2$modelo <- rep("CPC",nrow(DataObs2))
  setnames(DataObs2, "semana", "start")
  
  # Junto la data y dataobs en un mismo data frame 
  Data$fuente = NULL
  Data = rbind(Data,DataObs2)
  
# Funcion para generar un boxplot dentro del violin
  data_summary <- function(x) {
    m <- median(x,na.rm = T)
    ymin <- as.numeric(quantile(x,0.10))
    ymax <- as.numeric(quantile(x,0.90))
    return(c(y=m,ymin=ymin,ymax=ymax))
  }

g <- ggplot(data = Data, aes(x=modelo,y= media, color = modelo)) +
  # fijar boxwidth al maximo ancho (1) en todos los violines
    geom_violin(scale = "width") +
     stat_summary(fun.data=data_summary) + 
    facet_wrap( .~ get(Week)) +
    scale_color_manual(values=c("#000000",scales::hue_pal()(7)) ) +
  
  ylim(-5,5) +
  
  theme(axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        text = element_text(size=15)) +
  theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
  theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey86"),
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey86")) +

  ggtitle(Titulo) +
  theme(plot.title = element_text(hjust = 0.5))
  
return(g)
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
# ----------------------------------------------------------------------------------------------
# Funcion que calcula la media semanal en un punto de grilla 
PromediarSemanas <-function(Longitud, Latitud){
  ## Longitud: numeric del 1 al 66 indicando longitud
  ## Latitud: numeric del 1 al 76 indicando latitud
  
  # tomo un punto (todos los dias) y acomodo en un array con la semana en las columnas
  punto = ar.anom[Longitud, Latitud,]
  dias = length(punto)
  t2m_punto = array(punto,dim=c(7,floor(dias/7))) # Redondea para abajo, quita la ultima 
  # semana q tiene dos dias
  prom_semanal = colMeans(t2m_punto, na.rm = T)
  
  # A cada valor le asigno la fecha de inicio y final de esa semana
  ini_sem = seq.Date(as.Date("1999-01-01"),as.Date("2016-12-31"),by=7)
  ini_sem = ini_sem[-length(ini_sem)] # quito ultima semana
  df.promsem = data.frame("Inicio" = ini_sem,
                          "Final" = ini_sem+6, 
                          "Promedio" = prom_semanal)
  
  # Restringo de octubre a abril
  OA = c(1,2,3,4,10,11,12)
  month(df.promsem$Inicio)
  oct_abr = which(month(df.promsem$Inicio) %in% OA )
  
  return(df.promsem[oct_abr,])
}  
#---------------------------------------------------------------------------------------
#-----------------------------------------------------------------------------------------
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
BuscarFechaExtrema <- function(Ext, Startdate, Columna){
  ## Ext : Data frame. Tabla con las fechas extremas separadas las regiones en columnas
  ## Startdate: Vector con las fechas de inicializacion del modelo
  ## Columna: Character vector. Columna o Columnas deseadas del data frame EXT
  
  # Fuerzo a data table
  ext = as.data.table(Ext)
  
  # Crea data table con las fechas de inicio totales
  inicios = data.table("ini" = as.Date(Startdate)) 
  
  # Crea data table con las fechas extremas de la region
  vecfechas <- c()
  for (n in 1:length(Columna)) {
    fecha_region = ext[,get(Columna[n])]
    vecfechas = append(vecfechas,fecha_region)
  }
  dt.fechas = data.table("region" = as.Date(vecfechas))
  
  # Encontrar las posiciones de las fechas extremas mas cercanas, ya que 
  # no son exactamente iguales
  pos_extrema = inicios[dt.fechas, on = .(ini = region),roll = "nearest", which = T]
  
  return(pos_extrema)
  
}
#-----------------------------------------------------------------------------------------
# Funcion que calcula el coeficiente de correlacion entre dos arrays de mismas dimensiones 
# punto a punto 
ACC <- function(Lon,Lat,Model,Anom){
  ## Lon: Numeric. Cantidad de puntos longitudinales
  ## Lat: Numeric. Cantidad de puntos latitudinales
  ## Model: Array de cuatro dimensiones (lon,lat,startdate,week)
  ## Anom: Array de cuatro dimensiones (lon,lat,startdate,week)
  # Para el calculo de ACC hago una vuelta mas, para recorrer todos los puntos y obtener un valor de correlacion
  acc <- array(NA, dim = c(Lon,Lat,4))
  for (week in 1:4) {
    for (lon in 1:Lon) {
      for (lat in 1:Lat) {
        
        # Me quedo solo con una semana a analizar y todos las fechas de pronostico
        anom.week <- Anom[,,,week]
        model.week <- Model[,,,week]
        # Me quedo solo con un punto particular y todas las fechas de pronostico
        observ <- anom.week[lon,lat,]
        modelo <- model.week[lon,lat,]
        
        coef_corr <- cor(observ,modelo,use="pairwise.complete.obs",method = "pearson")
        
        acc[lon,lat,week] <- coef_corr
        
      } # End loop lat
      
    }  # End loop lon
    
  } # End loop week
  return(acc)
}
#-------------------------------------------------------------------------------------------
# Funcion que encuentra los puntos dentro de un poligono delimitado en un data frame
# Funciona todo con df, devuelve uno
PuntoDentroPoligono <- function(Poli,Data) {
  ## Poli: Data frame del poligono
  ## Data: Array con la informacion 
  library(secr)
  
  # Convierto en Data frame 
  df = reshape2::melt(Data)
  
  # Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
  puntospoli=pointsInPolygon(df[,1:2],Poli) 
  df_poli = df[puntospoli,]
  
  return(df_poli)
}
#--------------------------------------------------------------------------------------
# Funcion que pega un asterisco al lado del numero cuando se cumple una condicion logica
EsSignificativo <- function(Cor,Test) {
  ## Cor: Numeric. Vector con los valores a testear
  ## Test: Logic. Vector del mismo largo que Cor que indique TRUE donde se quiera el asterisco
  
  Cor_as = ifelse(test, yes = paste0(Cor,"*"), no = Cor)
  return(Cor_as)
}
