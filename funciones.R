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
  
  # Cargo paquetes
  library("ggplot2")
  library("maps")
  library("RColorBrewer")
  
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$z, na.rm = T)
  max <- max(Data$z, na.rm = T)
  
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
