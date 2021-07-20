t<-dt.anom[1:10]
u<-as.data.table(dt.verif[1:10,])
dt.verif=merge(u,t,by=c("lat","lon","targetdate"))

# Ordeno un poco para que coincidan las columnas
dt.anom = dt.anom[order(lat)]
dt.anom = dt.anom[order(match(targetdate, dt.model$targetdate))]






t<-dt.anom[order(lat)][1:10]
u<-as.data.table(dt.verif[1:10,])
dt.anom[order(lat)]
merge(data1, data2, by = "id", all.x = TRUE)  
dt.verif=merge(u,t,by.x = c("X","Y","targetdate"),by.y=c("lat","lon","targetdate"))



h<- data.frame(lat = c(-100:99),
               lon = c(1:200),
               sarasa = runif(200))

j<- data.frame(X = rep(-100:99,3),
               Y = rep(1:200,3),
               sarasa = runif(600))
GH <- merge(h,j,by.x = c("lat","lon"),by.y = c("X","Y"))




#BUENO INVENTO MI DT.VERIF
dt.verif = data.table(X= rep(seq(265,330,1),6840),
                      Y= rep(seq(-60,15,1),5940),
                      L= rep(seq(1,45,1),10032),
                      startdate=rep("1999-01-01",451440),
                      tasaem = runif(451440),
                      targetdate = rep(c("1999-01-02","1999-01-03"),225720),
                      #week = rep(seq(1,4,1),112860),
                      anom = unique(dt.anom$anom)[1:451440])

dt.verif= dt.verif[order(Y)]
dt.verif= dt.verif[order(L)]
saveRDS(dt.verif,file = "probando.rds")
subset = dt.anom[1:10032] #agarro dos dias 
long = unique(subset$lon)
lati= unique(subset$lat)
tiempito = unique(subset$targetdate)

anom_array_2 <- array(subset$anom, dim = c(length(long), length(lati), length(tiempito)))
dimnames(anom_array_2) <- list("lon" = long , "lat" = lati , "day" = as.character(tiempito))








########################################################
ar.model.w1
mean(ar.model)
#media en la tercera dimension

model_media_semanal = apply(ar.model.w1, c(1,2,4), FUN = mean)
ar.anom.2 = array(NA, dim = c(66,76,28,1))
targetdate3 = targetdate2[1:28,]
targetdate2
g= as.vector(targetdate[1:7,1])
g
k = dimnames(ar.anom)$day  
targetweek = as.character(dimnames(ar.anom)$day) == as.character( g )


ar.anom.w1 = ar.anom[,,which()]
str_detect(k,g)
which(k %in% g)
targetweek = ar.anom[,,which(k %in% g)]
anom_media_semanal = apply(targetweek, c(1,2), FUN = mean)


anom_media_semanal <- array(NA, dim = c(66,76,1241))
days = dimnames(ar.anom)$day 
# Hago un ciclo para obtener un campo medio de las anom observadas segun la semana 
for (startdate in (1:1241) ) {
  
  
  week1 = as.vector(targetdate[1:7, startdate])
  
  targetweek = ar.anom[,,which(days %in% week1)]
  media_semanal = apply(targetweek, c(1,2), FUN = mean, na.rm = TRUE)
  anom_media_semanal[,,startdate] <- media_semanal
}

h = (anom_media_semanal - model_media_semanal)
rmse = (h)^2

AVER =apply(rmse, c(1,2), mean, na.rm = FALSE)
AVER = sqrt(AVER)

acc=cor(anom_media_semanal[,,1], model_media_semanal[,,1] , use = "pairwise.complete.obs",method = "pearson")

x <- as.numeric(dimnames(ar.anom)$lon)
y <- as.numeric(dimnames(ar.anom)$lat)
valores <- as.vector(model_media_semanal[,,1,1])
valores <- as.vector(h[,,1,1])
valores <- as.vector(AVER)
valores <- as.vector(targetweek[,,1])
valores <- as.vector(media_semanal[,,1])
valores <- as.vector(ar.anom[,,1])
valores <- as.vector(t2m_sa_years[,,1])
valores <- as.vector(dif[,,1,1])

# Creo los data frames necesarios
# En la primer columna tiene todas las long repetidas la cantidad de veces de las latitudes
# En la segunda columna tiene todas las lat repetidas la cantidad de veces de las longitudes
# En la tercera columna tiene los valores de media mensual para 1 mes en particular
data_temp<-data.frame(x=rep(x,length(y)),
                      y=rep(y,each=length(x)),
                      z=valores)
maximo = max(valores, na.rm = T)
minimo = min(valores, na.rm = T)

breaks = seq(minimo,maximo,1)
GraphDiscrete(data_temp, Breaks = breaks, Titulo = "A VER QUE SALE", Paleta = "RdBu",Label = "media") 
breaks = seq(-1,1,0.2)
GraphDiscrete(data, Breaks = breaks, Titulo = "A VER QUE SALE", Paleta = "RdBu",Label = "media") 
GraphDiscrete2()


# Coeficiente de correlacion para cada punto de lat y lon 
anom_media_semanal[1,1,]
acc <- apply(anom_media_semanal, c(1,2), FUN = cor, model_media_semanal)
acc <- array(NA, dim = c(66,76))

for (lon in 1:66) {
  for (lat in 1:76) {
    
    observ <- anom_media_semanal[lon,lat,]
    modelo <- model_media_semanal[lon,lat,]
    
    coef_corr <- cor(observ,modelo,use="pairwise.complete.obs",method = "pearson")
    
    acc[lon,lat] <- coef_corr
    
  }
  
}

# Calculo del rcritco para testear con 95% de confianza

rc <- 2.33/sqrt(612-2)

# Pruebo colocar datos en array a partir de data.table
ar <- array(1:72,dim=c(72,1))
dimnames(ar) <- list("lon","lat")
dt<- data.table(ar)

ar2<- array(dt$V1,dim = c(12,2,3))

dt.anom$anom[0:66]

dt.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_data.table_NOAA.rds")
test=dt.verif[,rlutaem.w:=mean(rlutaem,na.rm=TRUE),by=.(lat,lon,startdate,week)]
test=dt.anom[,,which(dt.anom$targetdate=="1999-01-01"),]
semana = c("1999-01-02","1999-01-03","1999-01-04","1999-01-05","1999-01-06","1999-01-07","1999-01-08")
semana = c("2011-11-08","2011-11-09","2011-11-10","2011-11-11","2011-11-12","2011-11-13","2011-11-14")

test<-which(dt.anom$targetdate %in% semana)
week1 <- dt.anom[test]
dimnames(week1)[[2]] <- list("x","y","targetdate","z")
week1$x <- as.numeric(week1$x)
week1$y <- as.numeric(week1$y)
week1.mean <- week1[,anom.w:=mean(z,na.rm = T),by=.(x,y,targetdate)]
week1.mean=unique(week1.mean, incomparables=FALSE, fromLast=FALSE)

dimnames(week1.mean)[[2]] <- list("x","y","targetdate","anom","z")
breaks = seq(-6,6,1)
GraphDiscrete(week1.mean, Breaks = breaks, Titulo = "T2M week mean \n 08-14 Nov 2011", Paleta = "RdBu",Label = "°C") 



#####################
array1 = array(1:6,dim=(c(2,3)))
array1

array_Rep = array(1:24,c(2,3,4))
array_Rep

array_rep = array(array_Rep,c(2,3,2,2))
array_rep

#------------------------- PROBANDING

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
  library("scales")
  # Seteo los parametros de mapa y gradiente 
  mapa<-map_data("world2") 
  min <- min(Data$z, na.rm = T)
  max <- max(Data$z, na.rm = T)
  ar.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")
  ggDataFrame()  
  Data = ggDataFrame(ar.anom[,,6200]) 
  Breaks = seq(-6,6,1)
  Label = "C"
  Titulo = "Prueba"
  Direccion = 1
  Paleta = "RdBu"
  Data$z=oob_squish(Data$z,range = c(-6,6))
  mascara = ar.anom[,,6200]>1
  mascara[mascara] <- 1
  ggDataFrame(mascara)
  lon
  lon = seq(265,330,1)
  lat = rev(seq(-60,15,1))
  
  mascara
  dimnames(mascara)$lon
  sum(mascara, na.rm = T)
  tito= which(mascara, arr.ind = T)
  punto_lon = lon[tito[,1]]
  punto_lat = lat[tito[,2]]  
  df=data.frame(lon = punto_lon, lat= punto_lat)
  datapoli = data.frame(
    lon = c(291,288,293,298),
    lat = c(-30,-40,-46,-40))
  datasacz = data.frame( lon = c(300,300,310,317),
                         lat=c(-1,-15,-30,-21)
  )
  # norte, oeste, sur, este (se lee de izq a derecha)
  # Grafico en si 
  ggplot() +
    geom_contour_fill(data=Data,aes(x, y, z = z),breaks = c(-6,-4,-2,0,2,4,6)) +
    #geom_point(data=df, aes( x=lon, y=lat), col="black",size=0.5, na.rm = T)+
    scale_x_longitude(breaks = c(280,300, 320),expand = c(0.09, 0.09)) +
    scale_y_latitude(breaks = c(-40,-20,0),expand = c(0.09, 0.09)) +
    scale_fill_distiller(name=Label,palette=Paleta,direction= Direccion,
                         labels = Breaks,
                         
                         breaks = Breaks,
                         limits = c(min(Breaks), max(Breaks)),
                         guide = guide_colorstrip()) +
    
    ggtitle(Titulo)  +
    geom_map(dat=mapa, map = mapa, aes(map_id=region), fill="NA", color="black", inherit.aes = F)+
    geom_polygon(data= datapoli, aes(x=lon, y=lat),color= 'slateblue',fill= NA,size=0.8)+
    geom_polygon(data= datasacz, aes(x=lon, y=lat),color= 'seagreen',fill= NA,size=0.8)
  theme(axis.text=element_text(size=12))+
    theme(strip.text.x = element_text(size = 12, colour = "black"))+
    
    theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
    theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                          size = 2, linetype = "solid"),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = "grey86"),
          panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                          colour = "grey86")) +
    
    theme(plot.title = element_text(hjust = 0.5))
  
}
library(mgcv)
data(columb.polys)
bnd <- columb.polys[[2]]
plot(bnd,type="n")
polygon(bnd)
x <- seq(7.9,8.7,length=20)
y <- seq(13.7,14.3,length=20)
gr <- as.matrix(expand.grid(x,y))
inside <- in.out(bnd,gr)
points(gr,col=as.numeric(inside)+1)

as.matrix(expand.grid(x_coords,y_coords))
p<-polygon(as.matrix(expand.grid(x_coords,y_coords)))

library(secr)
pointsInPolygon 
## 100 random points in unit square
xy <- matrix(runif(200), ncol = 2)
## triangle centred on (0.5, 0.5)
poly <- data.frame(x = c(0.2,0.5,0.8,0.2), y = c(0.2,0.8,0.2,0.2))
plot(xy, pch = 1 + pointsInPolygon(xy, poly))
lines(poly)

#######################3
# POLYGON
# load library
library(sp)

# Make a set of coordinates that represent vertices
# with longitude and latitude in the familiar
# degrees  (se repite el primer punto para unir el poligono)
lon = c(291,288,293,298)
lat = c(-30,-40,-46,-40)
x_coords <- c(291,288,293,298,291)
y_coords <- c(-30,-40,-46,-40,-30)
poligon = polygon()

poly1 <- sp::Polygon(cbind(x_coords,y_coords))
firstPoly <- sp::Polygons(list(poly1), ID = "A")
in.out(p,gr)
poly1 <- data.frame(x_coords <- c(291,288,293,298,291),
                    y_coords <- c(-30,-40,-46,-40,-30))
coord = data.frame(lon=rep(lon,length(lat)),
                   lat=rep(lat, each=length(lon)))

library(secr)
pointsInPolygon(coord,poly1) 
lat
cbind(lon=rep(lon,length(lat)),
      lat=rep(lat, each=length(lon)))

