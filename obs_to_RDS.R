# Observations dataset to compare to SubX data (1999-2015)
#
# M. Alvarez 2020
# Modified by Lucia M Castro 
#-----------------------------------------------------------------------------
rm(list=ls())


# Call libraries to be used
library("ncdf4")
library("metR")
library("abind")
library("fields")
library("data.table")
library("plyr")
#library("data.cube")

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/pikachu/datos4/Obs/t2m_cpc_daily"
#savepath = "/datos/SubX/hindcast/tassfc/daily/ensmean/"  # NO Pongas /vegeta porque ya estas parada ahi

# Seteo el directorio
setwd(savepath)

#----------------------------------------------------------------------------------
# LECTURA DE LOS DATOS TMAX Y TMIN Y CALCULO DE TPROMEDIO A 2M PARA TODO 
# EL PERIODO SIN RESTRINGIR MESES


# Hago una variable para llenar con todas las fechas diarias desde 1999 hasta 2015
# 4 anios bisiestos, 13 anios no bisiestos 
t2m_sa_years <- array(NA, c(66,76))

for (i in 1:17) {
  
  year <- 1999:2015
  
  #-----------------------------------------
  # ABRIR DATOS 
  
  # Abro los archivos para T maxima y T minima
  nc_path_max=paste0("/pikachu/datos4/Obs/t2m_cpc_daily/tmax.",year[i],".nc")
  nc_path_min=paste0("/pikachu/datos4/Obs/t2m_cpc_daily/tmin.",year[i],".nc")
  
  nc_tmax <- ReadNetCDF(nc_path_max,subset = list(lat = -60:15, lon = 265:330),out = "array")
  nc_tmin <- ReadNetCDF(nc_path_min,subset = list(lat = -60:15, lon = 265:330),out = "array")
  #nc_tmin <- nc_open(paste0("/pikachu/datos4/Obs/t2m_cpc_daily/tmin.",year[i],".nc"))
  
  # Consigo los datos 
  tmax = nc_tmax[[1]]
  tmin = nc_tmin[[1]]
  
  # Leo dimensiones (mismas para ambas variables)
  lon <- dimnames(tmax)$lon
  lat <- dimnames(tmax)$lat
  tiempos <- dimnames(tmax)$time
  
  # missig value -9.96921e+36  which(tmin==-9.96921e+36)
  
  # Variable temperatura a 2 metros se obtiene como el promedio de Tmax y Tmin
  t2m = (tmax + tmin)/2
  
  #------------------------------------------------------------------
  # INTERPOLACION
  # lograr que los datos esten en una grilla 1x1 con numeros enteros
  
  # Creo lista vacia para llenar con los datos de todo un anio
  lista <- list()
  
  # Necesita un loop alrededor de todos los dias del año
  
  for (dias in 1:length(tiempos)) {
    # Defino un objeto con coordenadas y datos de mi campo
    obj <- list(x = lon, y = lat,
                z = t2m[,,dias])
    
    # Defino un grid con la dimensión la resolución que deseo,
    grid.list <- list(x =seq(265,330, 1), y = rev(seq(-60,15, 1)))
    # Funcion para realizar una interpolación
    # bilineal a la nueva reticula
    obj2 <- interp.surface.grid(obj, grid.list)
    
    # Guardo en la variable todos los dias de un solo año
    lista[[dias]] <- obj2[[3]] 
  
  
  } # Termina loop sobre los dias del año

  
  #-----------------------------------------------------------------------------
  # GUARDADO DE DATOS
  
  # Convierto la lista en array con el paquete abind
  # en el tercer elemento se encuntran los datos de T2m
  array_2 <- abind(lista,along = 3)
  
  # Guardo los datos en un solo array con las dimensiones de lon,lat de Sudamerica
  # y todos los dias del periodo 
  t2m_sa_years <- abind(t2m_sa_years,array_2,along = 3)

  
  
}


# Se quita el primer valor que es NA
t2m_sa_years <- t2m_sa_years[,,-1]
# Lleno la variable con las fechas
tiempos_total <- seq.Date(as.Date("1999-01-01"),as.Date("2015-12-31"),by=1)

#-----------------------------------------------------------------------------------------
# ANIOS BISIESTOS

# Renombrar la dimension de tiempo

dimnames(t2m_sa_years)[[3]] <- tiempos_total

# Sigo trabajando con el array
bisis=c("2000-02-29","2004-02-29","2008-02-29","2012-02-29") 
nb=setdiff(tiempos_total,bisis) # me quedo solo con los dias que no son 29/2 en el período

data.nobis=t2m_sa_years[,,nb]

# Primer grafico prueba para la semana del 12-18 febrero 2000
med= apply(t2m_sa_years[,,408:416], c(1,2), mean)
p = ggDataFrame(med)
GraphDiscrete(Data = p,Titulo = "T2M ",Breaks = seq(0,30,5),Direccion = -1,Paleta = "RdBu",Label = "C")

# ---------------------------------------------------------------------------------------
# CLIMATOLOGIA
# Reshape en los años
obs = array(data.nobis,c(66,76,365,17))
# Le doy nombre a las dimensiones del nuevo array
dimnames(obs) <- list(lon = seq(265,330, 1),lat = rev(seq(-60,15, 1)),monday = substr(tiempos_total[1:365],6,10),year=1999:2015)

dimnames(t2m_sa_years) <- list(lon = dimnames(obs)$lon , lat = dimnames(obs)$lat, dia = tiempos_total)
# Calculo la media en la dimensión de los años (tomo 1999-2015)
climday = apply(obs, c(1,2,3), mean, na.rm=TRUE)

climdayperiodic=array(data=NA_real_,dim=c(66,76,(31+365+31)))
climdayperiodic[,,1:31]=climday[,,335:365]
climdayperiodic[,,32:(365+31)]=climday[,,]
climdayperiodic[,,(365+31+1):427]=climday[,,1:31]


smoothclim=array(data=NA_real_,dim=c(66,76,(31+365+31)))

for(i in 1:66){
  for(j in 1:76){
    smoothclim[i,j,]=frollapply(climdayperiodic[i,j,],31,mean,align = "center")
    smoothclim[i,j,]=frollapply(smoothclim[i,j,],31,mean,align = "center")
  }
}

smoothclim=smoothclim[,,32:(365+31)] # Mejor =)

# Agrego 29/2=(28/2+1/3)/2

clim=array(data=NA_real_,dim=c(66,76,366))
clim[,,1:(31+28)]=smoothclim[,,1:(31+28)]
clim[,,60]=(smoothclim[,,59]+smoothclim[,,60])/2 #28/2+1/3
clim[,,61:366]=smoothclim[,,60:365]

rm("climday","climdayperiodic","smoothclim")
dimnames(clim) = list(lon = dimnames(obs)$lon , lat = dimnames(obs)$lat, monday = substr(tiempos_total[(1+365):(365*2+1)],6,10))

# Calculo de anomalias usando arrays
# primero agrego el 29 de febrero en todos los años como NA para restarle la climatologia 
#usar append after
tiempos_total
mes_dia = substr(tiempos_total,6,10)
which(mes_dia == "02-28")
prueba = append(t2m_sa_years[,,1])
more.bis = array(NA, dim = c(66,76,366,17))
more.bis[,,1:59,1] <- t2m_sa_years[,,1:59]

# array a completar 
more.bis = array(NA, dim = c(66,76,366*17))

for (s in 1:17) {
  
  # Busca posiciones donde esta el 28 de febrero
  mes_dia = substr(nb,6,10)
  dia28_nb = which(mes_dia == "02-28")
  
  # dias 29/02 en el objeto final
  dia29_falso = seq(60,366*17,366)
  
  # completo con t2m y dejo un campo vacio para todos los 29 de febrero (bisiesto o no bisiesto)
  
  if (s==1) { # primer año distinto
    more.bis[,,1:59] <- data.nobis[,,1:59]
  } else if(s<17) {
    inicio = dia29_falso[s-1]+1   # salta un dia para dejar el 29 vacio
  more.bis[,,inicio:(inicio+364)] <- data.nobis[,,(dia28_nb[s-1]+1):(dia28_nb[s])]
  } else { # ultimo año distinto
    more.bis[,,(5916+1):(366*17)] <- data.nobis[,,(dia28_nb[s]+1):6205]
  }
}



# ahora lleno los campos bisiestos reales 
bis = tiempos_total %in% bisis
dia_mes_full = substr(tiempos_total,6,10)
bisiesto = c(426,1890,3354,4818)
for (b in 1:4) {
  more.bis[,,bisiesto[b]] <- t2m_sa_years[,,bis][b]
  
}

# 
more.bis 
arr = rep(clim,17)
ar.clim2 = array(clim, dim = c(66,76,366,17))
ar.clim = array(ar.clim2, dim = c(66,76,6222))
ar.clim = array(arr, dim = c(66,76,6222))
v = more.bis-ar.clim
semana = apply(v[,,409:415], c(1,2), mean)

df.anom= ggDataFrame(semana)
df.anom = ggDataFrame(more.bis[,,408])
GraphDiscrete(df.anom, Breaks = seq(0,30,5),Titulo = "Anom con array", Paleta = "RdBu",Label = "C", Direccion = -1)
GraphDiscrete(df.anom, Breaks = seq(-6,6,1),Titulo = "Anom con array", Paleta = "RdBu",Label = "C", Direccion = -1)

######
# REPLICA CALCULO MARISOL
week = c("2000-02-12","2000-02-13","2000-02-14","2000-02-15","2000-02-16","2000-02-17","2000-02-18")
promedio_full= apply(t2m_sa_years[,,408:414], c(1,2), mean)
promedio_clim = apply(clim[,,43:49], c(1,2), mean)

GraphDiscrete(ggDataFrame(promedio_full-promedio_clim), Breaks = seq(-6,6,1),Titulo = "Calculo marisol", Paleta = "RdBu",Label = "C", Direccion = -1)
Data= ggDataFrame(promedio_full-promedio_clim)
Data = reshape2::melt(promedio_full-promedio_clim)
dimnames(Data)[[2]] <- list("x","y","z")
ggplot() +
  geom_contour_fill(data=Data,aes(x, y, z = z))

### anomalias para cada dia sin merge 12-18 de febrero 2000
day =c("2000-02-12","2000-02-13","2000-02-14","2000-02-15","2000-02-16","2000-02-17","2000-02-18")
day2 =c("0212","0213","0214","0215","0216","0217","0218")
w_nomerge = t2m_sa_years[,,408:414]-clim[,,43:49]
for (y in 1:7) {

  df=ggDataFrame(w_nomerge[,,y])
  GraphDiscrete(Data=df,Breaks = seq(-6,6,1),Titulo = paste0("Anom T2M sin merge \n",day[y])
                , Paleta = "RdBu",Label = "C", Direccion = -1)
  ggsave(paste0("/home/lucia.castro/SubX_processed_Rdata/nomerge_",day2[y],".png"),width = 10, height = 11)
  
}
### anomalias para cada dia con merge 12-18 de febrero 2000
dt.aver = dt.anom[dia %in% day]

for (u in 1:7) {
  
  dt=dt.anom[dia == day[u]]
  GraphDiscrete(Data=dt,Breaks = seq(-6,6,1),Titulo = paste0("Anom T2M con merge \n",day[u])
                , Paleta = "RdBu",Label = "C", Direccion = -1)
  ggsave(paste0("/home/lucia.castro/SubX_processed_Rdata/simerge_",day2[u],".png"),width = 10, height = 11)
  
}

### anomalias para cada dia con array 12-18 de febrero 2000
w_array=v[,,409:415]
for (u in 1:7) {
  
  dt=ggDataFrame(w_array[,,u])
  GraphDiscrete(Data=dt,Breaks = seq(-6,6,1),Titulo = paste0("Anom T2M con array \n",day[u])
                , Paleta = "RdBu",Label = "C", Direccion = -1)
  ggsave(paste0("/home/lucia.castro/SubX_processed_Rdata/conarray_",day2[u],".png"),width = 10, height = 11)
  
}

# Calculo la anomalia 

# repito la climatologia para 17 años
clim2 = array(clim, dim = c(66,76,366,17))
full = array(more.bis,dim = c(66,76,366,17))
anom = full-clim2

m = apply(anom[,,43:49,2], c(1,2), FUN = mean)
df.anom = ggDataFrame(m)
GraphDiscrete(df.anom, Breaks = seq(-6,6,1),Titulo = "Anom con array", Paleta = "RdBu",Label = "C", Direccion = -1)

# -------
# solo esa semana 12 -18 febrero 2000
semana = c("02-12","02-13","02-14","02-15","02-16","02-17","02-18")
semanita = dia_mes_full %in% semana
sem2 =apply(t2m_sa_years[,,semanita], c(1,2),mean)
anom2 = sem2 - apply(sem_clim, c(1,2),mean)
sem_clim = clim[,,43:49]
sem_full = t2m_sa_years[,,408:414]
sem_anom2 = apply(sem_full, c(1,2), mean)- apply(sem_clim, c(1,2), mean)
sem_anom = sem_full- sem_clim
sem_media = apply(sem_anom, c(1,2), mean)

ggDataFrame(sem_media)
GraphDiscrete(Data = ggDataFrame(anom2), Titulo = "anom",Label = "C",Paleta = "RdBu",Direccion = -1,
              Breaks = seq(-6,6,1))
seq.Date(as.Date("2000-01-01"),as.Date("2000-12-31"),by = 1)

ggplot(ggDataFrame(sem_anom2), aes(x=x,y=y,z=z))+
  geom_contour_fill(breaks = seq(-6,6,1)) +
  scale_fill_divergent(breaks = seq(-6,6,1))
# Listo, ahora seguiría hacer las data tables, darle merge usando "monday,lat y lon" y después hacer la resta
# Aca es necesario que se mantengan los NaN para conservar las dimensiones
dt.clim=as.data.table(clim, na.rm = F)
dt.obs=as.data.table(t2m_sa_years, na.rm = F)


# Esta sentencia ordena de manera correcta las latitudes que generaba problemas al incluir negativos
dt.clim = dt.clim[order(match(lat, as.numeric(dimnames(obs)$lat)))]
dt.obs = dt.obs[order(match(lat, as.numeric(dimnames(obs)$lat)))]

# renombro variable
setnames(dt.clim, "value", "clim")
# Guardo la data table de la climatologia
saveRDS(dt.clim, file = "t2mclim_NOAA.rds")

# Agrego la dimension "monday" a dt.obs
dt.obs$monday=substr(as.character(dt.obs$dia),6,10)
setnames(dt.obs, "value", "t2m")

dt.anom=merge(dt.obs,dt.clim,by=c("lon","lat","monday"))
dt.anom$anom=dt.anom$t2m-dt.anom$clim

# Esta sentencia ordena de manera correcta las latitudes que generaba problemas al incluir negativos
dt.anom = dt.anom[order(match(lat, as.numeric(dimnames(obs)$lat)))]
dt.anom = dt.anom[order(match(dia, dt.obs$dia))]

dt.clim[lat == -22 & lon==300]
dt.obs[lat == -22 & lon==300]
f= dt.anom[lat == -22 & lon==300]
# Elimino las columnas de obs y climatología y esas data tables
dt.anom$t2m=NULL
dt.anom$clim=NULL
dt.anom$monday=NULL
rm("dt.obs","dt.clim")

dimnames(dt.anom)[[2]] <- list("x","y","dia","z")
dt.anom$x <- as.numeric(dt.anom$x)
dt.anom$y <- as.numeric(dt.anom$y)
aver = seq.Date()
dt.aver = dt.anom[dia %in% c("2000-02-12","2000-02-13","2000-02-14","2000-02-15","2000-02-16","2000-02-17","2000-02-18")]
dt.aver = dt.aver[,.(z = mean(z,na.rm=T)),by = .(x,y)]
dt.aver 
GraphDiscrete(dt.aver, Breaks = seq(-6,6,1),Titulo = "Anom con array", Paleta = "RdBu",Label = "C", Direccion = -1)

# renombro la fila de las fechas para luego combinar
setnames(dt.anom, "dia", "targetdate")

# Ajusto los tipos de las variables para luego acoplar a los datos de SubX
dt.anom$lon=as.numeric(dt.anom$lon)
dt.anom$lat=as.numeric(dt.anom$lat)
dt.anom$targetdate=as.Date(dt.anom$targetdate)

####
lon = unique(dt.anom$lon)
lat = unique(dt.anom$lat)

# Este array es de 237 Mb, la data table es de 1 GB
array.anom <- array(dt.anom$anom, dim = c(length(lon), length(lat), length(tiempos_total)))
dimnames(array.anom) <- list("lon" = lon , "lat" = lat , "day" = as.character(tiempos_total))


# Guardo la data table de las observaciones
saveRDS(array.anom, file = "t2manom_NOAA.rds")
saveRDS(dt.anom ,file = "t2manom_data.table_NOAA.rds")
#end


# ---------------------------------------------------------------------------------------------
# Graficos

# Reordeno los datos para graficar solo las medias mensuales desde Octubre a Marzo
## IGNORAR ESTO, AUN LO ESTOY TRABAJANDO
# Graficos de anomalia semanal media para comparar con los graficos de NOAA PSL
# https://psl.noaa.gov/data/composites/day/
# Lucia M. Castro 
# 


# Aca lei los datos por no correrlo todo seguido (ignorar la sentencia)
ar.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")



setDT(tmeasmax)[, .(MontlyMeans = mean(MEAN.C.)), by = .(year(TIMESTEP), month(TIMESTEP))]
dt.anom[,.(MonthMean = mean(anom), by = .(format(targetdate,"%Y"), months(targetdate)))]

dt.anom$mes <- month(dt.anom$targetdate)

# Creo estas columnas para calcular la media mensual
dt.anom$mesdia = substr(as.character(dt.anom$targetdate),6,10)
dt.anom$year = substr(as.character(dt.anom$targetdate),1,4)

dt.anom[,.(mediamensual= mean(anom), by = .(mesdia,year))]

# Seteo una key para realizar los procesos mas rapidos
setkey(dt.anom, mesdia)
# Ahora cuando aplico el filtro no es necesario indicar la columna en la que aplicar el filtro
enero <- dt.anom[J(1)]
dia <- dt.anom[c("01-01")]

# Uso el reordenamiento que usaba hasta ahora

for (s in 1:6) {
  meses <- c(1, 2, 3, 10, 11, 12)                   # Meses Octubre a Marzo
  month <- month(tiempos_total)                     # Vector con "01","02" repetidos segun la cant de dias
  
  # Tomo los valores de t2m para un solo mes por vez
  mes <- which(month == meses[s])                   # Vector con Indices del mes en cuestion
  t2m_mes <- t2m_sa_years[,,mes]                    # Evaluo en todas lat y lon para ese mes
  media_mensual <- apply(t2m_mes, c(1,2), mean)     # Calculo Media Mensual
  
  # Uso esta variable para graficar
  
  nombre_mes <- c("Enero", "Febrero", "Marzo", "Octubre", "Noviembre", "Diciembre")
  valores <- as.vector(media_mensual)
  
  # Obtengo las dimensiones necesarias
  # Convertirlo a numeric porque sino se generan problemas
  x <- as.numeric(dimnames(obs)$lon)
  y <- as.numeric(dimnames(obs)$lat)
  
  
  # Creo los data frames necesarios
  # En la primer columna tiene todas las long repetidas la cantidad de veces de las latitudes
  # En la segunda columna tiene todas las lat repetidas la cantidad de veces de las longitudes
  # En la tercera columna tiene los valores de media mensual para 1 mes en particular
  data_temp<-data.frame(x=rep(x,length(y)),
                        y=rep(y,each=length(x)),
                        z=valores)

  
  # Grafico 
  GraphDiscrete(Data = data_temp,
                Breaks = seq(-5,35,5),
                Titulo = p("T2M Media", "\n",nombre_mes[s]), 
                Label = "  C", 
                Paleta = "RdBu")
  
  ggsave(filename = p("t2mMedia_",nombre_mes[s],".png"), path = "/home/lucia.castro/tesina/imagenes/")
  
  GraphDiscrete2(Data = data_temp,
                Breaks = seq(-5,35,5),
                Titulo = p("T2M Media", "\n",nombre_mes[s]), 
                Label = "  C", 
                Paleta = "RdBu")
  
  ggsave(filename = p("t2mMedia_",nombre_mes[s],"_2",".png"), path = "/home/lucia.castro/tesina/imagenes/")
}
 















