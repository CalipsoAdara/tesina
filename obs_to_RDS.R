# Observations dataset to compare to SubX data (1999-2015)
#
# M. Alvarez 2020
#-----------------------------------------------------------------------------
rm(list=ls())

setwd("/home/lucia.castro/")

# Call libraries to be used
library("ncdf4")
library("metR")

# Read data from NOAA
inFname='http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP/.CPC/.GLOBAL/.daily/.olr/time/%281%20Jan%201998%29%2831%20Dec%202017%29RANGEEDGES/lat/%285N%29%2840S%29RANGEEDGES/lon/%28290%29%28327.5W%29RANGEEDGES/lon/%2870W%29%2832.5W%29RANGEEDGES/dods'


# Open File
ncid=nc_open(inFname, write=FALSE, readunlim=TRUE, verbose=FALSE,auto_GMT=TRUE, suppress_dimvals=FALSE )

# Determine number of variables, dimensions, etc. in file
ndims = ncid$ndims
nvars = ncid$nvars
ngatts = ncid$natts
unlimdimid = ncid$unlimdimid

# Determine dimensions and size of dimensions
lon=ncid$dim$lon$vals
lat=ncid$dim$lat$vals

# Convert netCDF days since values to calendar dates
origin=substr(ncid$dim$time$units,12,21)
times=as.character(as.Date(ncid$dim$time$vals,origin=as.Date(origin)))

# Get information about specified variable
# Set the dimensions to read - [nlons,nlats,nleads,nens,nlevs,nics]
vardimlen=ncid$var$olr$size   # EL TEMA ACÁ ES QUE ME QUEDA EL NOMBRE DE LA VARIABLE EN EL MEDIO DE VARDIMLEN

data = metR::ReadNetCDF(inFname, out='array')
data = data[[1]]
# Rename times dimension      
dimnames(data)$time=times
dimnames(data)$lon=seq(290,327.5,2.5)

# Si quisiera convertirlo en data.table
dt.olr=as.data.table(data)

# Sugo trabajando con el array
bisis=c("2000-02-29","2004-02-29","2008-02-29","2012-02-29","2016-02-29") 
nb=setdiff(times,bisis) # me quedo solo con los dias que no son 29/2 en el período

data.nobis=data[,,nb]

# Calculo los días climatológicos
# Reshape en los años
olr1=array(data,c(16,19,365,(7300/365))) 
# Le agrego las dimensiones a la variable
dimnames(olr1) = list(lon = dimnames(data.nobis)$lon , lat = dimnames(data.nobis)$lat, monday = substr(times[1:365],6,10), year=1998:2017)
olr1=olr1[,,,2:18] # Me quedo con 1999-2015

# Calculo la media en la dimensión de los años (tomo 1999-2015)
climday = apply(olr1, c(1,2,3), mean, na.rm=TRUE)

climdayperiodic=array(data=NA_real_,dim=c(16,19,(31+365+31)))
climdayperiodic[,,1:31]=climday[,,335:365]
climdayperiodic[,,32:(365+31)]=climday[,,]
climdayperiodic[,,(365+31+1):427]=climday[,,1:31]

#movavg From pracma v1.9.9
# t for ``triangular'', it computes the triangular moving average by calculating the first simple moving average with window width of ceil(n+1)/2; 
# then it calculates a second simple moving average on the first moving average with the same window size.

# Para que sea de ancho +/- 15 días debería elegir n=29 tal que ceil=15
# Eso no está dando como espero. El triangular mvavg son dos promedios móviles sucesivos. Creo que lo voy a discretizar por paso para hacerlo centrado

smoothclim=array(data=NA_real_,dim=c(16,19,(31+365+31)))

for(i in 1:16){
  for(j in 1:19){
    smoothclim[i,j,]=frollapply(climdayperiodic[i,j,],31,mean,align = "center")
    smoothclim[i,j,]=frollapply(smoothclim[i,j,],31,mean,align = "center")
  }
}

smoothclim=smoothclim[,,32:(365+31)] # Mejor =)

# Agrego 29/2=(28/2+1/3)/2

clim=array(data=NA_real_,dim=c(16,19,366))
clim[,,1:(31+28)]=smoothclim[,,1:(31+28)]
clim[,,60]=(smoothclim[,,59]+smoothclim[,,60])/2 #28/2+1/3
clim[,,61:366]=smoothclim[,,60:365]

rm("climday","climdayperiodic","smoothclim")
dimnames(clim) = list(lon = dimnames(data.nobis)$lon , lat = dimnames(data.nobis)$lat, monday = substr(times[(1+365*2):(365*3+1)],6,10))

# Listo, ahora seguiría hacer las data tables, darle merge usando "monday,lat y lon" y después hacer la resta

dt.clim=as.data.table(clim)
# renombro variable
setnames(dt.clim, "value", "clim")

# Agrego la dimension "monday" a dt.olr
dt.olr$monday=substr(as.character(dt.olr$time),6,10)
setnames(dt.olr, "value", "olr")

dt.anom=merge(dt.olr,dt.clim,by=c("lon","lat","monday"))
dt.anom$anom=dt.anom$olr-dt.anom$clim

# Elimino las columnas de olr y climatología y esas data tables
dt.anom$olr=NULL
dt.anom$clim=NULL
dt.anom$monday=NULL
rm("dt.olr","dt.clim")

# renombro la fila de las fechas para luego combinar
setnames(dt.anom, "time", "targetdate")

# Ajusto los tipos de las variables para luego acoplar a los datos de SubX
dt.anom$lon=as.numeric(dt.anom$lon)
dt.anom$lat=as.numeric(dt.anom$lat)
dt.anom$targetdate=as.Date(dt.anom$targetdate)

# Guardo la data table de las observaciones
saveRDS(dt.anom, file = "olranom_NOAA_9915.rds")

#end
