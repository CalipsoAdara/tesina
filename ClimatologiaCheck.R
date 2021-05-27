# Chequeo de climatologia y pronosticos del modelo GMAO
# 2000-02-10
#-------------------------------------------------------------------------------

# llamar paquetes
library(metR)

# seteo directorio
setwd("/datos/SubX/hindcast/tassfc/daily/")

#"clim/GMAO-GEOS_V2p1"
# 45
# Climatologia de la observacion 
clim.obs= readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2mclim_NOAA.rds")

# Cambio los nombres para graficar
dimnames(clim.obs)[[2]] <- list("x","y","monday","z")
clim.obs$x = as.numeric(clim.obs$x)
clim.obs$y= as.numeric(clim.obs$y)

# Busco los valores solo en mi semana
week = c("02-10","02-11","02-12","02-13","02-14","02-15","02-16")
semana = clim.obs[monday %in% week]
semana = semana[order(match(monday, week))]

# Hago la media de esa semana
media.obs = semana[,media.w:=mean(z,na.rm=T),by=.(x,y)]
media.obs$monday=NULL
media.obs$z=NULL

# Ahora elimino las filas duplicadas (porque los 7 días de cada semana quedaron con el mismo valor)
dt.verif.w=unique(media.obs, incomparables=FALSE, fromLast=FALSE)
dimnames(dt.verif.w)[[2]] <- list("x","y","z")
GraphDiscrete(Data = dt.verif.w, Titulo = "Climatologia\nMedia semanal 10-16 Feb 2002 \nCPC", Paleta = "RdBu",Label = "°C",Breaks = seq(-10,40,5))

# Climatologia del modelo 
fileclim = "/clim/GMAO-GEOS_V2p1"
clim.mod = metR::ReadNetCDF("./clim/GMAO-GEOS_V2p1/tas_sfc_GMAO-GEOS_V2p1_19600210.SouthAmerica.daily.clim.nc")
clim.mod=clim.mod[[1]]
# abro el archivo del modelo en la fecha "2000-02-10"
ar.model = readRDS("/home/lucia.castro/SubX_processed_Rdata/model_GMAO_ONDEFM.rds")
