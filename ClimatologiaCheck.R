# Chequeo de climatologia y pronosticos del modelo GMAO
# 2000-02-12   2000-02-18
#-------------------------------------------------------------------------------

# llamar paquetes
library(metR)

# seteo directorio
setwd("/datos/SubX/hindcast/tassfc/daily/")

# Periodo
periodo = seq.Date(as.Date("1999-01-01"),as.Date("2015-12-31"),by=1)
dia = weekdays(periodo)
sabados = periodo[which(dia == "Saturday")]

# 59
# Climatologia de la observacion 
clim.obs= readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2mclim_NOAA.rds")

# Cambio los nombres para graficar
dimnames(clim.obs)[[2]] <- list("x","y","monday","z")
clim.obs$x = as.numeric(clim.obs$x)
clim.obs$y= as.numeric(clim.obs$y)

# Busco los valores solo en mi semana
week = c("02-12","02-13","02-14","02-15","02-16","02-17","02-18")
week2 = c("2000-02-12","2000-02-13","2000-02-14","2000-02-15","2000-02-16","2000-02-17","2000-02-18")
week = c("11-03","11-04","11-05","11-06","11-07","11-08","11-09")
week2 = c("2001-11-03","2001-11-04","2001-11-05","2001-11-06","2001-11-07","2001-11-08","2001-11-09")
semana = clim.obs[monday %in% week]
semana = semana[order(match(monday, week))]

# Hago la media de esa semana
media.obs = semana[,media.w:=mean(z,na.rm=T),by=.(x,y)]
media.obs$monday=NULL
media.obs$z=NULL

# Ahora elimino las filas duplicadas (porque los 7 días de cada semana quedaron con el mismo valor)
dt.verif.w=unique(media.obs, incomparables=FALSE, fromLast=FALSE)
dimnames(dt.verif.w)[[2]] <- list("x","y","z")
GraphDiscrete(Data = dt.verif.w, Titulo = "CLimatologia\nMedia semanal 12-18 Feb \nCPC", Paleta = "RdBu",Label = "°C",Breaks = seq(0,30,5))
GraphDiscrete(Data = dt.verif.w, Titulo = "CLimatologia\nMedia semanal 03-09 Nov \nCPC", Paleta = "RdBu",Label = "°C",Breaks = seq(0,30,5), Direccion = -1)

# Climatologia del modelo 
for (n in 1:4) {
  
  #inicio = c("2000-02-05","2000-01-29","2000-01-22","2000-01-15")
  #inicio2 = c("0205","0129","0122","0115")
  inicio = c("2001-10-27","2001-10-20","2001-10-13","2001-10-06")
  inicio2 = c("1027","1020","1013","1006")
  w = c("Week 1","Week 2","Week 3","Week 4")
  
  # Tomo los sabados anteriores a la semana en cuestion 02-12
  inFilename = paste0("./clim/GMAO-GEOS_V2p1/tas_sfc_GMAO-GEOS_V2p1_1960",inicio2[n],".SouthAmerica.daily.clim.nc")
  clim.mod = metR::ReadNetCDF(inFilename, out="array")
  clim.mod=clim.mod[[1]]
  
  lead = as.character(as.Date(inicio[n]) + (1:45))
  mod.semana = clim.mod[,,which(lead %in% week2)]
  media.mod = apply(mod.semana, c(1,2), FUN = mean)
  
  # Acomodo para graficar
  dt.clim.mod = reshape2::melt(media.mod, value.name = "z")
  dimnames(dt.clim.mod)[[2]] <- list("x","y","z")
  dt.clim.mod$x = as.numeric(dt.clim.mod$x)
  dt.clim.mod$y= as.numeric(dt.clim.mod$y)
  
  titu = paste0("Climatologia INI",inicio2[n],"\nMedia semanal 03-09 Nov \n GMAO")
  GraphDiscrete(Data = dt.clim.mod, Titulo = titu, Paleta = "RdBu",Label = "°C",Breaks = seq(273,308,5), Direccion = -1)
  ggsave(filename=paste0("/home/lucia.castro/SubX_processed_Rdata/MODclim",inicio2[n],"sem1218.png"),width = 10, height = 11)
}


# abro el archivo del modelo en la fecha "2000-02-10"
ar.model = readRDS("/home/lucia.castro/SubX_processed_Rdata/model_GMAO_ONDEFM.rds")



# Esto lo hago para tener el valor= clim + anom del modelo GMAO
# es la media del ensamble 
# Call libraries to be used
library("ncdf4")
library("metR")
library('pracma')

#--------------------------------------------------------------------------------------
#  Settings
#--------------------------------------------------------------------------------------

inPath='/datos/SubX/hindcast/'
groups=c('GMAO')                       # Modeling Groups (must be same # elements as models below)
models=c('GEOS_V2p1')               # Model Name (must be same # of elements as groups above)
# Elimino el NRL-NESM porque tiene 1 miembro por día, la ensamble mean debería ser laggeada, debería calcularla de otra forma

varnames=c('tas')
plevstrs=c('sfc')

syrs=c(1999) # year of start (per model)
eyrs=c(2015)  # year of end (per model)
nleads=c(45)              # number of lead (per model)
nenss=c(4)


#nvars=numel(varnames)
#nmodels=numel(models)      

# VEO DESPUÉS SI ESTO ES NECESARIO (DÍAS POR MES, MESES POR AÑO)
dpml=c(31,29,31,30,31,30,31,31,30,31,30,31)  # days per month leap
dpmnl=c(31,28,31,30,31,30,31,31,30,31,30,31) # days per month non-leap 
nmpyr=12

imn1=1
imn2=12

# Set Global Attribute Values for netcdf
longtitle='SubX Ensemble mean'
title=longtitle
units='unitless'
unitst='days since 1960-01-01'
comments='SubX project http://cola.gmu.edu/~kpegion/subx/'
source='SubX IRI'
institution='IRI'

varname=varnames[1]
plevstr=plevstrs[1]
emvarname='tasem'

#---------------------------------------------------------------------------------------
#  Main Program  
#---------------------------------------------------------------------------------------

#for(imodel in 1:nmodels){

# Define the model and group and consequent start/end year, leads and ensemble numbers
imodel = 1
model=models[imodel]
group=groups[imodel]
syr=syrs[imodel]
eyr=eyrs[imodel]
nyr=eyr-syr+1
nlead=nleads[imodel]
nens=nenss[imodel]
time=seq(0.5,nlead,1)
  
# Define input directory
inDir1=paste0(inPath,varname,plevstr,'/daily/full/',group,'-',model,'/') # input filename for anomalies data
  
  # Loop over all years
  for(iyr in syr:eyr){
    yyyy=as.character(iyr)
    
    dpm=dpmnl # default: no-leap year
    if (mod(iyr,4)==0){
      dpm=dpml # it is a leap year
    }
    
    # Loop over months
    for(imn in imn1:imn2){
      mm=as.character(imn)
      if(imn<10){mm=paste0('0',mm)}
      
      # Loop over days
      for(idy in 1:dpm[imn]){
        dd=as.character(idy)
        if(idy<10){dd=paste0('0',dd)}
        
        # Create date string
        yyyymmdd=paste0(yyyy,mm,dd)
        mmdd=paste0(mm,dd)
        
        # Create Output directory if needed
        outDir=paste0(inPath,varname,plevstr,'/daily/ensmean_full/',group,'-',model,'/');
        dir.create(outDir,recursive=TRUE)
        
        
        # Loop over all ensemble members
        for(iens in 0:(nenss[imodel]-1)){
          
          # Set the ensemble member string for output file
          ee=as.character(as.integer(iens+1))
          
          # Define files
          inFname=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd,'.e',ee,'.SouthAmerica.daily.nc')
          
          # Evaluate existence of file
          if(file.exists(inFname)){
            
            # Read data
            data = metR::ReadNetCDF(inFname, out='array')
            
            # Check if all values of data are NA
            if(sum(is.na(data[[2]]))==prod(size(data[[2]]))){
              # Means all values are NA 
            }else{
              # Store data
              if(iens==0){
                dimens=dimnames(data[[2]])
                dimens$mmb=seq(1,nenss[imodel],1)
                data_members=array(data=NA_real_,dim=c(dim(data[[2]]),nenss[imodel]),dimnames = dimens)
                # Open nc to retrieve dimensions to be used to save
                ncid=nc_open(inFname, write=FALSE, readunlim=TRUE, verbose=FALSE,auto_GMT=TRUE, suppress_dimvals=FALSE )
              }
              data_members[,,,(iens+1)]=data[[1]]
              
            } # endif (are all values of data NA?)
            
          } # endif (file of data exists)
          
        } # end ensemble members (iens)
        # Evaluate again existence of file of anomalies in that date in order to save (avoid saving ncs in other dates)
        if(file.exists(inFname) & (sum(is.na(data[[2]]))!=prod(size(data[[2]]))) ){
          # Compute ensemble mean
          ensmean=apply(data_members,c(1,2,3),mean,na.rm=TRUE)
          
          # Construct output name
          ofname=paste0(outDir,varname,'_',plevstr,'_',group,'-',model,'_',yyyymmdd,'.ensmean.daily.nc')
          
          # Write Data
          fprintf('%s%s\n','Writing File: ',ofname)
          #0. Retrieve dimensions
          units=ncid$var$tassa$units
          fillValue=ncid$var$tassa$missval
          #1. Define dimensions
          londim <- ncdim_def("X",ncid$dim$lon$units,as.double(ncid$dim$lon$vals)) 
          latdim <- ncdim_def("Y",ncid$dim$lat$units,as.double(ncid$dim$lat$vals)) 
          timedim <- ncdim_def("L",ncid$dim$time$units,as.double(ncid$dim$time$vals))
          # 2. Define variables
          var_def <- ncvar_def(emvarname,ncid$var$tas$units,list(londim,latdim,timedim),fillValue,longname="tass full ensemble mean",prec="float")
          # Create netCDF file and put arrays
          ncout <- nc_create(ofname,list(var_def),force_v4=TRUE)
          # Put variables
          ncvar_put(ncout,var_def,ensmean)
          # Add global attributess
          ncatt_put(ncout,0,"title",title)
          ncatt_put(ncout,0,"institution",institution)
          ncatt_put(ncout,0,"source",source)
          ncatt_put(ncout,0,"CreationDate",as.character(Sys.Date()))
          ncatt_put(ncout,0,"CreatedBy","Mariano S. Alvarez")
          ncatt_put(ncout,0,"ModifiedBy","Lucia M. Castro")
          
          # close the file, writing data to disk
          nc_close(ncout)
          nc_close(ncid)
        }     # endif anomaly file existence   
        
      } # end days (idy)
      
    } # end months (imn)
    
  } # end years
  


# Hecho esto ahora puedo calcular la media semanal para la semana en cuestion a 1,2,3 y 4 semanas antes 
# arrancando en sabado

# Temperatura a 2 metros del modelo 
for (n in 1:4) {
  
  #inicio = c("2000-02-05","2000-01-31","2000-01-26","2000-01-16")
  #inicio2 = c("0205","0131","0126","0116")
  inicio = c("2001-10-27","2001-10-20","2001-10-13","2001-10-06")
  inicio2 = c("1027","1020","1013","1006")
  w = c("Week 1","Week 2","Week 3","Week 4")
  
  # Tomo los sabados anteriores a la semana en cuestion 02-12
  inFilename = paste0("./ensmean_full/GMAO-GEOS_V2p1/tas_sfc_GMAO-GEOS_V2p1_2000",inicio2[n],".ensmean.daily.nc")
  clim.mod = metR::ReadNetCDF(inFilename, out="array")
  clim.mod=clim.mod[[1]]
  
  
  lead = as.character(as.Date(inicio[n]) + (1:45))
  mod.semana = clim.mod[,,which(lead %in% week2)]
  media.mod = apply(mod.semana, c(1,2), FUN = mean)
  
  # Acomodo para graficar
  dt.mod = reshape2::melt(media.mod, value.name = "z")
  dimnames(dt.mod)[[2]] <- list("x","y","z")
  dt.mod$x = as.numeric(dt.mod$x)
  dt.mod$y= as.numeric(dt.mod$y)
  
  titu = paste0("t2m GMAO-GEOS INI",inicio2[n],"\nMedia semanal 12-18 Feb")
  GraphDiscrete(Data = dt.mod, Titulo = titu, Paleta = "RdBu",Label = "°C",Breaks = seq(0,45,5),Direccion = 1)
  ggsave(filename=paste0("/home/lucia.castro/SubX_processed_Rdata/MODfull",inicio2[n],"sem1218.png"),width = 10, height = 11)
}

df.prueba = reshape2::melt(mod.semana[,,1],value.name="z")
dimnames(df.prueba)[[2]] <- list("x","y","z")
df.prueba$x = as.numeric(df.prueba$x)
df.prueba$y= as.numeric(df.prueba$y)

GraphDiscrete(Data = df.prueba, Titulo = titu, Paleta = "RdBu",Label = "°C",Breaks = seq(0,45,5))

# FULL-------------------------------------------------------------------------------------------------------
for (w in 1:4) {
  
  for (miembro in 1:nenss) {
    inicio = c("2000-02-05","2000-01-31","2000-01-26","2000-01-16")
    inicio2 = c("0205","0131","0126","0116")
    
    # descargo los datos 
    inFilename = paste0("./full/GMAO-GEOS_V2p1/tas_sfc_GMAO-GEOS_V2p1_2000",inicio2[w],".e",miembro,".SouthAmerica.daily.nc")
    mod.full = metR::ReadNetCDF(inFilename, out="array")
    mod.full= mod.full[[2]]
    
    # busco la semana para calcular el promedio semanal
    fechas = as.character(as.Date(inicio[w]) + (1:45))
    full.semana = mod.full[,,fechas %in% week2]
    full.semana.media = apply(full.semana,c(1,2), FUN = mean)
    
    # convierto a data frame para graficar
    df.full = reshape2::melt(full.semana.media,value.name="z")
    dimnames(df.full)[[2]] <- list("x","y","z")
    df.full$x = as.numeric(df.full$x)
    df.full$y= as.numeric(df.full$y)
    
    # grafico
    titu = paste0("FULL T2M INI2000",inicio2[w],"\n media semanal  Feb 12-18" ,"\n Miembro ", miembro )
    GraphDiscrete(Data = df.full, Titulo = titu, Paleta = "YlOrRd",Label = "°C",Breaks = seq(273,310,5), Direccion = 1)
    ggsave(filename=paste0("/home/lucia.castro/SubX_processed_Rdata/MODfull",inicio2[w],"sem1218.e",miembro,".png"),width = 10, height = 11)
    
  } # end loop miembros ensamble

} # end loop week



# ANOM -----------------------------------------------------------------------------------------------------------
for (w in 1:4) {
  
  for (miembro in 1:nenss) {
    inicio = c("2000-02-05","2000-01-31","2000-01-26","2000-01-16")
    inicio2 = c("0205","0131","0126","0116")
    
    # descargo los datos 
    inFilename = paste0("./anom/GMAO-GEOS_V2p1/tas_sfc_GMAO-GEOS_V2p1_2000",inicio2[w],".e",miembro,".anoms.daily.nc")
    mod.anom = metR::ReadNetCDF(inFilename, out="array")
    mod.anom= mod.anom[[1]]
    
    # busco la semana para calcular el promedio semanal
    fechas = as.character(as.Date(inicio[w]) + (1:45))
    anom.semana = mod.anom[,,fechas %in% week2]
    anom.semana.media = apply(anom.semana,c(1,2), FUN = mean)
    
    # convierto a data frame para graficar
    df.anom = reshape2::melt(anom.semana.media,value.name="z")
    dimnames(df.anom)[[2]] <- list("x","y","z")
    df.anom$x = as.numeric(df.anom$x)
    df.anom$y= as.numeric(df.anom$y)
    
    # grafico
    titu = paste0("ANOM T2M INI2000",inicio2[w],"\n media semanal  Feb 12-18" ,"\n Miembro ", miembro )
    GraphDiscrete(Data = df.anom, Titulo = titu,  Paleta = "RdBu",Label = "°C",Breaks = seq(-6,6,1), Direccion = -1)
    ggsave(filename=paste0("/home/lucia.castro/SubX_processed_Rdata/MODanom",inicio2[w],"sem1218.e",miembro,".png"),width = 10, height = 11)
    
  } # end loop miembros ensamble
  
} # end loop week


# ENSAMBLE ---------------------------------------------------------------------------------------------------------

ensamble <- array(NA, dim = c(66,76,4))

for (w in 1:4) {

    inicio = c("2000-02-05","2000-01-31","2000-01-26","2000-01-16")
    inicio2 = c("0205","0131","0126","0116")
    
    # descargo los datos 
    inFilename = paste0("./ensmean2/GMAO-GEOS_V2p1/tas_sfc_GMAO-GEOS_V2p1_2000",inicio2[w],".ensmean.daily.nc")
    mod.anom = metR::ReadNetCDF(inFilename, out="array")
    mod.anom= mod.anom[[1]]
    
    # busco la semana para calcular el promedio semanal
    fechas = as.character(as.Date(inicio[w]) + (1:45))
    anom.semana = mod.anom[,,fechas %in% week2]
    anom.semana.media = apply(anom.semana,c(1,2), FUN = mean)
    
    # convierto a data frame para graficar
    df.anom = reshape2::melt(anom.semana.media,value.name="z")
    dimnames(df.anom)[[2]] <- list("x","y","z")
    df.anom$x = as.numeric(df.anom$x)
    df.anom$y= as.numeric(df.anom$y)
    
    # grafico
    titu = paste0("ANOM T2M INI2000",inicio2[w],"\n media semanal  Feb 12-18" ,"\n Media Ensamble ")
    GraphDiscrete(Data = df.anom, Titulo = titu,  Paleta = "RdBu",Label = "°C",Breaks = seq(-6,6,1), Direccion = -1)
    ggsave(filename=paste0("/home/lucia.castro/SubX_processed_Rdata/MODensam",inicio2[w],"sem1218.png"),width = 10, height = 11)
    
    ensamble[,,w] <- anom.semana.media

  } # end loop week



#-------------------------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------------
# Comparo la media del ensamble de esas fechas con el reanalisis
obs= readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")
semana12_18 = as.character(periodo) %in% week2
obs_semana = obs[,,semana12_18]
obs_semana_media = apply(obs_semana, c(1,2),  FUN = mean)

# Acomodo en data table
df.obs = reshape2::melt(obs_semana_media,value.name="z")
dimnames(df.obs)[[2]] <- list("x","y","z")
df.obs$x = as.numeric(df.obs$x)
df.obs$y= as.numeric(df.obs$y)

# grafico
titu = paste0("ANOM T2M NOAA \n media semanal  Feb 12-18")
GraphDiscrete(Data = df.obs, Titulo = titu,  Paleta = "RdBu",Label = "°C",Breaks = seq(-6,6,1), Direccion = -1)
ggsave(filename=paste0("/home/lucia.castro/SubX_processed_Rdata/OBSsem1218.png"),width = 10, height = 11)


# Calculo metricas como rmse
dif = obs_semana_media - ensamble[,,4]
rmse = sqrt(apply(dif^2,c(1,2), FUN = mean, na.rm = TRUE))
# Acomodo en data table
df.rmse = reshape2::melt(rmse,value.name="z")
dimnames(df.rmse)[[2]] <- list("x","y","z")
df.rmse$x = as.numeric(df.rmse$x)
df.rmse$y= as.numeric(df.rmse$y)

# grafico
titu = paste0("RSME WEEK 4 \n media semanal  Feb 12-18")
GraphDiscrete(Data = df.rmse, Titulo = titu,  Paleta = "YlOrRd",Label = "rmse",Breaks = seq(0,5,1), Direccion = 1)
ggsave(filename=paste0("/home/lucia.castro/SubX_processed_Rdata/RMSEweek4.png"),width = 10, height = 11)
