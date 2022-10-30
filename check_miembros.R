# SubX data - Computing ensemble mean for the special case of NRL (4 members initialized 1 per day)
#
# M. Alvarez - 2020
# Modificado por Lucia M Castro
#-----------------------------------------------------------------------------------------------------------------------
rm(list=ls())

setwd("/home/lucia.castro/")

# Call libraries to be used
library("ncdf4")
library("metR")
library('pracma')
library('lubridate')

#--------------------------------------------------------------------------------------
#  Settings
#--------------------------------------------------------------------------------------

inPath='/datos/SubX/hindcast/'
groups=c('NRL')                        # Modeling Groups (must be same # elements as models below)
models=c('NESM')               # Model Name (must be same # of elements as groups above)

varnames=c('tas')
plevstrs=c('sfc')

syrs=c(1999)  # year of start (per model)
eyrs=c(2015)  # year of end (per model)
nleads=c(45)  # number of lead (per model)
nenss=c(4)    # it really has 1, but will be used as 4 members (1 each day)
nmodels=numel(models)

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
emvarname='tasaem'

#---------------------------------------------------------------------------------------
#  Main Program  
#---------------------------------------------------------------------------------------
imodel=1
model=models[imodel]
group=groups[imodel]
syr=syrs[imodel]
eyr=eyrs[imodel]
nyr=eyr-syr+1
nlead=nleads[imodel]
nens=nenss[imodel]
time=seq(0.5,nlead,1)

# Define input directory
inDir1=paste0(inPath,varname,plevstr,'/daily/anom/',group,'-',model,'/') # input filename for anomalies data

# Create Output directory if needed
outDir=paste0(inPath,varname,plevstr,'/daily/ensmean/',group,'-',model,'/');
dir.create(outDir,recursive=TRUE)

# Create vector of initialization dates per week (only the first of the 4)
dateinits <- sttdate




for(idate in 1:length(dateinits)){
  # Create date string
  
  im=month(dateinits[idate])
  mm=as.character(im)
  if(im<10){mm=paste0('0',mm)}
  idy=day(dateinits[idate])
  dd=as.character(idy)
  if(idy<10){dd=paste0('0',dd)}
  yy=year(dateinits[idate])
  yyyymmdd1=paste0(as.character(yy),mm,dd)
  
  im=month(dateinits[idate]+1)
  mm=as.character(im)
  if(im<10){mm=paste0('0',mm)}
  idy=day(dateinits[idate]+1)
  dd=as.character(idy)
  if(idy<10){dd=paste0('0',dd)}
  yy=year(dateinits[idate]+1)
  yyyymmdd2=paste0(as.character(yy),mm,dd)
  
  im=month(dateinits[idate]+2)
  mm=as.character(im)
  if(im<10){mm=paste0('0',mm)}
  idy=day(dateinits[idate]+2)
  dd=as.character(idy)
  if(idy<10){dd=paste0('0',dd)}
  yy=year(dateinits[idate]+2)
  yyyymmdd3=paste0(as.character(yy),mm,dd)
  
  im=month(dateinits[idate]+3)
  mm=as.character(im)
  if(im<10){mm=paste0('0',mm)}
  idy=day(dateinits[idate]+3)
  dd=as.character(idy)
  if(idy<10){dd=paste0('0',dd)}
  yy=year(dateinits[idate]+3)
  yyyymmdd4=paste0(as.character(yy),mm,dd)
  
  # Loop over all "ensemble members"
  
  # Define files
  inFname1=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd1,'.e1.anoms.daily.nc')
  inFname2=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd2,'.e1.anoms.daily.nc')
  inFname3=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd3,'.e1.anoms.daily.nc')
  inFname4=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd4,'.e1.anoms.daily.nc')
  
  inFname=c(inFname1,inFname2,inFname3,inFname4)
  yyyymmdd = c(dateinits[idate],dateinits[idate]+1,dateinits[idate]+2,dateinits[idate]+3)
  # Loop over fake ensemble members
  for(e in 1:4){ 
    # Evaluate existence of file
    if(file.exists(inFname[e])){
      
      # Read data
      data = metR::ReadNetCDF(inFname[e], out='array')
      
      # Check if all values of data are NA
      if(sum(is.na(data[[1]]))==prod(size(data[[1]]))){
        # Means all values are NA 
      }else{
        a[idate,e]<-as.character(yyyymmdd[e])
        
      
        # Store data

      } # endif (are all values of data NA?)
      
    } # endif (file of data exists)
    
  } # end ensemble members (iens)

} # number of initialization dates (idate)  

saveRDS(a,file="SubX_processed_Rdata/miembros/ar_miembrosNRL.rds")


#---------------------------------------------------------------------------------------
#  Main Program  
#---------------------------------------------------------------------------------------

inPath='/datos/SubX/hindcast/'
groups=c('GMAO','RSMAS','ESRL','ECCC','EMC')                        # Modeling Groups (must be same # elements as models below)
models=c('GEOS_V2p1','CCSM4','FIMr1p1','GEM','GEFS')               # Model Name (must be same # of elements as groups above)
# Elimino el NRL-NESM porque tiene 1 miembro por día, la ensamble mean debería ser laggeada, debería calcularla de otra forma

varnames=c('tas')
plevstrs=c('sfc')

syrs=c(1999,1999,1999,1999,1999)  # year of start (per model)
eyrs=c(2015,2015,2015,2014,2015)  # year of end (per model)
nleads=c(45,45,32,32,35)              # number of lead (per model)
nenss=c(4,3,4,4,11)
nmodels=numel(models)

#nvars=numel(varnames)
#nmodels=numel(models)      

# VEO DESPUÉS SI ESTO ES NECESARIO (DÍAS POR MES, MESES POR AÑO)
dpml=c(31,29,31,30,31,30,31,31,30,31,30,31)  # days per month leap
dpmnl=c(31,28,31,30,31,30,31,31,30,31,30,31) # days per month non-leap 
nmpyr=12

imn1=1
imn2=12

# seteo de variables del Array a completar con las fechas si hay o no miembros
li.ar = list()
periodo = as.character(seq.Date(as.Date("1999-01-01"),as.Date("2015-12-31"),1)) #ECCC NO TIENE 2015 nunca
for (m in 1:nmodels) {
  
  ar = array(NA, dim = c(length(periodo),nenss[m]),dimnames = list(periodo,1:nenss[m]))
  li.ar[[m]] <- ar
  
}



varname=varnames[1]
plevstr=plevstrs[1]
emvarname='tasem'

for(imodel in 1:nmodels){
  # Define the model and group and consequent start/end year, leads and ensemble numbers
  model=models[imodel]
  group=groups[imodel]
  syr=syrs[imodel]
  eyr=eyrs[imodel]
  nyr=eyr-syr+1
  nlead=nleads[imodel]
  nens=nenss[imodel]
  time=seq(0.5,nlead,1)
  
  # Define input directory
  inDir1=paste0(inPath,varname,plevstr,'/daily/anom/',group,'-',model,'/') # input filename for anomalies data
  
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
        fecha = as.character(as.Date(paste0(yyyy,"-",mm,"-",dd)))
        
        # cual es la posicion a la que la fecha analizada cae en el periodo?
        pos = which(periodo ==fecha )
        
        # Loop over all ensemble members
        for(iens in 1:(nenss[imodel])){
          
          # Set the ensemble member string for output file
          ee=as.character(as.integer(iens))
          
          # Define files
          inFname=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd,'.e',ee,'.anoms.daily.nc')
          
          # Evaluate existence of file
          if(file.exists(inFname)){
            
            # Read data
            data = metR::ReadNetCDF(inFname, out='array')
            
            # Check if all values of data are NA
            if(sum(is.na(data[[1]]))==prod(size(data[[1]]))){
              # Means all values are NA 
            }else{
              # Store data
              # array a usar
              fe.ar = li.ar[[imodel]][pos,iens] <- fecha
              
              
            } # endif (are all values of data NA?)
            
          } # endif (file of data exists)
          
        } # end ensemble members (iens)
        
        
      } # end days (idy)
      
    } # end months (imn)
    
  } # end years
  
} # end imodel

saveRDS(li.ar,file="SubX_processed_Rdata/miembros/ar_miembros.rds")


#------------------------------------------------------------------------------------------------------------
# ENCONTRAR LAS FECHAS CORRESPONDIENTES DEL NRL-NESM

dateinits=seq.Date(as.Date("1999-01-02"),as.Date("2014-12-31"),1)


inPath='/datos/SubX/hindcast/'
group=c('NRL')                        # Modeling Groups (must be same # elements as models below)
model=c('NESM')               # Model Name (must be same # of elements as groups above)

varname=c('tas')
plevstr=c('sfc')
# Define input directory
inDir1=paste0(inPath,varname,plevstr,'/daily/anom/',group,'-',model,'/') # input filename for anomalies data
sttdate = list()

for (idate in 1:length(dateinits)) {
  
  print(paste("Check de la fecha:",dateinits[idate]))
  
  im=month(dateinits[idate])
  mm=as.character(im)
  if(im<10){mm=paste0('0',mm)}
  idy=day(dateinits[idate])
  dd=as.character(idy)
  if(idy<10){dd=paste0('0',dd)}
  yy=year(dateinits[idate])
  yyyymmdd1=paste0(as.character(yy),mm,dd)
  
  im=month(dateinits[idate]+1)
  mm=as.character(im)
  if(im<10){mm=paste0('0',mm)}
  idy=day(dateinits[idate]+1)
  dd=as.character(idy)
  if(idy<10){dd=paste0('0',dd)}
  yy=year(dateinits[idate]+1)
  yyyymmdd2=paste0(as.character(yy),mm,dd)
  
  im=month(dateinits[idate]+2)
  mm=as.character(im)
  if(im<10){mm=paste0('0',mm)}
  idy=day(dateinits[idate]+2)
  dd=as.character(idy)
  if(idy<10){dd=paste0('0',dd)}
  yy=year(dateinits[idate]+2)
  yyyymmdd3=paste0(as.character(yy),mm,dd)
  
  im=month(dateinits[idate]+3)
  mm=as.character(im)
  if(im<10){mm=paste0('0',mm)}
  idy=day(dateinits[idate]+3)
  dd=as.character(idy)
  if(idy<10){dd=paste0('0',dd)}
  yy=year(dateinits[idate]+3)
  yyyymmdd4=paste0(as.character(yy),mm,dd)
  
  # Loop over all "ensemble members"
  
  # Define files
  inFname1=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd1,'.e1.anoms.daily.nc')
  inFname2=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd2,'.e1.anoms.daily.nc')
  inFname3=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd3,'.e1.anoms.daily.nc')
  inFname4=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd4,'.e1.anoms.daily.nc')
  
  inFname=c(inFname1,inFname2,inFname3,inFname4)
  bool = c()
  for(e in 1:4) {
    
    # Read data
    data = metR::ReadNetCDF(inFname[e], out='array')
    bool[e] <- sum(is.na(data[[1]]))!=prod(size(data[[1]]))
    
  }
  if (sum(bool)==4) {
    # Guarda la primera fecha
    sttdate[[idate]] <-as.Date(dateinits[idate])
    
  }
  
}

print(sttdate)
sttdate = Reduce(c, sttdate) # Convierte a vector manteniendo Dates

saveRDS(sttdate,"./SubX_processed_Rdata/miembros/NRL_dates")
