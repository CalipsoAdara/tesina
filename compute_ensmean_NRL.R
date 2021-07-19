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
dateinits=seq(as.Date("1999/01/03"), as.Date("2015/12/27"), by="7 day")


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
        # Store data
        if(e==1){
          dimens=dimnames(data[[1]])
          dimens$mmb=seq(1,nenss[imodel],1)
          # create array
          data_members=array(data=NA_real_,dim=c(dim(data[[1]]),nenss[imodel]),dimnames = dimens)
          # Open nc to retrieve dimensions to be used to save
          ncid=nc_open(inFname[e], write=FALSE, readunlim=TRUE, verbose=FALSE,auto_GMT=TRUE, suppress_dimvals=FALSE )
        }
        datos=data[[1]]
        data_members[,,1:(nlead-(4-e)),e]=datos[,,(4-e+1):nlead]
      } # endif (are all values of data NA?)
    
    } # endif (file of data exists)
  
  } # end ensemble members (iens)
  # Evaluate again existence of file of anomalies in that date in order to save (avoid saving ncs in other dates)
  if(file.exists(inFname) & sum(is.na(data[[1]]))==prod(size(data[[1]]))){
    # Compute ensemble mean
    ensmean=apply(data_members,c(1,2,3),mean,na.rm=TRUE)
    
    # Construct output name (with the last of the 4 dates)
    ofname=paste0(outDir,varname,'_',plevstr,'_',group,'-',model,'_',yyyymmdd4,'.ensmean.daily.nc')
    
    # Write Data
    fprintf('%s%s\n','Writing File: ',ofname)
    #0. Retrieve dimensions
    units=ncid$var$tas$units
    fillValue=ncid$var$tas$missval
    #1. Define dimensions
    londim <- ncdim_def("X",ncid$dim$X$units,as.double(ncid$dim$X$vals)) 
    latdim <- ncdim_def("Y",ncid$dim$Y$units,as.double(ncid$dim$Y$vals)) 
    timedim <- ncdim_def("L",ncid$dim$L$units,as.double(ncid$dim$L$vals))
    # 2. Define variables
    var_def <- ncvar_def(emvarname,ncid$var$tas$units,list(londim,latdim,timedim),fillValue,longname="tas anomaly ensemble mean",prec="float")
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
  }   # endif anomaly file existence     
} # number of initialization dates (idate)  
  

