# SubX data - Computing anomalies for each ensemble member respect to climatology
# (differs form KPegion who computes ensemble mean and anoms of em respect to climatology)
#
# M. Alvarez - 2020
# modificado por Lucia Castro
#-----------------------------------------------------------------------------------------------------------------------
rm(list=ls())

setwd("/home/lucia.castro/")

# Call libraries to be used
library("ncdf4")
library("metR")
library('pracma')

#--------------------------------------------------------------------------------------
#  Settings
#--------------------------------------------------------------------------------------

inPath='/datos/SubX/hindcast/'
groups=c('GMAO','RSMAS','ESRL','ECCC','NRL','EMC')                        # Modeling Groups (must be same # elements as models below)
models=c('GEOS_V2p1','CCSM4','FIMr1p1','GEM','NESM','GEFS')               # Model Name (must be same # of elements as groups above)

varnames=c('tas')
plevstrs=c('sfc')
dominio=c('.SouthAmerica')

syrs=c(1999,1999,1999,1999,1999,1999)  # year of start (per model)
eyrs=c(2015,2015,2015,2014,2015,2015)  # year of end (per model)
nleads=c(45,45,32,32,45,35)              # number of lead (per model)
nenss=c(4,3,4,4,1,11)
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
longtitle='SubX Anomalies'
title=longtitle
unitst='days since 1960-01-01'
comments='SubX project http://cola.gmu.edu/~kpegion/subx/'
source='SubX IRI'
institution='IRI'

varname=varnames[1]
plevstr=plevstrs[1]
anomvarname='tassa'
#---------------------------------------------------------------------------------------
#  Main Program  
#---------------------------------------------------------------------------------------

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
  
  inDir1=paste0(inPath,varname,plevstr,'/daily/full/',group,'-',model,'/') # input filename for raw data
  inDir2=paste0(inPath,varname,plevstr,'/daily/clim/',group,'-',model,'/') # input filename for climatology
  
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
        outDir=paste0(inPath,varname,plevstr,'/daily/anom/',group,'-',model,'/');
        dir.create(outDir,recursive=TRUE)
        
        # Loop over all ensemble members
        for(iens in 0:(nenss[imodel]-1)){
          
          # Set the ensemble member string for output file
          ee=as.character(as.integer(iens+1))
          
          # Construct output name
          ofname=paste0(outDir,varname,'_',plevstr,'_',group,'-',model,'_',yyyymmdd,'.e',ee,'.anoms.daily.nc')
          
          # Define files
          inFname=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd,'.e',ee, dominio,'.daily.nc')
          inFnameClim=paste0(inDir2,varname,'_',as.character(plevstr),'_',group,'-',model,'_1960',mmdd, dominio,'.daily.clim.nc')
          
          # Evaluate existence of file
          if(file.exists(inFname)){
            
            # Read data
            data = metR::ReadNetCDF(inFname, out='array')
            
            # Check if all values of data are NA
            if(sum(is.na(data[[1]]))==prod(size(data[[1]]))){
              # Means all values are NA 
            }else{
              # Compute anomalies
              climdata = metR::ReadNetCDF(inFnameClim, out='array')
              anom=data[[2]]-climdata[[1]]
              
              # Write Data
              fprintf('%s%s\n','Writing File: ',ofname)
              #0. Open nc to retrieve dimensions
              ncid=nc_open(inFname, write=FALSE, readunlim=TRUE, verbose=FALSE,auto_GMT=TRUE, suppress_dimvals=FALSE )
              units=ncid$var$tas$units
              fillValue=ncid$var$tas$missval
              #1. Define dimensions
              londim <- ncdim_def("X",ncid$dim$lon$units,as.double(ncid$dim$lon$vals)) 
              latdim <- ncdim_def("Y",ncid$dim$lat$units,as.double(ncid$dim$lat$vals)) 
              timedim <- ncdim_def("L",ncid$dim$time$units,as.double(ncid$dim$time$vals))
              # 2. Define variables
              var_def <- ncvar_def(anomvarname,units,list(londim,latdim,timedim),fillValue,longname="tassfc anomaly",prec="float")
              # Create netCDF file and put arrays
              ncout <- nc_create(ofname,list(var_def),force_v4=TRUE)
              # Put variables
              ncvar_put(ncout,var_def,anom)
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
            } # endif (are all values of data NA?)
            
          } # endif (file of data exists)
          
        } # end ensemble members (iens)
        
      } # end days (idy)
      
    } # end months (imn)
    
  } # end years
  
} # end imodel
