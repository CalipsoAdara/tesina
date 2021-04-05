# SubX data - Computing ensemble mean
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

#--------------------------------------------------------------------------------------
#  Settings
#--------------------------------------------------------------------------------------

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
        
        # Create Output directory if needed
        outDir=paste0(inPath,varname,plevstr,'/daily/ensmean/',group,'-',model,'/');
        dir.create(outDir,recursive=TRUE)
        
        
        # Loop over all ensemble members
        for(iens in 0:(nenss[imodel]-1)){
          
          # Set the ensemble member string for output file
          ee=as.character(as.integer(iens+1))
          
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
              if(iens==0){
                dimens=dimnames(data[[1]])
                dimens$mmb=seq(1,nenss[imodel],1)
                data_members=array(data=NA_real_,dim=c(dim(data[[1]]),nenss[imodel]),dimnames = dimens)
                # Open nc to retrieve dimensions to be used to save
                ncid=nc_open(inFname, write=FALSE, readunlim=TRUE, verbose=FALSE,auto_GMT=TRUE, suppress_dimvals=FALSE )
              }
              data_members[,,,(iens+1)]=data[[1]]
              
            } # endif (are all values of data NA?)
            
          } # endif (file of data exists)
          
        } # end ensemble members (iens)
        
        # Evaluate again existence of file of anomalies in that date in order to save (avoid saving ncs in other dates)
        if(file.exists(inFname)){
          # Compute ensemble mean
          ensmean=apply(data_members,c(1,2,3),mean,na.rm=TRUE)
          
          # Construct output name
          ofname=paste0(outDir,varname,'_',plevstr,'_',group,'-',model,'_',yyyymmdd,'.ensmean.daily.nc')
          
          # Write Data
          fprintf('%s%s\n','Writing File: ',ofname)
          #0. Retrieve dimensions
          units=ncid$var$rlut$units
          fillValue=ncid$var$rlut$missval
          #1. Define dimensions
          londim <- ncdim_def("X",ncid$dim$X$units,as.double(ncid$dim$X$vals)) 
          latdim <- ncdim_def("Y",ncid$dim$Y$units,as.double(ncid$dim$Y$vals)) 
          timedim <- ncdim_def("L",ncid$dim$L$units,as.double(ncid$dim$L$vals))
          # 2. Define variables
          var_def <- ncvar_def(emvarname,ncid$var$tassa$units,list(londim,latdim,timedim),fillValue,longname="tas anomaly ensemble mean",prec="float")
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
  
} # end imodel
