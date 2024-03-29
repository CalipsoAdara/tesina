# SubX data - Opens ensemble mean to afterwards verify against NOAA's OLR
#
# M. Alvarez - 2020
# Modified by Lucia M Castro
#-----------------------------------------------------------------------------------------------------------------------
rm(list=ls())

setwd("/home/lucia.castro/")

# Call libraries to be used
library("ncdf4")
library("metR")
library('pracma')
library('lubridate')
library('reshape2')
library('data.table')
#--------------------------------------------------------------------------------------
#  Settings
#--------------------------------------------------------------------------------------

inPath='/datos/SubX/hindcast/'
groups=c('ECCC')                        # Modeling Groups (must be same # elements as models below)
models=c('GEM')               # Model Name (must be same # of elements as groups above)

varname=c('tas')
plevstr=c('sfc')

syrs=c(1999)  # year of start (per model)
eyrs=c(2014)  # year of end (per model)
nleads=c(32)  # number of lead (per model)
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

sdat=942 # number of start dates to allocate final array

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
inDir1=paste0(inPath,varname,plevstr,'/daily/ensmean/',group,'-',model,'/') # input filename for ensemble mean anom data

i=0


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
      
      # Define file
      inFname=paste0(inDir1,varname,'_',as.character(plevstr),'_',group,'-',model,'_',yyyymmdd,'.ensmean.daily.nc')
      
      # Evaluate existence of file
      if(file.exists(inFname)){
        
        i=i+1
        # Read data
        data = metR::ReadNetCDF(inFname, out='array')
        if(i==1){
          dimens=dimnames(data[[1]])
          dimens$startdate=seq(1,sdat,1)
          data_all=array(data=NA_real_,dim=c(66,76,nlead,sdat),dimnames = dimens)
          # Save the first date
          sttdate=as.Date(paste0(yyyy,'-',mm,'-',dd))
        }else{
          # Save date
          sttdate=c(sttdate,as.Date(paste0(yyyy,'-',mm,'-',dd)))
        } #endif first start date
      
        # Put data on array
        data_all[,,,i]=data[[1]]
        rm(data)
        } #endif file exists
        
    } # end days (idy)
    
  } # end months (imn)
  
} # end years
      
# Rename start date dimension      
dimnames(data_all)$startdate=as.character(sttdate)
# Rename lead (L) dimension to 1:45 (it was overwritten when interpolating with CDO but time sequence remains unchanged)
dimnames(data_all)$L=seq(1,nlead,1)

dt.model=reshape2::melt(data_all)
rm(data_all)

# Ahora debería agregar una columna que sea "targetdate"
dt.model$startdate=as.Date(dt.model$startdate)
dt.model$targetdate=dt.model$startdate+dt.model$L
setnames(dt.model, "value", "tasaem")
dt.model$week=dt.model$L
dt.model$week[(dt.model$L>=1 & dt.model$L<=7)]=1
dt.model$week[(dt.model$L>=8 & dt.model$L<=14)]=2
dt.model$week[(dt.model$L>=15 & dt.model$L<=21)]=3
dt.model$week[(dt.model$L>=22 & dt.model$L<=28)]=4
dt.model$week[(dt.model$L>=29)]=99
# Ahora que ya armé la variable "week" en función de los lead, y la variable "targetdate" usando L y startdate, puedo eliminar la variable L si quisiera

# Cargo observaciones, debería convertirlas en data table con lat, lon y (target)date y luego merge con los pronósticos
dt.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")

dt.verif=merge(dt.model,dt.anom,by=c("lat","lon","targetdate"))
dt.verif$startmonth=month(dt.verif$startdate)
OA = c(1,2,3,4,10,11,12);

dt.verifOA=dt.verif[dt.verif$startmonth %in% OA,]
dt.verifOA$startmonth=NULL #Elimino la columna con el mes de inicio
rm("dt.anom","dt.model")

# Todo listo para empezar la verificación octubre-abril. Guardo para limpiar y comenzar la verificación.
saveRDS(dt.verifOA,paste0("/home/alvarez/SubX_processed_Rdata/toverif_",group,"_ONDEFMA.rds"))
