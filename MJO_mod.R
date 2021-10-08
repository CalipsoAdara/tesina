# MJO analisis con los modelos 

# ---------------------------------------------------------------------------------
# Limpiar el espacio
rm(list=ls())

# cargar paquetes
library(metR)
library(ncdf4)
library(udunits2)
library(RNetCDF)
library(raster)
library('pracma')


# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata")

groups=c('GMAO','RSMAS','ESRL','ECCC','NRL','EMC')                       
models=c('GEOS_V2p1','CCSM4','FIMr1p1','GEM','NESM','GEFS')   

var_mjo = c("RMM1","RMM2","amplitude","phase")

syrs=c(1999,1999,1999,1999,1999,1999)  # year of start (per model)
eyrs=c(2015,2015,2015,2014,2015,2015)  # year of end (per model)
nleads=c(45,45,32,32,45,35)              # number of lead (per model)
nenss=c(4,3,4,4,1,11)


# Directorio donde guardar
outPath = '/datos/SubX/hindcast/rmm/'

for (mod in 1:length(groups)) {
  for(var in 2:length(var_mjo)) {
    
    grupo = groups[mod]
    model = models[mod]
    v = var_mjo[var] 
    
    # Bajar datos del indice RMM al server
    URL = paste0('https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.'
                 ,grupo,"/.",model,"/.hindcast/.RMM/.",v,"/dods")
    URL = paste0("https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/."
                 ,grupo,"/.",model,"/.hindcast/.RMM/.",v,"/data.nc")
    
    outDir = paste0(outPath,grupo,"-",model,"/",v)
    dir.create(outDir,recursive=TRUE)
    
    
    download.file(URL,paste0(outDir,"/",v,".txt"))
    ncfile <- ncdf4::nc_open(URL)
    URL = 'https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.GMAO/.GEOS_V2p1/.hindcast/.RMM/.amplitude/dods'
    AMP = metR::ReadNetCDF(URL, out = "array")
    ncfile <- ncdf4::nc_open(URL)
    var = ncvar_get(ncfile)
    
    f=seq.Date(as.Date("1999-01-01"),as.Date("2015-12-26"),by=1)
    amp = metR::ReadNetCDF(URL, out='array', subset = list(L = 1:45,M = 1:4,S = as.Date("1999-01-02"))) #, subset = list(L = 1,M = 1,S = "1999-01-15"))
    amp = metR::ReadNetCDF(ncfile, out='array',
                           subset = list(L = 1:45,M = 1:4,S =f[1:500])) #, subset = list(L = 1,M = 1,S = "1999-01-15"))
    hur<-var.get.nc(URL,"RMM1", start=c(0,0,0),count=c(45,4,6000))
    
    
     amp[[1]]
    library(raster)
    library(RNetCDF)
    r = raster(paste0(outDir,"/",v,".txt")) 
    
  }
  
  grupo = groups[mod]
  model = models[mod]
  v = var_mjo[var] 

    
  # Bajar datos del indice RMM al server
  URL = paste0("https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/."
               ,grupo,"/.",model,"/.hindcast/.RMM/.RMM1/data.nc")
  download.file(URL, paste0(outPath,grupo,"-",model,"/",var))
  
  
  
  # Bajar datos del IRI del indice RMM
  URL_rmm1 = paste0("https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/."
                   ,groups[mod],"/.",models[mod],"/.hindcast/.RMM/.RMM1/dods")
  URL_fase = "https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.NRL/.NESM/.hindcast/.RMM/.RMM1/data.nc"
  URL_rmm1 = "http://iridl.ldeo.columbia.edu/SOURCES/.BoM/.MJO/.RMM/.RMM1/T/%281%20Jan%201999%29%2831%20Dec%202015%29RANGEEDGES/dods"
  URL_rmm2 = "http://iridl.ldeo.columbia.edu/SOURCES/.BoM/.MJO/.RMM/.RMM2/T/%281%20Jan%201999%29%2831%20Dec%202015%29RANGEEDGES/dods"
  
  # Leer los datos 
  amp = metR::ReadNetCDF(URL_amp, out='array')
  ncid_max=nc_open(URL_rmm1, write=FALSE, readunlim=TRUE, verbose=FALSE,auto_GMT=TRUE, suppress_dimvals=FALSE )
  poke=ncvar_get(ncid_max)
  ncid_max=nc_open(URL_rmm1)
  
  poke = ncvar_get(ncid_max)
  fase = metR::ReadNetCDF(URL_fase, out='array')
  rmm1 = metR::ReadNetCDF(URL_rmm1, out='array')
  rmm2 = metR::ReadNetCDF(URL_rmm2, out='array')
  
  var.get.nc(nc,"RMM1")
  url = "https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.NRL/.NESM/.hindcast/.RMM/.RMM1/dods"
  nc=nc_open(url)
  
  attributes(ncid_max)$names
  p=ncvar_get(ncid_max, attributes(ncid_max$var)$names)
  
  
  amp = amp[[1]]
  fase = fase[[1]]
  rmm1 = rmm1[[1]]
  rmm2 = rmm2[[1]]
  
  # Acomodar en un solo data frame 
  MJO <- data.frame("DATE" = fechas,
                    "AMP" = amp,
                    "FASE" = fase,
                    "RMM1" = rmm1,
                    "RMM2" = rmm2, row.names = seq(1,length(fechas)))
}



# suponiendo que ya tengo los datos 
array = array(1:180,dim=c(45,4,6))

# Hacer una media del ensamble 
enmean = apply(array, c(1,3), FUN = mean)

# Acomodar las observaciones ?
# si tengo el evento 1 
fecha = df_rmm[Evento==1]$DATE

periodo = seq.Date(as.Date("1999-01-01"),as.Date("2015-12-31"),by=1)

for (mod in 1:length(models)) {
  for(var in 2:length(var_mjo)) {
   for (fecha in 1:length(periodo)) {
      
      # Defino
      grupo = groups[mod]
      model = models[mod]
      l = nleads[mod]
      en = nenss[mod]
      v = var_mjo[var]
      f = periodo[fecha]
      yyyymmdd = format(f,"%Y%m%d")
      
      # Bajar datos del indice RMM al server
      URL = paste0('https://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.'
                   ,grupo,"/.",model,"/.hindcast/.RMM/.",v,"/dods")
      
      # Crear carpeta
      outDir = paste0(outPath,grupo,"-",model,"/",v)
      dir.create(outDir,recursive=TRUE)
      
      # Construct output name
      ofname=paste0(outDir,"/",yyyymmdd,'.nc')
      
      data = metR::ReadNetCDF(URL, out='array',subset = list(L = 1:l,M = 1:en,S = f))
      data = data[[1]]
    
      
      # Si no esta vacio el array
      if(sum(is.na(data[[1]]))==prod(size(data[[1]]))) {
        
      } else {
        # Write Data
        fprintf('%s%s\n','Writing File: ',ofname)
        #1. Define dimensions
        londim <- ncdim_def("L","none",1:l, longname = "Lead") 
        latdim <- ncdim_def("M","none",1:en, longname = "Ensamble Members")
        # 2. Define variables
        var_def <- ncvar_def(v,"none",dim = list(londim,latdim),missval = 1e+15,
                             longname=v,prec="float")
        # Create netCDF file and put arrays
        ncout <- nc_create(ofname,list(var_def),force_v4=TRUE)
        # Put variables
        ncvar_put(ncout,var_def,data)
        # Add global attributess
        ncatt_put(ncout,0,"CreationDate",as.character(Sys.Date()))
        ncatt_put(ncout,0,"CreatedBy","Lucia M. Castro")
        
        # close the file, writing data to disk
        nc_close(ncout)
          
        }
        
    } 
    
    
  }
    
}
  


dat = metR::ReadNetCDF('/datos/SubX/hindcast/rmm/GMAO-GEOS_V2p1/RMM1/19990101.nc', out='array')
dat[[1]]
