# Analisis de la ola de calor 2013. 11/12 al 31/12
#
#
#


library(ggplot2)
library(metR)

# Cargo mis funciones
source("/home/lucia/Documents/tesis/scripts/funciones.R")
setwd("/home/lucia/Documents/tesis/data/")

targetdate <- readRDS("/home/lucia/Documents/tesis/data/targetdate_ECCC_ONDEFM.rds")
obsweek <- readRDS("/home/lucia/Documents/tesis/data/obsweek_ECCC.rds")
modweek <- readRDS("/home/lucia/Documents/tesis/data/modelweek_ECCC.rds")


# Resta en la fecha que incluye todo el evento 
# medias semanales

resta<-obsweek-modweek
ola<-resta[,,"2013-12-07",]
GraphDiscreteMultiple(Data = ggScoreSemanal(ola),Breaks = seq(-5,5,1),Label = "C",
                      Paleta = "RdBu", Direccion = -1)


ar.model = readRDS("model_ECCC_ONDEFM.rds")
targetdate <- readRDS("/home/lucia/Documents/tesis/data/targetdate_ECCC_ONDEFM.rds")
ar.anom = readRDS("obs2013.rds")

diasola = as.character(seq.Date(as.Date("2013-12-11"),as.Date("2013-12-31"),1))
dimnames(targetdate)$startdate
inicio<-ar.model[,,,"2013-12-07"]

modola<-inicio[,,targetdate[,"2013-12-07"] %in% diasola]
anomola <- ar.anom[,,diasola]

dif <- anomola -modola

# ACC
Lon = 66
Lat = 76
acc <- array(NA, dim = c(Lon,Lat))

for (lon in 1:Lon) {
  for (lat in 1:Lat) {
    
    # Me quedo solo con un punto particular y todas las fechas de pronostico
    observ <- anomola[lon,lat,]
    modelo <- modola[lon,lat,]
    
    coef_corr <- cor(observ,modelo,use="pairwise.complete.obs",method = "pearson")
    
    acc[lon,lat] <- coef_corr
    
  } # End loop lat
  
}  # End loop lon


# ---------------------------------------------------------------------------------------------------------
# G R A F I C O S 

GraphDiscrete(Data = ggDataFrame(acc),Breaks = seq(-1,1,0.25),Label = "ACC",
              Paleta = "RdBu", Direccion = -1, Titulo = "ACC")
require(devtools)
install_version("ggplot2", version = "3.3.3", repos = "http://cran.us.r-project.org")


for (dia in 1:length(diasola)) {
  t <- paste0("T2M ANOM OBS - MOD  \n",diasola[dia])
  g<-GraphDiscrete(Data = ggDataFrame(dif[,,dia]),Breaks = seq(-5,5,1),Label = " C",
                Paleta = "RdBu", Direccion = -1, Titulo = t)
  
  ggsave(filename=paste0("/home/lucia.castro/SubX_processed_Rdata/olacalordiff",,".png")
         ,plot=g,width = 10, height = 11)
  
  
}

df_rmm <- readRDS("mjo/df_rmm.rds")
