# Multi-model mean SubX 1999-2015
#
# by Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------

# Limpiar enviroment 
rm(list=ls())

# Call libraries to be used

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/"

# Seteo el directorio
setwd(savepath)

# Cargo los datos 
models = c("ESRL","ECCC","EMC","GMAO","RSMAS")

ESRL <- readRDS("model_ESRL_ONDEFM.rds")
ECCC <- readRDS("model_ECCC_ONDEFM.rds")
EMC <- readRDS("model_EMC_ONDEFM.rds")
GMAO <- readRDS("model_GMAO_ONDEFM.rds")
RSMAS <- readRDS("model_RSMAS_ONDEFM.rds")
targetdate_ESRL <- readRDS("targetdate_ESRL_ONDEFM.rds")
targetdate_ECCC <- readRDS("targetdate_ECCC_ONDEFM.rds")
targetdate_EMC <- readRDS("targetdate_EMC_ONDEFM.rds")
targetdate_GMAO <- readRDS("targetdate_GMAO_ONDEFM.rds")
targetdate_RSMAS <- readRDS("targetdate_RSMAS_ONDEFM.rds")

# startdate de los modelos de los modelos
startdateESRL = as.Date(dimnames(targetdate_ESRL)$stardate)
startdateECCC = as.Date(dimnames(targetdate_ECCC)$stardate)
startdateEMC = as.Date(dimnames(targetdate_EMC)$stardate)
startdateGMAO = as.Date(dimnames(targetdate_GMAO)$stardate)
startdateRSMAS = as.Date(dimnames(targetdate_RSMAS)$stardate)

# Obtengo el periodo de 1999 a 2015 con solo octubre a marzo
periodo = seq.Date(as.Date("1999-01-01"), as.Date("2015-12-31"), by = 1)
OM = c(1,2,3,10,11,12)
oct_mar <- which(month(periodo) %in% OM) # posiciones donde el mes cae entre Octubre a Marzo
periodo_OM <- periodo[oct_mar]

# Obtengo sabados y sus respectivos viernes siguientes para el periodo
sabado = periodo_OM[weekdays(periodo_OM)=="Saturday"][-length(sabado)] # Borra el ultimo sabado (2015-12-26) que no tiene viernes asocidao
vier_sig = periodo_OM[weekdays(periodo_OM)=="Friday"][-1]  # Borra el primer viernes (1999-01-01) que no tiene sabado asociado


for (i in 1:length(sabado)) {
  
  # Semana a analizar 
  startweek = seq.Date(sabado[i],vier_sig[i], by = 1)
  
  # Que startdate cae en esa semana para cada modelo
  sttd_ESRL = startdateESRL %in% startweek
  sttd_ECCC = startdateECCC %in% startweek
  sttd_EMC = startdateEMC %in% startweek
  sttd_GMAO = startdateGMAO %in% startweek
  sttd_RSMAS = startdateRSMAS %in% startweek
  
  sttd = c(sttd_ESRL, sttd_EMC, sttd_ECCC, sttd_GMAO, sttd_RSMAS)
  
  for (j in sttd) {
    if (sum(j)>1){   # si hay mas de una inicializacion esa semana, agarrar la ultima
      posiciones = which(sttd)
      
    }
    
  } # End loop modelos
  
} # End loop sabados
startweek = seq.Date(sabado[1],vier_sig[1], by = 1)
as.Date(targetdate_ECCC) %in% startweek 
startdate = as.Date(dimnames(targetdate_ESRL)$stardate)

startdate %in% startweek
dim(ESRL)
ESRL[,,,startdate %in% startweek]

for (i in 9:length(periodo)) {    # arranca el 9/02/1999
  
  # Dia de la seamana
  dia = weekday[i]
  
  if(dia == "Saturday"){
    
    # Obtengo la semana anterior al sabado en cuestion
    semana_anterior <-  seq.Date(periodo[i-6],periodo[i-1],by=1)
    
    # 
    startdate = dimnames(targetdate_ESRL)$stardate
    setdiff(startdate,periodo)
  }
  
}
