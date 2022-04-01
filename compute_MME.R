# SubX data - Compute MME of SubX models
#
# M. Alvarez - 2020
#-----------------------------------------------------------------------------------------------------------------------
rm(list=ls())

setwd("/home/alvarez/")

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
#groups=c('GMAO','RSMAS','ESRL','ECCC','NRL','EMC')                        # Modeling Groups (must be same # elements as models below)
#models=c('GEOS_V2p1','CCSM4','FIMr1p1','GEM','NESM','GEFS')               # Model Name (must be same # of elements as groups above)


dt.verif.GMAO=as.data.table(readRDS("/home/alvarez/SubX_processed_Rdata/toverif_GMAO_ONDEFMA.rds"))
dt.verif.RSMAS=as.data.table(readRDS("/home/alvarez/SubX_processed_Rdata/toverif_RSMAS_ONDEFMA.rds"))
dt.verif.ESRL=as.data.table(readRDS("/home/alvarez/SubX_processed_Rdata/toverif_ESRL_ONDEFMA.rds"))
dt.verif.ECCC=as.data.table(readRDS("/home/alvarez/SubX_processed_Rdata/toverif_ECCC_ONDEFMA.rds"))
dt.verif.NRL=as.data.table(readRDS("/home/alvarez/SubX_processed_Rdata/toverif_NRL_ONDEFMA.rds"))
dt.verif.EMC=as.data.table(readRDS("/home/alvarez/SubX_processed_Rdata/toverif_EMC_ONDEFMA.rds"))


# Empecemos probando con dos
dt.verif.GMAO=dt.verif.GMAO[, c("week","anom"):=NULL]
setnames(dt.verif.GMAO, "rlutaem", "GMAO")

dt.verif.RSMAS=dt.verif.RSMAS[, c("week","anom"):=NULL]
setnames(dt.verif.RSMAS, "rlutaem", "RSMAS")

dt.verif.ESRL=dt.verif.ESRL[, c("week","anom"):=NULL]
setnames(dt.verif.ESRL, "rlutaem", "ESRL")

dt.verif.ECCC=dt.verif.ECCC[, c("week","anom"):=NULL]
setnames(dt.verif.ECCC, "rlutaem", "ECCC")

dt.verif.NRL=dt.verif.NRL[, c("week","anom"):=NULL]
setnames(dt.verif.NRL, "rlutaem", "NRL")

dt.verif.EMC=dt.verif.EMC[, c("week","anom"):=NULL]
setnames(dt.verif.EMC, "rlutaem", "EMC")


dt.mme=merge(dt.verif.GMAO,dt.verif.RSMAS,by=c("lat","lon","targetdate","L","startdate"),all.x=TRUE,all.y=TRUE)
# ok, esto funciona! 
# Agrupo todos los modelos y después trabajo con las startdate para definir en qué semana se usan y targetdate para promediar

dt.mme=merge(dt.mme,dt.verif.ESRL,by=c("lat","lon","targetdate","L","startdate"),all.x=TRUE,all.y=TRUE)
dt.mme=merge(dt.mme,dt.verif.ECCC,by=c("lat","lon","targetdate","L","startdate"),all.x=TRUE,all.y=TRUE)
dt.mme=merge(dt.mme,dt.verif.NRL,by=c("lat","lon","targetdate","L","startdate"),all.x=TRUE,all.y=TRUE)
dt.mme=merge(dt.mme,dt.verif.EMC,by=c("lat","lon","targetdate","L","startdate"),all.x=TRUE,all.y=TRUE)

rm(dt.verif.ECCC,dt.verif.EMC,dt.verif.ESRL,dt.verif.GMAO,dt.verif.NRL,dt.verif.RSMAS)

# Para verificar
# a=dt.mme[(lat==-40 & lon==290 & year(targetdate)==1999),]

# Create date vector of Saturdays in 1999-2015
fridays=seq(as.Date("1999-01-08"),as.Date("2015-12-25"),7)
prevsats=seq(as.Date("1999-01-02"),as.Date("2015-12-19"),7)
rm(sats)

# # Vamos a probar con a
# a=dt.mme[(lat==-40 & lon==290 & year(targetdate)==1999),]
# a$gatherweek=NA_real_
# a$mmestartdate=as.Date("1800-01-01")
# a1=a
# for(i in 1:886){
#   a1$gatherweek[(a1$startdate>=prevsats[i] & a1$startdate<=fridays[i])]=i
#   a1$mmestartdate[(a1$startdate>=prevsats[i] & a1$startdate<=fridays[i])]=fridays[i]
# }
# 
# a1$L.MME=NA_real_
# a1$L.MME=as.numeric(a1$targetdate-a1$mmestartdate)
# 
# # Ahora L startdate y gatherweek no me sivern más. Me debería quedar además sólo con las L.MME >= 1
# a2=a1[(L.MME>0 & L.MME<50),!c("L","startdate","gatherweek")]
# 
# a4=a2[, .(MME=mean(as.matrix(.SD), na.rm = TRUE)), by = .(lon, lat, targetdate, mmestartdate, L.MME)]
# 

dt.mme$gatherweek=NA_real_
dt.mme$mmestartdate=as.Date("1800-01-01")
for(i in 1:886){
  dt.mme$gatherweek[(dt.mme$startdate>=prevsats[i] & dt.mme$startdate<=fridays[i])]=i
  dt.mme$mmestartdate[(dt.mme$startdate>=prevsats[i] & dt.mme$startdate<=fridays[i])]=fridays[i]
}

dt.mme$L.MME=NA_real_
dt.mme$L.MME=as.numeric(dt.mme$targetdate-dt.mme$mmestartdate)

# Ahora L startdate y gatherweek no me sivern más. Me debería quedar además sólo con las L.MME >= 1
dt.mme=dt.mme[(L.MME>0 & L.MME<50),!c("L","startdate","gatherweek")]

dt.mme2=dt.mme[, .(MME=mean(as.matrix(.SD), na.rm = TRUE)), by = .(lon, lat, targetdate, mmestartdate, L.MME)]
rm(dt.mme)

dt.mme2$week=dt.mme2$L.MME
dt.mme2$week[(dt.mme2$L.MME>=1 & dt.mme2$L.MME<=7)]=1
dt.mme2$week[(dt.mme2$L.MME>=8 & dt.mme2$L.MME<=14)]=2
dt.mme2$week[(dt.mme2$L.MME>=15 & dt.mme2$L.MME<=21)]=3
dt.mme2$week[(dt.mme2$L.MME>=22 & dt.mme2$L.MME<=28)]=4
dt.mme2$week[(dt.mme2$L.MME>=29)]=99
# Ahora que ya armé la variable "week" en función de los lead, y la variable "targetdate" usando L y startdate, puedo eliminar la variable L si quisiera

setnames(dt.mme2, "L.MME", "L")
setnames(dt.mme2, "mmestartdate", "startdate")

# Cargo observaciones, debería convertirlas en data table con lat, lon y (target)date y luego merge con los pronósticos
dt.anom = readRDS("olranom_NOAA_9915.rds")

dt.verif=merge(dt.mme2,dt.anom,by=c("lat","lon","targetdate"))
dt.verif$startmonth=month(dt.verif$startdate)
OA = c(1,2,3,4,10,11,12);

dt.verifOA=dt.verif[dt.verif$startmonth %in% OA,]
dt.verifOA$startmonth=NULL #Elimino la columna con el mes de inicio
rm("dt.anom","dt.mme2")

# Todo listo para empezar la verificación octubre-abril. Guardo para limpiar y comenzar la verificación.
saveRDS(dt.verifOA,paste0("/home/alvarez/SubX_processed_Rdata/toverif_MME_ONDEFMA.rds"))