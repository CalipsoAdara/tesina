# MJO ANALISIS. BAJAR Y ACOMODAR DATOS DEL INDICE RMM

# ---------------------------------------------------------------------------------
# Limpiar el espacio
rm(list=ls())

# cargar paquetes
library(metR)
library(ncdf4)
library(data.table)
library(ggplot2)
library(dplyr)
library(groupdata2)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")


# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata")

#MJO <- readRDS("./MJO.R") # CARGAR ESTO NO CORRER TODO

# Bajar datos del IRI del indice RMM
URL_amp = "http://iridl.ldeo.columbia.edu/SOURCES/.BoM/.MJO/.RMM/.amplitude/T/%281%20Jan%201999%29%2831%20Dec%202015%29RANGEEDGES/dods"
URL_fase = "http://iridl.ldeo.columbia.edu/SOURCES/.BoM/.MJO/.RMM/.phase/T/%281%20Jan%201999%29%2831%20Dec%202015%29RANGEEDGES/dods"
URL_rmm1 = "http://iridl.ldeo.columbia.edu/SOURCES/.BoM/.MJO/.RMM/.RMM1/T/%281%20Jan%201999%29%2831%20Dec%202015%29RANGEEDGES/dods"
URL_rmm2 = "http://iridl.ldeo.columbia.edu/SOURCES/.BoM/.MJO/.RMM/.RMM2/T/%281%20Jan%201999%29%2831%20Dec%202015%29RANGEEDGES/dods"

amp = metR::ReadNetCDF(URL_amp, out='array')
fase = metR::ReadNetCDF(URL_fase, out='array')
rmm1 = metR::ReadNetCDF(URL_rmm1, out='array')
rmm2 = metR::ReadNetCDF(URL_rmm2, out='array')

amp = amp[[1]]
fase = fase[[1]]
rmm1 = rmm1[[1]]
rmm2 = rmm2[[1]]

fechas = seq.Date(as.Date("1999-01-01"),as.Date("2015-12-31"),by=1)

# Acomodar en un solo data frame 
MJO <- data.frame("DATE" = fechas,
                  "AMP" = amp,
                  "FASE" = fase,
                  "RMM1" = rmm1,
                  "RMM2" = rmm2, row.names = seq(1,length(fechas)))

#saveRDS(MJO, "./MJO.R")

# ---------------------------------------------- EVENTOS ACTIVOS -------------------------------------------------
# Considero un evento activo si cumple la cantidad de dias minimos con amplitud mayor a 1 y desplazandose 
# hacia el este dos tercios de dichos dias 
median(amp) #1.195 es mayor a uno
min_dia = 10 # Cantidad de dias minima para que el evento se considere activo
dias_este_min = round(min_dia*2/3)
amp_min = 1.19

## Calculo el angulo entre la recta formada por el punto y el origen, y el eje x 
ang = atan2(y = rmm2, x = rmm1)

## Diff hace la resta entre cada elemento (tomando el segundo - el primero), donde es positivo es que 
## el segundo numero es mayor, el angulo crece, el sistema se desplaza al este
resta = as.numeric(diff(ang))

data = data.frame("DATE" = fechas,
                  "AMP" = amp,
                  "ANGULO" = ang,row.names = seq(1,length(fechas)))
setDT(data)

# Cuento los dias consecutivos
data[, Consec_Days_AMP := ifelse(AMP > amp_min, 1:.N, 0L), by = rleid(AMP > amp_min)]
data[, Cambio_Angulo := c(0,diff(ANGULO))]
data[, Consec_Days_ESTE := ifelse(Cambio_Angulo > 0, 1:.N, 0L), by = rleid(Cambio_Angulo > 0)]


# Ahora tomo los dias que cumplan con:
# - amplitud mayor al valor minimo la cant de dias minima (Ej 7) y 
# - desplazamiento este un tercio de la cant de dias minima (Ej 5)
#activo = data[Consec_Days_AMP >= min_dia & Consec_Days_ESTE >= dias_este_min]
activo = data[Consec_Days_AMP >= min_dia ]

# Creo una nueva columna separando los eventos segun los dias minimos consecutivos
# Hace nuevo evento cada vez que hay un valor == 7
activo = group(activo, n=as.list(which(activo$Consec_Days_AMP == min_dia)),
          method = "l_starts", starts_col = "index")
setnames(activo, ".groups","Evento")
setDT(activo)

# Ahora busco si hubo desplazamiento hacia el este (no tiene porque ser consecutivos)
este = setDT(activo[ ,sum(Cambio_Angulo>0 ), by = Evento])
d = este[V1 >= dias_este_min]$Evento
activo=activo[activo$Evento %in% d]


# Busco las fechas de inicio "0" y los 7 dias antes para iniciar el evento; y las fechas finales
fecha0 = activo[, .SD[1], by=Evento]$DATE
fecha_ini = fecha0 - 7
fecha_fin = activo[, .SD[.N], by=Evento]$DATE
cant_eventos = length(fecha0)

# Busco la amplitud media de cada evento
amp_media = activo[,list(amp_med=mean(AMP)),by=.(Evento)]

# Acomodo informacion en un data frame
df_eventos <- data.frame("Inicio" = fecha_ini, 
                         "Final" = fecha_fin,
                         "Duracion" = fecha_fin - fecha_ini,
                         "DuracionAct" = fecha_fin - fecha0,
                         "AmpMedia" = as.numeric(amp_media$amp_med))
setDT(df_eventos)

# Acomodo informacion en otro data frame con las comp rmm
list_rmm <- list()
setDT(MJO)
for (d in 1:length(fecha_ini)) {
  # Busco las fechas comprendidas entre la fecha inicial y final
  dfevento = MJO[DATE >= fecha_ini[d] & DATE <= fecha_fin[d]]
  # Guardo
  list_rmm[[d]] <- (dfevento)
}

# Convierto a data frame
df_rmm = bind_rows(list_rmm, .id = "Evento")

evento = df_rmm[Evento == 16]

GraphRMM(evento)
dia = format(evento$DATE[1], "%d_%m_%y")
ggsave(filename = paste0("MJO_diagrama_",dia,".png"), height = 7, width = 7) 




























# # Elimino los dias que "solitarios", es decir, una sola fecha suelta que cumple la condicion
# activo = activo[with(activo, c(DiaEvento[-1]!= DiaEvento[-nrow(activo)], TRUE)),]
# 
# 
# cant_eventos = sum(activo$DiaEvento == 0)
# 
# # Busco las fechas de inicio "0" y los 7 dias antes para iniciar el evento
# fecha0 = activo[DiaEvento == 0,(DATE)]
# fecha_ini = fecha0 - 7
# # Busco las fechas de final de evento como la posicion anterior a la de inicio
# pos_fin = (which(fecha_activo %in% fecha0)-1) [-1] #quito el primer elemento porque busco el final
# fecha_fin = c(fecha_activo[pos_fin],last(fecha_activo))
# 
# # Busco la amplitud media de cada evento
# amp_med <- list()
# for (d in 1:length(fecha_ini)) {
#   setDT(data)
#   # Busco las fechas comprendidas entre la fecha inicial y final
#   periodo_evento = data[DATE >= fecha_ini[d] & DATE <= fecha_fin[d]]
#   # Promedio la amplitud
#   amp_med[[d]] <- periodo_evento[,(mean(AMP))]
# }
# 
# # Acomodo informacion en un data frame
# df_eventos <- data.frame("Inicio" = fecha_ini, 
#                          "Final" = fecha_fin,
#                          "Duracion" = fecha_fin - fecha_ini,
#                          "DuracionAct" = fecha_fin - fecha0,
#                          "AmpMedia" = as.numeric(amp_med))
# setDT(df_eventos)
# 
# # Elimino eventos con duracion de 1 dia
# df_eventos <- df_eventos[DuracionAct != 1, ]
# 
# 
# # Acomodo informacion en otro data frame con las comp rmm
# list_rmm <- list()
# setDT(MJO)
# for (d in 1:length(fecha_ini)) {
#   # Busco las fechas comprendidas entre la fecha inicial y final
#   dfevento = MJO[DATE >= fecha_ini[d] & DATE <= fecha_fin[d]]
#   # Promedio la amplitud
#   list_rmm[[d]] <- (dfevento)
# }
# 
# # Convierto a data frame
# df_rmm = bind_rows(list_rmm, .id = "Evento")
# 
# GraphRMM(df_rmm[Evento == 1])
# 
# 
# ggsave(filename = "MJO_phase_diagram.png", height = 7, width = 7)  
# 
# 
# 
# # determine if the region should be considered an "interruption"
# activo[, interrupt := ifelse(dif != 1, TRUE, FALSE)]
# activo[, group := rleid(interrupt)]
# 
# activo[, lapply(.SD, function(x) sum(x != "")), by = DATE]
# # calculate run lengths for the observation values
# o <- rle(activo$dif)
# 
# # assign a new column assigning each row(timepoint/observation) its run length
# activo[, length := unlist(lapply(o$lengths, function(x) rep(x, each=x)))]
# 
# 
# eventos <- list()
# 
# for (salto in 1:length(diff(fecha_activo))) {
#   if(diff(fecha_activo)[salto]!=1) {
#     eventos[[salto]] <- activo[]
#   }
#   
# }
# 
# EventoActivo(Vector = as.numeric(amp),
#              MinValue = 1,
#              MinDay = min_dia)
# 
# 
# library(data.table)
# set.seed(120)
# 
# # Toy data set
# dat <- data.table(time=seq(1,1000), obs=sample(c(0,0.01, 0.1, 1), size=1000, replace=TRUE, prob=c(0.3, 0.3, 0.3, 0.1)))
# 
# # calculate run lengths for the observation values
# o <- rle(dat$obs)
# 
# # assign a new column assigning each row(timepoint/observation) its run length
# dat[, length := unlist(lapply(o$lengths, function(x) rep(x, each=x)))]
# 
# # determine if the region should be considered an ""interruption""
# dat[, interrupt := ifelse(obs==0 & length>= 4, TRUE, FALSE)]
# 
# # assign values to each alternating interruption/grouped region
# dat[, group := rleid(interrupt)]
# 
# # Remove sections with >= 4 obsevations of 0
# dat2 <- dat[interrupt==FALSE]
# 
# # Re-number groups starting at 1
# dat2[,group := as.numeric(as.factor(group))]