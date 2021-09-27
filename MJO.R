# MJO ANALISIS. BAJAR Y ACOMODAR DATOS DEL INDICE RMM

# ---------------------------------------------------------------------------------
# Limpiar el espacio
rm(list=ls())

# cargar paquetes
library(metR)
library(ncdf4)
library(data.table)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")


# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata")


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

# Buscar eventos activos 
median(amp) #1.19 es mayor a uno
min_dia = 7 # Cantidad de dias minima para que el evento se considere activo

## Calculo el angulo entre la recta formada por el punto y el origen, y el eje x 
ang = atan2(y = rmm2, x = rmm1)

## Diff hace la resta entre cada elemento (tomando el segundo - el primero), donde es positivo es que 
## el segundo numero es mayor, el angulo crece, el sistema se desplaza al este
resta = as.numeric(diff(ang))

# Considero un evento activo si cumple la cantidad de dias minimos con amplitud mayor a 1 y desplazandose 
# hacia el este dos tercios de dichos dias 
dias_este_min = round(min_dia*2/3)

activo 
EventoActivo <- function(Vector, MinValue, MinDay) {
  #Vector: Numeric vector que se analiza
  #MinValue: Valor minimo que el vector debe ser mayor
  #MinDay: Cantidad de dias consecutivos minimo mayores al valor minimo
  
  # Dias consecutivos mayores
  sum(with(rle(Vector > MinValue), values & lengths >= MinDay) &
      with(rle(resta > 0 ), values & lengths >= dias_este_min))
}

data = data.frame("DATE" = fechas,
                  "AMP" = amp,
                  "ANGULO" = ang,row.names = seq(1,length(fechas)))
setDT(data)

data[, Consec_Days_AMP := ifelse(AMP > 1, 1:.N, 0L), by = rleid(AMP > 1)]
data[, Cambio_Angulo := c(0,diff(ANGULO))]
data[, Consec_Days_ESTE := ifelse(Cambio_Angulo > 0, 1:.N, 0L), by = rleid(Cambio_Angulo > 0)]


# Ahora tomo los dias que cumplan con:
# - amplitud mayor al valor minimo la cant de dias minima (Ej 7) y 
# - desplazamiento este un tercio de la cant de dias minima (Ej 5)

activo = data[Consec_Days_AMP >= min_dia & Consec_Days_ESTE >= dias_este_min]

fecha_activo = activo$DATE
activo[, dif := c(0,diff(DATE))]
activo[, DiaEvento := ifelse(dif == 1, 1:.N, 0L), by = rleid(dif == 1)]

cant_eventos = sum(activo$DiaEvento == 0)

# Busco las fechas de inicio "0" y los 7 dias antes para iniciar el evento
fecha0 = activo[DiaEvento == 0,.(DATE)] 
fecha0_7 = fecha0 - 7


activo[, evento:= rleid())]
#DIFF(fecha) si el la posicion 17 es distinta, es la fecha numero 18
# determine if the region should be considered an "interruption"
activo[, interrupt := ifelse(dif != 1, TRUE, FALSE)]
activo[, group := rleid(interrupt)]

activo[, lapply(.SD, function(x) sum(x != "")), by = DATE]
# calculate run lengths for the observation values
o <- rle(activo$dif)

# assign a new column assigning each row(timepoint/observation) its run length
activo[, length := unlist(lapply(o$lengths, function(x) rep(x, each=x)))]


eventos <- list()

for (salto in 1:length(diff(fecha_activo))) {
  if(diff(fecha_activo)[salto]!=1) {
    eventos[[salto]] <- activo[]
  }
  
}

EventoActivo(Vector = as.numeric(amp),
             MinValue = 1,
             MinDay = min_dia)


library(data.table)
set.seed(120)

# Toy data set
dat <- data.table(time=seq(1,1000), obs=sample(c(0,0.01, 0.1, 1), size=1000, replace=TRUE, prob=c(0.3, 0.3, 0.3, 0.1)))

# calculate run lengths for the observation values
o <- rle(dat$obs)

# assign a new column assigning each row(timepoint/observation) its run length
dat[, length := unlist(lapply(o$lengths, function(x) rep(x, each=x)))]

# determine if the region should be considered an "interruption"
dat[, interrupt := ifelse(obs==0 & length>= 4, TRUE, FALSE)]

# assign values to each alternating interruption/grouped region
dat[, group := rleid(interrupt)]

# Remove sections with >= 4 obsevations of 0
dat2 <- dat[interrupt==FALSE]

# Re-number groups starting at 1
dat2[,group := as.numeric(as.factor(group))]