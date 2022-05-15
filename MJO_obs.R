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
library(csv)
library(fancycut)

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

#saveRDS(MJO, "./MJO/MJO_obs.rds")

# ---------------------------------------------- EVENTOS ACTIVOS -------------------------------------------------
# Considero un evento activo si cumple la cantidad de dias minimos con amplitud mayor a 1 y desplazandose 
# hacia el este dos tercios de dichos dias 
median(amp) #1.195 es mayor a uno
min_dia = 7 # Cantidad de dias minima para que el evento se considere activo
dias_este_min = round(min_dia*2/3)
amp_min = 1.19

## Calculo el angulo entre la recta formada por el punto y el origen, y el eje x 
ang = atan2(y = rmm2, x = rmm1)

## Diff hace la resta entre cada elemento (tomando el segundo - el primero), donde es positivo es que 
## el segundo numero es mayor, el angulo crece, el sistema se desplaza al este
resta = as.numeric(diff(ang))

data = data.frame("DATE" = fechas,
                  "AMP" = amp,
                  "FASE" = fase,
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
fecha_ini = fecha0 - min_dia
fecha_fin = activo[, .SD[.N], by=Evento]$DATE
fase_ini= activo[, .SD[1], by=Evento]$FASE
cant_eventos = length(fecha0)

# Busco la amplitud media de cada evento
amp_media = activo[,list(amp_med=mean(AMP)),by=.(Evento)]



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
df_rmm$Evento <- as.numeric(df_rmm$Evento)


# Busco las fases que atraveso cada evento
fases = aggregate( FASE~Evento,df_rmm, function(x) unique(x))

# Acomodo informacion en un data frame
df_eventos <- data.frame("Inicio" = fecha_ini, 
                         "Final" = fecha_fin,
                         "Duracion" = fecha_fin - fecha_ini,
                         "AmpMedia" = as.numeric(amp_media$amp_med),
                         "FaseIni" = fase_ini,
                         "Fases" = as.character(fases$FASE))

setDT(df_eventos)
#Guardo
saveRDS(df_rmm, "./MJO/df_rmm.rds")


# Graficos de cada evento
for (e in 1:cant_eventos) {
  evento = df_rmm[Evento == e]
  g = GraphRMM(evento)
  dia = format(evento$DATE[1], "%d_%m_%y")
  ggsave(g,filename = paste0("./MJO/E",e,"_diagrama_",dia,".png"), height = 7, width = 7) 
}

# TABLA CON ESTADISTICAS 
# ver cuantas inicializaciones en cada fase
# Convierto el 8 a -8 para que funque el CUT 
df_eventos[FaseIni == 8]$FaseIni <- c(-8)
# Agrupo segun dos fases cada uno
df_eventos[,Bin := fancycut(FaseIni, Fase81 = '[-8,1]',
                            Fase23 = '[2,3]',
                            Fase45 = '[4,5]',
                            Fase67 = '[6,7]')]
diasvsfases=df_eventos[, .(.N), by = .(Bin)]

# Esta informacion la guardo en df_rmm tambien
df_rmm[,Bin:=(rep(df_eventos$Bin, (df_eventos$Duracion+1)))]
# Repite el valor de Bin por la duracion del evento 
# (+1 para que cuente el dia inicial)
saveRDS(df_eventos,"./MJO/df_eventos.rds")
saveRDS(df_rmm,"./MJO/df_rmm.rds")

#

#######
# TABLA DE EVENTOS VS FASES 
# hago una tabla que dice la cantidad de dias en cada fase segun el evento
# Ademas agrego en que fase del NIño estaba durante el evento

# Cuento cantidad de dias en cada fase segun el evento 
diasfases = df_rmm[, .(.N), by = .(Evento,FASE)]
# Junto evento y fase en una columna
diasfases[, EvenFase:= paste(Evento,FASE)]
EvenFaseComp=paste(rep(1:cant_eventos,each=8),rep(1:8,cant_eventos))
y=match(EvenFaseComp, diasfases$EvenFase)


# Cargo datos de ONI (NIÑO)
t<-download.file("https://origin.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/detrend.nino34.ascii.txt", destfile = "./MJO/oni")
t = download.oni.cpc.ncep.noaa(GUI = FALSE)
#Cargo datos de MEI (el niño)
mei <- read.table("./MJO/meiv2.data", header = F, nrows = 43, skip = 1,
                  col.names = c("YEAR","DecJan", "JanFeb", "FebMar", "MarApr", "AprMay", 
                                "MayJun", "JunJul", "JulAug", "AugSep", "SepOct", "OctNov", "NovDec"))

# Obtengo el año y mes de cada evento
aniomes <- GetMesEvento(DF = df_rmm, NEvento = cant_eventos)
mesmei = colnames(mei)                      # mes del indice mei
mesevent = substr(aniomes,5,nchar(aniomes)) # mes del evento

list_nino <- array()
for (i in 1:cant_eventos) {
  
  if (nchar(mesevent[i])==6) {  # Si el evento ocurre en dos meses, usa el indice como viene
    # La columna es aquella igual a los meses del evento y
    # la fila es aquella igual al año del evento
    col = which(mesmei %in% mesevent[i])            
    fila = which(mei$YEAR == substr(aniomes[i],1,4)) 
    index = mei[fila,col]}
  
  if (nchar(mesevent[i])==9) {  # SI el evento ocurre en tres meses se usa el indice 
                                # de los dos meses mas prominentes (mayor canti de dias)
    evento = df_rmm[Evento == i]
    # Extraigo mes (en letras) y año, cuenta la cant de dias en cada mes
    anio_mes = rle(format(evento$DATE, "%b"))
    # quito el mes con menos dias, osea, me quedo con los dos meses prominentes
    mesp = paste(anio_mes$values[-which.min((anio_mes$lengths))], collapse = "")
    
    col = which(mesmei %in% mesp)            
    fila = which(mei$YEAR == substr(aniomes[i],1,4)) 
    index = mei[fila,col]}
  
  if (nchar(mesevent[i])==12) { # Si el evento ocurre en cuatro meses tomo los dos indices
                                # bimestrales y los promedio
    col1 = which(mesmei %in% substr(mesevent[i],1,6))
    col2 = which(mesmei %in% substr(mesevent[i],4,9))
    col3 = which(mesmei %in% substr(mesevent[i],7,12))
    fila = which(mei$YEAR == substr(aniomes[i],1,4)) 
    index = (mei[fila,col1] + mei[fila,col2] + mei[fila,col3]) /3}
  
  if (nchar(mesevent[i])==3) { # Si el evento ocurre en un mes tomo los dos indices en los
                               # que esta el mes y los promedio
    
    col = grepl( substr(mesevent[i],1,3), mesmei, fixed = TRUE)
    fila = which(mei$YEAR == substr(aniomes[i],1,4))
    index = mean(as.numeric(mei[fila,col])) }
  
  list_nino[[i]] = NinooNina(Ind = index, Criterio = 0.5)
  
}


# Acomodo en tabla y tengo NA donde no hay dias en esas fases
tabla_diafases = t(array(data = diasfases$N[y], dim = c(8,cant_eventos), 
      dimnames = list("Fase"=1:8, "Evento"=1:cant_eventos)))
# Agrego columna de fase NIño
df_diafases <- cbind(as.data.frame(tabla_diafases),list_nino) # converti a dataframe para tener letras y num en el mismo objeto

# Guardo en csv
write.csv(df_diafases, file = "./tabladiafases.csv")


# -------------------------- 
# G R A F I C O S 


 
 

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