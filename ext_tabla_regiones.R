# Script para realizar la verificacion de losmodelos 
# en extremos de temperatura
# Heatmap con los scores calculados en las 3 regiones, para cada fase

## By Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------------
# Limpiar enviroment 
rm(list=ls())

# Llamar paquetes
library(secr)
library(data.table)

# Para graficar
library(ggplot2)
library(ggtext)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath ="/home/lucia.castro/SubX_processed_Rdata/model/viernes/ext/verif/"

# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata/model/viernes")

# --------------------------------------------------------------------------------------------------------
# Cargo datos modelos y MJO
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
nmodels = length(models)
MODELOS <- list()
OBS <- list()
targetdateMODELOS <- list()
startdateMODELOS <- list()

for (m in 1:nmodels) {
  MODELOS[[m]] <- readRDS(paste0("./model_",models[m],"_OA.rds"))
  OBS[[m]]<- readRDS(paste0("./obs_",models[m],".rds"))
  targetdateMODELOS[[m]] <- readRDS(paste0("./targetdate_",models[m],"_OA.rds"))
  startdateMODELOS[[m]] <- dimnames(targetdateMODELOS[[m]])$startdate
}

# La tabla de extremos de temperatura observadas
ext <- read.csv("./ext/extMME_reg.csv",stringsAsFactors = F)
colnames(ext) <- c("X","SACZ%10","SACZ%90","PAT%10","PAT%90","SESA%10","SESA%90")
colnombre <- colnames(ext[,-1]) # todas las columnas menos la primera
leadbin <- c(7,14,21)
#--------------------------------
# Poligonos. 
SP <- readRDS("../poligonos/SP.rds")
SACZ <- readRDS("../poligonos/SACZ.rds")
SESA <- readRDS("../poligonos/SESA.rds")

POLI = list("SACZ"=SACZ, "PAT" = SP, "SESA" = SESA)



# Necesito calcular la correlacion y el RMSE para las semanas extremas OBJETIVO
# es decir, una semana es pronosticada con 1-7, 8-14 y 15-21 dias antes

# Porque lo calculo para todo el dominio en vez de en solo las regiones de interes? 
# Bueno porque el array es cuadrado y la region no. 
# Entonces deberia buscar la forma de calcular metricas dataframes 
# De seguro es posible pero es un gasto extra de tiempo 

for (col in 1:length(colnombre)) { # Por cada region y percentil
  string = unlist(strsplit(colnombre[col],"%"))
  region = string[1]
  percentil = string[2]
  fechas = as.Date(ext[,colnombre[col]])
  
  
  for (m in 1:nmodels) { # Por cada modelo
    
    # Arrays a llenar
    pron = array(NA, dim = c(66,76,length(leadbin),length(fechas))) # 7 es la duracion de cada semana 7
    obs = array(NA, dim = c(66,76,length(leadbin),length(fechas))) 
    
    for (i in 1:length(fechas)) { # POr cada semana extrema
      iniext = fechas[i]
      
      
      for (bin in 1:length(leadbin)) { # Por cada leadbin
        l = leadbin[bin]
        semext = seq.Date(iniext,iniext+6,1)
        startweek= seq.Date(iniext-l,iniext-l+6,1)
        
 
        
        # Encuentro el modelo y la observacion que pronostique esa semana ext
        # con los leads adecuados
        modelo_objetivo = EncontrarPronosticoModelo(Modelo = MODELOS[[m]],
                                                    ModelTG = targetdateMODELOS[[m]],
                                                    ModelST = startdateMODELOS[[m]],
                                                    ModelName = models[m],
                                                    Target = semext,
                                                    StartWeek = startweek)
        
        obs_objetivo = EncontrarPronosticoModelo(Modelo =OBS[[m]],
                                                    ModelTG = targetdateMODELOS[[m]],
                                                    ModelST = startdateMODELOS[[m]],
                                                    ModelName = models[m],
                                                    Target = semext,
                                                    StartWeek = startweek)
        
        modweek = apply(modelo_objetivo, c(1,2), mean, na.rm=T)
        obsweek = apply(obs_objetivo, c(1,2), mean, na.rm=T)
        
        # Guardo
        pron[,,bin,i] <- modweek
        obs[,,bin,i] <- obsweek
      }# end leadbin
    } # End loop sem extrama
    
    # Calculo las metricas
    # ACC
    #acc = ACC3(Ar1 = pron,Ar2 = obs, Dim = 3)
    acc = ACC2(Ar1 = pron,Ar2 = obs, Dim = 4)
    
    # RMSE
    dif = (pron-obs)**2
    rmse = sqrt(apply(dif,c(1,2,3),mean,na.rm = T))
    
    # Guardado
    saveRDS(rmse, file = paste0(savepath,region,"/rmse_",models[m],"_",region,"_",percentil,".rds"))
    saveRDS(acc, file = paste0(savepath,region,"/acc_",models[m],"_",region,"_",percentil,".rds"))
    
    
  } # End loop modelos
} # End loop region y percentil



#--------------------------------------------------------------------------------------------
# Restriccion a Poligonos
# Longitud y latitud, son las mismas para todos los modelos
lon = dimnames(MODELOS[[1]])$X
lat = dimnames(MODELOS[[1]])$Y


for (col in 1:length(colnombre)) { # Por cada region y percentil
  string = unlist(strsplit(colnombre[col],"%"))
  region = string[1]
  percentil = string[2]
  fechas = as.Date(ext[,colnombre[col]])
  
  for (m in 1:nmodels) { # Por cada modelo 
    acc = readRDS(paste0(savepath,region,"/acc_",models[m],"_",region,"_",percentil,".rds"))
    rmse = readRDS(paste0(savepath,region,"/rmse_",models[m],"_",region,"_",percentil,".rds"))
    
    # Cambio nombres
    dimnames(acc) <- list("lon" = lon, "lat" = lat, "Leads" = c("1-7 Días","8-14 Días","15-21 Días"))
    dimnames(rmse) <- list("lon" = lon, "lat" = lat, "Leads" = c("1-7 Días","8-14 Días","15-21 Días"))
    

    # restringo
    df_acc = reshape2::melt(acc)
    df_rmse = reshape2::melt(rmse)
    poli= POLI[[region]]
    
    # Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
    puntos=pointsInPolygon(df_acc[,1:2],poli) 
    df_region_acc = df_acc[puntos,]
    df_region_rm = df_rmse[puntos,]
    
    # Promedio en lat y long
    med_ac = DTPromEspacPesado(df_region_acc, "value", "Leads", Lat = "lat")
    med_rm = DTPromEspacPesado(df_region_rm, "value", "Leads", Lat = "lat")
    
    # Guardo
    saveRDS(med_ac,paste0(savepath,"accsemanal_",models[m],"_p",percentil,"_",region,"2.rds"))
    saveRDS(med_rm,paste0(savepath,"rmsesemanal_",models[m],"_p",percentil,"_",region,"2.rds"))
  }
}

# Armo un dataframe para graficar la tabla. Filas: Modelos, Columnas:Region y percentil

df_acc <-data.frame()
df_rmse <-data.frame()

for (m in 1:nmodels) {
  
  for (col in 1:length(colnombre)) {
    string = unlist(strsplit(colnombre[col],"%"))
    region = string[1]
    percentil = string[2]
    fechas = as.Date(ext[,colnombre[col]])
    acc = readRDS(paste0(savepath,"accsemanal_",models[m],"_p",percentil,"_",region,"2.rds"))
    rmse = readRDS(paste0(savepath,"rmsesemanal_",models[m],"_p",percentil,"_",region,"2.rds"))
    
    # Le agrego una columna de modelos
    acc$model = rep(models[m],nrow(acc))
    rmse$model = rep(models[m],nrow(rmse))
    
    # Le agrego una columna de region
    acc$region = rep(region,nrow(acc))
    rmse$region = rep(region,nrow(rmse))
    
    # Le agrego una columna de lead y region
    setDT(acc)
    setDT(rmse)
    acc[,regionl:=paste0(region,Leads)]
    rmse[,regionl:=paste0(region,Leads)]

    # Le agrego columna de percentil
    acc$per <- rep(percentil, nrow(acc))
    rmse$per <- rep(percentil, nrow(rmse))
    
    # uno
    df_acc  <- rbind(df_acc,acc)
    df_rmse  <- rbind(df_rmse,rmse)
    

  }
  
}


# -------------------------------------------------------------------------------------------------
# GRAFICOS acc y rmse, uno por percentil

per = c("10","90")
for (percentil in per) { # Por cada percentil
  
  setDT(df_acc)
  setDT(df_rmse)
  df_acc_per = df_acc[per == percentil]
  df_rmse_per = df_rmse[per == percentil]
  
  # Le pongo factors para que tenga el orden que yo quiero
  df_acc_per = FactorsModelsPretty(DF = df_acc_per, "model")
  df_rmse_per = FactorsModelsPretty(DF = df_rmse_per, "model")
  
  # Para que plotee los leads de forma correcta los convierto en factors
  df_acc_per$regionl=  factor(df_acc_per$regionl, levels=c('SACZ1-7 Días','SESA1-7 Días','PAT1-7 Días',
                                                   'SACZ8-14 Días','SESA8-14 Días','PAT8-14 Días',
                                                   'SACZ15-21 Días','SESA15-21 Días','PAT15-21 Días'))
  df_rmse_per$regionl=  factor(df_rmse_per$regionl, levels=c('SACZ1-7 Días','SESA1-7 Días','PAT1-7 Días',
                                                     'SACZ8-14 Días','SESA8-14 Días','PAT8-14 Días',
                                                     'SACZ15-21 Días','SESA15-21 Días','PAT15-21 Días'))
  
  # Graficos
  df_acc_per$media <- df_acc_per$media*100
  gacc <- GraphHeatmap(Data = df_acc_per, X="regionl", Y="MODEL", Fill="media",
                       Paleta = "acc",Label = "%", Breaks = c(-1,seq(0,60,10)))
  gacc <- gacc + geom_vline(xintercept=c(3.5,6.5,9.5), color = "white",size=1)
  gacc <- gacc +theme(plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0.5),
                      legend.title.align = 0.1) +
    labs(title = paste0("<span style = 'font-size:14pt; font-family:Helvetica;'>ACC del percentil **",percentil,"** Proyecto SubX *(99-14, Oct-Abr)*
    </span><br>*Segun región y días de antelación a la semana objetivo* "))
  gacc <- gacc+geom_text(aes(label = format(round(get("media"),2),nsmall=1)),size=3, colour = "black",fontface = ifelse(df_acc_per$media>acc_cr*100,"bold.italic","italic"))


  
  grmse <- GraphHeatmap(Data = df_rmse_per, X="regionl", Y="MODEL", Fill="media",
               Paleta = "rmse",Label = "°C", Breaks = seq(0,3,0.5))
  grmse <- grmse + geom_vline(xintercept=c(3.5,6.5,9.5), color = "white",size=1)
  grmse <- grmse +theme(plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0.5),
                        legend.title.align = 0.1) +
    labs(title = paste0("<span style = 'font-size:14pt; font-family:Helvetica;'>RMSE del percentil **",percentil,"** Proyecto SubX *(99-14, Oct-Abr)*
    </span><br>*Segun región y días de antelación a la semana objetivo* "))
  grmse <- grmse+geom_text(aes(label = format(round(get("media"),2),nsmall=1)),size=3, colour = "grey30",fontface = "italic")

  # guardo
  ggsave(plot = gacc,paste0(savepath,"heatmap_p",percentil,"_acc.png"),width = 8, height = 5)
  ggsave(plot = grmse,paste0(savepath,"heatmap_p",percentil,"_rmse.png"),width = 8, height = 5)
  
  
}

# SIgnificancia
p90acc = df_acc[per=="90"]$media
p10acc = df_acc[per=="10"]$media

# Estadistico
n90 = 47
n10 = 47
t90 = (p90acc * sqrt(n90 - 2)) / sqrt(1-p90acc^2)
t10 = (p10acc * sqrt(n10 - 2)) / sqrt(1-p10acc^2)


# Significancia de 0.05 y grados de libertad  (el n de 10 y 90 son iguales)
critc = qt(p=0.95, df = trunc(n90-2))
test90 = t90 > critc
test10 = t10 > critc

# Despejo el valor de acc del estadistico
acc_cr = sqrt(critc^2/(n90-2+critc))
# Osea que los valores significativos son
p90acc[test90]
p10acc[test10]

df_90=df_acc[per=="90"]
df_90[test90,]

df_10=df_acc[per=="10"]
df_10[test10,]
#---------------------



# Le pongo factors para que tenga el orden que yo quiero
df_acc = FactorsModelsPretty(DF = df_acc, "model")
df_rmse = FactorsModelsPretty(DF = df_rmse, "model")


# Para que plotee los leads de forma correcta los convierto en factors
df_acc$regionl=  factor(df_acc$regionl, levels=c('SACZ1-7 Días','SESA1-7 Días','PAT1-7 Días',
                                                 'SACZ8-14 Días','SESA8-14 Días','PAT8-14 Días',
                                                 'SACZ15-21 Días','SESA15-21 Días','PAT15-21 Días'))
df_rmse$regionl=  factor(df_rmse$regionl, levels=c('SACZ1-7 Días','SESA1-7 Días','PAT1-7 Días',
                                                   'SACZ8-14 Días','SESA8-14 Días','PAT8-14 Días',
                                                   'SACZ15-21 Días','SESA15-21 Días','PAT15-21 Días'))



# Graficos
gacc <- GraphHeatmap(Data = df_acc, X="regionl", Y="MODEL", Fill="media",
                     Paleta = "acc",Label = "%", Breaks = c(-1,seq(0,0.6,0.1)))
gacc <- gacc + geom_vline(xintercept=c(3.5,6.5,9.5), color = "white",size=1)
gacc <- gacc +theme(plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0.5),
                    legend.title.align = 0.1) +
  labs(title = paste0("<span style = 'font-size:14pt; font-family:Helvetica;'>ACC del percentil **",percentil,"** Proyecto SubX *(99-14, Oct-Abr)*
    </span><br>*Segun región y días de antelación a la semana objetivo* "))

# guardo
ggsave(plot = gacc,paste0(savepath,"heatmap_p",percentil,"_acc.png"),width = 8, height = 5)
ggsave(plot = gacc,paste0(savepath,"heatmap_p",percentil,"_acc.png"),width = 8, height = 5)

GraphHeatmap(Data = df_rmse, X="regionl", Y="MODEL", Fill="media",
             Paleta = "rmse",Label = "%", Breaks = seq(0,3,0.5))

dimnames(rmse) <- list("x" = lon, "y" = lat, "week" = c("1-7 Días","8-14 Días","15-21 Días"))
dimnames(acc) <- list("x" = lon, "y" = lat, "week" = c("1-7 Días","8-14 Días","15-21 Días"))
df_acc = reshape2::melt(acc)
df_rmse = reshape2::melt(rmse)
colnames(df_acc) <- c("x","y","week","z")
colnames(df_rmse) <- c("x","y","week","z")
GraphDiscreteMultiple(Data =df_acc, Breaks = seq(0,1,0.2), Paleta = "Greens", Direccion = 1, Label = "%" )
GraphDiscreteMultiple(Data =df_rmse, Breaks = seq(0,4,0.5), Paleta = "Greens", Direccion = -1, Label = "%" )

acc1 = acc[,,,1]
dimnames(acc1) <- list("x" = lon, "y" = lat, "week" = c("1","2","3","4","5","6","7"))
df_acc1 = reshape2::melt(acc1)
colnames(df_acc1) <- c("x","y","week","z")
GraphDiscreteMultiple(Data =df_acc1, Breaks = seq(0,1,0.2), Paleta = "Greens", Direccion = 1, Label = "%" )
