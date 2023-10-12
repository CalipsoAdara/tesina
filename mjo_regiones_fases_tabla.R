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
savepath = "/home/lucia.castro/SubX_processed_Rdata/model/viernes/MJO/fase/"

# Seteo el directorio
setwd("/home/lucia.castro/SubX_processed_Rdata/model/viernes")

# --------------------------------------------------------------------------------------------------------
# Cargo datos modelos y MJO
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
nmodels = length(models)
# La tabla de MJO no depende de la formacion del MME 
df_rmm <- readRDS("/home/lucia.castro/SubX_processed_Rdata/model/MJO/df_rmmOA.rds")
Bins = levels(df_rmm$Bin)
#--------------------------------
# Poligonos. 
SP <- readRDS("../poligonos/SP.rds")
SACZ <- readRDS("../poligonos/SACZ.rds")
SESA <- readRDS("../poligonos/SESA.rds")

POLI = list("SACZ"=SACZ, "SP" = SP, "SESA" = SESA)
#-----------------------------------------------



for (m in models) {  # por cada modelo
  
  # Tomo la observacion y el modelo
  modelweek = readRDS(paste0("./modelweek_",m,".rds"))
  obsweek = readRDS(paste0("./obsweek_",m,".rds"))
  
  # Cargo el targetdate
  targetdate = readRDS(paste0("./targetdate_",m,"_OA.rds"))
  startdate <- as.Date(dimnames(targetdate)$startdate)
  
  for (b in Bins) { # por cada MJO BIN
    # Busco que inicializaciones coinciden con las fase inicial
    f1 = as.numeric(substr(b,5,5))
    f2 = as.numeric(substr(b,6,6))
    fase_obs = df_rmm[FASE==f1 |FASE==f2 ,DATE]
    fase_stdt = startdate %in% fase_obs
    
    # Evaluo en esas startdates
    model_fase = modelweek[,,fase_stdt,]
    obs_fase = obsweek[,,fase_stdt,]
    
    # Metricas -------------------------
    # hago los calculos
    lon = dim(modelweek)[1]
    lat = dim(modelweek)[2]
    acc = ACC(Lon=lon, Lat=lat, Model = model_fase, Anom = obs_fase)
    
    # RMSE
    dif = (obs_fase-model_fase)**2
    rmse = sqrt(apply(dif,c(1,2,4),mean))
    
    # ponerle nombres 
    # Guardo
    dimnames(acc) <- dimnames(rmse)
    metric <- list( "acc" = acc,"rmse" = rmse)
    saveRDS(metric,paste0("./MJO/fase/metricweekINI_",m,"_",b,".rds"))
    
  } # end bin 

} # end loop modelo


# --------------------------------
# PROMEDIO ESPACIAL

for (m in models) { # por cada modelo
  for (b in Bins) { # por cada bin
  
    # cargo datos
    metric = readRDS(paste0("./MJO/fase/metricweekINI_",m,"_",b,".rds"))
    acc = metric[[1]]
    rmse = metric[[2]]
    
    # --------------------------------------------------------------------------------------------
    # PROMEDIOS EN REGIONES
    
    # Convierto a data frame
    df_acc = reshape2::melt(acc)
    df_rmse = reshape2::melt(rmse)
    
    for (p in 1:length(POLI)) {
      # Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
      puntos=pointsInPolygon(df_acc[,1:2],POLI[[p]]) 
      df_region = df_acc[puntos,]
      df_region_rm = df_rmse[puntos,]
      
      # Promedio en lat y long
      med_ac = DTPromEspacPesado(df_region, "value", "week", Lat = "lat")
      med_rm = DTPromEspacPesado(df_region_rm, "value", "week", Lat = "lat")
      
      # Guardo
      saveRDS(med_ac,paste0(savepath,"accsemanal_",m,"_",b,"_",names(POLI)[p],"2.rds"))
      saveRDS(med_rm,paste0(savepath,"rmsesemanal_",m,"_",b,"_",names(POLI)[p],"2.rds"))
      
    } # End loop regiones
  } # end loop bin
}# end loop model
#-----------------------------------------------------------------------------------
# Creacion de Tabla

## Modelos en filas, columnas de cada region, separadas por cada fase
# Una tabla para week 2 y otra para week 3
# Como no puedo agregar una linea extra para los bins, tienen este orden
# "Fase81" "Fase23" "Fase45" "Fase67"

# Data frame a llenar
df = data.frame()
met = "rmse"  # o "acc" 

for (m in models) {  # por cada modelo
  for (b in Bins) {  # por cada bin
    
    
    # Cargo
    score_sacz=readRDS(paste0(savepath,met,"semanal_",m,"_",b,"_SACZ2.rds"))
    score_sp=readRDS(paste0(savepath,met,"semanal_",m,"_",b,"_SP2.rds"))
    score_sesa=readRDS(paste0(savepath,met,"semanal_",m,"_",b,"_SESA2.rds"))
    
    # Agrego una columna con su respectiva region a cada uno
    score_sacz$region = rep("SACZ", nrow(score_sacz))
    score_sp$region = rep("PAT", nrow(score_sp))
    score_sesa$region = rep("SESA", nrow(score_sesa))
    score = rbind(score_sacz,score_sp, score_sesa)
    
    # Agrego una columna con el modelo y otra con la fase
    score$modelo = rep(m, nrow(score))
    score$fase = rep(b, nrow(score))
    df = rbind(df, score)
    
    
  }  # End loop bins
} # End loop models


# -----------------------------------------------------------------------------------
# G R A F I C O

sem = c("Week 2", "Week 3")
semlabel = c("Semana 2", "Semana 3")
for (w in 1:2) { # Voy a hacer el mismo grafico para dos plazos (week 2 y week 3)
  
  # Restringo a la semana 
  s = sem[w]
  dfs = df[week==s]
  
  # Agrego una columna que une la region y la fase
  dfs$regfa =  paste0(dfs$region,substr(dfs$fase,5,6))
  # Me quedo con las columnas que me interesan
  dfs <- dfs[,c("media","modelo","regfa")]
  
  # FACTORS ----------------------------------------------------------
  # La columna con los modelos la convierto en factors
  rep_str = c('GMAO'='**GMAO**<br>*GEOS*<br>',
              'RSMAS'='**RSMAS**<br>*CCSM4*<br>',
              'ESRL'='**ESRL**<br>*FIMr1p1*<br>',
              'ECCC'='**ECCC**<br>*GEM*<br>',
              'NRL'='**NRL**<br>*NESM*<br>',
              'EMC'='**EMC**<br>*GEFS*<br>',
              'MME'='**MME**')
  
  # Reemplazo
  dfs$modelo <- str_replace_all(dfs$modelo, rep_str)

  # Para que plotee los leads de forma correcta los convierto en factors
  dfs$regfa=  factor(dfs$regfa, levels=c('SACZ81','SESA81','PAT81',
                                           'SACZ23','SESA23','PAT23',
                                           'SACZ45','SESA45','PAT45',
                                           'SACZ67','SESA67','PAT67'))
  dfs$modelo =  factor(dfs$modelo, levels=c('**GMAO**<br>*GEOS*<br>',
                                            '**RSMAS**<br>*CCSM4*<br>',
                                            '**ESRL**<br>*FIMr1p1*<br>',
                                            '**ECCC**<br>*GEM*<br>',
                                            '**NRL**<br>*NESM*<br>',
                                            '**EMC**<br>*GEFS*<br>',
                                            '**MME**'))
  
  # GRAFICO  ----------------------------------------------------------

  g<-GraphHeatmap(Data = dfs, X = "regfa",Y = "modelo",Fill = "media",
                  Breaks = seq(0.5,2,0.25),  #seq(0.5,2,0.25)
                  Label = "%", Paleta = "rmse")
  #dfs$media <- dfs$media*100
  #g<-GraphHeatmap(Data = dfs, X = "regfa",Y = "modelo",Fill = "media",
  #                Breaks = c(-10,seq(0,60,10)),  #seq(0.5,2,0.25)
  #                Label = "%", Paleta = "acc")
  
  g <- g+geom_text(aes(label = format(round(get("media"),2),nsmall=1)),size=3, colour = "grey20",fontface = "italic")
  
  # detalles
  # Le agrego lineas para separar los cuatro MJO bins
  g <- g +geom_vline(xintercept=c(3.5,6.5,9.5), color = "white",size=1)
  g <- g +theme(plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0.5),
           legend.title.align = 0.1) +
    labs(title = paste0("<span style = 'font-size:14pt; font-family:Helvetica;'>RMSE de la **",semlabel[w],"** Proyecto SubX *(99-14, Oct-Abr)*
    </span><br>*Segun región y fase inicial de Madden-Julian* "))

  # guardo
  ggsave(paste0(savepath,"heatmap_w",substr(s,6,6),"_rmse2.png"),width = 8, height = 5)
  
}

## Significancia, solo en ACC

# Data frame a llenar
df_acc = data.frame()
met = "acc"  # 

for (m in models) {  # por cada modelo
  for (b in Bins) {  # por cada bin
    
    
    # Cargo
    score_sacz=readRDS(paste0(savepath,met,"semanal_",m,"_",b,"_SACZ2.rds"))
    score_sp=readRDS(paste0(savepath,met,"semanal_",m,"_",b,"_SP2.rds"))
    score_sesa=readRDS(paste0(savepath,met,"semanal_",m,"_",b,"_SESA2.rds"))
    
    # Agrego una columna con su respectiva region a cada uno
    score_sacz$region = rep("SACZ", nrow(score_sacz))
    score_sp$region = rep("PAT", nrow(score_sp))
    score_sesa$region = rep("SESA", nrow(score_sesa))
    score = rbind(score_sacz,score_sp, score_sesa)
    
    # Agrego una columna con el modelo y otra con la fase
    score$modelo = rep(m, nrow(score))
    score$fase = rep(b, nrow(score))
    df_acc = rbind(df_acc, score)
    
    
  }  # End loop bins
} # End loop models

# Hago la significancia por cada fase (todas las regiones tienen la misma cant de casos)
# y por cada modelo

bool = c()

for (m in models){
  # Cargo el targetdate
  targetdate = readRDS(paste0("./targetdate_",m,"_OA.rds"))
  startdate <- as.Date(dimnames(targetdate)$startdate)
  
  for (b in Bins) { # por cada MJO BIN
    # Busco que inicializaciones coinciden con las fase inicial
    f1 = as.numeric(substr(b,5,5))
    f2 = as.numeric(substr(b,6,6))
    fase_obs = df_rmm[FASE==f1 |FASE==f2 ,DATE]
    fase_stdt = startdate %in% fase_obs
  
    
    # Variable (estan las tres regiones)
    var = df_acc[modelo==m & fase==b]$media
    # Estadistico
    n = sum(fase_stdt)
    t = (var * sqrt(n - 2)) / sqrt(1-var^2)
    # Significancia de 0.05 y grados de libertad 
    critc = qt(p=0.95, df = trunc(n-2))
    test= t > critc
    # Despejo el valor de acc del estadistico
    acc_cr = sqrt(critc^2/(n-2+critc))
    
    # Guardo el test (un bool) en un vector
    bool = append(bool,test)
  }
}

# Agrego una nueva columna segun si pasa el test o no
df_acc$test = bool

# GRAFICOS

sem = c("Week 2", "Week 3")
semlabel = c("Semana 2", "Semana 3")

for (w in 1:2) { # Voy a hacer el mismo grafico para dos plazos (week 2 y week 3)
  
  # Restringo a la semana 
  s = sem[w]
  dfs = df_acc[week==s]
  
  # Agrego una columna que une la region y la fase
  dfs$regfa =  paste0(dfs$region,substr(dfs$fase,5,6))
  # Me quedo con las columnas que me interesan
  dfs <- dfs[,c("media","modelo","regfa","test")]
  
  # FACTORS ----------------------------------------------------------
  # La columna con los modelos la convierto en factors
  rep_str = c('GMAO'='**GMAO**<br>*GEOS*<br>',
              'RSMAS'='**RSMAS**<br>*CCSM4*<br>',
              'ESRL'='**ESRL**<br>*FIMr1p1*<br>',
              'ECCC'='**ECCC**<br>*GEM*<br>',
              'NRL'='**NRL**<br>*NESM*<br>',
              'EMC'='**EMC**<br>*GEFS*<br>',
              'MME'='**MME**')
  
  # Reemplazo
  dfs$modelo <- str_replace_all(dfs$modelo, rep_str)
  
  # Para que plotee los leads de forma correcta los convierto en factors
  dfs$regfa=  factor(dfs$regfa, levels=c('SACZ81','SESA81','PAT81',
                                         'SACZ23','SESA23','PAT23',
                                         'SACZ45','SESA45','PAT45',
                                         'SACZ67','SESA67','PAT67'))
  dfs$modelo =  factor(dfs$modelo, levels=c('**GMAO**<br>*GEOS*<br>',
                                            '**RSMAS**<br>*CCSM4*<br>',
                                            '**ESRL**<br>*FIMr1p1*<br>',
                                            '**ECCC**<br>*GEM*<br>',
                                            '**NRL**<br>*NESM*<br>',
                                            '**EMC**<br>*GEFS*<br>',
                                            '**MME**'))
  
  # GRAFICO  ----------------------------------------------------------
  

  dfs$media <- dfs$media*100
  g<-GraphHeatmap(Data = dfs, X = "regfa",Y = "modelo",Fill = "media",
                  Breaks = c(-10,seq(0,60,10)),  #seq(0.5,2,0.25)
                 Label = "%", Paleta = "acc")
  
  g <- g+geom_text(aes(label = format(round(get("media"),2),nsmall=1)),size=3, colour = "black",fontface = ifelse(dfs$test,"bold.italic","italic"))
  
  # detalles
  # Le agrego lineas para separar los cuatro MJO bins
  g <- g +geom_vline(xintercept=c(3.5,6.5,9.5), color = "white",size=1)
  g <- g +theme(plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0.5),
                legend.title.align = 0.1) +
    labs(title = paste0("<span style = 'font-size:14pt; font-family:Helvetica;'>ACC de la **",semlabel[w],"** Proyecto SubX *(99-14, Oct-Abr)*
    </span><br>*Segun región y fase inicial de Madden-Julian* "))
  
  # guardo
  ggsave(paste0(savepath,"heatmap_w",substr(s,6,6),"_acc2.png"),width = 8, height = 5)
  
}
