# Obtencion de Fechas extremas para la creacion del MME


# Las semanas extremas observadas son las que queres pronosticar con 1-28 dias
# por lo que, al crear el MME para la predictibilidad, es necesario definir
# los targetdates y no, los startdates!!

# Me gustaria pronosticar una semana extrema. En la semana 1 el fin de la semana
# deberia caer
# Supongo que no es tan grave no tener la primer semana, porque no queresmo 
# pronosticar con la primer semana

# Lo voy a hacer similar a la ola de calor

#
# Limpiar enviroment 
rm(list=ls())

# Llamar paquetes
library(ggplot2)
library(data.table)
library(abind)
library(secr)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/model/viernes"
FilePath = "/home/lucia.castro/SubX_processed_Rdata/model/viernes/predicc/ext/"

# Seteo el directorio
setwd(savepath)



# Cargo los datos 
models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL")
nmodels = length(models)

MODELOS <- list()
targetdateMODELOS <- list()
startdateMODELOS <- list()

for (m in 1:nmodels) {
  MODELOS[[m]] <- readRDS(paste0("./model_",models[m],"_OA.rds"))
  targetdateMODELOS[[m]] <- readRDS(paste0("./targetdate_",models[m],"_OA.rds"))
  startdateMODELOS[[m]] <- dimnames(targetdateMODELOS[[m]])$startdate
}

# Longitud y latitud, son las mismas para todos los modelos
lon = dimnames(MODELOS[[1]])$X
lat = dimnames(MODELOS[[1]])$Y

# Cargo datos de los poligonos
PAT <- readRDS("/home/lucia.castro/SubX_processed_Rdata/model/poligonos/SP.rds")
SACZ <- readRDS("/home/lucia.castro/SubX_processed_Rdata/model/poligonos/SACZ.rds")
SESA <- readRDS("/home/lucia.castro/SubX_processed_Rdata/model/poligonos/SESA.rds")

polis = list("PAT" = PAT, "SACZ" = SACZ, "SESA"=SESA)

# Cargo datos de fechas extremas
ext <- read.csv("./ext/extMME_reg.csv",stringsAsFactors = F)
# Le cambio los nombres a las columnas
colnames(ext) <- c("X","SACZ%10","SACZ%90","PAT%10","PAT%90","SESA%10","SESA%90")
colnombre <- colnames(ext[,-1]) # todas las columnas menos la primera
leadbin <- c(7,14,21)

for (col in 1:length(colnombre)){ # Por cada columna (percentil y region)
  string = unlist(strsplit(colnombre[col],"%"))
  region = string[1]
  percentil = string[2]
  fechas = as.Date(ext[,colnombre[col]])
  
  for (l in leadbin){ # Voyr a hacer la predictibilidad de c/grupo por separado 1-7, 8-14, 15-21
    
    # array a completar 
    MME_nmenos1 <- array(NA, dim = c(66,76,7,(nmodels-1),length(fechas)))
    mod_aparte <- array(NA, dim = c(66,76,7,length(fechas)))
    
    for (model in 1:nmodels) { # Por cada modelo
      mod_restante_stdate = startdateMODELOS[[model]]
      mod_restante_tgdate = targetdateMODELOS[[model]]
      MODELO_restante = MODELOS[[model]]
      
      mod_ensamble_stdate = startdateMODELOS[-model]
      mod_ensamble_tgdate = targetdateMODELOS[-model]
      MODELOS_ensamble = MODELOS[-model]
      nombres_ensamble = models[-model]
      
      for (i in 1:length(fechas)) { # Por cada inicio de semana extrema
        iniext = fechas[i]
        # Semana donde buscar la inicializacion
        # -1 para que la inic no caiga en la semana extrema
        semext = seq.Date(iniext,iniext+6,1)
        startweek= seq.Date(iniext-l,iniext-l+6,1)
        
        modelo_objetivo_rest = EncontrarPronosticoModelo(Modelo = MODELO_restante ,
                                                         ModelTG = mod_restante_tgdate,
                                                         ModelST = mod_restante_stdate,
                                                         ModelName = models[model],
                                                         Target = semext,
                                                         StartWeek = startweek)
        # guardo
        mod_aparte[,,,i] <- modelo_objetivo_rest
        
        # MEDIA ENSAMBLE N-1 MODELOS ----------------------------------
        for (mod in 1:(nmodels-1)) { # por cada modelo
          # Ahora busco los modelos del ensamble en las mismas fechas
          modelo_ensamble = EncontrarPronosticoModelo(Modelo = MODELOS_ensamble[[mod]],
                                                      ModelTG = mod_ensamble_tgdate[[mod]],
                                                      ModelST = mod_ensamble_stdate[[mod]],
                                                      ModelName = nombres_ensamble[[mod]],
                                                      Target = semext,
                                                      StartWeek = startweek)
          
          # Guardo
          MME_nmenos1[,,,mod,i] <- modelo_ensamble
          
        }# End ensamble
        
      }# End semanas extremas
      
      MME_pro = apply(MME_nmenos1 , c(1,2,3,5), mean, na.rm = T)  # Creado la media del ensamble n-1
      
      # Correlacion entre mod y ens n-1
      cor_mod <- Predictibilidad(mod_aparte, MME_pro)

      
      saveRDS(cor_mod, paste0(FilePath,region,"/predic_",region,"_",l,"_p",percentil,"_",models[model],".rds"))
      print(paste("termino la correlacion del modelo",models[model],"en el lead",l, "en la region",region))
      
      # ELimino para hacer espacio
      rm(cor_mod,MME_pro)
      
    } # End loop models
    
    # Ahora promedio todas las correlaciones obtenidad de cada modelo (cuarta dimension)
    # Guarde todas las correlaciones por separado para que sea menos pesado y ahora promedio
    
    cor_mod <- array(NA, dim = c(66,76,7,nmodels))
    for (m in 1:nmodels) {
      cor_mod[,,,model] <- readRDS(paste0(FilePath,region,"/predic_",region,"_",l,"_p",percentil,"_",models[model],".rds"))
    }
    
    predictibilidad = apply(cor_mod , c(1,2,3), mean, na.rm = T)
    saveRDS(predictibilidad, paste0(FilePath,region,"/predic_ext_",region,"_",l,"_p",percentil,".rds"))
  } #End leads

} # End percentiles


#--------------------------------------------------------------------------------------------------------
# PREDICTIBILIDAD NO EXTREMA
# hago lo mismo con el resto de la semanas


# MME
targetdateMME <- readRDS(paste0("./targetdate_MME_OA.rds"))
TotalesMME<- as.Date(dimnames(targetdateMME)$startdate)


# Busco la fechas no extremas de cada region
sacz_ext = as.Date(c(ext$`SACZ%10`,ext$`SACZ%90`))
sesa_ext = as.Date(c(ext$`SESA%10`,ext$`SESA%90`))
pat_ext = as.Date(c(ext$`PAT%10`,ext$`PAT%90`))

fechas_reg = list("SACZ" = TotalesMME[!(TotalesMME %in% sacz_ext)],
              "SESA" = TotalesMME[!(TotalesMME %in% sesa_ext)],
              "PAT" = TotalesMME[!(TotalesMME %in% pat_ext)])
regiones = c("SACZ","SESA","PAT")

for (region in regiones) { # Por cada region
  fechas = fechas_reg[[region]]


for (l in leadbin){ # Voyr a hacer la predictibilidad de c/grupo por separado 1-7, 8-14, 15-21
  
  # array a completar 
  MME_nmenos1 <- array(NA, dim = c(66,76,7,(nmodels-1),length(fechas)))
  mod_aparte <- array(NA, dim = c(66,76,7,length(fechas)))
  
  for (model in 1:nmodels) { # Por cada modelo
    mod_restante_stdate = startdateMODELOS[[model]]
    mod_restante_tgdate = targetdateMODELOS[[model]]
    MODELO_restante = MODELOS[[model]]
    
    mod_ensamble_stdate = startdateMODELOS[-model]
    mod_ensamble_tgdate = targetdateMODELOS[-model]
    MODELOS_ensamble = MODELOS[-model]
    nombres_ensamble = models[-model]
    
    for (i in 1:length(fechas)) { # Por cada inicio de semana extrema
      iniext = fechas[i]
      # Semana donde buscar la inicializacion
      # -1 para que la inic no caiga en la semana extrema
      semext = seq.Date(iniext,iniext+6,1)
      startweek= seq.Date(iniext-l,iniext-l+6,1)
      
      modelo_objetivo_rest = EncontrarPronosticoModelo(Modelo = MODELO_restante ,
                                                       ModelTG = mod_restante_tgdate,
                                                       ModelST = mod_restante_stdate,
                                                       ModelName = models[model],
                                                       Target = semext,
                                                       StartWeek = startweek)
      # guardo
      mod_aparte[,,,i] <- modelo_objetivo_rest
      
      # MEDIA ENSAMBLE N-1 MODELOS ----------------------------------
      for (mod in 1:(nmodels-1)) { # por cada modelo
        # Ahora busco los modelos del ensamble en las mismas fechas
        modelo_ensamble = EncontrarPronosticoModelo(Modelo = MODELOS_ensamble[[mod]],
                                                    ModelTG = mod_ensamble_tgdate[[mod]],
                                                    ModelST = mod_ensamble_stdate[[mod]],
                                                    ModelName = nombres_ensamble[[mod]],
                                                    Target = semext,
                                                    StartWeek = startweek)
        
        # Guardo
        MME_nmenos1[,,,mod,i] <- modelo_ensamble
        
      }# End ensamble
      
    }# End semanas extremas
    
    MME_pro = apply(MME_nmenos1 , c(1,2,3,5), mean, na.rm = T)  # Creado la media del ensamble n-1
    
    # Correlacion entre mod y ens n-1
    cor_mod <- Predictibilidad(mod_aparte, MME_pro)
    saveRDS(cor_mod, paste0(FilePath,region,"/predic_",region,"_",l,"_noext_",models[model],".rds"))
    print(paste("termino la correlacion del modelo",models[model],"en el lead",l, "en la region",region))
    
    # ELimino para hacer espacio
    rm(cor_mod,MME_pro)
    
  } # End loop models
  
  # Ahora promedio todas las correlaciones obtenidad de cada modelo (cuarta dimension)
  # Guarde todas las correlaciones por separado para que sea menos pesado y ahora promedio
  
  cor_mod <- array(NA, dim = c(66,76,7,nmodels))
  for (m in 1:nmodels) {
    cor_mod[,,,model] <- readRDS(paste0(FilePath,region,"/predic_",region,"_",l,"_noext_",models[model],".rds"))
  }
  
  predictibilidad = apply(cor_mod , c(1,2,3), mean, na.rm = T)
  saveRDS(predictibilidad, paste0(FilePath,region,"/predic_noext_",l,"_",region,".rds"))
} #End leads

} # End region
  
# ------------------------------------------------------------------------------------
# ACOMODAR LOS ARRAYS para unir todos los grupos


# -----------------
# NO EXTREMOS
predleads = array(NA, dim = c(66,76,7,length(leadbin)))
regiones = c("SACZ","SESA","PAT")
for (region in regiones) {
  for (l in 1:length(leadbin)) {
    predleads[,,,l] <- readRDS(paste0(FilePath,region,"/predic_noext_",leadbin[l],"_",region,".rds"))
  }
  saveRDS(predleads, paste0(FilePath,region,"/predictibilidad_noext_",region,".rds"))
}


# -----------------
# EXTREMOS
# Cargo los tres leads en un solo array
for (col in 1:length(colnombre)) {
  string = unlist(strsplit(colnombre[col],"%"))
  region = string[1]
  percentil = string[2]
  predleads = array(NA, dim = c(66,76,7,length(leadbin)))
  
  for (l in 1:length(leadbin)) {
    lb = leadbin[l]
    predleads[,,,l]<- readRDS(paste0(FilePath,region,"/predic_ext_",region,"_",lb,"_p",percentil,".rds"))
  }
  saveRDS(predleads, paste0(FilePath,region,"/predictibilidad",region,"_ext",percentil,".rds"))
}



# -------------------------------------------------------------
# Graficos

for (col in 1:length(colnombre)) {
  string = unlist(strsplit(colnombre[col],"%"))
  region = string[1]
  percentil = string[2]
  
  # Cargo datos de predictibilidad del bin
  pred <- readRDS(paste0(FilePath,region,"/predictibilidad",region,"_ext",percentil,".rds"))
  prednox <- readRDS(paste0(FilePath,region,"/predictibilidad_noext_",region,".rds"))
  
  # Hago la resta
  resta = pred*100-prednox*100
  
  # Hago el promedio semanal para cada leadbin
  predsem= apply(resta, c(1,2,4), mean, na.rm = T)
  dimnames(predsem) <- list("lon" = lon, "lat" = lat, "Leads" = c("1-7 Días","8-14 Días","15-21 Días"))
  
  # Convierto DF y busco la region
  df = reshape2::melt(predsem)
  poli = polis[[region]]
  points=pointsInPolygon(df[,1:2],poli) 
  df_region = df[points,]
  # Promedio en lat y long
  med = DTPromEspacPesado(df_region, "value", "Leads", Lat = "lat")
  # Guardado
  saveRDS(med, paste0(FilePath,region,"/df_",region,"_p",percentil,".rds"))
  
}

# Acomodo para graficar un heatmap por percentil, COlumnas leads, filas Regiones
df <- data.frame()
percentil = c("10","90")

for (per in percentil) { # Por cada percentil
  med_sacz = readRDS(paste0(FilePath,"SACZ/df_SACZ_p",per,".rds"))
  med_sesa = readRDS(paste0(FilePath,"SESA/df_SESA_p",per,".rds"))
  med_pat = readRDS(paste0(FilePath,"PAT/df_PAT_p",per,".rds"))
  
  # Agrego una columna con su respectiva region a cada uno
  med_sacz$region = rep(paste0("SACZ",per), nrow(med_sacz))
  med_sesa$region = rep(paste0("SESA",per), nrow(med_sesa))
  med_pat$region = rep(paste0("PAT",per), nrow(med_pat))
  df_med = rbind(med_sacz,med_sesa,med_pat)
  df = rbind(df,df_med)

  

}

# Le cambio el orden a las cosas conviertinedo a factors
df$region <- factor(df$region , levels=c("SACZ10","SESA10","PAT10","SACZ90","SESA90","PAT90"))
df$Leads <- factor(df$Leads, levels = c("15-21 Días","8-14 Días","1-7 Días"))

# Grafico
palette = c("#2166AC","#4393C3", "#92C5DE","#D1E5F0","#FDDBC7", "#F4A582","#D6604D", "#B2182B")

g <- GraphHeatmapPaleta(Data = df, X = "region", Y="Leads", Fill="media",
                  Breaks = seq(-20,20,5), Label = "p.p.", Paleta =  palette)


g<- g +geom_vline(xintercept=c(3.5), color = "white",linewidth=1)
g <- g +theme(plot.title = element_markdown(size = 11, lineheight = 1.2, hjust = 0.5),
              legend.title.align = 0.1) +
  labs(title = paste0("<span style = 'font-size:12pt; font-family:Helvetica;'>Dif. de Predictibilidad en **Extremos de T2M**"," Proyecto SubX 
    </span><br>*Segun región y percentil (99-14, Oct-Abr)* "))
g <- g+geom_text(aes(label = format(round(get("media"),2),nsmall=1)),size=3, colour = "grey5",fontface = "italic")
g
ggsave(plot = g, filename = paste0(FilePath,"heatmap_predic_ext.png"),width = 8, height = 5)
