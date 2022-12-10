# Script para ordenar los datos del reanalisis del CPC  segun las dimensiones de los datos SubX
# tiene en cuenta los leads


# By Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------------
# Limpiar enviroment 
rm(list=ls())

# Llamar paquetes
library(ggplot2)
library(ggpubr)
library(secr)

# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/model"

# Seteo el directorio
setwd(savepath)

# --------------------------------------------------------------------------------------------------------
# Cargo datos observacionales
ar.anom = readRDS("/pikachu/datos4/Obs/t2m_cpc_daily/t2manom_NOAA.rds")
periodo = dimnames(ar.anom)$day

models = c("ESRL","ECCC","EMC","GMAO","RSMAS","NRL","MME")
nmodels = length(models)

for (mod in models) {   # por cada modelo 
  
  model <- readRDS(paste0("./model_",mod,"_OA.rds"))
  target <- readRDS(paste0("./targetdate_",mod,"_OA.rds"))
  
  # Acomodar las observaciones de forma de que se puedan restar con los modelos, teniendo
  # en cuenta los leads
  
  startdates = dimnames(target)$start
  obs_mod = array(NA, dim = dim(model))
  
  for (stdt in 1:length(startdates)) {  # para cada inicio del modelo
    
    # Busco los targetdates
    s = startdates[stdt]    # la fecha de inicio
    tgdt = target[,s]       # las fechas de plazos
    obs_tgdt = ar.anom[,,periodo %in% tgdt]
    
    # Guardo en el array
    obs_mod[,,,stdt] <- obs_tgdt
    
    
  } # end startdates
  
  # Guardo el array generado
  dimnames(obs_mod) <- dimnames(model)
  saveRDS(obs_mod, paste0("./obs_",mod,".rds"))
  
} # end models

# borro para hacer espacio
rm(model,obs_mod,obs_tgdt)


# Ahora realizo la cuenta de los scores para cada modelo

for (m in models) {    # para cada modelo
  
  # Cargo el mod y su observacion
  obs <- readRDS(paste0("./obs_",m,".rds"))
  mod <- readRDS(paste0("./model_",m,"_OA.rds"))
  startdates = dimnames(mod)$start
  
  # hago los calculos
  acc = ACC2(obs,mod, Dim=4)
  
  # RMSE
  dif = (obs-mod)**2
  rmse = sqrt(apply(dif,c(1,2,3),mean))
  
  # ponerle nombres 
  # Guardo
  metric <- list( "acc" = acc,"rmse" = rmse)
  saveRDS(metric,paste0("./metric/metric_",m,".rds"))
  
  
  # # recorto a solo los primeros 28 leads (4 semanas)
  # acc <- acc[,,1:28]
  # 
  # # separo en semanas de 7 dias
  # acc.w <- SepararSemanas(acc)
  
  
}  # End loop model


#--------------------------------------------------------------------------------------------------------
#  G R A F I C O S  (de puerba, dsp pasalos a ext verif)

# Poligonos. Lon de menor a mayor, el primer punto se repite para cerrar el poligono
SP <- data.frame(x_coords = c(293,289,290,293,298,293),
                 y_coords = c(-28,-35,-50,-50,-40,-28))

SACZ <- data.frame(x_coords = c(305,305,312,319,323,305),
                   y_coords = c(-10,-20,-24,-25,-10,-10))
saveRDS(SP, "./poligonos/SP.rds")
saveRDS(SACZ, "./poligonos/SACZ.rds")

# cargo datos
df = data.frame()
for (m in models) {
  mod <- readRDS(paste0("./model_",m,"_OA.rds"))
  metric=readRDS(paste0("./metric/metric_",m,".rds"))
  dimnames(metric$acc) <- dimnames(mod)[1:3]
  dimnames(metric$rmse) <- dimnames(mod)[1:3]
  
  df.model = reshape2::melt(metric)
  # agrego una columna del modelo
  df.model$MODEL = rep(m,nrow(df.model))
  df = rbind(df,df.model)
  
}
# Restringir el data frame al area del poligono (primeras 2 col son lat y lon)
puntossacz=pointsInPolygon(df[,1:2],SACZ) 
puntossp=pointsInPolygon(df[,1:2],SP) 
df_sacz = df[puntossacz,]
df_sp = df[puntossp,]

# Promedio pesado
prom_sacz = DTPromEspacPesado(df_sacz, Variable = "value", Lat= "Y",Grupo=c("L","MODEL","L1"))
prom_sp = DTPromEspacPesado(df_sp, Variable = "value", Lat= "Y",Grupo=c("L","MODEL","L1"))


# junto en un data frame solo
prom_sacz$REGION = rep("SACZ",nrow(prom_sacz))
prom_sp$REGION = rep("SEPG",nrow(prom_sp))
prom = rbind(prom_sacz,prom_sp)
setnames(prom, old = "L",new = "LEADS")

# Cambio el nombre de modelos por factores
# Uso factors para cambiar el orden de los models
prom = FactorsModels(prom, "MODEL")
saveRDS(prom, "./metric/dt_regions.rds")

# Hago un grafico para rmse y otro para acc (x las escalas)
g1 <- GraphLine(DF = prom[L1=="acc"], Label = "ACC", Breaks = seq(0,0.5,0.1), Linetype = "REGION", 
          Color = "MODEL")
g2 <- GraphLine(DF = prom[L1=="rmse"], Label = "RMSE", Breaks = seq(0,5,0.5), Linetype = "REGION", 
                Color = "MODEL")

# JUnto con ggarrange
g <- ggarrange(g1, g2, 
          labels = c("a)", "b)"),
          common.legend = T,
          legend = "right",
          ncol = 2, nrow = 1)


ggsave(filename="./poligonos/scores_poli.png",plot=g,width = 15, height = 10)

# ------------------------ SIGNIFICACIA ----------------------
# Hago el promedio con el rho1 
rho1 <- readRDS("../rho1.rds")

# RHO1 sirve para todas las weeks (1,2.3 y 4). Repito el tamaño de muestra para cada semana
#468 starts of mme (el de menor)
n_eff = 468*((1 - rho1)/(1 + rho1))

t = (acc * sqrt(n_eff - 2)) / sqrt(1-acc^2)

# Significancia de 0.05 y grados de libertad 
critc = qt(p=0.95, df = trunc(n_eff-2))
test = t < critc




df_rho = melt(rho1)
rho_sacz = pointsInPolygon(df_rho[,1:2],SACZ) 
r_sacz = df_rho[rho_sacz,]

rho_sp = pointsInPolygon(df_rho[,1:2],SP) 
r_sp = df_rho[rho_sp,]

DTPromEspacPesado(r_sacz,"value")
r_sacz=as.data.table(r_sacz)
r_sacz = r_sacz[,varlat := value*cos(lat*pi/180)]
r_sacz= r_sacz[,list(media=mean(varlat,na.rm=TRUE))]

r_sp=as.data.table(r_sp)
r_sp = r_sp[,varlat := value*cos(lat*pi/180)]
r_sp= r_sp[,list(media=mean(varlat,na.rm=TRUE))]

# grafico
ggplot(prom_sacz, aes(x = L, y = media, group = model)) +
  geom_line(aes(color = model))
ggplot(prom_sp, aes(x = L, y = media, group = model)) +
  geom_line(aes(color = model))
  
prom2 = prom[model=="MME",]






dimnames(mod) <- list(X = seq(265,330,1),
                      Y = rev(seq(-60,15,1)),
                      L = seq(1,28,1),
                      start = startdates)
saveRDS(mod,"./model_MME_OA.rds")

for(m in models){
  mod <- readRDS(paste0("./model_",m,"_OA.rds"))
  print(dimnames(mod)[1:3])
}
DTPromEspacPesado()


obs <- readRDS(paste0("./obs_",mod,".rds"))
mod <- readRDS(paste0("./model_",mod,"_OA.rds"))
startdates = dimnames(mod)$start

resta = obs-mod
rmse = resta**2


ar =ModelMediaSemanal(Modelo=resta, PronoDate=length(startdates))
acc = ACC(Lon=66, Lat =76,Model = mod, Anom = obs)
dimnames(ar) <- list(dimnames(mod)$X,dimnames(mod)$Y,
                  dimnames(mod)$start,"week" = c("Week 1","Week 2","Week 3","Week 4"))

acc = ACC2(obs,mod,4)
acc_week = ModelMediaSemanal(Modelo=acc, PronoDate=length(startdates))


# gRfico dde prueba 
df = melt(ar[,,1,])
colnames(df) <- c("x","y","week","z")
GraphDiscreteMultiple(Data= df,Label = "",Breaks = seq(-3,3,0.5), Paleta = "YlOrRd",Direccion = 1)





porm = setDT(prom)

ggplot(porm,aes(x = L, y = media, linetype = region,size=model)) +
  geom_line(aes(color = model)) +
  scale_color_manual(name="Models",values = paleta) +
  scale_linetype_manual(name = "Linetype",values = c("solid","dashed")) +
  scale_size_manual(values = c(rep(0.5,6),1),guide = 'none') + # que la ultima linea sea mas gruesa
  
  scale_y_continuous(Label,breaks = seq(0,0.5,0.1), limits = c(0,1)) +
  
  theme(legend.key = element_rect(fill = "transparent")) +
  theme(axis.text=element_text(size=12))+
  theme(strip.text.x = element_text(size = 12, colour = "black"))+
  theme(strip.background = element_rect(color="black", fill="white", size=1.2, linetype="blank"))+
  theme(panel.background = element_rect(fill = "white",colour = "grey70",
                                        size = 2, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey86"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey86")) 
ggarrange(bxp, dp, lp,
          labels = c("A", "B", "C"),
          ncol = 2, nrow = 2)
figure

