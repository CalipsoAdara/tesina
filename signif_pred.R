# Significancia de Predictibilidad
# 
#
# by Lucia M. Castro
#-----------------------------------------------------------------------------------------------------------

# Limpiar enviroment 
rm(list=ls())

# Llamar paquetes


# Cargo mis funciones
source("/home/lucia.castro/tesina/funciones.R")

# Path a donde guardar los archivos
savepath = "/home/lucia.castro/SubX_processed_Rdata/model/viernes/predicc"


setwd("/home/lucia.castro/SubX_processed_Rdata/model/viernes/")

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

#- -----------------------------------------------------------------------------
# P R E D I C T I B I L I D A D 
# Primero calculo la predictibilidad para todo el periodo, con viernes de inicio del ensamble
tgdtMME <- readRDS("./targetdate_MME_OA.rds")
stdtMME <- dimnames(tgdtMME)$startdate

predict = EnsamblesPredictiblidad(Modelos=MODELOS,
                                       TgdtMod=targetdateMODELOS, 
                                       StdtMod=startdateMODELOS, 
                                       FechEnsam = stdtMME,
                                       TgdtEnsam = tgdtMME,
                                       FilePath = savepath)

saveRDS(predict,paste0(savepath,"/predic.rds"))




#-------------------------------------------------------------------------------
# S I G N I F I C A N C I A 
# La significancia de la predictibilidad la hare sobre el promedio final de las correlaciones
# considerando el tamaño de meustra efectivo. El n inicial la cantidad de startdate del mme

predict <- readRDS(paste0(savepath,"/predic.rds"))
predsem = SepararSemanas(predict)  # hace el promedio semanal
n = length(stdtMME)

# Cargo el coeficiente de autocorrelacion (lag=1) calculado semanalmente
rho1 = readRDS("/home/lucia.castro/SubX_processed_Rdata/rho1.rds")

# Tamaño de muestra efectivo
# RHO1 sirve para todas las weeks (1,2.3 y 4). Repito el tamaño de muestra para cada semana
n_eff = n*((1 - rho1)/(1 + rho1))
n_eff = array(n_eff, dim = c(66,76,4))

# Estadistico
t = (predsem * sqrt(n_eff - 2)) / sqrt(1-predsem^2)

# Significancia de 0.05 y grados de libertad 
critc = qt(p=0.95, df = trunc(n_eff-2))
test = t < critc

# -------------------------------------------------------------------------------------------------
# G R A F I C O 
predsem <- predsem*100
ggpred=ggScoreSemanal(predsem)     # convierte a data frame

ggpred = WeeksToSemanas(DF=ggpred,Col ="week")

# Grafico 
g <- GraphMultiplePuntos(Data = ggpred, ArLogic = test, Breaks = seq(0,100,10), Label = "(%)",Paleta = "Greens",Direccion = "1",
                         Lang="en")
#g <- g + ggtitle(paste0("Predictibilidad  \nT2M (99-14, Oct-Abr)"))
g <- g + ggtitle(paste0("Predictability \nT2M (99-14, Oct-Apr)"))

ggsave(filename = paste0(savepath,"/predic_in_sign.png"),plot=g,width = 10, height = 4)
