t<-dt.anom[1:10]
u<-as.data.table(dt.verif[1:10,])
dt.verif=merge(u,t,by=c("lat","lon","targetdate"))

# Ordeno un poco para que coincidan las columnas
dt.anom = dt.anom[order(lat)]
dt.anom = dt.anom[order(match(targetdate, dt.model$targetdate))]






t<-dt.anom[order(lat)][1:10]
u<-as.data.table(dt.verif[1:10,])
dt.anom[order(lat)]
merge(data1, data2, by = "id", all.x = TRUE)  
dt.verif=merge(u,t,by.x = c("X","Y","targetdate"),by.y=c("lat","lon","targetdate"))



h<- data.frame(lat = c(-100:99),
               lon = c(1:200),
               sarasa = runif(200))

j<- data.frame(X = rep(-100:99,3),
               Y = rep(1:200,3),
               sarasa = runif(600))
GH <- merge(h,j,by.x = c("lat","lon"),by.y = c("X","Y"))




#BUENO INVENTO MI DT.VERIF
dt.verif = data.table(X= rep(seq(265,330,1),6840),
                      Y= rep(seq(-60,15,1),5940),
                      L= rep(seq(1,45,1),10032),
                      startdate=rep("1999-01-01",451440),
                      tasaem = runif(451440),
                      targetdate = rep(c("1999-01-02","1999-01-03"),225720),
                      #week = rep(seq(1,4,1),112860),
                      anom = unique(dt.anom$anom)[1:451440])

dt.verif= dt.verif[order(Y)]
dt.verif= dt.verif[order(L)]
saveRDS(dt.verif,file = "probando.rds")
subset = dt.anom[1:10032] #agarro dos dias 
long = unique(subset$lon)
lati= unique(subset$lat)
tiempito = unique(subset$targetdate)

anom_array_2 <- array(subset$anom, dim = c(length(long), length(lati), length(tiempito)))
dimnames(anom_array_2) <- list("lon" = long , "lat" = lati , "day" = as.character(tiempito))








########################################################
ar.model.w1
mean(ar.model)
#media en la tercera dimension

model_media_semanal = apply(ar.model.w1, c(1,2,4), FUN = mean)
ar.anom.2 = array(NA, dim = c(66,76,28,1))
targetdate3 = targetdate2[1:28,]
targetdate2
g= as.vector(targetdate[1:7,1])
g
k = dimnames(ar.anom)$day  
targetweek = as.character(dimnames(ar.anom)$day) == as.character( g )


ar.anom.w1 = ar.anom[,,which()]
str_detect(k,g)
which(k %in% g)
targetweek = ar.anom[,,which(k %in% g)]
anom_media_semanal = apply(targetweek, c(1,2), FUN = mean)


anom_media_semanal <- array(NA, dim = c(66,76,1241))
days = dimnames(ar.anom)$day 
# Hago un ciclo para obtener un campo medio de las anom observadas segun la semana 
for (startdate in (1:1241) ) {
  
  
  week1 = as.vector(targetdate[1:7, startdate])
  
  targetweek = ar.anom[,,which(days %in% week1)]
  media_semanal = apply(targetweek, c(1,2), FUN = mean, na.rm = TRUE)
  anom_media_semanal[,,startdate] <- media_semanal
}

h = (anom_media_semanal - model_media_semanal)
rmse = (h)^2

AVER =apply(rmse, c(1,2), mean, na.rm = FALSE)
AVER = sqrt(AVER)

acc=cor(anom_media_semanal[,,1], model_media_semanal[,,1] , use = "pairwise.complete.obs",method = "pearson")

x <- as.numeric(dimnames(ar.anom)$lon)
y <- as.numeric(dimnames(ar.anom)$lat)
valores <- as.vector(model_media_semanal[,,1,1])
valores <- as.vector(h[,,1,1])
valores <- as.vector(AVER)
valores <- as.vector(targetweek[,,1])
valores <- as.vector(media_semanal[,,1])
valores <- as.vector(ar.anom[,,1])
valores <- as.vector(t2m_sa_years[,,1])
valores <- as.vector(dif[,,1,1])

# Creo los data frames necesarios
# En la primer columna tiene todas las long repetidas la cantidad de veces de las latitudes
# En la segunda columna tiene todas las lat repetidas la cantidad de veces de las longitudes
# En la tercera columna tiene los valores de media mensual para 1 mes en particular
data_temp<-data.frame(x=rep(x,length(y)),
                      y=rep(y,each=length(x)),
                      z=valores)
maximo = max(valores, na.rm = T)
minimo = min(valores, na.rm = T)

breaks = seq(minimo,maximo,1)
GraphDiscrete(data_temp, Breaks = breaks, Titulo = "A VER QUE SALE", Paleta = "RdBu",Label = "media") 
breaks = seq(-1,1,0.2)
GraphDiscrete(data, Breaks = breaks, Titulo = "A VER QUE SALE", Paleta = "RdBu",Label = "media") 
GraphDiscrete2()


# Coeficiente de correlacion para cada punto de lat y lon 
anom_media_semanal[1,1,]
acc <- apply(anom_media_semanal, c(1,2), FUN = cor, model_media_semanal)
acc <- array(NA, dim = c(66,76))

for (lon in 1:66) {
  for (lat in 1:76) {
    
    observ <- anom_media_semanal[lon,lat,]
    modelo <- model_media_semanal[lon,lat,]
    
    coef_corr <- cor(observ,modelo,use="pairwise.complete.obs",method = "pearson")
    
    acc[lon,lat] <- coef_corr
    
  }
  
}

# Calculo del rcritco para testear con 95% de confianza

rc <- 2.33/sqrt(612-2)

# Pruebo colocar datos en array a partir de data.table
ar <- array(1:72,dim=c(72,1))
dimnames(ar) <- list("lon","lat")
dt<- data.table(ar)

ar2<- array(dt$V1,dim = c(12,2,3))
