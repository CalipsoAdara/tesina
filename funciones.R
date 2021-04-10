# Funciones


# -----------------------------------------------------------------------------------------
# Funcion que obtiene las dimensiones de un archivo netcdf y devuelve un vector con dichas
# dimensiones
DimNc <- function(nc) {
  
  # Obtener los nombres de dichas dimensiones (probablemente lon,lat,time)
  name_dim <- attributes(nc$dim)$names
  
  # Creo vector vacio donde guardar estos datos
  dimension <- vector()
  
  for (i in 1:nc$ndims) {
    nc_dim <- ncvar_get( nc, attributes(nc$dim)$names[i])
    
    dimension[i] <- length(nc_dim)
  
  }  
  print(dimension)
}

# -----------------------------------------------------------------------------------------