# Funciones


# Retrieve the latitude and longitude values.
attributes(nc$dim)$names
## [1] "lat" "lon"
nc_lat <- ncvar_get( nc, attributes(nc$dim)$names[1])
nc_lon <- ncvar_get( nc, attributes(nc$dim)$names[2])