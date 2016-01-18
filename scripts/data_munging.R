library("raster")
library("rgdal")

library("plyr")
library("dplyr")
library("tidyr")

# Leyendo csv's:

invasoras_snib <- read.csv("../datos/presencias/invasoras_snib.csv",
  stringsAsFactors = FALSE) %>%
  mutate(
      esquema = "SNIB"
  )
glimpse(invasoras_snib)

invasoras_snmb <- read.csv("../datos/presencias/invasoras_snmb.csv",
  stringsAsFactors = FALSE) %>%
  mutate(
      esquema = "SNMB"
  )
glimpse(invasoras_snmb)

## Revisando datos:

# Leyendo la lista de invasoras del SNMB, con nombres estandarizados
lista_invasoras_snmb <- read.csv("../datos/referencias/lista_invasoras.csv") %>%
  transmute(
    nombre_cientifico = paste(genero, especie, sep = " ")
  ) %>%
  .$nombre_cientifico

# Viendo cuáles especies invasoras del SNIB no están en la lista de invasoras del SNMB:
nombres_invasoras_snib <- unique(invasoras_snib$nombre_cientifico)
nombres_invasoras_snib[!(nombres_invasoras_snib %in% lista_invasoras_snmb)]
# Las que fallan son las Tamarix, que no tiene problema porque éstas no se incluyeron
# en la lista (en el cliente se especifica Tamarix sp)

# Viendo cuáles especies invasoras del SNMB no están en la lista de invasoras del SNMB
# ie. viendo cuáles nombres faltan por estandarizar:

nombres_invasoras_snmb <- unique(invasoras_snmb$nombre_cientifico)
nombres_invasoras_snmb[!(nombres_invasoras_snmb %in% lista_invasoras_snmb)]
# Fallan:
# Cyperus papyrus: hay que quitarle lo de (ANP)
# Rottboellia cochinchinensis: hay una con un espacio de más.
# Salsola sp. separarla en sus dos especies.

#Arreglándola:
invasoras_snmb_correccion <- invasoras_snmb %>%
  mutate(
    nombre_cientifico = ifelse(nombre_cientifico == "Cyperus papyrus (ANP)",
      "Cyperus papyrus", nombre_cientifico),
    nombre_cientifico = ifelse(nombre_cientifico == "Rottboellia cochinchinensis ",
      "Rottboellia cochinchinensis", nombre_cientifico),
    nombre_cientifico = ifelse(nombre_cientifico == "Salsola sp. (vermiculata o tragus)",
      "Salsola tragus", nombre_cientifico)
  )

# Revisando:
unique(invasoras_snmb_correccion$nombre_cientifico) %in% lista_invasoras_snmb

# Haciendo un único data_frame:

invasoras_snib_snmb <- rbind(invasoras_snmb_correccion, invasoras_snib)
glimpse(invasoras_snib_snmb)

# Ahora se tomará un subconjunto de "invasoras_snib_snmb", para tomar únicamente los
# puntos que caen en México, para ello, se espacializará el data frame y 
# se cortará más fácilmente.

coordinates(invasoras_snib_snmb) <-~ longitud + latitud
raster_lat_lon <- raster("../datos/referencias/lat_lon.tif")
#plot(raster_lat_lon)
projection(invasoras_snib_snmb) <- projection(raster_lat_lon)

## Recortando puntos fuera de México: la idea es primero extraer los valores del
# raster de México que ocurren en las coordenadas de cada especie invasora, y
# eliminar las que sacan NA.
val <- raster::extract(raster_lat_lon, invasoras_snib_snmb)
#head(val)
#sum(is.na(val))

invasoras_snib_snmb <- invasoras_snib_snmb %>%
  as.data.frame() %>%
  .[!is.na(val),]

# Espacializando de nuevo para guardar como shape-file (ésto se hace porque la
# columna "nombre_cientifico" tiene un nombre muy largo, por lo que hay que editarlo):

#coordinates(invasoras_snib_snmb) <-~ longitud + latitud
#raster_lat_lon <- raster("../datos/referencias/lat_lon.tif")
#projection(invasoras_snib_snmb) <- projection(raster_lat_lon)

#writeOGR(invasoras_snib_snmb,
#  "../datos/presencias/invasoras_snib_snmb",
#  "invasoras_snib_snmb", driver="ESRI Shapefile")

#Guardando la tabla final:
#write.csv(invasoras_snib_snmb, "../datos/presencias/invasoras_snib_snmb.csv", row.names= FALSE)


