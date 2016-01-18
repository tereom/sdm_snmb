library("raster")
library("rgdal")
library("plyr")
library("tidyr")
library("dplyr")
library("lubridate")

# Conexión a la base de datos SNMB
base_snmb_ruta <- "../datos/bases/snmb_2015_08_05.sqlite"
base_snmb_input <- src_sqlite(base_snmb_ruta)

# Obteniendo las tablas necesarias de la base de datos del SNMB:

Conglomerado_muestra <- collect(tbl(base_snmb_input, "Conglomerado_muestra")) %>%
  mutate(
    nombre = as.integer(nombre)
  )
Sitio_muestra <- collect(tbl(base_snmb_input, "Sitio_muestra"))

Transecto_especies_invasoras_muestra <- collect(tbl(base_snmb_input, "Transecto_especies_invasoras_muestra"))
Especie_invasora <- collect(tbl(base_snmb_input, "Especie_invasora"))

Especie_invasora_extra <- collect(tbl(base_snmb_input, "Especie_invasora_extra"))

# Código para asignar a cada conglomerado las coordenadas aceptadas por Julián,
# y luego asignar la mediana de las coordenadas de cada sitio a los conglomerados
# que no están en dicha lista.

coordenadas_lcc <- read.csv('../datos/referencias/congs_coords.csv')
glimpse(coordenadas_lcc)

# A los conglomerados que se encuentran en la lista de julián, asignarles las 
# coordenadas especificadas ahí.

datos_conglomerado_coords_lcc <- Conglomerado_muestra %>%
  inner_join(coordenadas_lcc, by = c("nombre" = "Cgl")) %>%
  dplyr::select(
    conglomerado_muestra_id = id,
    nombre,
    fecha_visita,
    estado,
    x,
    y
  ) %>%
  # para poder espacializar correctamente
  as.data.frame()
glimpse(datos_conglomerado_coords_lcc)

# Espacializando el df para reproyectar las coordenadas xy. Es importante fijarse
# en el orden: x + y (o longitud + latitud).

coordinates(datos_conglomerado_coords_lcc) <- ~ x + y

raster_lcc <- raster("../datos/referencias/lcc.tif")
proj4string(datos_conglomerado_coords_lcc) <- projection(raster_lcc)

# Reproyectando:
raster_lat_lon <- raster("../datos/referencias/lat_lon.tif")
datos_conglomerado_coords_lat_lon <- spTransform(datos_conglomerado_coords_lcc,
  projection(raster_lat_lon)) %>%
  as.data.frame() %>%
  mutate(
    longitud = x,
    latitud = y
  ) %>%
  select(
    -x,
    -y
  )
glimpse(datos_conglomerado_coords_lat_lon)

# A los conglomerados que no están en la lista, asignarles la mediana de las
# coordenadas de sus sitios.

datos_conglomerado_coords_mediana <- Conglomerado_muestra %>%
  anti_join(coordenadas_lcc, by = c("nombre" = "Cgl")) %>%
  inner_join(Sitio_muestra, by = c("id" = "conglomerado_muestra_id")) %>%
  mutate(
    #lat_grado siempre es positiva
    latitud = lat_grado + lat_min/60 + lat_seg/3600,
    longitud = ifelse(lon_grado >= 0,
      -(lon_grado + lon_min/60 + lon_seg/3600),
      lon_grado - lon_min/60 - lon_seg/3600)
  ) %>%
  dplyr::select(
    conglomerado_muestra_id = id,
    nombre,
    fecha_visita,
    estado,
    elipsoide,
    latitud,
    longitud
    ) %>%
  group_by(conglomerado_muestra_id) %>%
    summarize(
      nombre = first(nombre),
      fecha_visita = first(fecha_visita),
      estado = first(estado),
      longitud = median(longitud, na.rm = TRUE),
      latitud = median(latitud, na.rm = TRUE)
    )
glimpse(datos_conglomerado_coords_mediana)

# Creando tabla final de coordenadas SNMB:
datos_conglomerado = rbind(datos_conglomerado_coords_lat_lon,
  datos_conglomerado_coords_mediana) %>%
  as.data.frame()
glimpse(datos_conglomerado)

#coordinates(datos_conglomerado) <- ~ longitud + latitud
#proj4string(datos_conglomerado) <- projection(raster_lat_lon)
#nrow(datos_conglomerado) == nrow(Conglomerado_muestra)
#plot(raster_lat_lon)
#points(datos_conglomerado, col="red", pch=1, bg="red")

# Obteniendo especies invasoras asociadas a cada conglomerado, para asociarles
# coordenadas y fecha:

invasoras_muestreo_snmb <- datos_conglomerado %>%
  inner_join(Transecto_especies_invasoras_muestra, by = "conglomerado_muestra_id") %>%
  select(
    nombre,
    fecha_visita,
    estado,
    longitud,
    latitud,
    transecto_especies_invasoras_id = id
  ) %>%
  inner_join(Especie_invasora, by = "transecto_especies_invasoras_id") %>%
  select(
    nombre,
    fecha_visita,
    estado,
    latitud,
    longitud,
    nombre_en_lista,
    nombre_cientifico
    ) %>%
  filter(nombre_en_lista == "T")

invasoras_extra_snmb <- datos_conglomerado %>%
  inner_join(Especie_invasora_extra, by = "conglomerado_muestra_id") %>%
  select(
    nombre,
    fecha_visita,
    estado,
    latitud,
    longitud,
    nombre_en_lista,
    nombre_cientifico
  ) %>%
  filter(nombre_en_lista == "T")

# Creando tabla de invasoras SNMB

invasoras_snmb <- rbind(invasoras_muestreo_snmb, invasoras_extra_snmb) %>%
  mutate(
    anio = year(fecha_visita),
    mes = month(fecha_visita)
  ) %>%
  select(
    longitud,
    latitud,
    anio,
    mes,
    nombre_cientifico
    )
glimpse(invasoras_snmb)

#write.csv(invasoras_snmb, "../datos/presencias/invasoras_snmb.csv",
#  row.names = FALSE)
