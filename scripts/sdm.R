library("raster")
library("rgdal")

library("dismo")
library("rJava")

library("plyr")
library("dplyr")
library("tidyr")
library("ggplot2")

# Leyendo datos del SNIB y SNMB:
datos_snib_snmb <- read.csv("../datos/presencias/invasoras_snib_snmb.csv",
  stringsAsFactors = FALSE)
glimpse(datos_snib_snmb)

shape_snib_snmb <- readOGR("../datos/presencias/invasoras_snib_snmb",
  "invasoras_snib_snmb")


# En principio, deberíamos contar con las variables ambientales de la fecha en 
# que se tomó cada observación, para ajustar bien el Maxent. Como ésta es una
# versión 0, y las variables ambientales son, en su mayoría, del 2013, se tomará
# un intervalo alrededor de ese año para las observaciones.

# Las siguientes tablas son especialmente informativas para mostrar los beneficios
# del SNMB

comparacion_snib_snmb_2005_2015 <- datos_snib_snmb %>%
  filter(anio %in% seq(2005, 2015)) %>%
  group_by(nombre_cientifico, esquema) %>%
  tally() %>%
  spread(esquema, n, fill = 0)

comparacion_snib_snmb_2010_2015 <- datos_snib_snmb %>%
  filter(anio %in% seq(2010, 2015)) %>%
  group_by(nombre_cientifico, esquema) %>%
  tally() %>%
  spread(esquema, n, fill = 0)

# Guardando las anteriores tablas en csv para la presentación:
#write.csv(comparacion_snib_snmb_2005_2015,
#  "../insumos_presentacion/comparacion_snib_snmb_2005_2015.csv",
#  row.names = FALSE)

#write.csv(comparacion_snib_snmb_2010_2015,
#  "../insumos_presentacion/comparacion_snib_snmb_2010_2015.csv",
#  row.names = FALSE)

# Graficando año contra número de observaciones SNIB para las especies con
# suficiente número de observaciones en el SNIB (para tener esperanza de que
# sirvan para un SDM)

datos_plot_snib_snmb_2005_2015 <- comparacion_snib_snmb_2005_2015 %>%
  filter(SNIB > 10) %>%
  select(nombre_cientifico) %>%
  inner_join(datos_snib_snmb, by = "nombre_cientifico") %>%
  filter(anio %in% seq(2005, 2015)) %>%
  group_by(nombre_cientifico, anio, esquema) %>%
  tally()

ggplot(data = datos_plot_snib_snmb_2005_2015,
  aes(x = as.integer(anio), y = n, group = esquema, colour = esquema)) +
  geom_point() +
  # Agregar los puntos para que se vean valores únicos (como en el primer plot)
  geom_line() +
  facet_wrap(~nombre_cientifico, scales = "free") +
  scale_x_continuous(breaks = seq(2005, 2015))
  
# Haré el modelo para Melinis repens:

## Preparando los datos para correr un modelo de Maxent:

#1. Paths a los rasters:
paths_covariables <- list.files("../datos/variables_ambientales/todas/",pattern="\\.tif$",
  full.names=TRUE)

#2. Crear un ladrillo con los rasters anteriores:
brik <- brick()
for(i in 1:length(paths_covariables))
{
  aux <- raster(paths_covariables[i])
  brik <- addLayer(brik,aux)
}

#3. revisando proyecciones:
projection(brik) == projection(shape_snib_snmb)

#4. reproyectando shape_snib_snmb a la proyección de brik:
shape_snib_snmb_lcc <- spTransform(shape_snib_snmb, projection(brik))

projection(brik) == projection(shape_snib_snmb_lcc)

#5. detalles finales y modelo para cada especie elegida:
especies_elegidas <- c("Melinis repens")

ajuste_modelos <- lapply(especies_elegidas, function(nombre){
  
  # Ajustando el modelo con las observaciones del SNIB y SNMB
  observaciones_snib_snmb <- shape_snib_snmb_lcc[
    shape_snib_snmb_lcc$nmbr_cn == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2015),]
  
  #valores_covariables <- raster::extract(brik, observaciones)
  
  maxent_snib_snmb <- maxent(brik, observaciones_snib_snmb,
    removeDuplicates = TRUE,
    path = paste0("../resultados_sdm/",
      nombre %>% tolower() %>% gsub("\\s", "_", .),
        "_snib_snmb"),
    args = c(
      "-P",
      "replicates=5",
      "writebackgroundpredictions=true")
    )
  #-P: curvas de respuesta

    #creo que es lo mejor dado el sesgo del SNIB

  # Ajustándolo sólo con las observaciones del SNIB
  observaciones_snib  <- shape_snib_snmb_lcc[
    shape_snib_snmb_lcc$nmbr_cn == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2015) &
    shape_snib_snmb_lcc$esquema == "SNIB",]
  
  maxent_snib <- maxent(brik, observaciones_snib,
    removeDuplicates = TRUE,
    path = paste0("../resultados_sdm/",
      nombre %>% tolower() %>% gsub("\\s", "_", .),
      "_snib"),
    args = c(
      "-P",
      "replicates=5",
      "writebackgroundpredictions=true")
    )

  return(list(maxent_snib, maxent_snib_snmb))
})

# Al revisar los modelos en ../resultados_sdm/.../species.html, se puede ver que
# las variables de importancia difieren enormemente dependiendo si se usan los
# datos del SNIB+SNMB, a si se usan sólo los datos del SNIB. Se utilizará la
# medida de importancia de permutación. 

#A falta de conocimiento experto, se revisarán ambas tablas y se hará una comparación:

#Leyendo la tabla de importancias:

importancias_snib <- read.delim(
  "../resultados_sdm/melinis_repens_snib/tablas_manuales/importancia_variables.txt",
  stringsAsFactors = FALSE) %>%
  arrange(desc(Permutation.importance)) %>%
  mutate(
    ranking = 1:nrow(importancias_snib)
  )

importancias_snib_snmb <- read.delim(
  "../resultados_sdm/melinis_repens_snib_snmb/tablas_manuales/importancia_variables.txt",
  stringsAsFactors = FALSE) %>%
  arrange(desc(Permutation.importance)) %>%
    mutate(
    ranking = 1:nrow(importancias_snib_snmb)
  )

importancias_snib_sub <- importancias_snib %>%
  filter(ranking <= 20)

importancias_snib_snmb_sub <- importancias_snib_snmb %>%
  filter(ranking <= 20)

variables_elegidas <- importancias_snib_sub %>%
  inner_join(importancias_snib_snmb_sub, by = "Variable")

#Obteniendo paths de las variables elegidas:

paths_covariables_elegidas <- data_frame(
  Variable = substr(basename(paths_covariables), 1,
    nchar(basename(paths_covariables))-4), path = paths_covariables) %>%
  inner_join(variables_elegidas) %>%
  .$path

#Corriendo de nuevo Maxent para las variables elegidas:

brik_sub <- brick()
for(i in 1:length(paths_covariables_elegidas))
{
  aux <- raster(paths_covariables_elegidas[i])
  brik_sub <- addLayer(brik_sub,aux)
}

#3. revisando proyecciones:
projection(brik_sub) == projection(shape_snib_snmb)

#4. reproyectando shape_snib_snmb a la proyección de brik:
shape_snib_snmb_lcc <- spTransform(shape_snib_snmb, projection(brik_sub))

projection(brik_sub) == projection(shape_snib_snmb_lcc)

#5. detalles finales y modelo para cada especie elegida:
especies_elegidas <- c("Melinis repens")

ajuste_modelos_2 <- lapply(especies_elegidas, function(nombre){
  
  # Ajustando el modelo con las observaciones del SNIB y SNMB
  observaciones_snib_snmb <- shape_snib_snmb_lcc[
    shape_snib_snmb_lcc$nmbr_cn == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2015),]
  
  maxent_snib_snmb <- maxent(brik_sub, observaciones_snib_snmb,
    removeDuplicates = TRUE,
    path = paste0("../resultados_sdm/",
      nombre %>% tolower() %>% gsub("\\s", "_", .),
        "_snib_snmb_2"),
    args = c(
      "-P",
      "replicates=5",
      "writebackgroundpredictions=true")
    )

  # Ajustándolo sólo con las observaciones del SNIB
  observaciones_snib  <- shape_snib_snmb_lcc[
    shape_snib_snmb_lcc$nmbr_cn == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2015) &
    shape_snib_snmb_lcc$esquema == "SNIB",]
  
  maxent_snib <- maxent(brik_sub, observaciones_snib,
    removeDuplicates = TRUE,
    path = paste0("../resultados_sdm/",
      nombre %>% tolower() %>% gsub("\\s", "_", .),
      "_snib_2"),
    args = c(
      "-P",
      "replicates=5",
      "writebackgroundpredictions=true")
    )

  return(list(maxent_snib, maxent_snib_snmb))
})

# se ven mucho mejor, ahora ajustamos el modelo final (sin validación cruzada),
# para predecir.

ajuste_modelos_3 <- lapply(especies_elegidas, function(nombre){
  
  # Ajustando el modelo con las observaciones del SNIB y SNMB
  observaciones_snib_snmb <- shape_snib_snmb_lcc[
    shape_snib_snmb_lcc$nmbr_cn == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2015),]
  
  maxent_snib_snmb <- maxent(brik_sub, observaciones_snib_snmb,
    removeDuplicates = TRUE,
    path = paste0("../resultados_sdm/",
      nombre %>% tolower() %>% gsub("\\s", "_", .),
        "_snib_snmb_3"),
    args = c(
      "-P",
      "writebackgroundpredictions=true")
    )

  # Ajustándolo sólo con las observaciones del SNIB
  observaciones_snib  <- shape_snib_snmb_lcc[
    shape_snib_snmb_lcc$nmbr_cn == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2015) &
    shape_snib_snmb_lcc$esquema == "SNIB",]
  
  maxent_snib <- maxent(brik_sub, observaciones_snib,
    removeDuplicates = TRUE,
    path = paste0("../resultados_sdm/",
      nombre %>% tolower() %>% gsub("\\s", "_", .),
      "_snib_3"),
    args = c(
      "-P",
      "writebackgroundpredictions=true")
    )

  return(list(maxent_snib, maxent_snib_snmb))
})

# prediciendo con brik_sub
modelo_snib_v0 <- (ajuste_modelos_3[[1]])[[1]]
modelo_snib_snmb_v0 <- (ajuste_modelos_3[[1]])[[2]]

pred_snib_v0 <- predict(modelo_snib_v0, brik_sub,
  filename="../resultados_sdm/predicciones_v0/pred_snib_v0.tif",
  format="GTiff", overwrite=TRUE)

pred_snib_snmb_v0 <- predict(modelo_snib_snmb_v0, brik_sub,
  filename="../resultados_sdm/predicciones_v0/pred_snib_snmb_v0.tif",
  format="GTiff", overwrite=TRUE)

#ploteando resultados:

plot(pred_snib_v0)
points(shape_snib_snmb_lcc[
    shape_snib_snmb_lcc$nmbr_cn == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2015) &
    shape_snib_snmb_lcc$esquema == "SNIB",])

plot(pred_snib_snmb_v0)
points(shape_snib_snmb_lcc[
    shape_snib_snmb_lcc$nmbr_cn == nombre &
    shape_snib_snmb_lcc$anio %in% seq(2010, 2015),])

