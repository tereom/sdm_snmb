 # Código que se corre una vez:

library("shiny")
library("rgdal")
library("ggplot2")
library("ggmap")
#library("gpclib")
library("plyr")
library("dplyr")

# Licencia de gpclib:
#gpclibPermit()

# Leyendo datos y lista de invasoras
invasoras_snib_snmb <- read.csv("../datos/presencias/invasoras_snib_snmb.csv",
  stringsAsFactors = FALSE) %>%
  mutate(
    # Lo necesitamos para que no cambie de color el mapa cada vez que se recalcula
    esquema = as.factor(esquema)
  ) %>%
  filter(
    anio < 9999
  )

# Creando las categorías para el slider de años:
#anios <- unique(invasoras_snib_snmb$anio)
anio_min <- 2000
anio_max <- 2016
anio_actual <- 2015

# Creando las opciones para la combobox de "nombre científico", para ello,
# primero se creará una tabla que indique cuantas observaciones se encontraron
# (en ambos esquemas), de cada especie, y se filtrarán las que no han sido vistas

observaciones_especie <- invasoras_snib_snmb %>%
  filter(anio %in% seq(anio_min, anio_max)) %>%
  group_by(nombre_cientifico) %>%
  tally()

# Creando el nombre científico para la combobox, de la misma manera que en
# "data_munging.R"
nombres_invasoras <- read.csv("../datos/referencias/lista_invasoras.csv",
  stringsAsFactors = FALSE) %>%
  transmute(
    nombre_cientifico = paste(genero, especie, sep = " ")
  ) %>%
  inner_join(
    observaciones_especie
  ) %>%
  filter(n > 0) %>%
  .$nombre_cientifico


# Información del shape file de estados
#ogrInfo(dsn = "../datos/referencias/mex_edos", layer = "mex_edos")

# Leyendo el shape file de estados:
edos_sf <- readOGR("../datos/referencias/mex_edos", "mex_edos")

# Convirtiendo edos_sf en data frame para plotearlo con ggplot, agregando "NOM_ENT"
#como id.
edos_df <- fortify(edos_sf, region = "NOM_ENT")

# Ploteando los estados (que son comunes, independientemente de lo que seleccione
# el usuario)
plot_edos <- ggplot() +
  geom_polygon(data = edos_df, aes(long, lat, group = group), colour = 'darkgrey', fill = 'white') +
  coord_map(projection = "mercator") +
  theme_nothing(legend = TRUE)

shinyServer(function(input, output) {
  # Cada vez que se cambie un parámetro en la ui lo siguiente se recalcula:
  output$mapa_invasoras <- renderPlot({
    
    # Filtrando tabla de invasoras por los parámetros que eligió el usuario
    invasoras_seleccionadas <- invasoras_snib_snmb %>%
      filter(
        nombre_cientifico == input$invasora_seleccionada,
        anio >= input$anio_seleccionado)
    
      # Ploteando 
      plot_edos <- plot_edos + geom_point(
        data = invasoras_seleccionadas,
        aes(x = longitud, y = latitud, colour = esquema)
        ) +
        scale_colour_discrete(drop=TRUE,
          limits = levels(invasoras_seleccionadas$esquema))

    plot_edos
  })

})

