library("plyr")
library("dplyr")

# Conexión a la base de datos con dplyr
PASS_SNIB <- Sys.getenv("PASS_SNIB")
base_input <- src_mysql(dbname = "geoportal", host = "172.16.1.139", port = 3306,
   user = "lectura", password = PASS_SNIB)

# Función para ejecutar los queries al SNIB, dados el género y la especie.
ejecutarQuery <- function(genero, especie){
  # Definiendo un query: SELECT * FROM InformacionGeoportal; este es el primer 
  # paso para hacer queries más específicos.
  tabla_input <- tbl(base_input, "InformacionGeoportal")

  # Definiendo un query: SELECT  FROM  WHERE;
  query <- tabla_input %>%
    filter(generoconabio == genero && especieconabio == especie) %>%
    select(longitud, latitud, aniocolecta, mescolecta, generoconabio, 
      especieconabio)
  
  # Ejecutando el query (ésto se llama lazy evaluation, porque el query no se ejecuta
  # inmediatamente)
  datos <- collect(query)
  return(datos)
}

# Lista invasoras SNMB
lista_invasoras <- read.csv("../datos/referencias/lista_invasoras.csv", 
  colClasses = "character")
# falta Tamarix sp.

# Ejecutando el query de invasoras
invasoras_snib <- adply(lista_invasoras, 1,
  function(x) ejecutarQuery(as.character(x[1]), as.character(x[2]))) %>%
  select(-genero, -especie)
glimpse(invasoras_snib)

# Ahora ejecutando el query de Tamarix sp:
tamarix_snib <- tbl(base_input, "InformacionGeoportal") %>%
  filter(generoconabio == "Tamarix") %>%
  select(
    longitud,
    latitud,
    aniocolecta,
    mescolecta,
    generoconabio,
    especieconabio) %>%
  collect()

invasoras_snib <- rbind(invasoras_snib, tamarix_snib) %>%
  mutate(
    nombre_cientifico = paste(generoconabio, especieconabio, sep = " ")
  ) %>%
  select(
    longitud,
    latitud,
    anio = aniocolecta,
    mes = mescolecta,
    nombre_cientifico
    )

num_invasoras <- invasoras_snib %>%
  group_by(nombre_cientifico) %>%
    tally()

# Escribiendo archivo de invasoras:
#write.csv(invasoras_snib, "../datos/presencias/invasoras_snib.csv",
  #row.names = FALSE)
