library("plyr")
library("dplyr")
library("RMySQL")

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
lista_invasoras <- read.csv("../data/lista_invasoras.csv", 
  colClasses = "character")
# falta Tamarix sp.

# Ejecutando el query de invasoras
invasoras_snib <- adply(lista_invasoras, 1,
  function(x) ejecutarQuery(as.character(x[1]), as.character(x[2]))) %>%
  select(-genero, -especie)

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
  filter(aniocolecta > 2008)

num_invasoras <- invasoras_snib %>%
  group_by(generoconabio, especieconabio) %>%
    summarise(n = n()) 

num_invasoras
# Escribiendo archivo de invasoras:
#write.table(invasoras_snib, "../datos/invasoras_snib.csv", quote = FALSE, sep = ",")
