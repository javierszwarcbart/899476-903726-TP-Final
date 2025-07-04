# Instalar paquetes en caso de no tenerlos.

# Carga de librerías
library(here)         
library(tidyverse)    
library(lubridate)    
library(readxl)       
library(stringr)      

# Definir rutas
raw_path   <- here("raw")
input_path <- here("input")

# Listar archivos de entrada
files <- list.files(
  path       = raw_path,
  pattern    = "\\.(xlsx|xls|csv)$",
  full.names = TRUE
)

# Función de lectura
read_file <- function(f) {
  ext <- tolower(tools::file_ext(f))
  message("→ Procesando ", basename(f))
  
  if (ext %in% c("xlsx", "xls")) {
    df <- read_excel(f)
    
  } else if (ext == "csv") {
    df <- read_delim(
      file           = f,
      delim          = ";",
      quote          = "",
      trim_ws        = TRUE,
      show_col_types = FALSE,
      locale         = locale(decimal_mark=".", grouping_mark=",")
    ) %>%
      
      mutate(across(everything(), ~ str_remove_all(., '^"|"$')))
  } else {
    stop("Extensión no soportada: ", ext)
  }
  
}

# Leer y unir los archivos
datos_subte <- map_dfr(files, read_file)

# Limpiar nombres
names(datos_subte) <- str_remove_all(names(datos_subte), '^"|"$')

# Eliminar columnas duplicadas por nombre
datos_subte <- datos_subte[, !duplicated(names(datos_subte))]

# Seleccionar 10 columnas válidas
datos_subte <- datos_subte %>%
  select(
    FECHA,
    DESDE,
    HASTA,
    LINEA,
    MOLINETE,
    ESTACION,
    pax_pagos,
    pax_pases_pagos,
    pax_franq,
    pax_TOTAL
  )

# Acomodar Fecha y Hora
datos_subte <- datos_subte %>%
  mutate(
    FECHA = dmy(FECHA),
    DESDE = hms(DESDE),
    HASTA = hms(HASTA)
  )

# Guardar CSV
output_file <- here("input", "datos_subte_completo.csv")
write_csv(datos_subte, output_file)