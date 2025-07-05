# scripts/01_extraccion_datos.R
# ----------------------------------------
# Lee todos los archivos raw (CSV, XLS, XLSX) y guarda un CSV combinado

# 1) Librerías
library(here)
library(tidyverse)
library(readxl)
library(janitor)

# 2) Rutas
raw_path   <- here("raw")
input_path <- here("input")

# 3) Crear carpeta input/ si no existe
if (!dir.exists(input_path)) dir.create(input_path)

# 4) Listar archivos
archivos <- list.files(
  path       = raw_path,
  pattern    = "\\.(csv|xls|xlsx)$",
  full.names = TRUE
)

# 5) Función de lectura
read_file <- function(fichero) {
  ext <- tolower(tools::file_ext(fichero))
  message("Procesando: ", basename(fichero))
  
  df <- switch(
    ext,
    "csv"  = read_delim(
      fichero,
      delim          = ";",
      quote          = "",
      trim_ws        = TRUE,
      show_col_types = FALSE,
      locale         = locale(decimal_mark = ".", grouping_mark = ",")
    ),
    "xls"  = read_excel(fichero),
    "xlsx" = read_excel(fichero),
    stop("Extensión no soportada: ", ext)
  )
  
  df %>%
    clean_names() %>%
    mutate(across(everything(), ~ str_remove_all(., '^"|"$')))
}

# 6) Leer y combinar
datos_raw <- map_dfr(archivos, read_file)

# 7) Guardar
output_raw <- file.path(input_path, "datos_raw.csv")
write_csv(datos_raw, output_raw)
message("datos_raw guardado en: ", output_raw)

