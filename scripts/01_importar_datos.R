# scripts/01_importar_datos.R

# 1. Carga de librerías
library(tidyverse)    
library(lubridate)    
library(readxl)       

# 2. Definición de rutas
raw_path   <- "raw"
input_path <- "input"

# 3. Listar todos los archivos de datos
files <- list.files(
  path       = raw_path,
  pattern    = "\\.(xlsx|xls|csv)$",
  full.names = TRUE
)

# 4. Función de lectura “inteligente”
read_file <- function(file) {
  ext <- tolower(tools::file_ext(file))
  
  if (ext %in% c("xlsx", "xls")) {
    df <- read_excel(file)
    
  } else if (ext == "csv") {
    # leer CSV con ';', deshabilitando parsing de comillas
    df <- read_delim(
      file            = file,
      delim           = ";",
      quote           = "",
      trim_ws         = TRUE,
      show_col_types  = FALSE,
      locale          = locale(decimal_mark=".", grouping_mark=",")
    ) %>%
      # quitar comillas sobrantes al inicio/final de cada campo
      mutate(across(everything(), ~ str_remove_all(., '^"|"$')))
    
  } 
 
}

# 5. Leer y combinar todos los archivos en un tibble
datos_subte <- files %>%
  set_names() %>%          
  map_dfr(read_file)      

# 6. Parsear FECHA y horas con lubridate
datos_subte <- datos_subte %>%
  mutate(
    FECHA = dmy(FECHA),
    DESDE = hms(DESDE),
    HASTA = hms(HASTA)
  )

# 7. Crear la carpeta input/ si no existe
if (!dir.exists(input_path)) {
  dir.create(input_path)
}

# 8. Guardar el CSV combinado
output_file <- file.path(input_path, "datos_subte_completo.csv")
write_csv(datos_subte, output_file)