# scripts/02_limpieza_de_datos.R
# ----------------------------------------
# Toma datos_raw.csv y genera datos_subte_completo.csv con tipos correctos

# 1) Librerías
library(here)
library(tidyverse)
library(lubridate)

# 2) Leer bruto
raw_file    <- here("input", "datos_raw.csv")
datos_raw   <- read_csv(raw_file, show_col_types = FALSE)

# 3) Renombrar columnas clave
datos_subte <- datos_raw %>%
  rename_with(tolower) %>%
  rename(
    fecha      = starts_with("fecha"),
    desde      = starts_with("desde"),
    hasta      = starts_with("hasta"),
    linea      = starts_with("linea"),
    molinete   = starts_with("molinete"),
    estacion   = starts_with("estacion"),
    pax_pagos  = contains("pax_pagos"),
    pax_pases  = contains("pax_pases"),
    pax_franq  = contains("pax_franq"),
    pax_total  = contains("pax_total")
  ) %>%
  select(fecha, desde, hasta, linea, molinete, estacion,
         pax_pagos, pax_pases, pax_franq, pax_total)

# 4) Convertir tipos
datos_subte <- datos_subte %>%
  mutate(
    fecha     = dmy(fecha),
    desde     = hms(desde),
    hasta     = hms(hasta),
    across(starts_with("pax_"), as.numeric)
  )

# 5) Verificar
print(colSums(is.na(datos_subte)))
glimpse(datos_subte)

# 6) Guardar limpio
clean_file <- here("input", "datos_subte_completo.csv")
write_csv(datos_subte, clean_file)
message("datos_subte_completo guardado en: ", clean_file)
