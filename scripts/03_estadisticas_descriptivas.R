# scripts/03_estadisticas_descriptivas.R
# ----------------------------------------
# Cálculo de tablas resumen para usar en las visualizaciones

# 1) Librerías y rutas
library(here)
library(tidyverse)
library(lubridate)

# 2) Asegurar carpeta output/ existe
out <- here("output")
if (!dir.exists(out)) dir.create(out)

# 3) Leer datos limpios
datos_subte <- read_csv(
  here("input", "datos_subte_completo.csv"),
  show_col_types = FALSE
) %>%
  rename_with(tolower)

# 4) Variables derivadas
datos_subte <- datos_subte %>%
  mutate(
    weekday    = wday(fecha, label = TRUE, abbr = TRUE, week_start = 1),
    is_weekend = weekday %in% c("sáb", "dom"),
    hour       = parse_number(desde)  # extrae la hora
  )

# 5) Serie diaria (total, media, mediana, desv, cuantiles)
diarios <- datos_subte %>%
  group_by(fecha) %>%
  summarise(
    total_viajes   = sum(pax_total, na.rm = TRUE),
    media_viajes   = mean(pax_total, na.rm = TRUE),
    mediana_viajes = median(pax_total, na.rm = TRUE),
    sd_viajes      = sd(pax_total, na.rm = TRUE),
    q25_viajes     = quantile(pax_total, .25, na.rm = TRUE),
    q75_viajes     = quantile(pax_total, .75, na.rm = TRUE),
    n_obs          = n(),
    .groups        = "drop"
  )

write_csv(diarios, here(out, "03_viajes_diarios.csv"))


# 6) Comparativo fin de semana vs hábiles
semana <- datos_subte %>%
  group_by(is_weekend) %>%
  summarise(
    total_viajes   = sum(pax_total),
    media_viajes   = mean(pax_total),
    mediana_viajes = median(pax_total),
    sd_viajes      = sd(pax_total),
    .groups        = "drop"
  )

write_csv(semana, here(out, "03_viajes_semana_vs_findesemana.csv"))


# 7) Resumen por línea
linea <- datos_subte %>%
  group_by(linea) %>%
  summarise(
    total_viajes   = sum(pax_total),
    media_viajes   = mean(pax_total),
    mediana_viajes = median(pax_total),
    sd_viajes      = sd(pax_total),
    .groups        = "drop"
  ) %>%
  arrange(desc(total_viajes))

write_csv(linea, here(out, "03_viajes_por_linea.csv"))


# 8) Ranking de estaciones
estacion <- datos_subte %>%
  group_by(estacion) %>%
  summarise(
    total_viajes   = sum(pax_total),
    media_viajes   = mean(pax_total),
    .groups        = "drop"
  ) %>%
  arrange(desc(total_viajes))

write_csv(estacion, here(out, "03_ranking_estaciones.csv"))


# 9) Perfil horario global
horario <- datos_subte %>%
  group_by(hour) %>%
  summarise(
    total_viajes = sum(pax_total),
    media_viajes = mean(pax_total),
    .groups      = "drop"
  )

write_csv(horario, here(out, "03_perfil_horario_global.csv"))


# 10) Perfil horario por línea
horario_linea <- datos_subte %>%
  group_by(linea, hour) %>%
  summarise(
    total_viajes = sum(pax_total),
    .groups      = "drop"
  )

write_csv(horario_linea, here(out, "03_perfil_horario_por_linea.csv"))


# 11) Estadísticas generales de la serie diaria
stats_general <- diarios %>%
  summarise(
    min_viajes   = min(total_viajes),
    max_viajes   = max(total_viajes),
    avg_viajes   = mean(total_viajes),
    med_viajes   = median(total_viajes),
    sd_viajes    = sd(total_viajes)
  )

write_lines(
  paste0(
    "Mínimo diario: ", stats_general$min_viajes, "\n",
    "Máximo diario: ", stats_general$max_viajes, "\n",
    "Media diaria: ", round(stats_general$avg_viajes,1), "\n",
    "Mediana diaria: ", stats_general$med_viajes, "\n",
    "Desv. est. diaria: ", round(stats_general$sd_viajes,1), "\n"
  ),
  here(out, "03_estadisticas_generales.txt")
)

message("03_estadisticas_descriptivas.R completado: se guardaron ",
        "diarios, comparativo semana, línea, estaciones y perfiles horarios.")

