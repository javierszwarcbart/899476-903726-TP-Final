# scripts/04_visualizaciones.R
# ----------------------------------------
# Genera y guarda todos los gráficos descriptivos corregidos

# 1) Librerías y rutas
library(here)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(scales)

out <- here("output")
if (!dir.exists(out)) dir.create(out)

# 2) Leer datos limpios y garantizar POSIXct
datos_subte <- read_csv(
  here("input", "datos_subte_completo.csv"),
  show_col_types = FALSE
) %>%
  mutate(fecha = as_datetime(fecha))

# 3) Serie diaria con banda de cuartiles ------------------------------------
diarios <- read_csv(
  here(out, "03_viajes_diarios.csv"),
  show_col_types = FALSE
)

p1 <- ggplot(diarios, aes(x = fecha)) +
  geom_ribbon(aes(ymin = q25_viajes, ymax = q75_viajes),
              fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = total_viajes), color = "steelblue") +
  labs(
    title = "Serie diaria de viajes (2024) con banda de cuartiles",
    x     = "Fecha",
    y     = "Total viajes"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

ggsave(
  filename = "04_serie_diaria_cuartiles.png",
  plot     = p1,
  path     = out,
  width    = 8,
  height   = 4,
  dpi      = 300
)

# 4) Comparativo fin de semana vs. hábiles ----------------------------------
# 4) Comparativo: Lunes–Viernes vs Sábado–Domingo
semana2 <- datos_subte %>%
  mutate(
    dia_semana = wday(fecha, week_start = 1),            # 1 = lunes ... 7 = domingo
    tipo       = if_else(dia_semana <= 5,
                         "Lunes–Viernes",
                         "Sábado–Domingo")
  ) %>%
  group_by(tipo) %>%
  summarise(
    total_viajes = sum(pax_total, na.rm = TRUE),
    .groups      = "drop"
  )

p2 <- ggplot(semana2, aes(x = tipo, y = total_viajes, fill = tipo)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Comparativo: Lunes–Viernes vs Sábado–Domingo",
    x     = "",
    y     = "Total viajes (2024)"
  ) +
  theme_minimal()

ggsave(
  filename = "04_comparativo_semana.png",
  plot     = p2,
  path     = out,
  width    = 6,
  height   = 4,
  dpi      = 300
)


# 5) Barras: viajes por línea -----------------------------------------------
linea <- read_csv(
  here(out, "03_viajes_por_linea.csv"),
  show_col_types = FALSE
)

p3 <- ggplot(linea, aes(x = fct_reorder(linea, total_viajes), y = total_viajes)) +
  geom_col(fill = "coral") +
  coord_flip() +
  labs(
    title = "Total de viajes por línea (2024)",
    x     = "Línea",
    y     = "Total viajes"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

ggsave(
  filename = "04_viajes_por_linea.png",
  plot     = p3,
  path     = out,
  width    = 6,
  height   = 5,
  dpi      = 300
)

# 6) Top 10 estaciones más concurridas --------------------------------------
estacion <- read_csv(
  here(out, "03_ranking_estaciones.csv"),
  show_col_types = FALSE
) %>%
  slice_max(total_viajes, n = 10)

p4 <- ggplot(estacion, aes(x = fct_reorder(estacion, total_viajes), y = total_viajes)) +
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(
    title = "Top 10 estaciones por total de viajes (2024)",
    x     = "Estación",
    y     = "Total viajes"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal()

ggsave(
  filename = "04_top10_estaciones.png",
  plot     = p4,
  path     = out,
  width    = 6,
  height   = 5,
  dpi      = 300
)

# 7) Perfil horario global (horas operativas 5–23) --------------------------
perfil_global <- datos_subte %>%
  mutate(
    datetime = fecha + as.duration(desde),
    hour     = hour(datetime)
  ) %>%
  group_by(hour) %>%
  summarise(total_viajes = sum(pax_total, na.rm = TRUE), .groups = "drop") %>%
  filter(hour >= 5, hour <= 23)

p5 <- ggplot(perfil_global, aes(x = hour, y = total_viajes)) +
  geom_line(color = "purple", group = 1) +
  scale_x_continuous(breaks = 5:23, limits = c(5, 23)) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Perfil horario global de viajes (2024)\nHoras operativas 5–23",
    x     = "Hora del día",
    y     = "Total viajes"
  ) +
  theme_minimal()

ggsave(
  filename = "04_perfil_horario_global.png",
  plot     = p5,
  path     = out,
  width    = 8,
  height   = 4,
  dpi      = 300
)

# 8) Heatmap por línea y hora (horas 5–23) ----------------------------------
perfil_linea_hora <- datos_subte %>%
  mutate(
    datetime = fecha + as.duration(desde),
    hour     = hour(datetime),
    linea    = factor(linea)
  ) %>%
  filter(hour >= 5, hour <= 23) %>%
  group_by(linea, hour) %>%
  summarise(total_viajes = sum(pax_total, na.rm = TRUE), .groups = "drop")

p6 <- ggplot(perfil_linea_hora, aes(x = hour, y = linea, fill = total_viajes)) +
  geom_tile() +
  scale_x_continuous(breaks = 5:23, limits = c(5, 23)) +
  scale_fill_viridis_c(option = "C", labels = comma) +
  labs(
    title = "Heatmap de viajes por hora y línea (horas operativas 5–23)",
    x     = "Hora del día",
    y     = "Línea",
    fill  = "Total viajes"
  ) +
  theme_minimal()

ggsave(
  filename = "04_heatmap_linea_hora.png",
  plot     = p6,
  path     = out,
  width    = 8,
  height   = 5,
  dpi      = 300
)

message("04_visualizaciones.R completado: todos los gráficos guardados en output/")

