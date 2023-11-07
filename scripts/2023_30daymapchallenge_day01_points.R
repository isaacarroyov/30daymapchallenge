library(tidyverse)
library(sf)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)

# = = GOOGLE FONTS = = #
title_font <- "Roboto Condensed"
subtitle_font <- "Cabin"

font_add_google(title_font, title_font)
font_add_google(subtitle_font, subtitle_font)
showtext_auto()

# = = LOAD DATA = = #
# Puntos de las estaciones de Metrobus de la CDMX
sf_estaciones_mb <- read_sf("./data/mb_shp/Metrobus_estaciones.shp") %>% janitor::clean_names() # nolint
# Manzanas de la CDMX
sf_manzanas_cdmx <- read_sf("./data/manzanas_cdmx_2022.geojson") %>% janitor::clean_names() # nolint

# = = DATA PROCESSING AND TRANSFORMATION = = #
# - -  Colores y transbordos de los puntos - - #
 sf_estaciones_mb_interes <- sf_estaciones_mb %>%
  mutate(
    color_id = case_when(
      linea == "01" ~ "#B32118",
      linea == "02" ~ "#893893",
      linea == "03" ~ "#78A331",
      linea == "04" ~ "#F68E1C",
      linea == "05" ~ "#06357A",
      linea == "06" ~ "#E83E97",
      linea == "07" ~ "#017337",
      .default = "#222222"
    )
  ) %>%
  select(nombre, linea, tipo, color_id) %>%
  replace_na(list(tipo = "Otro"))

# = = DATAVIS = = #
# Marcar transbordos entre diferentes lineas
vec_estaciones_transbordos_lineas <- sf_estaciones_mb_interes %>% # nolint
  filter(tipo == "Transbordo") %>%
  group_by(nombre) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  distinct(nombre, linea) %>%
  count(nombre, sort = TRUE) %>%
  filter(n > 1) %>%
  pull(nombre)

p1 <- ggplot() +
  geom_sf(
    data = sf_manzanas_cdmx,
    mapping = aes(fill = tipomza),
    linewidth = 0, color = "white", alpha = 1) +
  geom_sf(
    data = sf_estaciones_mb_interes,
    mapping = aes(
      color = color_id,
      shape = if_else(nombre %in% vec_estaciones_transbordos_lineas, 17, 16), # nolint
      size = if_else(nombre %in% vec_estaciones_transbordos_lineas == "Transbordo", 3, 1)), # nolint
  ) +
  scale_color_identity() +
  scale_shape_identity() +
  scale_size_identity() +
  scale_fill_manual(
    values = MoMAColors::moma.colors("Picabia", n = 45, direction = 1)[35:45]) +
  labs(title = "#30DayMapChallenge. Day 1: Points",
       subtitle = "Estaciones _(círculos)_ y Transbordos _(triángulos)_ del Metrobús de la CDMX", # nolint
       caption = '<strong>Datos:</strong> <em>Ubicación de líneas y estaciones del Metrobús</em> a través del <strong>Portal de Datos Abiertos de la Ciudad de México</strong>. <em>Marco Geoestadístico de México (2022)</em> a través del <strong>INEGI</strong><br><br><em>Isaac Arroyo (@unisaacarroyov)</em>') + # nolint
  theme_void() +
  theme(
    plot.background = element_rect(color = "#222222", fill = "#222222"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(color = "white",
                                 width = unit(8, "in"),
                                 size = 80,
                                 family = title_font, face = "bold"),
    plot.subtitle = element_textbox(colour = "white",
                                    width = unit(8, "in"), size = 40,
                                    family = subtitle_font),
    plot.caption = element_textbox(color = "white",
                                   width = unit(8, "in"),
                                   margin = margin(t = -0.75, unit = "in"),
                                   size = 30, lineheight = 0.3,
                                   family = subtitle_font,
                                   hjust = 0)
  )

tgutil::ggpreview(p1, width = 8, height = 10, units = "in", dpi = 300)
ggsave(p1, width = 8, height = 10, units = "in", dpi = 300,
       filename = "./maps/2023_30daymapchallenge_day01_points.png")
