library(tidyverse)
library(sf)
library(osmdata)
library(MoMAColors)
library(ggtext)
library(sysfonts)
library(showtext)

font_add_google("Oxygen", "title", regular.wt = 300)
font_add_google("Open Sans", "body")
showtext_auto()

tabasco_bb <- getbb("Tabasco, Mexico")
tabasco_sf <- geojsonsf::geojson_sf("https://raw.githubusercontent.com/isaacarroyov/datos_facil_acceso/main/Gobierno-Mexicano/geometrias/00ent_mexico.geojson") %>% filter(nombre_estado == "Tabasco") # nolint

# = = Rivers = = #
tabasco_rivers <- tabasco_bb %>%
  opq() %>%
  add_osm_features(features = list("water" = "river", "waterway" = "river")) %>%
  osmdata_sf()

sf_tabasco_rivers <- tabasco_rivers$osm_lines

# = = Clipping rivers = = #
sf_tabasco_rivers_clip <- st_intersection(sf_tabasco_rivers, tabasco_sf)

# = = DATAVIS = = #

p1 <- ggplot() +
  geom_sf(data = tabasco_sf,
          color = "#BCA37F",
          fill = "transparent",
          linewidth = 0.3) + # nolint
  geom_sf(data = sf_tabasco_rivers_clip,
          color = "#113946",
          linewidth = 0.5) +
  labs(title = "#30DayMapChallenge Day 26: Minimal",
       subtitle = "Ríos atravesando Tabasco, México",
       caption = "Isaac Arroyo (@unisaacarroyov)<br>Datos: Map data © OpenStreetMap contributors") + # nolint
  theme_void() +
  theme(
    plot.title = element_textbox(hjust = 0.5, halign = 0.5, family = "title", size = 90), # nolint
    plot.subtitle = element_textbox(hjust = 0.5, halign = 0.5, family = "body", size = 30), # nolint
    plot.caption = element_textbox(hjust = 0.5, halign = 0.5, family = "body", size = 30, lineheight = 0.4) # nolint
  )

# tgutil::ggpreview(plot = p1, width = 10, height = 6, bg = "#FEFAF7", units = "in") # nolint

ggsave(plot = p1,
  filename = "./maps/2023_30daymapchallenge_day26_minimal.png",
  width = 10, height = 6, units = "in",
  bg = "#FEFAF7")