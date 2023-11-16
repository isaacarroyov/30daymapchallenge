library(tidyverse)
library(sf)
library(sysfonts)
library(showtextdb)
library(showtext)
library(ggtext)
library(osmdata)

# = = LOAD DATA = = #
# - - Chetumal - - #
# ~ ~ Bouding Box ~ ~ #
chetumal_bb <- getbb("Chetumal, Quintana Roo, Mexico")

# ~ ~ Parques ~ ~ #
chetumal_parks <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "leisure", value = c("park", "playground", "garden", "track")) %>% # nolint
  osmdata_sf()

# ~ ~ Natural: Green areas ~ ~ #
chetumal_natural_green_areas <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "natural", value = c("grassland", "heath", "scrub", "shrubbery", "wood")) %>% # nolint
  osmdata_sf()

chetumal_agua <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "natural", value = c("bay", "water", "wetland", "shoal", "beach", "strait", "coastline")) %>% # nolint
  osmdata_sf()

# ~ ~ educación: escuelas y universidades ~ ~ #
chetumal_schools <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = c("school", "university")) %>%
  osmdata_sf()

# ~ ~ highway: street data ~ ~ #
cheutmal_big_streets <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>% # nolint
  osmdata_sf()

chetumal_med_streets <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>% # nolint
  osmdata_sf()

chetumal_small_streets <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street", "unclassified", "service", "footway")) %>% # nolint
  osmdata_sf()

# = = DATAVIS = = #
# - - Google Fonts - - #
font_add_google("Montserrat", "title")
font_add_google("Lato", "body")
showtext_auto()

# - - Map - - #
mapa_chetumal <- ggplot() +
  # ~ ~ Areas verdes ~ ~ #
  geom_sf(data = chetumal_natural_green_areas$osm_polygons,
          fill = "#556B2F",
          color = "black", linewidth = 0) +
  # ~ ~ Calles ~ ~ #
  geom_sf(data = cheutmal_big_streets$osm_lines,
          color = "black", linewidth = 0.5) +
  geom_sf(data = chetumal_med_streets$osm_lines,
          color = "black", linewidth = 0.2) +
  geom_sf(data = chetumal_small_streets$osm_lines,
          color = "black", linewidth = 0.1) +
  # ~ ~ Agua  ~ ~ #
  geom_sf(data = chetumal_agua$osm_lines,
          color = "#4169E1") +
  geom_sf(data = chetumal_agua$osm_polygons,
          fill = "#4169E1",
          color = "#000000", linewidth = 0.2) +
  # ~ ~ Escuelas ~ ~ #
  geom_sf(data = chetumal_schools$osm_polygons,
          fill = "#DAA520",
          color = "black", linewidth = 0) +
  # ~ ~ Parques ~ ~ #
  geom_sf(data = chetumal_parks$osm_polygons,
          fill = "#8B4513",
          color = "black", linewidth = 0) +
  # ~ ~ Title + Annotations ~ ~ #
  labs(title = "#30DayMapChallenge Day 15: OpenStreetMap<br> Lugares de Chetumal, Quintana Roo", # nolint
       subtitle = "<b style='color:#000000;'>Calles</b>, <b style='color:#556B2F;'>Monte</b>, <b style='color:#8B4513;'>Parques</b>, <b style='color:#C3941D;'>Escuelas</b> y <b style='color:#2B58DE;'>Cuerpos de agua</b><br><em style='font-size:30px;'><b>Nota</b>: En los <b style='color:#2B58DE;'>cuerpos de agua</b> se incluye la costa, bordes de cuerpos agua o elementos donde hay presencia significativa de agua)</em>", # nolint
       caption = "<em>Isaac Arroyo (@unisaacarroyov)<br><b>Data</b>: Map data © OpenStreetMap contributors</em>") + # nolint
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FDF5E6", color = "#FDF5E6"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(
      face = "bold",
      color = "#000000",
      size = 65,
      halign = 0.5,
      hjust = 0.5,
      lineheight = 0.4,
      margin = margin(l = 0, t = 0.1, unit = "in"),
      family = "title",
      width = unit(8, "in")),
    plot.subtitle = element_textbox(
      size = 40,
      color = "#000000",
      halign = 0.5,
      hjust = 0.5,
      lineheight = 0.2,
      margin = margin(b = -0.2, t = 0.2, unit = "in"), # nolint
      family = "body",
      width = unit(7, "in")),
    plot.caption = element_textbox(
      size = 30, color = "#000000",
      halign = 0.5,
      hjust = 0.5,
      lineheight = 0.4,
      margin = margin(t = -0.4, unit = "in"),
      family = "body",
      width = unit(8, "in"))
  )

tgutil::ggpreview(plot = mapa_chetumal, width = 9, height = 8, units = "in")

ggsave(plot = mapa_chetumal, width = 9, height = 8, units = "in", dpi = 300,
       filename = "./maps/2023_30daymapchallenge_day15_open-street-map.png")
