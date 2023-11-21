library(tidyverse)
library(sf)
library(sysfonts)
library(showtext)
library(ggtext)
library(osmdata)

font_add_google("Ruda", "title")
font_add_google("Arvo", "body")
showtext_auto()

# = = LOAD CITY DATA = = #
# ~ ~ Bouding Box ~ ~ #
merida_bb <- getbb("Mérida, Yucatán, México")

# ~ ~ Parques ~ ~ #
merida_parks <- merida_bb %>%
  opq() %>%
  add_osm_feature(key = "leisure", value = c("park", "playground", "garden", "track")) %>% # nolint
  osmdata_sf()

# ~ ~ Natural: Green areas ~ ~ #
merida_natural_green_areas <- merida_bb %>%
  opq() %>%
  add_osm_feature(key = "natural", value = c("grassland", "heath", "scrub", "shrubbery", "wood", "tree", "tree_row")) %>% # nolint
  osmdata_sf()

# ~ ~ highway: street data ~ ~ #
merida_big_streets <- merida_bb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", "motorway_link", "primary_link")) %>% # nolint
  osmdata_sf()

merida_med_small_streets <- merida_bb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link",
                            "tertiary_link", "residential", "living_street",
                            "unclassified", "service", "footway")) %>%
  osmdata_sf()

# = = DATAVIS = = #

mapa_merida <- ggplot() +
  # ~ ~ Calles ~ ~ #
  geom_sf(data = merida_big_streets$osm_lines,
          color = "#ADADAD", linewidth = 0.1) +
  geom_sf(data = merida_med_small_streets$osm_lines,
          color = "#ADADAD", linewidth = 0.05) +
    # ~ ~ Areas verdes ~ ~ #
  geom_sf(data = merida_natural_green_areas$osm_polygons,
          fill = "#B6DD5D",
          linewidth = 0) +
  # ~ ~ Parques ~ ~ #
  geom_sf(data = merida_parks$osm_polygons,
          fill = "#76DD5D",
          linewidth = 0) +
  theme_void() +
  labs(title = "#30DayMapChallenge Day 20: Outdoors<br>Las <span style='color:#B6DD5D;'>áreas verdes</span> y <span style='color:#76DD5D;'>parques</span> de Mérida.", # nolint
       subtitle = "¿Dónde están? ¿Hay muchas? ¿Se necesitan más?<br>¿Quiénes tienen más parques y áreas verdes cerca?", # nolint
       caption = "Isaac Arroyo (@unisaacarroyov)<br>Datos: <em>Map data © <b>OpenStreetMap</b> contributors</em>") + # nolint
  theme(
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(color = "#F4EAD5",
                                 family = "title",
                                 size = 70,
                                 hjust = 0.5,
                                 halign = 0.5,
                                 margin = margin(t = 0.3, b = 0.1, unit = "in"),
                                 lineheight = 0.4),
    plot.subtitle = element_textbox(color = "#F4EAD5",
                                    family = "body",
                                    lineheight = 0.2,
                                    hjust = 0.5,
                                    halign = 0.5,
                                    margin = margin(b = -2,
                                                    t = 0.1,
                                                    unit = "in"),
                                    size = 40),
    plot.caption = element_textbox(color = "#F4EAD5",
                                   family = "body",
                                   hjust = 0.5,
                                   halign = 0.5,
                                   margin = margin(t = -1,
                                                   b = 0.2,
                                                   unit = "in"),
                                   lineheight = 0.5,
                                   size = 30)
  )

tgutil::ggpreview(plot = mapa_merida,
                  width = 10, height = 10,
                  bg = "#232323", dpi = 300)

ggsave(plot = mapa_merida, bg = "#232323",
       width = 10, height = 10, unit = "in",
       filename = "./maps/2023_30daymapchallenge_day20_outdoors.png")
