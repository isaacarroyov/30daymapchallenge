library(tidyverse)
library(sf)
library(osmdata)
library(ggtext)
library(sysfonts)
library(showtext)

font_add_google("Noto Sans", "body")
font_add_google("Oxygen", "title")
showtext_auto()

isla_mujeres_bb <- getbb("Isla Mujeres, Quintana Roo, Mexico")

# = = Calles = = #
isla_mujeres_streets <- isla_mujeres_bb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>% # nolint
  osmdata_sf()

isla_mujeres_small_streets <- isla_mujeres_bb %>%
  opq() %>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street", "unclassified", "service", "footway")) %>% # nolint
  osmdata_sf()

# - - Buildings - - #
isla_mujeres_small_builing <- isla_mujeres_bb %>%
  opq() %>%
  add_osm_feature(key = "building") %>% # nolint
  osmdata_sf()

# = = other = = #
isla_mujeres_small_other <- isla_mujeres_bb %>%
  opq() %>%
  add_osm_feature(key = "landuse", value = c("greenfield")) %>% # nolint
  osmdata_sf()

# = = Natural = = #
# - - Greens - - #
isla_mujeres_natural_woods <- isla_mujeres_bb %>%
  opq() %>%
  add_osm_feature(key = "natural",
                  value = c("wood")) %>% # nolint
  osmdata_sf()

# - - Water - - #
isla_mujeres_natural_coastline <- isla_mujeres_bb %>%
  opq() %>%
  add_osm_feature(key = "natural",
                  value = c("coastline")) %>% # nolint
  osmdata_sf()

isla_mujeres_natural_water <- isla_mujeres_bb %>%
  opq() %>%
  add_osm_feature(key = "natural",
                  value = c("water")) %>% # nolint
  osmdata_sf()

isla_mujeres_natural_beach <- isla_mujeres_bb %>%
  opq() %>%
  add_osm_feature(key = "natural",
                  value = c("shoal", "beach")) %>% # nolint
  osmdata_sf()

# = = Datavis = = #

mapa <- ggplot() +
  geom_sf(data = isla_mujeres_natural_woods$osm_polygons, fill = "darkgreen") +
  geom_sf(data = isla_mujeres_streets$osm_lines, color = "red") +
  geom_sf(data = isla_mujeres_small_streets$osm_lines, color = "blue") +
  geom_sf(data = isla_mujeres_natural_beach$osm_polygons, fill = "purple") +
  geom_sf(data = isla_mujeres_natural_water$osm_polygons, fill = "yellow") +
  geom_sf(data = isla_mujeres_small_builing$osm_polygons, fill = "gray60") +
  geom_sf(data = isla_mujeres_natural_coastline$osm_lines, color = "black") +
  geom_richtext(
    data = tibble(
      x = c(-86.71),
      y = c(21.266),
      label = c("#30DayMapChallenge Day 30:<br>My favourite . . . island<br><span style='font-size:90pt;'>Isla Mujeres</span>")), # nolint
    mapping = aes(x = x, y = y, label = label),
    hjust = 1,
    vjust = 1,
    family = "title",
    size = 23,
    lineheight = 0.5,
    fill = "transparent",
    label.colour = "transparent",
    text.colour = "black",
    fontface = "bold",
  ) +
  geom_richtext(
    data = tibble(
      x = c(-86.75),
      y = c(21.20),
      label = c("<b>Isaac Arroyo (@unisaacarroyov)</b><br>Map data © OpenStreetMap contributors")), # nolint
    mapping = aes(x = x, y = y, label = label),
    hjust = 0,
    vjust = 0,
    family = "body",
    lineheight = 0.5,
    fill = "transparent",
    label.colour = "transparent",
    text.colour = "black",
    size = 12
  )

tgutil::ggpreview(height = 10, width = 6, units = "in", plot = mapa, bg = "lightblue") # nolint
