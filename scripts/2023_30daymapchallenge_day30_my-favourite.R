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
  geom_sf(data = isla_mujeres_natural_woods$osm_polygons, fill = "#9FBB73", linewidth = 0) + # nolint - greens
  geom_sf(data = isla_mujeres_streets$osm_lines, color = "white", linewidth = 0.3) + # nolint - calle 01
  geom_sf(data = isla_mujeres_small_streets$osm_lines, color = "white", linewidth = 0.1) + # nolint - calle 02
  geom_sf(data = isla_mujeres_natural_beach$osm_polygons, fill = "#FFEA36", linewidth = 0) + # nolint - beaches
  geom_sf(data = isla_mujeres_natural_water$osm_polygons, fill = "#427D9D", linewidth = 0) + # nolint - in_water
  geom_sf(data = isla_mujeres_natural_coastline$osm_lines, color = "#DDF2FD", linewidth = 0.5) + # nolint - water
  geom_richtext(
    data = tibble(
      x = c(-86.71),
      y = c(21.266),
      label = c("#30DayMapChallenge Day 30:<br>My favourite . . . island<br><span style='font-size:65pt;'>Isla Mujeres, Quintana Roo</span>")), # nolint
    mapping = aes(x = x, y = y, label = label),
    hjust = 1,
    vjust = 1,
    family = "title",
    size = 18,
    lineheight = 0.5,
    fill = "transparent",
    label.colour = "transparent",
    text.colour = "#FBFAE3",
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
    text.colour = "#FBFAE3",
    size = 12
  ) +
  theme_void()

tgutil::ggpreview(height = 9, width = 6, units = "in", plot = mapa, bg = "#164863") # nolint

ggsave(
  height = 9, width = 6, units = "in", plot = mapa, bg = "#164863",
  filename = "./maps/2023_30daymapchallenge_day30_my-favourite.png")
