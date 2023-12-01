library(tidyverse)
library(sf)
library(osmdata)
library(ggtext)
library(sysfonts)
library(showtext)

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
  geom_sf(data = isla_mujeres_natural_coastline$osm_lines, color = "black")

tgutil::ggpreview(height = 10, width = 6, units = "in", plot = mapa, bg = "lightblue") # nolint
