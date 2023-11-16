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

# ~ ~ Landuse: cualquier cosa que sea green y algo recreativo ~ ~ #
chetumal_parks <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "leisure", value = c("park", "playground", "garden", "track")) %>% # nolint
  osmdata_sf()

chetumal_landuse_green_areas <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "landuse", value = c("greenfield", "recreation_ground", "grass", "village_green")) %>% # nolint
  osmdata_sf()

chetumal_agua <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "natural", value = c("bay", "water", "wetland", "shoal", "beach", "strait", "coastline")) %>% # nolint
  osmdata_sf()

# ~ ~ educación: escuelas y universidades ~ ~ #
chetumal_schools <- chetumal_bb %>%
  opq() %>%
  add_osm_feature(key = "amenity", value = c("school", "unversity")) %>%
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


# - - Map - - #
ggplot() +
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
          fill = "#4169E1", color = "#4169E1") +
  # ~ ~ Escuelas ~ ~ #
  geom_sf(data = chetumal_schools$osm_polygons,
          fill = "#DAA520",
          color = "black", linewidth = 0) +
  # ~ ~ Parques ~ ~ #
  geom_sf(data = chetumal_parks$osm_polygons,
          fill = "#8B4513",
          color = "black", linewidth = 0) +
  # ~ ~ Areas verdes ~ ~ #
  geom_sf(data = chetumal_landuse_green_areas$osm_polygons,
          fill = "#556B2F",
          color = "black", linewidth = 0) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#FDF5E6", color = "#FDF5E6")
  )

tgutil::ggpreview(width = 11, height = 8, units = "in")
