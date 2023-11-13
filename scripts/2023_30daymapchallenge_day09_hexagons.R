library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtext)
library(sf)

# = = LOAD DATA = = #
# - - Manzanas de Mérida - - #
sf_merida <- geojsonsf::geojson_sf("./data/manzanas_merida_2022.geojson") %>%
  relocate(geometry, .after = tipomza)

# - - Centroides de las manzanas de Mérida - - #
sf_merida_centroids <- st_centroid(x = sf_merida, of_largest_polygon = TRUE)

# = = DATA VIS = = #
sf_merida_centroids %>%
  mutate(long = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  as_tibble() %>%
  select(-geometry) %>%
  ggplot(aes(long, lat)) +
  geom_hex(bins = 200)
