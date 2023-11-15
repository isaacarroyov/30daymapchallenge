library(tidyverse)
library(ggtext)
library(MoMAColors)
library(sysfonts)
library(showtextdb)
library(showtext)
library(sf)
sf_use_s2(FALSE)

# = = LOAD DATA = = #
uso_suelo_s1 <- st_read("path/to/usv_serie1_gcs_JydS1Ls.shp") # nolint

sf_ent_yuc <- geojsonsf::geojson_sf("https://raw.githubusercontent.com/isaacarroyov/datos_facil_acceso/main/Gobierno-Mexicano/geometrias/00ent_mexico.geojson") %>% filter(cve_geo == "31") # nolint

# = = DATA PROCESSING 00 = = #
# - - Dar ID a los elementos del sfc - - #
uso_suelo_s1 <- mutate(uso_suelo_s1, id_row = 1:nrow(uso_suelo_s1))

# - - Crear copia de centroides - - #
uso_suelo_s1_centroids <- st_centroid(uso_suelo_s1, of_largest_polygon = TRUE)

# - - Encontrar los centroides que se encuentran dentro de Yucatán - - #
uso_suelo_s1_centroids_in_yuc <- st_filter(x = uso_suelo_s1_centroids, y = sf_ent_yuc, .predicate = st_within) # nolint
vec_ids_uso_suelo_s1_in_yuc <- uso_suelo_s1_centroids_in_yuc %>% as_tibble() %>% pull(id_row) # nolint

# - - Filtrar a los centroides "yucatecos" y guardar como GeoJSON - - #
uso_suelo_s1_in_yuc <- filter(.data = uso_suelo_s1, id_row %in% vec_ids_uso_suelo_s1_in_yuc) # nolint

uso_suelo_s1_in_yuc %>%
  select(-id_row) %>%
  st_write("./data/usv_series_01_yucatan.geojson", driver = "GeoJSON")
