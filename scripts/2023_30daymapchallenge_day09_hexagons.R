library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtext)
library(sf)

font_add_google("Oswald", "title")
font_add_google("Quattrocento", "body")
showtext_auto()

# = = LOAD DATA = = #
# - - Manzanas de Mérida - - #
sf_merida <- geojsonsf::geojson_sf("./data/manzanas_merida_2022.geojson") %>%
  relocate(geometry, .after = tipomza)

# - - Centroides de las manzanas de Mérida - - #
sf_merida_centroids <- st_centroid(x = sf_merida, of_largest_polygon = TRUE)

# = = DATA VIS = = #
sf_datavis <- sf_merida_centroids %>%
  mutate(long = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  as_tibble() %>%
  select(-geometry)

p1 <- ggplot() +
  geom_hex(data = sf_datavis,
           mapping = aes(x = long, y = lat),
           bins = 180,
           linewidth = 0.1,
           color = "black") +
  MetBrewer::scale_fill_met_c("Tam") +
  guides(fill = guide_colorsteps(title = "Número de\nmanzanas",
                                 title.position = "top")) +
  labs(title = "#30DayMapChallenge Day 9: Hexagons",
       subtitle = "Concentración de manzanas en Mérida, Yucatán, México.<br><br><span style='font-size:30pt'><b>Nota</b>: A cada manzana del municipio de Mérida, se le extrajo su centroide y esos puntos fueron usados para crear un mapa de calor de la concentración de manzanas.</span>", # nolint
       caption = "<b>Datos</b>: Marco Geostadístico de México (2022) a través del <b>INEGI</b><br>Isaac Arroyo (@unisaacarroyov)") + # nolint
  theme_void() +
  theme(
    legend.position = c(0.1, 0.2),
    legend.title = element_text(face = "bold", color = "#FEFAE4",
                                family = "title",
                                size = 50,
                                lineheight = 0.2,
                                margin = margin(b = -0.25, unit = "in"),
                                ), # nolint
    legend.text = element_text(family = "body", color = "#FEFAE4",
                               size = 35,
                               margin = margin(l = -0.25, unit = "in")
                               ),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#04091F"),
    plot.title = element_textbox(family = "title", color = "#FEFAE4",
                                 face = "bold",
                                 margin = margin(b = 0.1, t = 0.1, l = 0.1, unit = "in"), # nolint
                                 size = 70),
    plot.subtitle = element_textbox(family = "body",
                                    color = "#FEFAE4",
                                    width = unit(7, "in"),
                                    margin = margin(l = 0.1, b = 0.15, unit = "in"), # nolint
                                    size = 50,
                                    lineheight = 0.1
                                    ), # nolint
    plot.caption = element_textbox(hjust = 0,
                                   family = "body", color = "#FEFAE4",
                                   size = 35,
                                   lineheight = 0.3,
                                   margin = margin(l = 0.1, b = 0.1, unit = "in") # nolint
                                   )
  )

tgutil::ggpreview(p1, width = 8, height = 11, units = "in")

ggsave(p1, width = 8, height = 11, units = "in",
       filename = "./maps/2023_30daymapchallenge_day09_hexagons.png")
