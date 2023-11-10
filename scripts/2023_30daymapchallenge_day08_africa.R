library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)
library(sf)

# = = LOAD DATA = = #
# - - Rhino Pop. - - #
black_rhino <- read_csv("./data/black-rhinos.csv") %>% janitor::clean_names() %>% rename(date_year = year) # nolint
southern_white_rhino <- read_csv("./data/southern-white-rhinos.csv") %>% janitor::clean_names() %>% rename(date_year = year) # nolint

df <- bind_rows(mutate(black_rhino, type_rhino = "Black Rhino") %>% rename(n_rhinos = black_rhino_population_af_rsg_other_sources_2019), # nolint
                mutate(southern_white_rhino, type_rhino = "Southern White Rhino") %>% rename(n_rhinos = southern_white_rhino_population_af_rsg_other_sources_2019)) %>% # nolint
  filter(date_year == 2021)

# - - World - - #
sf_world <- geojsonsf::geojson_sf("./../repo_datos_facil_acceso/Mundo/world_shapes_from_altair_website.geojson") %>% janitor::clean_names() # nolint
sf_africa <- sf_world %>%
  filter(continent == "Africa") %>%
  select(iso_a3)

# = = JOIN DATA WITH GEOMETRIES = = #
sf_rhino_2021 <- left_join(x = df, y = sf_africa, by = join_by(code == iso_a3)) %>% st_as_sf() # nolint


# = = DAVAIS = = #

ggplot() +
  geom_sf(data = sf_africa, fill = "red", color = "blue") +
  geom_sf(data = sf_rhino_2021,
          mapping = aes(fill = n_rhinos)) +
  facet_wrap(vars(type_rhino)) +
  guides(fill = guide_colorbar(title.position = "top", title = "Estimated number", # nolint
                               barwidth = unit(8,"in"), ticks = FALSE)) + # nolint
  labs(title = "#30DayMapChallenge Day 8: Africa",
       subtitle = "The state of the Black and Southern White Rhinos, 2021", # nolint
       caption = "Data: <em>African and Asian Rhino Specialist Groups (AfRSG) & other sources_ via <b>Our World In Data</b><br><em>Isaac Arroyo (@unisaacarroyov)</em>") + # nolint
  theme_void() +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(fill = "gray50", margin = margin(b = 0.05, unit = "in")), # nolint
    plot.subtitle = element_textbox(fill = "gray50", margin = margin(b = 0.15, unit = "in")), # nolint
    plot.caption = element_textbox(fill = "gray50", hjust = 0),
    strip.text = element_text(face = "bold"),
  )


tgutil::ggpreview(width = 11, height = 8, units = "in", bg = "yellow")
