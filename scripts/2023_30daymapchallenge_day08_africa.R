library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtextdb)
library(showtext)
library(sf)


font_add_google("Merriweather", "title")
font_add_google("Mulish", "body")
showtext_auto()


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
p1 <- ggplot() +
  geom_sf(data = sf_africa, fill = "#E6E6E6", color = "#1D1D1D", linewidth = 0.05) + # nolint
  geom_sf(data = sf_rhino_2021,
          mapping = aes(fill = n_rhinos)) +
  facet_wrap(vars(type_rhino)) +
  MoMAColors::scale_fill_moma_c("Exter", n.breaks = 8, labels = scales::label_comma()) + # nolint
  guides(fill = guide_colorbar(title.position = "top", title = "Estimated number of rhinos", # nolint
                               barwidth = unit(8,"in"), ticks = FALSE)) + # nolint
  labs(title = "#30DayMapChallenge Day 8: Africa",
       subtitle = "The state of the Black and Southern White Rhinos, 2021", # nolint
       caption = "<b>Data</b>: <em>African and Asian Rhino Specialist Groups (AfRSG) & other sources</em> via <b>Our World In Data</b><br><em>Isaac Arroyo (@unisaacarroyov)</em>") + # nolint
  theme_void() +
  theme(
    legend.position = "top",
    legend.box.margin = margin(b = 0.1, t = 0.1, unit = "in"),
    legend.title = element_text(face = "bold", family = "body",
                                size = 50,
                                margin = margin(b = -0.25, unit = "in")), # nolint
    legend.text = element_text(family = "body", size = 35,
                               margin = margin(t = -0.23, unit = "in")),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(family = "title", face = "bold", # nolint
                                 margin = margin(b = 0.05, t = 0.1, unit = "in"), # nolint
                                 size = 70), # nolint
    plot.subtitle = element_textbox(family = "body",
                                    margin = margin(b = 0.15, unit = "in"), # nolint
                                    size = 50,
                                    lineheight = 0.3), # nolint
    plot.caption = element_textbox(hjust = 0, family = "body", size = 35, lineheight = 0.3, margin = margin(b = 0.1, unit = "in")), # nolint
    strip.text = element_text(face = "bold", family = "title", size = 50),
  )

# tgutil::ggpreview(p1, width = 11, height = 8, units = "in") # nolint

ggsave(plot = p1, filename = "./maps/2023_30daymapchallenge_day08_africa.png", width = 11, height = 8, units = "in", bg = "#F8FFFF") # nolint
