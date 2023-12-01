library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtext)
library(MoMAColors)
library(sf)
sf_use_s2(FALSE)

font_add_google("Ubuntu", "title")
font_add_google("Kreon", "body")
showtext_auto()

# = = LOAD DATA = = #
# - - Geometrias - - #
sf_mza_merida_mun <- geojsonsf::geojson_sf("./data/manzanas_merida_2022.geojson") %>% relocate(geometry, .after = tipomza) # nolint

# - - Info de manzanas - - #
df_merida_mun <- read_csv("./data/RESAGEBURB_31CSV20.csv") %>%
  janitor::clean_names() %>%
  select(entidad, mun, loc, ageb, mza, nom_mun, nom_loc, pobtot, pob65_mas) %>%
  filter(nom_mun == "Mérida") %>%
  filter(!str_detect(nom_loc, "Total")) %>%
  mutate(pob65_mas = as.numeric(pob65_mas)) %>%
  # replace_na(list(pob65_mas = 0)) %>%
  mutate(prop_pob65_mas_pobtot = pob65_mas / pobtot) %>%
  mutate(cve_geo = paste0(entidad, mun, loc, ageb, mza)) %>%
  relocate(cve_geo, .before = entidad)

# = = JOIN DATA = = #
sf_pob_merida_mun_mza <- df_merida_mun %>%
  left_join(y = sf_mza_merida_mun, by = join_by(cve_geo == cvegeo)) %>%
  st_as_sf()

mapa <- sf_pob_merida_mun_mza %>%
  filter(prop_pob65_mas_pobtot > 0) %>%
  ggplot() +
  geom_sf(data = sf_pob_merida_mun_mza, fill = "transparent", color = "#F3D9CA", linewidth = 0.01) + # nolint
  geom_sf(mapping = aes(fill = prop_pob65_mas_pobtot), linewidth = 0) +
  scale_fill_gradientn(colours = moma.colors(palette_name = "Alkalay1"), na.value = "transparent", labels = scales::label_percent()) + # nolint
  guides(
    fill = guide_colorbar(
      title.position = "top",
      title = "Porcentaje con respecto a la población total de la manzana",
      barwidth = unit(5, "in"),
      ticks = FALSE,
      )) +
  labs(title = "#30DayMapChallenge Day 29: Population<br>Población mayor de 65 años en Mérida, Yucatán", # nolint
       caption = "<b>Datos</b>: Principales resultados por AGEB y manzana urbana 2020, INEGI.<br><b>Isaac Arroyo (@unisaacarroyov)</b>") + # nolint
  theme_void() +
  theme(
    legend.position = "top",
    legend.title = element_textbox(
      color = "#F3D9CA",
      family = "title",
      face = "bold",
      halign = 0.5,
      hjust = 0.5,
      size = 35,
      margin = margin(b = -0.15, unit = "in")),
    legend.text = element_text(
      color = "#F3D9CA",
      family = "body",
      margin = margin(t = -0.15, unit = "in"),
      size = 25),

    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(
      width = unit(7.5, "in"),
      family = "title",
      size = 70,
      face = "bold",
      color = "#F3D9CA",
      lineheight = 0.4,
      hjust = 0.5,
      halign = 0.5,
      margin = margin(t = 0.3, b = 0.2, unit = "in")),
    plot.caption = element_textbox(
      width = unit(7, "in"),
      family = "body",
      size = 30,
      color = "#F3D9CA",
      hjust = 0.5,
      halign = 0.5,
      lineheight = 0.4,
      margin = margin(t = -1.8, b = 0.3, unit = "in"))
  )

tgutil::ggpreview(width = 8, height = 10, units = "in", plot = mapa, bg = "#040D12") # nolint

ggsave(plot = mapa, width = 8, height = 10, units = "in", bg = "#040D12",
       filename = "./maps/2023_30daymapchallenge_day29_population.png")
