library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtext)
library(MoMAColors)
library(sf)
sf_use_s2(FALSE)

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
  geom_sf(data = sf_pob_merida_mun_mza, fill = "transparent", color = "white", linewidth = 0.01) + # nolint
  geom_sf(mapping = aes(fill = prop_pob65_mas_pobtot), linewidth = 0) +
  scale_fill_gradientn(colours = moma.colors(palette_name = "Alkalay1"), na.value = "transparent", labels = scales::label_percent()) + # nolint
  guides(
    fill = guide_colorsteps(
      title.position = "top",
      title = "Porcentaje con respecto a la población total de la manzana",
      barwidth = unit(5, "in"),
      ticks = FALSE,
      )) +
  labs(title = "#30DayMapChallenge Day 29: Population<br>Población mayor de 65 años", # nolint
       caption = "<b>Datos</b>: Principales resultados por AGEB y manzana urbana 2020, INEGI.<br><b>Isaac Arroyo (@unisaacarroyov)</b>") + # nolint
  theme_void() +
  theme(
    legend.position = "top",
    legend.title = element_textbox(color = "white",
                                   halign = 0.5,
                                   hjust = 0.5
                                   ),
    legend.text = element_text(color = "white"),

    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(width = unit(7, "in"),
                                 color = "white",
                                 hjust = 0.5,
                                 halign = 0.5
                                 ),
    plot.caption = element_textbox(width = unit(7, "in"),
                                   color = "white",
                                   hjust = 0.5,
                                   halign = 0.5,
                                   lineheight = 0.3,
                                   )
  )

tgutil::ggpreview(width = 8, height = 10, units = "in", plot = mapa, bg = "#040D12") # nolint
