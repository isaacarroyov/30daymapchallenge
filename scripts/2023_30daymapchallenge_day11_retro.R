library(tidyverse)
library(ggtext)
library(MoMAColors)
library(sysfonts)
library(showtextdb)
library(showtext)
library(sf)
library(geojsonsf)
sf_use_s2(FALSE)

# = = LOAD DATA = = #
# - - Uso de Suego y Vegetación Serie I de Yucatán (1978 - 1991)
sf_usv_s01_yuc <- geojson_sf("./data/usv_series_01_yucatan.geojson") %>%
janitor::clean_names()

# - - Geometrías del estado de Yucatán de acuerdo al Marco Geostadístico de México (2022) - - # nolint
sf_ent_yuc <- st_read("https://raw.githubusercontent.com/isaacarroyov/datos_facil_acceso/main/Gobierno-Mexicano/geometrias/00ent_mexico.geojson") %>% # nolint
  janitor::clean_names() %>%
  filter(cve_geo == "31")

# = = DATA PROCESSING = = #
# - - Agrupar `clase` en categorias más reducidas - - #
sf_usv_s01_yuc_datavis <- sf_usv_s01_yuc %>%
  mutate(clase_reduced = case_when(
    str_detect(clase, "AGRICULTURA") ~ "Agricultura",
    clase == "AREA SIN VEGETACION APARENTE" ~ "Sin vegetación aparente",
    clase == "OTRO TIPO DE VEGETACION" ~ "Otro tipo de vegetación",
    str_detect(clase, "PASTIZAL") ~ "Pastizal",
    str_detect(clase, "SELVA") ~ "Selva",
    str_detect(clase, "HIDROFILA") ~ "Vegetación hidrófila",
    .default = str_to_sentence(clase)))

# = = DATAVIS = = #
mapa_usv_s01_yuc <- sf_usv_s01_yuc_datavis %>%
  ggplot(aes(fill = clase_reduced)) +
  geom_sf(linewidth = 0.1, color = "black") +
  geom_sf(data = sf_ent_yuc,
          fill = "transparent",
          color = "#E4E5E2",
          linewidth = 0.5) +
  scale_fill_manual(values = moma.colors("Ohchi", n = 9)) +
  labs(title = "#30DayMapChallenge Day 11: Retro",
       subtitle = "Uso de Suelos y Vegetación en Yucatán, Serie I (1978-1991)",
       caption = "<b>Datos</b>:<em>Uso del Suelo y Vegetación, (Serie I), en México, INEGI, 1978-1991</em> via <b>IDEGEO</b>") + # nolint
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#171D0C"),

    legend.position = "bottom",
    legend.title = element_text(color = "#EBE7F2", face = "bold"),
    legend.text = element_text(color = "#E4E5E2"),

    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(color = "#E4E5E2"),
    plot.subtitle = element_textbox(color = "#E4E5E2"),
    plot.caption = element_textbox(color = "#E4E5E2", hjust = 0)
  )

tgutil::ggpreview(mapa_usv_s01_yuc, width = 11, height = 10, dpi = 300)
