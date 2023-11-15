library(tidyverse)
library(ggtext)
library(MoMAColors)
library(sysfonts)
library(showtextdb)
library(showtext)
library(sf)
library(geojsonsf)
sf_use_s2(FALSE)


font_add_google("Cardo", "title")
font_add_google("Libre Franklin", "body")
showtext_auto()

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
       caption = "<b>Datos</b>: <em>Uso del Suelo y Vegetación, (Serie I), en México, INEGI, 1978-1991</em> via <b>IDEGEO</b>") + # nolint
  theme_void() +
  guides(fill = guide_legend(title = "Tipo de suelo y vegetación",
                             title.position = "top",
                             label.position = "left",
                             ncol = 2)) +
  theme(
    plot.background = element_rect(fill = "#171D0C", color = "transparent"),

    legend.position = c(0.77, 0.1),
    legend.title.align = 1,
    legend.title = element_markdown(color = "#EBE7F2", face = "bold",
                                    family = "title", size = 50,
                                    lineheight = 0.2,
                                    margin = margin(b = -0.2, unit = "in"),
                                    ),
    legend.text = element_text(color = "#E4E5E2", family = "body",
                               size = 30,
                               margin = margin(r = -0.25, unit = "in"),
                               ),

    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(color = "#E4E5E2", family = "title",
                                 size = 100, face = "bold",
                                 margin = margin(l = 0.1, b = 0.05, unit = "in"), # nolint
                                 ),
    plot.subtitle = element_textbox(color = "#E4E5E2", family = "body",
                                    size = 50,
                                    margin = margin(l = 0.1, t = 0.1, unit = "in")), # nolint
    plot.caption = element_textbox(color = "#E4E5E2", hjust = 0,
                                   family = "body", size = 30,
                                   margin = margin(t = 1, l = 0.1, unit = "in") # nolint
                                   )
  )

# tgutil::ggpreview(mapa_usv_s01_yuc, width = 11, height = 10, dpi = 300, units = "in")

ggsave(plot = mapa_usv_s01_yuc, width = 11, height = 10, dpi = 300,
      units = "in", filename = "./maps/2023_30daymapchallenge_day11_retro.png")
