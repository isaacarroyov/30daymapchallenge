library(tidyverse)
library(sf)
library(geojsonsf)
library(ggtext)
library(patchwork)
library(sysfonts)
library(showtextdb)
library(showtext)

font_add_google("Josefin Sans", "title")
font_add_google("Open Sans", "body")
showtext_auto()

# = = LOAD DATA = = #
# - - Climate data - - #
df_climate_mun <- read_csv("https://raw.githubusercontent.com/nmasfocusdatos/desplazamiento-climatico/main/datos/ee_terraclimate_db/ts_nac-ent-mun_year_terraclimate.csv") # nolint

# ~ ~ Focus only in 1960 and 2022 data ~ ~ #
df_climate_mun_tmmx_pr <- df_climate_mun %>%
  filter(year(date_year) %in% c(1960, 2022),
         nombre_municipio != "Estados_Nacionales") %>%
  select(cve_geo, date_year, anomaly_tmmx_mean, anomaly_pr_mean)

# - - Geometries - - #
sf_mun <- geojson_sf("https://raw.githubusercontent.com/nmasfocusdatos/desplazamiento-climatico/main/datos/poligonos_mex/00mun_simplified-t005-geopandas.geojson") %>% janitor::clean_names() # nolint

# = = JOIN DATA = = #
sf_climate_mun_tmmx_pr <- left_join(
  x = df_climate_mun_tmmx_pr,
  y = select(sf_mun, cvegeo),
  by = join_by(cve_geo == cvegeo)) %>%
  mutate(date_year = year(date_year)) %>%
  st_as_sf()

# = = DATAVIS = = #
# - - Anomaly temp - - #
p_tmmx <- sf_climate_mun_tmmx_pr %>%
  ggplot(aes(fill = anomaly_tmmx_mean)) +
  geom_sf(linewidth = 0.01, color = "black") +
  facet_wrap(vars(date_year)) +
  scale_fill_gradient2(low = "gray50", high = "#C23006",
                       limits = c(-2, 2),
                       breaks = seq(-2, 2, 0.5),
                       labels = scales::label_number(suffix = " °C")) +
  guides(fill = guide_colorbar(title = "Anomalía de temperatura máxima",
                               title.position = "top",
                               barwidth = unit(8, "in"),
                               barheight = unit(0.1, "in"),
                               ticks = FALSE)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#1E1E1E", color = "#1E1E1E"),
    # - - Legend config - - #
    legend.position = "bottom",
    legend.title = element_markdown(color = "#EBE7F2", face = "bold",
                                    family = "title", size = 50,
                                    margin = margin(b = -0.25, unit = "in"),
                                    ),
    legend.text = element_text(color = "#E4E5E2",
                               family = "body",
                               size = 30,
                               margin = margin(t = -0.25, b = 0.1, unit = "in")), # nolint
    strip.text = element_markdown(color = "#E4E5E2",
                                  face = "bold",
                                  family = "title", size = 60)
  )

# - - Anomaly precipitation - - #
p_pr <- sf_climate_mun_tmmx_pr %>%
  ggplot(aes(fill = anomaly_pr_mean)) +
  geom_sf(linewidth = 0.01, color = "black") +
  facet_wrap(vars(date_year)) +
  scale_fill_gradient2(low = "#8B4A27", high = "#278B46",
                       limits = c(-1, 1),
                       breaks = seq(-1, 1, 0.25),
                       labels = scales::label_percent()) +
  guides(fill = guide_colorbar(title = "Anomalía precipitación",
                               title.position = "top",
                               barwidth = unit(8, "in"),
                               barheight = unit(0.1, "in"),
                               ticks = FALSE)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#1E1E1E", color = "#1E1E1E"),
    # - - Legend config - - #
    legend.position = "bottom",
    legend.title = element_markdown(color = "#EBE7F2", face = "bold",
                                    family = "title", size = 50,
                                    margin = margin(b = -0.25, unit = "in"),
                                    ),
    legend.text = element_text(color = "#E4E5E2",
                               family = "body",
                               size = 30,
                               margin = margin(t = -0.25, unit = "in")),
    strip.text = element_markdown(color = "#E4E5E2",
                                  face = "bold",
                                  family = "title", size = 60)
  )

tgutil::ggpreview(p_pr, width = 11, height = 5, units = "in")
tgutil::ggpreview(p_tmmx, width = 11, height = 5, units = "in")


mapa_final <- p_tmmx / p_pr + plot_annotation(
  title = "#30DayMapChallenge Day 13: Choropleth", # nolint
  subtitle = "Detrás del mapa del Bad Map (Day 4). Anomalía de temperatura máxima y de precipitación de México a nivel municipal", # nolint
  caption = "<b>Datos</b>: <em>TerraClimate</em> a través de <b>Google Earth Engine</b>, tomados de <em>Desplazamiento climático: La migración que no vemos</em> de <b>N+ Focus</b>. El promedio histórico de ambas variables se toma del periodo 1960 - 1989.<br><em>Isaac Arroyo (@unisaacarroyov)</em>", # nolint
  theme = theme(
    plot.background = element_rect(fill = "#1E1E1E", color = "#1E1E1E"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.title = element_textbox(color = "#E4E5E2",
                                 family = "title",
                                 size = 100, face = "bold"),
    plot.subtitle = element_textbox(color = "#E4E5E2",
                                    family = "body",
                                    lineheight = 0.4,
                                    width = unit(10, "in"),
                                    margin = margin(b = 0.2, unit = "in"),
                                    size = 40),
    plot.caption = element_textbox(color = "#E4E5E2",
                                   width = unit(9, "in"),
                                   margin = margin(t = 0.25, unit = "in"),
                                   hjust = 0, lineheight = 0.4,
                                   family = "body", size = 27),
  )
)

tgutil::ggpreview(mapa_final, width = 11, height = 10, units = "in")

ggsave(plot = mapa_final,  width = 11, height = 10, units = "in",
       filename = "./maps/2023_30daymapchallenge_day13_choropleth.png")
