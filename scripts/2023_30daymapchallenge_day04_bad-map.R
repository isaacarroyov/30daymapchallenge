library(tidyverse)
library(sf)
library(geojsonsf)
library(biscale)
library(sysfonts)
library(showtextdb)
library(showtext)

font_add_google("Quattrocento Sans", "Quattrocento Sans")
font_add_google("Quattrocento", "Quattrocento")
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
  st_as_sf()

# = = CREATE BIVARIATE DATA = = #
sf_bivariate <- bi_class(.data = sf_climate_mun_tmmx_pr, x = anomaly_tmmx_mean, y = anomaly_pr_mean, style = "quantile", dim = 4) %>% # nolint
  relocate(bi_class, .after = date_year) %>%
  mutate(date_year = year(date_year))

# = = DATAVIS = = #

p1 <- sf_bivariate %>%
  ggplot(aes(fill = bi_class)) +
  geom_sf(linewidth = 0) +
  bi_scale_fill(pal = "DkViolet2", dim = 4) +
  facet_wrap(vars(date_year), ncol = 1) +
  labs(title = "#30DayMapChallenge Day 4: Bad Map",
       subtitle = "Un <b>mal mapa</b>, es el que no te dice como se lee.",
       caption = "<b>Datos</b>: <em>TerraClimate</em> a través de <b>Google Earth Engine</b><br><em>Isaac Arroyo (@unisaacarroyov)</em>") + # nolints
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = "#F1EFE3", color = "transparent"),
    plot.caption.position = "plot",
    plot.title.position = "plot",
    plot.title = ggtext::element_markdown(family = "Quattrocento", size = 60, face = "bold"), # nolint
    plot.subtitle = ggtext::element_markdown(family = "Quattrocento Sans", size = 30), # nolint
    plot.caption = ggtext::element_textbox(family = "Quattrocento Sans", size = 25, lineheight = 0.5, hjust = 0, margin = margin(b = 0.2, unit = "in")), # nolint
    strip.text = element_text(face = "bold", size = 50, family = "Quattrocento", margin = margin(t=0.15, unit = "in")) # nolint
  )

# tgutil::ggpreview(p1, width = 6, height = 8, unit = "in") # nolint

ggsave(plot = p1,
      filename = "./maps/2023_30daymapchallenge_day04_bad-map.png",
      width = 6, height = 8, unit = "in")
