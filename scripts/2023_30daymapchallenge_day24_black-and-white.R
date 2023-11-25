library(tidyverse)
library(ggtext)
library(sysfonts)
library(showtext)

font_add_google("Ruda", "title")
font_add_google("Aleo", "body")
showtext_auto()

df <- read_csv("./data/raster_h_world.csv.bz2")


mapa <- ggplot(data = df,
               mapping = aes(x = x, y = y, fill = HYP_LR_SR_OB_DR_1)) +
  geom_raster() +
  scale_fill_gradient(low = "#FFFFFF", high = "#000000") +
  guides(fill = "none") +
  labs(title = "#30DayMapChallenge Day 24: Black & White<br><span style='font-size:80px;'>Shaded Relief, Water, Drainages, and Ocean Bottom.</span>", # nolint
       subtitle = 'Mapa hecho durante el taller de "Análisis Geográfico con R" impartido por <b>@fridagcelis</b>.', # nolint
       caption = "<b>Data</b>: Natural Earth.<br><b>Isaac Arroyo (@unisaacarroyov)</b>") + # nolint
  theme_void() +
  theme(
    plot.title = element_textbox(face = "bold", family = "title",
                                 color = "white",
                                 size = 80,
                                 lineheight = 0.3,
                                 margin = margin(t = 0.2, l = 0.45, unit = "in") # nolint
                                ),
    plot.subtitle = element_textbox(family = "body", color = "white",
                                    size = 40,
                                    lineheight = 0.4,
                                    width = unit(9.5, "in"),
                                    margin = margin(t = 0.1, l = 0.45, b = -0.2, unit = "in") # nolint
                                    ),
    plot.caption = element_textbox(family = "body", color = "white",
                                   size = 30,
                                   hjust = 0,
                                   lineheight = 0.4,
                                   margin = margin(b = 0.1, l = 0.45, t = -0.2, unit = "in")) # nolint
  )

tgutil::ggpreview(plot = mapa, bg = "#000000",
  width = 9, height = 8, units = "in")

ggsave(plot = mapa, width = 9, height = 8, units = "in", bg = "black",
       filename = "./maps/2023_30daymapchallenge_day24_black-and-white.png")
