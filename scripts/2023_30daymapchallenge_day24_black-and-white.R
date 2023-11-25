library(tidyverse)

df <- read_csv("./data/raster_h_world.csv.bz2")


mapa <- ggplot(data = df,
               mapping = aes(x = x, y = y, fill = HYP_LR_SR_OB_DR_1)) +
  geom_raster() +
  scale_fill_gradient(low = "#FFFFFF", high = "#000000") +
  guides(fill = "none") +
  theme_void()

tgutil::ggpreview(plot = mapa, width = 10, height = 8, units = "in")
