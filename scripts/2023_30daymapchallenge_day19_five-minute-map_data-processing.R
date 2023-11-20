library(tidyverse)

eras_tour_info <- read_csv("./data/eras_tour_dates.csv")
eras_tour_venues <- read_csv("./data/eras_tour_long-lat.csv")

# = = Combine data = = #

eras_tour_info %>%
  count(date_year = year(date_year_month_day),
        address) %>%
  left_join(y = eras_tour_venues,
            by = join_by(address)) %>%
  rename(n_events = n) %>%
  write_csv("./data/eras_tour_datawrapper.csv")
