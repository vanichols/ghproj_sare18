#--map of sampling locs
#--updated: 2/4/2021


library(scales)
library(devtools)
library(tidyverse)
library(readxl)
library(ggpubr)
library(wesanderson)
library(patchwork)
library(maps)
#devtools::install_github("vanichols/PFIweeds2020")
library(PFIweeds2020)
library(ggthemes)

#--figs, all together


# map ---------------------------------------------------------------------

pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"
show_col(pfi_red)


map_iowa <- as_tibble(map_data('state')) %>%
  filter(region == "iowa")

map_county <- as_tibble(map_data('county')) %>%
  filter(region == "iowa")

map_county3 <-
  map_county %>% filter(subregion %in% c("boone", "greene", "washington"))

set.seed(3)

ggplot() +
  geom_polygon(
    data = map_iowa,
    aes(x = long, y = lat, group = group),
    color = "gray20",
    fill = "white"
  ) +
  geom_polygon(
    data = map_county,
    aes(x = long, y = lat, group = group),
    color = "gray80",
    fill = NA
  ) +
  geom_polygon(
    data = map_county3,
    aes(x = long, y = lat, group = group),
    color = pfi_blue,
    fill = "white",
    size = 3
  ) +
  geom_jitter(
    data = pfi_siteinfo %>% rename("long" = "lon"),
    aes(x = long, y = lat),
    size = 4,
    fill = pfi_orng,
    width = 0.07,
    pch = 21
  ) +
  geom_text(aes(x = -95.05, y = 42.105), size = 7, label = "West", color = "black") +
  geom_text(aes(x = -93.5, y = 42.315), size = 7, label = "Central", color = "black") +
  geom_text(aes(x = -91.7, y = 41.05), size = 7, label = "East", color = "black") +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  coord_quickmap() +
  theme_map()

ggsave("02_figs/fig_manu_map.png")
