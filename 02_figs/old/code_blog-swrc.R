#--make fig showing swrc for blog
#--updated: 2/22/2021

rm(list = ls())
library(tidyverse)
library(scales)
library(purrr)
library(PFIswhc)
library(Hmisc)

theme_set(theme_bw())

dpress <- 
  sare_pressure %>%  
  left_join(sare_plotkey)


dat <- 
  sare_gallons %>%  
  left_join(sare_plotkey)


#--wh capacity (max - min)
wh <- 
  dat %>%
  filter(press_cm == max(press_cm) | press_cm == min(press_cm)) %>% 
  #group_by(site_name, sys_trt, cc_trt, press_cm) %>% 
  #summarise(gal_3inac = mean(gal_3inac, na.rm = T)) %>% 
  pivot_wider(names_from = press_cm, values_from = gal_3inac) %>% 
  janitor::clean_names() %>% 
  mutate(whc = x0 - x500)


# figure ------------------------------------------------------------------


pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"
show_col(pfi_red)


#--gallons of water
fig_wh <- 
  wh %>%
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye Cover Crop",
    grepl("no", cc_trt) ~ "No Cover",
    TRUE ~ cc_trt)
  ) %>%
  mutate(site_name = case_when(
    site_name == "Central" ~ "Boone",
    site_name == "West" ~ "Greene",
    site_name == "East" ~ "Washington",
    TRUE ~ "other"),
    sys_trt = str_to_title(sys_trt)) %>%  
  ggplot(aes(cc_trt, whc, fill = cc_trt)) + 
  stat_summary(geom = "bar") + 
  stat_summary(geom = "errorbar", fun.data = "mean_cl_boot", size = 1.5, width = 0.5) + 
  scale_fill_manual(values = c("Rye Cover Crop" = pfi_green,
                               "No Cover" = pfi_brn)) +
  facet_grid(.~site_name + sys_trt) + 
  labs(x = NULL,
       y = "Gallons of water\nin 3 inch acre slice",
       fill = NULL) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text = element_text(size = rel(1.3)),
        legend.position = "bottom", 
        legend.text = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_blank(),
        axis.title = element_text(size = rel(1.3)))

fig_wh  
ggsave("06_figs/fig_whc-gallons.png")


#--curve
fig_curve <- 
  dpress %>%
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye Cover Crop",
    grepl("no", cc_trt) ~ "No Cover",
    TRUE ~ cc_trt)
  ) %>%
  mutate(site_name = case_when(
    site_name == "Central" ~ "Boone",
    site_name == "West" ~ "Greene",
    site_name == "East" ~ "Washington",
    TRUE ~ "other"),
    sys_trt = str_to_title(sys_trt)) %>%  
  ggplot(aes(press_cm, vtheta_grav, color = cc_trt)) + 
  stat_summary(geom = "line", size = 1.5)  + 
  scale_color_manual(values = c("Rye Cover Crop" = pfi_green,
                               "No Cover" = pfi_brn)) +
  scale_x_continuous(breaks = c(100, 250, 500)) + 
  scale_y_continuous(breaks = c(0.3, 0.4, 0.5)) +
  facet_grid(.~site_name + sys_trt) + 
  labs(x = "Pressure (cm H2O)",
       y = "Soil water content\n(%vol)",
       color = NULL) +
  #guides(color = F) +
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        strip.text = element_text(size = rel(1.3)),
        legend.position = "bottom", 
        legend.text = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        axis.title = element_text(size = rel(1.3)))

fig_curve
ggsave("06_figs/fig_curves.png", width= 8, height = 5)

# 
# library(patchwork)
# 
# fig_curve/fig_wh + 
#   plot_layout(heights = c(1, 2))
# 
# ggsave("06_figs/fig_curve-plus-whc.png")
