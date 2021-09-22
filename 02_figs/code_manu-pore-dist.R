############################
# author: Gina (vnichols@iastate.edu)
# created: may 19 2021
# purpose: estimate pore size dist
# 
#
#
# last modified: may 26 2021
##############################

# 1 cm water = 0.0980665 kpa

rm(list = ls())
library(tidyverse)
#remotes::install_github("vanichols/PFIswhc")
library(PFIswhc)
library(scales)

theme_set(theme_bw())


pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_ltblue <- "#8DC4EB"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"
show_col(pfi_brn)



# data --------------------------------------------------------------------

dp <- read_csv("01_fit-models/dat_poresizes.csv") %>% 
  group_by(pore_cat, site_sys, cc_trt, rep_id) %>% 
  summarise(pct = sum(pct, na.rm = T))



# do macro/micro summary --------------------------------------------------
raws <- 
  dp %>% 
  filter(pore_cat == "Macropores (>30 um)") %>% 
  mutate(
    site_sys = factor(site_sys, 
                      levels = c("West-grain",
                                 "Central-silage", 
                                 "Central-grain",
                                 "East-grain")))



#--not sure which order to calculate them (lag/lead)

dat_pores <- 
  dp %>% 
  filter(pore_cat == 
           "Micropores (<30 um)") %>% 
  #--get mean for each trial
  group_by(pore_cat, site_sys, cc_trt) %>% 
  summarise(pct = mean(pct, na.rm = T)) %>% 
  pivot_wider(names_from = pore_cat, values_from = pct) %>% 
  #--force the macro to fill in the rest
  mutate(`Macropores (>30 um)` = 1 - `Micropores (<30 um)`) %>% 
    pivot_longer(3:4, names_to = "pore_cat", values_to = "pct") %>% 
  mutate(cc_trt = fct_rev(cc_trt)) %>% 
  mutate(
    site_sys = factor(site_sys, 
                      levels = c("West-grain",
                                 "Central-silage", 
                                 "Central-grain",
                                 "East-grain")),
    pore_cat = factor(pore_cat, 
                      levels = c("Macropores (>30 um)",
                                 "Micropores (<30 um)")),
    pore_cat = fct_rev(pore_cat))
  


# bar graphs --------------------------------------------------------------

dat_pores %>% 
  ggplot() + 
  geom_col(aes(cc_trt, pct, alpha = pore_cat, fill = cc_trt), color = "black") + 
#  geom_linerange(data = maxmin, aes(x = cc_trt, ymin = min_pct, ymax = max_pct)) +
  #geom_linerange(data = maxmin, aes(x = cc_trt, ymin = sdlo, ymax = sdhi)) +
  geom_point(data = raws, aes(x = cc_trt, y = pct)) +
  scale_fill_manual(values = c("No Cover" = pfi_brn, "Cover Crop" = pfi_green)) +
  guides(fill = F) +
  scale_y_continuous(labels = label_percent()) +
  labs(x = NULL, y = "Percentage of Pores",
       alpha = NULL) +
  guides(alpha = guide_legend(reverse = T)) +
  facet_grid(.~site_sys, scales = "free") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)), 
        legend.position = "bottom") 

ggsave("02_figs/fig_manu_poresize.png", width = 7)



# pie charts --------------------------------------------------------------

dat_pie <- 
  dat_pores %>% 
  mutate(pore_cat = fct_inorder(pore_cat),
         pore_cat = fct_rev(pore_cat)) %>% 
  group_by(site_sys, cc_trt, pore_cat) %>% 
  summarise(value = mean(pct, na.rm = T)) %>% 
  arrange(pore_cat) %>% 
  group_by(site_sys, cc_trt) %>% 
  nest() %>% 
  mutate(data2 = data %>% purrr::map(. %>%
                                       arrange(pore_cat) %>% 
                                       mutate(half = value/2,
                                              prev = lag(value),
                                              prev = ifelse(is.na(prev), 0, prev),
                                              cumprev = cumsum(prev),
                                              pos = half + cumprev) %>% 
                                       select(-c(half, prev, cumprev)))) %>% 
  unnest(data2)



# pie chart ---------------------------------------------------------------

dat_pie %>% 
  mutate(site = site_sys,
         pos2 = 1-pos,
         value2 = ifelse(pore_cat == "Micropores (<30 um)", NA, value),
         clr = paste(cc_trt, pore_cat)) %>% 
  ggplot(aes(x = "", y = value, fill = pore_cat)) +
  geom_bar(stat = "identity", color = "black") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = pos2,
                label = percent(value2, accuracy = 2)), size = 5, show.legend = FALSE) +
  scale_fill_manual(values = c(pfi_ltblue, "white")) + 
  facet_grid(cc_trt~site) +
  guides(alpha = F) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)),
        legend.position = "bottom") +
  labs(fill = NULL)



dat_pie %>% 
  mutate(site = site_sys,
         pos2 = 1-pos,
         value2 = ifelse(pore_cat == "Micropores (<30 um)", NA, value),
         clr = paste(cc_trt, pore_cat)) %>% 
  ggplot(aes(x = "", y = value, fill = clr)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = pos2,
                label = percent(value2, accuracy = 2)), size = 5, show.legend = FALSE) +
  scale_fill_manual(values = c(pfi_brn, "white",
                               pfi_brn, "white",
                               pfi_green, "white",
                               pfi_green, "white")) + 
  facet_grid(cc_trt~site) +
  guides(alpha = F) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)),
        legend.position = "bottom") +
  labs(fill = NULL)
