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


theme_set(theme_bw())


pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"
show_col(pfi_green)



# data --------------------------------------------------------------------


dp <- 
  sare_pressure %>%
  group_by(plot_id) %>%
  mutate(
    vlag = lag(vtheta),
    thng = (vlag - vtheta) / vlag * 100,
    d_um = 300 / press_cm * 0.0980665,
    tot = sum(thng, na.rm = T),
    pct = thng / tot
  ) %>% 
  filter(!is.na(pct))

dp %>% 
  left_join(sare_plotkey) %>% 
  ggplot(aes(d_um, pct)) + 
  geom_col(position = "dodge", aes(fill = cc_trt)) + 
  facet_wrap(~site_name+sys_trt)



# create dummy dataset for distribution -----------------------------------

#--something isn't right...

theplots <- dp %>% pull(plot_id) %>% unique()

for (i in 1:length(theplots)) {
  myplot <- theplots[i]
  
  fd <-
    dp %>%
    select(plot_id, d_um, pct) %>%
    filter(plot_id == myplot)
  
  Type <- c()
  for (j in 1:nrow(fd)) {
    Type <- c(Type,
              rep(fd$d_um[j], fd$pct[j] * 100))
  }
  
  if (i == 1) {
    dat <- tibble(plot_id = myplot,
                  pore_d = Type)
  } else {
    dat.tmp <- tibble(plot_id = myplot,
                      pore_d = Type)
    dat <- bind_rows(dat, dat.tmp)
  }
}


dat %>% 
  left_join(sare_plotkey) %>% 
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye Cover Crop",
    grepl("no", cc_trt) ~ "No Cover",
    TRUE ~ cc_trt),
    site_sys = paste(site_name, sys_trt, sep = "-"),
    site_sys = factor(site_sys, 
                      levels = c("West-grain",
                                 "Central-silage", 
                                 "Central-grain",
                                 "East-grain"))) %>%  
  ggplot(aes(pore_d)) + 
  geom_density(aes(fill = cc_trt), alpha = 0.5) + 
  scale_fill_manual(values = c(pfi_green, pfi_brn)) +
  facet_wrap(~site_sys + cc_trt, ncol = 2) 
  

# do macro/micro summary --------------------------------------------------

#--not sure which order to calculate them (lag/lead)

sare_pressure %>% 
  group_by(plot_id) %>% 
  mutate(vlag = lead(vtheta),
         thng = (vtheta - vlag)/vtheta*100) %>% 
  group_by(plot_id ) %>% 
  mutate(tot = sum(thng, na.rm = T),
         pct = thng/tot,
         pore_cat = ifelse(press_cm < 100, "macro", "micro")) %>% 
  group_by(plot_id, pore_cat) %>% 
  summarise(pct = sum(pct, na.rm = T)) %>% 
  left_join(sare_plotkey) %>% 
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye Cover Crop",
    grepl("no", cc_trt) ~ "No Cover",
    TRUE ~ cc_trt),
    site_sys = paste(site_name, sys_trt, sep = "-"),
    site_sys = factor(site_sys, 
                      levels = c("West-grain",
                                 "Central-silage", 
                                 "Central-grain",
                                 "East-grain"))) %>%  
  group_by(pore_cat, site_sys, cc_trt) %>% 
  summarise(pct = mean(pct, na.rm = T)) %>% 
  ggplot(aes(cc_trt, pct, alpha = pore_cat)) + 
  geom_col(aes(fill = cc_trt)) + 
  scale_fill_manual(values = c(pfi_green, pfi_brn)) +
  facet_grid(.~site_sys, scales = "free")

sare_pressure %>% 
  group_by(plot_id) %>% 
  mutate(vlag = lead(vtheta),
         thng = (vtheta - vlag)/vtheta*100) %>% 
  group_by(plot_id ) %>% 
  mutate(tot = sum(thng, na.rm = T),
         pct = thng/tot,
         pore_cat = ifelse(press_cm < 100, "macro", "micro")) %>% 
  group_by(plot_id, pore_cat) %>% 
  summarise(pct = sum(pct, na.rm = T)) %>% 
  left_join(sare_plotkey) %>% 
  filter(pore_cat == "macro") %>% 
  ggplot(aes(cc_trt, pct)) + 
  geom_point() + 
  facet_grid(.~site_name + sys_trt)

