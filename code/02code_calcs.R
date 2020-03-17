############################
# author: Gina (vnichols@iastate.edu)
# created: Oct 10 2019
# last modified: Oct 21 2019 (do calcs, oh ASA...)
#
# purpose: calculate things in data
# 
# inputs: td_pressure-cells
#
# outputs:
#
# notes
#
##############################


rm(list = ls())
library(tidyverse)
library(broom)


# read in data -----------------------------------------------------


key <- read_csv("_data/raw/rd_euIDs.csv")

datraw <- 
  read_csv("_data/tidy/td_pressure-cells.csv")  %>% 
  left_join(key)

# do calcs based on 479 lab -----------------------------------------------


dat <- 
  datraw %>%
  # assume volume of soil sample is 347.50 cm3
  mutate(soilvol_cm3 = 347.5) %>%
  # calc bulk density based on dry weight of soil
  mutate(bulkden_gcm3 = drysoil_g / soilvol_cm3) %>%
  # calc actual amount of water released at each pressure
  
  mutate(
    w_0cm_g = satwater_g,
    w_2.5cm_g = atm - cylinder_g,
    w_10cm_g = `10_cm` - cylinder_g,
    w_25cm_g = `25_cm` - cylinder_g,
    w_50cm_g = `50_cm` - cylinder_g,
    w_100cm_g = `100_cm` - cylinder_g,
    w_200cm_g = `200_cm` - cylinder_g,
    w_500cm_g = `500_cm` - cylinder_g,
    w_999cm_g = sampafter500_g - ringpluscrap_g - drysoil_g
  ) %>%
  
  # eliminate things I don't need
  select(-(satsamp_g:sampafter500_g), -ringpluscrap_g, -ring_g, -cell_nu) %>%
  
  # gather into long form
  gather(w_0cm_g:w_999cm_g, key = press_cm, value = water_g) %>%
  separate(press_cm, into = c("water", "press_cm", "grams"), sep = "_") %>%
  
  # get pressure as a numeric value
  mutate(press_cm = as.numeric(str_sub(press_cm, 1, -3))) %>%
  group_by(code) %>%
  arrange(code, -press_cm) %>%

  # get cumulative water
  mutate(cumwater_g = cumsum(water_g)) %>%
  
  # divide cum water by dry soil, gravimetric h2o content
  mutate(gtheta = cumwater_g / drysoil_g,
         vtheta = gtheta * bulkden_gcm3) %>% 
  ungroup() %>% 
  unite(site_name, sys_trt, col = "site_trt") 
  





# look at it --------------------------------------------------------------

#--log scale
#
dat %>%
   # no idea why I did this
  filter(press_cm != 0,
         press_cm != 999) %>% 
  # log scale easier to see
  mutate(lpress_cm = log(press_cm)) %>% 
  ggplot(aes(lpress_cm, vtheta, color = cc_trt)) + 
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5, aes(group = code)) + 
  stat_summary(fun.y = mean, geom="line", size = 2) +
  stat_summary(fun.data = "mean_se", size = 1) +
  facet_grid(~site_trt)

ggsave("_figs/glimpse/fig_lpress-curves.png")

#--raw scale
dat %>%
  filter(press_cm != 0,
         press_cm != 999) %>% 
  mutate(lpress_cm = log(press_cm)) %>% 
  ggplot(aes(press_cm, vtheta, color = cc_trt)) + 
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5, aes(group = code)) + 
  stat_summary(fun.y = mean, geom="line", size = 2) +
  stat_summary(fun.data = "mean_se", size = 1) +
  facet_grid(~site_trt)


ggsave("_figs/glimpse/fig_press-curves.png")


dat %>% 
  #filter(site == "St") %>% 
  ggplot(aes(cc_trt, bulkden_gcm3)) + 
  geom_point(size = 2) + 
  stat_summary(size = 2, color = "red") + 
  facet_grid(.~ site_trt)

ggsave("_figs/glimpse/fig_bd-stout.png")


# Fit linear model to lpress vs vwc ---------------------------------------

# ??

lmdat <- 
  dat %>%
  filter(press_cm != 0,
         press_cm != 999) %>% 
  mutate(lpress_cm = log(press_cm)) %>%
  select(code, site, cctrt, lpress_cm, vtheta) %>%
  
  # fit a line to every sample
  group_by(code, cctrt, site) %>%
  nest() %>% 
  mutate(mod = data %>% map(~lm(vtheta ~ lpress_cm, data = .)),
         res = mod %>% map(tidy)) %>%
  unnest(res) %>%
  #filter(term == "lpress_cm") %>%
  ungroup() %>%
  # see if slope is sig different by cctrt
  select(code, cctrt, site, estimate, term) %>% 
  spread(term, value = estimate) %>% 
  rename("intercept" = 4, "slope" = 5)

anova(lm(slope ~ cctrt*site, data = lmdat))

# %>%
#   nest() %>% 
#   mutate(mod = data %>% map(~lm(estimate ~ cctrt + site, data = .)),
#          res = mod %>% map(anova)) %>%
#   unnest(res)
  

library(broom)
tidy((dat %>% lm(vtheta ~ press_cm, data = .)))
