############################
# author: Gina (vnichols@iastate.edu)
# created: Oct 10 2019
# last modified: Oct 21 2019 (do calcs, oh ASA...)
#                march 27 2018 (update based on britt's calcs)
#
# purpose: calculate things in data
# 
# inputs: td_pressure-cells, re_euIDs
#
# outputs:
#
# notes: for porosity calcs used: http://lawr.ucdavis.edu/classes/SSC100/probsets/pset01.html
#
##############################


rm(list = ls())
library(tidyverse)
library(broom)
library(readxl)


# read in data -----------------------------------------------------

key <- read_csv("data/raw/rd_euIDs.csv")

datraw <- 
  read_csv("data/tidy/td_pressure-cells.csv")  %>% 
  mutate(soilvol_cm3 = 347.5,
         bulkden_gcm3 = drysoil_g / soilvol_cm3
  ) # assume volume of soil sample is 347.50 cm3 (? is this right???)

dat_soil <- 
  datraw %>% 
  select(code, soilvol_cm3, drysoil_g, bulkden_gcm3)


myminden <- 2

# water -------------------------------------------------------------------

# calculate water amts (subtract cylinder wgts),

dat_water <- 
  datraw %>%
    mutate(   ##--# calc actual amount of water released at each pressure
    w_0cm_g = 0, ##-britt adds this to 2.5 atm value, keep a 0 as a place-holder
    w_2.5cm_g = atm - cylinder_g + satwater_g,
    w_10cm_g = `10_cm` - cylinder_g,
    w_25cm_g = `25_cm` - cylinder_g,
    w_50cm_g = `50_cm` - cylinder_g,
    w_100cm_g = `100_cm` - cylinder_g,
    w_200cm_g = `200_cm` - cylinder_g,
    w_500cm_g = `500_cm` - cylinder_g,
    w_999cm_g = sampafter500_g - ringpluscrap_g - drysoil_g
  ) %>% 
  mutate(  #--total amt of water released
    w_tot_g = purrr::reduce(select(., starts_with("w")), `+`)
  ) %>% 
  select(code, soilvol_cm3, bulkden_gcm3, starts_with("w"))

dat_water %>% 
  ggplot(aes(code, w_tot_g)) + 
  geom_col()


# porosity ----------------------------------------------------------------

# via britt and mineral methods

dat_poros <- 
  datraw %>%
  # calc porosity via britt
  mutate(
    soil_at_sat_g = (satsamp_g - ringpluscrap_g),
    water_at_sat_g = soil_at_sat_g - drysoil_g, 
    air_cm3 = water_at_sat_g,
    poros_britt = air_cm3/soilvol_cm3,
    poros_mineral = 1 - bulkden_gcm3/myminden
    ) %>% 
  select(code, drysoil_g, bulkden_gcm3, 
         water_at_sat_g, air_cm3, poros_britt, poros_mineral)

dat_poros

dat_poros  %>% 
  ggplot(aes(poros_mineral, poros_britt)) + 
  geom_point() + 
  geom_abline()
  
# what does britt's look like?
britt <- read_excel("data/raw/rd_britt-porosity.xlsx")

britt %>% 
  ggplot(aes(e2, e1)) + 
  geom_point(color = "red", size = 4) + 
  geom_point(data = dat_poros, size = 4, aes(poros_mineral, poros_britt)) +
  geom_abline() + 
  coord_cartesian(xlim = c(0.35, 0.65), ylim = c(0.35, 0.65)) +
  labs(x = "porosity based on mineral density of 2.65",
       y = "porosity based on water in saturated sample",
       title = "Britt samples = red\nGina samples = black")

#--show this to britt
ggsave("porosity-2-methods.png", width = 5, height = 5)

# soil water contents at each pressure ------------------------------------

# cumulative amount of water purged at each pressure
dat_cum <- 
  dat_water %>% 
  select(-w_999cm_g) %>% 
  # gather into long form
  pivot_longer(w_0cm_g:w_500cm_g, 
               names_to = "press_cm", 
               values_to = "water_g") %>% 
  separate(press_cm, 
           into = c("water", "press_cm", "grams"), 
           sep = "_") %>%
  # get pressure as a numeric value
  mutate(press_cm = as.numeric(str_sub(press_cm, 1, -3))) %>%
  group_by(code) %>%
  arrange(code, press_cm) %>%
  # get cumulative water
  mutate(cumwater_g = cumsum(water_g)) %>% 
  select(code, press_cm, water_g, cumwater_g)

dat_cum %>% 
  ggplot(aes(press_cm, cumwater_g, color = code)) + 
  geom_line()

# convert this water amount to a volumetric/gravimetric water content
dat_theta <- 
  dat_cum %>%
  left_join(dat_poros) %>% 
  left_join(dat_soil) %>% 
  mutate(
    water_inside_soil_still = water_at_sat_g - cumwater_g,
    gtheta = water_inside_soil_still / drysoil_g,
    vtheta1 = gtheta * bulkden_gcm3,
    frac_water = cumwater_g/water_at_sat_g,
    vtheta2a = (1 - frac_water)*poros_britt,
    vtheta2b = (1 - frac_water)*poros_mineral
    ) %>% 
  select(code, press_cm, vtheta1, vtheta2a, vtheta2b)



dat_theta %>% 
  pivot_longer(vtheta1:vtheta2b) %>% 
  ggplot(aes(press_cm, value, group = interaction(code, name))) + 
  geom_line(aes(color = name)) + 
  facet_grid(.~name) + 
  labs(title = "gtheta method = red\nbritt method = green\nmineral method = blue")
  
# so vetha1 and vtheta2a are the same

# look at it --------------------------------------------------------------

dat_theta %>%
  select(-vtheta2a) %>% 
  filter(press_cm != 0) %>% 
  pivot_longer(vtheta1:vtheta2b) %>% 
  left_join(key) %>% 
  unite(site_name, sys_trt, col = "site_trt") %>% 
  # plot
  ggplot(aes(press_cm, value, color = cc_trt)) + 
  # raw data
  geom_point(alpha = 0.5) + 
  geom_line(alpha = 0.5, aes(group = code)) + 
  scale_x_log10() +
  # summary of data
  stat_summary(fun.y = mean, geom="line", size = 2) +
  stat_summary(fun.data = "mean_se", size = 1) +
  facet_grid(name ~ site_trt)

# they look the same, no matter which method you use, tell britt
ggsave("porosity-2-methods-results.png")


#--bulk densities?
dat_soil %>% 
  left_join(key) %>% 
  unite(site_name, sys_trt, col = "site_trt") %>% 
  ggplot(aes(site_trt, bulkden_gcm3, color = cc_trt)) + 
  geom_point(aes(color = cc_trt)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2) + 
  stat_summary(fun.data = "mean_se", size = 1)

dat_soil %>% 
  left_join(key) %>% 
  unite(site_name, sys_trt, col = "site_trt") %>% 
  ggplot(aes(cc_trt, bulkden_gcm3)) + 
  geom_point(aes(color = cc_trt)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2) + 
  stat_summary(fun.data = "mean_se", size = 1)


#--power analysis, detect a 0.5 change
# what is the variation?
dat_soil %>% 
  summarise(sd(bulkden_gcm3))
# sd = 0.104
# var = 0.0108

# from https://www.r-bloggers.com/calculating-required-sample-size-in-r-and-sas/
delta <- 0.5 #--this is the minimum change I want to be significant
sigma <- 0.01 #--this is variance

(4 * (1.96 + 1.28)^2 * sigma ) / (delta^2) #--I had the power