############################
# author: Gina (vnichols@iastate.edu)
# created: Oct 10 2019
# last modified: Oct 21 2019 (do calcs, oh ASA...)
#                march 27 2020 (update based on britt's calcs)
#                march 30 2020 (made sure it runs for britt)
#                may 8 2020 (rearrange folders, make sure it still runs)
#
# purpose: calculate things in data
# 
# inputs: pp_pressure-cells, rd_euIDs
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

cctrtpal <- c("#45AD45", "#69431D")

# read in data -----------------------------------------------------

key <- read_csv("raw-data/rd_euIDs.csv")

datraw <- 
  read_csv("01_pre-process/pp_pressure-cells.csv")  %>% 
  mutate(soilvol_cm3 = 347.5,
         bulkden_gcm3 = drysoil_g / soilvol_cm3
  ) # assume volume of soil sample is 347.50 cm3 (? is this right???)


# soil data ---------------------------------------------------------------

dat_soil <- 
  datraw %>% 
  select(code, soilvol_cm3, drysoil_g, bulkden_gcm3)


myminden <- 2

#--bulk density viz

dat_soil %>% 
  left_join(key) %>% 
  unite(site_name, sys_trt, col = "site_trt") %>% 
  ggplot(aes(cc_trt, bulkden_gcm3, color = cc_trt)) + 
  geom_point(aes(color = cc_trt), pch = 17, size = 3, alpha = 0.5) + 
  stat_summary(fun.data = "mean_se", size = 2) + 
  scale_color_manual(values = cctrtpal) + 
  facet_grid(.~site_trt) + 
  theme_bw()

ggsave("02_data-calcs/fig_bulk-dens.png")



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

#--look at it
dat_poros  %>% 
  ggplot(aes(poros_mineral, poros_britt)) + 
  geom_point() + 
  geom_abline()
  

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
    #--calculate using gravimetric water and bulk density
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

#--write final values to data frame

tidy_data <- 
  dat_theta %>%
  rename(vtheta_poros1 = vtheta2a,
         vtheta_poros2 = vtheta2b) %>% 
#  pivot_longer(vtheta1:vtheta2b) %>% 
  left_join(key) %>% 
  unite(site_name, sys_trt, col = "site_trt") %>% 
  mutate(sample_year = 2019) %>% 
  select(sample_year, code, site_trt, site_desc, 
         crop_trt, cc_trt, rep, plot, press_cm, vtheta_poros1, vtheta_poros2)

tidy_data %>% write_csv("02_data-calcs/dc_swrc.csv")

tidy_boyd <- 
  tidy_data %>% 
  filter(str_detect(site_trt, "Boyd")) 


# look at it --------------------------------------------------------------

tidy_boyd %>%
  filter(press_cm != 0) %>% #--can't take log of 0 :(
  pivot_longer(vtheta_poros1:vtheta_poros2) %>% 
  # plot
  ggplot(aes(press_cm, value, color = cc_trt)) + 
  # raw data
  geom_point(alpha = 0.8) + 
  geom_line(alpha = 0.8, aes(group = code)) + 
  scale_x_log10() +
  # summary of data
  #stat_summary(fun.y = mean, geom="line", size = 2) +
  #stat_summary(fun.data = "mean_se", size = 1) +
  facet_grid(name ~ site_trt, scales = "free") + 
  theme_bw()

ggsave("02_data-calcs/fig_boyd-swrc-reps.png")


tidy_data %>%
  filter(str_detect(site_trt, "Fun")) %>% 
  filter(press_cm != 0) %>% #--can't take log of 0 :(
  pivot_longer(vtheta_poros1:vtheta_poros2) %>% 
  # plot
  ggplot(aes(press_cm, value)) + 
  # raw data
  geom_point(alpha = 0.8, aes(color = as_factor(rep))) + 
  geom_line(alpha = 0.8, aes(group = code, color = as_factor(rep), linetype = cc_trt), size = 2) + 
  scale_x_log10() +
  # summary of data
  #stat_summary(fun.y = mean, geom="line", size = 2) +
  #stat_summary(fun.data = "mean_se", size = 1) +
  facet_grid(name ~ site_trt, scales = "free") + 
  theme_bw()

ggsave("02_data-calcs/fig_funcke-wrc-reps.png")

tidy_boyd %>%
  filter(press_cm != 0) %>% #--can't take log of 0 :(
  pivot_longer(vtheta_poros1:vtheta_poros2) %>% 
  # plot
  ggplot(aes(press_cm, value, color = cc_trt)) + 
  # raw data
  #geom_point(alpha = 0.8) + 
  #geom_line(alpha = 0.8, aes(group = code)) + 
  scale_x_log10() +
  # summary of data
  stat_summary(fun.y = mean, geom="line", size = 2) +
  stat_summary(fun.data = "mean_se", size = 1) +
  facet_grid(name ~ site_trt, scales = "free") + 
  theme_bw()

ggsave("02_data-calcs/fig_boyd-swrc-means.png")



#--bulk densities?
dat_soil %>% 
  left_join(key) %>% 
  unite(site_name, sys_trt, col = "site_trt") %>% 
  ggplot(aes(site_trt, bulkden_gcm3, color = cc_trt)) + 
  geom_point(aes(color = cc_trt)) + 
  stat_summary(fun.y = mean, geom = "point", size = 2) + 
  stat_summary(fun.data = "mean_se", size = 1)

ggsave("02_data-calcs/fig_bulk-dens.png")

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
delta <- 0.5 #--how big of a difference is meaningful? say 0.5
sigma <- 0.1 #--this is the amount of variation

(4 * (1.96 + 1.28)^2 * sigma ) / (delta^2) #--I had the power, I took 20 samples. Argh. 
