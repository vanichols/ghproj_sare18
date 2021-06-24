############################
# author: Gina (vnichols@iastate.edu)
# created: 5/6/2021
# purpose: do stats at certain pressure values
# 
#
#
# last modified: 5/21/2021 (do rand intcp for location when using sand cov)
# 6/24/2021 (update sat to 3.8 from 2.5)
##############################


rm(list = ls())
library(tidyverse)
#remotes::install_github("vanichols/PFIswhc")
library(PFIswhc)
library(lme4)
library(lmerTest)
library(emmeans)

# 1 cm water = 0.0980665 kpa
# britt leaves it in cm water

#--data
#--note, there is a 5th no cover rep at stout where we measured the whc, but not texture
rd <- 
  sare_pressure %>%
  left_join(sare_plotkey) %>% 
  select(plot_id, site_name, sys_trt, cc_trt, rep, press_cm, vtheta) %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = F) %>% 
  mutate(rep_id = paste(site_sys, rep)) %>% 
  left_join(sare_texture) %>% 
  left_join(sare_om) %>% 
  filter(!is.na(sand)) #--get rid of extra east grain no cover plot


# saturation ----------------------------------------------------------

sat <- 
  rd %>% 
  filter(press_cm %in% c(0)) %>% 
  group_by(site_name, site_sys, cc_trt, rep_id) %>% 
  summarise(vtheta = mean(vtheta, na.rm = T),
            sand = mean(sand, na.rm = T))

sat %>% 
  ggplot(aes(cc_trt, vtheta)) + 
  geom_point(aes(size = sand)) +
  facet_grid(.~site_sys) + 
  scale_color_viridis_c()

#--sand vs sat
sat %>% 
  ggplot(aes(sand, vtheta)) + 
  geom_point() + 
  geom_smooth(method = "lm")


#--does it make sense to correct for sand at the saturation? I don't know
msat <- lmer(vtheta~cc_trt*site_sys + (1|rep_id), data = sat)
msat_sand <- lm(vtheta~cc_trt*site_sys + sand, data = sat)

emsat <- emmeans(msat, ~cc_trt|site_sys)
emsat_sand <- emmeans(msat_sand, ~cc_trt|site_sys)

pairs(emsat)  #--sig lower w/cc at west and central grain, if no sand correction
pairs(emsat_sand)  #--still lower at central grain, not at west


sat_sig <- 
  contrast(emsat_sand) %>% 
  broom::tidy() %>% 
  filter(contrast == "cc effect") %>% 
  mutate(cov = "sand", 
         param = "saturation") %>% 
  bind_rows(
    contrast(emsat) %>% 
      broom::tidy() %>% 
      filter(contrast == "cc effect") %>% 
      mutate(cov = "none", 
             param = "saturation")
    
  )

sat_sig %>% write_csv("01_fit-models/dat_sat-emmeans-diff.csv")

#emmeans(emsat_sand, ~cctrt|site_sys)

#--sand as cov
sat_res_sand <- 
  emsat_sand %>% 
  broom::tidy() %>%
  left_join(confint(emsat_sand, level = 0.95) %>%
              broom::tidy()) %>% 
  mutate(cov = "sand") 

#--no cov
sat_res_nosand <- 
  emsat %>% 
  broom::tidy() %>%
  left_join(confint(emsat, level = 0.95) %>%
              broom::tidy()) %>% 
  mutate(cov = "none") 

#--combine
sat_res <-
  sat_res_sand %>%
  bind_rows(sat_res_nosand) %>%
  mutate(param = "saturation",
         cilevel = 0.95) %>%
  select(param, everything())

sat_res %>% write_csv("01_fit-models/dat_sat-emmeans-cis.csv")

sat_res %>% 
  ggplot(aes(cc_trt, estimate, color = cov)) + 
  geom_point() + 
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) + 
  facet_grid(cov~site_sys)


# field capacity ----------------------------------------------------------

#--britt suggests doing 100
fc <- 
  rd %>% 
  filter(press_cm %in% c(100)) %>% 
  group_by(site_sys, cc_trt, rep_id) %>% 
  summarise(vtheta = mean(vtheta, na.rm = T),
            sand = mean(sand, na.rm = T))


fc %>% 
  ggplot(aes(cc_trt, vtheta)) + 
  geom_point(aes(size = sand)) + 
  facet_grid(.~site_sys) + 
  labs(title = "field capacity",
       subtitle = "need to include sand as covariate")

fc %>% 
  ggplot(aes(sand, vtheta)) + 
  geom_point() 


fc %>% 
  filter(site_sys == "East_grain")

#--physically, should include sand as covariate for field capacity
mfc <- lmer(vtheta~cc_trt*site_sys + (1|rep_id), data = fc)
anova(mfc)
mfc_sand <- lm(vtheta~cc_trt*site_sys + sand, data = fc)
anova(mfc_sand)

emfc <- emmeans(mfc, ~cc_trt|site_sys) #-no sand
contrast(emfc) #--centtal silage sig diff

emfc_sand <- emmeans(mfc_sand, ~cc_trt|site_sys) #-with sand
pairs(emfc_sand) #--central silage plus west

fc_sig <- 
    bind_rows(
  contrast(emfc_sand) %>% 
  broom::tidy() %>% 
  filter(contrast == "cc effect") %>% 
  mutate(cov = "sand", 
         param = "field capacity"),
  
  contrast(emfc) %>% 
    broom::tidy() %>% 
    filter(contrast == "cc effect") %>% 
    mutate(cov = "none", 
           param = "field capacity")
  )



 
fc_sig %>% write_csv("01_fit-models/dat_fc-emmeans-diff.csv")


#--sand as cov
fc_res_sand <- 
  emfc_sand %>% 
  broom::tidy() %>%
  left_join(confint(emfc_sand, level = 0.95) %>%
              broom::tidy()) %>% 
  mutate(cov = "sand") 

#--no cov
fc_res_nosand <- 
  emfc %>% 
  broom::tidy() %>%
  left_join(confint(emfc, level = 0.95) %>%
              broom::tidy()) %>% 
  mutate(cov = "none") 

#--combine
fc_res <-
  fc_res_sand %>%
  bind_rows(fc_res_nosand) %>%
  mutate(param = "field capacity",
         cilevel = 0.95) %>%
  select(param, everything())

fc_res %>% write_csv("01_fit-models/dat_fc-emmeans-cis.csv")

fc_res %>% 
  ggplot(aes(cc_trt, estimate, color = cov)) + 
  geom_point() + 
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) + 
  facet_grid(cov~site_sys)

fc_res %>% 
  ggplot(aes(cc_trt, estimate, color = cov)) + 
  geom_point() + 
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) + 
  facet_grid(.~site_sys)


