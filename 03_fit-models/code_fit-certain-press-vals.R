############################
# author: Gina (vnichols@iastate.edu)
# created: march 10 2021
# purpose: get values at certain pressures
# 
#
#
# last modified: 
##############################


rm(list = ls())
library(tidyverse)
library(HydroMe)
library(nlraa)
library(PFIswhc)
library(lme4)
library(lmerTest)
library(emmeans)

# 1 cm water = 0.0980665 kpa

#--data
rd <- 
  sare_pressure %>%
  left_join(sare_plotkey) %>% 
  select(plot_id, site_name, sys_trt, cc_trt, rep, press_cm, vtheta_grav) %>% 
  mutate(press_kPa = press_cm * 0.0980665) %>% 
  unite(site_name, sys_trt, col = "site_sys") %>% 
  mutate(rep_id = paste(site_sys, rep)) %>% 
  left_join(sare_texture)



# field capacity ----------------------------------------------------------

fc <- 
  rd %>% 
  filter(press_cm %in% c(50, 100)) %>% 
  group_by(site_sys, cc_trt, rep_id) %>% 
  summarise(vtheta_grav = mean(vtheta_grav, na.rm = T),
            sand = mean(sand, na.rm = T))


fc %>% 
  ggplot(aes(cc_trt, vtheta_grav)) + 
  geom_point() + 
  facet_grid(.~site_sys)


fc

m1 <- lmer(vtheta_grav~cc_trt*site_sys + (1|rep_id), data = fc)
anova(m1)

m1a <- lmer(vtheta_grav~cc_trt*site_sys + sand + (1|rep_id), data = fc)
anova(m1a)

em1 <- emmeans(m1, ~cc_trt|site_sys)
em1a <- emmeans(m1a, ~cc_trt|site_sys)

fc_sig <- 
  contrast(em1a) %>% 
  broom::tidy() %>% 
  filter(contrast == "cc effect") %>% 
  mutate(cov = "sand", 
         param = "field capacity")
 
fc_sig %>% write_csv("03_fit-models/03dat_fc-emmeans-sig.csv")

fc_res <- 
  confint(em1a, level = 0.9) %>% 
  broom::tidy() %>% 
  mutate(cov = "sand") %>% 
  bind_rows(
    confint(em1, level = 0.9) %>% 
      broom::tidy() %>% 
      mutate(cov = "no sand")
  ) %>% 
  mutate(param = "avg of 50-100, field capacity") %>% 
  select(param, everything())

fc_res %>% write_csv("03_fit-models/03dat_fc-emmeans.csv")

fc_res %>% 
  ggplot(aes(cc_trt, estimate, color = cov)) + 
  geom_point() + 
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) + 
  facet_grid(cov~site_sys)


# saturation ----------------------------------------------------------

sat <- 
  rd %>% 
  filter(press_cm %in% c(0.038)) %>% 
  group_by(site_sys, cc_trt, rep_id) %>% 
  summarise(vtheta_grav = mean(vtheta_grav, na.rm = T),
            sand = mean(sand, na.rm = T))


sat %>% 
  ggplot(aes(cc_trt, vtheta_grav)) + 
  geom_point() + 
  facet_grid(.~site_sys)


m2 <- lmer(vtheta_grav~cc_trt*site_sys + (1|rep_id), data = sat)
m2a <- lmer(vtheta_grav~cc_trt*site_sys + sand + (1|rep_id), data = sat)

em2 <- emmeans(m2, ~cc_trt|site_sys)
em2a <- emmeans(m2a, ~cc_trt|site_sys)

sat_sig <- 
  contrast(em2a) %>% 
  broom::tidy() %>% 
  filter(contrast == "cc effect") %>% 
  mutate(cov = "sand", 
         param = "saturation")

sat_sig %>% write_csv("03_fit-models/03dat_sat-emmeans-sig.csv")



sat_res <-
  confint(em2a, level = 0.9) %>%
  broom::tidy() %>%
  mutate(cov = "sand") %>%
  bind_rows(confint(em2, level = 0.9) %>%
              broom::tidy() %>%
              mutate(cov = "no sand")) %>%
  mutate(param = "saturation") %>%
  select(param, everything())

sat_res %>% write_csv("03_fit-models/03dat_sat-emmeans.csv")


sat_res %>% 
  ggplot(aes(cc_trt, estimate, color = cov)) + 
  geom_point() + 
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) + 
  facet_grid(cov~site_sys)
