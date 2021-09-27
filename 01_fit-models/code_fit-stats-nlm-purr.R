############################
# author: Gina (vnichols@iastate.edu)
# created: may 8 2020
# last modified: May 11 2020 (continue model fitting)
#                March 2 2021 (update using package data)
#                may 24 2021 (rerunning...still works?)
#                june 24 2021 (rerunning with 3.8 instead of 2.5)
#                sept 24 2021 (getting stats for manu)
#
# purpose: fit non-linear models
# 
# inputs: dc_swrc
#
# outputs:
#
# notes: Using this as reference:https://www.nature.com/articles/s41598-019-54449-8
#
##############################


rm(list = ls())
library(lme4)
library(emmeans)
library(tidyverse)
library(HydroMe)
library(nlraa)
library(PFIswhc)
??SSgardner
?SSgard

#--data
rd <- 
  sare_pressure %>%
  left_join(sare_plotkey) %>% 
  select(plot_id, site_name, sys_trt, cc_trt, press_cm, vtheta) %>% 
  mutate(x = ifelse(press_cm == 0, 0.038, press_cm), #--so models will converge 
         y = vtheta) %>% 
  unite(site_name, sys_trt, col = "site_sys")


rd %>% 
  ggplot(aes(x, y, color = plot_id)) + 
  geom_point() + 
  geom_line() + 
  guides(color = F) + 
  facet_wrap(~site_sys)

eus <- 
  rd %>% 
  select(plot_id, site_sys, cc_trt) 

# 1 - thr - threshold moisture content at very high pressures
# 2 - ths - saturated moisture content
# 3 - alp - inverse of air-entry potential
# 4 - nscal - index for pore-size distribution

#--note: SSgard is for grouped data, SSgardener is not
#-- their parameter names are different


#--fit model to all data at once (use SSgardner)
#--note: nlsLM is a modified version of nls
#--in HydroMe example they don't supply starting vals

#--test on one group of data
tst <- 
  rd %>% 
  filter(site_sys == "Central_silage",
         cc_trt == "no") %>% 
  ungroup() 

tst2 <- 
  rd %>% 
  filter(site_sys == "Central_silage") %>% 
  ungroup() %>% 
  select(cc_trt, x,y) %>% 
  mutate(cc_trt = as.factor(cc_trt))


#--gard, works
f1 <- nls(y ~ SSgard(x, Thr, Ths, alp, scal),
    data = tst)

#--van gen 5 parm, doesn't work
nlsList(y ~ SSvgm(x, thr, ths, alp, nscal, mscal)|cc_trt,
    data = tst)

#--van gen 4 parm, doesn't work
nlsList(y ~ SSvgm4(x, Thr, Ths, alp, nscal)|cc_trt,
    data = tst,
    control = nls.lm.control(maxiter=200,options(warn=-1)))

#--try new library
library(soilphysics)
#--this give me starting values
#fitsoilwater(tst$y, tst$x)

f2 <- 
  nls(y ~ soilwater(x, theta_R, theta_S, alpha, n), 
           data = tst, 
           start = list(theta_R = 0.239, 
                        theta_S = 0.4224,
                        alpha = 0.01,
                        n = 2))


AIC(f1, f2)


# choose gardener ---------------------------------------------------------


nls_gardfit <- function(x){
  
  nls(y ~ SSgard(x, Thr, Ths, alp, scal),
      data = x)
}



#--map function to data, each eu (doesn't run?)
dparms_eu <-
  rd %>%
  select(plot_id, x, y) %>% 
  nest(data = c(x,y)) %>%
  mutate(mod = data %>% map(possibly(nls_gardfit, NULL)),
         is_null = mod %>% map_lgl(is.null)) %>% 
  filter(is_null == 0) %>% 
  mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
  unnest(cols = c(res)) %>% 
    select(plot_id, term:p.value)

dparms_eu %>% write_csv("01_fit-models/dat_gard-parms-eu.csv")

#--map function to data, each trt (pool reps) (doesn't run?)

dparms_gr <-
  rd %>%
  ungroup() %>% 
  select(site_sys, cc_trt, x, y) %>% 
  nest(data = c(x,y)) %>%
  mutate(mod = data %>% map(possibly(nls_gardfit, NULL)),
         is_null = mod %>% map_lgl(is.null)) %>% 
  filter(is_null == 0) %>% 
  mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
  unnest(cols = c(res)) %>% 
  select(site_sys, cc_trt, term:p.value)

dparms_gr %>% write_csv("01_fit-models/dat_gard-parms-trt.csv")



# stats -------------------------------------------------------------------

#--the above code sin't running any more, so just read in the results it wrote when it did run, sigh
dparms_eu <- 
  read_csv("01_fit-models/dat_gard-parms-eu.csv") %>% 
  left_join(sare_plotkey) %>% 
  mutate(site_sys = paste(site_name, sys_trt),
         block = paste(site_name, "B", rep)) %>% 
  left_join(sare_texture)


# air entry potential -----------------------------------------------------

m1 <- lmer(estimate ~ site_sys*cc_trt + (1|block), data = dparms_eu %>% filter(term == "alp"))
m1_sand <- lm(estimate ~ site_sys*cc_trt + sand, data = dparms_eu %>% filter(term == "alp"))

anova(m1)
anova(m1_sand)
summary(m1_sand)

#--p-values
pairs(emmeans(m1, ~cc_trt|site_sys))
#--estimates
emmeans(m1, ~site_sys)
emmeans(m1, ~cc_trt|site_sys)

#--texture by site
sare_texture %>% 
  left_join(sare_plotkey) %>% 
  mutate(site_sys = paste(site_name, sys_trt)) %>% 
  group_by(site_sys) %>% 
  summarise(sand = mean(sand, na.rm = T)) %>% 
  left_join(emmeans(m1, ~site_sys) %>% 
              broom::tidy()) %>% 
  ggplot(aes(sand, estimate)) + 
  geom_point() + 
  geom_label(aes(label = site_sys))


#--texture by site+cc_trt
sare_texture %>% 
  left_join(sare_plotkey) %>% 
  mutate(site_sys = paste(site_name, sys_trt)) %>% 
  group_by(site_sys, cc_trt) %>% 
  summarise(sand = mean(sand, na.rm = T)) %>% 
  left_join(
    emmeans(m1, ~cc_trt|site_sys) %>% 
              broom::tidy()
  ) %>% 
  ggplot(aes(sand, estimate)) + 
  geom_point() + 
  geom_label(aes(label = site_sys, fill = cc_trt))


# pores size dist -----------------------------------------------------

m2 <- lmer(estimate ~ site_sys*cc_trt + (1|block), data = dparms_eu %>% filter(term == "scal"))

anova(m2)
pairs(emmeans(m2, ~cc_trt|site_sys))
emmeans(m2, ~site_sys)
emmeans(m2, ~cc_trt|site_sys)

sare_texture %>% 
  left_join(sare_plotkey) %>% 
  mutate(site_sys = paste(site_name, sys_trt)) %>% 
  group_by(site_sys) %>% 
  summarise(sand = mean(sand, na.rm = T)) %>% 
  left_join(emmeans(m2, ~site_sys) %>% 
              broom::tidy()) %>% 
  ggplot(aes(sand, estimate)) + 
  geom_point() + 
  geom_label(aes(label = site_sys))

#--look at it
#--shoudl use 95%cis to emphasize no differences
dparms_gr %>% 
  ggplot(aes(site_sys, estimate, color = cc_trt)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error,
                     ymax = estimate + std.error)) + 
  facet_wrap(~term, scales = "free")

# 1 - thr - threshold moisture content at very high pressures
# 2 - ths - saturated moisture content
# 3 - alp - inverse of air-entry potential
# 4 - nscal - index for pore-size distribution
