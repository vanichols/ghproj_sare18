############################
# author: Gina (vnichols@iastate.edu)
# created: may 8 2020
# last modified: May 11 2020 (continue model fitting)
#                March 2 2021 (update using package data)
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
  select(plot_id, site_name, sys_trt, cc_trt, press_cm, vtheta_grav) %>% 
  mutate(x = ifelse(press_cm == 0, 0.038, press_cm), 
         y = vtheta_grav) %>% 
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

tst %>% 


newdata <- data.frame(x = c(0.01, 1, 2, 3, 4, seq(5, 500, 1)))

newdata %>% 
  mutate(y = predict(f1, newdata = newdata)) %>% 
  as_tibble() %>% 
  ggplot(aes(x, y)) + 
  geom_line()


ps <- 
  read_csv("03_fit-models/03dat_gard-parms-eu.csv") %>% 
  select(plot_id, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate)


nd <- 
  ps %>% 
  expand_grid(., x = c(0.01, 1, 2, 3, 4, seq(5, 500, 1))) %>% 
  mutate(exp1 = Ths - Thr,
         exp2 = x^scal,
         exp4 = 1 + (alp*exp2),
         exp5 = exp4^-1,
         exp9 = exp4^-2,
         y = Thr + (exp1 * exp5)) 

ps %>% 
  filter(scal == max(scal))


nd %>% 
  filter(plot_id %in% c("B42-p27", "F-5cc")) %>% 
  ggplot(aes(x, y, group = plot_id, )) + 
  geom_line(aes(color = plot_id)) + 
  labs(title = "F is min scal (0.4), B42 is max (1.5)")

#--curious, can I make a staright line?


  ps %>% 
  filter(plot_id %in% c("B42-p27")) %>%
    bind_rows(  ps %>% 
                  filter(plot_id %in% c("B42-p27"))) %>% 
    bind_rows(  ps %>% 
                  filter(plot_id %in% c("B42-p27"))) %>% 
  mutate(scal = c(0.4, 1, 1.5)) %>% 
  expand_grid(., x = c(0.01, 1, 2, 3, 4, seq(5, 500, 1))) %>% 
  mutate(exp1 = Ths - Thr,
         exp2 = x^scal,
         exp4 = 1 + (alp*exp2),
         exp5 = exp4^-1,
         exp9 = exp4^-2,
         y = Thr + (exp1 * exp5)) %>% 
    ggplot(aes(x, y, group = scal, )) + 
    geom_line(aes(color = scal), size = 2) + 
    scale_color_viridis_c()
  
#--so scal = 1 --> all size pores evenly distributed
#--scal > 1 --> more small pores (?)
  #--scale < 1 --> more large pores?


ps %>% 
  filter(alp == max(alp))


nd %>% 
  filter(plot_id %in% c("B42-p27", "St-4cc")) %>% 
  ggplot(aes(x, y, group = plot_id, )) + 
  geom_line(aes(color = plot_id)) + 
  labs(title = "St is max alp (0.28), B42 is min (0.001)")


# data by trt -------------------------------------------------------------

dt <- 
  read_csv("03_fit-models/03dat_gard-parms-trt.csv") %>% 
  select(site_sys, cc_trt, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate) %>% 
  expand_grid(., x = c(0.01, 1, 2, 3, 4, seq(5, 500, 1))) %>% 
  mutate(exp1 = Ths - Thr,
         exp2 = x^scal,
         exp4 = 1 + (alp*exp2),
         exp5 = exp4^-1,
         exp9 = exp4^-2,
         y = Thr + (exp1 * exp5)) 

dt %>% 
  ggplot(aes(x, y, color = cc_trt)) + 
  geom_line() + 
  facet_grid(.~site_sys)

