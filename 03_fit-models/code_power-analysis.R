############################
# author: Gina (vnichols@iastate.edu)
# created: may 8 2020
# last modified: May 11 2020 (continue model fitting)
#                May 12 2020 (confused by df problem)
# edits: Fernando Miguez (2020-05-28)
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
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(purrr)


cctrtpal <- c("#45AD45", "#69431D")

#--data
rd <- read_csv("02_data-calcs/dc_swrc.csv") %>% 
  select(code, cc_trt, press_cm, vtheta_poros1) %>% 
  mutate(x = ifelse(press_cm == 0, 0.01, press_cm), 
         y = vtheta_poros1) %>% 
  separate(code, into = c("site", "plot"), remove = F)

#--I have 36 unique eus
rd %>% 
  select(code) %>% 
  distinct()

#--visual
rd %>% 
  ggplot(aes(x, y, color = code)) + 
  geom_point() + 
  geom_line() + 
  guides(color = F)

  rd %>% 
  filter(x == 0.01) %>% 
  group_by(site, cc_trt) %>% 
  summarise(y = mean(y)) %>% 
    pivot_wider(names_from = cc_trt, values_from = y) %>% 
  mutate(diff = no - cc)



rd_pwr <- 
  rd %>% 
  filter(x == 0.01) %>% 
  group_by(site) %>% 
  summarise(y_sd = sd(y)) %>% 
  mutate(effsize = 0.02/y_sd) 


library(pwr)
#--practice
a <- pwr.t.test(d = 0.7, sig.level = 0.05, power = 0.9, type = c("paired"))


rd_pwr %>% 
  mutate(needn = effsize %>% map_dbl(function(.) pwr.t.test(d = ., power = 0.9, type = "paired")$n))
