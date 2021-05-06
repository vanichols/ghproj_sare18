############################
# author: Gina (vnichols@iastate.edu)
# created: aug 11 2020
# last modified: 
#
# purpose: calc pred intervals using andrea's data
# 
# inputs: 
#
# outputs:
#
# notes: 
#
##############################


rm(list = ls())
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(ggplot2)
library(stringr)


# andrea data -------------------------------------------------------------

adraw <- read_excel("04_pred-intervals/Basche2016ccsw.xlsx") %>% 
  rename(bulkden_se = 4)

ad <- 
  adraw %>%  
  pivot_longer(bulkden:paw_se) %>% 
  filter(!grepl("_se", name)) %>% 
  left_join(
    ad %>%  
      pivot_longer(bulkden:paw_se) %>% 
      filter(grepl("_se", name)) %>% 
      rename(value_se = value) %>% 
      mutate(name = str_replace_all(name, "_se", ""))
  )

ad %>% 
  filter(name != "bulkden") %>% 
  ggplot(aes(treatment, value)) + 
  geom_point() + 
  geom_linerange(aes(x = treatment, ymin = value - value_se, ymax = value + value_se)) +
  facet_grid(depth_cm~name)

ad %>% 
  mutate(n = 4,
         value_sd = value_se * sqrt(n-1),
         pred_int = 1.96 * (1 + 1/sqrt(n)) * value_sd,
         conf_int = 1.96 * (value_sd/sqrt(n))) %>% 
  filter(name != "bulkden") %>% 
  ggplot(aes(treatment, value)) + 
  geom_point() + 
  geom_linerange(aes(x = treatment, ymin = value - conf_int, ymax = value + conf_int), color = "blue", size = 3) +
  geom_linerange(aes(x = treatment, ymin = value - pred_int, ymax = value + pred_int), color = "red") +
  facet_grid(depth_cm~name)


#--do it on ratio
adraw %>%  
  pivot_longer(bulkden:paw_se) %>% 
  filter(!grepl("_se", name)) %>%
  pivot_wider(names_from = treatment, values_from = value) %>% 
  rename(cc_value = cc,
         none_value = none) %>% 
  left_join(
    adraw %>%  
      pivot_longer(bulkden:paw_se) %>% 
      filter(grepl("_se", name)) %>%
      mutate(n = 4,
             value_sd = value * sqrt(n - 1)) %>%
      mutate(name = str_replace_all(name, "_se", "")) %>% 
      select(-value, -n) %>% 
      pivot_wider(names_from = treatment, values_from = value_sd) %>% 
      rename(cc_sd = cc,
             none_sd = none)
    ) %>% 
  mutate(
    lrr = cc_value/none_value,
    lrr_sd = sqrt(cc_sd^2/4 + none_sd^2/4),
    conf_int = 1.96 * lrr_sd/sqrt(4),
    pred_int = 1.96 * (1 + 1/sqrt(4)) * lrr_sd) %>% 
  #--figure
  ggplot(aes(name, lrr)) + 
  geom_point() + 
  geom_linerange(
    aes(ymin = lrr - conf_int,
        ymax = lrr + conf_int),
    color = "blue", size = 3) +
  geom_linerange(
    aes(ymin = lrr - pred_int, 
        ymax = lrr + pred_int), 
    color = "red") +
  geom_hline(yintercept = 1) +
  facet_grid(.~depth_cm)



  