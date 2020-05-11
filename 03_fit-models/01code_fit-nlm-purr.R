############################
# author: Gina (vnichols@iastate.edu)
# created: may 8 2020
# last modified: May 11 2020 (continue model fitting)
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
?SSgardner


#--data
rd <- read_csv("02_data-calcs/dc_swrc.csv") %>% 
  select(code, cc_trt, press_cm, vtheta_poros1) %>% 
  mutate(x = ifelse(press_cm == 0, 0.01, press_cm), 
         y = vtheta_poros1) %>% 
  separate(code, into = c("site", "plot"), remove = F) 


rd %>% 
  filter(site == "F") %>% 
  filter(cc_trt == "cc")
  


rd %>% 
  filter(site == "F") %>% 
  ggplot(aes(x, y, color = code)) + 
  geom_point() + 
  geom_line() + 
  guides(color = F) + 
  facet_wrap(~code)

eus <- 
  rd %>% 
  select(code, cc_trt) %>% 
  separate(code, into = c("site", "plot"), remove = F) %>% 
  select(code, site, cc_trt)

# 1 - thr - threshold moisture content at very high pressures
# 2 - ths - saturated moisture content
# 3 - alp - inverse of air-entry potential
# 4 - nscal - index for pore-size distribution

#--note: SSgard is for grouped data, SSgardener is not
#-- their parameter names are different


#--fit model to all data at once (use SSgardner)
#--note: nlsLM is a modified version of nls
#--in HydroMe example they don't supply starting vals


nls_gardfit <- function(x){
  
  nls(y ~ SSgardner(x, thr, ths, alp, nscal),
      data = x)
}

#--map function to data
dat_parms <-
  rd %>%
  select(code, x, y) %>% 
  nest(data = c(x,y)) %>%
  mutate(mod = data %>% map(possibly(nls_gardfit, NULL)),
         is_null = mod %>% map_lgl(is.null)) %>% 
  filter(is_null == 0) %>% 
  mutate(res = mod %>% map(possibly(~broom::tidy(.), NULL))) %>% 
  unnest(cols = c(res)) %>% 
    select(code, term:p.value)

  
  #--is F-xcc never converging?
  dat_parms %>%
    left_join(eus) %>% 
    ggplot(aes(code, estimate, color = site)) + 
    geom_point() + 
    geom_linerange(aes(ymin = estimate - std.error,
                       ymax = estimate + std.error)) + 
    facet_grid(cc_trt ~ term, scales = "free") + 
    coord_flip()
