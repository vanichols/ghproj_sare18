# created 2/4/2021
# purpose: process raw texture data from Dustin
# last updated:

rm(list = ls())
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(forcats)
library(purrr)

# raw data ----------------------------------------------------------------

datraw <- read_excel("raw-data/rd-texture-hand-made-from-Dustin-sheet.xlsx")

dkey <- read_csv("raw-data/template_texture-msmts.csv") %>% select(-labeled) %>% mutate(Sample = textsamp_id)


# wrangle data ------------------------------------------------------------

dat <- 
  datraw %>% 
  mutate(tot = Clay + Silt + Sand + Pebbles) %>% 
  left_join(dkey) %>% 
  mutate(site = site_name, 
         site_sys_crop = paste(site_name, sys_trt, crop_trt)) %>% 
  select(site, site_sys_crop, code, cc_trt, rep, Clay, Silt, Sand)

dat %>% 
  pivot_longer(Clay:Sand) %>% 
  ggplot(aes(name, value)) + 
  geom_point(aes(color = cc_trt)) + 
  facet_grid(.~site_sys_crop)


dat %>% write_csv("05_texture/dat_texture.csv")


