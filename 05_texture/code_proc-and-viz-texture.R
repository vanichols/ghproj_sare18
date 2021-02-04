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


# figure ------------------------------------------------------------------

#--can combine boyd42
dat_pie <- 
  dat %>% 
  pivot_longer(Clay:Sand) %>% 
  group_by(site, name) %>% 
  summarise(value  = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from =  name, values_from = value)  %>% 
  mutate_if(is.numeric, round, 0) %>% 
  mutate(Sand = 100 - Clay - Silt) %>% 
  pivot_longer(Clay:Silt) %>% 
  mutate(value = value/100)
  

myorder <- c("Clay", "Silt", "Sand")

#--try using purrr
dat_pie2 <- 
  dat_pie %>% 
  mutate(name = factor(name, levels = myorder),
         name2 = fct_rev(name),
         value = round(value, 2)) %>% 
  arrange(name2) %>% 
  group_by(site) %>% 
  nest() %>% 
  mutate(data2 = data %>% map(. %>% 
                                arrange(name2) %>% 
                                mutate(half = value/2,
                                          prev = lag(value),
                                          prev = ifelse(is.na(prev), 0, prev),
                                          cumprev = cumsum(prev),
                                          pos = half + cumprev) %>% 
                               select(-c(half, prev, cumprev)))) %>% 
  unnest(data2)


dat_pie2 %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) +
  geom_text(aes(y = pos,
                label = percent(value, accuracy = 2)), size = 5) +
  scale_fill_manual(values = c("Clay" = "brown", "Silt" = "tan", "Sand" = "gold2")) + 
  facet_grid(.~site)



# nice figure -------------------------------------------------------------

dat_pie2 %>% 
  mutate(site = case_when(
    site == "Boyd42" ~ "Boone",
    site == "Funcke" ~ "Greene",
    site == "Stout" ~ "Washington",
    TRUE ~ "other")
    ) %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = pos,
                label = percent(value, accuracy = 2)), size = 5) +
  scale_fill_manual(values = c("Clay" = "brown", "Silt" = "tan", "Sand" = "gold2")) + 
  facet_grid(.~site) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)),
        legend.position = "bottom") +
  labs(fill = NULL)
  
ggsave("fig_texture.png")

  
