#--make texture fig
#--updated: 2/4/2021


rm(list = ls())
library(tidyverse)
library(scales)
library(purrr)
dat <- read_csv("05_texture/dat_texture.csv")


pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"
show_col(pfi_red)


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
  mutate(data2 = data %>% purrr::map(. %>% 
                                arrange(name2) %>% 
                                mutate(half = value/2,
                                       prev = lag(value),
                                       prev = ifelse(is.na(prev), 0, prev),
                                       cumprev = cumsum(prev),
                                       pos = half + cumprev) %>% 
                                select(-c(half, prev, cumprev)))) %>% 
  unnest(data2)

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
  scale_fill_manual(values = c("Clay" = pfi_brn, "Silt" = pfi_red, "Sand" = pfi_orng)) + 
  facet_grid(.~site) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)),
        legend.position = "bottom") +
  labs(fill = NULL)

ggsave("06_figs/fig_texture.png")

