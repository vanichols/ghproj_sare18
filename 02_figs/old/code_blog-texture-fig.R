#--make texture fig
#--updated: 2/4/2021
#           2/22/2021 - added organic matters to pie chart


rm(list = ls())
library(tidyverse)
library(scales)
library(purrr)
library(PFIswhc)

dat <- 
  sare_texture %>% 
  left_join(sare_plotkey)

om <- 
  sare_om %>% 
  left_join(sare_plotkey)

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
  select(site_name, sys_trt, cc_trt, clay, silt, sand) %>% 
  pivot_longer(clay:sand) %>% 
  group_by(site_name, name) %>% 
  summarise(value  = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from =  name, values_from = value)  %>% 
  mutate_if(is.numeric, round, 0) %>% 
  mutate(sand = 100 - clay - silt) %>% 
  pivot_longer(clay:silt) %>% 
  mutate(value = value/100)


myorder <- c("clay", "silt", "sand")

#--try using purrr
dat_pie2 <- 
  dat_pie %>% 
  mutate(name = factor(name, levels = myorder),
         name2 = fct_rev(name),
         value = round(value, 2)) %>% 
  arrange(name2) %>% 
  group_by(site_name) %>% 
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

dat_pie2

#--
dat_pie2 %>% 
  left_join(om %>% 
  group_by(site_name) %>% 
  summarise(om = round(mean(om, na.rm = T), 1)))

# nice figure -------------------------------------------------------------

dat_pie2 %>% 
  left_join(om %>% 
              group_by(site_name) %>% 
              summarise(om = round(mean(om, na.rm = T), 1))) %>% 
  mutate(site = case_when(
    site_name == "Central" ~ "Boone",
    site_name == "West" ~ "Greene",
    site_name == "East" ~ "Washington",
    TRUE ~ "other"),
    site = paste0(site, ", ", om, "% OM")
  ) %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = pos,
                label = percent(value, accuracy = 2)), size = 5) +
  scale_fill_manual(values = c("clay" = pfi_brn, "silt" = pfi_red, "sand" = pfi_orng)) + 
  facet_grid(.~site) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)),
        legend.position = "bottom") +
  labs(fill = NULL)

ggsave("06_figs/fig_texture.png")

