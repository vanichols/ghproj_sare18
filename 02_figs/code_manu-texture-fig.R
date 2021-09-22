#--make texture fig
#--updated: 2/4/2021
#           2/22/2021 - added organic matters to pie chart
#           3/3/2021 - separating central site by crop sys
#           9/22/2021 - putting in supp mat


rm(list = ls())
library(patchwork)
library(tidyverse)
library(scales)
library(purrr)
library(PFIswhc)

library(lme4)
library(lmerTest)
library(emmeans)

dat <- 
  sare_texture %>% 
  left_join(sare_plotkey) %>% 
  unite(site_name, sys_trt, col = "site_sys", sep = "-")

om <- 
  sare_om %>% 
  left_join(sare_plotkey) %>% 
  unite(site_name, sys_trt, col = "site_sys", sep = "-")


pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"


# quick stats -------------------------------------------------------------

dst <- 
  dat %>% 
  left_join(om) %>% 
  mutate(repid = paste(site_sys, rep)) %>% 
  select(repid, cc_trt, site_sys, om, clay, silt)

dst
fom <- lmer(om ~ site_sys * cc_trt + (1|repid), data = dst)
anova(fom)

#--no effect of cover crop
emmeans(fom,  ~cc_trt|site_sys)

# figure, averaged over all plots  ------------------------------------------------------------------

#--don't combine cetnral sys
dat_pie <- 
  dat %>% 
  select(site_sys, cc_trt, clay, silt, sand) %>% 
  pivot_longer(clay:sand) %>% 
  group_by(site_sys, name) %>% 
  summarise(value  = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from =  name, values_from = value)  %>% 
  mutate_if(is.numeric, round, 0) %>% 
  mutate(sand = 100 - clay - silt) %>% 
  pivot_longer(clay:silt) %>% 
  mutate(value = value/100)

dat_pie %>% 
  mutate(value = value * 100)


myorder <- c("clay", "silt", "sand")

#--try using purrr
dat_pie2 <- 
  dat_pie %>% 
  mutate(name = factor(name, levels = myorder),
         name2 = fct_rev(name),
         value = round(value, 2)) %>% 
  arrange(name2) %>% 
  group_by(site_sys) %>% 
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

#--nice fig
dat_pie2 %>% 
  left_join(om %>% 
              group_by(site_sys) %>% 
              summarise(om = round(mean(om, na.rm = T), 1))) %>% 
  mutate(
    site = site_sys,
    site = paste0(site, "\n", om, "% OM")
  ) %>% 
  ggplot(aes(x = "", y = value, fill = name)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = pos,
                label = percent(value, accuracy = 2)), size = 5) +
  scale_fill_manual(values = c("clay" = pfi_brn, "silt" = pfi_red, "sand" = pfi_orng)) + 
  facet_wrap(~site, ncol = 2) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)),
        legend.position = "bottom") +
  labs(fill = NULL)

# cover cover plots and no-cover separate----------------------------------------------------------

dat_pie_cc <- 
  dat %>% 
  select(site_sys, cc_trt, clay, silt, sand) %>% 
  pivot_longer(clay:sand) %>% 
  group_by(site_sys, cc_trt, name) %>% 
  summarise(value  = mean(value, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(names_from =  name, values_from = value)  %>% 
  mutate_if(is.numeric, round, 0) %>% 
  mutate(sand = 100 - clay - silt) %>% 
  pivot_longer(clay:silt) %>% 
  mutate(value = value/100)


myorder <- c("clay", "silt", "sand")

#--use purrr to amek labels
dat_pie2_cc <- 
  dat_pie_cc %>% 
  mutate(name = factor(name, levels = myorder),
         name2 = fct_rev(name),
         value = round(value, 2)) %>% 
  arrange(name2) %>% 
  group_by(site_sys, cc_trt) %>% 
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

dat_pie2_cc


#--fig
dat_pie2_cc %>% 
  # left_join(om %>% 
  #             group_by(site_sys, cc_trt) %>% 
  #             summarise(om = round(mean(om, na.rm = T), 1))) %>% 
  mutate(
    cc_trt = case_when(
      grepl("cc", cc_trt) ~ "Rye Cover Crop",
      grepl("no", cc_trt) ~ "No Cover",
      TRUE ~ cc_trt),
    site = site_sys,
    site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain")) 
    #site = paste0(site, "\n", om, "% OM")
  ) %>% 
  ggplot(aes(x = "", y = value, fill = name, alpha = name)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) +
  geom_text(aes(y = pos,
                label = paste(round(value, 4)*100, "%")),
                #label = percent(value, accuracy = 4)), 
            size = 5) +
  scale_fill_manual(values = c("clay" = pfi_brn, "silt" = pfi_red, "sand" = pfi_orng)) + 
  facet_grid(cc_trt~site_sys) +
  scale_alpha_manual(values = c(0.4, 0.4, 1)) +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = rel(1.5)),
        legend.text = element_text(size = rel(1.3)),
        legend.position = "bottom") +
  labs(fill = NULL) +
  guides(alpha = F)
  

ggsave("02_figs/fig_manu-text-by-cctrt.png")


