#--make curves w/ obs and predictions (maybe)
#--updated: 3/4/2021


rm(list = ls())
library(tidyverse)
library(scales)
library(purrr)
library(PFIswhc)

library(lme4)
library(lmerTest)
library(emmeans)


# theme and colors --------------------------------------------------------

theme_set(theme_bw())

pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"


# data --------------------------------------------------------------------

fdat <- 
  read_csv("01_fit-models/dat_gard-parms-eu.csv") %>% 
  select(plot_id, term, estimate) %>% 
  pivot_wider(names_from = term, values_from = estimate) %>% 
  expand_grid(., x = c(seq(0.01, 1, 0.01), 2, 3, 4, seq(5, 500, 1))) %>% 
  mutate(exp1 = Ths - Thr,
         exp2 = x^scal,
         exp4 = 1 + (alp*exp2),
         exp5 = exp4^-1,
         exp9 = exp4^-2,
         y = Thr + (exp1 * exp5)) %>% 
  left_join(sare_plotkey) %>% 
  unite(site_name, sys_trt, col = "site_sys", sep = "-") %>% 
  mutate(site_sys2 = ifelse(site_sys %in% c("East-grain", "West-grain"), 
                            paste(site_sys, "\n(commercial farm)", sep = ""),
                            site_sys)) %>% 
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye cover crop",
    grepl("no", cc_trt) ~ "No cover",
    TRUE ~ cc_trt),
    site_sys = factor(site_sys, levels = c("West-grain", "East-grain", 
                                           "Central-silage", "Central-grain")),
    site_sys2 = factor(site_sys2, levels = c("West-grain\n(commercial farm)", 
                                             "East-grain\n(commercial farm)", 
                                             "Central-silage", "Central-grain"))
  ) %>% 
  #--change to kpa
  mutate(press_cm = x,
         press_kpa = press_cm * 0.0980665,
         vtheta = y)


eus <- 
  fdat %>% 
  select(field_id, site_sys, cc_trt, rep) %>% 
  distinct() %>% 
  arrange(field_id, site_sys, cc_trt, rep)

dat <- 
  sare_pressure %>% 
  left_join(sare_plotkey) %>% 
  unite(site_name, sys_trt, col = "site_sys", sep = "-") %>% 
  mutate(site_sys2 = ifelse(site_sys %in% c("East-grain", "West-grain"), 
                            paste(site_sys, "\n(commercial farm)", sep = ""),
                            site_sys)) %>% 
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye cover crop",
    grepl("no", cc_trt) ~ "No cover",
    TRUE ~ cc_trt),
    site_sys = factor(site_sys, levels = c("West-grain", "East-grain", 
                                           "Central-silage", "Central-grain")),
    site_sys2 = factor(site_sys2, levels = c("West-grain\n(commercial farm)", 
                                             "East-grain\n(commercial farm)", 
                                           "Central-silage", "Central-grain")) 
  ) %>% 
  #--change to kpa
  mutate(press_kpa = press_cm * 0.0980665,
         cc_trt = fct_rev(cc_trt))
    
davg <- 
  dat %>% 
  group_by(site_sys, cc_trt, press_cm) %>% 
  summarise(vtheta = mean(vtheta, na.rm  = T)) 





# figure, smoothed and averaged --------------------------------------------------------

fdat_avg <- 
  fdat %>% 
  group_by(site_sys, site_sys2, cc_trt, press_cm) %>% 
  summarise(vtheta = mean(vtheta, na.rm = T))

ggplot() + 
  geom_line(data = fdat_avg, 
            aes(press_cm, vtheta, color = cc_trt), 
            size = 1.5) +
  geom_line(data = fdat, 
            aes(press_cm, vtheta, color = cc_trt, group = plot_id), 
            size = 1, 
            alpha = 0.5, 
            linetype = "dotted") +
  #geom_line(data = davg, aes(vtheta, press_cm, color = cc_trt), size = 3) +
  scale_color_manual(values = c("Rye cover crop" = pfi_green,
                                "No cover" = pfi_brn)) +
  facet_grid(.~site_sys2) + 
  scale_y_continuous(labels = label_percent(accuracy = 2)) +
  labs(y = "Volumetric Water (vol%)",
       color = NULL,
       x = "Soil Matric Potential (cmH2O)") + 
  theme(strip.text = element_text(size = rel(1.2)),
        strip.background = element_blank(),
        legend.position = "bottom", 
        legend.text = element_text(size = rel(1)),
        axis.text = element_text(size = rel(1)),
        #axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.title = element_text(size = rel(1))
  )

ggsave("02_figs/fig_manu-curves-dotted.png", width = 8.85, height = 3.72)  


