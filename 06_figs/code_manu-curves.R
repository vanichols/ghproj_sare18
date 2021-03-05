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

theme_set(theme_bw())


dat <- 
  sare_pressure %>% 
  left_join(sare_plotkey) %>% 
  unite(site_name, sys_trt, col = "site_sys", sep = "-") %>% 
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye Cover Crop",
    grepl("no", cc_trt) ~ "No Cover",
    TRUE ~ cc_trt),
    site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain")) 
  ) %>% 
  #--change to kpa
  mutate(press_kpa = press_cm * 0.0980665)
    
davg <- 
  dat %>% 
  group_by(site_sys, cc_trt, press_kpa) %>% 
  summarise(vtheta_grav = mean(vtheta_grav, na.rm  = T)) 



pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"
show_col(pfi_red)


# figure ------------------------------------------------------------------

ggplot() + 
  geom_line(data = dat, aes(vtheta_grav, press_kpa, color = cc_trt, group = plot_id), alpha = 0.5) +
  geom_line(data = davg, aes(vtheta_grav, press_kpa, color = cc_trt), size = 3) +
  scale_color_manual(values = c("Rye Cover Crop" = pfi_green,
                                "No Cover" = pfi_brn)) +
  facet_grid(.~site_sys) + 
  scale_y_log10() +
  scale_x_continuous(labels = label_percent(accuracy = 2)) +
  labs(x = "Volumetric Water (%)",
       color = NULL,
       y = "Soil Matric Potential (kPa)\nLog-scale") + 
  theme(strip.text = element_text(size = rel(1.3)),
        strip.background = element_blank(),
        legend.position = "top", 
        legend.text = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        #axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.title = element_text(size = rel(1.3))
  )

ggsave("06_figs/fig_manu-curves.png")  
