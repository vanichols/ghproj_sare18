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


# fig settings ------------------------------------------------------------

theme_set(theme_bw())
pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"
show_col(pfi_red)


# data --------------------------------------------------------------------

#--field capacity and saturation
fc <- 
  read_csv("01_fit-models/dat_fc-emmeans-cis.csv") %>% 
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye Cover Crop",
    grepl("no", cc_trt) ~ "No Cover",
    TRUE ~ cc_trt),
    site_sys = str_replace_all(site_sys, "_", "-"),
    site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain"))
    ) 

sat <- 
  read_csv("01_fit-models/dat_sat-emmeans-cis.csv") %>% 
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye Cover Crop",
    grepl("no", cc_trt) ~ "No Cover",
    TRUE ~ cc_trt),
    site_sys = str_replace_all(site_sys, "_", "-"),
    site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain"))
    ) 


sat %>% 
  ggplot(aes(cc_trt, estimate, color = cov)) + 
  geom_point() + 
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) + 
  facet_grid(.~site_sys)




# figs --------------------------------------------------------------------

#both on one fig

dat_both <- 
  sat %>% 
  bind_rows(fc) %>% 
  mutate(#estimate = round(estimate, 2),
         param = ifelse(param == "saturation", "Saturation", "Field capacity"), 
         thing = ifelse(cov == "sand", "Sand as a co-variate", "No covariates"),
         site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain")),
         param = factor(param, levels = c("Saturation", "Field capacity"))
  ) 

dat_sig <- 
  dat_both %>% 
  mutate(sig = ifelse(param == "Field capacity" & site_sys == "Central-silage" & cc_trt == "No Cover", "*", 
                      ifelse(param == "Field capacity" & site_sys == "West-grain" & cc_trt == "No Cover", "*", " "))) %>% 
  filter(sig == "*", cov == "sand") %>% 
  select(-cov)
  

dat_both %>% 
  ggplot(aes(cc_trt, estimate, alpha = thing, color = cc_trt)) + 
  geom_point(position = position_dodge(width = 0.1), aes(pch = thing), size = 2.5) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.1), size = 1) +
  geom_text(data = dat_sig, x = 1.5, y = 0.32, label = "*", size = 13, show.legend = FALSE) +
  labs(y = "Volumetric water content (%)",
       x = NULL,
       color = NULL, 
       alpha = NULL,
       pch = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 2)) +
  scale_color_manual(values = c(pfi_brn, pfi_green)) + 
  scale_alpha_manual(values = c(0.2, 1)) +
  facet_grid(param ~ site_sys, scales = "free") + 
  guides(color = F,
         label = F,
         text = F) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)), 
        legend.position = "bottom") 


ggsave("02_figs/fig_manu-sat-fc-ses-sand.png")

#--get sig ones
fc_sig <- read_csv("01_fit-models/dat_fc-emmeans-diff.csv")
sat_sig <- read_csv("01_fit-models/dat_sat-emmeans-diff.csv")
