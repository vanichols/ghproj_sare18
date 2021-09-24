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




# figs --------------------------------------------------------------------

#--but both together

sat %>% 
  bind_rows(fc) %>% 
  mutate(estimate = round(estimate, 2),
         param = ifelse(param == "saturation", "Saturation", "Field capacity"), 
         thing = ifelse(cov == "sand", "Sand as a co-variate", "No covariates"),
         site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain")),
         param = factor(param, levels = c("Saturation", "Field capacity"))
         ) %>% 
  ggplot(aes(cc_trt, estimate, alpha = thing, color = cc_trt)) + 
  geom_point(position = position_dodge(width = 0.2)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.2)) +
  labs(y = "Volumetric water content (%)",
       x = NULL,
       color = NULL, 
       alpha = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 2)) +
  scale_color_manual(values = c(pfi_brn, pfi_green)) + 
  scale_alpha_manual(values = c(0.3, 1)) +
  facet_grid(param ~ site_sys) + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2))) 

#--don't include the covariate thing
sat %>% 
  bind_rows(fc) %>% 
  mutate(estimate = round(estimate, 2),
         param = ifelse(param == "saturation", "Saturation", "Field capacity"), 
         thing = ifelse(cov == "sand", "Sand as a co-variate", "No covariates"),
         site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain")),
         param = factor(param, levels = c("Saturation", "Field capacity"))
  ) %>% 
  filter(thing == "Sand as a co-variate") %>% 
  ggplot(aes(cc_trt, estimate, color = cc_trt)) + 
  geom_point(size = 2) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 1.1) +
  guides(color = F) +
  labs(y = "Volumetric water content (%)",
       x = NULL,
       color = NULL, 
       alpha = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 2)) +
  scale_color_manual(values = c(pfi_brn, pfi_green)) + 
  facet_grid(param ~ site_sys, scales = "free") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2))) 

#--site on x-axis

#--get sig ones
fc_sig <- read_csv("01_fit-models/dat_fc-emmeans-diff.csv")
sat_sig <- read_csv("01_fit-models/dat_sat-emmeans-diff.csv")

sigs <- 
  fc_sig %>% 
  bind_rows(sat_sig) %>% 
  mutate(estimate = round(estimate, 2),
         param = ifelse(param == "saturation", "Saturation", "Field capacity"), 
         thing = ifelse(cov == "sand", "Sand as a co-variate", "No covariates"),
         site_sys = str_replace_all(site_sys, "_", "-"),
         site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain")),
         param = factor(param, levels = c("Saturation", "Field capacity"))
  ) %>% 
  filter(adj.p.value < 0.05) %>% 
  mutate(cc_trt = "xx") %>% 
  filter(thing != "No covariates")



# patchwork them ----------------------------------------------------------

#--to get shapes different, have to manually add stars

#--none
sigs %>% filter(grepl("Sat", param))

# sat fig, se bars --------------------------------------------------------


fig_sat <- 
  sat %>% 
  mutate(estimate = round(estimate, 2),
         param = ifelse(param == "saturation", "Saturation", "Field capacity"), 
         thing = ifelse(cov == "sand", "Sand as a co-variate", "No covariates"),
         site_sys = as.character(site_sys),
         #site_sys = ifelse(site_sys == "Central-grain", "Central-grain*", site_sys),
         site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain")),
         param = factor(param, levels = c("Saturation", "Field capacity"))
  ) %>% 
  filter(thing == "Sand as a co-variate") %>% 
  ggplot(aes(site_sys, estimate, color = cc_trt, shape = cc_trt)) + 
  geom_point(
    size = 3, 
    position = position_dodge2(width = 0.2)) + 
  geom_linerange(aes(ymin = estimate - std.error, 
                     ymax = estimate + std.error), 
                 size = 1.1, position = position_dodge2(width = 0.2)) +
  #guides(color = F) +
  labs(y = "Volumetric water content (%)",
       x = NULL,
       shape = NULL,
       color = NULL, 
       alpha = NULL) +
  #geom_text(data = sigs, aes(x = site_sys, y = 0.3, label = "*"), size = 10) +
  scale_y_continuous(labels = label_percent(accuracy = 2)) +
  scale_color_manual(values = c(pfi_brn, pfi_green, "black")) + 
  facet_grid(. ~ param, scales = "free") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)),
        legend.position = c(0.1, 0.1),
        legend.justification = c(0, 0),
        legend.background = element_blank()) 

fig_sat


# fc fig, se bars ---------------------------------------------------------

#--sat sig w/sand = central silage, west grain
sigs %>% filter(grepl("Field", param))

fig_fc <-
  fc %>% 
  mutate(estimate = round(estimate, 2),
         param = ifelse(param == "saturation", "Saturation", "Field capacity (100 cm H2O)"), 
         thing = ifelse(cov == "sand", "Sand as a co-variate", "No covariates"),
         site_sys = as.character(site_sys),
         site_sys = ifelse(site_sys == "West-grain", "West-grain*", 
                           ifelse(site_sys == "Central-silage", "Central-silage*", site_sys)),
         site_sys = factor(site_sys, levels = c("West-grain*", "Central-silage*", "Central-grain", "East-grain")),
         param = factor(param, levels = c("Saturation", "Field capacity (100 cm H2O)"))
  ) %>% 
  filter(thing == "Sand as a co-variate") %>% 
  ggplot(aes(site_sys, estimate, color = cc_trt, shape = cc_trt)) + 
  geom_point(
    size = 3, 
    position = position_dodge2(width = 0.2)) + 
  geom_linerange(aes(ymin = estimate - std.error, 
                     ymax = estimate + std.error), 
                 size = 1.1, position = position_dodge2(width = 0.2)) +
  #guides(color = F) +
  labs(y = "Soil volumetric water content (%)",
       x = NULL,
       shape = NULL,
       color = NULL, 
       alpha = NULL) +
    guides(shape = F, color = F) +
  #geom_text(data = sigs, aes(x = site_sys, y = 0.3, label = "*"), size = 10) +
  scale_y_continuous(labels = label_percent(accuracy = 2)) +
  scale_color_manual(values = c(pfi_brn, pfi_green, "black")) + 
  facet_grid(. ~ param, scales = "free") + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)),
        legend.position = c(0.1, 0.1),
       # axis.text.y = element_blank(),
        axis.title.y = element_blank()) 


library(patchwork)
fig_sat + fig_fc

ggsave("02_figs/old/fig_manu-sat-fc-ses.png")



#--ugly, imo

# try again ---------------------------------------------------------------
fc_thing <- 
  fc %>% 
  mutate(estimate = round(estimate, 2),
         param = ifelse(param == "saturation", "Saturation", "Field capacity"), 
         thing = ifelse(cov == "sand", "Sand as a co-variate", "No covariates"),
         site_sys = as.character(site_sys),
         site_sys = ifelse(site_sys == "West-grain", "West-grain*", 
                           ifelse(site_sys == "Central-silage", "Central-silage*", site_sys)),
         site_sys = factor(site_sys, levels = c("West-grain*", "Central-silage*", "Central-grain", "East-grain")),
         param = factor(param, levels = c("Saturation", "Field capacity"))
  ) %>% 
  filter(thing == "Sand as a co-variate") 
  
sat_thing <- 
  sat %>% 
  mutate(estimate = round(estimate, 2),
         param = ifelse(param == "saturation", "Saturation", "Field capacity"), 
         thing = ifelse(cov == "sand", "Sand as a co-variate", "No covariates"),
         site_sys = as.character(site_sys),
         site_sys = ifelse(site_sys == "East-grain", "East-grain*", site_sys),
         site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain*")),
         param = factor(param, levels = c("Saturation", "Field capacity"))
  ) %>% 
  filter(thing == "Sand as a co-variate") 

bind_rows(fc_thing, sat_thing) %>% 
  ggplot(aes(site_sys, estimate, color = cc_trt, shape = cc_trt)) + 
  geom_point(
    size = 3, 
    position = position_dodge2(width = 0.2)) + 
  geom_linerange(aes(ymin = estimate - std.error, 
                     ymax = estimate + std.error), 
                 size = 1.1, position = position_dodge2(width = 0.2)) +
  #guides(color = F) +
  labs(y = "Volumetric water content (%)",
       x = NULL,
       shape = NULL,
       color = NULL, 
       alpha = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 2)) +
  scale_color_manual(values = c(pfi_brn, pfi_green, "black")) + 
  facet_grid(. ~ param, scales = "free") + 
  theme(legend.position = c(0.01, 0.01),
        legend.justification = c(0, 0),
        legend.background = element_blank(),
        strip.text = element_text(size = rel(1.3)),
        strip.background = element_blank(),
        legend.text = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.title = element_text(size = rel(1.3))) 

