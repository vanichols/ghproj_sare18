#--make curves w/ obs and predictions (maybe)
#--updated: 3/4/2021
#--         9/22/2021 - try a bar graph


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


# data --------------------------------------------------------------------

#--field capacity and saturation
fc <- 
  read_csv("01_fit-models/dat_fc-emmeans-cis.csv") %>% 
  mutate(
    cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye cover crop",
    grepl("no", cc_trt) ~ "No cover",
    TRUE ~ cc_trt),
    site_sys = str_replace_all(site_sys, "_", "-")
    ) %>% 
  mutate(
    site_sys2 = ifelse(site_sys %in% c("East-grain", "West-grain"), 
                       paste(site_sys, "\n(commercial farm)", sep = ""),
                       site_sys),
    site_sys = factor(site_sys, levels = c("West-grain", 
                                           "Central-silage", 
                                           "Central-grain",
                                           "East-grain")),
    site_sys2 = factor(site_sys2, levels = c("West-grain\n(commercial farm)", 
                                             "East-grain\n(commercial farm)", 
                                             "Central-silage", "Central-grain"))
    ) 


sat <- 
  read_csv("01_fit-models/dat_sat-emmeans-cis.csv") %>% 
  mutate(
    cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye cover crop",
    grepl("no", cc_trt) ~ "No cover",
    TRUE ~ cc_trt),
    site_sys = str_replace_all(site_sys, "_", "-")
    ) %>% 
  mutate(
    site_sys2 = ifelse(site_sys %in% c("East-grain", "West-grain"), 
                       paste(site_sys, "\n(commercial farm)", sep = ""),
                       site_sys),
    site_sys = factor(site_sys, levels = c("West-grain", 
                                           "Central-silage", 
                                           "Central-grain",
                                           "East-grain")),
    site_sys2 = factor(site_sys2, levels = c("West-grain\n(commercial farm)", 
                                             "East-grain\n(commercial farm)", 
                                             "Central-silage", "Central-grain"))
  ) 


dat_both <- 
  sat %>% 
  bind_rows(fc) %>% 
  mutate(#estimate = round(estimate, 2),
    param = ifelse(param == "saturation", "Saturation", "Field capacity"), 
    thing = ifelse(cov == "sand", "Sand as a co-variate", "No covariates"),
    param = factor(param, levels = c("Saturation", "Field capacity"))
  ) 

dat_sig <- 
  dat_both %>% 
  mutate(sig = ifelse(param == "Field capacity" & site_sys == "Central-silage" & cc_trt == "No cover", "*", 
                      ifelse(param == "Field capacity" & site_sys == "West-grain" & cc_trt == "No cover", "*", " "))) %>% 
  filter(sig == "*", cov == "sand") %>% 
  select(-cov) %>% 
  group_by(param, site_sys) %>% 
  mutate(est_place = mean(estimate))



# just sand, bars ---------------------------------------------------------


dat_sig_bars <- 
  dat_sig %>%
  mutate(sig_lab = ifelse(site_sys == "West-grain", "+2.4 vol%*", "+2.5 vol%**"))


dat_both %>% 
  filter(cov == "sand") %>% 
  mutate(cc_trt = fct_rev(cc_trt)) %>% 
  ggplot(aes(cc_trt, estimate, fill = cc_trt)) + 
  geom_col(color = "black", width = 0.5) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 1) +
  geom_text(data = dat_sig_bars, 
            aes(
              x = 1.5, 
              y = 0.5, 
              label = sig_lab),
            size = 5, 
            show.legend = FALSE, 
            color = "black",
            fontface = "italic") +
  labs(y = "Volumetric water content (%)",
       x = NULL,
       color = NULL, 
       alpha = NULL,
       pch = NULL,
       #caption = "* = p<0.10, ** = p<0.05"
       caption = "*p<=0.10, **p<=0.05") +
  scale_y_continuous(labels = label_percent()) +
  scale_fill_manual(values = c("No cover" = pfi_brn, "Rye cover crop" = pfi_green)) + 
  scale_color_manual(values = c("No cover" = pfi_brn, "Rye cover crop" = pfi_green)) + 
  guides(color = F,
         label = F,
         text = F,
         fill = F) +
  facet_grid(site_sys2 ~ param)+#, scales = "free") + 
  theme(strip.background = element_blank(),
        axis.title.y = element_text(size = rel(1.1)),
        axis.text.x = element_text(angle = 30, hjust = 1, size = rel(1.1)),
        strip.text.x = element_text(size = rel(1.2)), 
        strip.text.y = element_text(size = rel(0.9)), 
        legend.position = "bottom")
  

ggsave("02_figs/fig_manu-sat-fc-vert-bars.png", width = 4.04, height = 6.62)






# crap --------------------------------------------------------------------


# has wo sand correction in gray, points ------------------------------------------

dat_both %>% 
  mutate(cc_trt = fct_rev(cc_trt)) %>% 
  ggplot(aes(cc_trt, estimate, alpha = thing, color = cc_trt)) + 
  geom_point(position = position_dodge(width = 0.1), aes(pch = thing), size = 2.5) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), position = position_dodge(width = 0.1), size = 1) +
  geom_text(data = dat_sig, x = 1.5, y = 0.305, label = "*", size = 13, show.legend = FALSE) +
  labs(y = "Volumetric water content (%)",
       x = NULL,
       color = NULL, 
       alpha = NULL,
       pch = NULL) +
  scale_y_continuous(labels = label_percent(accuracy = 2)) +
  scale_color_manual(values = c("No cover" = pfi_brn, "Rye cover crop" = pfi_green)) + 
  scale_alpha_manual(values = c(0.2, 1)) +
  facet_grid(param ~ site_sys, scales = "free") + 
  guides(color = F,
         label = F,
         text = F) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size = rel(1.2)), 
        legend.position = "bottom") 


ggsave("02_figs/fig_manu-sat-fc-ses-sand.png")



# just sand results, points -------------------------------------------------------

dat_both %>% 
  filter(cov == "sand") %>% 
  mutate(cc_trt = fct_rev(cc_trt)) %>% 
  ggplot(aes(cc_trt, estimate, color = cc_trt)) + 
  geom_point(size = 3) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 1) +
  geom_text(data = dat_sig, x = 1.5, y = 0.305, label = "*", size = 13, show.legend = FALSE, color = "black") +
  labs(y = "Volumetric water content (%)",
       x = NULL,
       color = NULL, 
       alpha = NULL,
       pch = NULL) +
  scale_y_continuous(labels = label_percent(),
                     breaks = c(.3, .4, .5)) +
  scale_color_manual(values = c("No cover" = pfi_brn, "Rye cover crop" = pfi_green)) + 
  facet_grid(param ~ site_sys)+#, scales = "free") + 
  guides(color = F,
         label = F,
         text = F) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(size = rel(1.2)), 
        legend.position = "bottom") 


ggsave("02_figs/fig_manu-sat-fc-ses.png")





n# other crap --------------------------------------------------------------


dat_both %>% 
  filter(cov == "sand") %>% 
  mutate(cc_trt = fct_rev(cc_trt)) %>% 
  ggplot(aes(cc_trt, estimate, color = cc_trt)) + 
  geom_point(size = 3, aes(pch = param)) + 
  geom_linerange(aes(ymin = conf.low, ymax = conf.high), size = 1) +
  geom_text(data = dat_sig, aes(x = 1.5, y = est_place), label = "*", size = 13, show.legend = FALSE, color = "black") +
  labs(y = "Volumetric water content (%)",
       x = NULL,
       color = NULL, 
       alpha = NULL,
       pch = NULL) +
  scale_y_continuous(labels = label_percent(),
                     breaks = c(.3, .4, .5)) +
  scale_color_manual(values = c("No cover" = pfi_brn, "Rye cover crop" = pfi_green)) + 
  facet_grid(. ~ site_sys)+#, scales = "free") + 
  guides(color = F,
         label = F,
         text = F) +
  theme(strip.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text = element_text(size = rel(1.2)), 
        legend.position = "bottom") 

ggsave("02_figs/fig_manu-sat-fc-ses-one-panel.png")

#--get sig ones
fc_sig <- read_csv("01_fit-models/dat_fc-emmeans-diff.csv")
sat_sig <- read_csv("01_fit-models/dat_sat-emmeans-diff.csv")
