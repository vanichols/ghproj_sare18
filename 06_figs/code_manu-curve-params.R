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


dat <- read_csv("03_fit-models/03dat_gard-parms-trt.csv") %>% 
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye Cover Crop",
    grepl("no", cc_trt) ~ "No Cover",
    TRUE ~ cc_trt),
    site_sys = str_replace_all(site_sys, "_", "-"),
    site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain")),
    term2 = case_when(
      grepl("alp", term) ~ "Inverse of Air-entry Potential",
      grepl("scal", term) ~ "Pore-size distribution index",
      grepl("Thr", term) ~ "SWC at Plant Wilting",
      grepl("Ths", term) ~ "SWC at Saturation"
    )
  ) 


dat

mdatcl <- read_csv("03_fit-models/03dat_meta-parms-eu-claycov.csv") 
mdatsa <- read_csv("03_fit-models/03dat_meta-parms-eu-sandcov.csv") 





# individual fits ---------------------------------------------------------

# 1 - thr - threshold moisture content at very high pressures
# 2 - ths - saturated moisture content
# 3 - alp - inverse of air-entry potential
# 4 - nscal - index for pore-size distribution


#--get 95% CIS
crit_val <- qnorm(p = .975)
#--correct for # of comparisons (16)
crit_val <- qnorm(p = 1 - (.05 /16))


#--don't include PW and SAT things?

dat %>%
  filter(term %in% c("alp", "scal")) %>% 
  mutate(estimate = round(estimate, 2)) %>% 
  ggplot(aes(site_sys, estimate, color = cc_trt)) +
  geom_point() +
  geom_linerange(aes(ymin = estimate - std.error*crit_val,
                     ymax = estimate + std.error*crit_val)) +
  scale_color_manual(values = c("Rye Cover Crop" = pfi_green,
                                "No Cover" = pfi_brn)) +
  #scale_y_continuous() + 
  scale_y_continuous(breaks= pretty_breaks()) +
  facet_wrap(~term2, scales = "free", labeller = label_wrap_gen()) +
  coord_flip() +
  labs(y = "Estimate",
       color = NULL,
       x = NULL) + 
  theme(strip.text = element_text(size = rel(1.3)),
        strip.background = element_blank(),
        legend.position = "bottom", 
        legend.text = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        #axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.title = element_text(size = rel(1.3))
  )

ggsave("06_figs/fig_manu-parms-fit-by-trt.png")  


#--use patchwork, look at estimated differnces
f1 <- 
  dat %>%
  filter(term %in% c("alp")) %>% 
  mutate(estimate = round(estimate, 2)) %>% 
  ggplot(aes(site_sys, estimate, color = cc_trt, shape = cc_trt)) +
  geom_point(position = position_dodge(width = 0.2), size = 2) +
  geom_linerange(aes(ymin = estimate - std.error*crit_val,
                     ymax = estimate + std.error*crit_val),
                 position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Rye Cover Crop" = pfi_green,
                                "No Cover" = pfi_brn)) +
  #scale_y_continuous() + 
  scale_y_continuous(breaks= pretty_breaks()) +
  facet_wrap(~term2, scales = "free", labeller = label_wrap_gen()) +
  coord_flip() +
  guides(color = F, shape = F) +
  labs(y = "Estimate",
       color = NULL,
       shape = NULL,
       x = NULL) + 
  theme(strip.text = element_text(size = rel(1.3)),
        strip.background = element_blank(),
        #legend.position = "bottom", 
        #legend.text = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        #axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.title = element_text(size = rel(1.3))
  )
f1

f2 <- 
  dat %>%
  filter(term %in% c("scal")) %>% 
  mutate(estimate = round(estimate, 2)) %>% 
  ggplot(aes(site_sys, estimate, color = cc_trt, shape = cc_trt)) +
  geom_point( position = position_dodge(width = 0.2), size = 2) +
  geom_linerange(aes(ymin = estimate - std.error*crit_val,
                     ymax = estimate + std.error*crit_val),
                 position = position_dodge(width = 0.2)) +
  scale_color_manual(values = c("Rye Cover Crop" = pfi_green,
                                "No Cover" = pfi_brn)) +
  #scale_y_continuous() + 
  scale_y_continuous(breaks= pretty_breaks()) +
  facet_wrap(~term2, scales = "free", labeller = label_wrap_gen()) +
  coord_flip() +
  #guides(shape = F, color = F) +
  labs(y = "Estimate",
       color = NULL,
       shape = NULL,
       x = NULL) + 
  theme(strip.text = element_text(size = rel(1.3)),
        strip.background = element_blank(),
        legend.position = "right", 
        legend.text = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.y = element_blank(),
        #axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5),
        axis.title = element_text(size = rel(1.3))
  )
f2


f1+f2

ggsave("06_figs/fig_manu-curve-params.png")


# fit by eu ---------------------------------------------------------------


# meta-analysis clay -----------------------------------------------------------

mdatcl %>% 
  #filter(grepl("clay", param)) %>% 
  mutate(thing = ifelse(grepl("clay", param), "Clay as a co-variate", "No covariates"),
         param_clean = str_remove_all(param, "-clay")) %>% 
  filter(term == "cc_trtzcc") %>% 
  mutate(
  param2 = case_when(
    grepl("alp", param_clean) ~ "Inverse of Air-entry Potential (units?)",
    grepl("scal", param_clean) ~ "Pore-size distribution index (unitless)",
    grepl("Thr", param_clean) ~ "SWC at Plant Wilting (vol. frac)",
    grepl("Ths", param_clean) ~ "SWC at Saturation (vol. frac)"
  )) %>%  
  ggplot(aes(reorder(param2, estimate), estimate, color = thing)) + 
  geom_point(position = position_dodge(width = 0.1)) + 
  geom_linerange(aes(ymin = ci.lb, ymax = ci.up), position = position_dodge(width = 0.1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() + 
  labs(y = "Change due to cover cropping (units specific to paramater)",
        x = NULL,
       color = NULL) +
  scale_color_manual(values = c("black", "gray70"))

ggsave("06_figs/fig_manu-params-clay.png")



# meta-analysis sand ------------------------------------------------------

mdatsa %>% 
  #filter(grepl("sand", param)) %>% 
  mutate(thing = ifelse(grepl("sand", param), "Sand as a co-variate", "No covariates"),
         param_clean = str_remove_all(param, "-sand")) %>% 
  filter(term == "cc_trtzcc") %>% 
  mutate(
    param2 = case_when(
      grepl("alp", param_clean) ~ "Inverse of Air-entry Potential (units?)",
      grepl("scal", param_clean) ~ "Pore-size distribution index (unitess)",
      grepl("Thr", param_clean) ~ "SWC at Plant Wilting (vol. frac.)",
      grepl("Ths", param_clean) ~ "SWC at Saturation (vol. frac)"
    )) %>%  
  ggplot(aes(reorder(param2, estimate), estimate, color = thing)) + 
  geom_point(position = position_dodge(width = 0.1)) + 
  geom_linerange(aes(ymin = ci.lb, ymax = ci.up), position = position_dodge(width = 0.1)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() + 
  labs(y = "Change due to cover cropping (units specific to paramater)",
       x = NULL,
       color = NULL) +
  scale_color_manual(values = c("gray70", "black"))

ggsave("06_figs/fig_manu-params-sand.png")


