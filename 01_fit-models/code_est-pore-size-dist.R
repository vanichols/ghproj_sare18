############################
# author: Gina (vnichols@iastate.edu)
# created: may 19 2021
# purpose: estimate pore size dist
# 
#
#
# last modified: sept 24 2021 (getting stats for manu table)
##############################

# 1 cm water = 0.0980665 kpa

rm(list = ls())
library(tidyverse)
#remotes::install_github("vanichols/PFIswhc")
library(PFIswhc)
library(lme4)
library(lmerTest)
library(emmeans)

dp_raw <- 
  sare_pressure %>% 
  group_by(plot_id) %>% 
  #--each pressure is reflective of the pore size it is draining
  mutate(h = lead(press_cm),
         d_cm = 0.3/h,
         d_um = d_cm * 1E4,
         pore_cat = ifelse(d_um > 30, "Macropores (>30 um)", "Micropores (<30 um)")) %>% 
  #--percentage of water released at each pore size
  mutate(vlag = lead(vtheta)) %>% 
  group_by(plot_id) %>% 
  mutate(vsat = max(vtheta)) %>% 
  ungroup() %>% 
  mutate(thng = (vtheta - vlag)/vsat*100) %>% 
  #--normalize to a percentage
  group_by(plot_id ) %>% 
  mutate(tot = sum(thng, na.rm = T),
         pct = thng/tot) %>% 
  group_by(plot_id) %>% 
  mutate(sum = sum(pct, na.rm = T)) %>% 
  left_join(sare_plotkey) %>% 
  ungroup() 

dp <- 
  dp_raw %>% 
  mutate(
    site_sys = paste(site_name, sys_trt, sep = "-"),
    rep_id = paste(field_id, rep),
    cc_trt = case_when(
      grepl("cc", cc_trt) ~ "Cover Crop",
      grepl("no", cc_trt) ~ "No Cover",
      TRUE ~ cc_trt)) %>% 
  select(d_um, pore_cat, site_sys, cc_trt, rep_id, pct) %>% 
  filter(!is.na(pct))

dp %>% 
  write_csv("01_fit-models/dat_poresizes.csv")


dp_macro <- 
  dp %>% 
  group_by(pore_cat, site_sys, cc_trt, rep_id) %>% 
  summarise(pct = sum(pct, na.rm = T)) %>% 
  filter(pore_cat == "Macropores (>30 um)")


# stats -------------------------------------------------------------------

dp_stat <- 
  dp_raw %>% 
  filter(!is.na(pct)) %>% 
  select(plot_id, press_cm, pore_cat, pct) %>% 
  group_by(plot_id, pore_cat) %>% 
  summarise(pct = sum(pct, na.rm = T)) %>% 
  filter(pore_cat == "Macropores (>30 um)") %>% 
  left_join(sare_plotkey %>% 
              mutate(site_sys = paste(site_name, sys_trt, sep = "-"),
                     rep = paste0("B", rep))
              )

dp_stat %>% 
  write_csv("01_fit-models/dat_macropores-for-stats.csv")



# stats for real ----------------------------------------------------------


#--physically, should include sand as covariate for field capacity
m1 <- lmer(pct ~ cc_trt*site_sys + (1|rep), data = dp_stat)
anova(m1)

em1 <- emmeans(m1, ~cc_trt|site_sys) 
em1
pairs(em1) 


pore_sig <- 
  pairs(em1) %>% 
  broom::tidy() %>% 
  rename("est_diff" = estimate,
         "pval_diff" = p.value) %>% 
  select(site_sys, term, contrast, est_diff, pval_diff)


pore_res <- 
  confint(em1, level = 0.9) %>% 
  broom::tidy() %>% 
  mutate(cov = "none") %>% 
  mutate(param = "pct macropores") %>% 
  select(param, everything())

pore_res %>% 
  left_join(pore_sig) %>% 
  write_csv("01_fit-models/dat_poresize-emmeans.csv")

