############################
# author: Gina (vnichols@iastate.edu)
# created: 5/6/2021
# purpose: do stats at certain pressure values
# 
#
#
# last modified: 
# 5/21/2021 (do rand intcp for location when using sand cov)
#
##############################


rm(list = ls())
library(tidyverse)
#remotes::install_github("vanichols/PFIswhc")
library(PFIswhc)
library(lme4)
library(lmerTest)
library(emmeans)

# 1 cm water = 0.0980665 kpa
# britt leaves it in cm water

#--data
#--note, there is a 5th no cover rep at stout where we measured the whc, but not texture
rd <- 
  sare_pressure %>%
  left_join(sare_plotkey) %>% 
  select(plot_id, site_name, sys_trt, cc_trt, rep) %>% 
  unite(site_name, sys_trt, col = "site_sys", remove = F) %>% 
  mutate(rep_id = paste(site_sys, rep)) %>% 
  left_join(sare_texture) %>% 
  left_join(sare_om) %>% 
  left_join(sare_bulkden) %>% 
  distinct() %>% 
  filter(!is.na(sand)) #--get rid of extra east grain no cover plot
 

rd

# texture -----------------------------------------------------------------

#--sand is higher in cover crop trts
m_sand <- lmer(sand~cc_trt*site_sys + (1|rep_id), data = rd)
anova(m_sand)

res_sand <- 
  emmeans(m_sand, ~cc_trt|site_sys) %>% 
  broom::tidy() %>% 
  select(site_sys, cc_trt, estimate, std.error) %>% 
  mutate(respvar = "sand") %>% 
  select(-std.error) %>% 
  pivot_wider(names_from = cc_trt, values_from = estimate) %>% 
  left_join(
    pairs(emmeans(m_sand, ~cc_trt|site_sys)) %>% 
      broom::tidy() %>% 
      select(site_sys, contrast, estimate, std.error, p.value) %>% 
      rename("est_diff" = estimate,
             "diff_pval" = p.value,
             "diff_se" = std.error) )

res_sand %>% write_csv("01_fit-models/dat_sand-stats.csv")

#--clay is lower in cc trts
m_clay <- lmer(clay~cc_trt*site_sys + (1|rep_id), data = rd)
anova(m_clay)
pairs(emmeans(m_clay, ~cc_trt|site_sys))

res_clay <- 
  emmeans(m_clay, ~cc_trt|site_sys) %>% 
  broom::tidy() %>% 
  select(site_sys, cc_trt, estimate, std.error) %>% 
  mutate(respvar = "clay") %>% 
  select(-std.error) %>% 
  pivot_wider(names_from = cc_trt, values_from = estimate) %>% 
  left_join(
    pairs(emmeans(m_clay, ~cc_trt|site_sys)) %>% 
      broom::tidy() %>% 
      select(site_sys, contrast, estimate, std.error, p.value) %>% 
      rename("est_diff" = estimate,
             "diff_pval" = p.value,
             "diff_se" = std.error) )

res_clay %>% write_csv("01_fit-models/dat_clay-stats.csv")

#--silt is lower in cc trts
m_silt <- lmer(silt~cc_trt*site_sys + (1|rep_id), data = rd)
anova(m_silt)
pairs(emmeans(m_silt, ~cc_trt|site_sys))

res_silt <- 
  emmeans(m_silt, ~cc_trt|site_sys) %>% 
  broom::tidy() %>% 
  select(site_sys, cc_trt, estimate, std.error) %>% 
  mutate(respvar = "silt") %>% 
  select(-std.error) %>% 
  pivot_wider(names_from = cc_trt, values_from = estimate) %>% 
  left_join(
    pairs(emmeans(m_silt, ~cc_trt|site_sys)) %>% 
      broom::tidy() %>% 
      select(site_sys, contrast, estimate, std.error, p.value) %>% 
      rename("est_diff" = estimate,
             "diff_pval" = p.value,
             "diff_se" = std.error) )


res_silt %>% write_csv("01_fit-models/dat_silt-stats.csv")

res_sand %>% 
  mutate(diff_pval = round(diff_pval, 3))

res_silt %>% 
  mutate(diff_pval = round(diff_pval, 3))

res_clay %>% 
  mutate(diff_pval = round(diff_pval, 3))


# om -----------------------------------------------------------------

#--what are site-avg OMs?
rd %>% 
  group_by(site_sys) %>% 
  summarise(om = mean(om, na.rm = T))

#--om is the same
m_om <- lmer(om~cc_trt*site_sys + (1|rep_id), data = rd)
m_om_sand <- lm(om~cc_trt*site_sys + sand, data = rd)
pairs(emmeans(m_om_sand, ~cc_trt|site_sys))
pairs(emmeans(m_om, ~cc_trt|site_sys))

res_om_nosand <- 
  emmeans(m_om, ~cc_trt|site_sys) %>% 
  broom::tidy() %>% 
  select(site_sys, cc_trt, estimate, std.error) %>% 
  mutate(respvar = "om") %>% 
  select(-std.error) %>% 
  pivot_wider(names_from = cc_trt, values_from = estimate) %>% 
  left_join(
    pairs(emmeans(m_om, ~cc_trt|site_sys)) %>% 
      broom::tidy() %>% 
      select(site_sys, contrast, estimate, std.error, p.value) %>% 
      rename("est_diff" = estimate,
             "diff_pval" = p.value,
             "diff_se" = std.error) ) %>% 
  mutate(cov = "none")

res_om_sand <- 
  emmeans(m_om_sand, ~cc_trt|site_sys) %>% 
  broom::tidy() %>% 
  select(site_sys, cc_trt, estimate, std.error) %>% 
  mutate(respvar = "om") %>% 
  select(-std.error) %>% 
  pivot_wider(names_from = cc_trt, values_from = estimate) %>% 
  left_join(
    pairs(emmeans(m_om_sand, ~cc_trt|site_sys)) %>% 
      broom::tidy() %>% 
      select(site_sys, contrast, estimate, std.error, p.value) %>% 
      rename("est_diff" = estimate,
             "diff_pval" = p.value,
             "diff_se" = std.error) ) %>% 
  mutate(cov = "sand")

res_om_nosand %>% 
  bind_rows(res_om_sand) %>% 
  select(cov, everything()) %>% 
  write_csv("01_fit-models/dat_om-stats.csv")

res_om_sand

# bd -----------------------------------------------------------------


m_bd <- lmer(bulkden_gcm3 ~ cc_trt*site_sys + (1|rep_id), data = rd)
m_bd_sand <- lmer(bulkden_gcm3 ~ cc_trt*site_sys + sand + (1|rep_id), data = rd)
m_bd_sand_fe <- lm(bulkden_gcm3 ~ cc_trt*site_sys + sand, data = rd)
pairs(emmeans(m_bd_sand, ~cc_trt|site_sys))
pairs(emmeans(m_bd, ~cc_trt|site_sys))
pairs(emmeans(m_bd_sand_fe, ~cc_trt|site_sys))

res_bd_nosand <- 
  emmeans(m_bd, ~cc_trt|site_sys) %>% 
  broom::tidy() %>% 
  select(site_sys, cc_trt, estimate, std.error) %>% 
  mutate(respvar = "bd") %>% 
  select(-std.error) %>% 
  pivot_wider(names_from = cc_trt, values_from = estimate) %>% 
  left_join(
    pairs(emmeans(m_bd, ~cc_trt|site_sys)) %>% 
      broom::tidy() %>% 
      select(site_sys, contrast, estimate, std.error, p.value) %>% 
      rename("est_diff" = estimate,
             "diff_pval" = p.value,
             "diff_se" = std.error) ) %>% 
  mutate(cov = "none")

res_bd_sand <- 
  emmeans(m_bd_sand_fe, ~cc_trt|site_sys) %>% 
  broom::tidy() %>% 
  select(site_sys, cc_trt, estimate, std.error) %>% 
  mutate(respvar = "bd") %>% 
  select(-std.error) %>% 
  pivot_wider(names_from = cc_trt, values_from = estimate) %>% 
  left_join(
    pairs(emmeans(m_bd_sand_fe, ~cc_trt|site_sys)) %>% 
      broom::tidy() %>% 
      select(site_sys, contrast, estimate, std.error, p.value) %>% 
      rename("est_diff" = estimate,
             "diff_pval" = p.value,
             "diff_se" = std.error) ) %>% 
  mutate(cov = "sand")

res_bd_nosand %>% 
  bind_rows(res_bd_sand) %>% 
  select(cov, everything()) %>% 
  write_csv("01_fit-models/dat_bd-stats.csv")

bind_rows(
  emmeans(m_bd, ~cc_trt|site_sys) %>% 
    broom::tidy() %>% 
    mutate(cov = "sand"),
  
  emmeans(m_bd_sand, ~cc_trt|site_sys) %>% 
  broom::tidy() %>% 
  mutate(cov = "none")
)
  



# biomass -----------------------------------------------------------------

library(PFIweeds2020)

pfi_mccbio %>% 
  filter(!grepl("B44", field))
sare_plotkey
