#--do simple stats on om, bulk den, texture
#--updated: 3/3/2021


rm(list = ls())
library(tidyverse)
library(scales)
library(purrr)
library(PFIswhc)
library(lme4)
library(lmerTest)
library(emmeans)
library(broom)

# data --------------------------------------------------------------------

dat <- 
  sare_texture %>% 
  left_join(sare_bulkden) %>% 
  left_join(sare_om) %>% 
  left_join(sare_plotkey) %>% 
  unite(site_name, sys_trt, col = "site_sys", sep = "-") %>% 
  mutate(repid = paste(site_sys, rep)) %>% 
  select(repid, cc_trt, site_sys, om, clay, silt, bulkden_gcm3) %>% 
  pivot_longer(om:bulkden_gcm3)



# organic matter ----------------------------------------------------------

#--ranges in om values
dat %>% 
  filter(name == "om") %>% 
  pull(value) %>% 
  summary()

#--sig interaction
fom <- lmer(value ~ site_sys * cc_trt + (1|repid), data = dat %>% filter(name == "om"))
anova(fom)

#--diff at rob's
emmeans(fom,  ~site_sys) 
eom <- emmeans(fom,  ~cc_trt|site_sys) 
pairs(eom)


eom %>% 
  tidy() %>% 
  mutate(var = "om") %>% 
  ggplot(aes(cc_trt, estimate, color = cc_trt)) + 
  geom_point(size = 2) + 
  facet_grid(~site_sys) + 
  labs(title = "org matt")


# bulk den ----------------------------------------------------------------

#--ranges in om values
dat %>% 
  filter(name == "bulkden_gcm3") %>% 
  pull(value) %>% 
  summary()

#--no interactions, nothing is different
fbd <- lmer(value ~ site_sys * cc_trt + (1|repid), data = dat %>% filter(name == "bulkden_gcm3"))
anova(fbd)

#--ignore the west-grain diff, not sig and am doing a lot of comparisons
ebd <- emmeans(fbd,  ~cc_trt|site_sys) 
pairs(ebd)


ebd %>% 
  tidy() %>% 
  mutate(var = "bd") %>% 
  ggplot(aes(cc_trt, estimate, color = cc_trt)) + 
  geom_point(size = 2) + 
  facet_grid(~site_sys) + 
  labs(title = "bulk den")


# clay ----------------------------------------------------------------

#--could use clay as a covariate?

#--interactions
fcl <- lmer(value ~ site_sys * cc_trt + (1|repid), data = dat %>% filter(name == "clay"))
anova(fcl)

ecl <- emmeans(fcl,  ~cc_trt|site_sys) 
pairs(ecl)


ecl %>% 
  tidy() %>% 
  mutate(var = "cl") %>% 
  ggplot(aes(site_sys, estimate, fill = cc_trt, color = cc_trt)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) + 
  labs(title = "clay")


# parameters --------------------------------------------------------------

#--do a meta-analysis?
library(metafor)

dp <- 
  read_csv("03_fit-models/03dat_gard-parms-eu.csv") %>%
  left_join(sare_plotkey) %>% 
  left_join(sare_texture) %>% 
  select(plot_id, cc_trt, term, estimate, std.error, clay, site_name) %>% 
  mutate(yi = estimate, 
         vi = std.error^2)

dp

#--I don't know what I'm doing...

#--residual water, reduced by not using a cover crop, regardless of clay amount. Huh. 
rma.mv(yi, vi, mods = ~cc_trt + clay, random = ~1|site_name, data = dp %>% filter(term == "Thr"))
rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "Thr"))


#--Saturated value, cc_trt NOT sig if we include clay
rma.mv(yi, vi, mods = ~cc_trt + clay, random = ~1|site_name, data = dp %>% filter(term == "Ths"))
rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "Ths"))

#--alp, including clay makes it not sig
rma.mv(yi, vi, mods = ~cc_trt + clay, random = ~1|site_name, data = dp %>% filter(term == "alp"))
rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "alp"))

#--scal, never sig, doesn't matter
rma.mv(yi, vi, mods = ~cc_trt + clay, random = ~1|site_name, data = dp %>% filter(term == "scal"))
rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "scal"))
