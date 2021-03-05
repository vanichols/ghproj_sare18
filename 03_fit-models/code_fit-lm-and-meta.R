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
  select(plot_id, cc_trt, term, estimate, std.error, clay, silt, sand, site_name) %>% 
  mutate(yi = estimate, 
         vi = std.error^2) %>% 
  mutate(cc_trt = recode(cc_trt, "cc" = "zcc"))

dp

#--clay as covariate? yes. 
dp %>% 
  ggplot(aes(clay, estimate)) + 
  geom_point(aes(color = cc_trt)) + 
  facet_grid(.~term)

cleanrma <- function(a = a) {
  tibble(
    term = rownames(a$b),
    estimate = a$b %>% as.vector(),
    se = a$se,
    pval = a$pval,
    ci.lb = a$ci.lb,
    ci.up = a$ci.ub
  )
}


#--I don't know what I'm doing...

#--residual water, reduced by not using a cover crop, regardless of clay amount. Huh. 
#--does order matter? No. whew. 
m1a <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt + clay, random = ~1|site_name, data = dp %>% filter(term == "Thr"))) %>% 
  mutate(param = "Thr-clay")
m1b <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "Thr"))) %>% 
  mutate(param = "Thr")


#--Saturated value, cc_trt NOT sig if we include clay
m2a <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt + clay, random = ~1|site_name, data = dp %>% filter(term == "Ths"))) %>% 
  mutate(param = "Ths-clay")
m2b <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "Ths"))) %>% 
  mutate(param = "Ths")

#--alp, including clay makes it not sig
m3a <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt + clay, random = ~1|site_name, data = dp %>% filter(term == "alp"))) %>% 
  mutate(param = "alp-clay")
m3b <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "alp"))) %>% 
  mutate(param = "alp")

#--scal, never sig, doesn't matter
m4a <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt + clay, random = ~1|site_name, data = dp %>% filter(term == "scal"))) %>% 
  mutate(param = "scal-clay")
m4b <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "scal"))) %>% 
  mutate(param = "scal")

meta_res <- 
  m1a %>% 
  bind_rows(m1b) %>% 
  bind_rows(m2a) %>% 
  bind_rows(m2b) %>% 
  bind_rows(m3a) %>% 
  bind_rows(m3b) %>% 
  bind_rows(m4a) %>% 
  bind_rows(m4b)

meta_res %>% write_csv("03_fit-models/03dat_meta-parms-eu.csv")
