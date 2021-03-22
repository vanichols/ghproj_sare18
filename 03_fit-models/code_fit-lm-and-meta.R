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
library(metafor)

# data --------------------------------------------------------------------

dat <- 
  sare_texture %>% 
  left_join(sare_bulkden) %>% 
  left_join(sare_om) %>% 
  left_join(sare_plotkey) %>% 
  unite(site_name, sys_trt, col = "site_sys", sep = "-") %>% 
  mutate(repid = paste(site_sys, rep)) %>% 
  filter(!is.na(sand)) %>% #--eliminate stout 5th no cover plot (if it's in there)
  select(repid, cc_trt, site_sys, om, clay, silt, sand, bulkden_gcm3) %>% 
  pivot_longer(om:bulkden_gcm3)



# clay ----------------------------------------------------------------

#--does clay vary by cover crop treatment?
dat %>% 
  filter(name == "clay") %>% 
  pull(value) %>% 
  summary()

fcl <- lmer(value ~ site_sys * cc_trt + (1|repid), data = dat %>% filter(name == "clay"))

#--yes, interaction site x cc trt
anova(fcl)

#--both east and west have higher in cc trt
ecl <- emmeans(fcl,  ~cc_trt|site_sys) 
pairs(ecl)

dat %>% 
  filter(name == "clay") %>% 
  ggplot(aes(cc_trt, value, color = cc_trt)) + 
  geom_point() +
  facet_grid(.~site_sys) + 
  labs(title = "cover crop plots have lower clay")

ecl2 <- emmeans(fcl,  ~cc_trt) 
pairs(ecl2)


ecl %>% 
  tidy() %>% 
  mutate(var = "cl") %>% 
  ggplot(aes(site_sys, estimate, fill = cc_trt, color = cc_trt)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) + 
  labs(title = "clay")

# sand ----------------------------------------------------------------

dat %>% 
  filter(name == "sand") %>% 
  pull(value) %>% 
  summary()

dat %>% 
  filter(name == "sand") %>%
  ggplot(aes(cc_trt, value)) + 
  geom_point(aes(color = cc_trt)) + 
  facet_grid(.~site_sys) + 
  labs(title = "cover crop plots have higher sand")

#--are sand and clay related?

dat %>% 
  filter(name %in% c("sand", "clay")) %>%
  pivot_wider(names_from = name, values_from = value) %>% 
  ggplot(aes(sand, clay)) + 
  geom_point(aes(color = cc_trt))

#--no interactions for sand
fsa <- lmer(value ~ site_sys * cc_trt + (1|repid), data = dat %>% filter(name == "sand"))
anova(fsa)

esa <- emmeans(fsa,  ~cc_trt|site_sys) 
pairs(esa)

esa2 <- emmeans(fsa,  ~cc_trt) 
pairs(esa2)


esa %>% 
  tidy() %>% 
  mutate(var = "sa") %>% 
  ggplot(aes(site_sys, estimate, fill = cc_trt, color = cc_trt)) + 
  geom_point() + 
  geom_linerange(aes(ymin = estimate - std.error, ymax = estimate + std.error)) + 
  labs(title = "sand")


# organic matter ----------------------------------------------------------

#--ranges in om values
dat %>% 
  filter(name %in% c("om")) %>%
  pull(value) %>% 
  summary()

  
#--need to include clay as covariate, as it impacts organic matter accrual
om_dat <- 
  dat %>% 
  filter(name %in% c("om", "clay")) %>%
  pivot_wider(names_from = name, values_from = value)

#---don't include clay  
fom_nocl <- lmer(om ~ site_sys * cc_trt + (1|repid), data = om_dat)
anova(fom_nocl)

#--ok just at rob's, same res as when you don't include clay. 
eom_nocl <- emmeans(fom_nocl,  ~cc_trt|site_sys) 
pairs(eom_nocl)


#---include clay  
fom <- lmer(om ~ site_sys * cc_trt + clay + (1|repid), data = om_dat)
anova(fom)

#--ok just at rob's, same res as when you don't include clay. 
emmeans(fom,  ~site_sys) 
eom <- emmeans(fom,  ~cc_trt|site_sys) 
pairs(eom)
pairs(eom_nocl)

#--yeah, it just enhances the rob effect
res_om <- 
  eom %>% 
  tidy() %>%
  mutate(cvar = "clay") %>% 
  bind_rows(eom_nocl %>% tidy() %>% mutate(cvar = "no clay")) %>% 
  mutate(var = "om")

res_om %>% write_csv("03_fit-models/03dat_om-emmeans.csv")
pairs(eom) %>% tidy() %>% 
  mutate(var = "om, clay cov") %>% 
  write_csv("03_fit-models/03dat_om-emmeans-sig.csv")

res_om %>% 
  ggplot(aes(cc_trt, estimate, color = cvar)) + 
  geom_point(size = 2) + 
  facet_grid(~site_sys) + 
  labs(title = "org matt")


# bulk den ----------------------------------------------------------------

#--does this need to have a covariate? not sure
bd_dat <- 
  dat %>% 
  filter(name %in% c("bulkden_gcm3", "sand")) %>%
  pivot_wider(names_from = name, values_from = value)

bd_dat %>% 
  ggplot(aes(cc_trt, bulkden_gcm3)) + 
  geom_point() + 
  facet_grid(.~site_sys)

#--no interactions, nothing is different
fbd <- lmer(bulkden_gcm3 ~ site_sys * cc_trt + (1|repid), data = bd_dat)
anova(fbd)

#--ignore the west-grain diff, not sig and am doing a lot of comparisons
ebd <- emmeans(fbd,  ~cc_trt|site_sys) 
pairs(ebd)

#--with sand? makes a big diff...east grain is now sig
fbd_sand <- lmer(bulkden_gcm3 ~ site_sys * cc_trt + sand + (1|repid), data = bd_dat)
anova(fbd_sand)

ebd_sand <- emmeans(fbd_sand,  ~cc_trt|site_sys) 
pairs(ebd_sand)

ebd_sand %>% 
  tidy() %>% 
  mutate(var = "bd") %>% 
  ggplot(aes(cc_trt, estimate, color = cc_trt)) + 
  geom_point(size = 2) + 
  facet_grid(~site_sys) + 
  labs(title = "bulk den")




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

dp %>% 
  filter(term == "scal") %>% 
  pull(estimate) %>% 
  summary()

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




# use clay as covariate ---------------------------------------------------


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

meta_res %>% write_csv("03_fit-models/03dat_meta-parms-eu-claycov.csv")


# use sand as covariate ---------------------------------------------------


#--residual water, reduced by not using a cover crop, regardless of sand amount. Huh. 
#--does order matter? No. whew. 
m1a <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt + sand, random = ~1|site_name, data = dp %>% filter(term == "Thr"))) %>% 
  mutate(param = "Thr-sand")
m1b <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "Thr"))) %>% 
  mutate(param = "Thr")


#--Saturated value, cc_trt NOT sig if we include sand
m2a <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt + sand, random = ~1|site_name, data = dp %>% filter(term == "Ths"))) %>% 
  mutate(param = "Ths-sand")
m2b <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "Ths"))) %>% 
  mutate(param = "Ths")

#--alp, including sand makes it not sig
m3a <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt + sand, random = ~1|site_name, data = dp %>% filter(term == "alp"))) %>% 
  mutate(param = "alp-sand")
m3b <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt, random = ~1|site_name, data = dp %>% filter(term == "alp"))) %>% 
  mutate(param = "alp")

#--scal, never sig, doesn't matter
m4a <- cleanrma(rma.mv(yi, vi, mods = ~cc_trt + sand, random = ~1|site_name, data = dp %>% filter(term == "scal"))) %>% 
  mutate(param = "scal-sand")
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

meta_res %>% write_csv("03_fit-models/03dat_meta-parms-eu-sandcov.csv")

