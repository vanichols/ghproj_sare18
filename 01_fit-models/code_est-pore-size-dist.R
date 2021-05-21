############################
# author: Gina (vnichols@iastate.edu)
# created: may 19 2021
# purpose: estimate pore size dist
# 
#
#
# last modified: 
##############################

# 1 cm water = 0.0980665 kpa

rm(list = ls())
library(tidyverse)
#remotes::install_github("vanichols/PFIswhc")
library(PFIswhc)
library(lme4)
library(lmerTest)
library(emmeans)

dp <- 
  sare_pressure %>%
  group_by(plot_id) %>%
  mutate(
    vlag = lag(vtheta),
    thng = (vlag - vtheta) / vlag * 100,
    d_um = 300 / press_cm * 0.0980665,
    tot = sum(thng, na.rm = T),
    pct = thng / tot
  ) %>% 
  filter(!is.na(pct))

dp %>% 
  left_join(sare_plotkey) %>% 
  ggplot(aes(d_um, pct)) + 
  geom_col(position = "dodge", aes(fill = cc_trt)) + 
  facet_wrap(~site_name+sys_trt)

theplots <- dp %>% pull(plot_id) %>% unique()

for (i in 1:length(theplots)) {
  myplot <- theplots[i]
  
  fd <-
    dp %>%
    select(plot_id, d_um, pct) %>%
    filter(plot_id == myplot)
  
  Type <- c()
  for (j in 1:nrow(fd)) {
    Type <- c(Type,
              rep(fd$d_um[j], fd$pct[j] * 100))
  }
  
  if (i == 1) {
    dat <- tibble(plot_id = myplot,
                  pore_d = Type)
  } else {
    dat.tmp <- tibble(plot_id = myplot,
                      pore_d = Type)
    dat <- bind_rows(dat, dat.tmp)
  }
}


dat %>% 
  left_join(sare_plotkey) %>% 
  ggplot(aes(pore_d)) + 
  geom_density(aes(fill = cc_trt), alpha = 0.5) + 
  facet_wrap(~site_name+sys_trt)

#--create dummy dist
dp %>% 
  select()

dp %>% 
  left_join(sare_plotkey) %>% 
  ggplot(aes(d_um)) + 
  geom_density(aes(fill = cc_trt), alpha = 0.5) + 
  facet_wrap(~site_name + sys_trt)

dp %>% 
  mutate(pore_cat = ifelse(press_cm < 100, "macro", "micro")) %>% 
  group_by(plot_id, pore_cat) %>% 
  summarise(pct = sum(pct, na.rm = T)) %>% 
  left_join(sare_plotkey) %>% 
  ggplot(aes(plot_id, pct, fill = pore_cat)) + 
  geom_col() + 
  facet_grid(.~cc_trt, scales = "free")
  

sare_pressure %>% 
  group_by(plot_id) %>% 
  mutate(vlag = lead(vtheta),
         thng = (vtheta - vlag)/vtheta*100) %>% 
  group_by(plot_id ) %>% 
  mutate(tot = sum(thng, na.rm = T),
         pct = thng/tot,
         pore_cat = ifelse(press_cm < 100, "macro", "micro")) %>% 
  group_by(plot_id, pore_cat) %>% 
  summarise(pct = sum(pct, na.rm = T)) %>% 
  left_join(sare_plotkey) %>% 
  group_by(pore_cat, field_id, sys_trt, cc_trt) %>% 
  summarise(pct = mean(pct, na.rm = T)) %>% 
  ggplot(aes(cc_trt, pct, fill = pore_cat)) + 
  geom_col() + 
  facet_grid(.~field_id+sys_trt, scales = "free")

sare_pressure %>% 
  group_by(plot_id) %>% 
  mutate(vlag = lead(vtheta),
         thng = (vtheta - vlag)/vtheta*100) %>% 
  group_by(plot_id ) %>% 
  mutate(tot = sum(thng, na.rm = T),
         pct = thng/tot,
         pore_cat = ifelse(press_cm < 100, "macro", "micro")) %>% 
  group_by(plot_id, pore_cat) %>% 
  summarise(pct = sum(pct, na.rm = T)) %>% 
  left_join(sare_plotkey) %>% 
  filter(pore_cat == "macro") %>% 
  ggplot(aes(cc_trt, pct)) + 
  geom_point() + 
  facet_grid(.~site_name + sys_trt)



# 1 cm water = 0.0980665 kpa

#--data
#--note, there is a 5th no cover rep at stout where we measured the whc, but not texture
rd <- 
  sare_pressure %>%
  left_join(sare_plotkey) %>% 
  select(plot_id, site_name, sys_trt, cc_trt, rep, press_cm, vtheta_grav) %>% 
  mutate(press_kPa = press_cm * 0.0980665) %>% 
  unite(site_name, sys_trt, col = "site_sys") %>% 
  mutate(rep_id = paste(site_sys, rep)) %>% 
  left_join(sare_texture) %>% 
  filter(!is.na(sand)) #--get rid of extra east grain no cover plot


# saturation ----------------------------------------------------------

sat <- 
  rd %>% 
  filter(press_cm %in% c(2.5)) %>% 
  group_by(site_sys, cc_trt, rep_id) %>% 
  summarise(vtheta_grav = mean(vtheta_grav, na.rm = T),
            sand = mean(sand, na.rm = T))

sat %>% 
  ggplot(aes(cc_trt, vtheta_grav)) + 
  geom_point(aes(size = sand)) +
  facet_grid(.~site_sys) + 
  scale_color_viridis_c()


#--does it make sense to correct for sand at the saturation? I'm not convinced
m2 <- lmer(vtheta_grav~cc_trt*site_sys + (1|rep_id), data = sat)
m2_sand <- lmer(vtheta_grav~cc_trt*site_sys + sand + (1|rep_id), data = sat)

em2 <- emmeans(m2, ~cc_trt|site_sys)
em2_sand <- emmeans(m2_sand, ~cc_trt|site_sys)

pairs(em2)  #--sig lower w/cc at west and central grain, if no sand correction
pairs(em2_sand)  #--still lower at central grain, not at west

sat_sig <- 
  contrast(em2_sand) %>% 
  broom::tidy() %>% 
  filter(contrast == "cc effect") %>% 
  mutate(cov = "sand", 
         param = "saturation")

sat_sig %>% write_csv("03_fit-models/03dat_sat-emmeans-sig.csv")



sat_res <-
  confint(em2_sand, level = 0.9) %>%
  broom::tidy() %>%
  mutate(cov = "sand") %>%
  bind_rows(confint(em2, level = 0.9) %>%
              broom::tidy() %>%
              mutate(cov = "no sand")) %>%
  mutate(param = "saturation") %>%
  select(param, everything())

sat_res %>% write_csv("03_fit-models/03dat_sat-emmeans.csv")


sat_res %>% 
  ggplot(aes(cc_trt, estimate, color = cov)) + 
  geom_point() + 
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) + 
  facet_grid(cov~site_sys)


# field capacity ----------------------------------------------------------

#--britt suggests doing 100
fc <- 
  rd %>% 
  filter(press_cm %in% c(100)) %>% 
  group_by(site_sys, cc_trt, rep_id) %>% 
  summarise(vtheta_grav = mean(vtheta_grav, na.rm = T),
            sand = mean(sand, na.rm = T))


fc %>% 
  ggplot(aes(cc_trt, vtheta_grav)) + 
  geom_point(aes(size = sand)) + 
  facet_grid(.~site_sys) + 
  labs(title = "field capacity",
       subtitle = "need to include sand as covariate")


fc %>% 
  filter(site_sys == "East_grain")

#--physically, should include sand as covariate for field capacity
m1 <- lmer(vtheta_grav~cc_trt*site_sys + (1|rep_id), data = fc)
anova(m1)
m1_sand <- lmer(vtheta_grav~cc_trt*site_sys + sand + (1|rep_id), data = fc)
anova(m1_sand)

em1 <- emmeans(m1, ~cc_trt|site_sys) #-no sand
contrast(em1) #--centtal silage sig diff

em1_sand <- emmeans(m1_sand, ~cc_trt|site_sys) #-with sand
pairs(em1_sand) #--central silage plus west

fc_sig <- 
  contrast(em1_sand) %>% 
  broom::tidy() %>% 
  filter(contrast == "cc effect") %>% 
  mutate(cov = "sand", 
         param = "field capacity")
 
fc_sig %>% write_csv("03_fit-models/03dat_fc-emmeans-sig.csv")

fc_res <- 
  confint(em1_sand, level = 0.9) %>% 
  broom::tidy() %>% 
  mutate(cov = "sand") %>% 
  bind_rows(
    confint(em1, level = 0.9) %>% 
      broom::tidy() %>% 
      mutate(cov = "no sand")
  ) %>% 
  mutate(param = "avg of 50-100, field capacity") %>% 
  select(param, everything())

fc_res %>% write_csv("03_fit-models/03dat_fc-emmeans.csv")

fc_res %>% 
  ggplot(aes(cc_trt, estimate, color = cov)) + 
  geom_point() + 
  geom_linerange(aes(ymin = conf.low,
                     ymax = conf.high)) + 
  facet_grid(cov~site_sys)


