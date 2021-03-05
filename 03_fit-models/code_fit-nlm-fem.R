############################
# author: Gina (vnichols@iastate.edu)
# created: may 8 2020
# last modified: May 11 2020 (continue model fitting)
#                May 12 2020 (confused by df problem)
# edits: Fernando Miguez (2020-05-28)
# purpose: fit non-linear models
# 
# inputs: dc_swrc
#
# outputs:
#
# notes: Using this as reference:https://www.nature.com/articles/s41598-019-54449-8
#
##############################


rm(list = ls())
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(HydroMe) #--for SSgardner/SSgard
library(nlraa)

#?SSgardner


#--data
rd <- read_csv("02_data-calcs/dc_swrc.csv") %>% 
  select(code, cc_trt, press_cm, vtheta_poros1) %>% 
  mutate(x = ifelse(press_cm == 0, 0.01, press_cm), 
         y = vtheta_poros1) %>% 
  separate(code, into = c("site", "plot"), remove = F)

#--I have 36 unique eus
rd %>% 
  select(code) %>% 
  distinct()

#--visual
rd %>% 
  ggplot(aes(x, y, color = code)) + 
  geom_point() + 
  geom_line() + 
  guides(color = F)

#--none look terrible funny
rd %>% 
  ggplot(aes(x, y, color = code)) + 
  geom_point() + 
  geom_line() + 
  guides(color = F) + 
  facet_wrap(~code)


# 1 - thr - threshold moisture content at very high pressures
# 2 - ths - saturated moisture content
# 3 - alp - inverse of air-entry potential
# 4 - nscal - index for pore-size distribution

#--note: SSgard is for grouped data, SSgardener is not
#-- their parameter names are different



# fit model to all data at once -------------------------------------------

#--note: nlsLM is a modified version of nls
#--in HydroMe example they don't supply starting vals, thus the SS, duh

fm1 <- nlsLM(y ~ SSgardner(x, thr, ths, alp, nscal),
             rd)

fm1
coef(fm1)

#--look reasonable
rd %>% 
  ggplot(aes(x, y, color = code)) + 
  geom_point() + 
  geom_line() + 
  geom_line(aes(y = fitted(fm1)), color = "black", size = 3) +
  guides(color = F)



# fit to each eu ----------------------------------------------------------

#--create grouped data
rdG <- groupedData(y ~ x | code, data = rd)

#--use SSgard this time
fmL <- nlraa::nlsLMList(y ~ SSgard(x, Thr, Ths, alp, scal), data = rdG)

#--Ths may vary by something.
plot(intervals(fmL))

#--residuals don't look terrible
plot(fmL)
plot(augPred(fmL))
#- Note: the level 0:1 is not needed at this stage

# fit a mixed model -------------------------------------------------------


#--this takes forever, doesn't work
#fmL2 <- nlme(fmL)

#--simplify the variance-covariance structure
#--nope this doesn't converge
#fmm <- nlme(fmL, random = pdDiag(Ths + scal + alp ~ 1))

#-- FEM: I only removed scal
fmm1 <- nlme(fmL, random = pdDiag(Ths + Thr + alp ~ 1))

## alp could potentially be removed later
plot(fmm1)
plot(augPred(fmm1, level = 0:1))

## the random effect intervals seem to be well constrained (FEM wording)
## FEM: By this I mean that the upper interval doesn't go crazy
## it is bad it migh be several orders of magnitude higher
## Indicating that something is not right
intervals(fmm1)

## Some outliers from Stout 4cc
plot(fmm1, id = 0.01)

#--I know there's a way to assign colors on the fly....
#--St-4cc looks extra flat - need to look at notes to see if anything weird happened.
rd %>% 
  mutate(mycolor = ifelse(code == "St-4cc", "y", "n")) %>% 
  ggplot(aes(x, y, group = code)) + 
  geom_line(aes(color = mycolor))

# incorporate cc_trt ------------------------------------------------------

#--get fixed effects from last model as starting points
fxf <- fixef(fmm1)

## FEM: the error is silly. cc_trt is not a factor, it is a character
class(rdG$cc_trt)
rdG$cc_trt <- as.factor(rdG$cc_trt)

fmm_cc <- update(fmm1, 
               fixed = list(Thr + Ths + alp + scal ~ cc_trt),
               start = c(fxf[1], 0, #--Thr
                         fxf[2], 0, #--Ths
                         fxf[3], 0, #--alp
                         fxf[4], 0)) #--scal

## Adding the random effect of site
fmm_cc2 <- update(fmm_cc, random = list(site = pdDiag(Thr + Ths + scal ~ 1),
                                        code = pdDiag(Thr + Ths + scal ~ 1)),
                  groups = ~ site/code)

anova(fmm_cc2)
intervals(fmm_cc2)
## There seems to be very weak evidence that the cover crops
## affect the parameters of a water retention curve


fmm_cc2_sim <- simulate_nlme(fmm_cc2, nsim = 100, psim = 1, level = 1)

rdG$mn.s <- apply(fmm_cc2_sim, 1, mean)
rdG$mxn.s <- apply(fmm_cc2_sim, 1, max)
rdG$mnn.s <- apply(fmm_cc2_sim, 1, min)



library(emmeans)
## Parameter values and contrast among groups
contrast(emmeans(fmm_cc2, ~ cc_trt, param = "Thr"), "pairwise")
contrast(emmeans(fmm_cc2, ~ cc_trt, param = "Ths"), "pairwise")
contrast(emmeans(fmm_cc2, ~ cc_trt, param = "scal"), "pairwise")
contrast(emmeans(fmm_cc2, ~ cc_trt, param = "alp"), "pairwise")


## Do we want a plot of cover crop treatment effect?
rdG$prd0 <- predict(fmm_cc2, level = 1)


cctrtpal <- rev(c("#69431D", "#45AD45"))

library(scales)
show_col(cctrtpal)

rdG %>%
  as_tibble() %>%
  mutate(cc_trt = recode(cc_trt,
                         "no" = "None",
                         "cc" = "Rye Cover Crop")) %>%
  mutate(county = case_when(
    grepl("B42", site) ~ "Boone County",
    grepl("F", site) ~ "Greene County",
    grepl("St", site) ~ "Washington County"
  )) %>%
  ggplot(aes(x, y, color = cc_trt)) +
  geom_line(aes(y = prd0), size = 2) +
  scale_color_manual(values = cctrtpal) +
  facet_grid(. ~ county) +
  #  scale_y_continuous(label_percent()) +
  scale_x_log10() +
  labs(y = "Soil water content (0-1)",
       x = "Pressure (cm H20)",
       color = NULL) +
  theme(
    legend.direction = "horizontal",
    legend.position = "top",
    #legend.justification = c(1, 1),
    panel.background = element_rect(fill = "gray90"),
    axis.title.y = element_text(angle = 90, vjust = 0.5),
    strip.text = element_text(size = rel(1.5)),
    legend.background = element_rect(color = "black"),
    axis.text = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.3)),
    axis.title = element_text(size = rel(1.3))
  )



#--with ribbons

rdG %>%
  as_tibble() %>%
  mutate(cc_trt = recode(cc_trt,
                         "no" = "None",
                         "cc" = "Rye Cover Crop")) %>%
  mutate(county = case_when(
    grepl("B42", site) ~ "Boone County",
    grepl("F", site) ~ "Greene County",
    grepl("St", site) ~ "Washington County"
  )) %>%
  ggplot(aes(x, y, color = cc_trt)) +
  geom_ribbon(aes(
    x = x,
    ymin = mxn.s,
    ymax = mnn.s,
    fill = cc_trt
  ),
  alpha = 0.4) +
  
  geom_line(aes(y = prd0), size = 2) +
  scale_color_manual(values = cctrtpal) +
  scale_fill_manual(values = cctrtpal) +
  facet_grid(. ~ county) +
  #  scale_y_continuous(label_percent()) +
#  scale_x_log10() +
  guides(fill = F) +
  labs(y = "Soil water content (0-1)",
       x = "Pressure (cm H20)",
       color = NULL) +
  theme_bw() +
  theme(
    legend.direction = "horizontal",
    legend.position = "top",
    #legend.justification = c(1, 1),
    #panel.background = element_rect(fill = "gray90"),
    axis.title.y = element_text(angle = 90, vjust = 0.5),
    strip.text = element_text(size = rel(1.5)),
    legend.background = element_rect(color = "black"),
    axis.text = element_text(size = rel(1.2)),
    legend.text = element_text(size = rel(1.3)),
    axis.title = element_text(size = rel(1.3))
  )

ggsave("03_fit-models/fig_90cis.png")

## By site
## Site has a large effect but cover crop does not
ggplot(data = rdG, aes(x,y, color = cc_trt)) + 
  geom_point() + facet_wrap(~ site) + 
  geom_line(aes(y = prd1, group = site)) +
  ylab("Soil water content (0-1)") + 
  xlab("Pressure") +
  ggtitle("No visible effect of cover crops on \n water retention curves")

