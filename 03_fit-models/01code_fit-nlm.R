############################
# author: Gina (vnichols@iastate.edu)
# created: may 8 2020
# last modified: May 11 2020 (continue model fitting)
#
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
library(tidyverse)
library(HydroMe)
library(nlraa)
?SSgardner


#--data
rd <- read_csv("02_data-calcs/dc_swrc.csv") %>% 
  select(code, cc_trt, press_cm, vtheta_poros1) %>% 
  mutate(x = ifelse(press_cm == 0, 0.01, press_cm), 
         y = vtheta_poros1) 

rd %>% 
  select(code) %>% 
  distinct()

#--visual
rd %>% 
  ggplot(aes(x, y, color = code)) + 
  geom_point() + 
  geom_line() + 
  guides(color = F)


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


#--fit model to all data at once (use SSgardner)
#--note: nlsLM is a modified version of nls
#--in HydroMe example they don't supply starting vals
fm1 <- nlsLM(y ~ SSgardner(x, thr, ths, alp, nscal),
             rd)

fm1
coef(fm1)

rd %>% 
  ggplot(aes(x, y, color = code)) + 
  geom_point() + 
  geom_line() + 
  geom_line(aes(y = fitted(fm1)), color = "black", size = 3) +
  guides(color = F)

#--create grouped data
rdG <- groupedData(y ~ x | code, data = rd)

#--use SSgard this time
fmL <- nlme::nlsList(y ~ SSgard(x, Thr, Ths, alp, scal), data = rdG)

#--Ths may vary by something.
plot(intervals(fmL))

#--residuals don't look terrible
plot(fmL)
plot(augPred(fmL, level = 0:1))

#--this takes forever, doesn't work
#fmL2 <- nlme(fmL)

#--simplify the variance-covariance structure
#--nope this doesn't converge
#fmm <- nlme(fmL, random = pdDiag(Ths + scal + alp ~ 1))

#--remove Thr and alp
fmm <- nlme(fmL, random = pdDiag(Ths + scal ~ 1))

# works!
summary(fmm)
anova(fmm)

plot(fmm)
plot(augPred(fmm, level = 0:1))


## the random effect intervals seem to be well constrained (FEM wording)
intervals(fmm)

## Some outliers from Stout 4cc
plot(fmm, id = 0.01)

#--I know there's a way to assign colors on the fly....
rd %>% 
  mutate(mycolor = ifelse(code == "St-4cc", "y", "n")) %>% 
  ggplot(aes(x, y, group = code)) + 
  geom_line(aes(color = mycolor))



#--let's try to incorporate cover crop treatment

fxf <- fixef(fmm)

fmm2 <- update(fmm, 
               fixed = list(Thr + Ths + alp + scal ~ cc_trt),
               start = c(fxf[1], 0, #--Thr
                         fxf[2], 0, #--Ths
                         fxf[3], 0, #--alp
                         fxf[4], 0)) #--scal

#--not enough degrees of freedom?!
#--get rid of everything except Ths?

fmm2 <- update(fmm, 
               fixed = list(Ths ~ cc_trt),
               start = c(fxf[2], 0)) #--Ths
               
#--hmm. Do I need to explicitly state the other parameters are not to vary by cc_trt?

fmm2 <- update(fmm, 
               fixed = list(Ths ~ cc_trt, Thr + alp + scal ~ 1),
               start = c(fxf[2], 0)) #--Ths

#--what if I further simplify the random things
#--do random factors eat up dfs?
#--
fmm3 <- nlme(fmL, random = pdDiag(Ths ~ 1))
fmm3

anova(fmm, fmm3)
#--ok the first one fits better, remember this is just an experiment.

plot(fmm3)
plot(augPred(fmm3, level = 0:1))

fxf2 <- fixef(fmm3)

fmm4 <- update(fmm3, 
               fixed = list(Thr ~ cc_trt, Thr + alp + scal ~ 1),
               start = c(fxf2[1], 0)) #--Thr
#--still not enough. 