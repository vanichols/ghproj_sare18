############################
# author: Gina (vnichols@iastate.edu)
# created: may 8 2020
# last modified: May 11 2020 (continue model fitting)
#                May 12 2020 (confused by df problem)
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
fmL <- nlme::nlsList(y ~ SSgard(x, Thr, Ths, alp, scal), data = rdG)

#--Ths may vary by something.
plot(intervals(fmL))

#--residuals don't look terrible
plot(fmL)
plot(augPred(fmL, level = 0:1))


# fit a mixed model -------------------------------------------------------


#--this takes forever, doesn't work
#fmL2 <- nlme(fmL)

#--simplify the variance-covariance structure
#--nope this doesn't converge
#fmm <- nlme(fmL, random = pdDiag(Ths + scal + alp ~ 1))

#--remove Thr and alp
fmm1 <- nlme(fmL, random = pdDiag(Ths + scal ~ 1))

#--remove Thr and scal
fmm2 <- nlme(fmL, random = pdDiag(Ths + alp ~ 1))

#--fmm1 is better, let scal vary
anova(fmm1, fmm2)

#--look at fmm1 in more detail
summary(fmm1)
#--Thr and scal are correlated at 0.73. Hmm. 

plot(fmm1)
plot(augPred(fmm1, level = 0:1))


## the random effect intervals seem to be well constrained (FEM wording)
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

fmm_cc <- update(fmm1, 
               fixed = list(Thr + Ths + alp + scal ~ cc_trt),
               start = c(fxf[1], 0, #--Thr
                         fxf[2], 0, #--Ths
                         fxf[3], 0, #--alp
                         fxf[4], 0)) #--scal

#--not enough degrees of freedom?!
#--get rid of everything except Ths?

fmm_cc <- update(fmm1, 
               fixed = list(Ths ~ cc_trt),
               start = c(fxf[2], 0)) #--Ths
               
#--hmm. Do I need to explicitly state the other parameters are not to vary by cc_trt?

fmm_cc <- update(fmm1, 
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

fmm_cc <- update(fmm3, 
               fixed = list(Thr ~ cc_trt, Thr + alp + scal ~ 1),
               start = c(fxf2[1], 0)) #--Thr
#--still not enough. 


# try letting random effects vary by site instead of eu -------------------

#--create new grouped data
rdG2 <- groupedData(y ~ x | site, data = rd)

fmL2 <- nlme::nlsList(y ~ SSgard(x, Thr, Ths, alp, scal), data = rdG2)

#--Ths and Thr seem to vary by site
plot(intervals(fmL2))

#--residuals don't look terrible
plot(fmL2)

fmm5 <- nlme(fmL2, random = pdDiag(Ths + Thr ~ 1))
fxf3 <- fixef(fmm5)

fmm_cc <- update(fmm5, 
                 fixed = list(Thr ~ cc_trt, Thr + alp + scal ~ 1),
                 start = c(fxf2[1], 0)) #--Thr

#bah!

########################### IGNORE, SCRATCH AREA

# try nlme from scratch (not on lists) ------------------------------------

#--fit overall model just to get coefficients for starting values
g1 <- nls(y ~ SSgardner(x, thr, ths, alp, nscal),
          data = rd)

coef(g1)

me_code <- 
  nlme(y ~ SSgardner(x, thr, ths, alp, nscal),
     data = rd, 
     fixed = thr + ths + alp + nscal ~ 1,
     groups = ~ code,
     random = pdDiag(ths + nscal ~ 1),
     start = coef(g1))

#--this should be identical
fmm1



me_site <- 
  nlme(y ~ SSgardner(x, thr, ths, alp, nscal),
     data = rd, 
     fixed = thr + ths + alp + nscal ~ 1,
     groups = ~ site,
     random = pdDiag(ths + nscal ~ 1),
     start = coef(g1))

#--including grouping on a code level is better
anova(me_code, me_site)

me_cc <- 
  nlme(y ~ SSgardner(x, thr, ths, alp, nscal),
       data = rd, 
       fixed = list(ths ~ cc_trt, thr + alp + nscal ~ 1),
       groups = ~ code,
       random = nscal ~ 1,
       start = c(coef(g1)[1], 0,
                 coef(g1)[2], 0,
                 coef(g1)[3], 0,
                 coef(g1)[4], 0))

# https://rpubs.com/aforren1/orange-nonlinear -----------------------------

#--orange is already a grouped dataset. grouped by tree
Orange

f1 <- circumference ~ phi1 / (1 + exp(-(age - phi2)/phi3))
n1 <- nls(f1,
          data = Orange, 
          start = list(phi1 = 200, phi2 = 700, phi3 = 350))
coef(n1)

#--is the groups argument redundant in this case?
n2a <- nlme(f1,
           data = Orange,
           fixed = phi1 + phi2 + phi3 ~ 1,
           random = phi1 ~ 1,
           groups = ~ Tree,
           start = coef(n1))

#--yes it seems so
n2b <- nlme(f1,
           data = Orange,
           fixed = phi1 + phi2 + phi3 ~ 1,
           random = phi1 ~ 1,
#           groups = ~ Tree,
           start = coef(n1))

# nlme help page ex -------------------------------------------------------

Loblolly

fm1 <- nlme(height ~ SSasymp(age, Asym, R0, lrc),
            data = Loblolly,
            fixed = Asym + R0 + lrc ~ 1,
            random = Asym ~ 1,
            start = c(Asym = 103, R0 = -8.5, lrc = -3.3))
summary(fm1)
fm2 <- update(fm1, random = pdDiag(Asym + lrc ~ 1))
summary(fm2)


pd1 <- pdDiag(diag(1:3), nam = c("A","B","C"))
pd1

