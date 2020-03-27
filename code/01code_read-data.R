############################
# author: Gina (vnichols@iastate.edu)
# created: Oct 10 2019
# last modified: Oct 31 2019 (updated to Funcke from Funke)
#                Dec 19 2019 (adding boyd measurements)
#                march 27 2020 (updating based on britt's calcs)
#
# purpose: read in raw pressure cell data
# 
# inputs:
#
# outputs:
#
# notes
#
##############################


rm(list = ls())
library(tidyverse)
library(readxl)



# read in excel sheet -----------------------------------------------------

stout <- 
  read_excel("data/raw/rd_pressure-cells2.xlsx", sheet = "Stout", skip = 1) %>%
  filter(!is.na(code))
  
funcke <- 
  read_excel("data/raw/rd_pressure-cells2.xlsx", sheet = "Funcke", skip = 1) %>%
  select(-orig_code) %>%
  filter(!is.na(code)) %>%
  mutate(atm1 = as.numeric(atm1))

boydgr <- 
  read_excel("data/raw/rd_pressure-cells2.xlsx", sheet = "Boydgrain", skip = 1) %>% 
  filter(!is.na(code)) %>% 
  mutate_at(vars(contains("cm")), as.numeric) %>% 
  mutate_at(vars(contains("_g")), as.numeric)

boydsil <- 
  read_excel("data/raw/rd_pressure-cells2.xlsx", sheet = "Boydsilage", skip = 1) %>% 
  filter(!is.na(code)) %>% 
  mutate_at(vars(contains("cm")), as.numeric) %>% 
  mutate_at(vars(contains("_g")), as.numeric) %>% 
  select(-notes)


# fix things? and combine ------------------------------------------------------

# funcke had two measurements at atm, combine them
funcke2 <- 
  funcke %>%
  mutate(atm = (atm1 - cylinder_g) + (atm2 - cylinder_g) + cylinder_g) %>%
  select(-atm1, -atm2)

#p28 was 'clogged', eliminate it from boydsil
boydsil2 <- 
  boydsil %>% 
  filter(code != "B42-p28")


# write to tidy data ------------------------------------------------------
bind_rows(stout, funcke2, boydgr, boydsil2) %>%
  write_csv("data/tidy/td_pressure-cells.csv")
