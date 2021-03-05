# - look at studies from agevidence
# 3/4/2021



library(tidyverse)
library(janitor)


# organic matter ----------------------------------------------------------

dc <- read_csv("00_lit-stuff/AgEvidence-data.csv") %>% 
  clean_names()

dc %>% 
  select(percent_change, sample_depth, title, authors) %>% 
  ggplot(aes(sample_depth, percent_change)) + 
  geom_point()
  
