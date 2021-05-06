library(scales)
library(tidyverse)
library(devtools)
#devtools::install_github("vanichols/ccweedmetapkg")
library(ccweedmetapkg)

data("ccweedmetapkg")
data(package = "ccweedmetapkg")

ccweeddat %>% 
  select(cc_bm_kgha, cc_wbio, ctl_wbio, cc_type) %>% 
  filter(cc_type == "grass") %>% 
  mutate(weedsup_pct = (ctl_wbio - cc_wbio)/ctl_wbio*100) %>% 
  ggplot(aes(cc_bm_kgha, weedsup_pct)) + 
  geom_point() + 
  coord_cartesian(ylim = c(0, 100))

library(nlraa)

?SSasymp

tibble(x = 0:10000) %>% 
  mutate(
    Yo = 100,
    k = 0.0004,
    y = Yo * (1 - exp(-k*x))/100
    ) %>% 
  ggplot(aes(x, y)) + 
  geom_line(size = 2, color = "green4") + 
  scale_y_continuous(labels = label_percent()) +
  scale_x_continuous(labels = label_comma()) +
  labs(x = "\nPounds of cover crop biomass per acre",
       y = "Weed\nSuppression") + 
  theme_bw() + 
  theme(axis.title.y = element_text(angle = 0, vjust = 0.5),
        axis.title = element_text(size = rel(1.5)),
        axis.text = element_text(size = rel(1.2))
        ) 

ggsave("06_figs/fig_theoretical-wsupp-vs-ccbio.png")
