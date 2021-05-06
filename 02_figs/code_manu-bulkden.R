#--make fig of bulk density
#--updated: 3/2/2021

rm(list = ls())
library(tidyverse)
library(scales)
library(purrr)
library(PFIswhc)
library(Hmisc)

theme_set(theme_bw())


pfi_red <- "#9d3c22"
pfi_green <- "#b2bb1e"
pfi_blue <- "#036cb6"
pfi_orng <- "#e87d1e"
pfi_brn <- "#574319"
show_col(pfi_green)


dat <- 
  sare_bulkden %>% 
  left_join(sare_plotkey) %>% 
  mutate(cc_trt = case_when(
    grepl("cc", cc_trt) ~ "Rye Cover Crop",
    grepl("no", cc_trt) ~ "No Cover",
    TRUE ~ cc_trt),
    site_sys = paste(site_name, sys_trt, sep = "-"),
    site_sys = factor(site_sys, levels = c("West-grain", "Central-silage", "Central-grain", "East-grain")) 
  ) %>% 
  select(site_sys, cc_trt, bulkden_gcm3)
  

#--fig
dat %>%
  mutate(cc_trt2 = ifelse(cc_trt == "No Cover", "No\nCover\nCrop", "Rye\nCover\nCrop")) %>% 
  ggplot(aes(cc_trt2, bulkden_gcm3, color = cc_trt)) + 
  geom_jitter(color = "gray80", width = 0.1) +
  stat_summary(geom = "point", size = 3) + 
  stat_summary(geom = "linerange", fun.data = 'mean_cl_boot', size= 1.1) +
  facet_grid(.~site_sys) +
  scale_color_manual(values = c("Rye Cover Crop" = pfi_green,
                               "No Cover" = pfi_brn)) + 
  guides(color = F) +
  labs(x = NULL,
       y = "Bulk Density (g/cm3)") + 
  theme(strip.text = element_text(size = rel(1.3)),
        strip.background = element_blank(),
        legend.position = "bottom", 
        legend.text = element_text(size = rel(1.2)),
        axis.text = element_text(size = rel(1.2)),
        axis.text.x = element_text(angle = 0),
        axis.title = element_text(size = rel(1.3)))

ggsave("06_figs/fig_bulkden.png")

