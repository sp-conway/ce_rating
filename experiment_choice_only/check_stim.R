rm(list=ls())
library(tidyverse)
library(jsonlite)
d <- read_json(here("experiment/trials.json"),simplifyVector = T)
d %>%
  filter(effect!="catch" ) %>%
  pivot_longer(c(d1_c,d1_t,d1_d,d2_c,d2_t,d2_d),
               names_to = "tmp") %>%
  separate(tmp,into = c("dim","option"),sep="_") %>%
  pivot_wider(names_from = dim,
              values_from = value) %>%
  ggplot(aes(d1,d2,col=option))+
  coord_fixed(xlim=c(0,120),ylim=c(0,120))+
  geom_point()+
  facet_grid(effect~t_high)+
  ggthemes::theme_few()
