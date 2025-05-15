rm(list=ls())
library(tidyverse)
library(here)
library(fs)
library(glue)

d <- here("data_choice_only/clean/choice.csv") %>%
  read_csv() %>%
  filter(effect!="catch")

s <- d %>% 
  distinct(effect, set, t_high, d1_t,d2_t,d1_c,d2_c,d1_d,d2_d) %>%
  pivot_longer(contains("d"),names_sep = "_",names_to = c("dim","option")) %>%
  pivot_wider(names_from = dim, values_from = value)

s %>%
  ggplot(aes(d1, d2, col=option))+
  geom_point(size=2.5,alpha=.75)+
  coord_fixed(xlim=c(0,150),ylim=c(0,150))+
  facet_grid(effect~set+t_high,labeller=label_both)+
  ggthemes::theme_few()
